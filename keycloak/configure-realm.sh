#!/bin/bash
# Configure Keycloak cave realm settings that aren't reliably set via realm import.
# Requires: curl, python3
# Usage: ./configure-realm.sh [KEYCLOAK_URL] [MAILPIT_HOST]

KC_URL="${1:-http://localhost:8180}"
MAILPIT_HOST="${2:-mailpit}"

echo "Configuring Keycloak realm at ${KC_URL}..."

# Wait for Keycloak to be ready
for i in $(seq 1 60); do
  curl -sf "${KC_URL}/realms/cave/.well-known/openid-configuration" > /dev/null 2>&1 && break
  if [ "$i" -eq 60 ]; then
    echo "Keycloak not ready after 60s, giving up." >&2
    exit 1
  fi
  sleep 1
done

# Get admin token
TOKEN=$(curl -sf -X POST "${KC_URL}/realms/master/protocol/openid-connect/token" \
  -d "client_id=admin-cli&grant_type=password&username=admin&password=admin" \
  | python3 -c "import sys,json; print(json.load(sys.stdin)['access_token'])")

if [ -z "$TOKEN" ]; then
  echo "Failed to get admin token" >&2
  exit 1
fi

AUTH="Authorization: Bearer ${TOKEN}"

# Enable email verification, password reset, and configure SMTP
curl -sf -X PUT "${KC_URL}/admin/realms/cave" \
  -H "${AUTH}" \
  -H "Content-Type: application/json" \
  -d '{
    "verifyEmail": true,
    "resetPasswordAllowed": true,
    "loginTheme": "cave",
    "emailTheme": "cave",
    "smtpServer": {
      "host": "'"${MAILPIT_HOST}"'",
      "port": "1025",
      "from": "cave@localhost",
      "fromDisplayName": "Cave",
      "ssl": "false",
      "starttls": "false",
      "auth": "false"
    }
  }'

echo "Realm configured: cave theme, email verification, SMTP → ${MAILPIT_HOST}:1025"

# --- Create custom browser flow with OTP support ---
# Only create if it doesn't already exist

FLOW_EXISTS=$(curl -sf "${KC_URL}/admin/realms/cave/authentication/flows" \
  -H "${AUTH}" | python3 -c "
import sys,json
flows = json.load(sys.stdin)
print('yes' if any(f['alias'] == 'cave-browser' for f in flows) else 'no')")

if [ "$FLOW_EXISTS" = "no" ]; then
  echo "Creating cave-browser authentication flow..."

  # Copy the built-in browser flow
  curl -sf -X POST "${KC_URL}/admin/realms/cave/authentication/flows/browser/copy" \
    -H "${AUTH}" \
    -H "Content-Type: application/json" \
    -d '{"newName": "cave-browser"}' -o /dev/null

  # Get the new flow's executions
  EXECUTIONS=$(curl -sf "${KC_URL}/admin/realms/cave/authentication/flows/cave-browser/executions" \
    -H "${AUTH}")

  # Find the "cave-browser Browser - Conditional OTP" execution and enable it
  OTP_ID=$(echo "$EXECUTIONS" | python3 -c "
import sys,json
execs = json.load(sys.stdin)
for e in execs:
    dn = e.get('displayName','')
    if 'Conditional OTP' in dn:
        print(e['id'])
        break
")

  if [ -n "$OTP_ID" ]; then
    curl -sf -X PUT "${KC_URL}/admin/realms/cave/authentication/flows/cave-browser/executions" \
      -H "${AUTH}" \
      -H "Content-Type: application/json" \
      -d "{\"id\":\"${OTP_ID}\",\"requirement\":\"CONDITIONAL\"}" -o /dev/null
    echo "  OTP conditional flow enabled"
  fi

  # Move password form before OTP (critical — OTP must come after password)
  PASSWORD_ID=$(echo "$EXECUTIONS" | python3 -c "
import sys,json
execs = json.load(sys.stdin)
for e in execs:
    if 'Username Password' in e.get('displayName',''):
        print(e['id'])
        break
")
  if [ -n "$PASSWORD_ID" ]; then
    curl -sf -X POST "${KC_URL}/admin/realms/cave/authentication/executions/${PASSWORD_ID}/raise-priority" \
      -H "${AUTH}" -o /dev/null
    echo "  Password form moved before OTP"
  fi

  # Bind the cave-browser flow as the realm's browser flow
  curl -sf -X PUT "${KC_URL}/admin/realms/cave" \
    -H "${AUTH}" \
    -H "Content-Type: application/json" \
    -d '{"browserFlow": "cave-browser"}' -o /dev/null

  echo "  cave-browser flow bound as browser flow"
else
  echo "cave-browser flow already exists"
fi

# --- Fix reset credentials flow: disable forced OTP setup ---
# The default reset credentials flow has a "Reset OTP" step set to REQUIRED,
# which forces users to configure an authenticator after resetting their password.
# Set it to DISABLED so password reset just resets the password.

RESET_EXECUTIONS=$(curl -sf "${KC_URL}/admin/realms/cave/authentication/flows/reset%20credentials/executions" \
  -H "${AUTH}")

RESET_COND_OTP_ID=$(echo "$RESET_EXECUTIONS" | python3 -c "
import sys,json
execs = json.load(sys.stdin)
for e in execs:
    dn = e.get('displayName','')
    if 'Conditional OTP' in dn and e.get('authenticationFlow'):
        print(e['id'])
        break
")

if [ -n "$RESET_COND_OTP_ID" ]; then
  curl -sf -X PUT "${KC_URL}/admin/realms/cave/authentication/flows/reset%20credentials/executions" \
    -H "${AUTH}" \
    -H "Content-Type: application/json" \
    -d "{\"id\":\"${RESET_COND_OTP_ID}\",\"requirement\":\"DISABLED\"}" -o /dev/null
  echo "Reset credentials flow: Conditional OTP subflow disabled"
else
  echo "Reset credentials flow: no Conditional OTP subflow found (OK)"
fi

# --- Configure social identity providers ---
# Each provider is only added if the corresponding env vars are set.
# Skipped silently otherwise, so the script works in dev without any OAuth apps.

add_identity_provider() {
  local alias="$1" name="$2" provider_id="$3" client_id="$4" client_secret="$5"
  shift 5
  local extra_config="$*"

  # Check if already exists
  local exists
  exists=$(curl -sf "${KC_URL}/admin/realms/cave/identity-provider/instances/${alias}" \
    -H "${AUTH}" -o /dev/null -w "%{http_code}")

  if [ "$exists" = "200" ]; then
    echo "Identity provider '${name}' already configured"
    return
  fi

  local config="{
    \"alias\": \"${alias}\",
    \"displayName\": \"${name}\",
    \"providerId\": \"${provider_id}\",
    \"enabled\": true,
    \"trustEmail\": true,
    \"storeToken\": false,
    \"firstBrokerLoginFlowAlias\": \"first broker login\",
    \"config\": {
      \"clientId\": \"${client_id}\",
      \"clientSecret\": \"${client_secret}\",
      \"syncMode\": \"IMPORT\"${extra_config:+, ${extra_config}}
    }
  }"

  local status
  status=$(curl -sf -X POST "${KC_URL}/admin/realms/cave/identity-provider/instances" \
    -H "${AUTH}" \
    -H "Content-Type: application/json" \
    -d "${config}" -o /dev/null -w "%{http_code}")

  if [ "$status" = "201" ]; then
    echo "Identity provider '${name}' added"
  else
    echo "Failed to add identity provider '${name}' (HTTP ${status})" >&2
  fi
}

# GitHub
if [ -n "$CAVE_GITHUB_CLIENT_ID" ] && [ -n "$CAVE_GITHUB_CLIENT_SECRET" ]; then
  add_identity_provider "github" "GitHub" "github" \
    "$CAVE_GITHUB_CLIENT_ID" "$CAVE_GITHUB_CLIENT_SECRET"
else
  echo "Skipping GitHub IdP (CAVE_GITHUB_CLIENT_ID / CAVE_GITHUB_CLIENT_SECRET not set)"
fi

# GitLab
if [ -n "$CAVE_GITLAB_CLIENT_ID" ] && [ -n "$CAVE_GITLAB_CLIENT_SECRET" ]; then
  add_identity_provider "gitlab" "GitLab" "gitlab" \
    "$CAVE_GITLAB_CLIENT_ID" "$CAVE_GITLAB_CLIENT_SECRET"
else
  echo "Skipping GitLab IdP (CAVE_GITLAB_CLIENT_ID / CAVE_GITLAB_CLIENT_SECRET not set)"
fi

# Google
if [ -n "$CAVE_GOOGLE_CLIENT_ID" ] && [ -n "$CAVE_GOOGLE_CLIENT_SECRET" ]; then
  add_identity_provider "google" "Google" "google" \
    "$CAVE_GOOGLE_CLIENT_ID" "$CAVE_GOOGLE_CLIENT_SECRET"
else
  echo "Skipping Google IdP (CAVE_GOOGLE_CLIENT_ID / CAVE_GOOGLE_CLIENT_SECRET not set)"
fi

echo "Done."
