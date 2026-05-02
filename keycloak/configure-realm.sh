#!/bin/bash
# Configure Keycloak cave realm settings that aren't reliably set via realm import.
# Requires: curl, python3
# Usage: ./configure-realm.sh [KEYCLOAK_URL]

KC_URL="${1:-http://localhost:8180}"

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

# Enable email verification and configure SMTP to use Mailpit
curl -sf -X PUT "${KC_URL}/admin/realms/cave" \
  -H "Authorization: Bearer ${TOKEN}" \
  -H "Content-Type: application/json" \
  -d '{
    "verifyEmail": true,
    "smtpServer": {
      "host": "mailpit",
      "port": "1025",
      "from": "cave@localhost",
      "fromDisplayName": "Cave",
      "ssl": "false",
      "starttls": "false",
      "auth": "false"
    }
  }'

echo "Realm configured: email verification enabled, SMTP → mailpit:1025"
