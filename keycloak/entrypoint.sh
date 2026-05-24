#!/bin/sh
# cave-keycloak entrypoint: substitute the CAVE_OIDC_CLIENT_SECRET into the
# baked realm import file before keycloak imports it. Lets cavectl rotate the
# OIDC client secret per-deploy without rebuilding the image.
set -e

TPL=/opt/keycloak/data/import/cave-realm.json
if [ -f "$TPL" ]; then
    TMP=$(mktemp)
    cp "$TPL" "$TMP"

    sub() {
        # sub VAR_NAME value — replace __VAR_NAME__ in the template
        sed -i "s|__$1__|$2|g" "$TMP"
    }

    [ -n "$CAVE_OIDC_CLIENT_SECRET" ] && sub CAVE_OIDC_CLIENT_SECRET "$CAVE_OIDC_CLIENT_SECRET"
    [ -n "$CAVE_BASE_URL" ]           && sub CAVE_BASE_URL "$CAVE_BASE_URL"

    # SMTP — defaults are sensible if env not set
    sub SMTP_HOST          "${SMTP_HOST:-mailpit}"
    sub SMTP_PORT          "${SMTP_PORT:-1025}"
    sub SMTP_FROM          "${SMTP_FROM:-cave@localhost}"
    sub SMTP_FROM_DISPLAY  "${SMTP_FROM_DISPLAY:-Cave}"
    sub SMTP_SSL           "${SMTP_SSL:-false}"
    sub SMTP_STARTTLS      "${SMTP_STARTTLS:-false}"
    sub SMTP_AUTH          "${SMTP_AUTH:-false}"
    sub SMTP_USER          "${SMTP_USER:-}"
    sub SMTP_PASSWORD      "${SMTP_PASSWORD:-}"

    cat "$TMP" > "$TPL"
    rm -f "$TMP"
fi

exec /opt/keycloak/bin/kc.sh "$@"
