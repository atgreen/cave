#!/bin/sh
# cave-keycloak entrypoint: substitute the CAVE_OIDC_CLIENT_SECRET into the
# baked realm import file before keycloak imports it. Lets cavectl rotate the
# OIDC client secret per-deploy without rebuilding the image.
set -e

TPL=/opt/keycloak/data/import/cave-realm.json
if [ -f "$TPL" ]; then
    TMP=$(mktemp)
    cp "$TPL" "$TMP"
    if [ -n "$CAVE_OIDC_CLIENT_SECRET" ]; then
        sed -i "s|__CAVE_OIDC_CLIENT_SECRET__|$CAVE_OIDC_CLIENT_SECRET|g" "$TMP"
    fi
    if [ -n "$CAVE_BASE_URL" ]; then
        # base URL may contain slashes; use pipe as sed delimiter (already done)
        sed -i "s|__CAVE_BASE_URL__|$CAVE_BASE_URL|g" "$TMP"
    fi
    cat "$TMP" > "$TPL"
    rm -f "$TMP"
fi

exec /opt/keycloak/bin/kc.sh "$@"
