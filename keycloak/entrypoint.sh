#!/bin/sh
# cave-keycloak entrypoint: substitute the CAVE_OIDC_CLIENT_SECRET into the
# baked realm import file before keycloak imports it. Lets cavectl rotate the
# OIDC client secret per-deploy without rebuilding the image.
set -e

TPL=/opt/keycloak/data/import/cave-realm.json
if [ -n "$CAVE_OIDC_CLIENT_SECRET" ] && [ -f "$TPL" ]; then
    if grep -q __CAVE_OIDC_CLIENT_SECRET__ "$TPL"; then
        TMP=$(mktemp)
        sed "s|__CAVE_OIDC_CLIENT_SECRET__|$CAVE_OIDC_CLIENT_SECRET|g" "$TPL" > "$TMP"
        cat "$TMP" > "$TPL"
        rm -f "$TMP"
    fi
fi

exec /opt/keycloak/bin/kc.sh "$@"
