#!/bin/bash
set -e

CONFIG="/etc/cave.conf"

# Generate config if it doesn't exist
if [ ! -f "$CONFIG" ]; then
  cat > "$CONFIG" <<CONF
(:http-port 8080
 :ssh-port 22
 :data-dir "/var/lib/cave"
 :db-host "${CAVE_DB_HOST:-localhost}"
 :db-port ${CAVE_DB_PORT:-5432}
 :db-name "${CAVE_DB_NAME:-cave}"
 :db-user "${CAVE_DB_USER:-cave}"
 :db-password "${CAVE_DB_PASSWORD:-cave}"
 :secret-key "${CAVE_SECRET_KEY:-$(openssl rand -hex 32)}"
 :base-url "${CAVE_BASE_URL:-http://localhost:8080}"
 :authorized-keys-path "/home/cave/.ssh/authorized_keys"
 :cave-binary "/usr/bin/cave")
CONF
fi

# Run migrations
cave migrate --config "$CONFIG"

# Create admin user if CAVE_ADMIN_USER is set and user doesn't exist
if [ -n "$CAVE_ADMIN_USER" ] && [ -n "$CAVE_ADMIN_PASSWORD" ]; then
  cave init \
    --admin-user "$CAVE_ADMIN_USER" \
    --admin-password "$CAVE_ADMIN_PASSWORD" \
    --config "$CONFIG" || true
fi

# Generate initial authorized_keys
cave update-keys \
  --config "$CONFIG" \
  --output /home/cave/.ssh/authorized_keys \
  --cave-binary /usr/bin/cave || true

chown cave:cave /home/cave/.ssh/authorized_keys 2>/dev/null || true

# Start sshd
/usr/sbin/sshd

# Start Cave (foreground)
exec cave serve --config "$CONFIG"
