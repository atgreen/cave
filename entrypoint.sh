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
 :secret-key "${CAVE_SECRET_KEY:-$(head -c 32 /dev/urandom | od -An -tx1 | tr -d ' \n')}"
 :base-url "${CAVE_BASE_URL:-http://localhost:8080}"
 :authorized-keys-path "/home/cave/.ssh/authorized_keys"
 :cave-shell "/usr/bin/cave-shell.sh")
CONF
fi

# Wait for PostgreSQL to accept connections
DB_HOST="${CAVE_DB_HOST:-localhost}"
DB_PORT="${CAVE_DB_PORT:-5432}"
echo "Waiting for PostgreSQL at ${DB_HOST}:${DB_PORT}..."
for i in $(seq 1 30); do
  if bash -c "echo >/dev/tcp/${DB_HOST}/${DB_PORT}" 2>/dev/null; then
    echo "PostgreSQL is accepting connections."
    break
  fi
  if [ "$i" -eq 30 ]; then
    echo "PostgreSQL not ready after 30s, giving up." >&2
    exit 1
  fi
  sleep 1
done

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
  --cave-shell /usr/bin/cave-shell.sh || true

chown cave:cave /home/cave/.ssh/authorized_keys 2>/dev/null || true

# Ensure cave user owns data dirs and repos
chown -R cave:cave /var/lib/cave

# Trust all cave repos (ownership may differ between init and runtime)
git config --global --add safe.directory '*'

# Start sshd
/usr/sbin/sshd

# Start Cave (foreground) — run from /opt/cave so static/ is found
cd /opt/cave
exec cave serve --config "$CONFIG"
