#!/bin/bash
# Cave restore — restores from a backup archive.
# Usage: ./restore.sh ARCHIVE [CONTAINER_PREFIX]
#
# ARCHIVE is the path to a cave-*.tar.gz backup
# CONTAINER_PREFIX defaults to "cave-prod"
#
# WARNING: This will overwrite all data in the target containers.

set -euo pipefail

ARCHIVE="${1:?Usage: ./restore.sh ARCHIVE [CONTAINER_PREFIX]}"
PREFIX="${2:-cave-prod}"
WORK_DIR=$(mktemp -d)

if [ ! -f "${ARCHIVE}" ]; then
  echo "Archive not found: ${ARCHIVE}" >&2
  exit 1
fi

echo "=== Cave Restore ==="
echo "Archive: ${ARCHIVE}"
echo "Target:  ${PREFIX}"
echo ""

# Extract
tar -xzf "${ARCHIVE}" -C "${WORK_DIR}"
SNAPSHOT=$(ls -d "${WORK_DIR}"/cave-* | head -1)

if [ -f "${SNAPSHOT}/metadata.txt" ]; then
  echo "Backup metadata:"
  cat "${SNAPSHOT}/metadata.txt"
  echo ""
fi

read -p "This will OVERWRITE all data in ${PREFIX}. Continue? [y/N] " confirm
if [ "${confirm}" != "y" ] && [ "${confirm}" != "Y" ]; then
  echo "Aborted."
  rm -rf "${WORK_DIR}"
  exit 0
fi

# 1. Restore Postgres — cave DB
echo "Restoring cave database..."
podman exec "${PREFIX}-pg" dropdb -U cave --if-exists cave
podman exec "${PREFIX}-pg" createdb -U cave cave
podman exec -i "${PREFIX}-pg" pg_restore -U cave -d cave --no-owner \
  < "${SNAPSHOT}/cave.pgdump"

# 3. Restore git repos
if [ -d "${SNAPSHOT}/repos" ]; then
  echo "Restoring git repositories..."
  # Clear existing repos and copy backup
  podman exec "${PREFIX}" rm -rf /var/lib/cave/repos
  podman cp "${SNAPSHOT}/repos" "${PREFIX}:/var/lib/cave/repos"
  podman exec "${PREFIX}" chown -R cave:cave /var/lib/cave/repos
fi

# 4. Restore config (optional — usually regenerated)
if [ -f "${SNAPSHOT}/cave.conf" ]; then
  echo "Restoring config..."
  podman cp "${SNAPSHOT}/cave.conf" "${PREFIX}:/etc/cave.conf"
fi

# 5. Regenerate authorized_keys from DB
echo "Regenerating authorized_keys..."
podman exec "${PREFIX}" cave-server update-keys \
  --config /etc/cave.conf \
  --output /home/cave/.ssh/authorized_keys \
  --cave-shell /usr/bin/cave-shell.sh 2>/dev/null || true

# Cleanup
rm -rf "${WORK_DIR}"

echo ""
echo "Restore complete. Restart services:"
echo "  systemctl --user restart ${PREFIX/cave-prod/cave}"
echo "  (or: make prod-stop && make prod-start)"
