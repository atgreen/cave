#!/bin/bash
# Cave backup — dumps Postgres, copies git repos, saves config.
# Usage: ./backup.sh [BACKUP_DIR] [CONTAINER_PREFIX]
#
# BACKUP_DIR defaults to ~/cave-backups
# CONTAINER_PREFIX defaults to "cave-prod" (use "cave" for dev)
#
# Produces: BACKUP_DIR/cave-YYYY-MM-DD-HHMMSS.tar.gz
# Optional: set CAVE_BACKUP_S3 to sync to S3 (e.g., s3://bucket/cave-backups)

set -euo pipefail

BACKUP_DIR="${1:-$HOME/cave-backups}"
PREFIX="${2:-cave-prod}"
TIMESTAMP=$(date +%Y-%m-%d-%H%M%S)
WORK_DIR=$(mktemp -d)
SNAPSHOT="${WORK_DIR}/cave-${TIMESTAMP}"

mkdir -p "${BACKUP_DIR}" "${SNAPSHOT}"

echo "=== Cave Backup ${TIMESTAMP} ==="
echo "Container prefix: ${PREFIX}"

# 1. Dump Postgres (both cave and keycloak databases)
echo "Dumping PostgreSQL..."
podman exec "${PREFIX}-pg" pg_dump -U cave -d cave --format=custom \
  > "${SNAPSHOT}/cave.pgdump"
podman exec "${PREFIX}-pg" pg_dump -U cave -d keycloak --format=custom \
  > "${SNAPSHOT}/keycloak.pgdump" 2>/dev/null || echo "  (no keycloak DB, skipping)"

# 2. Copy git repos
echo "Backing up git repositories..."
podman cp "${PREFIX}:/var/lib/cave/repos" "${SNAPSHOT}/repos" 2>/dev/null || \
  echo "  (no repos directory)"

# 3. Copy config
echo "Backing up config..."
podman cp "${PREFIX}:/etc/cave.conf" "${SNAPSHOT}/cave.conf" 2>/dev/null || \
  echo "  (no config file)"

# 4. Copy SSH authorized_keys
podman cp "${PREFIX}:/home/cave/.ssh/authorized_keys" \
  "${SNAPSHOT}/authorized_keys" 2>/dev/null || true

# 5. Record metadata
cat > "${SNAPSHOT}/metadata.txt" <<EOF
timestamp: ${TIMESTAMP}
cave_version: $(podman exec "${PREFIX}" cave --version 2>/dev/null || echo unknown)
postgres_version: $(podman exec "${PREFIX}-pg" postgres --version 2>/dev/null || echo unknown)
schema_version: $(podman exec "${PREFIX}-pg" psql -U cave -d cave -Atc \
  "SELECT COALESCE(MAX(version),0) FROM cave_schema_version" 2>/dev/null || echo unknown)
repo_count: $(find "${SNAPSHOT}/repos" -name "*.git" -type d 2>/dev/null | wc -l)
EOF
cat "${SNAPSHOT}/metadata.txt"

# 6. Compress
ARCHIVE="${BACKUP_DIR}/cave-${TIMESTAMP}.tar.gz"
tar -czf "${ARCHIVE}" -C "${WORK_DIR}" "cave-${TIMESTAMP}"
rm -rf "${WORK_DIR}"

SIZE=$(du -h "${ARCHIVE}" | cut -f1)
echo ""
echo "Backup complete: ${ARCHIVE} (${SIZE})"

# 7. Optional S3 sync
if [ -n "${CAVE_BACKUP_S3:-}" ]; then
  echo "Syncing to ${CAVE_BACKUP_S3}..."
  aws s3 cp "${ARCHIVE}" "${CAVE_BACKUP_S3}/cave-${TIMESTAMP}.tar.gz"
  echo "S3 sync complete."
fi

# 8. Prune old local backups (keep last 10)
BACKUP_COUNT=$(ls -1 "${BACKUP_DIR}"/cave-*.tar.gz 2>/dev/null | wc -l)
if [ "${BACKUP_COUNT}" -gt 10 ]; then
  ls -1t "${BACKUP_DIR}"/cave-*.tar.gz | tail -n +11 | xargs rm -f
  echo "Pruned old backups, keeping last 10."
fi
