#!/bin/bash
set -e

CONFIG="/etc/cave.conf"

# Generate config if it doesn't exist
if [ ! -f "$CONFIG" ]; then
  # Render the optional workflow image allowlist (space-separated prefixes) as a
  # Lisp list, or nil when unset.
  WF_ALLOWLIST="nil"
  if [ -n "${CAVE_WORKFLOWS_IMAGE_ALLOWLIST:-}" ]; then
    WF_ALLOWLIST="("
    for prefix in ${CAVE_WORKFLOWS_IMAGE_ALLOWLIST}; do
      WF_ALLOWLIST="${WF_ALLOWLIST}\"${prefix}\" "
    done
    WF_ALLOWLIST="${WF_ALLOWLIST})"
  fi
  cat > "$CONFIG" <<CONF
(:http-port 8080
 :ssh-port 22
 :ssh-user "cave"
 :data-dir "/var/lib/cave"
 :db-host "${CAVE_DB_HOST:-localhost}"
 :db-port ${CAVE_DB_PORT:-5432}
 :db-name "${CAVE_DB_NAME:-cave}"
 :db-user "${CAVE_DB_USER:-cave}"
 :db-password "${CAVE_DB_PASSWORD:-cave}"
 :secret-key "${CAVE_SECRET_KEY:-$(head -c 32 /dev/urandom | od -An -tx1 | tr -d ' \n')}"
 :base-url "${CAVE_BASE_URL:-http://localhost:8080}"
 :runner-clone-base-url "${CAVE_RUNNER_CLONE_BASE_URL:-}"
 :authorized-keys-path "${CAVE_AUTHORIZED_KEYS_PATH:-/var/lib/cave/ssh/authorized_keys}"
 :cave-shell "/usr/bin/cave-shell.sh"
 :oidc-issuer "${CAVE_OIDC_ISSUER:-}"
 :oidc-issuer-internal "${CAVE_OIDC_ISSUER_INTERNAL:-}"
 :oidc-client-id "${CAVE_OIDC_CLIENT_ID:-cave}"
 :oidc-client-secret "${CAVE_OIDC_CLIENT_SECRET:-}"
 :zoekt-enabled ${CAVE_ZOEKT_ENABLED:-nil}
 :zoekt-web-url "${CAVE_ZOEKT_WEB_URL:-http://cave-prod-zoekt-web:6070}"
 :chamber-enabled ${CAVE_CHAMBER_ENABLED:-t}
 :zoekt-index-dir "${CAVE_ZOEKT_INDEX_DIR:-/data/zoekt-index}"
 :deps-scan-enabled ${CAVE_DEPS_SCAN_ENABLED:-nil}
 :deps-scan-image "${CAVE_DEPS_SCAN_IMAGE:-ghcr.io/atgreen/cave-scan:main}"
 :deps-scan-labels "${CAVE_DEPS_SCAN_LABELS:-}"
 :advisory-feeds "${CAVE_ADVISORY_FEEDS:-}"
 :scheduler-enabled ${CAVE_SCHEDULER_ENABLED:-t}
 :advisory-sync-interval-hours ${CAVE_ADVISORY_SYNC_INTERVAL_HOURS:-24}
 :workflows-allow-privileged ${CAVE_WORKFLOWS_ALLOW_PRIVILEGED:-nil}
 :workflows-image-allowlist ${WF_ALLOWLIST}
 :checks-allow-network ${CAVE_CHECKS_ALLOW_NETWORK:-nil}
 :checks-require-network-isolation ${CAVE_CHECKS_REQUIRE_NETWORK_ISOLATION:-nil}
 :checks-memory-mb ${CAVE_CHECKS_MEMORY_MB:-nil}
 :checks-timeout-seconds ${CAVE_CHECKS_TIMEOUT_SECONDS:-120}
 :sandbox-landlock ${CAVE_SANDBOX_LANDLOCK:-t})
CONF
fi

# CAVE_ROLE=ssh runs this container as a git-SSH front end only: sshd, the shared
# repos, and nothing else. It exists so the internet-facing listener can sit on a
# network mode that preserves client source addresses (pasta), which the bridge's
# rootless port forwarding cannot - every client arrives at sshd as the container's
# own address there, so per-source decisions (OpenSSH PerSourcePenalties) and any
# audit of who pushed are both meaningless. The main server keeps the bridge.
CAVE_ROLE="${CAVE_ROLE:-server}"

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

# Schema and authorized_keys belong to the server; the front end only reads what
# the server wrote into the shared volume.
if [ "$CAVE_ROLE" != "ssh" ]; then
  # Run migrations
  cave-server migrate --config "$CONFIG"

  # Generate initial authorized_keys
  cave-server update-keys \
    --config "$CONFIG" \
    --output "${CAVE_AUTHORIZED_KEYS_PATH:-/var/lib/cave/ssh/authorized_keys}" \
    --cave-shell /usr/bin/cave-shell.sh || true

  chown cave:cave "${CAVE_AUTHORIZED_KEYS_PATH:-/var/lib/cave/ssh/authorized_keys}" 2>/dev/null || true

  # Ensure cave user owns data dirs and repos
  chown -R cave:cave /var/lib/cave
fi

# serve runs as the cave user (see below), so the zoekt index dir must be
# cave-writable. A freshly-mounted volume can come up root-owned — chown the
# mount point so indexing works. Non-recursive: it stays cave-owned thereafter.
chown cave:cave /data/zoekt-index 2>/dev/null || true

# Trust all cave repos (ownership may differ between init and runtime)
# Set for both root (Cave server) and cave user (SSH/git-shell)
git config --global --add safe.directory '*'
git config --global user.email "cave@localhost"
git config --global user.name "Cave"
su -c "git config --global --add safe.directory '*'" cave
su -c "git config --global user.email 'cave@localhost'" cave
su -c "git config --global user.name 'Cave'" cave

# This instance's SSH identity and the keys it trusts, both in the data volume
# so they survive redeploys and so a separate SSH front-end container serves the
# same endpoint identity from the same files.
#
# Generated here rather than baked into the image on purpose: image-time keys
# are shared by every instance built from it, and their private halves ship to
# anyone who can pull it, which makes a client's known_hosts pin worthless.
# Instances that adopted those shared keys (under the old /var/lib/cave/ paths)
# get a fresh identity here - a one-time known_hosts change for their users.
setup_ssh_identity() {
  install -d -m 700 -o cave -g cave /var/lib/cave/ssh
  if [ ! -f /var/lib/cave/ssh/ssh_host_ed25519_key ]; then
    echo "Generating this instance's SSH host keys..."
    ssh-keygen -q -t ed25519 -N '' -f /var/lib/cave/ssh/ssh_host_ed25519_key
    ssh-keygen -q -t rsa -b 4096 -N '' -f /var/lib/cave/ssh/ssh_host_rsa_key
  fi
  chmod 600 /var/lib/cave/ssh/ssh_host_*_key
  touch /var/lib/cave/ssh/authorized_keys
  chown cave:cave /var/lib/cave/ssh/authorized_keys
  chmod 600 /var/lib/cave/ssh/authorized_keys
  cat > /etc/ssh/sshd_config.d/10-cave.conf <<SSHD
HostKey /var/lib/cave/ssh/ssh_host_ed25519_key
HostKey /var/lib/cave/ssh/ssh_host_rsa_key
AuthorizedKeysFile /var/lib/cave/ssh/authorized_keys
SSHD
  # sshd hands the forced command a sanitised environment - none of this
  # container's variables survive - so where cave lives has to be stated here
  # for cave-shell.sh and the post-receive hook to find it.
  if [ -n "${CAVE_INTERNAL_URL:-}" ]; then
    echo "SetEnv CAVE_INTERNAL_URL=${CAVE_INTERNAL_URL}" >> /etc/ssh/sshd_config.d/10-cave.conf
  fi
}

setup_ssh_identity

# -D -e keeps sshd in the foreground with its log on stderr, so auth failures,
# refusals and penalties land in `podman logs` instead of nowhere at all. The ssh
# role execs it as pid 1 further down instead of backgrounding it here.
if [ "$CAVE_ROLE" != "ssh" ]; then
  /usr/sbin/sshd -D -e &
fi

# Start Cave (foreground) — run from /opt/cave so static/ is found.
#
# Run serve as the non-root `cave` user (issue #22). Pushes arrive over SSH and
# run as `cave`; if serve ran as root, PR merges — and the `git gc --auto` they
# trigger — would write root-owned objects/packs into repos, and the next push's
# quarantine migration could no longer write into them ("unable to migrate
# objects to permanent storage"). Sharing one uid across merges, auto-gc, and
# pushes keeps object/pack ownership consistent. sshd stays root (started above);
# only this final process drops. HOME is set explicitly so git reads the cave
# user's ~/.gitconfig (safe.directory, identity) rather than root's.
#
# catatonit is pid 1 as a reaping init (issue #23). The post-receive hook syncs
# mirrors via a backgrounded `cave-server sync-mirrors &`; when the push session
# ends that process is orphaned to pid 1, and any git child of a thread that died
# mid-push reparents there too. A bare `runuser`/`cave-server` pid 1 does not reap
# adopted zombies, so those defunct entries accumulate until the container's PID
# budget is spent and it stops forking git-SSH handlers. catatonit reaps them and
# forwards SIGTERM for a clean shutdown.
if [ "$CAVE_ROLE" = "ssh" ]; then
  # The front end owns no state: the main server writes authorized_keys into the
  # shared volume, and git-shell reads permissions straight from the database.
  echo "Cave git-SSH front end ready (role=ssh)."
  exec /usr/libexec/catatonit/catatonit -- /usr/sbin/sshd -D -e
fi

cd /opt/cave
exec /usr/libexec/catatonit/catatonit -- \
  /usr/sbin/runuser -u cave -- env HOME=/home/cave cave-server serve --config "$CONFIG"
