#!/bin/bash
# cave-shell.sh — Git SSH transport wrapper
# Called by sshd via authorized_keys command= prefix.
# Authenticates via cave git-shell, then execs the git command.
#
# Usage in authorized_keys:
#   command="/usr/bin/cave-shell.sh /etc/cave.conf 7",...  ssh-ed25519 AAAA...

set -e

CONFIG="$1"
KEY_ID="$2"
HTTP_PORT="${3:-8080}"

if [ -z "$SSH_ORIGINAL_COMMAND" ]; then
    echo "cave: interactive shell access is not supported" >&2
    exit 1
fi

# Parse the git command and repo path
read -r GIT_CMD REPO_PATH <<< "$SSH_ORIGINAL_COMMAND"
REPO_PATH=$(echo "$REPO_PATH" | tr -d "'\"")

case "$GIT_CMD" in
    git-upload-pack|git-receive-pack) ;;
    *)
        echo "cave: invalid command" >&2
        exit 1
        ;;
esac

# Authenticate: cave git-shell checks key->user->repo permissions.
# It prints the repo disk path on stdout (may be mixed with log noise).
# We grab the last non-empty line that looks like a path.
OUTPUT=$(cave git-shell --config "$CONFIG" --key-id "$KEY_ID" 2>/dev/null)
EXIT=$?

if [ $EXIT -ne 0 ]; then
    echo "cave: authentication failed" >&2
    exit 1
fi

# Extract the disk path — last line containing a slash
DISK_PATH=$(echo "$OUTPUT" | grep '/' | tail -1)

if [ -z "$DISK_PATH" ]; then
    echo "cave: could not determine repository path" >&2
    exit 1
fi

# For pushes, bracket with Chamber write lock acquire/release.
# For fetches, exec directly — reads don't need write locks.
if [ "$GIT_CMD" = "git-receive-pack" ]; then
    PUSH_TOKEN=$(curl -sf -X POST "http://localhost:${HTTP_PORT}/-/internal/push/acquire/${REPO_PATH}")
    if [ $? -ne 0 ] || [ -z "$PUSH_TOKEN" ]; then
        echo "cave: server busy, try again" >&2
        exit 1
    fi
    # Run (not exec) so we can release the lock after git exits
    $GIT_CMD "$DISK_PATH"
    EXIT_CODE=$?
    curl -sf -X POST -d "$PUSH_TOKEN" \
         "http://localhost:${HTTP_PORT}/-/internal/push/release/${REPO_PATH}" >/dev/null 2>&1
    exit $EXIT_CODE
else
    exec $GIT_CMD "$DISK_PATH"
fi
