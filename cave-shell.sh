#!/bin/bash
# cave-shell.sh — Git SSH transport wrapper
# Called by sshd via authorized_keys command= prefix.
# Authenticates via cave git-shell, then proxies through Chamber gRPC.
#
# Usage in authorized_keys:
#   command="/usr/bin/cave-shell.sh /etc/cave.conf 7",...  ssh-ed25519 AAAA...

set -e

CONFIG="$1"
KEY_ID="$2"

if [ -z "$SSH_ORIGINAL_COMMAND" ]; then
    echo "cave: interactive shell access is not supported" >&2
    exit 1
fi

# Parse the git command and repo path
read -r GIT_CMD REPO_PATH <<< "$SSH_ORIGINAL_COMMAND"
REPO_PATH=$(echo "$REPO_PATH" | tr -d "'\"" | sed 's/\.git$//; s|^/||')

case "$GIT_CMD" in
    git-upload-pack|git-receive-pack) ;;
    *)
        echo "cave: invalid command" >&2
        exit 1
        ;;
esac

# Authenticate: cave git-shell checks key->user->repo permissions.
cave git-shell --config "$CONFIG" --key-id "$KEY_ID" >/dev/null 2>&1
if [ $? -ne 0 ]; then
    echo "cave: authentication failed" >&2
    exit 1
fi

# Proxy git protocol through Chamber gRPC (handles locking, cache, etc.)
exec cave git-proxy --config "$CONFIG" --command "$GIT_CMD" --repo "$REPO_PATH"
