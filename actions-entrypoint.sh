#!/bin/sh
# cave-actions runtime entrypoint.
#
# Invoked by the cave runner as:  <image> <action-dir> <main-file>
# Runs the user-provided `using: lisp` action in THIS throwaway container (which
# shares only the workspace volume + /__cave_rt with the job), never in the
# runner. The SDK is preloaded; the action file runs on load.
set -e
ACTION_DIR="${1:-/action}"
MAIN_FILE="${2:-main.lisp}"
exec sbcl --noinform --disable-ldb --non-interactive \
  --load /opt/cave-actions/sdk.lisp \
  --eval "(cave-actions:run-action \"${ACTION_DIR}\" \"${MAIN_FILE}\")"
