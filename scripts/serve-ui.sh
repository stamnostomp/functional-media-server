#!/usr/bin/env bash
# serve-ui.sh - run the media server with the stamno-media Elm UI served
# from it (same-origin: the UI calls the API at http://localhost:8080,
# our default port).
#
# usage: serve-ui.sh [LIBRARY_DIR] [PORT]
#
#   LIBRARY_DIR   media library root (default: scripts/demo-library)
#   PORT          HTTP port (default 8080)
#
# env overrides:
#   FMS_BIN         server binary (default: nix build .#default of this repo)
#   FMS_UI_DIR      UI dir to serve; when set, FMS_UI_OUT is not built
#   FMS_UI_OUT      pre-built UI output dir (default: nix build of the
#                   sibling stamno-media repo, read-only)
#   FMS_STATE_DIR   state dir (default: /tmp/fms-ui-state-$$)
#   FMS_REQUIRE_AUTH  auth gate (default false)
#
# ASCII only. Exits when the server exits; kills it on EXIT (trap).

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

PORT="${2:-8080}"
LIB="${1:-$SCRIPT_DIR/demo-library}"

# --- UI output dir ---------------------------------------------------------
if [ -n "${FMS_UI_DIR:-}" ]; then
  UI_OUT="$FMS_UI_DIR"
elif [ -n "${FMS_UI_OUT:-}" ]; then
  UI_OUT="$FMS_UI_OUT"
else
  UI_OUT="$(nix build "$REPO_ROOT/../stamno-media" --no-link --print-out-paths)"
fi
echo "UI dir: $UI_OUT"

# --- library ---------------------------------------------------------------
mkdir -p "$LIB"

# --- server binary ---------------------------------------------------------
if [ -n "${FMS_BIN:-}" ]; then
  BIN="$FMS_BIN"
else
  BIN="$(cd "$REPO_ROOT" && nix build .#default --print-out-paths)/bin/fms"
fi
echo "server binary: $BIN"

STATE="${FMS_STATE_DIR:-/tmp/fms-ui-state-$$}"
mkdir -p "$STATE"
echo "state dir: $STATE"
echo "library:   $LIB"

SRV=""
cleanup() {
  if [ -n "$SRV" ]; then kill "$SRV" 2>/dev/null || true; fi
}
trap cleanup EXIT

MEDIA_SERVER_PORT="$PORT" \
MEDIA_LIBRARY_PATH="$LIB" \
FMS_STATE_DIR="$STATE" \
FMS_UI_DIR="$UI_OUT" \
FMS_REQUIRE_AUTH="${FMS_REQUIRE_AUTH:-false}" \
FMS_LOG_LEVEL="${FMS_LOG_LEVEL:-info}" \
  "$BIN" &
SRV=$!

# wait for /healthz (max 30s)
i=0
while [ $i -lt 30 ]; do
  if curl -s -o /dev/null -w '%{http_code}' "http://127.0.0.1:$PORT/healthz" 2>/dev/null | grep -q '^200$'; then
    break
  fi
  i=$((i + 1))
  sleep 1
done

echo ""
echo "UI URL:   http://localhost:$PORT/"
echo "API base: http://localhost:$PORT/"
echo "(Ctrl-C or kill $SRV to stop)"
wait "$SRV"
