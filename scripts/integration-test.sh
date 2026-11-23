#!/usr/bin/env bash
# integration-test.sh -- end-to-end integration test against a live server.
#
# Usage:
#   scripts/integration-test.sh            # builds the server via nix, then runs
#   FMS_BIN=/path/to/fms scripts/integration-test.sh   # use an existing binary
#
# The script builds (or reuses) the server binary, creates a fresh sandbox
# under /tmp/fms-it-<pid>-<pid>, generates two media files (mp4 + mp3) plus
# an .nfo sidecar with ffmpeg when available, starts the server on a free
# port with auth enabled, and runs a curl+python3 check matrix.
# Exits 0 when all checks pass, 1 otherwise. Prints:
#   INTEGRATION: <passed> passed, <failed> failed
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(git -C "$SCRIPT_DIR" rev-parse --show-toplevel)"
cd "$REPO_ROOT"

# --- server binary -----------------------------------------------------------
if [[ -n "${FMS_BIN:-}" ]]; then
  BIN="$FMS_BIN"
else
  OUT="$(nix build .#default --print-out-paths)"
  BIN="$OUT/bin/fms"
fi
if [[ ! -x "$BIN" ]]; then
  echo "FATAL: server binary not found/executable at $BIN" >&2
  exit 2
fi
echo "server binary: $BIN"

# --- ffmpeg (for media generation; falls back to fake files) -----------------
FFMPEG="$(command -v ffmpeg 2>/dev/null || true)"
if [[ -z "$FFMPEG" ]]; then
  P="$(nix eval --raw nixpkgs#ffmpeg.outPath 2>/dev/null || echo none)"
  if [[ "$P" != "none" && -x "$P/bin/ffmpeg" ]]; then FFMPEG="$P/bin/ffmpeg"; fi
fi
if [[ -z "$FFMPEG" ]]; then
  for c in /nix/store/*ffmpeg*/bin/ffmpeg; do
    if [[ -x "$c" ]]; then FFMPEG="$c"; break; fi
  done
fi
if [[ -n "$FFMPEG" ]]; then
  echo "ffmpeg: $FFMPEG"
else
  echo "ffmpeg not found; writing minimal placeholder media files" >&2
fi

# --- sandbox -----------------------------------------------------------------
TMP="/tmp/fms-it-$$"
LIB="$TMP/library"
STATE="$TMP/state"
mkdir -p "$LIB" "$STATE"

MOVIE="Integration Test Movie"
"$FFMPEG" -hide_banner -loglevel error \
  -f lavfi -i "testsrc=duration=3:size=160x120:rate=10" \
  -f lavfi -i "sine=frequency=440:duration=3" \
  -c:v libx264 -pix_fmt yuv420p -c:a aac -y "$LIB/$MOVIE.mp4" 2>/dev/null \
  || printf 'FAKEMP4DATA' > "$LIB/$MOVIE.mp4"
"$FFMPEG" -hide_banner -loglevel error \
  -f lavfi -i "sine=frequency=330:duration=2" \
  -c:a libmp3lame -y "$LIB/tone.mp3" 2>/dev/null \
  || printf 'FAKEMP3DATA' > "$LIB/tone.mp3"
cat > "$LIB/$MOVIE.nfo" <<'NFO'
<?xml version="1.0" encoding="UTF-8"?>
<movie><title>Integration Test Movie</title><overview>IT overview text</overview><year>2026</year></movie>
NFO

# --- free port ---------------------------------------------------------------
PORT="$(python3 -c 'import socket; s=socket.socket(); s.bind(("127.0.0.1",0)); print(s.getsockname()[1]); s.close()')"
BASE="http://127.0.0.1:$PORT"
echo "sandbox: $TMP  port: $PORT"

# --- start server ------------------------------------------------------------
env MEDIA_SERVER_PORT="$PORT" \
    MEDIA_LIBRARY_PATH="$LIB" \
    FMS_STATE_DIR="$STATE" \
    FMS_REQUIRE_AUTH=true \
    FMS_ADMIN_PASSWORD=it-admin-pass \
    FMS_LOG_LEVEL=warn \
  "$BIN" > "$TMP/server.log" 2>&1 &
SRV=$!
cleanup() {
  kill "$SRV" 2>/dev/null || true
  wait "$SRV" 2>/dev/null || true
  rm -rf "$TMP"
}
trap cleanup EXIT

for i in $(seq 1 60); do
  code="$(curl -s -o /dev/null -w '%{http_code}' -m 2 "$BASE/healthz" 2>/dev/null || true)"
  [[ "$code" == "200" ]] && break
  sleep 0.5
done
if [[ "${code:-}" != "200" ]]; then
  echo "FATAL: server did not become healthy; log tail:" >&2
  tail -20 "$TMP/server.log" >&2
  exit 2
fi
echo "server healthy at $BASE"

# --- helpers -----------------------------------------------------------------
BODY="$TMP/body.json"
HDRS="$TMP/headers.txt"
req() { # req METHOD PATH [JSON_DATA] [TOKEN] -> prints http code, body in $BODY
  local method="$1" path="$2" data="${3:-}" tok="${4:-}"
  local args=(-s -o "$BODY" -w '%{http_code}' -m 30 -X "$method")
  if [[ -n "$tok" ]]; then args+=(-H "X-Emby-Token: $tok"); fi
  if [[ -n "$data" ]]; then args+=(-H 'Content-Type: application/json' -d "$data"); fi
  curl "${args[@]}" "$BASE$path"
}
jget() { # jget 'python-expr over d' -> prints value; reads JSON from $BODY
  python3 -c 'import json,sys
d = json.load(open(sys.argv[1]))
print(eval(sys.argv[2]))' "$BODY" "$1"
}

PASS=0
FAIL=0
ok()  { PASS=$((PASS+1)); echo "PASS $1"; }
bad() { FAIL=$((FAIL+1)); echo "FAIL $1 -- $2"; }
check_code() { # name want_code got_code
  if [[ "$2" == "$3" ]]; then ok "$1 (http $3)"; else bad "$1" "want http $2, got $3 (body: $(head -c 200 "$BODY" 2>/dev/null))"; fi
}
check_any2xx() {
  if [[ "$3" =~ ^2 ]]; then ok "$1 (http $3)"; else bad "$1" "want 2xx, got $3 (body: $(head -c 200 "$BODY" 2>/dev/null))"; fi
}
check_eq() { # name want got
  if [[ "$2" == "$3" ]]; then ok "$1"; else bad "$1" "want [$2] got [$3]"; fi
}

# --- check matrix ------------------------------------------------------------

code="$(req GET /System/Info/Public)"
check_code "info-public-200" 200 "$code"
check_eq "info-public-has-ServerName" True "$(jget "isinstance(d.get('ServerName'), str)")"

code="$(req GET /Items)"
check_code "items-unauth-401" 401 "$code"

code="$(req POST /Users/AuthenticateByName '{"Username":"admin","Pw":"it-admin-pass"}')"
check_code "auth-ok-200" 200 "$code"
TOKEN="$(jget "d.get('AccessToken','')")"
USERID="$(jget "d['User']['Id']")"
if [[ -n "$TOKEN" && -n "$USERID" ]]; then ok "auth-returns-token-and-user"; else bad "auth-returns-token-and-user" "token/uid missing"; fi

code="$(req POST /Users/AuthenticateByName '{"Username":"admin","Pw":"wrong-pass"}')"
check_code "auth-wrong-pw-401" 401 "$code"

# m93: logout revokes the session token server-side (uses a DEDICATED token
# so the shared $TOKEN stays valid for the rest of the suite)
code="$(req POST /Users/AuthenticateByName '{"Username":"admin","Pw":"it-admin-pass"}')"
T2="$(jget "d.get('AccessToken','')")"
code="$(req GET /Users/Me "" "$T2")"
check_code "me-before-logout-200" 200 "$code"
code="$(req POST /Users/Logout "" "$T2")"
check_code "logout-204" 204 "$code"
code="$(req GET /Users/Me "" "$T2")"
check_code "me-after-logout-401" 401 "$code"

# scan the library + refresh metadata, then poll until >= 2 items are visible
req POST /library/rescan "" "$TOKEN" > /dev/null
req POST "/Library/Refresh?replaceAll=true" "" "$TOKEN" > /dev/null
N=""
for i in $(seq 1 60); do
  code="$(req GET /Items "" "$TOKEN")"
  N="$(jget "d.get('TotalRecordCount', 0)" 2>/dev/null || echo 0)"
  if [[ "${N:-0}" -ge 2 ]]; then break; fi
  sleep 0.5
done
check_code "items-auth-200" 200 "$code"
if [[ "${N:-0}" -ge 2 ]]; then ok "items-total>=2-after-scan ($N)"; else bad "items-total>=2-after-scan" "TotalRecordCount=$N"; fi

code="$(req GET "/Items?search=Integration" "" "$TOKEN")"
check_code "items-search-200" 200 "$code"
check_eq "items-search-finds-movie" True "$(jget "d.get('TotalRecordCount',0) >= 1")"

VID="$(jget "next((i['Id'] for i in d.get('Items',[]) if i.get('Type')=='Movie'), '')" 2>/dev/null || true)"
if [[ -z "$VID" ]]; then
  code="$(req GET /Items "" "$TOKEN")"
  VID="$(jget "next((i['Id'] for i in d.get('Items',[]) if i.get('Type')=='Movie'), '')")"
fi
if [[ -n "$VID" ]]; then ok "found-movie-id ($VID)"; else bad "found-movie-id" "no Movie item"; VID="1"; fi

code="$(req GET "/Items/$VID" "" "$TOKEN")"
check_code "item-detail-200" 200 "$code"
check_eq "item-detail-Type-Movie" Movie "$(jget "d.get('Type')")"
check_eq "item-detail-Name-from-title" "$MOVIE" "$(jget "d.get('Name')")"

code="$(req POST "/Items/$VID/PlaybackInfo" "" "$TOKEN")"
check_code "playbackinfo-200" 200 "$code"
check_eq "playbackinfo-has-MediaSources" True "$(jget "isinstance(d.get('MediaSources'), list) and len(d['MediaSources']) >= 1")"

# discover the real stream route path from the generated OpenAPI spec
code="$(req GET /api-docs/openapi.json "" "$TOKEN")"
STREAM_PATH="$(jget "next((p for p in d.get('paths',{}) if 'stream' in p and '{id}' in p and 'transcode' not in p), '')" 2>/dev/null || true)"
if [[ -z "$STREAM_PATH" ]]; then
  STREAM_PATH="/stream/{id}"
fi
STREAM_URL_PATH="${STREAM_PATH//\{id\}/$VID}"
code="$(curl -s -o "$BODY" -D "$HDRS" -w '%{http_code}' -m 30 \
  -H "X-Emby-Token: $TOKEN" -H 'Range: bytes=0-1023' "$BASE$STREAM_URL_PATH")"
check_code "stream-range-206 ($STREAM_URL_PATH)" 206 "$code"
if grep -qi '^Content-Range: bytes 0-1023/' "$HDRS"; then
  ok "stream-content-range-header"
else
  bad "stream-content-range-header" "$(grep -i content-range "$HDRS" || echo none)"
fi

# m32: api_key query-param auth (Jellyfin-style) so <video src=/stream/:id?api_key=..> works
code="$(curl -s -o /dev/null -w '%{http_code}' -m 30 -H 'Range: bytes=0-1023' "$BASE$STREAM_URL_PATH?api_key=$TOKEN")"
check_code "stream-range-206-api-key-query ($STREAM_URL_PATH?api_key=...)" 206 "$code"
code="$(curl -s -o /dev/null -w '%{http_code}' -m 30 -H 'Range: bytes=0-1023' "$BASE$STREAM_URL_PATH")"
check_code "stream-range-401-no-creds" 401 "$code"
code="$(curl -s -o /dev/null -w '%{http_code}' -m 30 -H 'Range: bytes=0-1023' "$BASE$STREAM_URL_PATH?api_key=definitely-wrong")"
check_code "stream-range-401-bad-api-key" 401 "$code"

code="$(req GET "/Items/$VID/Images" "" "$TOKEN")"
check_code "images-list-200" 200 "$code"
check_eq "images-is-list" True "$(jget "isinstance(d, list)")"

code="$(req POST "/Users/$USERID/PlayedItems/$VID" '{"Played":true}' "$TOKEN")"
check_any2xx "mark-played-2xx" 2 "$code"
code="$(req GET "/Users/$USERID/Items/$VID/UserData" "" "$TOKEN")"
check_code "userdata-200" 200 "$code"
check_eq "userdata-played-true" True "$(jget "d.get('played') == True")"

code="$(req GET /Sessions "" "$TOKEN")"
check_code "sessions-200" 200 "$code"
check_eq "sessions-is-list" True "$(jget "isinstance(d, list)")"

code="$(req GET /System/Info "" "$TOKEN")"
check_code "system-info-auth-200" 200 "$code"

# --- m35 metadata settings: auth matrix --------------------------------------
code="$(req GET /Metadata/Settings)"
check_code "meta-settings-unauth-401" 401 "$code"

code="$(req GET /Metadata/Settings "" "$TOKEN")"
check_code "meta-settings-admin-200" 200 "$code"
check_eq "meta-settings-admin-has-key-field" True "$(jget "'TmdbApiKey' in d")"

code="$(req POST /Metadata/Settings '{"tmdbApiKey":"it-key","fetchImages":true}' "$TOKEN")"
check_code "meta-settings-post-admin-200" 200 "$code"
check_eq "meta-settings-post-echoes-key" "it-key" "$(jget "d.get('TmdbApiKey','')")"

code="$(req POST /Metadata/Settings/Test "" "$TOKEN")"
check_code "meta-settings-test-admin-200" 200 "$code"
check_eq "meta-settings-test-has-Ok" True "$(jget "'Ok' in d")"

code="$(req POST /Users '{"Name":"it-user","Password":"it-user-pass"}' "$TOKEN")"
check_code "create-nonadmin-user-201" 201 "$code"
code="$(req POST /Users/AuthenticateByName '{"Username":"it-user","Pw":"it-user-pass"}')"
check_code "nonadmin-auth-200" 200 "$code"
UTOKEN="$(jget "d.get('AccessToken','')")"

code="$(req GET /Metadata/Settings "" "$UTOKEN")"
check_code "meta-settings-user-200" 200 "$code"
check_eq "meta-settings-nonadmin-key-redacted" True "$(jget "d.get('TmdbApiKey') is None")"

code="$(req POST /Metadata/Settings '{"tmdbApiKey":"x"}' "$UTOKEN")"
check_code "meta-settings-post-nonadmin-403" 403 "$code"

code="$(req POST /Metadata/Settings/Test "" "$UTOKEN")"
check_code "meta-settings-test-nonadmin-403" 403 "$code"

code="$(req POST /Metadata/Settings '{"tmdbApiKey":""}' )"
check_code "meta-settings-post-unauth-401" 401 "$code"

# m78: POST /items/Intro/Scan - admin 200 with counters, non-admin 403, unauth 401
code="$(req POST /items/Intro/Scan "" "$TOKEN")"
check_code "intro-scan-admin-200" 200 "$code"
check_eq "intro-scan-has-counters" True "$(jget "isinstance(d.get('Scanned'), int) and isinstance(d.get('IntroFound'), int) and isinstance(d.get('AlreadyCached'), int)")"
code="$(req POST /items/Intro/Scan "" "$UTOKEN")"
check_code "intro-scan-nonadmin-403" 403 "$code"
code="$(req POST /items/Intro/Scan "")"
check_code "intro-scan-unauth-401" 401 "$code"

# --- summary -----------------------------------------------------------------
echo "INTEGRATION: $PASS passed, $FAIL failed"
if [[ "$FAIL" -gt 0 ]]; then exit 1; fi
exit 0
