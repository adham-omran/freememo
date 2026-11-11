#!/bin/sh
# Start an installed Local Install headless and wait for READY on its local port.
# Usage: tools/installer/smoke.sh <path of the launcher> [timeout seconds]
# Seam 1 (test/freememo/local_install_harness.clj) detects READY the same way: the
# launcher prints `FREEMEMO_READY port=<n> pg_port=<n>` when the web server and
# the embedded PostgreSQL both answer. An account.json is seeded so the launcher
# starts PostgreSQL. Without one it prints FREEMEMO_SIGNIN_READY and starts none,
# which would not test the bundled PostgreSQL.
# Exit: 0 on READY, 1 on a launcher that exits or on a timeout. The launcher log
# goes to stdout on failure.
set -u
launcher="${1:?usage: smoke.sh <launcher> [timeout seconds]}"
timeout="${2:-300}"
dir="${SMOKE_DIR:-$(mktemp -d)}"
data="$dir/data"
log="$dir/launcher.log"
mkdir -p "$data"
printf '%s' '{"origin":"http://127.0.0.1:1","token":"smoke-token","user_id":7,"username":"smoke"}' > "$data/account.json"
chmod 600 "$data/account.json"

FREEMEMO_HEADLESS=true "$launcher" --data-dir "$data" --port 47600 --origin http://127.0.0.1:1 > "$log" 2>&1 &
pid=$!
trap 'kill "$pid" 2>/dev/null; wait "$pid" 2>/dev/null' EXIT

waited=0
while [ "$waited" -lt "$timeout" ]; do
  if grep -q '^FREEMEMO_READY port=[0-9]* pg_port=[0-9]*' "$log"; then
    grep '^FREEMEMO_READY' "$log"
    port="$(sed -n 's/^FREEMEMO_READY port=\([0-9]*\).*/\1/p' "$log" | head -1)"
    if command -v curl >/dev/null 2>&1; then
      curl -s -o /dev/null -w 'GET / answered %{http_code}\n' "http://127.0.0.1:$port/" || true
    fi
    exit 0
  fi
  if ! kill -0 "$pid" 2>/dev/null; then
    echo "smoke: the launcher exited before READY" >&2
    cat "$log"
    exit 1
  fi
  sleep 2
  waited=$((waited + 2))
done
echo "smoke: no READY after $timeout seconds" >&2
cat "$log"
exit 1
