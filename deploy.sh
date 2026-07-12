#!/usr/bin/env bash
# Zero-downtime blue-green deploy for the prod stack (docker-compose.prod.yml).
#
# Contract:
#   Precondition  — run from the repo working tree on the deploy box; config.edn,
#                   .env present; Docker daemon reachable; the tree is checked out
#                   at the commit to deploy (the Forgejo workflow does the sync).
#   Postcondition — Caddy routes to a healthy container built from the current
#                   commit; the previously-live color is stopped after a drain
#                   window; a non-zero exit means NO flip happened.
#   Invariant     — Caddy keeps owning :8080 throughout; on any failure before the
#                   flip, the previously-live color still serves (zero downtime).
#
# Limitation: zero downtime holds only for SCHEMA-COMPATIBLE deploys. Both colors
# share one Postgres; the new color runs db/setup-schema on boot, so a breaking
# schema change can destabilize the old color during the drain window. DB backup
# and rollback are out of scope.
#
# Serialized with flock: a second run exits rather than interleaving a flip.
set -euo pipefail

REPO_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$REPO_DIR"

# --- preconditions: bind-mount sources must exist as FILES ----------------
# A missing host file makes Docker create a directory in its place, silently
# breaking the container (config.edn → credits fail closed; Caddyfile → proxy
# won't start). Fail fast with a clear message instead.
for required in config.edn Caddyfile; do
  [[ -f "$REPO_DIR/$required" ]] || { echo "deploy: FATAL — $required missing in $REPO_DIR"; exit 1; }
done

# --- tunables -------------------------------------------------------------
HEALTH_TIMEOUT="${HEALTH_TIMEOUT:-120}"   # seconds to wait for the new color to become healthy
DRAIN_SECONDS="${DRAIN_SECONDS:-90}"      # keep old color alive post-flip (>= app ws-idle-timeout 60s)
KEEP_IMAGES="${KEEP_IMAGES:-3}"           # freememo:<version> images to retain after a deploy
UPSTREAM_FILE="$REPO_DIR/caddy/upstream.caddy"
COMPOSE=(docker compose -f docker-compose.yml -f docker-compose.prod.yml)

# --- serialize: one deploy at a time --------------------------------------
exec 9>"$REPO_DIR/.deploy.lock"
flock -n 9 || { echo "deploy: another run holds the lock; exiting"; exit 1; }

# --- version: host-supplied (the Docker build context has no .git) --------
APP_VERSION="$(git describe --tags --long --always --dirty)"
export APP_VERSION
echo "deploy: version ${APP_VERSION}"

# --- pick the idle color from Caddy's live upstream (single source of truth) --
live_color=""
if [[ -f "$UPSTREAM_FILE" ]]; then
  if   grep -q app-blue  "$UPSTREAM_FILE"; then live_color=blue
  elif grep -q app-green "$UPSTREAM_FILE"; then live_color=green
  fi
fi
case "$live_color" in
  blue)  target_color=green ;;
  green) target_color=blue  ;;
  *)     target_color=blue  ;;   # cold start: no color live yet
esac
target_svc="app-${target_color}"
target_ctr="freememo-${target_color}"
echo "deploy: live=${live_color:-none} target=${target_color}"

mkdir -p caddy logs/blue logs/green

# --- shared infra (built only if the image is missing; never rebuilt here) --
"${COMPOSE[@]}" up -d postgres ontop

# --- build + start the idle color -----------------------------------------
"${COMPOSE[@]}" build "$target_svc"
"${COMPOSE[@]}" up -d --no-deps "$target_svc"

# --- health-gate: no flip until the new color reports healthy -------------
echo "deploy: waiting up to ${HEALTH_TIMEOUT}s for ${target_ctr} to become healthy"
deadline=$(( SECONDS + HEALTH_TIMEOUT ))
until [[ "$(docker inspect -f '{{.State.Health.Status}}' "$target_ctr" 2>/dev/null)" == "healthy" ]]; do
  if (( SECONDS >= deadline )); then
    echo "deploy: FAILED — ${target_ctr} not healthy in ${HEALTH_TIMEOUT}s; leaving ${live_color:-none} live"
    "${COMPOSE[@]}" rm -sf "$target_svc" || true
    exit 1
  fi
  sleep 3
done
echo "deploy: ${target_ctr} healthy"

# --- flip: point Caddy at the new color, reload gracefully ----------------
# Write the new upstream, then either reload (Caddy already up) or start Caddy
# fresh (cold start reads the file). If a reload fails, revert the file so it
# never disagrees with what Caddy is actually serving.
printf 'reverse_proxy %s:8080\n' "$target_svc" > "$UPSTREAM_FILE"
if [[ "$(docker inspect -f '{{.State.Running}}' freememo-caddy 2>/dev/null)" == "true" ]]; then
  if ! "${COMPOSE[@]}" exec -T caddy caddy reload --config /etc/caddy/Caddyfile; then
    if [[ -n "$live_color" ]]; then
      printf 'reverse_proxy app-%s:8080\n' "$live_color" > "$UPSTREAM_FILE"
    fi
    echo "deploy: FAILED — Caddy reload rejected the new upstream; ${live_color:-none} still live"
    exit 1
  fi
else
  "${COMPOSE[@]}" up -d --no-deps caddy
fi
echo "deploy: Caddy now routing to ${target_svc}"

# --- drain + stop the previous color --------------------------------------
if [[ -n "$live_color" && "$live_color" != "$target_color" ]]; then
  echo "deploy: draining app-${live_color} for ${DRAIN_SECONDS}s before stopping it"
  sleep "$DRAIN_SECONDS"
  "${COMPOSE[@]}" stop "app-${live_color}"
  echo "deploy: stopped app-${live_color}"
fi

# --- reclaim old images (keep the newest $KEEP_IMAGES; in-use images are skipped) --
docker images freememo --format '{{.Repository}}:{{.Tag}}' \
  | tail -n +"$(( KEEP_IMAGES + 1 ))" \
  | xargs -r docker image rm 2>/dev/null || true

echo "deploy: done — ${APP_VERSION} live on ${target_color}"
