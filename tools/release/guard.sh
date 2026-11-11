#!/bin/sh
# Stop a release whose Release Version is not larger than the latest
# published GitHub release. Draft and pre-release entries do not count.
# Usage: guard.sh <release-version>      (for example 26.11.14336)
# Exit 0: go ahead. Exit 1: stop. A repository with no release passes.
set -e
. "$(dirname "$0")/lib.sh"
[ $# -eq 1 ] || die "usage: guard.sh <release-version>"
need_env GH_REPO GH_TOKEN
new="$1"
gh_api GET "/repos/$GH_REPO/releases/latest"
case "$GH_STATUS" in
  404) echo "guard: no published release yet, so $new may go out"; exit 0 ;;
  200) ;;
  *) die "GET releases/latest answered $GH_STATUS: $(head -c 300 "$GH_BODY")" ;;
esac
latest="$(jq -r '.tag_name' "$GH_BODY")"
latest="${latest#v}"
if version_gt "$new" "$latest"; then
  echo "guard: $new is larger than the latest release $latest"
else
  die "Release Version $new is not larger than the latest release $latest. Nothing was published."
fi
