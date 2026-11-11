#!/bin/sh
# Print the Release Version of a commit (docs/adr/0002), and nothing else.
# Usage: tools/release/release-version.sh [<rev>]       (default HEAD)
#        tools/release/release-version.sh --epoch <unix seconds>
# Lives in tools/release, which the public-history cleaner keeps: the GitHub
# Actions workflow of the public repository calls it too.
# Plain sh, git and date, with no JVM, so the Forgejo deploy, the GitHub
# Actions release and the Docker build can call it on any runner.
# freememo.release-version/from-epoch-seconds is the same rule, and
# release_version_test checks that both agree.
set -e
cd "$(dirname "$0")/../.."
if [ "$1" = "--epoch" ]; then
  ct="$2"
else
  ct="$(git log -1 --format=%ct "${1:-HEAD}")"
fi
parts="$(date -u -d "@$ct" '+%y %m %d %H %M' 2>/dev/null || date -u -r "$ct" '+%y %m %d %H %M')"
set -- $parts
y="$1"; m="${2#0}"; d="${3#0}"; h="${4#0}"; mi="${5#0}"
echo "$y.$m.$(( (d - 1) * 1440 + h * 60 + mi ))"
