#!/bin/sh
# Print the release notes: the commit subjects in the public history between
# the previous release tag and the new tip, newest first, one per line.
# Usage: notes.sh <public-dir> <previous-rev> <tip-rev>
#   <previous-rev> may be empty or unknown (first release): the notes then list
#   the newest 50 commits.
set -e
. "$(dirname "$0")/lib.sh"
[ $# -eq 3 ] || die "usage: notes.sh <public-dir> <previous-rev> <tip-rev>"
dir="$1"; prev="$2"; tip="$3"
if [ -n "$prev" ] && git -C "$dir" rev-parse --verify --quiet "$prev^{commit}" >/dev/null; then
  range="$prev..$tip"; limit=500
else
  range="$tip"; limit=50
fi
out="$(git -C "$dir" log --no-merges -n "$limit" --format='- %s' "$range")"
if [ -n "$out" ]; then printf '%s\n' "$out"; else echo "No public changes."; fi
