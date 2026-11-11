#!/bin/sh
# Create the GitHub release for a Release as a DRAFT, with the notes. Ticket 17's
# workflow attaches the installers and publishes it. An older draft with the
# same tag (left by a failed run) is deleted first, so a re-run is safe.
# Usage: create-draft.sh <release-version> <target-sha> <notes-file>
# Prints the release id.
set -e
. "$(dirname "$0")/lib.sh"
[ $# -eq 3 ] || die "usage: create-draft.sh <release-version> <target-sha> <notes-file>"
need_env GH_REPO GH_TOKEN
version="$1"; sha="$2"; notes="$3"
tag="v$version"
releases="$(gh_list "/repos/$GH_REPO/releases")"
gh_delete_each "$releases" '.[] | select(.draft and .tag_name == $t) | .id' "/repos/$GH_REPO/releases/" "" "$tag"
body="$(jq -n --arg t "$tag" --arg n "FreeMemo $version" --arg s "$sha" --rawfile b "$notes" \
  '{tag_name: $t, name: $n, target_commitish: $s, body: $b, draft: true, prerelease: false}')"
gh_ok POST "/repos/$GH_REPO/releases" "$body"
jq -r '.id' "$GH_BODY"
