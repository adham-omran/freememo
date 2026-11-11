#!/bin/sh
# Delete every release and every v* tag except the one to keep. Ticket 17's
# workflow calls it as its last step, after it published the release.
# It refuses to run unless the release to keep exists and is published, so a
# failed build never deletes the last good release.
# Usage: prune.sh <release-version>
set -e
. "$(dirname "$0")/lib.sh"
[ $# -eq 1 ] || die "usage: prune.sh <release-version>"
need_env GH_REPO GH_TOKEN
keep="v$1"
releases="$(gh_list "/repos/$GH_REPO/releases")"
published="$(printf '%s' "$releases" | jq -r --arg t "$keep" '[.[] | select(.tag_name == $t and (.draft | not))] | length')"
[ "$published" -ge 1 ] || die "release $keep is not published, so nothing was deleted"
gh_delete_each "$releases" '.[] | select(.tag_name != $t) | .id' "/repos/$GH_REPO/releases/" "prune: deleted release" "$keep"
tags="$(gh_list "/repos/$GH_REPO/tags")"
gh_delete_each "$tags" '.[] | select(.name != $t and (.name | startswith("v"))) | .name' "/repos/$GH_REPO/git/refs/tags/" "prune: deleted tag" "$keep"
