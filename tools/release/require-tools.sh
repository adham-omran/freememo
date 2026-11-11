#!/bin/sh
# Make sure curl, jq, git and git-filter-repo exist. Install what is missing
# when the image allows it, and fail with a clear message when it does not.
set -e
missing=""
for t in curl jq git; do command -v "$t" >/dev/null 2>&1 || missing="$missing $t"; done
git filter-repo --version >/dev/null 2>&1 || missing="$missing git-filter-repo"
[ -z "$missing" ] && exit 0
echo "require-tools: installing:$missing" >&2
sudo=""; [ "$(id -u)" -eq 0 ] || sudo="sudo"
if command -v apt-get >/dev/null 2>&1; then
  $sudo apt-get update -qq && $sudo apt-get install -y -qq $missing
elif command -v apk >/dev/null 2>&1; then
  $sudo apk add --no-cache $missing
elif command -v dnf >/dev/null 2>&1; then
  $sudo dnf install -y $missing
else
  echo "require-tools: no package manager; add these to the ci image:$missing" >&2; exit 1
fi
git filter-repo --version >/dev/null 2>&1 \
  || { echo "require-tools: git-filter-repo is still missing; add it to the ci image (pip install git-filter-repo)" >&2; exit 1; }
