#!/bin/sh
# The asset names of a Release, read with jq from the one list
# resources/freememo/release-assets.json (see tools/installer/asset-names.mjs).
# Usage: asset-names.sh <version>             the required names, one per line
#        asset-names.sh <version> --optional  the optional names
#        asset-names.sh <version> <kind>      one name: macos, windows, linux, app, release, sig
set -e
manifest="$(dirname "$0")/../../resources/freememo/release-assets.json"
[ $# -ge 1 ] || { echo "usage: asset-names.sh <version> [--optional|kind]" >&2; exit 1; }
case "$2" in
  '')         filter='.assets[] | select(.optional | not)' ;;
  --optional) filter='.assets[] | select(.optional)' ;;
  *)          filter='.assets[] | select(.kind == $kind)' ;;
esac
out="$(jq -r --arg v "$1" --arg kind "${2:-}" "$filter | .name | split(\"{version}\") | join(\$v)" "$manifest")"
[ -n "$out" ] || { echo "asset-names.sh: nothing for '${2:-required}'" >&2; exit 1; }
printf '%s\n' "$out"
