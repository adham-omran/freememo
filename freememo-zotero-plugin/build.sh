#!/usr/bin/env bash
# Pack the plugin directory into a .xpi (zip) under dist/.
# Pre:  run from any cwd; manifest.json present in the script's directory.
# Post: dist/freememo-zotero-plugin-<version>.xpi exists with manifest.json
#       at the archive root (required by Zotero / Mozilla loader).
set -euo pipefail
cd "$(dirname "$0")"

NAME=freememo-zotero-plugin
VERSION=$(grep -E '"version"' manifest.json | head -1 | sed -E 's/.*"version"[^"]*"([^"]+)".*/\1/')
if [ -z "$VERSION" ]; then
  echo "build.sh: could not parse version from manifest.json" >&2
  exit 1
fi
OUT="dist/${NAME}-${VERSION}.xpi"
mkdir -p dist
rm -f "$OUT"
zip -qr "$OUT" manifest.json bootstrap.js src \
  -x "*.DS_Store" -x "*/.DS_Store" -x "src/**/.*"
echo "Built $OUT"
