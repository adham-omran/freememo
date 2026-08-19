#!/usr/bin/env bash
# Pack the extension directory into a .zip under dist/, for the Chrome Web Store.
# Pre:  run from any cwd; manifest.json present in the script's directory.
# Post: dist/freememo-web-capture-<version>.zip exists with manifest.json at the
#       archive root, which the store requires.
#
# There is no bundler. Every file is a plain ES module the browser loads
# directly, and the one thing that would have needed a bundler — a PDF library —
# is a hundred lines of pdf.js instead.
set -euo pipefail
cd "$(dirname "$0")"

NAME=freememo-web-capture
VERSION=$(grep -E '"version"' manifest.json | head -1 | sed -E 's/.*"version"[^"]*"([^"]+)".*/\1/')
if [ -z "$VERSION" ]; then
  echo "build.sh: could not parse version from manifest.json" >&2
  exit 1
fi
OUT="dist/${NAME}-${VERSION}.zip"
mkdir -p dist
rm -f "$OUT"
zip -qr "$OUT" manifest.json src \
  -x "*.DS_Store" -x "*/.DS_Store" -x "src/**/.*"
echo "Built $OUT"
