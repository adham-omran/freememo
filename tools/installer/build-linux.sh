#!/bin/sh
# Build the Linux Local Install on this computer, and start it headless.
# One command, from the repository root:
#
#   tools/installer/build-linux.sh
#
# It builds the AOT uberjar (the prod client build is part of it, so it is heavy:
# about 4 GB of RAM), then runs tools/installer/build.mjs, then smoke.sh.
# Package type: PACKAGE_TYPE, else `deb` when dpkg-deb exists, else `app-image`
# (a directory that runs in place; the check starts it from there).
# Environment:
#   FREEMEMO_INSTALLER_CACHE  where downloads go (default ~/.cache/freememo-installer)
#   INSTALLER_WORK            build directory (default target/installer)
#   RELEASE_VERSION           default: the Release Version of HEAD
#   SKIP_UBERJAR=1            reuse target/app.jar
# Needs: clojure, node 20+, a JDK 21+ with jpackage and jlink, 7z, tar.
set -e
cd "$(dirname "$0")/../.."
work="${INSTALLER_WORK:-target/installer}"
rv="${RELEASE_VERSION:-$(tools/release/release-version.sh)}"
type="${PACKAGE_TYPE:-}"
if [ -z "$type" ]; then
  if command -v dpkg-deb >/dev/null 2>&1; then type=deb; else type=app-image; fi
fi

if [ "${SKIP_UBERJAR:-}" != 1 ]; then
  clojure -X:build vendor/fetch!
  clojure -J-Xss4m -X:build:prod uberjar :build/jar-name '"app.jar"' :build/aot true \
    :release-version "\"$rv\""
fi

# jlink and jpackage come from Temurin, as in CI. A distribution JDK can fail:
# Fedora's jlink stops with "java.security has been modified". With no JAVA_HOME,
# the script fetches Temurin 25.0.5+8 once into the cache, and checks its
# SHA-256 (the value that Adoptium publishes beside the file). To bump the JDK,
# change both lines below. USE_SYSTEM_JDK=1 skips that.
JDK_URL="https://github.com/adoptium/temurin25-binaries/releases/download/jdk-25.0.5%2B8/OpenJDK25U-jdk_x64_linux_hotspot_25.0.5_8.tar.gz"
JDK_SHA256="68982529869257d0ee6b824ec5aa270ff8b118bb438670da0b97ebdab178d7fb"
if [ -z "${JAVA_HOME:-}" ] && [ "${USE_SYSTEM_JDK:-}" != 1 ]; then
  jdkdir="${FREEMEMO_INSTALLER_CACHE:-$HOME/.cache/freememo-installer}/jdk"
  if [ ! -d "$jdkdir" ] || [ -z "$(ls "$jdkdir")" ]; then
    mkdir -p "$jdkdir"
    tgz="$jdkdir/.download.tar.gz"
    curl -fsSL -o "$tgz" "$JDK_URL"
    echo "$JDK_SHA256  $tgz" | sha256sum -c - || { rm -f "$tgz"; echo "the JDK download does not match the pinned SHA-256" >&2; exit 1; }
    tar -xz -C "$jdkdir" -f "$tgz"
    rm -f "$tgz"
  fi
  JAVA_HOME="$(echo "$jdkdir"/jdk-*)"
  export JAVA_HOME
fi

node tools/installer/build.mjs --version "$rv" --jar target/app.jar --type "$type" --work "$work"

case "$type" in
  app-image) tools/installer/smoke.sh "$work/out/FreeMemo-app-image/bin/FreeMemo" ;;
  deb) echo "Built $work/out. Install with: sudo apt install ./$work/out/freememo-$rv-linux-x64.deb, then run tools/installer/smoke.sh /opt/freememo/bin/FreeMemo" ;;
esac
