// The names of the assets on a GitHub release. They are a contract: the update
// feed (ticket 18) reads them, and the workflow checks that each one is
// attached before it publishes the release.
//
//   freememo-<Release Version>-macos-arm64.dmg     installer, Apple Silicon
//   freememo-<Release Version>-windows-x64.msi     installer, Windows
//   freememo-<Release Version>-linux-x64.deb       installer, Debian and Ubuntu
//   freememo-<Release Version>-app.jar             the app alone: the App Update
//   freememo-<Release Version>-release.json        Runtime, Sync Protocol and jar hash of the commit
//
// Optional, not listed: freememo-<Release Version>-app.jar.sig, the Ed25519 signature of the app
// jar (ticket 19; `sigAssetName`). The release job attaches it once the signing key exists, and
// refuses to publish without it from then on.
// The update feed (src/freememo/update_feed.clj) keeps its own list; update_feed_test
// compares the two.
//
// <Release Version> is the tag name without its "v" (docs/adr/0002), such as
// 26.11.13832. The same pattern as a regular expression:
//   ^freememo-([0-9]+[.][0-9]+[.][0-9]+)-(macos-arm64[.]dmg|windows-x64[.]msi|linux-x64[.]deb|app[.]jar|release[.]json)$
//
// Command line, for the workflow:
//   node asset-names.mjs <version>        the five names, one per line
//   node asset-names.mjs <version> <os>   the name of one: macos, windows, linux, app or release

import { fileURLToPath } from 'node:url'

const OS = { 'darwin-arm64': 'macos-arm64', 'win32-x64': 'windows-x64', 'linux-x64': 'linux-x64' }

export function targetKey () {
  return `${process.platform}-${process.arch}`
}

// target: darwin-arm64, win32-x64, linux-x64, or 'any' for the jar.
export function assetName (version, target, ext) {
  if (target === 'any') return `freememo-${version}-app.jar`
  if (!OS[target]) throw new Error(`no asset name for ${target}`)
  return `freememo-${version}-${OS[target]}.${ext}`
}

export function sigAssetName (version) {
  return `freememo-${version}-app.jar.sig`
}

export function allAssetNames (version) {
  return [
    assetName(version, 'darwin-arm64', 'dmg'),
    assetName(version, 'win32-x64', 'msi'),
    assetName(version, 'linux-x64', 'deb'),
    assetName(version, 'any', 'jar'),
    `freememo-${version}-release.json`
  ]
}

if (process.argv[1] === fileURLToPath(import.meta.url)) {
  const [version, os] = process.argv.slice(2)
  if (!/^[0-9]+[.][0-9]+[.][0-9]+$/.test(version || '')) {
    console.error('usage: asset-names.mjs <version> [macos|windows|linux|app|release|sig]')
    process.exit(1)
  }
  const all = allAssetNames(version)
  if (!os) console.log(all.join('\n'))
  else if (os === 'sig') console.log(sigAssetName(version))
  else {
    const hit = all.find(n => n.endsWith(os === 'app' ? '-app.jar' : os === 'release' ? '-release.json' : '') && (os === 'app' || os === 'release' || n.includes(`-${os}-`)))
    if (!hit) { console.error(`unknown asset kind ${os}`); process.exit(1) }
    console.log(hit)
  }
}
