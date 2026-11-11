// The names of the assets on a GitHub release. They are a contract: the update
// feed (ticket 18) reads them, and the workflow checks that each one is
// attached before it publishes the release.
//
// The one list is resources/freememo/release-assets.json. Three readers use it:
// this file, src/freememo/update_feed.clj and tools/release/asset-names.sh (jq,
// for the workflow). release_assets_test checks that they agree. The names:
//
//   freememo-<Release Version>-macos-arm64.dmg     installer, Apple Silicon
//   freememo-<Release Version>-windows-x64.msi     installer, Windows
//   freememo-<Release Version>-linux-x64.deb       installer, Debian and Ubuntu
//   freememo-<Release Version>-app.jar             the app alone: the App Update
//   freememo-<Release Version>-release.json        Runtime, Sync Protocol and jar hash of the commit
//   freememo-<Release Version>-app.jar.sig         optional: the Ed25519 signature of the app jar
//                                                  (ticket 19). The release job attaches it once the
//                                                  signing key exists, and refuses to publish without
//                                                  it from then on.
//
// <Release Version> is the tag name without its "v" (docs/adr/0002), such as
// 26.11.13832.
//
// Command line:
//   node asset-names.mjs <version>        the required names, one per line
//   node asset-names.mjs <version> <kind> the name of one: macos, windows, linux, app, release or sig

import fs from 'node:fs'
import { fileURLToPath } from 'node:url'

const manifest = JSON.parse(fs.readFileSync(
  fileURLToPath(new URL('../../resources/freememo/release-assets.json', import.meta.url)), 'utf8'))

const fill = (entry, version) => entry.name.replace('{version}', version)

export function targetKey () {
  return `${process.platform}-${process.arch}`
}

// target: darwin-arm64, win32-x64, linux-x64, or 'any' for the jar.
export function assetName (version, target, ext) {
  const entry = manifest.assets.find(a => a.target === target && a.ext === ext)
  if (!entry) throw new Error(`no asset name for ${target} ${ext}`)
  return fill(entry, version)
}

export function kindName (version, kind) {
  const entry = manifest.assets.find(a => a.kind === kind)
  if (!entry) throw new Error(`unknown asset kind ${kind}`)
  return fill(entry, version)
}

// The names a release must carry. The optional ones are not here.
export function allAssetNames (version) {
  return manifest.assets.filter(a => !a.optional).map(a => fill(a, version))
}

if (process.argv[1] === fileURLToPath(import.meta.url)) {
  const [version, kind] = process.argv.slice(2)
  if (!/^[0-9]+[.][0-9]+[.][0-9]+$/.test(version || '')) {
    console.error('usage: asset-names.mjs <version> [macos|windows|linux|app|release|sig]')
    process.exit(1)
  }
  if (!kind) console.log(allAssetNames(version).join('\n'))
  else {
    try { console.log(kindName(version, kind)) } catch (e) { console.error(e.message); process.exit(1) }
  }
}
