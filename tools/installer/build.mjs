#!/usr/bin/env node
// Build the Local Install installer for the machine that runs this script
// (ticket 17, plans/local-install/issues/17-installers-in-github-actions.md).
// It lives in tools/, which the public-history cleaner keeps, because the
// GitHub Actions workflow of the public repository calls it.
//
//   node tools/installer/build.mjs --version 26.11.13832 --jar target/app.jar
//        [--type dmg|msi|deb|app-image] [--work DIR] [--out DIR] [--cache DIR]
//
// Pre:  --jar is the AOT uberjar, built without the :local-install alias:
//         clojure -J-Xss4m -X:build:prod uberjar :build/jar-name '"app.jar"' \
//           :build/aot true :release-version '"<version>"'
//       JAVA_HOME names a JDK 21 or later that has jmods and jpackage.
//       Windows: 7z and tar on PATH, and WiX for the MSI.
//       macOS: `brew install imagemagick graphviz dylibbundler`.
// Post: --out holds the installer, named by tools/installer/asset-names.mjs.
//
// The target is this machine: linux-x64, darwin-arm64 or win32-x64. jpackage
// cannot build for another OS.
//
// Installer layout (the jpackage input directory):
//   freememo-boot.jar        the bootstrap (tools/installer/bootstrap, ticket 19). It is the jpackage
//                            main jar. It picks the newest verified App Update jar in the data
//                            directory, else freememo.jar, and starts the app as a child JVM.
//                            An App Update never replaces it.
//   freememo.jar             the bundled app. It holds no PostgreSQL.
//   postgres-binaries.jar    zonky PostgreSQL 18.6.0 for this OS (docs/adr/0003). jpackage
//                            puts every jar of the input directory on the classpath, and
//                            zonky finds its binaries there.
//   tools/                   ffmpeg, ffprobe, ImageMagick, Graphviz (freememo.bundled-tools).
//                            Linux has none: the package declares them as dependencies.
// The launcher starts FreememoBoot with -Dfreememo.boot.appdir, -Dfreememo.boot.bundled-version and
// -Dfreememo.update-public-key (tools/installer/app-update-public-key.b64; without that file the
// build leaves App Update off with one log line). The bootstrap starts freememo.local_install with
// -Xss4m, -Dfreememo.tools-dir=$APPDIR/tools and the user's arguments, like --no-tab.

import { execFileSync } from 'node:child_process'
import crypto from 'node:crypto'
import fs from 'node:fs'
import os from 'node:os'
import path from 'node:path'
import { fileURLToPath } from 'node:url'
import { assetName, targetKey } from './asset-names.mjs'

const here = path.dirname(fileURLToPath(import.meta.url))

const PG_VERSION = '18.6.0'
// The zonky binary jar of each target, pinned by SHA-256. To bump: change
// PG_VERSION, run the script, and copy the hash from the error.
const POSTGRES = {
  'linux-x64': { artifact: 'embedded-postgres-binaries-linux-amd64', sha256: '008ee189eaa7ca2b58bd01dd950291e638b998392cf982297acd4779ec898ac1' },
  'darwin-arm64': { artifact: 'embedded-postgres-binaries-darwin-arm64v8', sha256: '8f6ed6f38195b50a40712ada8b2a12060b6193c1a6977a6eb5cff053bb5a1b89' },
  'win32-x64': { artifact: 'embedded-postgres-binaries-windows-amd64', sha256: 'b7ef2d03588e0439f0d0a2250f43905f7464f1e96ace47703ceaf79094198150' }
}
// The extensions that the schema creates.
const EXTENSIONS = ['pgcrypto', 'pg_trgm', 'fuzzystrmatch', 'unaccent']

// The modules the uberjar needs. java.desktop is PDFBox's ImageIO and the tray;
// jdk.crypto.ec is TLS to freememo.net; jdk.unsupported is Netty and friends.
const JRE_MODULES = [
  'java.base', 'java.compiler', 'java.desktop', 'java.instrument', 'java.logging',
  'java.management', 'java.naming', 'java.net.http', 'java.prefs', 'java.scripting',
  'java.security.jgss', 'java.security.sasl', 'java.sql', 'java.transaction.xa',
  'java.xml', 'jdk.crypto.ec', 'jdk.unsupported', 'jdk.zipfs', 'jdk.management',
  'jdk.charsets', 'jdk.localedata'
]

// Package dependencies of the Linux .deb. Linux has no bundled tools: a
// package manager keeps ImageMagick, Graphviz and ffmpeg current and shares
// them with the system. libgvplugin-neato-layout8 holds the sfdp layout engine
// (the Dockerfile explains why), libheif1 the HEIC decoder of ImageMagick.
// Ubuntu 24.04 has no libgvplugin-neato-layout8: its Graphviz 2.42 keeps that
// engine in libgvc6, so the dependency names both. Its ImageMagick is v6, which
// freememo.api reaches as `convert`.
const DEB_DEPENDS = 'ffmpeg, imagemagick, graphviz, libgvplugin-neato-layout8 | libgvc6, libheif1'

// The downloads of a Windows build, pinned. A changed file fails the build.
const WINDOWS_DOWNLOADS = {
  ffmpeg: {
    url: 'https://github.com/GyanD/codexffmpeg/releases/download/9.0.2/ffmpeg-9.0.2-essentials_build.zip',
    sha256: '60f467265b1e312373dbcd92200c2618a74850f98d3d078e94296bb3fa2047ba'
  },
  imagemagick: {
    url: 'https://github.com/ImageMagick/ImageMagick/releases/download/7.1.2-34/ImageMagick-7.1.2-34-portable-Q16-x64.7z',
    sha256: '52cc6a441c7884f5ff4274b410c9b90c4b7d54245dcdf4d32988bb4e4df4e5d1'
  },
  graphviz: {
    url: 'https://gitlab.com/api/v4/projects/4207231/packages/generic/graphviz-releases/16.1.0/windows_10_cmake_Release_Graphviz-16.1.0-win64.zip',
    sha256: '733e49626c492242eb8dca30ea627b6ead20710e207998c7933b4909d92d6abc'
  },
  // The Microsoft Visual C++ 2015-2022 runtime, x64. magick needs vcomp140,
  // and a fresh Windows has none of them, so they go beside it.
  vcRedist: {
    url: 'https://download.visualstudio.microsoft.com/download/pr/990c720a-46ce-4dc7-87b9-c2eb615d34b9/CC0FF0EB1DC3F5188AE6300FAEF32BF5BEEBA4BDD6E8E445A9184072096B713B/VC_redist.x64.exe',
    sha256: 'cc0ff0eb1dc3f5188ae6300faef32bf5beeba4bdd6e8e445a9184072096b713b'
  }
}
const VC_DLLS = ['vcruntime140.dll', 'vcruntime140_1.dll', 'msvcp140.dll', 'vcomp140.dll']

// macOS: static ffmpeg 9.0.2 builds (build 1789931890), pinned by version URL and SHA-256.
// To bump: take the new /download/... path from the redirect of
// https://ffmpeg.martin-riedl.de/redirect/latest/macos/arm64/release/ffmpeg.zip
// and hash both files.
const MAC_FFMPEG = {
  ffmpeg: {
    url: 'https://ffmpeg.martin-riedl.de/download/macos/arm64/1789931890_9.0.2/ffmpeg.zip',
    file: 'ffmpeg-9.0.2-macos-arm64.zip',
    sha256: 'c8ed4c4e6978a03c485edbfe4e0a5dc2380f8a30bba5150531b31b094492d924'
  },
  ffprobe: {
    url: 'https://ffmpeg.martin-riedl.de/download/macos/arm64/1789931890_9.0.2/ffprobe.zip',
    file: 'ffprobe-9.0.2-macos-arm64.zip',
    sha256: 'fcbe839537485eaee7a7a8bc5cbc0f90d53617e80943e8a5b2e31cb851197ea6'
  }
}

// Fixed for ever: Windows uses it to upgrade an older FreeMemo in place.
const WIN_UPGRADE_UUID = '20ce3b4e-2e44-470d-a65f-620e91f41fbb'
const MAC_BUNDLE_ID = 'net.freememo.desktop'

function log (...parts) { console.log('[installer]', ...parts) }
function fail (msg) { console.error(`[installer] ${msg}`); process.exit(1) }
function sh (cmd, args, opts = {}) { return execFileSync(cmd, args, { stdio: 'inherit', ...opts }) }
function out (cmd, args) { return execFileSync(cmd, args, { encoding: 'utf8' }) }
function which (name) {
  try { return out(process.platform === 'win32' ? 'where' : 'which', [name]).split(/\r?\n/)[0].trim() || null } catch { return null }
}
function arg (name, dflt) {
  const i = process.argv.indexOf(`--${name}`)
  return i >= 0 ? process.argv[i + 1] : dflt
}

let cacheDir

async function download (url, file) {
  if (fs.existsSync(file)) return file
  log(`downloading ${url}`)
  const res = await fetch(url)
  if (!res.ok) throw new Error(`${url}: HTTP ${res.status}`)
  fs.mkdirSync(path.dirname(file), { recursive: true })
  fs.writeFileSync(file + '.part', Buffer.from(await res.arrayBuffer()))
  fs.renameSync(file + '.part', file)
  return file
}

function checkHash (file, sha256) {
  const actual = crypto.createHash('sha256').update(fs.readFileSync(file)).digest('hex')
  if (actual !== sha256) {
    throw new Error(`${file} has sha256 ${actual}, but build.mjs pins ${sha256}. Delete the file to download it again.`)
  }
}

// Post: the file of `entry` in the cache, whose SHA-256 is entry.sha256.
async function pinned (entry) {
  const file = path.join(cacheDir, entry.file || path.basename(new URL(entry.url).pathname))
  await download(entry.url, file)
  checkHash(file, entry.sha256)
  return file
}

// Post: the archive unpacked into a new, empty directory. 7z reads zip, jar and 7z on every OS.
function unpack (archive, into) {
  fs.rmSync(into, { recursive: true, force: true })
  fs.mkdirSync(into, { recursive: true })
  if (/\.(zip|jar|7z|txz)$/.test(archive)) sh('7z', ['x', '-y', `-o${into}`, archive], { stdio: ['ignore', 'ignore', 'inherit'] })
  else sh('tar', ['-xf', archive, '-C', into])
  return into
}

function onlyChildName (dir) {
  const entries = fs.readdirSync(dir).filter(f => !f.startsWith('.'))
  if (entries.length !== 1) throw new Error(`${dir} holds ${entries.length} entries, not one`)
  return entries[0]
}

function onlyChild (dir) { return path.join(dir, onlyChildName(dir)) }

// Post: input/postgres-binaries.jar, and the bundle holds the four extensions.
async function postgres (target, input, work) {
  const p = POSTGRES[target]
  const jar = await download(
    `https://repo1.maven.org/maven2/io/zonky/test/postgres/${p.artifact}/${PG_VERSION}/${p.artifact}-${PG_VERSION}.jar`,
    path.join(cacheDir, `${p.artifact}-${PG_VERSION}.jar`))
  checkHash(jar, p.sha256)
  const unpacked = unpack(jar, path.join(work, 'pg-jar'))
  const txz = fs.readdirSync(unpacked).find(f => f.endsWith('.txz'))
  if (!txz) throw new Error(`${jar} holds no .txz`)
  // List it with 7z, not tar. On the Windows runner `tar` is Git's GNU tar, which reads
  // "D:\..." as host "D" and fails. 7z prints backslashes on Windows, so the test takes both.
  // zonky's Windows bundle keeps extensions in share/extension, the others in share/postgresql/extension.
  const tarDir = unpack(path.join(unpacked, txz), path.join(work, 'pg-tar'))
  const listing = out('7z', ['l', '-ttar', path.join(tarDir, onlyChildName(tarDir))])
  for (const ext of EXTENSIONS) {
    if (!new RegExp(`share[/\\\\](postgresql[/\\\\])?extension[/\\\\]${ext}\\.control`).test(listing)) {
      throw new Error(`the PostgreSQL bundle lacks the ${ext} extension`)
    }
  }
  fs.copyFileSync(jar, path.join(input, 'postgres-binaries.jar'))
  log(`postgres ${PG_VERSION}: ok`)
}

function jlinkRuntime (work) {
  const javaHome = process.env.JAVA_HOME
  const jlink = javaHome ? path.join(javaHome, 'bin', 'jlink') : which('jlink')
  if (!jlink) throw new Error('jlink not found. Set JAVA_HOME to a JDK 21 or later.')
  const dest = path.join(work, 'jre')
  fs.rmSync(dest, { recursive: true, force: true })
  sh(jlink, ['--add-modules', JRE_MODULES.join(','), '--strip-debug', '--no-man-pages',
    '--no-header-files', '--compress=zip-6', '--output', dest])
  log('jre: ok')
  return dest
}

// ── Windows tools ──────────────────────────────────────────────────

// Post: dirs hold VC_DLLS from the pinned VC++ redistributable. It is a WiX
// bundle: a run of cabinets is appended to the exe, and one of them holds
// each DLL as `<name>_amd64`.
async function vcRuntime (dirs, work) {
  const exe = await pinned(WINDOWS_DOWNLOADS.vcRedist)
  const outDir = path.join(work, 'vc_redist')
  fs.rmSync(outDir, { recursive: true, force: true })
  fs.mkdirSync(outDir, { recursive: true })
  const buf = fs.readFileSync(exe)
  const magic = Buffer.from('MSCF\0\0\0\0', 'binary')
  let n = 0
  for (let at = buf.indexOf(magic); at >= 0; at = buf.indexOf(magic, at + 1)) {
    const cab = path.join(outDir, `container-${n}.cab`)
    fs.writeFileSync(cab, buf.subarray(at))
    try { execFileSync('7z', ['x', '-y', `-o${path.join(outDir, `container-${n}`)}`, cab], { stdio: 'ignore' }) } catch { /* not a payload cabinet */ }
    n++
  }
  const found = new Map()
  const walk = dir => {
    for (const f of fs.readdirSync(dir, { withFileTypes: true })) {
      const p = path.join(dir, f.name)
      if (f.isDirectory()) { walk(p); continue }
      if (found.size < VC_DLLS.length && !/\.(cab|dll|exe|msi|xml)$/i.test(f.name)) {
        try {
          const list = execFileSync('7z', ['l', p], { encoding: 'utf8', stdio: ['ignore', 'pipe', 'ignore'] })
          if (VC_DLLS.every(d => list.includes(`${d}_amd64`))) {
            const inner = path.join(outDir, 'runtime')
            execFileSync('7z', ['x', '-y', `-o${inner}`, p, ...VC_DLLS.map(d => `${d}_amd64`)], { stdio: 'ignore' })
            for (const d of VC_DLLS) found.set(d, path.join(inner, `${d}_amd64`))
          }
        } catch { /* not a cabinet */ }
      }
    }
  }
  walk(outDir)
  if (found.size !== VC_DLLS.length) throw new Error(`${exe} holds no cabinet with ${VC_DLLS.join(', ')}`)
  for (const dir of dirs) for (const [name, file] of found) fs.copyFileSync(file, path.join(dir, name))
  log('vc runtime: ok')
}

// Layout: tools/bin/ffmpeg.exe and ffprobe.exe, tools/imagemagick/magick.exe and its files,
// tools/graphviz/ (sfdp.exe, its DLLs, plugins and config8).
async function windowsTools (tools, work) {
  const bin = path.join(tools, 'bin')
  fs.mkdirSync(bin, { recursive: true })
  const ffmpegBin = path.join(onlyChild(unpack(await pinned(WINDOWS_DOWNLOADS.ffmpeg), path.join(work, 'u-ffmpeg'))), 'bin')
  for (const name of ['ffmpeg.exe', 'ffprobe.exe']) fs.copyFileSync(path.join(ffmpegBin, name), path.join(bin, name))

  const magick = path.join(tools, 'imagemagick')
  fs.cpSync(unpack(await pinned(WINDOWS_DOWNLOADS.imagemagick), path.join(work, 'u-magick')), magick, { recursive: true })
  if (!fs.existsSync(path.join(magick, 'magick.exe'))) throw new Error('the ImageMagick archive holds no magick.exe')
  // compare.exe, convert.exe and the others are 32 MB copies of magick.exe. The server calls only magick.
  for (const f of fs.readdirSync(magick)) {
    if ((f.endsWith('.exe') && f !== 'magick.exe') || f === 'ChangeLog.md') fs.rmSync(path.join(magick, f))
  }

  const graphviz = path.join(tools, 'graphviz')
  fs.cpSync(path.join(onlyChild(unpack(await pinned(WINDOWS_DOWNLOADS.graphviz), path.join(work, 'u-graphviz'))), 'bin'), graphviz, { recursive: true })
  if (!fs.existsSync(path.join(graphviz, 'sfdp.exe'))) throw new Error('the Graphviz archive holds no sfdp.exe')
  await vcRuntime([magick], work)
  log('windows tools: ok')
}

// ── macOS tools ────────────────────────────────────────────────────

// Layout: tools/bin/{ffmpeg,ffprobe,magick,sfdp,libs/}, tools/imagemagick/{lib,etc,share},
// tools/graphviz/ (the layout plugins). magick and sfdp come from Homebrew, and
// dylibbundler copies the libraries they load into tools/bin/libs.
// NOT VERIFIED on a Mac by the author. The first CI run is the first test.
async function macTools (tools, work) {
  const bin = path.join(tools, 'bin')
  fs.mkdirSync(bin, { recursive: true })
  for (const name of ['ffmpeg', 'ffprobe']) {
    // unpack empties its target, so each zip gets its own directory, not bin.
    const from = path.join(unpack(await pinned(MAC_FFMPEG[name]), path.join(work, `u-${name}`)), name)
    if (!fs.existsSync(from)) throw new Error(`${MAC_FFMPEG[name].file} holds no ${name}`)
    fs.copyFileSync(from, path.join(bin, name))
    fs.chmodSync(path.join(bin, name), 0o755)
  }
  const bundler = which('dylibbundler')
  if (!bundler) throw new Error('dylibbundler not found. Run: brew install dylibbundler')
  const bundle = file => sh(bundler, ['-cd', '-of', '-b', '-x', file, '-d', path.join(bin, 'libs'), '-p', '@executable_path/libs/'])
  for (const name of ['magick', 'sfdp']) {
    const found = which(name)
    if (!found) throw new Error(`${name} not found. Run: brew install imagemagick graphviz`)
    fs.copyFileSync(fs.realpathSync(found), path.join(bin, name))
    fs.chmodSync(path.join(bin, name), 0o755)
    bundle(path.join(bin, name))
  }
  const prefix = (formula) => out('brew', ['--prefix', formula]).trim()
  // ImageMagick's coders (HEIC among them) are modules under lib/ and its configuration under etc/.
  const im = prefix('imagemagick')
  const imDest = path.join(tools, 'imagemagick')
  for (const sub of ['lib', 'etc', 'share']) {
    for (const entry of fs.existsSync(path.join(im, sub)) ? fs.readdirSync(path.join(im, sub)) : []) {
      if (/^ImageMagick/.test(entry)) fs.cpSync(path.join(im, sub, entry), path.join(imDest, sub, entry), { recursive: true, dereference: true })
    }
  }
  for (const so of out('find', [imDest, '-name', '*.so', '-o', '-name', '*.dylib']).split('\n').filter(Boolean)) bundle(so)
  // sfdp finds its layout plugins through GVBINDIR.
  const gv = path.join(prefix('graphviz'), 'lib', 'graphviz')
  fs.cpSync(gv, path.join(tools, 'graphviz'), { recursive: true, dereference: true })
  // The plugins load libgvc and friends from the Homebrew prefix. Point them at bin/libs, as sfdp is.
  for (const dylib of out('find', [path.join(tools, 'graphviz'), '-name', '*.dylib']).split('\n').filter(Boolean)) bundle(dylib)
  log('mac tools: ok')
}

// ── jpackage ───────────────────────────────────────────────────────

function macIcon (work) {
  const png = path.join(here, '..', '..', 'resources', 'public', 'freememo', 'icon-512.png')
  const set = path.join(work, 'icon.iconset')
  fs.rmSync(set, { recursive: true, force: true })
  fs.mkdirSync(set, { recursive: true })
  for (const [size, name] of [[16, 'icon_16x16'], [32, 'icon_16x16@2x'], [32, 'icon_32x32'], [64, 'icon_32x32@2x'],
    [128, 'icon_128x128'], [256, 'icon_128x128@2x'], [256, 'icon_256x256'], [512, 'icon_256x256@2x'], [512, 'icon_512x512']]) {
    sh('sips', ['-z', String(size), String(size), png, '--out', path.join(set, `${name}.png`)], { stdio: 'ignore' })
  }
  const icns = path.join(work, 'FreeMemo.icns')
  sh('iconutil', ['-c', 'icns', set, '-o', icns])
  return icns
}

const PUBLIC_KEY_FILE = path.join(here, 'app-update-public-key.b64')

// Post: the public key (base64 X.509 DER, one line) that the bootstrap trusts, or null.
function updatePublicKey () {
  if (!fs.existsSync(PUBLIC_KEY_FILE)) {
    log('App Update is off: tools/installer/app-update-public-key.b64 does not exist yet (run the wizard, ticket 19)')
    return null
  }
  const key = fs.readFileSync(PUBLIC_KEY_FILE, 'utf8').replace(/\s+/g, '')
  if (!/^[A-Za-z0-9+/=]+$/.test(key)) fail('app-update-public-key.b64 is not one line of base64')
  return key
}

// Pre:  javac and jar of JAVA_HOME (or PATH).
// Post: input/freememo-boot.jar holds the bootstrap.
function bootstrap (input, work) {
  const bin = process.env.JAVA_HOME ? path.join(process.env.JAVA_HOME, 'bin') : null
  const tool = name => bin ? path.join(bin, name) : name
  const classes = path.join(work, 'boot-classes')
  fs.mkdirSync(classes, { recursive: true })
  sh(tool('javac'), ['-d', classes, path.join(here, 'bootstrap', 'FreememoBoot.java')])
  sh(tool('jar'), ['--create', '--file', path.join(input, 'freememo-boot.jar'), '--main-class', 'FreememoBoot', '-C', classes, '.'])
}

function jpackageArgs ({ target, type, version, input, runtime, dest, work, publicKey }) {
  const publicDir = path.join(here, '..', '..', 'resources', 'public')
  const common = [
    '--type', type, '--name', 'FreeMemo', '--app-version', version, '--vendor', 'FreeMemo',
    '--description', 'FreeMemo Local: your flashcard library on this computer',
    '--input', input, '--main-jar', 'freememo-boot.jar', '--main-class', 'FreememoBoot',
    '--java-options', '-Xss4m',
    '--java-options', '-Dfreememo.tools-dir=$APPDIR/tools',
    '--java-options', '-Dfreememo.boot.appdir=$APPDIR',
    '--java-options', `-Dfreememo.boot.bundled-version=${version}`,
    ...(publicKey ? ['--java-options', `-Dfreememo.update-public-key=${publicKey}`] : []),
    '--runtime-image', runtime, '--dest', dest
  ]
  if (target === 'darwin-arm64') {
    return [...common, '--icon', macIcon(work), '--mac-package-name', 'FreeMemo', '--mac-package-identifier', MAC_BUNDLE_ID,
      // The tray makes a template image on macOS. See freememo.local-tray.
      '--java-options', '-Dapple.awt.enableTemplateImages=true']
  }
  if (target === 'win32-x64') {
    return [...common, '--icon', path.join(publicDir, 'favicon.ico'), '--win-menu', '--win-shortcut',
      '--win-per-user-install', '--win-upgrade-uuid', WIN_UPGRADE_UUID]
  }
  const icon = ['--icon', path.join(publicDir, 'freememo', 'icon-512.png')]
  // An app-image (a local check, no package tool) takes none of the package options.
  if (type === 'app-image') return [...common, ...icon]
  return [...common, ...icon, '--linux-package-name', 'freememo',
    '--linux-shortcut', '--linux-app-category', 'Education', '--linux-deb-maintainer', 'FreeMemo <support@freememo.net>',
    '--linux-package-deps', DEB_DEPENDS]
}

async function main () {
  const target = targetKey()
  if (!POSTGRES[target]) fail(`no installer for ${target}`)
  const version = arg('version')
  if (!version || !/^\d+\.\d+\.\d+$/.test(version)) fail('--version <Release Version, like 26.11.13832> is required')
  const jar = path.resolve(arg('jar', 'target/app.jar'))
  if (!fs.existsSync(jar)) fail(`${jar} is missing. See the header of this script for the uberjar command.`)
  const dflt = { 'linux-x64': 'deb', 'darwin-arm64': 'dmg', 'win32-x64': 'msi' }[target]
  const type = arg('type', dflt)
  const work = path.resolve(arg('work', 'target/installer'))
  const outDir = path.resolve(arg('out', path.join(work, 'out')))
  cacheDir = path.resolve(arg('cache', process.env.FREEMEMO_INSTALLER_CACHE || path.join(os.homedir(), '.cache', 'freememo-installer')))
  const input = path.join(work, 'input')
  fs.rmSync(work, { recursive: true, force: true })
  fs.mkdirSync(input, { recursive: true })
  fs.mkdirSync(outDir, { recursive: true })

  fs.copyFileSync(jar, path.join(input, 'freememo.jar'))
  bootstrap(input, work)
  const publicKey = updatePublicKey()
  await postgres(target, input, work)
  const tools = path.join(input, 'tools')
  if (target === 'win32-x64') await windowsTools(tools, work)
  else if (target === 'darwin-arm64') await macTools(tools, work)
  else fs.mkdirSync(tools) // Linux: the package declares its tools. The directory keeps $APPDIR/tools valid.
  const runtime = jlinkRuntime(work)

  const javaHome = process.env.JAVA_HOME
  const jpackage = javaHome ? path.join(javaHome, 'bin', 'jpackage') : which('jpackage')
  if (!jpackage) fail('jpackage not found. Set JAVA_HOME to a JDK 21 or later.')
  const dest = path.join(work, 'jpackage-out')
  sh(jpackage, jpackageArgs({ target, type, version, input, runtime, dest, work, publicKey }))

  // The installer takes its fixed asset name. An app-image (a local check) is a directory.
  const made = fs.readdirSync(dest).find(f => type === 'app-image' ? true : f.endsWith(`.${type}`))
  if (!made) fail(`jpackage made no .${type} in ${dest}`)
  const finalName = type === 'app-image' ? 'FreeMemo-app-image' : assetName(version, target, type)
  fs.rmSync(path.join(outDir, finalName), { recursive: true, force: true })
  fs.renameSync(path.join(dest, made), path.join(outDir, finalName))
  if (type !== 'app-image') {
    fs.copyFileSync(jar, path.join(outDir, assetName(version, 'any', 'jar')))
  }
  log(`built ${path.join(outDir, finalName)}`)
}

main().catch(err => { console.error(err.message); process.exit(1) })
