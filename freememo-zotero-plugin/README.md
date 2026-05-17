# FreeMemo for Zotero

A Zotero 7/8/9 plugin that exposes a small HTTP API for the FreeMemo web app.
Adds endpoints under `/freememo/` on Zotero's loopback server
(`127.0.0.1:23119`) with permissive CORS headers for `https://freememo.net`
and local development origins.

This plugin is the boundary that lets the FreeMemo browser tab read your
local Zotero library and attachments without routing your data through
FreeMemo's server.

## Why a plugin

Three Zotero behaviors stop regular page-origin JavaScript from reaching
the local library directly — a plugin is the smallest surface that
sidesteps all three:

1. **No CORS headers on built-in endpoints.** Zotero core only emits
   `Access-Control-Allow-Origin` for its own bookmarklet origin. Plugin
   handlers control their full response, so we emit per-origin
   `Access-Control-Allow-Origin` (plus `Access-Control-Allow-Private-Network`
   for Chrome's PNA rule when an HTTPS origin fetches a private host).
2. **`file://` redirects for attachment bytes.** `GET /api/users/.../file`
   replies `302 Location: file:///…`. Browsers refuse `file://` redirects
   from `http(s)://` origins. The plugin reads the bytes off disk via
   `Zotero.File.getBinaryContentsAsync` and returns them inline, base64-
   encoded (Zotero's response writer is UTF-8-only).
3. **Anti-CSRF guard that silently cancels browser-shaped requests.**
   Zotero's HTTP server has a `_processEndpoint` check that closes the
   connection (`Preventing request from browser`) for any request whose
   `User-Agent` starts with `Mozilla/` or carries an `Origin` header,
   unless the endpoint opts in via `this.allowRequestsFromUnsafeWebContent
   = true`. Our endpoints set that flag. The real security boundary moves
   into the handler — `src/server.js:originAllowed` rejects any Origin not
   in the configured allowlist, and the per-origin
   `Access-Control-Allow-Origin` ensures rogue pages cannot read replies
   even if they got through.

## Endpoints

All endpoints respond `Access-Control-Allow-Origin: <origin>` for origins
matching `https://freememo.net` or any `http://localhost:*` /
`http://127.0.0.1:*`. Wrong origin → `403 {error:"origin-not-allowed"}`.

- `GET /freememo/probe` — `{ok, plugin_version, zotero_version, library_id}`
- `GET /freememo/items/top?limit=&start=` — paged top-level library entries
- `GET /freememo/items/:key/csljson` — CSL-JSON for a single item
- `GET /freememo/items/:key/children/pdfs` — PDF children of an item
- `GET /freememo/items/:key/file` — `{filename, content_type, size, base64}`
  (base64 because Zotero's response writer is UTF-8-only and cannot stream
  binary verbatim; the 33% bloat is one-shot per import)

## Build

```sh
./build.sh
```

Produces `dist/freememo-zotero-plugin-<version>.xpi`.

## Install — release flow

1. Download the `.xpi` from the latest GitHub Release.
2. Zotero → Tools → Plugins → gear icon → "Install Plugin From File…".
3. Select the `.xpi`. Restart Zotero if prompted.

Zotero → Settings → Advanced → enable
**"Allow other applications on this computer to communicate with Zotero"**
if not already on.

## Install — dev / proxy flow

Drop an extension proxy file pointing at this directory so Zotero loads the
source live:

```sh
# Replace <PROFILE> with the actual profile dir name under Profiles/.
PROFILE_DIR="$HOME/Library/Application Support/Zotero/Profiles/<PROFILE>"
mkdir -p "$PROFILE_DIR/extensions"
printf '%s\n' "$PWD/freememo-zotero-plugin/" > "$PROFILE_DIR/extensions/freememo@freememo.net"
```

Restart Zotero with caches purged so the proxy is picked up:

```sh
/Applications/Zotero.app/Contents/MacOS/zotero -purgecaches -jsconsole
```

The `-jsconsole` flag opens the JS console — `Zotero.debug("...")` calls
from this plugin appear there (and in Help → Debug Output Logging).

## Verify

From a tab at `https://freememo.net` (production) or
`http://localhost:8080` (Electric dev server):

```js
fetch("http://127.0.0.1:23119/freememo/probe")
  .then(r => r.json())
  .then(console.log);
// → { ok: true, plugin_version: "0.1.0", zotero_version: "9.0.3", library_id: 1 }
```

If CORS blocks the response:
- Confirm "Allow other applications…" is enabled in Zotero Settings → Advanced.
- Confirm the plugin shows in Tools → Plugins.
- Open Zotero's Debug Output Logging and look for `FreeMemo: registered /freememo/probe`.

If you see `Preventing request from browser` in Zotero's debug log:
- The endpoint is missing `this.allowRequestsFromUnsafeWebContent = true`
  in its constructor. Every handler class in `src/endpoints/` sets this
  flag; new endpoints must do the same or Zotero will silently drop
  browser-originated requests.

If the response is `{ error: "origin-not-allowed", got: ... }`:
- Your browser origin isn't in the allowlist. Production allows
  `https://freememo.net`; dev allows any `http://localhost:*` or
  `http://127.0.0.1:*`. Edit `ALLOWED_ORIGINS` or `LOCAL_ORIGIN_RE` in
  `src/server.js` to extend.

## Layout

```
freememo-zotero-plugin/
├── manifest.json                plugin metadata (Zotero 7+ format)
├── bootstrap.js                 lifecycle hooks (startup, shutdown, …)
├── src/
│   ├── server.js                register/unregister, CORS, origin allowlist
│   └── endpoints/
│       ├── Probe.js             GET /freememo/probe
│       ├── ItemsTop.js          GET /freememo/items/top
│       ├── ItemCsljson.js       GET /freememo/items/:key/csljson
│       ├── ItemPdfChildren.js   GET /freememo/items/:key/children/pdfs
│       └── ItemFile.js          GET /freememo/items/:key/file
└── build.sh                     pack into dist/*.xpi
```
