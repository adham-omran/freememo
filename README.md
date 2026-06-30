# FreeMemo

Incremental reading with AI-generated flashcards. Import PDFs, EPUBs, and web articles, extract key concepts into reviewable cards, and sync with Anki.

**Try it at [freememo.net](https://freememo.net)** — no setup required. Or self-host using the instructions below.

Built with [Hyperfiddle Electric v3](https://github.com/hyperfiddle/electric) (Clojure/ClojureScript) — a reactive framework where client and server code coexist in the same `.cljc` files with automatic WebSocket sync.

## Features

- **Import** — PDFs, EPUBs, web articles (paste or Wikipedia lookup)
- **Live Document** — append photos (camera or library) as PDF pages; HEIC auto-converted to JPEG, rotate before saving, oversized images compressed
- **AI flashcard generation** — OpenAI extracts key concepts into basic and cloze cards
- **OCR** — OpenAI Vision converts PDF pages to editable semantic HTML
- **Rich text editor** — Quill-based editor with highlighting and selection-based card generation
- **Anki sync** — Push/pull cards directly to your Anki collection via AnkiConnect
- **Zotero import** — Pull PDFs (with citation metadata) straight from your local Zotero library via the [FreeMemo for Zotero](./freememo-zotero-plugin/) plugin
- **PDF viewer** — In-browser rendering via PDF.js with zoom and navigation
- **Learn queue** — Priority-first spaced-review ordering (SuperMemo model): due date gates the queue, priority orders it, ties shuffle daily ([details](./docs/learn-queue-ordering.md))
- **Subset review** — Review a chosen subset of cards from any topic
- **Search** — Full-text search across topics and cards
- **Library** — Browse imported documents
- **Contents** — Hierarchical knowledge tree with virtual scrolling
- **Status** — Progress overview across all documents

## Quick Start

### Prerequisites

- **Java 17+**
- **Clojure CLI** ([install guide](https://clojure.org/guides/install_clojure))
- **Docker** (for PostgreSQL)
- **Node.js** (for shadow-cljs)
- **ImageMagick** (optional — only for HEIC photo uploads in Live Documents; conversion shells out to `magick`/`convert`)

### Run

```bash
# Start PostgreSQL
docker compose up -d

# Start dev server (hot reload, port 8080)
clj -M:dev -m dev
```

Open http://localhost:8080.

### Configuration

**AI keys** — FreeMemo uses two AI providers:

- **Card generation** uses OpenAI. Set `OPENAI_API_KEY` server-wide, or let each user enter their own key in **Settings** (per-user BYOK). When a server-wide key is set, the per-user field is hidden and a "Server key" badge is shown.
- **OCR** (scanning PDF pages to text) uses **OpenRouter**, not OpenAI. Set `PLATFORM_OPENROUTER_API_KEY`. Without it, OCR is unavailable even if an OpenAI key is set.

**Sign-in** — `AUTH_MODE` selects the login controls:

- `password` (self-host default) — username/password only; no Google setup needed.
- `google` — Google OAuth only; requires `GOOGLE_CLIENT_ID` / `GOOGLE_CLIENT_SECRET` / `GOOGLE_REDIRECT_URI` (or a `resources/google_client.json` from Google Cloud Console).
- `both` — show both.

For password login, seed the first account with `ADMIN_USER` / `ADMIN_PASSWORD` (created at boot only if absent — see [Self-hosting](#self-hosting-with-docker)).

### Optional: Zotero import

Zotero import is gated by a small Zotero plugin (the `FreeMemo for Zotero` add-on under `freememo-zotero-plugin/`). The plugin runs inside the user's Zotero, exposes a CORS-permissive HTTP API under `/freememo/*` on Zotero's loopback server (port 23119), and streams attachment bytes inline. The FreeMemo browser tab talks to it directly — Zotero data never travels through the FreeMemo server.

To enable end-to-end:

1. Get the `.xpi`. Either:
   - **Download the pre-built `.xpi`** committed in this repo at [`freememo-zotero-plugin/dist/freememo-zotero-plugin-0.1.0.xpi`](https://github.com/adham-omran/freememo/raw/main/freememo-zotero-plugin/dist/freememo-zotero-plugin-0.1.0.xpi) — recommended for most users; or
   - **Build from source**: `cd freememo-zotero-plugin && ./build.sh` (produces `dist/freememo-zotero-plugin-<version>.xpi`).
2. Install in Zotero: Tools → Plugins → gear icon → "Install Plugin From File…", pick the `.xpi`. Restart Zotero.
3. Zotero → Settings → Advanced → enable **"Allow other applications on this computer to communicate with Zotero"**.
4. In FreeMemo: Settings → Zotero → toggle **Enable Zotero import** on. Click **Test Connection** to verify.

Why a plugin: Zotero's built-in API sends no CORS headers, returns `file://` redirects for binary attachments (unreachable from web JS), and silently cancels any "browser-shaped" request as a CSRF mitigation. A Zotero plugin is the smallest layer that solves all three. See `freememo-zotero-plugin/README.md` for the full rationale, endpoint contracts, and dev-proxy workflow.

## Tech Stack

| Layer | Technology |
|-------|-----------|
| Framework | Electric v3 — reactive full-stack Clojure |
| Database | PostgreSQL 16 (next.jdbc + HikariCP + HoneySQL) |
| Build | deps.edn + shadow-cljs |
| PDF rendering | PDF.js 3.11.174 (CDN) |
| Rich text | Quill 2.0.3 (CDN) |
| Logging | Telemere (structured, CLJ + CLJS) |
| AI | OpenAI GPT-5.1 (card generation) + Vision (OCR) |

### Loading indicators

Loading state lives in `freememo.loading` (`Spinner`, `WithLoading`) plus the
`.spinner` CSS class (`index.css`, `@keyframes spin`).

`WithLoading` exists because of an Electric subtlety: a pending `e/Offload` (or
any unresolved `e/server` value) latches to an **empty `e/amb`, not `nil`**, so
the naive `(if (nil? v) loading loaded)` never shows the loading branch.
`WithLoading` captures the resolved value into a client atom via a one-shot
`e/Token`, giving a real `nil → value` transition the UI can branch on.

Two consumption shapes:

- **Value render** — `(loading/WithLoading Thunk Loaded)` renders the spinner
  until `Thunk`'s server value resolves, then `(Loaded value)`. Re-fetch =
  remount (key the call site). See `ocr_compare`, `copy_text`, `bibliography_form`.
- **Token-commit flow** — when a Forms5 commit runs an `e/Offload` whose result
  drives navigation/branching, you can't swap the view to a loading screen: that
  unmounts the `e/for` token body and cancels the in-flight offload. Instead set
  a busy atom inside `(when token …)` (as a `case` test so it evaluates) and
  clear it with `e/on-unmount`; render a spinner overlay from that atom while the
  form stays mounted. See `import_modal`'s `!busy-msg` overlay.

### Tooltips

Tooltips are driven by the `data-tooltip` attribute plus a CSS `:hover` rule
(`index.css` ~line 2041) — no JS, instant on hover, unlike the browser's
delayed `title`.

Elements that open a menu suppress their tooltip while open via
`[data-tooltip][aria-expanded="true"]:hover::after { visibility: hidden }`.
Without it the cursor stays over the trigger after a click, `:hover` remains
true, and the tooltip paints over the open menu. Menu triggers already set
`aria-expanded` (e.g. `pdf_action_dropdowns.cljc`), so any new menu-button
gets the fix for free by setting that attribute.

## Database

PostgreSQL 16 via `docker-compose.yml`. Default connection: `cardmaker:dev@localhost:5432/cardmaker`.

Schema auto-creates on first startup (no migration framework). Configure via environment variables — see below.

## Environment Variables

| Variable | Default | Notes |
|----------|---------|-------|
| `DB_HOST` | `localhost` | |
| `DB_PORT` | `5432` | |
| `DB_NAME` | `cardmaker` | |
| `DB_USER` | `cardmaker` | |
| `DB_PASSWORD` | `dev` (dev) / **required** (prod) | Production boot fails if unset. Set before the first `up` — Postgres fixes it at init. |
| `ENC_KEY_SECRET` | _(none)_ | Session-cookie key derivation. **Required in production** (boot fails if unset). Generate: `openssl rand -hex 32` |
| `AUTH_MODE` | `google` (credits) / `password` (self-host) | Login controls: `password`, `google`, or `both`. |
| `ADMIN_USER` | _(none)_ | Seed username, created at boot if absent. |
| `ADMIN_PASSWORD` | _(none)_ | Seed password. Changing it later does **not** reset an existing account. |
| `COOKIE_SECURE` | `true` | Session-cookie `Secure` attribute. **Set `false` to log in over plain HTTP** (localhost/LAN without TLS). |
| `OPENAI_API_KEY` | _(none)_ | Server-wide OpenAI key for card generation. Falls back to `OPENAI_DEMO_KEY`. |
| `PLATFORM_OPENROUTER_API_KEY` | _(none)_ | OpenRouter key for OCR. Required for document scanning. |
| `OPENAI_DEMO_KEY` | _(none)_ | Backward-compatible alias for `OPENAI_API_KEY`. |
| `LOG_LEVEL` | `info` (prod) / `debug` (dev) | `trace`, `debug`, `info`, `warn`, `error`, `fatal` |
| `PORT` | `8080` | HTTP port |
| `STORAGE_QUOTA_BYTES` | `1073741824` (1 GB) | Total per-user storage. `0` = unlimited. |
| `STORAGE_PER_FILE_MAX_BYTES` | `104857600` (100 MB) | Per-file upload cap. `0` = unlimited. Also drives Jetty body + WebSocket size limits. |
| `APP_BASE_URL` | `https://freememo.net` | Public base URL embedded in Anki card source anchors. Set to your domain when self-hosting. |
| `GOOGLE_CLIENT_ID` | _(none)_ | Google OAuth client ID. Required when `AUTH_MODE` is `google`/`both`. |
| `GOOGLE_CLIENT_SECRET` | _(none)_ | Google OAuth client secret. |
| `GOOGLE_REDIRECT_URI` | `http://localhost:8080/auth/google/callback` | Must match the URI registered in Google Cloud Console. |

Alternative to the three `GOOGLE_*` envs: drop the OAuth client JSON downloaded from Google Cloud Console at `resources/google_client.json` (gitignored).

## Production

### Self-hosting with Docker

Self-host on Linux or macOS with Docker — username/password login, no Google account required.
Everything (Postgres + app) is defined in a single file, `docker-compose.selfhost.yml`.

**1. Configure `.env`:**

```bash
cp .env.example .env
```

Edit `.env` and set, at minimum:

```bash
DB_PASSWORD=$(openssl rand -hex 32)       # set BEFORE first up (Postgres fixes it at init)
ENC_KEY_SECRET=$(openssl rand -hex 32)    # session-cookie key
AUTH_MODE=password                        # username/password login
ADMIN_USER=admin
ADMIN_PASSWORD=your-password-here
COOKIE_SECURE=false                       # required to log in over plain HTTP (see security note)
OPENAI_API_KEY=sk-...                     # card generation
PLATFORM_OPENROUTER_API_KEY=sk-or-...     # OCR (document scanning)
```

**2. Start:**

```bash
APP_VERSION=$(git describe --tags --long --always --dirty 2>/dev/null || echo selfhost) \
  docker compose -f docker-compose.selfhost.yml up -d --build
```

The app refuses to boot if `DB_PASSWORD` or `ENC_KEY_SECRET` is unset.
`APP_VERSION` is optional here (defaults to `selfhost`); it's baked into the build for
client/server version matching, which holds because one build produces both.

**3. Log in** at http://localhost:8080 with your `ADMIN_USER` / `ADMIN_PASSWORD`.
The account is created at boot only if it doesn't already exist — changing `ADMIN_PASSWORD`
later will **not** reset it.

Override the published port with `APP_PORT` (default 8080).
Storage caps default to **unlimited**; set `STORAGE_QUOTA_BYTES` / `STORAGE_PER_FILE_MAX_BYTES` in `.env`.

> **⚠️ Security — plain HTTP over a LAN.** `COOKIE_SECURE=false` is needed to log in
> without HTTPS, but it means the login password and session cookie travel **unencrypted**.
> The app binds `0.0.0.0`, so anything on your network can reach it. Use plain HTTP only on a
> **trusted local network**. For any real or internet-facing deployment, put the app behind an
> HTTPS reverse proxy (see [Reverse Proxy](#reverse-proxy)) and set `COOKIE_SECURE=true`.

### Non-Docker (run the jar on the host)

Requires a reachable Postgres (e.g. `docker compose up` for just the DB) and ImageMagick on PATH
(HEIC upload conversion shells out to `magick`).

```bash
# Build client + uberjar
clj -X:build:prod build-client
clj -J-Xss4m -X:build:prod uberjar :build/jar-name '"app.jar"'

# Run. DB_HOST/PORT/NAME/USER default to localhost / cardmaker; DB_PASSWORD and
# ENC_KEY_SECRET are required (boot fails otherwise).
DB_PASSWORD=secret ENC_KEY_SECRET=$(openssl rand -hex 32) \
  AUTH_MODE=password ADMIN_USER=admin ADMIN_PASSWORD=pw COOKIE_SECURE=false \
  OPENAI_API_KEY=sk-... PLATFORM_OPENROUTER_API_KEY=sk-or-... PORT=9091 \
  java -cp target/app.jar clojure.main -m prod
```

### Reverse Proxy

The app runs on port 8080. For HTTPS, put it behind nginx or Caddy. **WebSocket support is required** — Electric v3 uses persistent WebSocket connections.

nginx essentials:
```nginx
location / {
    proxy_pass http://localhost:8080;
    proxy_http_version 1.1;
    proxy_set_header Upgrade $http_upgrade;
    proxy_set_header Connection "upgrade";
    proxy_read_timeout 300s;
    client_max_body_size 100M;  # must be ≥ STORAGE_PER_FILE_MAX_BYTES (or 0/large when unlimited)
}
```

Caddy (automatic HTTPS):
```
your-domain.com {
    reverse_proxy localhost:8080
}
```

### Backup & Restore

Backup the database:

```bash
docker compose exec postgres pg_dump -U cardmaker cardmaker > backup.sql
```

Restore:

```bash
cat backup.sql | docker compose exec -T postgres psql -U cardmaker cardmaker
```

A remote-pull variant is in `scripts/backup.sh`.

## Geo-IP Database

The app uses [DB-IP Lite Country](https://db-ip.com/db/lite.php) to switch the
Wayl checkout page between IQD and USD display based on the client's country.
The MMDB file lives at `resources/geo/dbip-country-lite.mmdb` and ships inside
the uberjar via `:paths ["src" "resources"]` in `deps.edn`.

DB-IP refreshes the free database monthly. To pull the latest:

```bash
mkdir -p resources/geo && \
  curl -L "https://download.db-ip.com/free/dbip-country-lite-$(date +%Y-%m).mmdb.gz" \
  | gunzip > resources/geo/dbip-country-lite.mmdb
```

Then rebuild the uberjar.

### Attribution

The free IP to Country Lite database by DB-IP is licensed under a Creative
Commons Attribution 4.0 International License. Use is permitted in this
application provided DB-IP.com is credited for the data. The required link on
any page that displays or uses results from the database:

```html
<a href='https://db-ip.com'>IP Geolocation by DB-IP</a>
```

## License

Electric v3 is free for bootstrappers and non-commercial use, and available commercially under a business source license. See [Electric v3 license](https://tana.pub/lQwRvGRaQ7hM/electric-v3-license-change).
