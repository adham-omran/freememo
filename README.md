# FreeMemo

Incremental reading with AI-generated flashcards. Import PDFs, EPUBs, and web articles, extract key concepts into reviewable cards, and sync with Anki.

**Try it at [freememo.net](https://freememo.net)** -- no setup required. Or self-host using the instructions below.

Built with [Hyperfiddle Electric v3](https://github.com/hyperfiddle/electric) (Clojure/ClojureScript) -- a reactive framework where client and server code coexist in the same `.cljc` files with automatic WebSocket sync.

## Features

- **Import** -- PDFs, EPUBs, web articles (paste or Wikipedia lookup)
- **AI flashcard generation** -- OpenAI extracts key concepts into basic and cloze cards
- **OCR** -- OpenAI Vision converts PDF pages to editable semantic HTML
- **Rich text editor** -- Quill-based editor with highlighting and selection-based card generation
- **Anki sync** -- Push/pull cards directly to your Anki collection via AnkiConnect
- **PDF viewer** -- In-browser rendering via PDF.js with zoom and navigation

## Quick Start

### Prerequisites

- **Java 17+**
- **Clojure CLI** ([install guide](https://clojure.org/guides/install_clojure))
- **Docker** (for PostgreSQL)
- **Node.js** (for shadow-cljs)

### Run

```bash
# Start PostgreSQL
docker compose up -d

# Start dev server (hot reload, port 8080)
clj -M:dev -m dev
```

Open http://localhost:8080. The nREPL server starts on port 9009.

### Configuration

Navigate to **Settings** in the UI and enter your OpenAI API key. All settings are persisted per-user in the database.

## Tech Stack

| Layer | Technology |
|-------|-----------|
| Framework | Electric v3 -- reactive full-stack Clojure |
| Database | PostgreSQL 16 (next.jdbc + HikariCP + HoneySQL) |
| Build | deps.edn + shadow-cljs |
| PDF rendering | PDF.js 3.11.174 (CDN) |
| Rich text | Quill 1.3.7 (CDN) |
| Logging | Telemere (structured, CLJ + CLJS) |
| AI | OpenAI GPT-5.1 (card generation) + Vision (OCR) |

## Project Structure

```
src/freememo/
  main.cljc              -- Root component, tab routing, auth gate
  login_page.cljc        -- Login/signup (Google OAuth + username/password)
  settings_page.cljc     -- API key, model, verbosity (auto-save)
  import_page.cljc       -- Paste/Wikipedia import
  pdf_page.cljc          -- PDF upload + document list
  ocr_page.cljc          -- Main workspace: PDF viewer + editor + card table
  content_toolbar.cljc   -- Generation controls: card type, count, generate, export
  learn_page.cljc        -- Review sessions
  rich_text_editor.cljc  -- Quill integration (global singleton)
  pdf_viewer.cljc        -- PDF.js integration
  keyboard.cljc          -- Global keyboard shortcuts
  anki_sync*.cljc        -- Anki sync (connect, push, pull)
  db.clj                 -- PostgreSQL schema, queries, connection pool
  cards.clj              -- OpenAI card generation + prompt templates
  ocr.clj                -- PDFBox rendering + OpenAI Vision OCR
  api.clj                -- Ring routes (upload, PDF serving)
  settings.clj           -- Per-user key-value settings
```

## Database

PostgreSQL 16 via `docker-compose.yml`. Default connection: `cardmaker:dev@localhost:5432/cardmaker`.

Schema auto-creates on first startup (no migration framework). Configure via environment variables for production:

| Variable | Default |
|----------|---------|
| `DB_HOST` | `localhost` |
| `DB_PORT` | `5432` |
| `DB_NAME` | `cardmaker` |
| `DB_USER` | `cardmaker` |
| `DB_PASSWORD` | `dev` |

## Production

```bash
# Build client
clj -X:build:prod build-client

# Run production server
clj -M:prod -m prod

# Or build an uberjar
clj -X:build:prod uberjar
java -jar target/app.jar
```

### Docker

```bash
docker build -t freememo:latest .
docker run -d --name freememo -p 8080:8080 \
  -e DB_HOST=your-db-host \
  -e DB_PASSWORD=your-password \
  freememo:latest
```

Or with Docker Compose (app + database):

```bash
docker compose -f docker-compose.prod.yml up -d
```

### Reverse Proxy

The app runs on port 8080. For HTTPS, put it behind nginx or Caddy. **WebSocket support is required** -- Electric v3 uses persistent WebSocket connections.

nginx essentials:
```nginx
location / {
    proxy_pass http://localhost:8080;
    proxy_http_version 1.1;
    proxy_set_header Upgrade $http_upgrade;
    proxy_set_header Connection "upgrade";
    proxy_read_timeout 300s;
    client_max_body_size 100M;
}
```

Caddy (automatic HTTPS):
```
your-domain.com {
    reverse_proxy localhost:8080
}
```

## License

Electric v3 is free for bootstrappers and non-commercial use, and available commercially under a business source license. See [Electric v3 license](https://tana.pub/lQwRvGRaQ7hM/electric-v3-license-change).
