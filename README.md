# FreeMemo

Incremental reading with AI-generated flashcards. Import PDFs, EPUBs, and web articles, extract key concepts into reviewable cards, and sync with Anki.

**Try it at [freememo.net](https://freememo.net)** — no setup required. Or self-host using the instructions below.

Built with [Hyperfiddle Electric v3](https://github.com/hyperfiddle/electric) (Clojure/ClojureScript) — a reactive framework where client and server code coexist in the same `.cljc` files with automatic WebSocket sync.

## Features

- **Import** — PDFs, EPUBs, web articles (paste or Wikipedia lookup)
- **AI flashcard generation** — OpenAI extracts key concepts into basic and cloze cards
- **OCR** — OpenAI Vision converts PDF pages to editable semantic HTML
- **Rich text editor** — Quill-based editor with highlighting and selection-based card generation
- **Anki sync** — Push/pull cards directly to your Anki collection via AnkiConnect
- **PDF viewer** — In-browser rendering via PDF.js with zoom and navigation
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

### Run

```bash
# Start PostgreSQL
docker compose up -d

# Start dev server (hot reload, port 8080)
clj -M:dev -m dev
```

Open http://localhost:8080.

### Configuration

Navigate to **Settings** in the UI and enter your OpenAI API key. All settings are persisted per-user in the database.

> **Note**: freememo.net currently uses a shared demo OpenAI key for convenience. This will be removed in the future and per-user keys will become a self-host-only option.

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
| `DB_PASSWORD` | `dev` | Override in production |
| `ENC_KEY_SECRET` | _(none)_ | Session-cookie key derivation. **Required in production.** |
| `LOG_LEVEL` | `info` (prod) / `debug` (dev) | `trace`, `debug`, `info`, `warn`, `error`, `fatal` |
| `PORT` | `8080` | HTTP port |

## Production

```bash
# Build client
clj -X:build:prod build-client

# Run production server
clj -M:prod -m prod

# Or build an uberjar
clj -J-Xss4m -X:build:prod uberjar :build/jar-name '"app.jar"'
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

The app runs on port 8080. For HTTPS, put it behind nginx or Caddy. **WebSocket support is required** — Electric v3 uses persistent WebSocket connections.

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

### Backup & Restore

Backup the database:

```bash
docker compose -f docker-compose.prod.yml exec db pg_dump -U cardmaker cardmaker > backup.sql
```

Restore:

```bash
cat backup.sql | docker compose -f docker-compose.prod.yml exec -T db psql -U cardmaker cardmaker
```

A remote-pull variant is in `scripts/backup.sh`.

## License

Electric v3 is free for bootstrappers and non-commercial use, and available commercially under a business source license. See [Electric v3 license](https://tana.pub/lQwRvGRaQ7hM/electric-v3-license-change).
