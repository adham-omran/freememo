(ns freememo.db
  "Database connection and schema management for PostgreSQL.
   Unified topics model — all entities (documents, pages, extracts) are topics
   in a parent_id tree."
  (:require
   [next.jdbc :as jdbc]
   [next.jdbc.connection :as connection]
   [next.jdbc.result-set :as rs]
   [honey.sql :as sql]
   [cheshire.core :as json]
   [taoensso.telemere :as tel]
   [clojure.string :as str]
   [freememo.cloze :as cloze]
   [freememo.csl-util :as csl]
   [freememo.html-cleaner :as cleaner]
   [freememo.input-check :as input]
   [freememo.largeobj :as lo]
   [freememo.quota :as quota]
   [freememo.config :as config]
   [freememo.fsrs :as fsrs]
   [freememo.logging :as log]
   [freememo.occlusion-ordinals :as ord]
   [freememo.text :as text])
  (:import [com.zaxxer.hikari HikariDataSource]
           [org.postgresql.util PGobject]))

(declare sanitize-utf8)
(declare migrate-to-topics!)
(declare backfill-content-text!)
(declare backfill-sources!)
(declare backfill-pdf-sources!)
(declare run-grandfather-migration!)
(declare normalize-inline-card-images!)
(declare start-purge-scheduler!)
;; Defined in the Incremental Video section; called from complete-credit-order!,
;; which sits far above it.
(declare promote-to-paid-storage-tier!)

;; ---------------------------------------------------------------------------
;; Connection configuration
;; ---------------------------------------------------------------------------

(def db-config
  {:dbtype "postgresql"
   :host (or (System/getenv "DB_HOST") "localhost")
   :port (Integer/parseInt (or (System/getenv "DB_PORT") "5432"))
   :dbname (or (System/getenv "DB_NAME") "cardmaker")
   ;; HikariCP setter is setUsername → key must be :username, NOT :user
   ;; (next.jdbc's ->pool passes leftover keys to the pool's Java setters;
   ;; :user maps to no setter and is silently dropped → driver would fall
   ;; back to the OS user and auth would fail).
   :username (or (System/getenv "DB_USER") "cardmaker")
   :password (or (System/getenv "DB_PASSWORD") "dev")
   :maximumPoolSize 10})

;; HikariCP connection pool. Fail-fast init (HikariCP default
;; initializationFailTimeout): probes one connection at construction and throws if
;; the DB is unreachable, so a down/misconfigured DB surfaces at boot rather than
;; on first query. Tradeoff: requiring this namespace now needs a live DB
;; (no lazy load) — dev tooling / compile-checks that loaded db.clj without a DB
;; will no longer work.
(defonce ds (connection/->pool HikariDataSource db-config))

;; ---------------------------------------------------------------------------
;; Schema setup
;; ---------------------------------------------------------------------------

(defn- old-tables-exist?
  "Check if the legacy documents table exists (migration indicator)."
  []
  (some? (jdbc/execute-one! ds
           ["SELECT 1 FROM information_schema.tables
             WHERE table_name = 'documents' AND table_schema = 'public'"])))

(defn- topics-table-exists?
  []
  (some? (jdbc/execute-one! ds
           ["SELECT 1 FROM information_schema.tables
             WHERE table_name = 'topics' AND table_schema = 'public'"])))

(defn- setup-schema-core!
  "Extensions, the users table (+ OAuth + storage-quota columns), and the
   per-user settings table.
   Split out of setup-schema (see 7.4); called from it in order."
  []
  ;; Enable pgcrypto for password hashing
  (jdbc/execute! ds ["CREATE EXTENSION IF NOT EXISTS pgcrypto"])

  ;; Enable pg_trgm for trigram-based substring and fuzzy search
  (jdbc/execute! ds ["CREATE EXTENSION IF NOT EXISTS pg_trgm"])

  ;; Create users table
  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS users (
      id SERIAL PRIMARY KEY,
      username TEXT NOT NULL UNIQUE,
      password_hash TEXT NOT NULL,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )"])

  ;; Google OAuth migrations
  (jdbc/execute! ds ["ALTER TABLE users ADD COLUMN IF NOT EXISTS google_id TEXT UNIQUE"])
  (jdbc/execute! ds ["ALTER TABLE users ADD COLUMN IF NOT EXISTS email TEXT"])
  (jdbc/execute! ds ["ALTER TABLE users ALTER COLUMN password_hash DROP NOT NULL"])
  ;; Human-readable name for addressing the user in outgoing mail. Filled from
  ;; Google's `name` claim at login, but only when NULL — see
  ;; `set-display-name-if-absent!`, which must never clobber a hand-set value.
  (jdbc/execute! ds ["ALTER TABLE users ADD COLUMN IF NOT EXISTS display_name TEXT"])

  ;; Storage quota: denormalized usage counter + optional per-user override.
  ;; ALTER + backfill run in one tx so partial failure rolls back; otherwise
  ;; columns could exist with default 0 and `had-usage?` becomes true forever.
  (let [had-usage? (some? (jdbc/execute-one! ds
                            ["SELECT 1 FROM information_schema.columns
                              WHERE table_name = 'users' AND column_name = 'usage_bytes'"]))
        topic-files-exists? (some? (jdbc/execute-one! ds
                                     ["SELECT 1 FROM information_schema.tables
                                       WHERE table_name = 'topic_files'"]))]
    (jdbc/with-transaction [tx ds]
      (jdbc/execute! tx ["ALTER TABLE users ADD COLUMN IF NOT EXISTS usage_bytes BIGINT NOT NULL DEFAULT 0"])
      (jdbc/execute! tx ["ALTER TABLE users ADD COLUMN IF NOT EXISTS quota_bytes BIGINT"])
      (jdbc/execute! tx ["ALTER TABLE users ADD COLUMN IF NOT EXISTS upload_max_bytes BIGINT"])
      (when (and (not had-usage?) topic-files-exists?)
        (tel/log! :info "Backfilling users.usage_bytes from topic_files.file_size")
        (jdbc/execute! tx
          ["UPDATE users SET usage_bytes = COALESCE(
              (SELECT SUM(tf.file_size)
               FROM topic_files tf JOIN topics t ON tf.topic_id = t.id
               WHERE t.user_id = users.id), 0)"]))))

  ;; Settings table (per-user key-value)
  (let [has-user-id (jdbc/execute-one! ds
                      ["SELECT column_name FROM information_schema.columns
                        WHERE table_name = 'settings' AND column_name = 'user_id'"])]
    (when-not has-user-id
      (tel/log! :info "Migrating settings table to per-user")
      (jdbc/execute! ds ["DROP TABLE IF EXISTS settings"])
      (jdbc/execute! ds ["
        CREATE TABLE IF NOT EXISTS settings (
          user_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,
          key TEXT NOT NULL,
          value TEXT NOT NULL,
          updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
          PRIMARY KEY (user_id, key)
        )"])))
)

(defn- setup-schema-topics!
  "The topics tree (+ repetition log, is_live/dismissed/content_text flags),
   topic_files, and flashcards — plus the legacy-table migration and its
   post-migration column/constraint cleanup. Order matters: flashcards'
   topic_id/root_topic_id FKs need topics first, and the migration needs
   both tables in place before it can move rows into them.
   Split out of setup-schema (see 7.4); called from it in order."
  []
  ;; Topics table (unified: documents + pages + content_items)
  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS topics (
      id SERIAL PRIMARY KEY,
      user_id INTEGER REFERENCES users(id) ON DELETE SET NULL,
      parent_id INTEGER REFERENCES topics(id) ON DELETE CASCADE,
      kind TEXT NOT NULL DEFAULT 'basic',
      title TEXT NOT NULL,
      content TEXT,
      page_number INTEGER,
      source_url TEXT,
      status TEXT DEFAULT 'active',
      priority INTEGER DEFAULT 50,
      interval_days REAL DEFAULT 1.0,
      a_factor REAL DEFAULT 2.0,
      next_review_at TIMESTAMP,
      last_review_at TIMESTAMP,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )"])

  ;; review_count scalar superseded by topic_repetitions append-only log
  ;; (count derivable as COUNT(*) WHERE event_type IN ('advance','touch')).
  (jdbc/execute! ds ["ALTER TABLE topics DROP COLUMN IF EXISTS review_count"])

  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_topics_parent ON topics(parent_id)"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_topics_user ON topics(user_id)"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_topics_next_review ON topics(next_review_at)"])
  (jdbc/execute! ds ["CREATE UNIQUE INDEX IF NOT EXISTS idx_topics_page
                      ON topics(parent_id, page_number) WHERE page_number IS NOT NULL"])

  ;; Topic repetition log — append-only history of session-driven mutations.
  ;; Each row captures pre-mutation snapshot of the six SR-relevant topic
  ;; fields so a SuperMemo-style history view can reconstruct what changed.
  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS topic_repetitions (
      id BIGSERIAL PRIMARY KEY,
      topic_id INTEGER NOT NULL REFERENCES topics(id) ON DELETE CASCADE,
      user_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,
      event_type TEXT NOT NULL
        CHECK (event_type IN ('advance','touch','postpone','done','restore','priority-change','dismiss','undismiss')),
      event_at TIMESTAMP NOT NULL DEFAULT NOW(),
      status_before TEXT NOT NULL,
      priority_before INTEGER,
      interval_days_before REAL NOT NULL,
      a_factor_before REAL NOT NULL,
      next_review_at_before TIMESTAMP,
      last_review_at_before TIMESTAMP
    )"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_topic_repetitions_topic_time
                      ON topic_repetitions(topic_id, event_at DESC)"])
  ;; Widen event_type for Dismiss/Undismiss on installs created before them
  ;; (the inline CHECK above only applies to a freshly-created table).
  (jdbc/execute! ds ["ALTER TABLE topic_repetitions DROP CONSTRAINT IF EXISTS topic_repetitions_event_type_check"])
  (jdbc/execute! ds ["ALTER TABLE topic_repetitions ADD CONSTRAINT topic_repetitions_event_type_check
                      CHECK (event_type IN ('advance','touch','postpone','done','restore','priority-change',
                                            'dismiss','undismiss','import'))"])

  ;; 'import' rows come from a SuperMemo collection, which records WHEN each
  ;; repetition happened but not the interval, A-factor or status in force at
  ;; the time. Those three columns become nullable so an imported repetition
  ;; reads as "unknown" rather than carrying an invented value; every native
  ;; writer still supplies them (see log-topic-repetition!).
  (jdbc/execute! ds ["ALTER TABLE topic_repetitions ALTER COLUMN status_before DROP NOT NULL"])
  (jdbc/execute! ds ["ALTER TABLE topic_repetitions ALTER COLUMN interval_days_before DROP NOT NULL"])
  (jdbc/execute! ds ["ALTER TABLE topic_repetitions ALTER COLUMN a_factor_before DROP NOT NULL"])

  ;; SuperMemo import provenance. sm_rank is the topic's exact position in the
  ;; source collection's priority order; `priority` holds the same ordering
  ;; rounded to 0-100, and sm_rank restores the precision that rounding loses
  ;; (get-learning-queue orders by it before the daily hash). sm_element_id is
  ;; the element id it came from. Both NULL for natively created topics.
  (jdbc/execute! ds ["ALTER TABLE topics ADD COLUMN IF NOT EXISTS sm_rank INTEGER"])
  (jdbc/execute! ds ["ALTER TABLE topics ADD COLUMN IF NOT EXISTS sm_element_id INTEGER"])

  ;; Live Document flag — a kind='pdf' root that the user keeps appending
  ;; camera/upload image pages to. Stays a first-class PDF everywhere; this
  ;; boolean only drives the viewer's add-photos affordance and empty-state.
  (jdbc/execute! ds ["ALTER TABLE topics ADD COLUMN IF NOT EXISTS is_live BOOLEAN NOT NULL DEFAULT false"])

  ;; Dismiss flag — removes a topic AND its whole subtree from the Learning
  ;; Queue while keeping it in the collection (SuperMemo Dismiss). Orthogonal to
  ;; status: a topic may be both done and dismissed (4 positions). Reversible.
  (jdbc/execute! ds ["ALTER TABLE topics ADD COLUMN IF NOT EXISTS dismissed BOOLEAN NOT NULL DEFAULT false"])

  ;; Search: plain-text column derived from HTML content, trigram GIN index
  (jdbc/execute! ds ["ALTER TABLE topics ADD COLUMN IF NOT EXISTS content_text text"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_topics_content_text_trgm
                      ON topics USING GIN (content_text gin_trgm_ops)"])

  ;; Topic files (binary storage, split from old documents.file_data)
  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS topic_files (
      id SERIAL PRIMARY KEY,
      topic_id INTEGER NOT NULL REFERENCES topics(id) ON DELETE CASCADE,
      file_data BYTEA NOT NULL,
      file_size INTEGER,
      mime_type TEXT
    )"])

  ;; Score topics own TWO blobs (role='main' PDF + role='audio' recording);
  ;; every other kind keeps a single 'main' row. The historical
  ;; UNIQUE(topic_id) relaxes to UNIQUE(topic_id, role) — the DROP CONSTRAINT
  ;; migrates existing installs, the index covers both old and new.
  (jdbc/execute! ds ["ALTER TABLE topic_files ADD COLUMN IF NOT EXISTS role TEXT NOT NULL DEFAULT 'main'"])
  (jdbc/execute! ds ["ALTER TABLE topic_files DROP CONSTRAINT IF EXISTS topic_files_topic_id_key"])
  (jdbc/execute! ds ["CREATE UNIQUE INDEX IF NOT EXISTS idx_topic_files_topic_role
                      ON topic_files(topic_id, role)"])

  ;; Flashcards table — create if not exists, then add new columns
  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS flashcards (
      id SERIAL PRIMARY KEY,
      document_id INTEGER,
      page_number INTEGER,
      kind TEXT NOT NULL,
      question TEXT,
      answer TEXT,
      cloze TEXT,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )"])

  ;; Add all flashcard columns (idempotent)
  (jdbc/execute! ds ["ALTER TABLE flashcards ADD COLUMN IF NOT EXISTS content_item_id INTEGER"])
  (jdbc/execute! ds ["ALTER TABLE flashcards ADD COLUMN IF NOT EXISTS anki_note_id BIGINT DEFAULT NULL"])
  (jdbc/execute! ds ["ALTER TABLE flashcards ADD COLUMN IF NOT EXISTS anki_synced_at TIMESTAMP DEFAULT NULL"])
  (jdbc/execute! ds ["ALTER TABLE flashcards ADD COLUMN IF NOT EXISTS updated_at TIMESTAMP DEFAULT NULL"])
  (jdbc/execute! ds ["ALTER TABLE flashcards ADD COLUMN IF NOT EXISTS topic_id INTEGER REFERENCES topics(id) ON DELETE CASCADE"])
  (jdbc/execute! ds ["ALTER TABLE flashcards ADD COLUMN IF NOT EXISTS root_topic_id INTEGER REFERENCES topics(id) ON DELETE CASCADE"])

  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_flashcards_topic ON flashcards(topic_id)"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_flashcards_root_topic ON flashcards(root_topic_id)"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_flashcards_anki_note_id ON flashcards(anki_note_id) WHERE anki_note_id IS NOT NULL"])

  ;; Run migration if old tables exist
  (when (old-tables-exist?)
    (migrate-to-topics!))

  ;; After migration: drop legacy columns if they still exist and add new unique constraint
  (when (and (topics-table-exists?)
          (not (old-tables-exist?)))
    ;; Drop legacy flashcard columns (safe — migration already moved data)
    (try
      (jdbc/execute! ds ["ALTER TABLE flashcards DROP COLUMN IF EXISTS document_id"])
      (jdbc/execute! ds ["ALTER TABLE flashcards DROP COLUMN IF EXISTS page_number"])
      (jdbc/execute! ds ["ALTER TABLE flashcards DROP COLUMN IF EXISTS content_item_id"])
      (catch Exception _ nil))
    ;; Unique constraints — two partial indexes to handle NULLs correctly
    ;; (PostgreSQL treats NULL != NULL in B-tree indexes, so a single index with nullable columns can't prevent duplicates)
    (try
      (jdbc/execute! ds ["DROP INDEX IF EXISTS idx_flashcards_unique_topic"])
      (jdbc/execute! ds ["CREATE UNIQUE INDEX IF NOT EXISTS idx_flashcards_unique_basic
                          ON flashcards(topic_id, kind, question) WHERE cloze IS NULL"])
      (jdbc/execute! ds ["CREATE UNIQUE INDEX IF NOT EXISTS idx_flashcards_unique_cloze
                          ON flashcards(topic_id, kind, cloze) WHERE question IS NULL"])
      (catch Exception _ nil)))
)

(defn- setup-schema-activity!
  "User events, the media registry, and topic pins (pins FK into media).
   Split out of setup-schema (see 7.4); called from it in order."
  []
  ;; User events (activity tracking)
  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS user_events (
      id SERIAL PRIMARY KEY,
      user_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,
      event_type TEXT NOT NULL,
      metadata JSONB,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_user_events_user_type
                      ON user_events (user_id, event_type, created_at DESC)"])

  ;; Media registry — per-user blobs (images today; audio/video/file reserved).
  ;; Served via /api/media/:id with per-user auth. Deduped by (user_id, sha256).
  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS media (
      id BIGSERIAL PRIMARY KEY,
      user_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,
      kind TEXT NOT NULL CHECK (kind IN ('image','audio','video','file')),
      bytes BYTEA NOT NULL,
      mime_type TEXT NOT NULL,
      sha256 CHAR(64) NOT NULL,
      byte_size INTEGER NOT NULL,
      source_url TEXT,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )"])
  (jdbc/execute! ds ["CREATE UNIQUE INDEX IF NOT EXISTS idx_media_user_sha
                      ON media(user_id, sha256)"])

  ;; Topic pins — up to 2 image references per topic with front/back placement.
  ;; K1 cap enforced in the insert layer (set-pin!). EC-snapshot: extract creation
  ;; copies parent's rows into child via copy-pins-to-child!.
  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS topic_pins (
      id SERIAL PRIMARY KEY,
      topic_id INTEGER NOT NULL REFERENCES topics(id) ON DELETE CASCADE,
      media_id BIGINT NOT NULL REFERENCES media(id) ON DELETE CASCADE,
      placement TEXT NOT NULL CHECK (placement IN ('front','back')),
      ord SMALLINT NOT NULL DEFAULT 0
    )"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_topic_pins_topic ON topic_pins(topic_id)"])
)

(defn- setup-schema-assistant!
  "Socratic AI assistant chats + message transcripts.
   Split out of setup-schema (see 7.4); called from it in order."
  []
  ;; Socratic AI assistant — per-document chats + their message transcripts.
  ;; A chat is scoped to (user_id, root_topic_id): the document being read.
  ;; The transcript holds learner/assistant turns only; the learner's live
  ;; reading context is injected transiently per send, never persisted
  ;; (see freememo.assistant/current-context-messages).
  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS assistant_chats (
      id SERIAL PRIMARY KEY,
      user_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,
      root_topic_id INTEGER NOT NULL REFERENCES topics(id) ON DELETE CASCADE,
      title TEXT,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      updated_at TIMESTAMP DEFAULT NULL
    )"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_assistant_chats_user_topic
                      ON assistant_chats(user_id, root_topic_id)"])
  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS assistant_messages (
      id SERIAL PRIMARY KEY,
      chat_id INTEGER NOT NULL REFERENCES assistant_chats(id) ON DELETE CASCADE,
      role TEXT NOT NULL CHECK (role IN ('user','assistant')),
      content TEXT NOT NULL,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )"])
  ;; Client-generated id for the optimistic echo of a user turn. The client
  ;; renders the sent turn immediately and retires that echo once the persisted
  ;; row bearing this id appears in the transcript — a globally-unique key that
  ;; survives the round trip. NULL on assistant/server-originated rows.
  (jdbc/execute! ds ["ALTER TABLE assistant_messages ADD COLUMN IF NOT EXISTS client_id TEXT"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_assistant_messages_chat
                      ON assistant_messages(chat_id, id)"])
  ;; Persona for this chat, an :id from freememo.assistant-modes/registry. Mode
  ;; is per chat so a transcript is one persona end to end. NULL on every chat
  ;; that predates the column, which resolve-mode maps to the Socratic default —
  ;; correct, since Socratic was the only persona that ever existed before it.
  (jdbc/execute! ds ["ALTER TABLE assistant_chats ADD COLUMN IF NOT EXISTS mode TEXT"])
)

(defn- setup-schema-annotations!
  "Image-occlusion groups, audio/score groups, and the overlapping-cloze
   column — the three non-basic flashcard kinds' supporting tables/columns.
   Both groups FK into media, so this runs after setup-schema-activity!.
   Split out of setup-schema (see 7.4); called from it in order."
  []
  ;; Occlusion groups — one row per image-occlusion authoring session.
  ;; geometry JSONB = {:width :height :rects [{:x :y :w :h :ordinal} ...]}
  ;; in natural-image pixels. An ordinal identifies a MASK GROUP, so several
  ;; rects may share one and sharing it is the grouping. next_ordinal is the
  ;; single ordinal authority: ordinals are assigned from it and NEVER reused,
  ;; because they bind the per-mask-group flashcard row ↔ Anki note ↔ SVG rect
  ;; id. anki_key names the group's Anki media files (fm-<anki_key>-…) and note IDs.
  ;; Placed after media (image FK) and before the flashcards ALTERs below.
  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS occlusion_groups (
      id SERIAL PRIMARY KEY,
      anki_key TEXT NOT NULL,
      image_media_id BIGINT NOT NULL REFERENCES media(id) ON DELETE CASCADE,
      mode TEXT NOT NULL CHECK (mode IN ('hide-all','hide-one')),
      geometry JSONB NOT NULL,
      next_ordinal INTEGER NOT NULL DEFAULT 1,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      updated_at TIMESTAMP DEFAULT NULL
    )"])
  (jdbc/execute! ds ["ALTER TABLE flashcards ADD COLUMN IF NOT EXISTS occlusion_group_id INTEGER REFERENCES occlusion_groups(id) ON DELETE CASCADE"])
  (jdbc/execute! ds ["ALTER TABLE flashcards ADD COLUMN IF NOT EXISTS mask_ordinal INTEGER"])
  (jdbc/execute! ds ["ALTER TABLE flashcards ADD COLUMN IF NOT EXISTS io_fields JSONB"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_flashcards_occlusion_group
                      ON flashcards(occlusion_group_id) WHERE occlusion_group_id IS NOT NULL"])
  ;; One row per mask ordinal — the grouping invariant, enforced rather than
  ;; conventional. Guarded like the other unique indexes above: a legacy
  ;; duplicate must not stop the app from booting.
  (try
    (jdbc/execute! ds ["CREATE UNIQUE INDEX IF NOT EXISTS uq_flashcards_mask_ordinal
                        ON flashcards(occlusion_group_id, mask_ordinal)
                        WHERE occlusion_group_id IS NOT NULL"])
    (catch Exception e
      (tel/log! {:level :warn :id ::mask-ordinal-index :data {:error (.getMessage e)}}
        "Could not create uq_flashcards_mask_ordinal — duplicate mask ordinals exist")))

  ;; Score groups — one row per (audio segment × notation rects) pair on a
  ;; kind='score' topic. geometry JSONB = {:pages [{:page :width :height
  ;; :rects [{:x :y :w :h :ordinal :media-id} ...]} ...]} in snapshot-pixel
  ;; space of the recorded page render. Clip + crops are materialized media
  ;; rows (cut/cropped client-side at card creation, NOT at push time).
  ;; anki_key names the group's Anki media files (fm-score-<anki_key>-…).
  ;; One flashcard row per direction ('audio-front'/'sheet-front'); 'Both'
  ;; fans out to two notes sharing the same media.
  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS score_groups (
      id SERIAL PRIMARY KEY,
      anki_key TEXT NOT NULL,
      start_ms INTEGER NOT NULL,
      end_ms INTEGER NOT NULL,
      clip_media_id BIGINT NOT NULL REFERENCES media(id) ON DELETE CASCADE,
      geometry JSONB NOT NULL,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      updated_at TIMESTAMP DEFAULT NULL
    )"])
  (jdbc/execute! ds ["ALTER TABLE flashcards ADD COLUMN IF NOT EXISTS score_group_id INTEGER REFERENCES score_groups(id) ON DELETE CASCADE"])
  (jdbc/execute! ds ["ALTER TABLE flashcards ADD COLUMN IF NOT EXISTS score_direction TEXT"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_flashcards_score_group
                      ON flashcards(score_group_id) WHERE score_group_id IS NOT NULL"])

  ;; Overlapping cloze — one row per authored list. The overlapping JSONB is the
  ;; whole card: {:question :items [...] :settings {...} :fields {:Original :Full
  ;; :Text1 ...}}. :items+:settings are the source edits write to; :fields is
  ;; the materialized Anki-field expansion (freememo.overlapping/expand),
  ;; re-derived on every save. question/answer/cloze stay NULL for this kind.
  (jdbc/execute! ds ["ALTER TABLE flashcards ADD COLUMN IF NOT EXISTS overlapping JSONB"])
)

(defn- setup-schema-sources!
  "Bibliography: the sources table, topics.source_id, the source_url/
   source_reference backfill + cleanup, and legacy title canonicalization.
   Must run after setup-schema-topics! (backfills read/write topics rows).
   Split out of setup-schema (see 7.4); called from it in order."
  []
  ;; Drop deprecated source_reference columns — title is the single source of truth.
  (jdbc/execute! ds ["ALTER TABLE topics DROP COLUMN IF EXISTS source_reference"])
  (jdbc/execute! ds ["ALTER TABLE flashcards DROP COLUMN IF EXISTS source_reference"])

  ;; Sources — bibliography records, decoupled from the FM topic tree.
  ;; One row per origin (wiki article, PDF, book). Many topics may cite the
  ;; same source via topics.source_id. csl JSONB stores the full CSL-JSON
  ;; record; url + title denormalize the hot fields for indexing.
  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS sources (
      id SERIAL PRIMARY KEY,
      user_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,
      csl_type TEXT NOT NULL,
      csl JSONB NOT NULL DEFAULT '{}'::jsonb,
      url TEXT,
      title TEXT,
      container_title TEXT,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )"])
  ;; Idempotent column add for installs that created the table without container_title.
  (jdbc/execute! ds ["ALTER TABLE sources ADD COLUMN IF NOT EXISTS container_title TEXT"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_sources_user_url ON sources(user_id, url)"])

  ;; topics.source_id — FK into sources. Nullable: a topic may have no
  ;; bibliography. ON DELETE SET NULL preserves topic content if a source row
  ;; is deleted (sources are reference data; topics are user content).
  (jdbc/execute! ds ["ALTER TABLE topics ADD COLUMN IF NOT EXISTS
                       source_id INTEGER REFERENCES sources(id) ON DELETE SET NULL"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_topics_source ON topics(source_id)"])

  ;; Backfill sources from legacy topics.source_url (idempotent — only runs
  ;; on rows where source_url IS NOT NULL AND source_id IS NULL).
  (backfill-sources!)

  ;; Backfill document sources for legacy PDF root topics with no source row
  ;; (idempotent — only touches kind='pdf' rows where source_id IS NULL).
  (backfill-pdf-sources!)

  ;; Drop the legacy topics.source_url column. All bibliography lives in
  ;; `sources` now; topics reference via source_id. Runs after backfill so
  ;; existing URL data is migrated, not lost.
  (jdbc/execute! ds ["ALTER TABLE topics DROP COLUMN IF EXISTS source_url"])

  ;; Canonicalize legacy titles: replace _ with space, strip trailing .pdf,
  ;; collapse whitespace, trim. Idempotent — clean rows are skipped by WHERE.
  (let [pretty-sql "btrim(regexp_replace(
                            regexp_replace(replace(title, '_', ' '), '\\.pdf$', '', 'i'),
                            '\\s+', ' ', 'g'))"
        result (jdbc/execute-one! ds
                 [(str "WITH updated AS (
                          UPDATE topics SET title = " pretty-sql "
                          WHERE title <> " pretty-sql "
                          RETURNING 1
                        )
                        SELECT COUNT(*) AS n FROM updated")])
        n (or (:n result) 0)]
    (when (pos? n)
      (tel/log! {:level :info :id ::prettify-titles :data {:count n}}
        (str "Prettified " n " legacy title(s)"))))

  ;; Backfill content_text for existing rows (idempotent)
  (backfill-content-text!)
)

(defn- setup-schema-credits!
  "Pass-through AI billing: per-user balance/markup columns, credit_orders,
   credit_transactions.
   Split out of setup-schema (see 7.4); called from it in order."
  []
  ;; Credits — pass-through AI billing (official deployment only).
  ;; Per-user IQD balance (denormalized, mirrors usage_bytes) + append-only
  ;; ledger (source of truth: SUM(amount_iqd) per user == credit_balance_iqd)
  ;; + Wayl order tracking (reference_id UNIQUE = webhook idempotency).
  ;; See plans/credits-wayl-payment-system.md §5.1.
  (jdbc/execute! ds ["ALTER TABLE users ADD COLUMN IF NOT EXISTS credit_balance_iqd BIGINT NOT NULL DEFAULT 0"])
  ;; Per-user markup override (nullable). NULL = use the config default markup;
  ;; a value replaces it for this user's billing (see freememo.credits/resolve-markup).
  (jdbc/execute! ds ["ALTER TABLE users ADD COLUMN IF NOT EXISTS markup_override DECIMAL"])

  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS credit_orders (
      id SERIAL PRIMARY KEY,
      user_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,
      reference_id TEXT NOT NULL UNIQUE,
      amount_iqd BIGINT NOT NULL,
      status TEXT NOT NULL DEFAULT 'pending'
        CHECK (status IN ('pending','complete','failed')),
      wayl_code TEXT,
      wayl_link_id TEXT,
      created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
      completed_at TIMESTAMP
    )"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_credit_orders_user
                      ON credit_orders(user_id, created_at DESC)"])

  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS credit_transactions (
      id BIGSERIAL PRIMARY KEY,
      user_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,
      kind TEXT NOT NULL CHECK (kind IN ('purchase','debit','grant','adjustment')),
      amount_iqd BIGINT NOT NULL,
      balance_after BIGINT NOT NULL,
      endpoint TEXT,
      model TEXT,
      input_tokens INTEGER,
      cached_tokens INTEGER,
      output_tokens INTEGER,
      reasoning_tokens INTEGER,
      attempts INTEGER,
      order_reference_id TEXT,
      created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
    )"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_credit_transactions_user
                      ON credit_transactions(user_id, created_at DESC)"])
)

(defn- setup-schema-card-versions!
  "Card edit history — superseded card/group renditions. Must run after
   setup-schema-topics! (FKs topics). See plans/card-edit-history.md.
   Split out of setup-schema (see 7.4); called from it in order."
  []
  ;; Write-behind version log: a row holds the rendition a save REPLACED, so
  ;; the current rendition is never duplicated here and the pre-edit state of
  ;; an LLM-generated card is captured on its first edit for free.
  ;; superseded_at therefore means "stopped being current at"; rendition k's
  ;; validity interval is [row(k-1).superseded_at, row(k).superseded_at), with
  ;; flashcards.created_at opening the first.
  ;;
  ;; Scope is (scope_type, scope_id), NOT an FK. Two reasons:
  ;; reconcile-occlusion-group! deletes flashcard rows whose mask ordinal
  ;; disappears, so an FK to flashcards would erase a mask's history during an
  ;; ordinary EDIT; and one scope column pair needs one index where three
  ;; nullable id columns would need three partial ones (~105 vs ~138 B/row).
  ;; root_topic_id is the only FK — history is reclaimed with the document.
  ;;
  ;; Payload columns mirror their source columns and are sparse: scope_type
  ;; selects which are non-null. geometry is shared by both group scopes
  ;; (occlusion rects vs score pages — scope_type disambiguates the shape).
  ;; occlusion_image_media_id is copied, not joined: remove-occlusion-mask!
  ;; deletes the group row once its last mask card is gone, and the version
  ;; must still render after that.
  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS card_versions (
      id BIGSERIAL PRIMARY KEY,
      root_topic_id INTEGER NOT NULL REFERENCES topics(id) ON DELETE CASCADE,
      scope_type TEXT NOT NULL CHECK (scope_type IN ('card','occlusion_group','score_group')),
      scope_id BIGINT NOT NULL,
      kind TEXT NOT NULL,
      superseded_at TIMESTAMPTZ NOT NULL DEFAULT now(),
      question TEXT,
      answer TEXT,
      cloze TEXT,
      overlapping JSONB,
      io_fields JSONB,
      geometry JSONB,
      occlusion_mode TEXT,
      occlusion_image_media_id BIGINT,
      score_direction TEXT,
      score_start_ms INTEGER,
      score_end_ms INTEGER,
      score_clip_media_id BIGINT
    )"])
  ;; Reads are always "newest first, within one scope" — the composite index
  ;; serves both the ordering and the scope filter. id DESC breaks ties when
  ;; two versions share a microsecond (no sequence counter needed).
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_card_versions_scope
                      ON card_versions(scope_type, scope_id, superseded_at DESC, id DESC)"])
  ;; Unindexed FK referencing columns make ON DELETE CASCADE table-scan.
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_card_versions_root_topic
                      ON card_versions(root_topic_id)"])
)

(defn- setup-schema-undo!
  "The undo log and topics.staged_delete_id (which FKs into it) — must run
   after setup-schema-topics! (topics must already exist).
   Split out of setup-schema (see 7.4); called from it in order."
  []
  ;; Undo log — bounded per-user action history for the Undo feature.
  ;; entity_refs/snapshot are JSONB; occurred_at drives the 12h window,
  ;; undone_at IS NULL marks an entry still reversible. Hard-capped to the
  ;; most recent 100 live-or-not rows per user by prune-undo-log!.
  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS undo_log (
      id BIGSERIAL PRIMARY KEY,
      user_id BIGINT NOT NULL REFERENCES users(id) ON DELETE CASCADE,
      action_type TEXT NOT NULL CHECK (action_type IN
        ('delete-card','bulk-delete-cards','remove-pin','reset-prompt','delete-document','move-topic',
         'reject-question','reject-fact','bulk-reject-facts')),
      entity_type TEXT NOT NULL CHECK (entity_type IN ('flashcard','pin','setting','document','question','fact')),
      entity_refs JSONB NOT NULL,
      snapshot JSONB NOT NULL,
      occurred_at TIMESTAMPTZ NOT NULL DEFAULT now(),
      undone_at TIMESTAMPTZ
    )"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_undo_log_user
                      ON undo_log(user_id, occurred_at DESC)"])
  ;; Widen the CHECK lists on installs created before 'delete-document'/'document'
  ;; (CREATE TABLE IF NOT EXISTS won't alter a live constraint).
  (jdbc/execute! ds ["ALTER TABLE undo_log DROP CONSTRAINT IF EXISTS undo_log_action_type_check"])
  (jdbc/execute! ds ["ALTER TABLE undo_log ADD CONSTRAINT undo_log_action_type_check
                      CHECK (action_type IN
                        ('delete-card','bulk-delete-cards','remove-pin','reset-prompt','delete-document','move-topic',
                         'reject-question','reject-fact','bulk-reject-facts'))"])
  (jdbc/execute! ds ["ALTER TABLE undo_log DROP CONSTRAINT IF EXISTS undo_log_entity_type_check"])
  (jdbc/execute! ds ["ALTER TABLE undo_log ADD CONSTRAINT undo_log_entity_type_check
                      CHECK (entity_type IN ('flashcard','pin','setting','document','question','fact'))"])

  ;; Staged-delete marker: a topic (and its whole subtree) hidden pending the
  ;; 12h undo window points at the undo_log entry that staged it. ON DELETE
  ;; SET NULL — see prune/purge: only the time-based purge removes a
  ;; delete-document entry, and it hard-deletes the topics first.
  (jdbc/execute! ds ["ALTER TABLE topics ADD COLUMN IF NOT EXISTS staged_delete_id BIGINT
                      REFERENCES undo_log(id) ON DELETE SET NULL"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_topics_staged_delete
                      ON topics(staged_delete_id) WHERE staged_delete_id IS NOT NULL"])
)

(defn- setup-schema-kg!
  "Knowledge graph: entities, predicates, facts, question bank (+ FSRS
   columns), question↔fact links, quiz/exam sessions, answers, review log,
   and the graph-layout cache. See plans/knowledge-graph-quizzes.md.
   Split out of setup-schema (see 7.4); called from it in order."
  []
  ;; Knowledge graph — LLM-distilled facts with human review (see
  ;; plans/knowledge-graph-quizzes.md). Physical store is annotated rows;
  ;; the Ontop service (ontop/) publishes approved rows as orthodox RDF.
  ;; IRIs are DERIVED, never stored: urn:freememo:{user_id}:entity/{id},
  ;; :predicate/{slug}, urn:freememo:fact/{id} — the OBDA mapping owns the
  ;; derivation, so rows and RDF cannot drift.

  ;; Entities — graph nodes. label/aliases double as the quiz keyword lexicon.
  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS kg_entities (
      id SERIAL PRIMARY KEY,
      user_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,
      label TEXT NOT NULL,
      aliases TEXT[] NOT NULL DEFAULT '{}',
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_kg_entities_user ON kg_entities(user_id)"])
  ;; Trigram index feeds the entity-linking shortlist during extraction.
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_kg_entities_label_trgm
                      ON kg_entities USING gin (label gin_trgm_ops)"])

  ;; Predicates — per-user vocabulary, born empty; extraction proposes, the
  ;; review queue approves. slug CHECK enforces IRI-safety at the boundary so
  ;; no insert site has to.
  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS kg_predicates (
      id SERIAL PRIMARY KEY,
      user_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,
      slug TEXT NOT NULL CHECK (slug ~ '^[a-z0-9]+(-[a-z0-9]+)*$'),
      label TEXT NOT NULL,
      description TEXT,
      status TEXT NOT NULL DEFAULT 'proposed'
        CHECK (status IN ('proposed','approved','rejected')),
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      reviewed_at TIMESTAMP,
      UNIQUE (user_id, slug)
    )"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_kg_predicates_user_status
                      ON kg_predicates(user_id, status)"])

  ;; Facts — one row per statement. Invariants: exactly one of
  ;; object_entity_id / object_literal (CHECK); no duplicate s/p/o within one
  ;; document (partial unique indexes below — writers must ON CONFLICT DO
  ;; NOTHING). graph_topic_id = root document topic (the named graph);
  ;; page_number = provenance. Only status='approved' rows are published via
  ;; SPARQL, feed question generation, or feed grading.
  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS kg_facts (
      id SERIAL PRIMARY KEY,
      user_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,
      subject_entity_id INTEGER NOT NULL REFERENCES kg_entities(id) ON DELETE CASCADE,
      predicate_id INTEGER NOT NULL REFERENCES kg_predicates(id),
      object_entity_id INTEGER REFERENCES kg_entities(id) ON DELETE CASCADE,
      object_literal TEXT,
      object_datatype TEXT,
      graph_topic_id INTEGER NOT NULL REFERENCES topics(id) ON DELETE CASCADE,
      page_number INTEGER,
      status TEXT NOT NULL DEFAULT 'proposed'
        CHECK (status IN ('proposed','approved','rejected')),
      source_model TEXT,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      reviewed_at TIMESTAMP,
      CHECK ((object_entity_id IS NULL) <> (object_literal IS NULL))
    )"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_kg_facts_graph_status
                      ON kg_facts(graph_topic_id, status)"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_kg_facts_user_status
                      ON kg_facts(user_id, status)"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_kg_facts_subject
                      ON kg_facts(subject_entity_id)"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_kg_facts_object_entity
                      ON kg_facts(object_entity_id) WHERE object_entity_id IS NOT NULL"])
  ;; Dedup within a document; the same s/p/o from a second document is a new
  ;; row (distinct provenance). md5() keeps long literals under btree limits.
  (jdbc/execute! ds ["CREATE UNIQUE INDEX IF NOT EXISTS uq_kg_facts_spo_entity
                      ON kg_facts(subject_entity_id, predicate_id, object_entity_id, graph_topic_id)
                      WHERE object_entity_id IS NOT NULL"])
  (jdbc/execute! ds ["CREATE UNIQUE INDEX IF NOT EXISTS uq_kg_facts_spo_literal
                      ON kg_facts(subject_entity_id, predicate_id, md5(object_literal), graph_topic_id)
                      WHERE object_literal IS NOT NULL"])

  ;; Question bank — generated from approved facts, curated by exception
  ;; (edit/reject); 'retired' marks questions whose underlying fact was
  ;; rejected. kind: 'atomic' covers one fact, 'synthesis' spans an entity's
  ;; neighborhood. Links live in kg_question_facts; a hard fact delete
  ;; (entity-merge dedup) cascades the link rows only.
  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS kg_questions (
      id SERIAL PRIMARY KEY,
      user_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,
      kind TEXT NOT NULL CHECK (kind IN ('atomic','synthesis')),
      question TEXT NOT NULL,
      reference_answer TEXT NOT NULL,
      status TEXT NOT NULL DEFAULT 'approved'
        CHECK (status IN ('approved','rejected','retired')),
      source_model TEXT,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_kg_questions_user_status
                      ON kg_questions(user_id, status)"])
  ;; FSRS-6 per-question scheduling state (see freememo.fsrs). fsrs_state:
  ;; 1=Learning 2=Review 3=Relearning. fsrs_due NULL = never introduced — a
  ;; "new" card, eligible only through the daily new-card cap. Cold-start (no
  ;; backfill): every existing question keeps NULL memory + NULL due and so is
  ;; treated as new. The Review quiz is the only writer; custom quiz/exam skip it.
  (doseq [col ["fsrs_stability REAL"
               "fsrs_difficulty REAL"
               "fsrs_due TIMESTAMPTZ"
               "fsrs_state SMALLINT NOT NULL DEFAULT 1"
               "fsrs_step SMALLINT DEFAULT 0"
               "fsrs_reps INTEGER NOT NULL DEFAULT 0"
               "fsrs_lapses INTEGER NOT NULL DEFAULT 0"
               "fsrs_last_review TIMESTAMPTZ"]]
    (jdbc/execute! ds [(str "ALTER TABLE kg_questions ADD COLUMN IF NOT EXISTS " col)]))
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_kg_questions_fsrs_due
                      ON kg_questions(user_id, fsrs_due)"])
  ;; Curation flags — orthogonal to status, both reversible (mirrors
  ;; topics.dismissed). flagged = "revisit this question later"; it never filters a
  ;; draw. suspended = withheld from every draw until cleared. Deliberately NOT
  ;; folded into status: the fact reject/restore path rewrites status
  ;; ('retired'/'approved'), which would silently clobber a suspension stored there.
  (doseq [col ["flagged BOOLEAN NOT NULL DEFAULT false"
               "suspended BOOLEAN NOT NULL DEFAULT false"]]
    (jdbc/execute! ds [(str "ALTER TABLE kg_questions ADD COLUMN IF NOT EXISTS " col)]))
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_kg_questions_suspended
                      ON kg_questions(user_id) WHERE suspended"])
  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS kg_question_facts (
      question_id INTEGER NOT NULL REFERENCES kg_questions(id) ON DELETE CASCADE,
      fact_id INTEGER NOT NULL REFERENCES kg_facts(id) ON DELETE CASCADE,
      PRIMARY KEY (question_id, fact_id)
    )"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_kg_question_facts_fact
                      ON kg_question_facts(fact_id)"])

  ;; Quiz/exam sessions — question_ids freezes the draw at start (reload-safe,
  ;; and an exam's paper must not shift mid-sitting). time_limit_seconds NULL =
  ;; quiz (untimed, instant feedback); set = exam. finished_at NULL = active.
  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS kg_sessions (
      id SERIAL PRIMARY KEY,
      user_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,
      kind TEXT NOT NULL CHECK (kind IN ('quiz','exam')),
      scope_topic_ids INTEGER[] NOT NULL,
      question_ids INTEGER[] NOT NULL,
      time_limit_seconds INTEGER,
      started_at TIMESTAMPTZ NOT NULL DEFAULT now(),
      finished_at TIMESTAMPTZ
    )"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_kg_sessions_user_active
                      ON kg_sessions(user_id, kind) WHERE finished_at IS NULL"])

  ;; One answer row per (session, question); verdict NULL until graded.
  ;; missed_fact_ids ⊆ the question's linked facts; matched_keywords ⊆ the
  ;; linked entities' labels/aliases — both enforced by the grading fn, which
  ;; is the only writer of graded fields.
  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS kg_answers (
      id SERIAL PRIMARY KEY,
      session_id INTEGER NOT NULL REFERENCES kg_sessions(id) ON DELETE CASCADE,
      question_id INTEGER NOT NULL REFERENCES kg_questions(id),
      position INTEGER NOT NULL,
      user_answer TEXT NOT NULL,
      verdict TEXT CHECK (verdict IN ('correct','partial','incorrect')),
      explanation TEXT,
      missed_fact_ids INTEGER[] NOT NULL DEFAULT '{}',
      matched_keywords TEXT[] NOT NULL DEFAULT '{}',
      created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
      graded_at TIMESTAMPTZ,
      UNIQUE (session_id, question_id)
    )"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_kg_answers_session
                      ON kg_answers(session_id, position)"])
  ;; The question's wording as first asked. History LISTS render this snapshot so a
  ;; later edit never rewrites the record; history DETAIL reads the live question by
  ;; id and marks the divergence. NULL on rows written before this column existed —
  ;; "not recorded", which is not the same as "unchanged".
  (jdbc/execute! ds ["ALTER TABLE kg_answers ADD COLUMN IF NOT EXISTS question_text TEXT"])

  ;; FSRS review log — append-only, one row per graded Review-quiz answer. Source
  ;; of truth for scheduling history + daily caps (mirrors topic_repetitions).
  ;; Custom quiz and exam never write here (practice / no-FSRS). reps_before = the
  ;; card's fsrs_reps at review time, so reps_before = 0 marks a card's first-ever
  ;; review — i.e. a "new" introduction — for the new-per-day cap.
  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS kg_reviews (
      id SERIAL PRIMARY KEY,
      user_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,
      question_id INTEGER NOT NULL REFERENCES kg_questions(id) ON DELETE CASCADE,
      rating SMALLINT NOT NULL CHECK (rating IN (1,2,3,4)),
      verdict TEXT CHECK (verdict IN ('correct','partial','incorrect')),
      state_before SMALLINT NOT NULL,
      state_after SMALLINT NOT NULL,
      reps_before INTEGER NOT NULL,
      stability_after REAL,
      difficulty_after REAL,
      elapsed_days INTEGER,
      scheduled_days INTEGER,
      reviewed_at TIMESTAMPTZ NOT NULL DEFAULT now()
    )"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_kg_reviews_user_time
                      ON kg_reviews(user_id, reviewed_at DESC)"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_kg_reviews_question
                      ON kg_reviews(question_id, reviewed_at DESC)"])
  ;; Answer record. The Review flow writes no kg_answers row, so these columns ARE
  ;; its answer history — one row per graded review. A grading failure writes no row
  ;; at all (the FSRS schedule is untouched), so a failed grade leaves no history and
  ;; the answer text is lost; the session flows persist before grading instead.
  ;; question_text = the wording at grade time (see kg_answers above).
  (doseq [col ["user_answer TEXT"
               "explanation TEXT"
               "question_text TEXT"
               "missed_fact_ids INTEGER[] NOT NULL DEFAULT '{}'"]]
    (jdbc/execute! ds [(str "ALTER TABLE kg_reviews ADD COLUMN IF NOT EXISTS " col)]))

  ;; Graph-view layout cache — one row per (user, scope). payload is the
  ;; positioned, wire-ready render payload {:nodes :edges :predicates :docs};
  ;; version = the :kg-mutations counter at compute time, so a stale row is
  ;; detected by a cheap integer compare on read (freememo.kg-graph). Positions
  ;; are recomputed lazily on the next Graph-tab open after any KG mutation.
  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS kg_graph_layout (
      user_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,
      scope TEXT NOT NULL,
      version BIGINT NOT NULL,
      payload JSONB NOT NULL,
      computed_at TIMESTAMPTZ NOT NULL DEFAULT now(),
      PRIMARY KEY (user_id, scope)
    )"])
)

(defn- setup-schema-card-quiz!
  "Flashcards as Quiz items: per-deletion FSRS state, and kg_reviews widened to log
   a card review. See plans/cards-in-quiz-queue.md §1. Runs after
   setup-schema-kg! — it alters kg_reviews."
  []
  ;; One row per QUIZ ITEM, not per card. A cloze card yields one item per
  ;; deletion (ord = the N in {{cN::…}}); a basic card yields exactly one (ord 0).
  ;; Rows are created lazily on first review, so an ABSENT row means "never
  ;; introduced" — the same signal `fsrs_due IS NULL` carries for a question.
  ;; Columns mirror kg_questions' FSRS set; freememo.fsrs owns their meaning.
  ;; No user_id: a card is scoped through flashcards.root_topic_id → topics.
  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS card_schedules (
      flashcard_id INTEGER NOT NULL REFERENCES flashcards(id) ON DELETE CASCADE,
      ord SMALLINT NOT NULL,
      state SMALLINT NOT NULL DEFAULT 1,
      step SMALLINT DEFAULT 0,
      stability REAL,
      difficulty REAL,
      due TIMESTAMPTZ,
      reps INTEGER NOT NULL DEFAULT 0,
      lapses INTEGER NOT NULL DEFAULT 0,
      last_review TIMESTAMPTZ,
      PRIMARY KEY (flashcard_id, ord)
    )"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_card_schedules_due
                      ON card_schedules(due) WHERE due IS NOT NULL"])

  ;; kg_reviews logs BOTH item types: exactly one of question_id / flashcard_id is
  ;; set, and a card row also carries its ord. One log table is what keeps
  ;; fsrs-daily-counts — and so the shared daily cap — a single query.
  (jdbc/execute! ds ["ALTER TABLE kg_reviews ALTER COLUMN question_id DROP NOT NULL"])
  (jdbc/execute! ds ["ALTER TABLE kg_reviews ADD COLUMN IF NOT EXISTS flashcard_id INTEGER
                      REFERENCES flashcards(id) ON DELETE CASCADE"])
  (jdbc/execute! ds ["ALTER TABLE kg_reviews ADD COLUMN IF NOT EXISTS ord SMALLINT"])
  ;; Which arm produced the rating. Without it a self-graded Good (rating 3, NULL
  ;; verdict) cannot be told apart from a row written before this column existed.
  (jdbc/execute! ds ["ALTER TABLE kg_reviews ADD COLUMN IF NOT EXISTS grade_source TEXT
                      NOT NULL DEFAULT 'ai'"])
  ;; Drop-then-add is the idempotent shape for a named CHECK (ADD CONSTRAINT has no
  ;; IF NOT EXISTS).
  (jdbc/execute! ds ["ALTER TABLE kg_reviews DROP CONSTRAINT IF EXISTS kg_reviews_grade_source"])
  (jdbc/execute! ds ["ALTER TABLE kg_reviews ADD CONSTRAINT kg_reviews_grade_source
                      CHECK (grade_source IN ('ai','self'))"])
  (jdbc/execute! ds ["ALTER TABLE kg_reviews DROP CONSTRAINT IF EXISTS kg_reviews_one_item"])
  (jdbc/execute! ds ["ALTER TABLE kg_reviews ADD CONSTRAINT kg_reviews_one_item
                      CHECK ((question_id IS NULL) <> (flashcard_id IS NULL)
                             AND (flashcard_id IS NULL OR ord IS NOT NULL))"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_kg_reviews_card
                      ON kg_reviews(flashcard_id, ord, reviewed_at DESC)
                      WHERE flashcard_id IS NOT NULL"]))

(defn- setup-schema-video!
  "Incremental Video (plans/incremental-video.md §4.1): the large-object anchor,
   the reference transcript, extract ranges, chunked-upload sessions, and the
   two storage-metering columns on users.

   Video bytes live in a Postgres large object, NOT in topic_files — `bytea`
   caps at 1 GB and cannot be range-read. topic_files still holds the
   ffmpeg-extracted MP3 under role='audio', which is small and read whole."
  []
  ;; One row per kind='video' topic. lo_oid is the large object; usage_bytes
  ;; counts byte_size, so the two must be written and cleared together.
  ;; last_pos_ms is the resume position (§4.1 Q2) — a column rather than a
  ;; settings key so it lives and dies with the video.
  (let [video-table-existed?
        (some? (jdbc/execute-one! ds
                 ["SELECT 1 FROM information_schema.tables
                   WHERE table_name = 'topic_videos' AND table_schema = 'public'"]))]
    (jdbc/execute! ds ["
      CREATE TABLE IF NOT EXISTS topic_videos (
        topic_id INTEGER PRIMARY KEY REFERENCES topics(id) ON DELETE CASCADE,
        lo_oid OID NOT NULL,
        byte_size BIGINT NOT NULL,
        mime_type TEXT NOT NULL,
        duration_ms INTEGER,
        last_pos_ms INTEGER NOT NULL DEFAULT 0,
        created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
      )"])
    (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_topic_videos_oid ON topic_videos(lo_oid)"])

    ;; §12.4 3.1 — TRUE while the stored bytes are still the ones the browser
    ;; uploaded and the pipeline may yet replace them with a remuxed MP4. The
    ;; playback handler reads it to decide whether the response may be cached:
    ;; `get-video-handler`'s 24-hour `max-age` rests on "the bytes behind a
    ;; topic id never change", which is true only once this clears.
    (jdbc/execute! ds ["ALTER TABLE topic_videos
                        ADD COLUMN IF NOT EXISTS remux_pending BOOLEAN NOT NULL DEFAULT FALSE"])

    ;; §4.2 2.1 — usage_bytes is now SUM(topic_files.file_size) +
    ;; SUM(topic_videos.byte_size). Recomputed once, at the moment the table is
    ;; introduced: the video term is provably 0 then, so this only repairs
    ;; pre-existing drift, and every later mutation maintains the invariant.
    (when-not video-table-existed?
      (tel/log! :info "Recomputing users.usage_bytes over topic_files + topic_videos")
      (jdbc/execute! ds
        ["UPDATE users SET usage_bytes = COALESCE(
            (SELECT SUM(tf.file_size)
             FROM topic_files tf JOIN topics t ON tf.topic_id = t.id
             WHERE t.user_id = users.id), 0)
          + COALESCE(
            (SELECT SUM(tv.byte_size)
             FROM topic_videos tv JOIN topics t ON tv.topic_id = t.id
             WHERE t.user_id = users.id), 0)"])))

  ;; Reference transcript — one row per Whisper segment (§4.1 1.2 / decision B1).
  ;; `ord` is the position within the whole video after chunk offsets are folded
  ;; in, so (topic_id, ord) is the natural read order and the overlap query's
  ;; index. Word-level timestamps would be a second table; the shape admits it.
  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS video_transcripts (
      id BIGSERIAL PRIMARY KEY,
      topic_id INTEGER NOT NULL REFERENCES topics(id) ON DELETE CASCADE,
      ord INTEGER NOT NULL,
      start_ms INTEGER NOT NULL,
      end_ms INTEGER NOT NULL,
      text TEXT NOT NULL
    )"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_video_transcripts_topic_ord
                      ON video_transcripts(topic_id, ord)"])

  ;; Extract ranges (§4.1 1.3 / decision C3). topic_id is the EXTRACT topic, not
  ;; the video — one range per extract. The range is provenance: the extract's
  ;; text was copied in at creation and is editable thereafter, so the range
  ;; never rewrites content (decision "derivation is initial-only").
  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS video_segments (
      id BIGSERIAL PRIMARY KEY,
      topic_id INTEGER NOT NULL REFERENCES topics(id) ON DELETE CASCADE,
      start_ms INTEGER NOT NULL,
      end_ms INTEGER NOT NULL,
      created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
    )"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_video_segments_topic
                      ON video_segments(topic_id)"])

  ;; Chunked-upload sessions (§4.3). The large object is created — and the
  ;; quota reserved — at init, so an abandoned session leaks BOTH until the
  ;; sweep reaps it (§4.3 3.3). received_bytes is the resume cursor (§4.3 3.2).
  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS video_upload_sessions (
      id TEXT PRIMARY KEY,
      user_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,
      lo_oid OID NOT NULL,
      total_bytes BIGINT NOT NULL,
      received_bytes BIGINT NOT NULL DEFAULT 0,
      filename TEXT NOT NULL,
      mime_type TEXT NOT NULL,
      parent_id INTEGER REFERENCES topics(id) ON DELETE CASCADE,
      created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
      updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
    )"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_video_upload_sessions_updated
                      ON video_upload_sessions(updated_at)"])

  ;; §4.1 1.4 — storage metering. last_metered_at seeds to now() for existing
  ;; users so the first accrual bills from migration forward, never retroactively.
  ;; storage_grace_started_at is NULL except while a zero-balance user's bytes
  ;; are counting down to reclamation.
  (jdbc/execute! ds ["ALTER TABLE users ADD COLUMN IF NOT EXISTS last_metered_at TIMESTAMP"])
  (jdbc/execute! ds ["ALTER TABLE users ADD COLUMN IF NOT EXISTS storage_grace_started_at TIMESTAMP"])
  ;; Sub-unit carry, in millionths of an IQD. Credits are whole IQD, but one
  ;; hourly tick of a small library prices below 1 IQD; without a carry the
  ;; charge would round to zero every hour forever and the storage meter would
  ;; collect nothing from exactly the users it was written for.
  (jdbc/execute! ds ["ALTER TABLE users ADD COLUMN IF NOT EXISTS
                      storage_debt_micro BIGINT NOT NULL DEFAULT 0"])
  (jdbc/execute! ds ["UPDATE users SET last_metered_at = now() WHERE last_metered_at IS NULL"])
)

(defn setup-schema []
  (tel/log! :info "Setting up database schema")

  (setup-schema-core!)
  (setup-schema-topics!)
  (setup-schema-card-versions!)
  (setup-schema-activity!)
  (setup-schema-assistant!)
  (setup-schema-annotations!)
  (setup-schema-sources!)
  (setup-schema-credits!)
  (setup-schema-undo!)
  (setup-schema-kg!)
  (setup-schema-card-quiz!)
  (setup-schema-video!)

  ;; One-time grandfather credit grant (idempotent; no-op when credits disabled)
  (run-grandfather-migration!)

  ;; Card-history precondition: inline base64 images out of card fields, so no
  ;; version row can ever duplicate an image payload. Idempotent; must run
  ;; before any save can version (plans/card-edit-history.md §3).
  (normalize-inline-card-images!)

  ;; Hourly purge of staged documents whose undo window has elapsed.
  (start-purge-scheduler!)

  (tel/log! :info "Database ready"))

;; ---------------------------------------------------------------------------
;; Data migration (old tables → topics)
;; ---------------------------------------------------------------------------

(defn migrate-to-topics!
  "Migrate documents, pages, content_items → unified topics table.
   Preserves document IDs. Pages and content_items get new IDs."
  []
  (tel/log! :info "Starting migration to unified topics model")
  (jdbc/with-transaction [tx ds]
    ;; 1. Migrate documents → topics (preserve IDs)
    (let [doc-count (:count (jdbc/execute-one! tx ["SELECT COUNT(*) AS count FROM documents"]))]
      (tel/log! {:level :info :id ::migrate-documents :data {:count doc-count}} "Migrating documents")
      (jdbc/execute! tx
        ["INSERT INTO topics (id, user_id, parent_id, kind, title, content, source_url,
                              status, priority, interval_days, a_factor, next_review_at, last_review_at,
                              created_at)
          SELECT id, user_id, NULL,
                 CASE source_type WHEN 'topic' THEN 'basic' WHEN 'pdf' THEN 'pdf'
                                  WHEN 'epub' THEN 'epub' WHEN 'web' THEN 'web'
                                  ELSE COALESCE(source_type, 'basic') END,
                 COALESCE(filename, 'Untitled'), html_content, source_url,
                 COALESCE(status, 'active'), COALESCE(priority, 50),
                 COALESCE(interval_days, 1.0), COALESCE(a_factor, 2.0),
                 next_review_at, last_review_at, uploaded_at
          FROM documents
          ON CONFLICT (id) DO NOTHING"]))

    ;; Reset sequence past max document ID to avoid conflicts with auto-generated IDs
    (jdbc/execute! tx ["SELECT setval('topics_id_seq', (SELECT COALESCE(MAX(id), 0) FROM topics))"])

    ;; 2. Migrate document files → topic_files
    (let [file-count (:count (jdbc/execute-one! tx
                               ["SELECT COUNT(*) AS count FROM documents WHERE file_data IS NOT NULL"]))]
      (tel/log! {:level :info :id ::migrate-files :data {:count file-count}} "Migrating document files")
      (jdbc/execute! tx
        ["INSERT INTO topic_files (topic_id, file_data, file_size, mime_type)
          SELECT id, file_data, file_size, mime_type
          FROM documents WHERE file_data IS NOT NULL
          ON CONFLICT (topic_id) DO NOTHING"]))

    ;; 3. Migrate pages → topics (new IDs, build mapping)
    (let [page-count (:count (jdbc/execute-one! tx ["SELECT COUNT(*) AS count FROM pages"]))]
      (tel/log! {:level :info :id ::migrate-pages :data {:count page-count}} "Migrating pages")
      ;; Create temp mapping table
      (jdbc/execute! tx ["CREATE TEMP TABLE page_id_map (old_page_id INTEGER PRIMARY KEY, new_topic_id INTEGER)"])
      ;; Insert pages as topics one by one and build mapping
      (let [pages (jdbc/execute! tx
                    ["SELECT p.id AS page_id, p.document_id, p.page_number, p.text, p.is_done,
                              p.priority, p.interval_days, p.a_factor, p.next_review_at,
                              p.last_review_at, p.created_at, d.user_id
                       FROM pages p JOIN documents d ON p.document_id = d.id
                       ORDER BY p.id"]
                    {:builder-fn rs/as-unqualified-maps})]
        (doseq [p pages]
          (let [new-topic (jdbc/execute-one! tx
                            ["INSERT INTO topics (user_id, parent_id, kind, title, content, page_number,
                                                  status, priority, interval_days, a_factor,
                                                  next_review_at, last_review_at, created_at)
                              VALUES (?, ?, 'page', ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
                              ON CONFLICT (parent_id, page_number) WHERE page_number IS NOT NULL
                              DO NOTHING
                              RETURNING id"
                             (:user_id p) (:document_id p) (str "Page " (:page_number p))
                             (:text p) (:page_number p)
                             (if (true? (:is_done p)) "done" "active")
                             (or (:priority p) 50)
                             (or (:interval_days p) 1.0)
                             (or (:a_factor p) 2.0)
                             (:next_review_at p) (:last_review_at p)
                             (:created_at p)]
                            {:builder-fn rs/as-unqualified-maps})]
            (when-let [new-id (:id new-topic)]
              (jdbc/execute! tx
                ["INSERT INTO page_id_map (old_page_id, new_topic_id) VALUES (?, ?)
                    ON CONFLICT DO NOTHING"
                 (:page_id p) new-id]))))))

    ;; 4. Migrate content_items → topics (two passes)
    (let [ci-count (:count (jdbc/execute-one! tx ["SELECT COUNT(*) AS count FROM content_items"]))]
      (tel/log! {:level :info :id ::migrate-content-items :data {:count ci-count}} "Migrating content items")
      (jdbc/execute! tx ["CREATE TEMP TABLE ci_id_map (old_ci_id INTEGER PRIMARY KEY, new_topic_id INTEGER)"])

      ;; Pass 1: content_items without parent_content_item_id
      (let [items (jdbc/execute! tx
                    ["SELECT ci.id, ci.document_id, ci.page_number, ci.kind, ci.content,
                            ci.status, ci.priority, ci.interval_days, ci.a_factor,
                            ci.next_review_at, ci.last_review_at,
                            ci.created_at, d.user_id
                     FROM content_items ci JOIN documents d ON ci.document_id = d.id
                     WHERE ci.parent_content_item_id IS NULL
                     ORDER BY ci.id"]
                    {:builder-fn rs/as-unqualified-maps})]
        (doseq [ci items]
          ;; Find parent: if page_number is set, look up the page topic; otherwise use document_id
          (let [parent-id (if-let [pn (:page_number ci)]
                            (or (:new_topic_id
                                 (jdbc/execute-one! tx
                                   ["SELECT new_topic_id FROM page_id_map pm
                                      JOIN pages p ON pm.old_page_id = p.id
                                      WHERE p.document_id = ? AND p.page_number = ?"
                                    (:document_id ci) pn]
                                   {:builder-fn rs/as-unqualified-maps}))
                                ;; Fallback: look up directly in topics
                              (:id
                               (jdbc/execute-one! tx
                                 ["SELECT id FROM topics WHERE parent_id = ? AND page_number = ?"
                                  (:document_id ci) pn]
                                 {:builder-fn rs/as-unqualified-maps}))
                                ;; Last resort: use document as parent
                              (:document_id ci))
                            (:document_id ci))
                ci-kind (or (:kind ci) "html")
                title (let [raw (or (:content ci) "")]
                        (-> raw
                          (str/replace #"<[^>]+>" "")
                          (subs 0 (min 80 (count (str/replace raw #"<[^>]+>" ""))))
                          str/trim
                          (#(if (str/blank? %) "Extract" %))))
                new-topic (jdbc/execute-one! tx
                            (sql/format {:insert-into :topics
                                         :values [{:user_id (:user_id ci)
                                                   :parent_id parent-id
                                                   :kind "basic"
                                                   :title title
                                                   :content (sanitize-utf8 (:content ci))
                                                   :status (or (:status ci) "active")
                                                   :priority (or (:priority ci) 50)
                                                   :interval_days (or (:interval_days ci) 1.0)
                                                   :a_factor (or (:a_factor ci) 2.0)
                                                   :next_review_at (:next_review_at ci)
                                                   :last_review_at (:last_review_at ci)
                                                   :created_at (:created_at ci)}]
                                         :returning [:id]}
                              {:builder-fn rs/as-unqualified-maps}))]
            (when-let [new-id (:id new-topic)]
              (jdbc/execute! tx
                ["INSERT INTO ci_id_map (old_ci_id, new_topic_id) VALUES (?, ?)
                  ON CONFLICT DO NOTHING"
                 (:id ci) new-id])))))

      ;; Pass 2: content_items with parent_content_item_id
      (let [items (jdbc/execute! tx
                    ["SELECT ci.id, ci.document_id, ci.page_number, ci.kind, ci.content,
                            ci.parent_content_item_id, ci.status, ci.priority,
                            ci.interval_days, ci.a_factor, ci.next_review_at,
                            ci.last_review_at, ci.created_at, d.user_id
                     FROM content_items ci JOIN documents d ON ci.document_id = d.id
                     WHERE ci.parent_content_item_id IS NOT NULL
                     ORDER BY ci.id"]
                    {:builder-fn rs/as-unqualified-maps})]
        (doseq [ci items]
          (let [parent-id (or (:new_topic_id
                               (jdbc/execute-one! tx
                                 ["SELECT new_topic_id FROM ci_id_map WHERE old_ci_id = ?"
                                  (:parent_content_item_id ci)]
                                 {:builder-fn rs/as-unqualified-maps}))
                            (:document_id ci))
                title (let [raw (or (:content ci) "")]
                        (-> raw
                          (str/replace #"<[^>]+>" "")
                          (subs 0 (min 80 (count (str/replace raw #"<[^>]+>" ""))))
                          str/trim
                          (#(if (str/blank? %) "Extract" %))))
                new-topic (jdbc/execute-one! tx
                            (sql/format {:insert-into :topics
                                         :values [{:user_id (:user_id ci)
                                                   :parent_id parent-id
                                                   :kind "basic"
                                                   :title title
                                                   :content (sanitize-utf8 (:content ci))
                                                   :status (or (:status ci) "active")
                                                   :priority (or (:priority ci) 50)
                                                   :interval_days (or (:interval_days ci) 1.0)
                                                   :a_factor (or (:a_factor ci) 2.0)
                                                   :next_review_at (:next_review_at ci)
                                                   :last_review_at (:last_review_at ci)
                                                   :created_at (:created_at ci)}]
                                         :returning [:id]}
                              {:builder-fn rs/as-unqualified-maps}))]
            (when-let [new-id (:id new-topic)]
              (jdbc/execute! tx
                ["INSERT INTO ci_id_map (old_ci_id, new_topic_id) VALUES (?, ?)
                  ON CONFLICT DO NOTHING"
                 (:id ci) new-id]))))))

    ;; 5. Migrate flashcards — set topic_id and root_topic_id
    (tel/log! :info "Migrating flashcards")
    ;; Cards linked to content_items
    (jdbc/execute! tx
      ["UPDATE flashcards f SET
          topic_id = m.new_topic_id,
          root_topic_id = f.document_id
        FROM ci_id_map m
        WHERE f.content_item_id = m.old_ci_id
          AND f.topic_id IS NULL"])
    ;; Cards linked to pages (no content_item_id)
    (jdbc/execute! tx
      ["UPDATE flashcards f SET
          topic_id = pm.new_topic_id,
          root_topic_id = f.document_id
        FROM page_id_map pm
        JOIN pages p ON pm.old_page_id = p.id
        WHERE f.content_item_id IS NULL
          AND f.document_id = p.document_id
          AND f.page_number = p.page_number
          AND f.topic_id IS NULL"])
    ;; Any remaining cards without topic_id — set root_topic_id at least
    (jdbc/execute! tx
      ["UPDATE flashcards SET root_topic_id = document_id
        WHERE root_topic_id IS NULL AND document_id IS NOT NULL"])
    ;; Backfill: cards with null topic_id get topic_id = root_topic_id
    (jdbc/execute! tx
      ["UPDATE flashcards SET topic_id = root_topic_id
        WHERE topic_id IS NULL AND root_topic_id IS NOT NULL"])

    ;; 5b. Fix web/wikipedia articles: reparent extracts from orphan "Page 1" topics to root
    ;; Old web/wikipedia imports created dummy page rows; migration turned them into page topics
    (let [reparented (jdbc/execute-one! tx
                       ["UPDATE topics SET parent_id = page_topics.parent_id
                         FROM (
                           SELECT p.id as page_id, p.parent_id
                           FROM topics p
                           JOIN topics r ON p.parent_id = r.id
                           WHERE p.kind = 'page' AND r.kind IN ('web', 'wikipedia')
                         ) AS page_topics
                         WHERE topics.parent_id = page_topics.page_id"])]
      (when (pos? (or (:next.jdbc/update-count reparented) 0))
        (tel/log! {:level :info :id ::reparent-extracts :data {:count (:next.jdbc/update-count reparented)}}
          "Reparented extracts from orphan page topics")))
    (jdbc/execute! tx
      ["DELETE FROM topics
        WHERE kind = 'page'
          AND parent_id IN (SELECT id FROM topics WHERE kind IN ('web', 'wikipedia'))"])

    ;; 6. Reset sequence
    (jdbc/execute! tx
      ["SELECT setval('topics_id_seq', GREATEST((SELECT COALESCE(MAX(id), 1) FROM topics), 1))"])

    ;; 7. Rename old tables
    (tel/log! :info "Renaming old tables")
    (jdbc/execute! tx ["ALTER TABLE IF EXISTS content_items RENAME TO content_items_old"])
    (jdbc/execute! tx ["ALTER TABLE IF EXISTS pages RENAME TO pages_old"])
    (jdbc/execute! tx ["ALTER TABLE IF EXISTS documents RENAME TO documents_old"])

    ;; Clean up temp tables
    (jdbc/execute! tx ["DROP TABLE IF EXISTS page_id_map"])
    (jdbc/execute! tx ["DROP TABLE IF EXISTS ci_id_map"])

    ;; Print stats
    (let [topic-count (:count (jdbc/execute-one! tx ["SELECT COUNT(*) AS count FROM topics"]))
          file-count (:count (jdbc/execute-one! tx ["SELECT COUNT(*) AS count FROM topic_files"]))
          card-count (:count (jdbc/execute-one! tx ["SELECT COUNT(*) AS count FROM flashcards WHERE topic_id IS NOT NULL"]))]
      (tel/log! {:level :info :id ::migration-complete
                 :data {:topics topic-count :files file-count :flashcards card-count}}
        "Migration complete"))))

;; ---------------------------------------------------------------------------
;; Settings (unchanged)
;; ---------------------------------------------------------------------------

(defn get-setting [user-id key]
  (-> (jdbc/execute-one! ds
        (sql/format {:select [:value]
                     :from [:settings]
                     :where [:and
                             [:= :user_id user-id]
                             [:= :key key]]}))
    :settings/value))

(defn set-setting
  "Upsert a per-user setting. Audits every change here (§3.4) — the single
   choke point for all settings/save-* callers, so no per-caller log is needed."
  [user-id key value]
  (let [r (jdbc/execute! ds
            (sql/format {:insert-into :settings
                         :values [{:user_id user-id :key key :value value}]
                         :on-conflict [:user_id :key]
                         :do-update-set {:value value
                                         :updated_at [:now]}}))]
    (log/audit! {:id ::setting-changed :user-id user-id :action :update
                 :entity :setting :entity-id key})
    r))

(defn delete-setting
  "Delete a per-user setting (settings reset path). Audits here (§3.4)."
  [user-id key]
  (let [r (jdbc/execute! ds
            (sql/format {:delete-from :settings
                         :where [:and
                                 [:= :user_id user-id]
                                 [:= :key key]]}))]
    (log/audit! {:id ::setting-deleted :user-id user-id :action :delete
                 :entity :setting :entity-id key})
    r))

(defn get-settings
  "Batch variant of get-setting: {key value} for all of `ks` in one query.
   Keys with no row are absent from the result map. [] ks → {} (no DB hit)."
  [user-id ks]
  (if (empty? ks)
    {}
    (into {}
      (map (juxt :settings/key :settings/value))
      (jdbc/execute! ds
        (sql/format {:select [:key :value]
                     :from [:settings]
                     :where [:and
                             [:= :user_id user-id]
                             [:in :key ks]]})))))

;; ---------------------------------------------------------------------------
;; Assistant chats (Socratic AI assistant)
;; ---------------------------------------------------------------------------

(defn create-assistant-chat!
  "Insert a chat for (user-id, root-topic-id) with `title` and `mode`.
   Pre:  `mode` is an assistant-modes :id — the caller validates, this fn does
         not (freememo.assistant/start-chat! resolves it first).
   Post: the row's mode = `mode`. Returns new id."
  [user-id root-topic-id title mode]
  (-> (jdbc/execute-one! ds
        (sql/format {:insert-into :assistant_chats
                     :values [{:user_id user-id
                               :root_topic_id root-topic-id
                               :title title
                               :mode mode}]
                     :returning [:id]}))
    :assistant_chats/id))

(defn get-assistant-chat
  "Chat row for `chat-id` IFF owned by `user-id`, else nil (ownership gate).
   Post: {:assistant_chats/id … :assistant_chats/title … :assistant_chats/mode …}
   or nil. :mode may be NULL on a chat that predates the column — callers
   resolve it through assistant-modes/resolve-mode, never read it raw."
  [user-id chat-id]
  (jdbc/execute-one! ds
    (sql/format {:select [:id :root_topic_id :title :mode]
                 :from [:assistant_chats]
                 :where [:and [:= :id chat-id] [:= :user_id user-id]]})))

(defn get-assistant-chats
  "Chats for (user-id, root-topic-id), newest-touched first, without bodies.
   :mode rides along so the panel reads the active chat's persona out of this
   already-watched list instead of issuing a second query for it."
  [user-id root-topic-id]
  (jdbc/execute! ds
    (sql/format {:select [:id :title :updated_at :mode]
                 :from [:assistant_chats]
                 :where [:and [:= :user_id user-id] [:= :root_topic_id root-topic-id]]
                 :order-by [[[:coalesce :updated_at :created_at] :desc] [:id :desc]]})))

(defn insert-assistant-message!
  "Append a message and touch the parent chat's updated_at. Returns new id.
   Pre: role ∈ {\"user\",\"assistant\"} (enforced by table CHECK); `client-id`
   is the client's echo-correlation id for a user turn, nil for assistant rows.
   Post: the row's client_id = `client-id`."
  [chat-id role content client-id]
  (let [id (-> (jdbc/execute-one! ds
                 (sql/format {:insert-into :assistant_messages
                              :values [{:chat_id chat-id :role role :content content
                                        :client_id client-id}]
                              :returning [:id]}))
             :assistant_messages/id)]
    (jdbc/execute! ds
      (sql/format {:update :assistant_chats
                   :set {:updated_at [:now]}
                   :where [:= :id chat-id]}))
    id))

(defn get-assistant-messages
  "Transcript for `chat-id`, oldest first (learner/assistant turns only —
   reading context is injected transiently per send, not stored here)."
  [chat-id]
  (jdbc/execute! ds
    (sql/format {:select [:id :role :content :client_id]
                 :from [:assistant_messages]
                 :where [:= :chat_id chat-id]
                 :order-by [[:id :asc]]})))

(defn set-assistant-chat-title!
  "Rename a chat."
  [chat-id title]
  (jdbc/execute! ds
    (sql/format {:update :assistant_chats
                 :set {:title title}
                 :where [:= :id chat-id]})))

(defn set-assistant-chat-mode!
  "Switch a chat's persona IFF owned by `user-id`.
   Pre:  `mode` is an assistant-modes :id — validated by the caller
         (freememo.assistant/set-chat-mode!).
   Post: the row's mode = `mode` when owned; nothing changes otherwise. Does NOT
   touch updated_at — switching a persona is not activity in the chat, and the
   picker orders on that column."
  [user-id chat-id mode]
  (jdbc/execute! ds
    (sql/format {:update :assistant_chats
                 :set {:mode mode}
                 :where [:and [:= :id chat-id] [:= :user_id user-id]]})))

(defn delete-assistant-chat!
  "Delete a chat (cascades to messages) IFF owned by `user-id`."
  [user-id chat-id]
  (jdbc/execute! ds
    (sql/format {:delete-from :assistant_chats
                 :where [:and [:= :id chat-id] [:= :user_id user-id]]})))

(defn list-email-update-recipients
  "Email addresses of users who opted into email updates (settings key
   'email_updates' = 'true') and have a non-blank email.
   Post: vector of distinct email strings (possibly empty)."
  []
  (->> (jdbc/execute! ds
         (sql/format {:select-distinct [:users.email]
                      :from [:users]
                      :join [:settings [:= :settings.user_id :users.id]]
                      :where [:and
                              [:= :settings.key "email_updates"]
                              [:= :settings.value "true"]
                              [:<> :users.email nil]
                              [:<> [:trim :users.email] ""]]}))
    (map :users/email)
    (remove str/blank?)
    vec))

;; ---------------------------------------------------------------------------
;; Credits — pass-through AI billing (see plans/credits-wayl-payment-system.md §5)
;; ---------------------------------------------------------------------------

(defn get-credit-balance
  "Current IQD credit balance for a user. 0 for unknown users.
   `connectable` defaults to the pool; pass a tx for transactional reads."
  ([user-id] (get-credit-balance ds user-id))
  ([connectable user-id]
   (or (:users/credit_balance_iqd
         (jdbc/execute-one! connectable
           ["SELECT credit_balance_iqd FROM users WHERE id = ?" user-id]))
     0)))

(defn list-credit-transactions
  "User's credit ledger newest-first, for the Settings → Account 'AI costs' view.
   Optional filters (nil/blank = no constraint): `kind`/`endpoint`/`model`
   exact-match; `since-days` keeps rows newer than N days; `min-amount` keeps
   rows whose magnitude |amount_iqd| >= min-amount (e.g. 500 finds 500+ debits
   or credits); `q` case-insensitive substring over endpoint OR model. Selects
   only display columns (no reasoning_tokens/attempts/order ref — out of scope).
   Pre:  user-id identifies a user; since-days/min-amount nil or non-negative.
   Post: ≤ 5000 unqualified-key maps ordered created_at DESC, id DESC (stable
         window tiebreak for same-timestamp rows). Over-cap ledgers are
         truncated with a :warn log.
   Note: the since-days boundary uses the app JVM clock, which is colocated with
         Postgres in this deployment; a cross-tz split would shift it by hours."
  [user-id {:keys [kind endpoint model since-days min-amount q]}]
  (let [cap 5000
        like (when-not (str/blank? q) (str "%" (str/trim q) "%"))
        cutoff (when since-days
                 (java.sql.Timestamp/valueOf
                   (.minusDays (java.time.LocalDateTime/now) (long since-days))))
        where (cond-> [:and [:= :user_id user-id]]
                (not (str/blank? kind))     (conj [:= :kind kind])
                (not (str/blank? endpoint)) (conj [:= :endpoint endpoint])
                (not (str/blank? model))    (conj [:= :model model])
                cutoff                      (conj [:>= :created_at cutoff])
                (and min-amount (pos? min-amount)) (conj [:>= [:abs :amount_iqd] min-amount])
                like                        (conj [:or [:ilike :endpoint like]
                                                   [:ilike :model like]]))
        rows (jdbc/execute! ds
               (sql/format {:select [:id :kind :amount_iqd :balance_after
                                     :endpoint :model :input_tokens :cached_tokens
                                     :output_tokens :created_at]
                            :from [:credit_transactions]
                            :where where
                            :order-by [[:created_at :desc] [:id :desc]]
                            :limit (inc cap)})
               {:builder-fn rs/as-unqualified-maps})]
    (when (> (count rows) cap)
      (tel/log! {:level :warn :id ::credit-history-truncated
                 :data {:user-id user-id :cap cap}}
        "Credit history exceeded display cap; truncated"))
    (vec (take cap rows))))

(defn credit-transaction-facets
  "Distinct non-empty endpoints and models across a user's whole ledger — the
   'AI costs' filter dropdown options. Filter-independent by design so options
   never vanish as the user narrows.
   Pre:  user-id. Post: {:endpoints [str…] :models [str…]}, each sorted asc,
         no nils/blanks."
  [user-id]
  (let [distinct-col (fn [col]
                       (->> (jdbc/execute! ds
                              (sql/format {:select-distinct [col]
                                           :from [:credit_transactions]
                                           :where [:and [:= :user_id user-id]
                                                   [:<> col nil] [:<> col ""]]
                                           :order-by [[col :asc]]})
                              {:builder-fn rs/as-unqualified-maps})
                         (map col)
                         (remove str/blank?)
                         vec))]
    {:endpoints (distinct-col :endpoint)
     :models    (distinct-col :model)}))

(defn get-user-markup
  "Per-user markup override, or nil when unset (caller falls back to config).
   Returns a BigDecimal (Postgres DECIMAL)."
  [user-id]
  (:users/markup_override
    (jdbc/execute-one! ds
      ["SELECT markup_override FROM users WHERE id = ?" user-id])))

(defn set-user-markup!
  "Set or clear a user's markup override (REPL/SQL admin path — no UI yet).
   Pass nil to clear and fall back to the config default."
  [user-id markup]
  (jdbc/execute! ds
    ["UPDATE users SET markup_override = ? WHERE id = ?" markup user-id]))

(defn- sum-tokens [maps k] (reduce + 0 (map #(or (k %) 0) maps)))

(defn debit-credits!
  "Atomic debit for a completed AI action. Locks the balance row, subtracts
   `cost-iqd`, appends a 'debit' ledger row with summed token detail.
   Pre:  cost-iqd >= 0; attempts is a non-empty seq of token maps.
   Post: balance decreased by cost-iqd (may go negative — the gate allows one
         overshoot); ledger row written. Returns the new balance.
   Invariant: between SELECT FOR UPDATE and UPDATE no other tx touches the row."
  [user-id cost-iqd {:keys [endpoint model attempts]}]
  (jdbc/with-transaction [tx ds]
    (let [locked (jdbc/execute-one! tx
                   ["SELECT credit_balance_iqd FROM users WHERE id = ? FOR UPDATE" user-id])
          bal (or (:users/credit_balance_iqd locked) 0)
          new-bal (- bal cost-iqd)]
      (jdbc/execute! tx
        ["UPDATE users SET credit_balance_iqd = ? WHERE id = ?" new-bal user-id])
      (jdbc/execute! tx
        (sql/format {:insert-into :credit_transactions
                     :values [{:user_id user-id
                               :kind "debit"
                               :amount_iqd (- cost-iqd)
                               :balance_after new-bal
                               :endpoint endpoint
                               :model model
                               :input_tokens (sum-tokens attempts :prompt-tokens)
                               :cached_tokens (sum-tokens attempts :cached-tokens)
                               :output_tokens (sum-tokens attempts :completion-tokens)
                               :reasoning_tokens (sum-tokens attempts :reasoning-tokens)
                               :attempts (count attempts)}]}))
      new-bal)))

(defn credit-account!
  "Add `amount-iqd` to a user's balance + append a credit ledger row.
   `kind` is \"purchase\" | \"grant\" | \"adjustment\". `connectable` MUST be a
   live transaction (the SELECT FOR UPDATE requires it) — webhook crediting
   reuses the order-completion tx; grants wrap their own.
   Pre: amount-iqd > 0. Post: balance increased; ledger row written.
   Returns the new balance."
  [connectable user-id amount-iqd kind {:keys [order-reference-id]}]
  (let [locked (jdbc/execute-one! connectable
                 ["SELECT credit_balance_iqd FROM users WHERE id = ? FOR UPDATE" user-id])
        bal (or (:users/credit_balance_iqd locked) 0)
        new-bal (+ bal amount-iqd)]
    (jdbc/execute! connectable
      ["UPDATE users SET credit_balance_iqd = ? WHERE id = ?" new-bal user-id])
    (jdbc/execute! connectable
      (sql/format {:insert-into :credit_transactions
                   :values [{:user_id user-id :kind kind :amount_iqd amount-iqd
                             :balance_after new-bal :order_reference_id order-reference-id}]}))
    new-bal))

(defn insert-credit-order!
  "Create a pending order. Returns the row."
  [user-id reference-id amount-iqd]
  (jdbc/execute-one! ds
    (sql/format {:insert-into :credit_orders
                 :values [{:user_id user-id :reference_id reference-id
                           :amount_iqd amount-iqd :status "pending"}]
                 :returning [:*]})))

(defn set-order-wayl-fields!
  "Record Wayl's code/link-id on a pending order after link creation."
  [reference-id wayl-code wayl-link-id]
  (jdbc/execute! ds
    ["UPDATE credit_orders SET wayl_code = ?, wayl_link_id = ? WHERE reference_id = ?"
     wayl-code wayl-link-id reference-id]))

(defn get-credit-order [reference-id]
  (jdbc/execute-one! ds
    (sql/format {:select [:*] :from [:credit_orders]
                 :where [:= :reference_id reference-id]})))

(defn complete-credit-order!
  "Idempotently mark an order complete and credit the buyer, in one tx. Locks
   the order row; a redelivered webhook (already complete) is a no-op.
   Pre:  reference-id from a verified webhook.
   Post: on first completion the order is 'complete' and balance += amount.
   Returns {:credited true :amount n :user-id u} or {:credited false :reason
            :already|:unknown}."
  [reference-id]
  (jdbc/with-transaction [tx ds]
    (let [order (jdbc/execute-one! tx
                  ["SELECT * FROM credit_orders WHERE reference_id = ? FOR UPDATE" reference-id]
                  {:builder-fn rs/as-unqualified-maps})]
      (cond
        (nil? order) {:credited false :reason :unknown}
        (= "complete" (:status order)) {:credited false :reason :already}
        :else
        (let [user-id (:user_id order)
              amount (:amount_iqd order)]
          (jdbc/execute! tx
            ["UPDATE credit_orders SET status = 'complete', completed_at = CURRENT_TIMESTAMP
              WHERE reference_id = ?" reference-id])
          (credit-account! tx user-id amount "purchase" {:order-reference-id reference-id})
          ;; §4.2 2.4 — first completed order promotes the storage cap from the
          ;; free tier to the paid tier (decision R3: the tier caps how much may
          ;; be stored; the meter prices how long). Guarded on quota_bytes IS
          ;; NULL inside the fn, so repeat purchases and admin overrides are
          ;; both no-ops. Same tx as the credit, so a rollback undoes both.
          (promote-to-paid-storage-tier! tx user-id quota/paid-quota-bytes)
          {:credited true :amount amount :user-id user-id})))))

(defn fail-credit-order!
  "Mark a pending order failed (provider error / Cancelled / Rejected / Returned).
   No-op once the order is complete."
  [reference-id]
  (jdbc/execute! ds
    ["UPDATE credit_orders SET status = 'failed'
      WHERE reference_id = ? AND status = 'pending'" reference-id]))

(defn- user-has-grant? [connectable user-id]
  (some? (jdbc/execute-one! connectable
           ["SELECT 1 FROM credit_transactions WHERE user_id = ? AND kind = 'grant' LIMIT 1"
            user-id])))

(defn grant-credits!
  "Grant `amount-iqd` to a user (own tx). Returns the new balance."
  [user-id amount-iqd]
  (jdbc/with-transaction [tx ds]
    (credit-account! tx user-id amount-iqd "grant" {})))

(defn grant-signup-credits!
  "One-time signup grant (idempotent per user). No-op when credits are disabled,
   the configured amount is 0, or the user already holds a grant."
  [user-id]
  (let [amount (config/signup-grant)]
    (when (and (config/credits-enabled?) (pos? amount)
               (not (user-has-grant? ds user-id)))
      (grant-credits! user-id amount)
      (tel/log! {:level :info :id ::signup-grant :data {:user-id user-id :amount amount}}
        "Signup credit grant"))))

(defn run-grandfather-migration!
  "One-time grant to every pre-existing user without a grant. Idempotent (the
   grant-row check excludes already-granted users). No-op when credits are
   disabled or the configured amount is 0. Called at boot from setup-schema."
  []
  (let [amount (config/grandfather-grant)]
    (when (and (config/credits-enabled?) (pos? amount))
      (let [users (jdbc/execute! ds
                    ["SELECT u.id FROM users u
                      WHERE NOT EXISTS (SELECT 1 FROM credit_transactions t
                                        WHERE t.user_id = u.id AND t.kind = 'grant')"]
                    {:builder-fn rs/as-unqualified-maps})]
        (when (seq users)
          (doseq [{:keys [id]} users]
            (grant-credits! id amount))
          (tel/log! {:level :info :id ::grandfather-grant
                     :data {:count (count users) :amount amount}}
            "Grandfather credit grant"))))))

;; ---------------------------------------------------------------------------
;; Google OAuth user queries (unchanged)
;; ---------------------------------------------------------------------------

(defn get-user-by-id [user-id]
  (jdbc/execute-one! ds
    (sql/format {:select [:id :username :google_id]
                 :from [:users]
                 :where [:= :id user-id]})))

(defn get-user-by-google-id [google-id]
  (jdbc/execute-one! ds
    (sql/format {:select [:id :username :email]
                 :from [:users]
                 :where [:= :google_id google-id]})))

(defn get-user-by-username [username]
  (jdbc/execute-one! ds
    (sql/format {:select [:id :username]
                 :from [:users]
                 :where [:= :username username]})))

(defn upsert-google-user [google-id email username]
  (jdbc/execute-one! ds
    (sql/format {:insert-into :users
                 :values [{:google_id google-id :email email :username username}]
                 :on-conflict [:google_id]
                 :do-update-set {:email email}
                 :returning [:id :username]})))

(defn set-display-name-if-absent!
  "Fill users.display_name for `user-id`, but only when it is currently NULL.
   Pre:  user-id identifies an existing users row; display-name is a string or nil.
   Post: a NULL display_name becomes the trimmed display-name; an existing
         non-NULL value is left untouched. No-op for nil/blank input.
   Invariant: a hand-curated display_name is never clobbered by a later login."
  [user-id display-name]
  (when-not (str/blank? display-name)
    (jdbc/execute! ds
      ["UPDATE users SET display_name = ? WHERE id = ? AND display_name IS NULL"
       (str/trim display-name) user-id])))

(defn get-mail-addressees
  "Rows needed to address outgoing mail to specific users.
   Pre:  user-ids is a non-empty collection of ints.
   Post: vector of {:users/id :users/email :users/display_name}, one per
         existing id, in ascending id order; ids with no row are absent, so
         callers MUST compare the count against what they asked for."
  [user-ids]
  (when (seq user-ids)
    (jdbc/execute! ds
      (sql/format {:select [:id :email :display_name]
                   :from [:users]
                   :where [:in :id (vec user-ids)]
                   :order-by [[:id :asc]]}))))

(defn user-ids-with-event
  "Set of user ids that already have at least one `event-type` row.
   Post: set of ints (possibly empty). Used for idempotent one-off sends."
  [event-type]
  (into #{}
    (map :user_events/user_id)
    (jdbc/execute! ds
      (sql/format {:select-distinct [:user_id]
                   :from [:user_events]
                   :where [:= :event_type event-type]}))))

(defn insert-user-event!
  "Record a user event. The 3-arity stores `metadata` as jsonb.
   Post: exactly one user_events row inserted."
  ([user-id event-type] (insert-user-event! user-id event-type nil))
  ([user-id event-type metadata]
   (jdbc/execute! ds
     (sql/format {:insert-into :user_events
                  :values [(cond-> {:user_id user-id :event_type event-type}
                             (some? metadata)
                             (assoc :metadata
                               [:cast (json/generate-string metadata) :jsonb]))]}))))

;; ---------------------------------------------------------------------------
;; Utility
;; ---------------------------------------------------------------------------

(defn sanitize-utf8
  "Re-encode string through UTF-8 to strip invalid byte sequences."
  [s]
  (when s
    (String. (.getBytes (str s) "UTF-8") "UTF-8")))

(defn backfill-content-text!
  "Populate topics.content_text for rows where it is NULL but content exists.
   Strips HTML via Jsoup. Batched to avoid loading all rows at once."
  []
  (let [batch-size 200]
    (loop [total 0]
      (let [rows (jdbc/execute! ds
                   ["SELECT id, content FROM topics
                      WHERE content IS NOT NULL AND content_text IS NULL
                      LIMIT ?" batch-size]
                   {:builder-fn rs/as-unqualified-maps})]
        (if (empty? rows)
          (when (pos? total)
            (tel/log! {:level :info :id ::backfill-content-text :data {:count total}}
              "Backfilled content_text"))
          (do
            (doseq [{:keys [id content]} rows]
              (jdbc/execute! ds
                ["UPDATE topics SET content_text = ? WHERE id = ?"
                 (text/strip-html content) id]))
            (recur (+ total (count rows)))))))))

;; ---------------------------------------------------------------------------
;; Topic CRUD
;; ---------------------------------------------------------------------------

(defn- audit-doc-created!
  "Audit a document (root/child topic) creation.
   Pre: user-id the owner (nil for owner-less system topics — skipped); topic-id
   the new row's id. Post: one :create audit signal (§3.2). Returns nil."
  [user-id topic-id]
  (when user-id
    (log/audit! {:id ::topic-created :user-id user-id :action :create
                 :entity :document :entity-id topic-id})))

(defn create-topic!
  "Create a topic. attrs is a map with keys:
   :user-id :kind :title :parent-id :content :page-number
   :source-id :status :priority
   :source-id attaches an existing `sources` row (bibliography).
   Returns the created row with :topics/id."
  [attrs]
  (let [clean-title (input/prettify-title (input/sanitize-filename (:title attrs)))
        _ (input/check-length! :title clean-title input/title-max)
        sanitized (when (:content attrs) (sanitize-utf8 (:content attrs)))
        row (cond-> {:kind (or (:kind attrs) "basic")
                     :title clean-title
                     :status (or (:status attrs) "active")
                     :priority (or (:priority attrs) 50)}
              (:user-id attrs) (assoc :user_id (:user-id attrs))
              (:parent-id attrs) (assoc :parent_id (:parent-id attrs))
              sanitized (assoc :content sanitized
                          :content_text (text/strip-html sanitized))
              (:page-number attrs) (assoc :page_number (:page-number attrs))
              (:source-id attrs) (assoc :source_id (:source-id attrs)))
        created (jdbc/execute-one! ds
                  (sql/format {:insert-into :topics
                               :values [row]
                               :returning [:*]}))]
    (audit-doc-created! (:user-id attrs) (:topics/id created))
    created))

(defn get-topic
  "Get a topic by ID. Excludes topics staged for deletion (hidden)."
  [id]
  (jdbc/execute-one! ds
    (sql/format {:select [:*]
                 :from [:topics]
                 :where [:and [:= :id id] [:is :staged_delete_id nil]]})))

(defn get-topic-priority
  "Current review priority for a topic (0=highest…100=lowest). Returns nil for
   a missing or staged-for-deletion topic; callers default to 50."
  [id]
  (some-> (jdbc/execute-one! ds
            (sql/format {:select [:priority]
                         :from [:topics]
                         :where [:and [:= :id id] [:is :staged_delete_id nil]]}))
    :topics/priority))

(defn get-topic-titles
  "Map {topic-id title} for `ids`, INCLUDING staged-for-deletion topics so
   undo-log sources still resolve. Missing/hard-deleted ids are simply absent."
  [ids]
  (if (empty? ids)
    {}
    (into {}
      (map (juxt :id :title))
      (jdbc/execute! ds
        (sql/format {:select [:id :title] :from [:topics] :where [:in :id (vec ids)]})
        {:builder-fn rs/as-unqualified-maps}))))

(defn get-topic-for-user
  "Get a topic by ID, scoped to a user. Excludes topics staged for deletion."
  [user-id id]
  (jdbc/execute-one! ds
    (sql/format {:select [:*]
                 :from [:topics]
                 :where [:and [:= :id id] [:= :user_id user-id] [:is :staged_delete_id nil]]})))

(def ^:private topic-file-sizes
  "Derived table summing STORED BYTES per topic. Join THIS instead of
   topic_files directly in list queries: score topics own TWO file rows
   (PDF + audio), so a bare row join fans a topic out into duplicates.

   Unions in topic_videos so a video topic reports its real footprint. A video's
   topic_files row holds only the extracted MP3 (a few MB); the video itself is
   a large object, and without this term Library would show a 700 MB video as
   5 MB — disagreeing with the same user's storage bar.

   The cast is load-bearing: `topic_videos.byte_size` is BIGINT, and Postgres'
   SUM(bigint) is NUMERIC, which arrives as a BigDecimal. Every consumer of
   :file_size was written against the Long that SUM(integer) used to return."
  {:select [:topic_id [[:cast [:sum :file_size] :bigint] :file_size]]
   :from [[{:union-all [{:select [:topic_id :file_size] :from [:topic_files]}
                        {:select [:topic_id [:byte_size :file_size]] :from [:topic_videos]}]}
           :stored_bytes]]
   :group-by [:topic_id]})

(defn get-root-topics
  "Get all root topics for a user (parent_id IS NULL). Replaces get-documents.
   Includes file_size (summed across a topic's file rows) or content length,
   plus bibliography fields joined from `sources` (NULL when topic has no
   source_id)."
  [user-id]
  (let [topics (jdbc/execute! ds
                 (sql/format {:select [:t/id :t/title :t/kind
                                       :s/url :s/title :s/csl_type :s/container_title
                                       :t/status :t/priority :t/created_at :t/content
                                       [:tf/file_size :file_size]]
                              :from [[:topics :t]]
                              :left-join [[topic-file-sizes :tf] [:= :t/id :tf/topic_id]
                                          [:sources :s] [:= :t/source_id :s/id]]
                              :where [:and [:= :t/user_id user-id] [:= :t/parent_id nil]
                                      [:is :t/staged_delete_id nil]]
                              :order-by [[:t/created_at :desc]]}))]
    (mapv (fn [t]
            (let [size (or (:file_size t)
                         (when-let [c (:topics/content t)]
                           (count (.getBytes c "UTF-8"))))]
              (-> t
                (assoc :topics/file_size size)
                (dissoc :topics/content :file_size))))
      topics)))

(defn get-children
  "Get direct children of a topic, ordered by page_number then created_at."
  [parent-id]
  (jdbc/execute! ds
    (sql/format {:select [:*]
                 :from [:topics]
                 :where [:and [:= :parent_id parent-id] [:is :staged_delete_id nil]]
                 :order-by [[:page_number :asc-nulls-last] [:created_at :asc]]})))

(defn update-topic-content!
  "Update the content of a topic.

   Strips `<span class=\"ql-token …\">` wrappers before persisting — Quill 2.0.3's
   `clipboard.convert` misreads them as `code-token: true`, corrupting render on
   reload. The browser's `syntax` module re-applies tokens on each load."
  [id content]
  (let [sanitized (sanitize-utf8 (cleaner/strip-ql-tokens content))]
    (jdbc/execute-one! ds
      (sql/format {:update :topics
                   :set {:content sanitized
                         :content_text (text/strip-html sanitized)}
                   :where [:= :id id]}))))

(defn rename-topic!
  "Rename a topic. Sanitizes and length-checks the new title.
   Throws ::blank-title when input is blank.
   Bumps updated_at on flashcards whose root_topic_id matches so Anki push
   detects the title change. For non-root ids the bump is a no-op."
  [id new-title]
  (when (or (nil? new-title) (str/blank? new-title))
    (throw (ex-info "title must not be blank"
             {:type ::blank-title :id id})))
  (let [clean-title (input/sanitize-filename new-title)]
    (input/check-length! :title clean-title input/title-max)
    (jdbc/execute-one! ds
      (sql/format {:update :topics
                   :set {:title clean-title}
                   :where [:= :id id]}))
    (jdbc/execute-one! ds
      ["UPDATE flashcards SET updated_at = CURRENT_TIMESTAMP WHERE root_topic_id = ?" id])))

(defn get-root-topic-id
  "Traverse parent_id upward to find the root topic (parent_id IS NULL).
   Returns the root topic's id, or the input id if it is already a root."
  [topic-id]
  (when topic-id
    (:id
     (jdbc/execute-one! ds
       ["WITH RECURSIVE ancestors AS (
           SELECT id, parent_id FROM topics WHERE id = ?
           UNION ALL
           SELECT t.id, t.parent_id FROM topics t
           JOIN ancestors a ON t.id = a.parent_id
         )
         SELECT id FROM ancestors WHERE parent_id IS NULL"
        topic-id]))))

(defn- bump-user-usage!
  "Adjust users.usage_bytes by delta (signed). Clamped at 0.
   Pass tx for transactional callers, ds otherwise."
  [connectable user-id delta]
  (jdbc/execute! connectable
    ["UPDATE users SET usage_bytes = GREATEST(0, usage_bytes + ?) WHERE id = ?"
     (long delta) user-id]))

(defn- subtree-file-bytes
  "Sum of stored bytes across the topic subtree rooted at id (including id).
   Returns 0 when nothing is found.

   Counts BOTH storage tables, matching the `usage_bytes` definition
   (plans/incremental-video.md §4.2 2.1): `topic_files.file_size` plus
   `topic_videos.byte_size`. This function is the deletion decrement, so
   omitting the video term would free the wrong amount and leave the counter
   permanently high by the size of every deleted video."
  [connectable id]
  (or (:sum (jdbc/execute-one! connectable
              ["WITH RECURSIVE subtree AS (
                  SELECT id FROM topics WHERE id = ?
                  UNION ALL
                  SELECT c.id FROM topics c
                  JOIN subtree s ON c.parent_id = s.id
                )
                SELECT CAST(
                  COALESCE((SELECT SUM(tf.file_size) FROM topic_files tf
                             JOIN subtree s ON tf.topic_id = s.id), 0)
                + COALESCE((SELECT SUM(tv.byte_size) FROM topic_videos tv
                             JOIN subtree s ON tv.topic_id = s.id), 0)
                  AS bigint) AS sum"
               id]))
    0))

(defn- subtree-video-oids
  "Large-object OIDs owned by the topic subtree rooted at id (including id).
   The list a hard-delete path must `lo_unlink` — Postgres will not do it when
   the topic_videos row cascades away."
  [connectable id]
  (mapv :lo_oid
    (jdbc/execute! connectable
      ["WITH RECURSIVE subtree AS (
          SELECT id FROM topics WHERE id = ?
          UNION ALL
          SELECT c.id FROM topics c JOIN subtree s ON c.parent_id = s.id
        )
        SELECT tv.lo_oid FROM topic_videos tv JOIN subtree s ON tv.topic_id = s.id"
       id]
      {:builder-fn rs/as-unqualified-maps})))

(defn delete-topic-for-user!
  "Delete a topic scoped to user. Decrements usage_bytes by the subtree's
   total stored bytes and unlinks its large objects atomically.

   §4.7 7.2: this is the hard-delete path (the staged path is
   stage-topic-for-deletion! + purge-staged-documents!). Unlinking here is safe
   precisely because the row disappears in the same transaction — there is no
   undo window to preserve."
  [user-id id]
  (jdbc/with-transaction [tx ds]
    (let [freed (subtree-file-bytes tx id)
          oids (subtree-video-oids tx id)
          result (jdbc/execute! tx
                   (sql/format {:delete-from :topics
                                :where [:and [:= :id id] [:= :user_id user-id]]}))]
      (doseq [oid oids] (lo/unlink! tx oid))
      (when (pos? freed)
        (bump-user-usage! tx user-id (- freed)))
      (log/audit! {:id ::topic-deleted :user-id user-id :action :delete
                   :entity :document :entity-id id})
      result)))

;; ---------------------------------------------------------------------------
;; File operations
;; ---------------------------------------------------------------------------

(defn get-topic-file
  "Get the binary file data for a topic (PDF/EPUB/audio). 1-arity reads the
   'main' blob — the only one non-score kinds have. Score topics carry a
   second 'audio' role."
  ([topic-id] (get-topic-file topic-id "main"))
  ([topic-id role]
   (jdbc/execute-one! ds
     (sql/format {:select [:file_data :file_size :mime_type]
                  :from [:topic_files]
                  :where [:and [:= :topic_id topic-id] [:= :role role]]}))))

(defn topic-file-exists?
  "True iff a topic has a stored binary file blob for the role ('main' default)."
  ([topic-id] (topic-file-exists? topic-id "main"))
  ([topic-id role]
   (some? (jdbc/execute-one! ds
            (sql/format {:select [[[:inline 1] :x]]
                         :from [:topic_files]
                         :where [:and [:= :topic_id topic-id] [:= :role role]]})))))

(defn count-pages
  "Number of kind='page' child topics under `parent-id` (excludes staged
   deletes). Drives the live-doc viewer's reload trigger."
  [parent-id]
  (or (:c (jdbc/execute-one! ds
            (sql/format {:select [[[:count :*] :c]]
                         :from [:topics]
                         :where [:and [:= :parent_id parent-id] [:= :kind "page"]
                                 [:is :staged_delete_id nil]]})
            {:builder-fn rs/as-unqualified-maps}))
    0))

(defn save-topic-file!
  "Store binary file for a topic. Upserts."
  [topic-id file-bytes file-size mime-type]
  (jdbc/execute! ds
    ["INSERT INTO topic_files (topic_id, file_data, file_size, mime_type)
      VALUES (?, ?, ?, ?)
      ON CONFLICT (topic_id)
      DO UPDATE SET file_data = excluded.file_data, file_size = excluded.file_size, mime_type = excluded.mime_type"
     topic-id file-bytes file-size mime-type]))

;; ---------------------------------------------------------------------------
;; Sources — bibliography records (CSL-JSON shaped)
;; ---------------------------------------------------------------------------

(defn- ->jsonb
  "Wrap a Clojure value as a PostgreSQL JSONB parameter."
  [v]
  (doto (PGobject.)
    (.setType "jsonb")
    (.setValue (json/generate-string (or v {})))))

(defn- pgobject->clj
  "Parse a PGobject (jsonb) back to a Clojure value. Keywordizes keys.
   nil → nil; non-PGobject inputs pass through."
  [v]
  (cond
    (nil? v) nil
    (instance? PGobject v) (json/parse-string (.getValue ^PGobject v) true)
    :else v))

(defn create-source!
  "Insert a sources row. attrs = {:user-id :csl-type :csl :url :title}.
   :csl is a CSL-JSON map (may be nil → {}). Returns full row with :sources/id.
   container_title is denormalized from csl.container-title."
  [{:keys [user-id csl-type csl url title] :as attrs}]
  (when (or (nil? user-id) (str/blank? csl-type))
    (throw (ex-info "create-source! requires :user-id and :csl-type"
             {:type ::invalid-source-attrs :attrs attrs})))
  (let [container (when (map? csl) (:container-title csl))
        row (jdbc/execute-one! ds
              (sql/format {:insert-into :sources
                           :values [{:user_id user-id
                                     :csl_type csl-type
                                     :csl (->jsonb (or csl {}))
                                     :url url
                                     :title title
                                     :container_title container}]
                           :returning [:*]}))]
    (log/audit! {:id ::source-created :user-id user-id :action :create
                 :entity :source :entity-id (:sources/id row)})
    (update row :sources/csl pgobject->clj)))

(defn get-source
  "Get a source by id. Parses csl JSONB to a Clojure map. Returns nil for nil id."
  [id]
  (when id
    (when-let [row (jdbc/execute-one! ds
                     (sql/format {:select [:*]
                                  :from [:sources]
                                  :where [:= :id id]}))]
      (update row :sources/csl pgobject->clj))))

(defn find-source-by-url
  "Look up a sources row for (user-id, url). Returns row or nil.
   Used at import to dedupe — same wiki article → one sources row, many topics."
  [user-id url]
  (when (and user-id (not (str/blank? url)))
    (when-let [row (jdbc/execute-one! ds
                     (sql/format {:select [:*]
                                  :from [:sources]
                                  :where [:and [:= :user_id user-id] [:= :url url]]
                                  :limit 1}))]
      (update row :sources/csl pgobject->clj))))

(defn update-source!
  "Update a sources row. attrs = {:id :csl-type :csl :url :title}. id required;
   other keys updated only when present. Bumps updated_at. container_title
   is re-denormalized when csl is provided."
  [{:keys [id csl-type csl url title]}]
  (when (nil? id) (throw (ex-info "update-source! requires :id" {:type ::missing-id})))
  (let [set-map (cond-> {:updated_at (java.sql.Timestamp. (System/currentTimeMillis))}
                  csl-type (assoc :csl_type csl-type)
                  (some? csl) (assoc :csl (->jsonb csl)
                                :container_title (when (map? csl) (:container-title csl)))
                  (some? url) (assoc :url url)
                  (some? title) (assoc :title title))
        row (jdbc/execute-one! ds
              (sql/format {:update :sources
                           :set set-map
                           :where [:= :id id]
                           :returning [:*]}))]
    (when row
      (log/audit! {:id ::source-updated :user-id (:sources/user_id row) :action :update
                   :entity :source :entity-id id}))
    (some-> row (update :sources/csl pgobject->clj))))

(defn attach-source-to-topic!
  "Set topics.source_id for a topic owned by user-id. Returns update count."
  [user-id topic-id source-id]
  (jdbc/execute-one! ds
    ["UPDATE topics SET source_id = ? WHERE id = ? AND user_id = ?"
     source-id topic-id user-id]))

(defn resolve-effective-source-id
  "Source id a topic should cite: its own source_id when set, else the nearest
   ancestor's (walking parent_id upward), else nil.
   Pre:  topic-id may be nil (→ nil).
   Post: returns a sources.id drawn from topic-id's ancestor chain (self
         included), preferring the shallowest; nil when no topic in the chain
         has a source. Legacy source-less extracts thus resolve to the root."
  [topic-id]
  (when topic-id
    (:source_id
     (jdbc/execute-one! ds
       ["WITH RECURSIVE anc(id, parent_id, source_id, depth) AS (
           SELECT id, parent_id, source_id, 0 FROM topics WHERE id = ?
           UNION ALL
           SELECT t.id, t.parent_id, t.source_id, a.depth + 1
             FROM topics t JOIN anc a ON t.id = a.parent_id)
         SELECT source_id FROM anc
          WHERE source_id IS NOT NULL
          ORDER BY depth
          LIMIT 1"
        topic-id]
       {:builder-fn rs/as-unqualified-maps}))))

(defn get-ancestor-ids
  "Topic ids from `topic-id` up to its root, nearest-first (self at depth 0).
   Pre:  topic-id may be nil.
   Post: [self parent … root] ordered by depth ascending; [] when topic-id nil
         or unknown. Used to resolve per-item settings that inherit up the tree."
  [topic-id]
  (if (nil? topic-id)
    []
    (mapv :id
      (jdbc/execute! ds
        ["WITH RECURSIVE anc(id, parent_id, depth) AS (
            SELECT id, parent_id, 0 FROM topics WHERE id = ?
            UNION ALL
            SELECT t.id, t.parent_id, a.depth + 1
              FROM topics t JOIN anc a ON t.id = a.parent_id)
          SELECT id FROM anc ORDER BY depth"
         topic-id]
        {:builder-fn rs/as-unqualified-maps}))))

(defn clone-source!
  "Duplicate sources row `source-id` into a NEW row owned by user-id, so later
   edits to either row stay independent. Copies csl/csl_type/url/title.
   Pre:  source-id may be nil (→ nil).
   Post: returns the new sources.id, or nil when source-id is nil or the row
         is missing."
  [user-id source-id]
  (when source-id
    (when-let [src (get-source source-id)]
      (:sources/id
       (create-source! {:user-id  user-id
                        :csl-type (:sources/csl_type src)
                        :csl      (:sources/csl src)
                        :url      (:sources/url src)
                        :title    (:sources/title src)})))))

(defn- kind->csl-type
  "Map legacy topics.kind values to CSL-JSON type tokens."
  [kind]
  (case kind
    "pdf" "document"
    "epub" "book"
    "audio" "song"
    "webpage"))

(defn- ts->date-parts
  "Convert a java.sql.Timestamp to a CSL date-parts vector [[year month day]]."
  [^java.sql.Timestamp ts]
  (let [ldt (.toLocalDateTime ts)]
    [[(.getYear ldt) (.getMonthValue ldt) (.getDayOfMonth ldt)]]))

(defn- now-date-parts
  "CSL date-parts for the current instant. Convenience for accessed-at."
  []
  (ts->date-parts (java.sql.Timestamp. (System/currentTimeMillis))))

(defn backfill-sources!
  "Idempotent migration: build a sources row for every distinct
   (user_id, source_url) on topics where source_id IS NULL. Links the
   topics back via source_id.

   Mapping for the legacy topics.kind on the earliest-created row in each group:
     pdf       → csl_type='document'
     epub      → csl_type='book'
     wikipedia → csl_type='webpage' + csl.container-title='Wikipedia'
     web/other → csl_type='webpage'

   csl.URL ← source_url, csl.title ← topics.title, csl.accessed ←
   topics.created_at (approx — accessed-at ≈ import time)."
  []
  (let [groups (try
                 (jdbc/execute! ds
                   ["SELECT user_id, source_url,
                            MIN(created_at) AS first_seen,
                            (ARRAY_AGG(kind ORDER BY created_at))[1] AS canonical_kind,
                            (ARRAY_AGG(title ORDER BY created_at))[1] AS canonical_title
                     FROM topics
                     WHERE source_url IS NOT NULL AND source_id IS NULL
                       AND user_id IS NOT NULL
                     GROUP BY user_id, source_url"]
                   {:builder-fn rs/as-unqualified-maps})
                 ;; If the column was already dropped (post-T4 reboot), this
                 ;; query errors; treat as "nothing to backfill".
                 (catch Exception _ []))]
    (when (seq groups)
      (jdbc/with-transaction [tx ds]
        (doseq [{:keys [user_id source_url first_seen canonical_kind canonical_title]} groups]
          (let [csl-type (kind->csl-type canonical_kind)
                csl (cond-> {:type csl-type
                             :title canonical_title
                             :URL source_url
                             :accessed {:date-parts (ts->date-parts first_seen)}}
                      (= canonical_kind "wikipedia")
                      (assoc :container-title "Wikipedia"))
                src (jdbc/execute-one! tx
                      (sql/format {:insert-into :sources
                                   :values [{:user_id user_id
                                             :csl_type csl-type
                                             :csl (->jsonb csl)
                                             :url source_url
                                             :title canonical_title
                                             :container_title (:container-title csl)
                                             :created_at first_seen}]
                                   :returning [:id]}))
                sid (:sources/id src)]
            (jdbc/execute! tx
              ["UPDATE topics SET source_id = ?
                WHERE user_id = ? AND source_url = ? AND source_id IS NULL"
               sid user_id source_url])))
        (tel/log! {:level :info :id ::backfill-sources :data {:count (count groups)}}
          (str "Backfilled " (count groups) " source(s) from topics.source_url"))))))

(defn backfill-pdf-sources!
  "Legacy PDF root topics created before create-pdf-topic! attached a sources row
   have source_id IS NULL, which leaves 'Refetch bibliography' disabled. Create a
   document source mirroring create-pdf-topic! (csl_type 'document', title from the
   topic) and link it. Idempotent: only touches kind='pdf' rows with source_id IS NULL."
  []
  (let [pdfs (jdbc/execute! ds
               ;; Skip orphaned topics (user deleted → user_id SET NULL); a
               ;; sources row requires a non-null user_id and they have no owner.
               ["SELECT id, user_id, title FROM topics
                 WHERE kind = 'pdf' AND source_id IS NULL AND user_id IS NOT NULL"]
               {:builder-fn rs/as-unqualified-maps})]
    (when (seq pdfs)
      (jdbc/with-transaction [tx ds]
        (doseq [{:keys [id user_id title]} pdfs]
          (let [csl {:type "document" :title title
                     :accessed {:date-parts (now-date-parts)}}
                src (jdbc/execute-one! tx
                      (sql/format {:insert-into :sources
                                   :values [{:user_id user_id
                                             :csl_type "document"
                                             :csl (->jsonb csl)
                                             :title title}]
                                   :returning [:id]}))
                sid (:sources/id src)]
            (jdbc/execute! tx
              ["UPDATE topics SET source_id = ? WHERE id = ? AND source_id IS NULL"
               sid id])))
        (tel/log! {:level :info :id ::backfill-pdf-sources :data {:count (count pdfs)}}
          (str "Backfilled " (count pdfs) " PDF source(s)"))))))

;; ---------------------------------------------------------------------------
;; Compound creation helpers
;; ---------------------------------------------------------------------------

(defn- insert-page-stubs!
  "Batch-insert kind='page' topic stubs titled \"Page N\" for page numbers
   `from-n`..`to-n` inclusive, under parent-id. One multi-row INSERT instead
   of one per page. ON CONFLICT (parent_id, page_number) DO NOTHING — a
   re-run, or a page already materialized by OCR, is a no-op.
   tx may be the pool `ds` or an in-flight transaction."
  [tx user-id parent-id from-n to-n]
  (when (<= from-n to-n)
    (jdbc/execute! tx
      (sql/format {:insert-into :topics
                   :values (mapv (fn [n] {:user_id user-id
                                           :parent_id parent-id
                                           :kind "page"
                                           :title (str "Page " n)
                                           :page_number n})
                             (range from-n (inc to-n)))
                   :on-conflict [:parent_id :page_number {:where [:is-not :page_number nil]}]
                   :do-nothing true}))))

(defn create-pdf-topic!
  "Create a PDF root topic with file data, page stubs, and a `sources` row.
   Atomically checks quota and increments users.usage_bytes by file-size.
   Throws `quota/quota-error` on cap violation — caller's tx aborts.
   Returns the result with :topics/id."
  [user-id filename file-bytes file-size page-count]
  (let [clean-name (input/prettify-title (input/sanitize-filename filename))]
    (input/check-length! :title clean-name input/title-max)
    (jdbc/with-transaction [tx ds]
      (quota/check-and-bump! tx user-id file-size)
      (let [csl {:type "document" :title clean-name
                 :accessed {:date-parts (now-date-parts)}}
            source (jdbc/execute-one! tx
                     (sql/format {:insert-into :sources
                                  :values [{:user_id user-id
                                            :csl_type "document"
                                            :csl (->jsonb csl)
                                            :title clean-name}]
                                  :returning [:id]}))
            source-id (:sources/id source)
            topic (jdbc/execute-one! tx
                    (sql/format {:insert-into :topics
                                 :values [{:user_id user-id
                                           :kind "pdf"
                                           :title clean-name
                                           :source_id source-id}]
                                 :returning [:id]}))
            topic-id (:topics/id topic)]
        (jdbc/execute! tx
          ["INSERT INTO topic_files (topic_id, file_data, file_size, mime_type)
            VALUES (?, ?, ?, ?)"
           topic-id file-bytes file-size "application/pdf"])
        (when (pos? page-count)
          (insert-page-stubs! tx user-id topic-id 1 page-count))
        (audit-doc-created! user-id topic-id)
        topic))))

(defn create-audio-topic!
  "Create an audio root topic with file data and a `sources` row.
   Atomically checks quota and increments users.usage_bytes by file-size.
   Throws `quota/quota-error` on cap violation — caller's tx aborts.
   Sets content to \"\" (not NULL) so the editor mounts and can receive a
   transcript instead of showing a permanent loading spinner.
   Returns the result with :topics/id."
  [user-id filename file-bytes file-size mime-type]
  (let [clean-name (input/prettify-title (input/sanitize-filename filename))]
    (input/check-length! :title clean-name input/title-max)
    (jdbc/with-transaction [tx ds]
      (quota/check-and-bump! tx user-id file-size)
      (let [csl {:type "song" :title clean-name
                 :accessed {:date-parts (now-date-parts)}}
            source (jdbc/execute-one! tx
                     (sql/format {:insert-into :sources
                                  :values [{:user_id user-id
                                            :csl_type "song"
                                            :csl (->jsonb csl)
                                            :title clean-name}]
                                  :returning [:id]}))
            source-id (:sources/id source)
            topic (jdbc/execute-one! tx
                    (sql/format {:insert-into :topics
                                 :values [{:user_id user-id
                                           :kind "audio"
                                           :title clean-name
                                           :content ""
                                           :source_id source-id}]
                                 :returning [:id]}))
            topic-id (:topics/id topic)]
        (jdbc/execute! tx
          ["INSERT INTO topic_files (topic_id, file_data, file_size, mime_type)
            VALUES (?, ?, ?, ?)"
           topic-id file-bytes file-size mime-type])
        (audit-doc-created! user-id topic-id)
        topic))))

(defn create-score-topic!
  "Create a score root topic owning TWO blobs: the sheet-music PDF as
   role='main' (so /api/pdf and the PDF viewer work unchanged) and the
   recording as role='audio'. No page stubs — cards attach to the root.
   Atomically checks quota against the combined byte size.
   Returns the result with :topics/id."
  [user-id title ^bytes pdf-bytes ^bytes audio-bytes audio-mime]
  (let [clean-name (input/prettify-title (input/sanitize-filename title))
        pdf-size (alength pdf-bytes)
        audio-size (alength audio-bytes)]
    (input/check-length! :title clean-name input/title-max)
    (jdbc/with-transaction [tx ds]
      (quota/check-and-bump! tx user-id (+ pdf-size audio-size))
      (let [csl {:type "song" :title clean-name
                 :accessed {:date-parts (now-date-parts)}}
            source (jdbc/execute-one! tx
                     (sql/format {:insert-into :sources
                                  :values [{:user_id user-id
                                            :csl_type "song"
                                            :csl (->jsonb csl)
                                            :title clean-name}]
                                  :returning [:id]}))
            source-id (:sources/id source)
            topic (jdbc/execute-one! tx
                    (sql/format {:insert-into :topics
                                 :values [{:user_id user-id
                                           :kind "score"
                                           :title clean-name
                                           :content ""
                                           :source_id source-id}]
                                 :returning [:id]}))
            topic-id (:topics/id topic)]
        (jdbc/execute! tx
          ["INSERT INTO topic_files (topic_id, role, file_data, file_size, mime_type)
            VALUES (?, 'main', ?, ?, 'application/pdf')"
           topic-id pdf-bytes pdf-size])
        (jdbc/execute! tx
          ["INSERT INTO topic_files (topic_id, role, file_data, file_size, mime_type)
            VALUES (?, 'audio', ?, ?, ?)"
           topic-id audio-bytes audio-size audio-mime])
        (audit-doc-created! user-id topic-id)
        topic))))

(defn create-live-doc!
  "Create an empty Live Document: a kind='pdf', is_live=true root topic with a
   bibliography `sources` row and NO file blob or page stubs. The backing PDF is
   created lazily on the first appended image batch (see commit-live-append!).
   Returns the topic with :topics/id."
  [user-id]
  (let [title (str "Live Document " (java.time.LocalDate/now))]
    (input/check-length! :title title input/title-max)
    (jdbc/with-transaction [tx ds]
      (let [csl {:type "document" :title title
                 :accessed {:date-parts (now-date-parts)}}
            source (jdbc/execute-one! tx
                     (sql/format {:insert-into :sources
                                  :values [{:user_id user-id
                                            :csl_type "document"
                                            :csl (->jsonb csl)
                                            :title title}]
                                  :returning [:id]}))
            source-id (:sources/id source)
            topic (jdbc/execute-one! tx
                    (sql/format {:insert-into :topics
                                 :values [{:user_id user-id
                                           :kind "pdf"
                                           :title title
                                           :is_live true
                                           :source_id source-id}]
                                 :returning [:id]}))]
        (audit-doc-created! user-id (:topics/id topic))
        topic))))

(defn commit-live-append!
  "Atomically persist an appended PDF blob for a Live Document.
   Pre:  topic-id is a kind='pdf', is_live=true topic owned by user-id (the HTTP
         handler enforces ownership); new-bytes is the full rebuilt PDF;
         new-total is its page count; prev-total the count before this batch;
         delta-bytes >= 0 is the blob growth to charge against quota.
   Post: blob replaced, users.usage_bytes bumped by delta-bytes, page stubs
         created for page numbers (prev-total+1 .. new-total). A quota violation
         throws quota/quota-error and aborts the whole tx — nothing persists.
   Returns {:pages-added (- new-total prev-total)}."
  [user-id topic-id ^bytes new-bytes new-size delta-bytes prev-total new-total]
  (jdbc/with-transaction [tx ds]
    (when (pos? delta-bytes)
      (quota/check-and-bump! tx user-id delta-bytes))
    (jdbc/execute! tx
      ["INSERT INTO topic_files (topic_id, file_data, file_size, mime_type)
        VALUES (?, ?, ?, 'application/pdf')
        ON CONFLICT (topic_id, role)
        DO UPDATE SET file_data = excluded.file_data, file_size = excluded.file_size,
                      mime_type = excluded.mime_type"
       topic-id new-bytes new-size])
    (insert-page-stubs! tx user-id topic-id (inc prev-total) new-total)
    {:pages-added (- new-total prev-total)}))

(defn find-web-topic-by-title
  "Find an existing root-level web topic by title (case-insensitive).
   Returns {:topics/id ...} or nil."
  [user-id title]
  (jdbc/execute-one! ds
    (sql/format {:select [:id :title]
                 :from [:topics]
                 :where [:and
                         [:= :user_id user-id]
                         [:= :kind "web"]
                         [:is :parent_id nil]
                         [:= [:lower :title] (str/lower-case title)]]})))

(defn find-web-topic-by-source-id
  "Find an existing root-level web topic for (user-id, source-id).
   Used to dedupe URL imports — a single sources row may already be
   linked from a topic, in which case re-importing should return the
   existing topic instead of creating a duplicate.
   Returns {:topics/id ...} or nil."
  [user-id source-id]
  (when (and user-id source-id)
    (jdbc/execute-one! ds
      (sql/format {:select [:id :title]
                   :from [:topics]
                   :where [:and
                           [:= :user_id user-id]
                           [:= :kind "web"]
                           [:is :parent_id nil]
                           [:= :source_id source-id]]
                   :limit 1}))))

(defn ensure-source-for-url!
  "Find or create a sources row for (user-id, url). Returns row map (with id).
   bib = {:source-type 'wikipedia'|'webpage'|nil
          :title nil-or-str
          :issued-date-parts nil-or-[[Y M? D?]]}.
   Used at import time to dedupe by URL."
  [user-id url bib]
  (when (and user-id (not (str/blank? url)))
    (or (find-source-by-url user-id url)
        (let [wikipedia? (= (:source-type bib) "wikipedia")
              title (:title bib)
              issued-dp (:issued-date-parts bib)
              csl (cond-> {:type "webpage"
                           :title title
                           :URL url
                           :accessed {:date-parts (now-date-parts)}}
                    wikipedia? (assoc :container-title "Wikipedia")
                    (seq issued-dp) (assoc :issued {:date-parts (csl/pad-date-parts issued-dp)}))]
          (create-source! {:user-id user-id
                           :csl-type "webpage"
                           :csl csl
                           :url url
                           :title title})))))

(defn create-web-topic!
  "Create a web article topic. Returns topic-id.
   `bib` (optional) = {:url ... :source-type 'wikipedia'|'webpage'|nil}.
   When :url is non-blank, ensures a `sources` row (per-user dedup by URL)
   and links topics.source_id. :source-type 'wikipedia' tags
   csl.container-title='Wikipedia'."
  [user-id title html-content bib]
  (let [clean-title (input/prettify-title (input/sanitize-filename title))
        _ (input/check-length! :title clean-title input/title-max)
        sanitized (sanitize-utf8 html-content)
        url (some-> bib :url str/trim not-empty)
        source-id (some-> (ensure-source-for-url! user-id url
                            (assoc bib :title clean-title))
                    :sources/id)
        topic (jdbc/execute-one! ds
                (sql/format {:insert-into :topics
                             :values [(cond-> {:user_id user-id
                                               :kind "web"
                                               :title clean-title
                                               :content sanitized
                                               :content_text (text/strip-html sanitized)}
                                        source-id (assoc :source_id source-id))]
                             :returning [:id]}))]
    (audit-doc-created! user-id (:topics/id topic))
    (:topics/id topic)))

(defn create-markdown-topic!
  "Create a Markdown-imported topic. Returns topic-id."
  [user-id title html-content]
  (let [clean-title (input/prettify-title (input/sanitize-filename title))
        _ (input/check-length! :title clean-title input/title-max)
        sanitized (sanitize-utf8 html-content)
        topic (jdbc/execute-one! ds
                (sql/format {:insert-into :topics
                             :values [{:user_id user-id
                                       :kind "markdown"
                                       :title clean-title
                                       :content sanitized
                                       :content_text (text/strip-html sanitized)}]
                             :returning [:id]}))]
    (audit-doc-created! user-id (:topics/id topic))
    (:topics/id topic)))

(defn create-epub-topic!
  "Create an EPUB root topic with file, chapter children, and a `sources` row.
   chapters is a vec of {:html :title} maps.
   Atomically checks quota and increments users.usage_bytes by file-size.
   Throws `quota/quota-error` on cap violation — caller's tx aborts.
   Returns {:topic-id N :chapter-ids [...]}"
  [user-id title file-bytes file-size chapters]
  (let [clean-title (input/prettify-title (input/sanitize-filename title))]
    (input/check-length! :title clean-title input/title-max)
    (jdbc/with-transaction [tx ds]
      (quota/check-and-bump! tx user-id file-size)
      (let [csl {:type "book" :title clean-title
                 :accessed {:date-parts (now-date-parts)}}
            source (jdbc/execute-one! tx
                     (sql/format {:insert-into :sources
                                  :values [{:user_id user-id
                                            :csl_type "book"
                                            :csl (->jsonb csl)
                                            :title clean-title}]
                                  :returning [:id]}))
            source-id (:sources/id source)
            topic (jdbc/execute-one! tx
                    (sql/format {:insert-into :topics
                                 :values [{:user_id user-id
                                           :kind "epub"
                                           :title clean-title
                                           :source_id source-id}]
                                 :returning [:id]}))
            topic-id (:topics/id topic)]
        (jdbc/execute! tx
          ["INSERT INTO topic_files (topic_id, file_data, file_size, mime_type)
            VALUES (?, ?, ?, ?)"
           topic-id file-bytes file-size "application/epub+zip"])
        ;; Create chapter children — one multi-row INSERT instead of one per
        ;; chapter. Each row already carries a stable, unique-within-this-batch
        ;; ordinal (:page_number = 1-based chapter index); RETURNING it back
        ;; alongside id and re-associating by that value keeps chapter-ids
        ;; correctly ordered without trusting Postgres' VALUES/RETURNING row
        ;; order, which SQL does not guarantee.
        (let [chapter-rows (map-indexed
                             (fn [i ch]
                               (let [sanitized (sanitize-utf8 (:html ch))]
                                 {:user_id user-id
                                  :parent_id topic-id
                                  :kind "basic"
                                  :title (input/prettify-title
                                           (or (:title ch) (str "Chapter " (inc i))))
                                  :content sanitized
                                  :content_text (text/strip-html sanitized)
                                  :page_number (inc i)
                                  :status "active"
                                  :priority 50}))
                             chapters)
              chapter-ids (when (seq chapter-rows)
                            (let [returned (jdbc/execute! tx
                                             (sql/format {:insert-into :topics
                                                          :values (vec chapter-rows)
                                                          :returning [:id :page_number]}))
                                  id-by-page (into {} (map (juxt :topics/page_number :topics/id)) returned)]
                              (mapv (comp id-by-page :page_number) chapter-rows)))]
          (audit-doc-created! user-id topic-id)
          {:topic-id topic-id :chapter-ids (vec (remove nil? chapter-ids))})))))

(defn create-standalone-topic!
  "Create a standalone empty topic. Returns {:topic-id N}."
  [user-id title]
  (let [clean-title (input/prettify-title (input/sanitize-filename title))
        _ (input/check-length! :title clean-title input/title-max)
        topic (jdbc/execute-one! ds
                (sql/format {:insert-into :topics
                             :values [{:user_id user-id
                                       :kind "basic"
                                       :title clean-title
                                       :content ""
                                       :content_text ""}]
                             :returning [:id]}))]
    (audit-doc-created! user-id (:topics/id topic))
    {:topic-id (:topics/id topic)}))

;; ---------------------------------------------------------------------------
;; Page-specific operations (kind='page' child topics)
;; ---------------------------------------------------------------------------

(defn create-page-stubs!
  "Batch-insert empty page topics for a parent. ON CONFLICT DO NOTHING."
  [parent-id page-count user-id]
  (when (pos? page-count)
    (insert-page-stubs! ds user-id parent-id 1 page-count)))

(defn save-page-text!
  "Save or update OCR text for a page topic (by parent + page_number).
   Upserts the page topic row."
  [parent-id page-number html]
  (let [plain (text/strip-html html)]
    (jdbc/execute! ds
      ["INSERT INTO topics (parent_id, kind, title, content, content_text, page_number,
                            user_id)
        SELECT ?, 'page', ?, ?, ?, ?,
               (SELECT user_id FROM topics WHERE id = ?)
        WHERE NOT EXISTS (SELECT 1 FROM topics WHERE parent_id = ? AND page_number = ?)
        ON CONFLICT DO NOTHING"
       parent-id (str "Page " page-number) html plain page-number
       parent-id parent-id page-number])
    ;; Update existing
    (jdbc/execute! ds
      ["UPDATE topics SET content = ?, content_text = ?
        WHERE parent_id = ? AND page_number = ?"
       html plain parent-id page-number])))

(defn get-page-text
  "Get the content (OCR text) for a page by parent + page_number."
  [parent-id page-number]
  (jdbc/execute-one! ds
    (sql/format {:select [:content]
                 :from [:topics]
                 :where [:and
                         [:= :parent_id parent-id]
                         [:= :page_number page-number]]})))

(defn list-pages
  "List all page topics for a parent, ordered by page_number."
  [parent-id]
  (jdbc/execute! ds
    (sql/format {:select [:*]
                 :from [:topics]
                 :where [:and [:= :parent_id parent-id] [:= :kind "page"]
                         [:is :staged_delete_id nil]]
                 :order-by [[:page_number :asc]]})))

(defn get-page-done-status
  "Check if a page is done. Returns boolean."
  [parent-id page-number]
  (let [result (jdbc/execute-one! ds
                 (sql/format {:select [:status]
                              :from [:topics]
                              :where [:and
                                      [:= :parent_id parent-id]
                                      [:= :page_number page-number]]}))]
    (= "done" (:topics/status result))))

(defn toggle-page-done!
  "Toggle a page topic between 'active' and 'done' status."
  [parent-id page-number]
  (jdbc/execute-one! ds
    ["UPDATE topics SET status = CASE WHEN status = 'done' THEN 'active' ELSE 'done' END
      WHERE parent_id = ? AND page_number = ?"
     parent-id page-number]))

(defn get-context-pages
  "Get content from page topics in a range, for card generation context."
  [parent-id start-page end-page]
  (jdbc/execute! ds
    (sql/format {:select [:page_number :content]
                 :from [:topics]
                 :where [:and
                         [:= :parent_id parent-id]
                         [:= :kind "page"]
                         [:>= :page_number start-page]
                         [:<= :page_number end-page]]
                 :order-by [[:page_number :asc]]})))

;; ---------------------------------------------------------------------------
;; Flashcard operations
;; ---------------------------------------------------------------------------

(def ^:private occlusion-mask-count-expr
  "Rects sharing this row's mask_ordinal — the size of the row's mask group,
   which the card tables render as \"group N · k masks\". Correlated on
   f.mask_ordinal, so it is 0 for non-occlusion rows (a NULL ordinal matches
   nothing) and for a group whose geometry somehow lacks :rects (coalesce)."
  [:raw (str "(select count(*) from"
          " jsonb_array_elements(coalesce(og.geometry->'rects', '[]'::jsonb)) as r(el)"
          " where (r.el->>'ordinal')::int = f.mask_ordinal)")])

(def ^:private card-group-selects
  "Occlusion/score group columns, projected by every query that returns whole
   flashcard rows: get-flashcards, get-all-flashcards, get-flashcards-for-subtree,
   get-flashcards-by-ids, get-user-flashcards. Defined once so those five cannot
   drift — the mask-count expression made the copy-paste load-bearing.
   Pre:  the query aliases flashcards as f and left-joins occlusion_groups as og
         and score_groups as sg.
   Post: aliases are the unqualified keys normalize-flashcard-row promises; the
         count is an expression column, which pgjdbc leaves unqualified already."
  [[:og.image_media_id :occlusion_image_media_id]
   [:og.mode :occlusion_mode]
   [occlusion-mask-count-expr :occlusion_mask_count]
   [:sg.start_ms :score_start_ms]
   [:sg.end_ms :score_end_ms]])

(defn- normalize-flashcard-row
  "Post-process a flashcard query row so it is plain data (Electric-wire safe):
   parse the io_fields JSONB, and move the occlusion-group and score-group
   join columns to the unqualified keys this ns promises
   (:occlusion_image_media_id :occlusion_mode :score_start_ms :score_end_ms) —
   pgjdbc qualifies plain aliased columns by their base table (see
   get-user-flashcards), so cover both key shapes."
  [row]
  (-> row
    (update :flashcards/io_fields pgobject->clj)
    (update :flashcards/overlapping pgobject->clj)
    (assoc :occlusion_image_media_id (or (:occlusion_image_media_id row)
                                       (:occlusion_groups/occlusion_image_media_id row))
      :occlusion_mode (or (:occlusion_mode row)
                        (:occlusion_groups/occlusion_mode row))
      :score_start_ms (or (:score_start_ms row)
                        (:score_groups/score_start_ms row))
      :score_end_ms (or (:score_end_ms row)
                      (:score_groups/score_end_ms row)))
    (dissoc :occlusion_groups/occlusion_image_media_id :occlusion_groups/occlusion_mode
      :score_groups/score_start_ms :score_groups/score_end_ms)))

(defn insert-flashcards!
  "Batch insert flashcards. Rows should include :topic_id and :root_topic_id.
   Uses ON CONFLICT DO NOTHING to prevent duplicates.
   Returns a vector of the inserted flashcards' ids in insertion order; rows
   skipped by ON CONFLICT DO NOTHING return no id, so the result may be shorter
   than `rows`. Returns nil when `rows` is empty.

   The 2-arity takes the connectable. Pass an open transaction when the cards'
   topics were inserted in that same transaction — on `ds` this runs on a
   different connection, cannot see the uncommitted parents, and fails the
   flashcards_topic_id_fkey constraint."
  ([rows] (insert-flashcards! ds rows))
  ([connectable rows]
   (when (seq rows)
     (doseq [row rows]
       (input/check-length! :question (:question row) input/card-max)
       (input/check-length! :answer (:answer row) input/card-max)
       (input/check-length! :cloze (:cloze row) input/card-max))
     ;; :overlapping is a plain map at the call site; convert to jsonb here so
     ;; callers never touch PGobject (same ownership as the io_fields path).
     (let [rows (mapv (fn [row]
                        (cond-> row
                          (some? (:overlapping row)) (update :overlapping ->jsonb)))
                  rows)]
       (mapv :flashcards/id
         (jdbc/execute! connectable
           (sql/format {:insert-into :flashcards
                        :values rows
                        :on-conflict []
                        :do-nothing true
                        :returning [:id]})))))))

;; ── Batch inserts for collection import ────────────────────────────
;; An importer owns values the user never typed — titles from a foreign
;; registry, schedules from a foreign algorithm — so these three bypass the
;; single-row creators (create-topic!, create-source!, log-topic-repetition!)
;; and their prettification, defaulting and per-row audit logging. They take
;; an explicit `tx` so a whole collection commits or rolls back as one unit.

(defn insert-topic-rows!
  "Batch-insert fully-formed topic rows; return their ids in insertion order.
   Pre : `tx` is an open transaction. Every row carries :user_id, :kind,
         :title, :status and :priority. A :parent_id, when present, names a
         topic already committed in `tx` — callers insert breadth-first so a
         parent precedes its children. Titles are already within title-max;
         over-length is a caller bug because only the caller can decide
         whether to truncate or reject.
   Post: vector of ids positionally aligned with `rows`; nil when empty.
   Invariant: no ON CONFLICT — topics have no natural key here, so a conflict
         would mean a malformed row rather than a duplicate."
  [tx rows]
  (when (seq rows)
    (doseq [row rows]
      (input/check-length! :title (:title row) input/title-max))
    (mapv :topics/id
      (jdbc/execute! tx
        (sql/format {:insert-into :topics :values (vec rows) :returning [:id]})))))

(defn insert-source-rows!
  "Batch-insert sources rows; return their ids in insertion order.
   Pre : `tx` is an open transaction; each row carries :user_id and :csl_type,
         and :csl is a plain Clojure map.
   Post: vector of ids positionally aligned with `rows`; nil when empty.
   `:csl` is converted to jsonb here so callers never touch PGobject — the
   same ownership rule insert-flashcards! applies to :overlapping."
  [tx rows]
  (when (seq rows)
    (let [rows (mapv #(update % :csl ->jsonb) rows)]
      (mapv :sources/id
        (jdbc/execute! tx
          (sql/format {:insert-into :sources :values rows :returning [:id]}))))))

(defn insert-topic-repetitions!
  "Batch-append rows to topic_repetitions; return the number inserted.
   Pre : `tx` is an open transaction. Each row carries :topic_id, :user_id,
         :event_type and :event_at. The three snapshot columns
         (:status_before, :interval_days_before, :a_factor_before) MAY be nil
         only when :event_type is \"import\" — a foreign collection records
         when a repetition happened but not the state it replaced. The
         database can no longer enforce this, so it is enforced here.
   Post: row count.
   Invariant: violating the nil rule throws rather than writing an
         unattributable gap into a log the history view presents as fact."
  [tx rows]
  (when (seq rows)
    (doseq [{:keys [event_type status_before interval_days_before a_factor_before] :as row} rows]
      (when (and (not= "import" event_type)
              (or (nil? status_before) (nil? interval_days_before) (nil? a_factor_before)))
        (throw (ex-info "Non-import repetition rows must carry their before-snapshot"
                 {:type ::incomplete-repetition-snapshot :event-type event_type
                  :topic-id (:topic_id row)}))))
    (count (jdbc/execute! tx
             (sql/format {:insert-into :topic_repetitions :values (vec rows) :returning [:id]})))))

(defn- strip-foreign-jsonb
  "Remove map entries whose value is an unparsed org.postgresql.util.PGobject.
   These are JSONB columns this branch's schema does not model — e.g. a column
   added by another branch sharing the same database — which are not
   Electric-serializable and must never cross the wire to the client. Modeled
   JSONB is parsed by its own query (pgobject->clj), so it is never raw here.
   Pre:  row is a result-set map, or nil.
   Post: the same map without PGobject-valued entries; nil → nil.
   Blame: environment (schema drift) — a client-render row must not carry a
   column this branch cannot serialize."
  [row]
  (when row
    (into {} (remove (fn [[_ v]] (instance? PGobject v))) row)))

(defn get-flashcards
  "Get all flashcards for a specific topic (page or extract).
   Includes page_number from the topic hierarchy (direct or via parent)."
  [topic-id]
  (if topic-id
    (mapv (comp strip-foreign-jsonb normalize-flashcard-row)
      (jdbc/execute! ds
        (sql/format {:select (into [[:f.*] [[:coalesce :t.page_number :parent.page_number] :page_number]]
                               card-group-selects)
                     :from [[:flashcards :f]]
                     :join [[:topics :t] [:= :f.topic_id :t.id]]
                     :left-join [[:topics :parent] [:= :t.parent_id :parent.id]
                                 [:occlusion_groups :og] [:= :f.occlusion_group_id :og.id]
                                 [:score_groups :sg] [:= :f.score_group_id :sg.id]]
                     :where [:= :f.topic_id topic-id]
                     :order-by [[:f.created_at :desc]]})))
    []))

(defn get-all-flashcards
  "Get all flashcards under a root topic.
   Includes page_number from the topic hierarchy (direct or via parent)."
  [root-topic-id]
  (mapv (comp strip-foreign-jsonb normalize-flashcard-row)
    (jdbc/execute! ds
      (sql/format {:select (into [[:f.*] [[:coalesce :t.page_number :parent.page_number] :page_number]]
                             card-group-selects)
                   :from [[:flashcards :f]]
                   :join [[:topics :t] [:= :f.topic_id :t.id]]
                   :left-join [[:topics :parent] [:= :t.parent_id :parent.id]
                               [:occlusion_groups :og] [:= :f.occlusion_group_id :og.id]
                               [:score_groups :sg] [:= :f.score_group_id :sg.id]]
                   :where [:= :f.root_topic_id root-topic-id]
                   :order-by [[:f.created_at :desc]]}))))

(defn get-user-flashcards
  "Get every flashcard owned by user-id, with topic/root context for the
   Library cards view. Ownership is scoped by the ROOT topic's user_id —
   root topics always carry user_id; child rows may not.
   Returns flashcard columns plus :topic_title, :root_title, :page_number
   (direct or via parent topic) and :formatted_date.
   pgjdbc qualifies plain aliased columns by their base table (here both
   titles → :topics/*), unlike expression columns — rename to the
   unqualified keys this fn promises."
  [user-id]
  (mapv (fn [row]
          (-> row
            normalize-flashcard-row
            strip-foreign-jsonb
            (assoc :topic_title (:topics/topic_title row)
              :root_title (:topics/root_title row))
            (dissoc :topics/topic_title :topics/root_title)))
    (jdbc/execute! ds
      (sql/format {:select (into [[:f.*]
                                  [[:coalesce :t.page_number :parent.page_number] :page_number]
                                  [:t.title :topic_title]
                                  [:root.title :root_title]
                                  [[:to_char :f.created_at [:inline "Mon DD"]] :formatted_date]]
                             card-group-selects)
                   :from [[:flashcards :f]]
                   :join [[:topics :t] [:= :f.topic_id :t.id]
                          [:topics :root] [:= :f.root_topic_id :root.id]]
                   :left-join [[:topics :parent] [:= :t.parent_id :parent.id]
                               [:occlusion_groups :og] [:= :f.occlusion_group_id :og.id]
                               [:score_groups :sg] [:= :f.score_group_id :sg.id]]
                   :where [:= :root.user_id user-id]
                   :order-by [[:f.created_at :desc]]}))))

(defn get-pushed-card-manifest
  "Card-id/note-id pairs for every pushed card the user owns (anki_note_id NOT
   NULL). Filter-independent — this is the Anki overlay's fetch set, so it must
   not narrow with the /library/cards filters. Empty → []."
  [user-id]
  (mapv (fn [r] {:card-id (:flashcards/id r) :note-id (:flashcards/anki_note_id r)})
    (jdbc/execute! ds
      (sql/format {:select [:f.id :f.anki_note_id]
                   :from [[:flashcards :f]]
                   :join [[:topics :root] [:= :f.root_topic_id :root.id]]
                   :where [:and
                           [:= :root.user_id user-id]
                           [:not= :f.anki_note_id nil]]}))))

(defn get-flashcards-by-ids
  "Flashcards owned by user-id (via root topic) with ids in card-ids.
   Full rows plus :page_number — the inputs for bulk push (build-update-fields
   reads page_number for the source anchor) and bulk pull. Empty ids → []."
  [user-id card-ids]
  (if (empty? card-ids)
    []
    (mapv (comp strip-foreign-jsonb normalize-flashcard-row)
      (jdbc/execute! ds
        (sql/format {:select (into [[:f.*]
                                    [[:coalesce :t.page_number :parent.page_number] :page_number]]
                               card-group-selects)
                     :from [[:flashcards :f]]
                     :join [[:topics :t] [:= :f.topic_id :t.id]
                            [:topics :root] [:= :f.root_topic_id :root.id]]
                     :left-join [[:topics :parent] [:= :t.parent_id :parent.id]
                                 [:occlusion_groups :og] [:= :f.occlusion_group_id :og.id]
                                 [:score_groups :sg] [:= :f.score_group_id :sg.id]]
                     :where [:and
                             [:= :root.user_id user-id]
                             [:in :f.id (vec card-ids)]]})))))

(defn delete-user-flashcards!
  "Bulk delete of user-id's flashcards. Ownership enforced via the root
   topic join. Returns the full deleted rows (RETURNING flashcards.*) so
   callers can both read :flashcards/anki_note_id and snapshot the rows for
   undo."
  [user-id card-ids]
  (if (empty? card-ids)
    []
    (jdbc/execute! ds
      (sql/format {:delete-from :flashcards
                   :using [[:topics :root]]
                   :where [:and
                           [:= :flashcards.root_topic_id :root.id]
                           [:= :root.user_id user-id]
                           [:in :flashcards.id (vec card-ids)]]
                   :returning [:flashcards.*]}))))

(defn get-flashcards-by-anki-note-ids
  "Flashcards owned by user-id (via root topic) whose anki_note_id is in
   note-ids. Returns id, kind, content fields, and anki_note_id — the diff
   inputs for the Anki overlay. Empty note-ids → []."
  [user-id note-ids]
  (if (empty? note-ids)
    []
    (jdbc/execute! ds
      (sql/format {:select [:f.id :f.kind :f.question :f.answer :f.cloze :f.anki_note_id]
                   :from [[:flashcards :f]]
                   :join [[:topics :root] [:= :f.root_topic_id :root.id]]
                   :where [:and
                           [:= :root.user_id user-id]
                           [:in :f.anki_note_id (vec note-ids)]]}))))

(defn delete-flashcard!
  "Delete flashcard `card-id` iff owned by `user-id` (via its root topic).
   Returns the full deleted row (RETURNING *) so callers can read
   :flashcards/anki_note_id and snapshot it for undo, or nil when the card is
   not the user's (0 rows — the ownership predicate is the security gate)."
  [user-id card-id]
  (jdbc/execute-one! ds
    ["DELETE FROM flashcards
      WHERE id = ? AND root_topic_id IN (SELECT id FROM topics WHERE user_id = ?)
      RETURNING *" card-id user-id]))

(defn update-flashcard!
  "Update content fields of flashcard `card-id` iff owned by `user-id` (via its
   root topic). Sets updated_at for sync tracking. :io_fields (a plain map) is
   converted to JSONB here — callers never touch PGobject. No-op (0 rows) when
   the card is not the user's."
  [user-id card-id fields]
  (jdbc/execute-one! ds
    (sql/format {:update :flashcards
                 :set (cond-> (assoc fields :updated_at [:now])
                        (contains? fields :io_fields) (update :io_fields ->jsonb)
                        (contains? fields :overlapping) (update :overlapping ->jsonb))
                 :where [:and
                         [:= :id card-id]
                         [:in :root_topic_id {:select :id :from :topics
                                              :where [:= :user_id user-id]}]]})))

;; ---------------------------------------------------------------------------
;; Card edit history — write-behind version log (plans/card-edit-history.md).
;; Every write here logs the rendition it REPLACED. Deliberately NOT folded
;; into update-flashcard!: the Anki pull path shares that fn
;; (anki_sync_server/apply-pull-updates!) and pulls must not version.
;; ---------------------------------------------------------------------------

(def ^:private card-content-columns
  "Flashcard columns that together constitute a card-scoped rendition."
  [:question :answer :cloze :overlapping])

(defn- any-changed?
  "True when any [stored, incoming] pair differs — the single change-detection
   rule for every versioned write path (card fields, occlusion group, score
   group). Stored values may be PGobjects; they are parsed first, so JSONB
   compares by value and key order is not significant. Non-PGobject values pass
   through pgobject->clj untouched, so TEXT and integer columns use the same
   path.

   Pre:  `pairs` is a seq of 2-element [stored incoming] tuples.
   Post: numeric widening (10 vs 10.0) reads as a change. That direction is
         deliberate — a false positive costs one redundant version row, a false
         negative would silently lose history."
  [pairs]
  (boolean (some (fn [[stored incoming]] (not= (pgobject->clj stored) incoming))
             pairs)))

(defn- card-content-changed?
  "any-changed? for a flashcard row: resolves each key of `fields` against
   `old`'s namespace-qualified keys.
   Pre: `fields` keys ⊆ card-content-columns (asserted by the caller)."
  [old fields]
  (any-changed?
    (map (fn [[k v]] [(get old (keyword "flashcards" (name k))) v]) fields)))

(defn- insert-card-version!
  "Insert one card_versions row. `payload` carries the scope keys and the
   non-null rendition columns; JSONB values MUST already be PGobjects (passed
   through verbatim from the row being superseded) or Clojure values wrapped by
   the caller. Returns nil."
  [tx payload]
  (jdbc/execute-one! tx (sql/format {:insert-into :card_versions :values [payload]}))
  nil)

(defn update-flashcard-versioned!
  "Log the superseded rendition, then apply `fields` to flashcard `card-id`.
   The versioning counterpart of update-flashcard!, for user-initiated saves.

   Pre:  `fields` keys ⊆ card-content-columns (ASSERTED — a wider key would be
         compared against a column this fn never selected, so it would read as
         changed on every save and its value would be written unwrapped);
         :overlapping is a plain map.
   Post: when `fields` would change at least one value, exactly one
         card_versions row holds the FULL pre-edit rendition (all four content
         columns, verbatim), and the card holds the new values — atomically.
         An unchanged save writes no version row and still bumps updated_at, so
         updated_at does NOT imply the card has history.
   Inv:  card_versions never holds the current rendition.
   No-op (0 rows, no version) when the card is not `user-id`'s.
   Returns {:updated? bool :versioned? bool}."
  [user-id card-id fields]
  (assert (every? (set card-content-columns) (keys fields))
    (str "update-flashcard-versioned! got non-content keys "
      (vec (remove (set card-content-columns) (keys fields)))))
  (jdbc/with-transaction [tx ds]
    (if-let [old (jdbc/execute-one! tx
                   ["SELECT id, kind, root_topic_id, question, answer, cloze, overlapping
                     FROM flashcards
                     WHERE id = ? AND root_topic_id IN (SELECT id FROM topics WHERE user_id = ?)
                     FOR UPDATE"
                    card-id user-id])]
      (let [changed? (card-content-changed? old fields)]
        (when changed?
          (insert-card-version! tx
            (into {:root_topic_id (:flashcards/root_topic_id old)
                   :scope_type "card"
                   :scope_id card-id
                   :kind (:flashcards/kind old)}
              (map (fn [k] [k (get old (keyword "flashcards" (name k)))]))
              card-content-columns)))
        (jdbc/execute-one! tx
          (sql/format {:update :flashcards
                       :set (cond-> (assoc fields :updated_at [:now])
                              (contains? fields :overlapping) (update :overlapping ->jsonb))
                       :where [:= :id card-id]}))
        {:updated? true :versioned? changed?})
      {:updated? false :versioned? false})))

(def max-versions-read
  "Read cap for one history view. Retention is unbounded by design, so without
   a cap a pathologically-edited card would materialize its entire log server-
   side. Callers MUST surface truncation when a result hits this count — a
   silent cap would read as 'this is the whole history'."
  500)

(defn- versions-for-scopes
  "Rows for the given scope predicates, newest first, capped at
   max-versions-read.
   Pre:  `scopes` is a non-empty seq of HoneySQL predicates over
         (scope_type, scope_id); ownership is ALREADY enforced by the caller —
         this fn does no authorization.
   Post: rows are plain data with UNQUALIFIED keys and JSONB parsed
         (Electric-wire safe — no PGobject, and no OffsetDateTime: the
         timestamp crosses only as the preformatted :superseded_label).
         A result of exactly max-versions-read means older versions exist."
  [scopes]
  (mapv (fn [row]
          (-> (update-keys row (comp keyword name))
            (update :overlapping pgobject->clj)
            (update :io_fields pgobject->clj)
            (update :geometry pgobject->clj)))
    (jdbc/execute! ds
      (sql/format {:select [:id :scope_type :kind
                            [[:to_char :superseded_at "YYYY-MM-DD HH24:MI"] :superseded_label]
                            :question :answer :cloze :overlapping :io_fields :geometry
                            :occlusion_mode :occlusion_image_media_id
                            :score_direction :score_start_ms :score_end_ms]
                   :from [:card_versions]
                   :where (into [:or] scopes)
                   :order-by [[:superseded_at :desc] [:id :desc]]
                   :limit max-versions-read}))))

(defn get-card-versions
  "Superseded renditions visible for flashcard `card-id`, newest first.
   Unions the card's own versions with those of the group it belongs to, so an
   occlusion card shows every group edit — including the geometry that still
   contained a mask this card no longer has.
   Pre:  card owned by `user-id` (enforced; a foreign card yields []).
   Returns a vector of rows, [] when the card has no history."
  [user-id card-id]
  (if-let [card (jdbc/execute-one! ds
                  ["SELECT occlusion_group_id, score_group_id FROM flashcards
                    WHERE id = ? AND root_topic_id IN (SELECT id FROM topics WHERE user_id = ?)"
                   card-id user-id])]
    (versions-for-scopes
      (cond-> [[:and [:= :scope_type "card"] [:= :scope_id card-id]]]
        (:flashcards/occlusion_group_id card)
        (conj [:and [:= :scope_type "occlusion_group"]
               [:= :scope_id (:flashcards/occlusion_group_id card)]])
        (:flashcards/score_group_id card)
        (conj [:and [:= :scope_type "score_group"]
               [:= :scope_id (:flashcards/score_group_id card)]])))
    []))

(defn get-occlusion-group-versions
  "Superseded renditions of occlusion group `group-id`, newest first — the
   group-keyed entry point, for the occlusion editor which knows a group but no
   single card.
   Pre:  group owned by `user-id` — enforced here via the group's cards, which
         carry root_topic_id (occlusion_groups has no user column). A foreign
         group, or one whose last card is gone, yields [].
   Returns a vector of rows, [] when the group has never been edited."
  [user-id group-id]
  (if (jdbc/execute-one! ds
        ["SELECT 1 FROM flashcards
          WHERE occlusion_group_id = ?
            AND root_topic_id IN (SELECT id FROM topics WHERE user_id = ?)
          LIMIT 1"
         group-id user-id])
    (versions-for-scopes [[:and [:= :scope_type "occlusion_group"]
                           [:= :scope_id group-id]]])
    []))

(defn get-anki-note-ids
  "Get anki_note_ids for a specific topic's flashcards."
  [topic-id]
  (->> (jdbc/execute! ds
         ["SELECT anki_note_id FROM flashcards WHERE topic_id = ? AND anki_note_id IS NOT NULL"
          topic-id])
    (mapv :flashcards/anki_note_id)))

(defn get-all-anki-note-ids
  "Get all anki_note_ids under a root topic."
  [root-topic-id]
  (->> (jdbc/execute! ds
         ["SELECT anki_note_id FROM flashcards WHERE root_topic_id = ? AND anki_note_id IS NOT NULL"
          root-topic-id])
    (mapv :flashcards/anki_note_id)))

(defn set-anki-note-id
  "Set anki_note_id and anki_synced_at for a `user-id`-owned flashcard."
  [user-id card-id anki-note-id]
  (jdbc/execute-one! ds
    ["UPDATE flashcards SET anki_note_id = ?, anki_synced_at = CURRENT_TIMESTAMP
      WHERE id = ? AND root_topic_id IN (SELECT id FROM topics WHERE user_id = ?)"
     anki-note-id card-id user-id]))

(defn set-anki-note-ids
  "Bulk set anki_note_id + anki_synced_at for `user-id`'s cards.
   Takes [[card-id note-id] ...]."
  [user-id pairs]
  (doseq [[card-id note-id] pairs]
    (set-anki-note-id user-id card-id note-id)))

(defn mark-anki-synced
  "Update anki_synced_at to now for a `user-id`-owned flashcard."
  [user-id card-id]
  (jdbc/execute-one! ds
    ["UPDATE flashcards SET anki_synced_at = CURRENT_TIMESTAMP
      WHERE id = ? AND root_topic_id IN (SELECT id FROM topics WHERE user_id = ?)"
     card-id user-id]))

(defn merge-flashcard-io-fields!
  "Shallow-merge a partial map into a card's io_fields (JSONB ||) and mark it
   synced — the Anki-pull write path for occlusion text fields. Unchanged
   keys keep their local values; updated_at is untouched so the row does not
   read as locally modified."
  [user-id card-id partial-fields]
  (jdbc/execute-one! ds
    ["UPDATE flashcards
      SET io_fields = COALESCE(io_fields, '{}'::jsonb) || ?,
          anki_synced_at = CURRENT_TIMESTAMP
      WHERE id = ? AND root_topic_id IN (SELECT id FROM topics WHERE user_id = ?)"
     (->jsonb partial-fields) card-id user-id]))

;; ---------------------------------------------------------------------------
;; Occlusion groups — geometry lives here, one 'occlusion' flashcard row per
;; MASK GROUP. Ordinal contract: an ordinal identifies a mask group, so several
;; rects MAY share one and sharing it IS the grouping (freememo.occlusion-ordinals
;; owns the algebra). Ordinals are assigned from the group's next_ordinal and
;; never reused — they bind row ↔ Anki note ↔ SVG rect id ↔ media filename.
;; All geometry writes are transactional with their row changes.
;; ---------------------------------------------------------------------------

(defn- parse-occlusion-group [row]
  (some-> row (update :occlusion_groups/geometry pgobject->clj)))

(defn get-occlusion-group
  "Group row with parsed :occlusion_groups/geometry, or nil."
  [group-id]
  (parse-occlusion-group
    (jdbc/execute-one! ds
      ["SELECT * FROM occlusion_groups WHERE id = ?" group-id])))

(defn get-occlusion-groups-by-ids
  "Map of group-id -> group row (geometry parsed). Empty ids → {}."
  [group-ids]
  (if (empty? group-ids)
    {}
    (into {}
      (map (fn [row]
             (let [row (parse-occlusion-group row)]
               [(:occlusion_groups/id row) row])))
      (jdbc/execute! ds
        (sql/format {:select [:*]
                     :from [:occlusion_groups]
                     :where [:in :id (vec group-ids)]})))))

(defn get-occlusion-cards
  "Flashcard rows of a group ordered by mask_ordinal, io_fields parsed."
  [group-id]
  (mapv normalize-flashcard-row
    (jdbc/execute! ds
      (sql/format {:select [:*]
                   :from [:flashcards]
                   :where [:= :occlusion_group_id group-id]
                   :order-by [[:mask_ordinal :asc]]}))))

(defn insert-occlusion-group!
  "Insert a group plus one 'occlusion' flashcard row per MASK GROUP, transactionally.
   attrs = {:topic-id :root-topic-id :image-media-id :mode :geometry :io-fields}
   Pre:  geometry = {:width :height :rects [{:x :y :w :h (:gid)} ...]}; rects
         sharing a :gid are one mask group (no :ordinal exists yet on create).
   Post: ordinals are assigned HERE, from 1, one per mask group — the group row
         stays the single ordinal authority — and next_ordinal points past them;
         stored rects carry :ordinal and no :gid.
   Returns {:group-id id :ids [flashcard-id ...]}, one id per mask group in
   first-appearance order."
  [{:keys [topic-id root-topic-id image-media-id mode geometry io-fields]}]
  (jdbc/with-transaction [tx ds]
    (let [[rects next-ordinal] (ord/assign-ordinals (vec (:rects geometry)) 1)
          group (jdbc/execute-one! tx
                  (sql/format {:insert-into :occlusion_groups
                               :values [{:anki_key (str (java.util.UUID/randomUUID))
                                         :image_media_id image-media-id
                                         :mode mode
                                         :geometry (->jsonb (assoc geometry :rects rects))
                                         :next_ordinal next-ordinal}]
                               :returning [:id]}))
          group-id (:occlusion_groups/id group)
          ids (mapv (fn [ordinal]
                      (:flashcards/id
                       (jdbc/execute-one! tx
                         (sql/format {:insert-into :flashcards
                                      :values [{:topic_id topic-id
                                                :root_topic_id root-topic-id
                                                :kind "occlusion"
                                                :occlusion_group_id group-id
                                                :mask_ordinal ordinal
                                                :io_fields (->jsonb (or io-fields {}))}]
                                      :returning [:id]}))))
                (ord/ordinals-in-order rects))]
      {:group-id group-id :ids ids})))

(defn reconcile-occlusion-group!
  "Apply a group edit in one transaction (full reconcile).
   attrs = {:group-id :mode :geometry :io-fields}
   Pre:  rects sharing an :ordinal are one mask group; rects sharing a :gid are
         one NEW mask group. An :ordinal naming a row this group no longer has
         is tolerated — the modal's snapshot goes stale whenever the card is
         deleted elsewhere while it is open.
   Post: rects with a live :ordinal are kept (position/size updated) and add no
         row; each distinct :gid mints one ordinal from next_ordinal and gets one
         row; rows whose ordinal is absent from the incoming rects are deleted;
         rects naming a dead ordinal are DROPPED, so a concurrent card deletion
         wins over the stale snapshot instead of persisting a rect no card covers.
   io-fields overwrite every surviving row, and every surviving row gets
   updated_at=now — geometry dirtiness is group-scoped because hide-all
   question masks embed the whole rect set.
   Returns {:group-id .. :added-ids [..]
            :removed [{:id .. :anki-note-id ..} ..]}."
  [{:keys [group-id mode geometry io-fields]}]
  (jdbc/with-transaction [tx ds]
    (let [group (jdbc/execute-one! tx
                  ["SELECT * FROM occlusion_groups WHERE id = ? FOR UPDATE" group-id])]
      (when-not group
        (throw (ex-info "Occlusion group not found" {:group-id group-id})))
      (let [next0 (:occlusion_groups/next_ordinal group)
            ;; ORDER BY mask_ordinal: `template` below supplies the io_fields
            ;; that get LOGGED as history and drives the change comparison, and
            ;; an Anki pull can diverge io_fields per row
            ;; (merge-flashcard-io-fields!). Unordered, which row Postgres
            ;; returned first would decide what history records. Lowest ordinal
            ;; matches get-group-for-edit's documented choice.
            existing (jdbc/execute! tx
                       ["SELECT id, mask_ordinal, anki_note_id, topic_id, root_topic_id, io_fields
                         FROM flashcards WHERE occlusion_group_id = ?
                         ORDER BY mask_ordinal ASC" group-id])
            existing-ordinals (set (map :flashcards/mask_ordinal existing))
            ;; Re-apply deletions that landed since the modal loaded: a rect
            ;; whose ordinal has no row belongs to a card someone deleted.
            {live-rects true stale-rects false}
            (group-by (fn [rect] (or (nil? (:ordinal rect))
                                   (contains? existing-ordinals (:ordinal rect))))
              (:rects geometry))
            [rects next'] (ord/assign-ordinals (vec live-rects) next0)
            kept-ordinals (set (map :ordinal rects))
            added-ordinals (filterv #(>= % next0) (ord/ordinals-in-order rects))
            template (first existing)
            removed (filterv #(not (contains? kept-ordinals (:flashcards/mask_ordinal %)))
                      existing)
            ;; Superseded rendition, captured before any write. The old geometry
            ;; still carries the rects of masks this edit removes, so a removed
            ;; mask stays viewable even though its flashcard row is deleted
            ;; below and card_versions is scoped to the GROUP, not the row.
            geometry' (assoc geometry :rects rects)
            io-fields' (or io-fields {})
            group-changed? (any-changed?
                             [[(:occlusion_groups/geometry group) geometry']
                              [(:occlusion_groups/mode group) mode]
                              [(:flashcards/io_fields template) io-fields']])]
        (when (seq stale-rects)
          (tel/log! {:level :warn :id ::stale-occlusion-rects
                     :data {:group-id group-id
                            :ordinals (vec (distinct (keep :ordinal stale-rects)))}}
            "Dropped occlusion rects whose card was deleted elsewhere"))
        (when (empty? rects)
          (throw (ex-info "Every mask in this edit was deleted elsewhere — reopen the group"
                   {:group-id group-id})))
        ;; History is logged AFTER the guards: an edit that aborts must not
        ;; leave a version row claiming a rendition was superseded.
        (when (and group-changed? template)
          (insert-card-version! tx
            {:root_topic_id (:flashcards/root_topic_id template)
             :scope_type "occlusion_group"
             :scope_id group-id
             :kind "occlusion"
             :geometry (:occlusion_groups/geometry group)
             :occlusion_mode (:occlusion_groups/mode group)
             :occlusion_image_media_id (:occlusion_groups/image_media_id group)
             :io_fields (:flashcards/io_fields template)}))
        (doseq [r removed]
          (jdbc/execute-one! tx ["DELETE FROM flashcards WHERE id = ?" (:flashcards/id r)]))
        (jdbc/execute! tx
          (sql/format {:update :flashcards
                       :set {:io_fields (->jsonb io-fields')
                             :updated_at [:now]}
                       :where [:= :occlusion_group_id group-id]}))
        (let [added-ids
              (mapv (fn [ordinal]
                      (:flashcards/id
                       (jdbc/execute-one! tx
                         (sql/format {:insert-into :flashcards
                                      :values [{:topic_id (:flashcards/topic_id template)
                                                :root_topic_id (:flashcards/root_topic_id template)
                                                :kind "occlusion"
                                                :occlusion_group_id group-id
                                                :mask_ordinal ordinal
                                                :io_fields (->jsonb io-fields')}]
                                      :returning [:id]}))))
                added-ordinals)]
          (jdbc/execute-one! tx
            (sql/format {:update :occlusion_groups
                         :set {:mode mode
                               :geometry (->jsonb geometry')
                               :next_ordinal next'
                               :updated_at [:now]}
                         :where [:= :id group-id]}))
          {:group-id group-id
           :added-ids added-ids
           :removed (mapv (fn [r] {:id (:flashcards/id r)
                                   :anki-note-id (:flashcards/anki_note_id r)})
                      removed)})))))

(defn remove-occlusion-mask!
  "Cleanup after a single occlusion flashcard row was deleted elsewhere:
   retire its rect (ordinal never reused), dirty the sibling rows so the next
   push regenerates the group's masks, and delete the group once no rows
   remain. Returns {:group-deleted? bool} (nil when the group is gone)."
  [group-id ordinal]
  (jdbc/with-transaction [tx ds]
    (when-let [group (jdbc/execute-one! tx
                       ["SELECT * FROM occlusion_groups WHERE id = ? FOR UPDATE" group-id])]
      (let [geometry (pgobject->clj (:occlusion_groups/geometry group))
            rects' (vec (remove #(= ordinal (:ordinal %)) (:rects geometry)))
            remaining (:n (jdbc/execute-one! tx
                            ["SELECT COUNT(*) AS n FROM flashcards WHERE occlusion_group_id = ?"
                             group-id]))]
        (if (zero? remaining)
          (do (jdbc/execute-one! tx ["DELETE FROM occlusion_groups WHERE id = ?" group-id])
            {:group-deleted? true})
          (do (jdbc/execute-one! tx
                (sql/format {:update :occlusion_groups
                             :set {:geometry (->jsonb (assoc geometry :rects rects'))
                                   :updated_at [:now]}
                             :where [:= :id group-id]}))
            (jdbc/execute! tx
              (sql/format {:update :flashcards
                           :set {:updated_at [:now]}
                           :where [:= :occlusion_group_id group-id]}))
            {:group-deleted? false}))))))

;; ---------------------------------------------------------------------------
;; Score groups — one (audio segment × notation rects) pair on a kind='score'
;; topic, fanning out to one 'score' flashcard row per direction. Clip and
;; per-rect crops are pre-materialized media rows; the group only references
;; them. Unlike occlusion there is no ordinal authority: rects don't bind to
;; individual cards, so an edit replaces geometry wholesale.
;; ---------------------------------------------------------------------------

(defn- parse-score-group [row]
  (some-> row (update :score_groups/geometry pgobject->clj)))

(defn get-score-group
  "Group row with parsed :score_groups/geometry, or nil."
  [group-id]
  (parse-score-group
    (jdbc/execute-one! ds
      ["SELECT * FROM score_groups WHERE id = ?" group-id])))

(defn get-score-groups-by-ids
  "Map of group-id -> group row (geometry parsed). Empty ids → {}."
  [group-ids]
  (if (empty? group-ids)
    {}
    (into {}
      (map (fn [row]
             (let [row (parse-score-group row)]
               [(:score_groups/id row) row])))
      (jdbc/execute! ds
        (sql/format {:select [:*]
                     :from [:score_groups]
                     :where [:in :id (vec group-ids)]})))))

(defn get-score-cards
  "Flashcard rows of a score group, audio-front first."
  [group-id]
  (jdbc/execute! ds
    (sql/format {:select [:*]
                 :from [:flashcards]
                 :where [:= :score_group_id group-id]
                 :order-by [[:score_direction :asc]]})))

(defn insert-score-group!
  "Insert a group plus one 'score' flashcard row per direction, transactionally.
   attrs = {:topic-id :root-topic-id :start-ms :end-ms :clip-media-id
            :geometry :directions [\"audio-front\" ...]}
   Rect ordinals are assigned HERE (1..N across pages in page order) — they
   index the crop stack on the card and name the Anki crop files.
   Returns {:group-id id :ids [flashcard-id ...]}."
  [{:keys [topic-id root-topic-id start-ms end-ms clip-media-id geometry directions]}]
  (jdbc/with-transaction [tx ds]
    (let [pages (vec (:pages geometry))
          pages' (first
                   (reduce (fn [[acc n] page]
                             (let [rects (mapv (fn [rect ordinal] (assoc rect :ordinal ordinal))
                                           (:rects page) (iterate inc n))]
                               [(conj acc (assoc page :rects rects))
                                (+ n (count rects))]))
                     [[] 1] pages))
          group (jdbc/execute-one! tx
                  (sql/format {:insert-into :score_groups
                               :values [{:anki_key (str (java.util.UUID/randomUUID))
                                         :start_ms start-ms
                                         :end_ms end-ms
                                         :clip_media_id clip-media-id
                                         :geometry (->jsonb {:pages pages'})}]
                               :returning [:id]}))
          group-id (:score_groups/id group)
          ids (mapv (fn [direction]
                      (:flashcards/id
                       (jdbc/execute-one! tx
                         (sql/format {:insert-into :flashcards
                                      :values [{:topic_id topic-id
                                                :root_topic_id root-topic-id
                                                :kind "score"
                                                :score_group_id group-id
                                                :score_direction direction}]
                                      :returning [:id]}))))
                directions)]
      {:group-id group-id :ids ids})))

(defn update-score-group!
  "Replace a group's segment, clip, and geometry wholesale (rect ordinals
   re-assigned 1..N — crops are regenerated media, nothing binds to old
   ordinals). Directions are fixed at creation; the edit dirties every card
   row so the next push re-uploads media and re-builds both notes.
   Returns {:group-id id}."
  [{:keys [group-id start-ms end-ms clip-media-id geometry]}]
  (jdbc/with-transaction [tx ds]
    (let [old (jdbc/execute-one! tx
                ["SELECT * FROM score_groups WHERE id = ? FOR UPDATE" group-id])]
      (when-not old
        (throw (ex-info "Score group not found" {:group-id group-id})))
      (let [pages' (first
                     (reduce (fn [[acc n] page]
                               (let [rects (mapv (fn [rect ordinal] (assoc rect :ordinal ordinal))
                                             (:rects page) (iterate inc n))]
                                 [(conj acc (assoc page :rects rects))
                                  (+ n (count rects))]))
                       [[] 1] (vec (:pages geometry))))
            geometry' {:pages pages'}
            ;; Superseded rendition, captured before the update. Direction is
            ;; per-card and fixed at creation, so it is not part of a group
            ;; rendition — the renderer reads it from the surviving card row.
            owner (jdbc/execute-one! tx
                    ["SELECT root_topic_id FROM flashcards WHERE score_group_id = ? LIMIT 1"
                     group-id])
            group-changed? (any-changed?
                             [[(:score_groups/geometry old) geometry']
                              [(:score_groups/start_ms old) start-ms]
                              [(:score_groups/end_ms old) end-ms]
                              [(:score_groups/clip_media_id old) clip-media-id]])]
        (when (and group-changed? owner)
          (insert-card-version! tx
            {:root_topic_id (:flashcards/root_topic_id owner)
             :scope_type "score_group"
             :scope_id group-id
             :kind "score"
             :geometry (:score_groups/geometry old)
             :score_start_ms (:score_groups/start_ms old)
             :score_end_ms (:score_groups/end_ms old)
             :score_clip_media_id (:score_groups/clip_media_id old)}))
        (jdbc/execute-one! tx
          (sql/format {:update :score_groups
                       :set {:start_ms start-ms
                             :end_ms end-ms
                             :clip_media_id clip-media-id
                             :geometry (->jsonb geometry')
                             :updated_at [:now]}
                       :where [:= :id group-id]}))
        (jdbc/execute! tx
          (sql/format {:update :flashcards
                       :set {:updated_at [:now]}
                       :where [:= :score_group_id group-id]}))
        {:group-id group-id}))))

(defn remove-score-card-cleanup!
  "After a single score flashcard row was deleted elsewhere: delete the group
   once no direction rows remain (its media rows stay — sha-deduped, orphan
   tolerated). Returns {:group-deleted? bool}."
  [group-id]
  (jdbc/with-transaction [tx ds]
    (let [remaining (:n (jdbc/execute-one! tx
                          ["SELECT COUNT(*) AS n FROM flashcards WHERE score_group_id = ?"
                           group-id]))]
      (if (zero? remaining)
        (do (jdbc/execute-one! tx ["DELETE FROM score_groups WHERE id = ?" group-id])
          {:group-deleted? true})
        {:group-deleted? false}))))

(defn mark-cards-exported
  "Mark cards as exported (sets anki_synced_at). Used after CSV export."
  [card-ids]
  (when (seq card-ids)
    (jdbc/execute! ds
      (sql/format {:update :flashcards
                   :set {:anki_synced_at [:now]}
                   :where [:in :id (vec card-ids)]}))))

(defn get-unsynced-card-count
  "Count unsynced cards for a specific topic."
  [topic-id]
  (let [result (jdbc/execute-one! ds
                 ["SELECT COUNT(*) AS cnt FROM flashcards
                   WHERE topic_id = ? AND (anki_synced_at IS NULL OR (updated_at IS NOT NULL AND updated_at > anki_synced_at))"
                  topic-id])]
    (or (:cnt result) 0)))

(defn get-unsynced-card-count-for-root
  "Count unsynced cards for an entire root topic tree."
  [root-topic-id]
  (let [result (jdbc/execute-one! ds
                 ["SELECT COUNT(*) AS cnt FROM flashcards
                   WHERE root_topic_id = ? AND (anki_synced_at IS NULL OR (updated_at IS NOT NULL AND updated_at > anki_synced_at))"
                  root-topic-id])]
    (or (:cnt result) 0)))

;; ---------------------------------------------------------------------------
;; Status overview (denormalized per-document stats)
;; ---------------------------------------------------------------------------

(defn get-document-status
  "Get progress stats for all root documents owned by user-id.
   For PDFs: counts kind='page' direct children.
   For non-PDFs: counts root itself + all descendants (recursive).
   Returns a vector of maps with :id, :title, :kind, :created_at,
   :total_items, :done_items, :total_cards, :synced_cards, plus
   bibliography fields from sources (:sources/title etc.)."
  [user-id]
  (jdbc/execute! ds
    ["SELECT t.id, t.title, t.kind, t.created_at,
             s.title, s.csl_type, s.container_title,
             CASE WHEN t.kind = 'pdf' THEN COALESCE(ps.total_pages, 0)
                  ELSE COALESCE(ds.total_items, 0)
             END AS total_items,
             CASE WHEN t.kind = 'pdf' THEN COALESCE(ps.done_pages, 0)
                  ELSE COALESCE(ds.done_items, 0)
             END AS done_items,
             COALESCE(cs.total_cards, 0)  AS total_cards,
             COALESCE(cs.synced_cards, 0) AS synced_cards
      FROM topics t
      LEFT JOIN sources s ON s.id = t.source_id
      LEFT JOIN LATERAL (
        SELECT COUNT(*)                                  AS total_pages,
               COUNT(*) FILTER (WHERE p.status = 'done') AS done_pages
        FROM topics p
        WHERE p.parent_id = t.id AND p.kind = 'page'
      ) ps ON true
      LEFT JOIN LATERAL (
        SELECT COUNT(*)                                  AS total_items,
               COUNT(*) FILTER (WHERE d.status = 'done') AS done_items
        FROM (
          WITH RECURSIVE descendants AS (
            SELECT t.id, t.status
            UNION ALL
            SELECT c.id, c.status
            FROM topics c
            JOIN descendants d ON c.parent_id = d.id
          )
          SELECT id, status FROM descendants
        ) d
      ) ds ON true
      LEFT JOIN LATERAL (
        SELECT COUNT(*)                                            AS total_cards,
               COUNT(*) FILTER (WHERE f.anki_synced_at IS NOT NULL
                                      AND (f.updated_at IS NULL OR f.updated_at <= f.anki_synced_at)) AS synced_cards
        FROM flashcards f
        WHERE f.root_topic_id = t.id
      ) cs ON true
      WHERE t.user_id = ? AND t.parent_id IS NULL AND t.staged_delete_id IS NULL
      ORDER BY t.created_at DESC"
     user-id]))

;; ---------------------------------------------------------------------------
;; Scheduling (unified — no topic-type dispatch)
;; ---------------------------------------------------------------------------

;; ---------------------------------------------------------------------------
;; Repetition history — append-only log of session-driven mutations.
;; Every mutation that writes to topics from a learn session also writes one
;; row capturing the pre-mutation snapshot of the six SR-relevant fields,
;; inside the same transaction.
;; ---------------------------------------------------------------------------

(defn- snapshot-topic-row
  "Read the six SR-relevant fields + user_id from `topics` inside an open tx.
   Returns nil if the topic doesn't exist."
  [tx id]
  (jdbc/execute-one! tx
    ["SELECT user_id, status, priority, interval_days, a_factor,
            next_review_at, last_review_at
       FROM topics WHERE id = ?" id]
    {:builder-fn rs/as-unqualified-maps}))

(defn- insert-repetition!
  "Append one row to topic_repetitions inside an open tx.
   Pre:  `before` is the snapshot returned by `snapshot-topic-row`.
   Post: one row exists with `event_type` and pre-mutation fields equal to `before`."
  [tx topic-id event-type before]
  (jdbc/execute-one! tx
    ["INSERT INTO topic_repetitions
        (topic_id, user_id, event_type,
         status_before, priority_before, interval_days_before, a_factor_before,
         next_review_at_before, last_review_at_before)
      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"
     topic-id (:user_id before) event-type
     (or (:status before) "active")
     (:priority before)
     (double (or (:interval_days before) 1.0))
     (double (or (:a_factor before) 2.0))
     (:next_review_at before)
     (:last_review_at before)]))

;; Forward decl: the scheduling + pin mutations below own-gate via subtree-owner,
;; which is defined further down (near owns-topic?).
(declare subtree-owner)

(defn advance-topic!
  "Advance a topic's review schedule using A-Factor algorithm.
   Logs 'advance' event with pre-mutation snapshot. Owner-gated by `user-id`
   (the topic's root carries the owner); a non-owner call is a no-op."
  [user-id id]
  (jdbc/with-transaction [tx ds]
    (when (= user-id (subtree-owner tx id))
     (when-let [before (snapshot-topic-row tx id)]
      (insert-repetition! tx id "advance" before)
      ;; Floor the interval to >= 1 day before multiplying. COALESCE guards NULL
      ;; but not 0/negatives, and 0 is a multiplication fixed point (0*a_factor=0
      ;; → next_review_at = NOW() → topic stays due forever). GREATEST(...,1.0)
      ;; enforces the "interval >= 1 day" invariant so every advance pushes the
      ;; review strictly into the future.
      (jdbc/execute-one! tx
        ["UPDATE topics
          SET interval_days = GREATEST(COALESCE(interval_days, 1.0), 1.0) * COALESCE(a_factor, 2.0),
              next_review_at = NOW() + (GREATEST(COALESCE(interval_days, 1.0), 1.0) * COALESCE(a_factor, 2.0)) * INTERVAL '1 day',
              last_review_at = NOW()
          WHERE id = ?"
         id])))))

(defn update-topic-priority!
  "Update a topic's priority (0=highest, 100=lowest). Owner-gated by `user-id`.
   Logs 'priority-change' event only when new value differs from current (no-op skipped)."
  [user-id id priority]
  (jdbc/with-transaction [tx ds]
    (when (= user-id (subtree-owner tx id))
     (when-let [before (snapshot-topic-row tx id)]
      (when (not= (:priority before) priority)
        (insert-repetition! tx id "priority-change" before)
        (jdbc/execute-one! tx
          ["UPDATE topics SET priority = ? WHERE id = ?" priority id]))))))

(defn postpone-topic!
  "Postpone a topic by N days without changing interval/a-factor. Owner-gated by
   `user-id`. Logs 'postpone' event with pre-mutation snapshot."
  [user-id id days]
  (jdbc/with-transaction [tx ds]
    (when (= user-id (subtree-owner tx id))
     (when-let [before (snapshot-topic-row tx id)]
      (insert-repetition! tx id "postpone" before)
      (jdbc/execute-one! tx
        ["UPDATE topics
          SET next_review_at = NOW() + ? * INTERVAL '1 day',
              last_review_at = NOW()
          WHERE id = ?"
         (double days) id])))))

(defn done-topic!
  "Mark a topic as done. Owner-gated by `user-id`. Logs 'done' event with
   pre-mutation snapshot."
  [user-id id]
  (jdbc/with-transaction [tx ds]
    (when (= user-id (subtree-owner tx id))
     (when-let [before (snapshot-topic-row tx id)]
      (insert-repetition! tx id "done" before)
      (jdbc/execute-one! tx
        ["UPDATE topics SET status = 'done' WHERE id = ?" id])))))

(defn restore-topic!
  "Restore a done topic back to active queue. Owner-gated by `user-id`.
   Logs 'restore' event."
  [user-id id]
  (jdbc/with-transaction [tx ds]
    (when (= user-id (subtree-owner tx id))
     (when-let [before (snapshot-topic-row tx id)]
      (insert-repetition! tx id "restore" before)
      (jdbc/execute-one! tx
        ["UPDATE topics SET status = 'active', next_review_at = NULL WHERE id = ?" id])))))

(defn touch-topic!
  "Update last_review_at without advancing the interval (subset-review soft rep).
   Owner-gated by `user-id`. Logs 'touch' event with pre-mutation snapshot."
  [user-id id]
  (jdbc/with-transaction [tx ds]
    (when (= user-id (subtree-owner tx id))
     (when-let [before (snapshot-topic-row tx id)]
      (insert-repetition! tx id "touch" before)
      (jdbc/execute-one! tx
        ["UPDATE topics SET last_review_at = NOW() WHERE id = ?" id])))))

(defn get-topic-history
  "Return repetition history rows for a topic, newest first.
   Timestamps pre-formatted server-side; interval_since_prev_days computed
   via LAG over ascending event_at so each row shows the gap from the prior
   chronological event. Numeric columns are cast to float8 so they cross the
   Transit wire as JS doubles (BigDecimal would arrive as an opaque tagged value)."
  [topic-id]
  (try
    (jdbc/execute! ds
      ["SELECT id,
              event_type,
              TO_CHAR(event_at, 'YYYY-MM-DD HH24:MI:SS')               AS event_at,
              status_before,
              priority_before,
              interval_days_before::float8                             AS interval_days_before,
              a_factor_before::float8                                  AS a_factor_before,
              TO_CHAR(next_review_at_before, 'YYYY-MM-DD HH24:MI:SS')  AS next_review_at_before,
              TO_CHAR(last_review_at_before, 'YYYY-MM-DD HH24:MI:SS')  AS last_review_at_before,
              (EXTRACT(EPOCH FROM
                       (event_at - LAG(event_at)
                                     OVER (ORDER BY event_at ASC, id ASC)))
                / 86400.0)::float8                                     AS interval_since_prev_days
         FROM topic_repetitions
         WHERE topic_id = ?
         ORDER BY event_at DESC, id DESC"
       topic-id]
      {:builder-fn rs/as-unqualified-maps})
    (catch Exception e
      (tel/error! {:id ::get-topic-history :data {:topic-id topic-id}} e)
      [])))

(defn get-topic-next-review
  "Return current next_review_at + status + title for a topic, formatted for the
   HistoryModal's 'Next' header row (SuperMemo parity).
   days_until_next is cast to float8 — see get-topic-history for the rationale."
  [topic-id]
  (jdbc/execute-one! ds
    ["SELECT title,
            status,
            priority,
            interval_days::float8                            AS interval_days,
            a_factor::float8                                 AS a_factor,
            TO_CHAR(next_review_at, 'YYYY-MM-DD HH24:MI:SS') AS next_review_at,
            CASE WHEN next_review_at IS NULL THEN NULL
                 ELSE (EXTRACT(EPOCH FROM (next_review_at - NOW())) / 86400.0)::float8 END
                                                              AS days_until_next
       FROM topics WHERE id = ?"
     topic-id]
    {:builder-fn rs/as-unqualified-maps}))

;; ---------------------------------------------------------------------------
;; Queue queries (single SELECT, no UNION ALL)
;; ---------------------------------------------------------------------------

(defn get-learning-queue
  "Due topics for incremental reading. Single query, no UNION.
   Feeds BOTH the Start Learning session and the Learn overview list, so the
   list IS the queue: the top row is the topic the session opens.
   Ordering (SuperMemo outstanding-queue model): date gates membership only;
   priority orders the queue; a per-day hash of the id breaks ties within a
   priority band, so equal-priority topics shuffle daily (stable within a day,
   immune to mid-session :refresh) rather than ordering by due date.
   `sm_rank` sits between the two: rounding a SuperMemo collection's priority
   order into 0-100 collapses ~15 topics per band, and without this term the
   daily hash would discard the order the import went to the trouble of
   preserving. NULLS LAST keeps natively created topics on the hash.
   Selects `t.content` for the overview's preview titles; the session reads it
   per-row via nth, so it stays server-side until a row is accessed.
   Joins `sources` to surface bibliography fields (source_url, source_title,
   source_csl_type, source_container); NULL when the topic has no source_id."
  [user-id]
  (try
    (jdbc/execute! ds
      ["SELECT t.id, t.parent_id, t.kind, t.title, t.priority,
              t.next_review_at, t.interval_days, t.a_factor,
              t.status, t.content,
              s.url, s.title, s.csl_type, s.container_title
       FROM topics t
       LEFT JOIN sources s ON s.id = t.source_id
       WHERE t.user_id = ?
         AND t.kind != 'page'
         AND t.staged_delete_id IS NULL
         AND (t.next_review_at::date <= CURRENT_DATE OR t.next_review_at IS NULL)
         AND (t.status = 'active' OR t.status IS NULL)
         AND NOT t.dismissed
       ORDER BY t.priority ASC, t.sm_rank ASC NULLS LAST,
                md5(t.id::text || CURRENT_DATE::text) ASC, t.id ASC" user-id])
    (catch Exception e
      (tel/error! {:id ::get-learning-queue} e)
      [])))

(defn get-learning-queue-count
  "Count of due topics."
  [user-id]
  (let [result (jdbc/execute-one! ds
                 ["SELECT COUNT(*) AS total FROM topics
                   WHERE user_id = ?
                     AND kind != 'page'
                     AND staged_delete_id IS NULL
                     AND (next_review_at::date <= CURRENT_DATE OR next_review_at IS NULL)
                     AND (status = 'active' OR status IS NULL)
                     AND NOT dismissed"
                  user-id])]
    (or (:total result) 0)))

(defn get-total-topic-count
  "Count ALL topics (due and not due, excluding done and pages)."
  [user-id]
  (let [result (jdbc/execute-one! ds
                 ["SELECT COUNT(*) AS total FROM topics
                   WHERE user_id = ?
                     AND kind != 'page'
                     AND staged_delete_id IS NULL
                     AND (status = 'active' OR status IS NULL)
                     AND NOT dismissed"
                  user-id])]
    (or (:total result) 0)))

(defn get-queue-summary
  "Aggregate queue stats in a single SQL query."
  [user-id]
  (try
    (let [result (jdbc/execute-one! ds
                   ["SELECT COUNT(*) AS total,
                           COUNT(*) FILTER (WHERE status = 'done' AND NOT dismissed) AS inactive,
                           COUNT(*) FILTER (WHERE (status = 'active' OR status IS NULL)
                                             AND NOT dismissed
                                             AND (next_review_at IS NULL
                                                  OR next_review_at <= CURRENT_TIMESTAMP)) AS due_today,
                           COUNT(*) FILTER (WHERE (status = 'active' OR status IS NULL)
                                             AND NOT dismissed
                                             AND (next_review_at IS NULL
                                                  OR next_review_at <= CURRENT_TIMESTAMP + INTERVAL '7 days')) AS due_week
                    FROM topics
                    WHERE user_id = ? AND kind != 'page' AND staged_delete_id IS NULL" user-id])]
      {:total (or (:total result) 0)
       :inactive (or (:inactive result) 0)
       :due-today (or (:due_today result) 0)
       :due-week (or (:due_week result) 0)})
    (catch Exception e
      (tel/error! {:id ::get-queue-summary} e)
      {:total 0 :inactive 0 :due-today 0 :due-week 0})))

(defn get-review-calendar
  "Topic counts per calendar day from today (inclusive) through +N days.
   Date-bucketed (not timestamp), so today's already-past items count at day 0
   rather than leaking into the overdue backlog (see get-overdue-count)."
  [user-id days]
  (jdbc/execute! ds
    ["SELECT DATE(next_review_at) AS review_date, COUNT(*) AS count
      FROM topics
      WHERE user_id = ?
        AND kind != 'page'
        AND staged_delete_id IS NULL
        AND next_review_at::date BETWEEN CURRENT_DATE AND CURRENT_DATE + ?::int
        AND (status = 'active' OR status IS NULL)
        AND NOT dismissed
      GROUP BY DATE(next_review_at)
      ORDER BY review_date"
     user-id days]))

(defn get-overdue-count
  "Backlog: active topics whose review date is before today (overdue).
   Charted as the single backlog bar in the Future Due chart."
  [user-id]
  (let [r (jdbc/execute-one! ds
            ["SELECT COUNT(*) AS n
              FROM topics
              WHERE user_id = ?
                AND kind != 'page'
                AND staged_delete_id IS NULL
                AND next_review_at::date < CURRENT_DATE
                AND (status = 'active' OR status IS NULL)
                AND NOT dismissed"
             user-id])]
    (or (:n r) 0)))

(defn get-study-calendar
  "Distinct topics advanced per day over the last 14 days (incl. today).
   Keys: :d (YYYY-MM-DD string, server zone), :c (distinct topic count)."
  [user-id]
  (try
    (jdbc/execute! ds
      ["SELECT TO_CHAR(DATE(event_at), 'YYYY-MM-DD') AS d,
               COUNT(DISTINCT topic_id) AS c
        FROM topic_repetitions
        WHERE user_id = ?
          AND event_type = 'advance'
          AND event_at >= CURRENT_DATE - INTERVAL '13 days'
        GROUP BY DATE(event_at)
        ORDER BY d"
       user-id])
    (catch Exception e
      (tel/error! {:id ::get-study-calendar} e)
      [])))

(defn get-study-streak
  "Consecutive calendar days with >=1 advance, Anki-style.
   Anchor = today if studied today, else yesterday; counts backward from anchor.
   A zero-advance today does not break the streak until the day fully passes."
  [user-id]
  (try
    (let [rows (jdbc/execute! ds
                 ["SELECT DISTINCT DATE(event_at) AS d
                   FROM topic_repetitions
                   WHERE user_id = ? AND event_type = 'advance'" user-id])
          dayset (set (map #(java.time.LocalDate/parse (str (:d %))) rows))
          today (java.time.LocalDate/now)
          yday (.minusDays today 1)
          anchor (cond (contains? dayset today) today
                       (contains? dayset yday) yday
                       :else nil)]
      (if (nil? anchor)
        0
        (loop [d anchor n 0]
          (if (contains? dayset d)
            (recur (.minusDays d 1) (inc n))
            n))))
    (catch Exception e
      (tel/error! {:id ::get-study-streak} e)
      0)))

(defn get-review-counts
  "Advance-event counts. :all-time = all advances; :this-week = rolling 7 days."
  [user-id]
  (try
    (let [r (jdbc/execute-one! ds
              ["SELECT COUNT(*) AS all_time,
                       COUNT(*) FILTER (WHERE event_at >= NOW() - INTERVAL '7 days') AS this_week
                FROM topic_repetitions
                WHERE user_id = ? AND event_type = 'advance'" user-id])]
      {:all-time (or (:all_time r) 0)
       :this-week (or (:this_week r) 0)})
    (catch Exception e
      (tel/error! {:id ::get-review-counts} e)
      {:all-time 0 :this-week 0})))

(defn get-status-breakdown
  "Topic counts by status, same scope as get-queue-summary."
  [user-id]
  (try
    (let [r (jdbc/execute-one! ds
              ["SELECT COUNT(*) FILTER (WHERE (status = 'active' OR status IS NULL) AND NOT dismissed) AS active,
                       COUNT(*) FILTER (WHERE status = 'done' AND NOT dismissed) AS done,
                       COUNT(*) FILTER (WHERE dismissed) AS dismissed
                FROM topics
                WHERE user_id = ? AND kind != 'page' AND staged_delete_id IS NULL" user-id])]
      {:active (or (:active r) 0)
       :done (or (:done r) 0)
       :dismissed (or (:dismissed r) 0)})
    (catch Exception e
      (tel/error! {:id ::get-status-breakdown} e)
      {:active 0 :done 0 :dismissed 0})))

(defn get-inactive-topics
  "Topics excluded from the Learning Queue for a user: done OR dismissed.
   (dismissed is an orthogonal flag, so a topic can be both.)"
  [user-id]
  (try
    (jdbc/execute! ds
      ["SELECT id, parent_id, kind, title, created_at, status, dismissed
       FROM topics
       WHERE user_id = ?
         AND kind != 'page'
         AND staged_delete_id IS NULL
         AND (status = 'done' OR dismissed)
       ORDER BY created_at DESC" user-id])
    (catch Exception e
      (tel/error! {:id ::get-inactive-topics} e)
      [])))

;; ---------------------------------------------------------------------------
;; Tree navigation + Knowledge tree
;; ---------------------------------------------------------------------------

(defn get-knowledge-tree
  "Fetch all topics with parent references for building the knowledge tree.
   Includes page topics so parent chain (PDF → page → extract) is intact.
   Joins `sources` to surface bibliography fields (source_title,
   source_csl_type, source_container) for badge + subtitle rendering."
  [user-id]
  (jdbc/execute! ds
    (sql/format {:select [[:t.id :id] [:t.parent_id :parent_id] [:t.title :title]
                          [:t.kind :kind] [:t.status :status] [:t.created_at :created_at]
                          [:t.dismissed :dismissed]
                          [:t.page_number :page_number] [:t.last_review_at :last_review_at]
                          :s.title :s.csl_type :s.container_title
                          [[:coalesce :tf.file_size [:octet_length [:coalesce :t.content ""]]] :file_size]
                          [[:to_char :t.created_at [:inline "Mon DD"]] :formatted_date]]
                 :from [[:topics :t]]
                 ;; Aggregated file sizes — a direct topic_files join would
                 ;; duplicate score topics (two file rows: PDF + audio).
                 :left-join [[topic-file-sizes :tf] [:= :tf.topic_id :t.id]
                             [:sources :s] [:= :t.source_id :s.id]]
                 :where [:and [:= :t.user_id user-id] [:is :t.staged_delete_id nil]]
                 :order-by [[:t.parent_id :asc-nulls-first] [:t.page_number :asc-nulls-first] [:t.created_at :asc]]})))

(defn get-extract-source-page
  "For an extract whose immediate parent is a PDF page, return
   {:root <pdf-root-id> :page <page-number>}; nil otherwise (e.g. a web/epub
   extract, or a page/root topic). Used by the 'Go to page' toolbar button."
  [extract-id]
  (when extract-id
    (let [ext    (get-topic extract-id)
          parent (when (:topics/parent_id ext) (get-topic (:topics/parent_id ext)))]
      (when (and parent
              (= "page" (:topics/kind parent))
              (:topics/page_number parent))
        (let [root-id (get-root-topic-id extract-id)
              root    (when root-id (get-topic root-id))]
          (when (= "pdf" (:topics/kind root))
            {:root root-id :page (:topics/page_number parent)}))))))

(defn get-subtree
  "Get a topic and all its descendants via recursive CTE.
   Selects from topics directly (not the CTE alias) so JDBC metadata
   reports the correct table name and next.jdbc produces :topics/xxx keys."
  [user-id root-id]
  (jdbc/execute! ds
    ["WITH RECURSIVE subtree(id) AS (
        SELECT id FROM topics WHERE id = ?
        UNION ALL
        SELECT child.id FROM topics child
        JOIN subtree ON child.parent_id = subtree.id
      )
      SELECT t.* FROM topics t
      JOIN subtree s ON t.id = s.id
      WHERE (t.user_id = ? OR t.user_id IS NULL)
        AND t.staged_delete_id IS NULL
      ORDER BY t.parent_id ASC NULLS FIRST, t.page_number ASC NULLS LAST, t.id ASC"
     root-id user-id]))

;; ---------------------------------------------------------------------------
;; Media registry — per-user blobs (images today)
;; ---------------------------------------------------------------------------

(defn- bytes-sha256
  "Hex-encoded SHA-256 of a byte array."
  [^bytes b]
  (let [md (java.security.MessageDigest/getInstance "SHA-256")
        digest (.digest md b)]
    (apply str (map #(format "%02x" (bit-and % 0xff)) digest))))

(defn find-media-by-sha
  "Look up an existing media row for (user-id, sha256). Returns row or nil."
  [user-id sha256]
  (jdbc/execute-one! ds
    (sql/format {:select [:id :mime_type :byte_size :kind]
                 :from [:media]
                 :where [:and [:= :user_id user-id] [:= :sha256 sha256]]})))

(defn get-media
  "Get a media row by id (includes bytes for serving)."
  [id]
  (jdbc/execute-one! ds
    (sql/format {:select [:id :user_id :kind :bytes :mime_type :byte_size]
                 :from [:media]
                 :where [:= :id id]})))

(defn upsert-media!
  "Insert media or return existing id if same bytes already stored for this user.
   Charges quota only on actual insert.

   `:meter-quota?` defaults to true and MUST stay true for anything accepting
   new bytes from a user. Pass false only when the bytes are already stored
   elsewhere in this user's own rows and are merely being relocated — metering
   those would charge for storage the user already holds, and
   quota/check-and-bump! THROWS on cap violation, which would abort the
   relocation. Sole such caller: normalize-inline-card-images!."
  [{:keys [user-id kind ^bytes bytes mime-type source-url meter-quota?]
    :or {meter-quota? true}}]
  (let [sha256 (bytes-sha256 bytes)
        byte-size (alength bytes)]
    (if-let [existing (find-media-by-sha user-id sha256)]
      (:media/id existing)
      (jdbc/with-transaction [tx ds]
        (when meter-quota?
          (quota/check-and-bump! tx user-id byte-size))
        (-> (jdbc/execute-one! tx
              (sql/format {:insert-into :media
                           :values [{:user_id user-id
                                     :kind (or kind "image")
                                     :bytes bytes
                                     :mime_type mime-type
                                     :sha256 sha256
                                     :byte_size byte-size
                                     :source_url source-url}]
                           :returning [:id]}))
          :media/id)))))

(defn- rewrite-inline-images
  "Replace every data:image URI in `html` with /api/media/<id>, storing each
   payload as media owned by `user-id`. Returns [html' image-count].
   Pre : `html` may be nil or blank.
   Post: html' contains no 'data:image'; identical payloads collapse to one
         media row (upsert-media! dedupes on (user_id, sha256))."
  [user-id html]
  (if (or (str/blank? html) (not (str/includes? html "data:image")))
    [html 0]
    (let [n (volatile! 0)
          ;; The payload class excludes quotes, so a match cannot run past the
          ;; closing attribute delimiter; \s is included because HTML may wrap
          ;; long base64, and getMimeDecoder tolerates the whitespace.
          html' (str/replace html
                  #"data:image/([A-Za-z0-9.+-]+);base64,([A-Za-z0-9+/=\s]+)"
                  (fn [[_ subtype payload]]
                    (let [bytes (.decode (java.util.Base64/getMimeDecoder) ^String payload)
                          media-id (upsert-media! {:user-id user-id :kind "image"
                                                   :bytes bytes
                                                   :mime-type (str "image/" subtype)
                                                   ;; Relocation, not an upload —
                                                   ;; see upsert-media!.
                                                   :meter-quota? false})]
                      (vswap! n inc)
                      (str "/api/media/" media-id))))]
      [html' @n])))

(defn normalize-inline-card-images!
  "One-time: move inline data: URIs out of card fields into media rows.

   A few production cards predate the editor's upload-and-rewrite paths
   (the quill-field paste matcher and editor-image-menu, which both produce
   /api/media/<id>) and carry base64 inline. Left there, every version of such
   a card would duplicate the whole payload — measured at 8.86 MB for three
   cards, 2.6x the entire fleet's text history (plans/card-edit-history.md §3).
   Running at boot guarantees the precondition holds before any save can
   version anything.

   Pre : none — safe on an already-clean database.
   Post: every card this run converted has no 'data:image' left, and each
         payload is a media row referenced as /api/media/<id>. updated_at is
         deliberately NOT touched: this is not a user edit. No card_versions row
         is written — the pre-normalization rendition is deliberately
         unversioned.
   Inv : idempotent — a second run matches converted cards no more.
   Inv : NEVER throws. It runs inside setup-schema, so an escaping exception
         would stop the server from booting; a card that fails to convert is
         logged and skipped, leaving it exactly as it was. Its versions will
         duplicate its payload, which is a storage cost, not a correctness bug.
   Returns {:cards n :images n :failed n}."
  []
  (let [rows (jdbc/execute! ds
               ["SELECT f.id, f.question, f.answer, f.cloze, t.user_id
                 FROM flashcards f JOIN topics t ON t.id = f.root_topic_id
                 WHERE f.question LIKE '%data:image%'
                    OR f.answer   LIKE '%data:image%'
                    OR f.cloze    LIKE '%data:image%'"])
        result (reduce
                 (fn [acc row]
                   ;; Per-card isolation: malformed base64, a missing user, or a
                   ;; DB error must cost one card, not the boot.
                   (try
                     (let [user-id (:topics/user_id row)
                           [q nq] (rewrite-inline-images user-id (:flashcards/question row))
                           [a na] (rewrite-inline-images user-id (:flashcards/answer row))
                           [c nc] (rewrite-inline-images user-id (:flashcards/cloze row))
                           total (+ nq na nc)]
                       (if (zero? total)
                         acc
                         (do
                           ;; Direct UPDATE, deliberately NOT
                           ;; update-flashcard-versioned! — see Post above.
                           (jdbc/execute-one! ds
                             (sql/format {:update :flashcards
                                          :set {:question q :answer a :cloze c}
                                          :where [:= :id (:flashcards/id row)]}))
                           (-> acc (update :cards inc) (update :images + total)))))
                     (catch Exception e
                       (tel/error! {:id ::normalize-inline-card-images
                                    :data {:card-id (:flashcards/id row)}} e)
                       (update acc :failed inc))))
                 {:cards 0 :images 0 :failed 0}
                 rows)]
    (when (or (pos? (:cards result)) (pos? (:failed result)))
      (tel/log! {:level (if (pos? (:failed result)) :warn :info)
                 :id ::normalize-inline-card-images :data result}
        "Normalized inline card images to media references"))
    result))

;; ---------------------------------------------------------------------------
;; Topic pins — per-topic media references with front/back placement (K1 cap = 2)
;; ---------------------------------------------------------------------------

(defn get-pins
  "Return all pins for a topic, ordered by ord ASC."
  [topic-id]
  (jdbc/execute! ds
    (sql/format {:select [:id :topic_id :media_id :placement :ord]
                 :from [:topic_pins]
                 :where [:= :topic_id topic-id]
                 :order-by [[:ord :asc]]})))

(defn set-pin!
  "Insert a new pin for a topic. Enforces K1 cap (≤2 per topic).
   Throws ex-info :pin-cap-exceeded on violation. Returns new row id."
  [{:keys [topic-id media-id placement]}]
  (when-not (#{"front" "back"} placement)
    (throw (ex-info "Invalid placement" {:placement placement})))
  (jdbc/with-transaction [tx ds]
    (let [existing (jdbc/execute! tx
                     (sql/format {:select [:id] :from [:topic_pins]
                                  :where [:= :topic_id topic-id]}))]
      (when (>= (count existing) 2)
        (throw (ex-info "Topic already has 2 pins (max)"
                 {:reason :pin-cap-exceeded :topic-id topic-id})))
      (-> (jdbc/execute-one! tx
            (sql/format {:insert-into :topic_pins
                         :values [{:topic_id topic-id
                                   :media_id media-id
                                   :placement placement
                                   :ord (count existing)}]
                         :returning [:id]}))
        :topic_pins/id))))

(defn remove-pin!
  "Delete pin `pin-id` iff owned by `user-id` (the pin's topic's root carries the
   owner). Returns the full deleted row (RETURNING *) for undo, or nil when not
   owned / absent."
  [user-id pin-id]
  (jdbc/with-transaction [tx ds]
    (when-let [pin (jdbc/execute-one! tx
                     ["SELECT topic_id FROM topic_pins WHERE id = ?" pin-id])]
      (when (= user-id (subtree-owner tx (:topic_pins/topic_id pin)))
        (jdbc/execute-one! tx
          (sql/format {:delete-from :topic_pins
                       :where [:= :id pin-id]
                       :returning [:*]}))))))

(defn update-pin-placement!
  "Update pin `pin-id`'s placement iff owned by `user-id`. Throws on invalid value."
  [user-id pin-id new-placement]
  (when-not (#{"front" "back"} new-placement)
    (throw (ex-info "Invalid placement" {:placement new-placement})))
  (jdbc/with-transaction [tx ds]
    (when-let [pin (jdbc/execute-one! tx
                     ["SELECT topic_id FROM topic_pins WHERE id = ?" pin-id])]
      (when (= user-id (subtree-owner tx (:topic_pins/topic_id pin)))
        (jdbc/execute-one! tx
          (sql/format {:update :topic_pins
                       :set {:placement new-placement}
                       :where [:= :id pin-id]}))))))

(defn copy-pins-to-child!
  "Snapshot parent topic's pins into a newly created child topic.
   Called from extract-create flow (EC-snapshot). No-op if parent has no pins."
  [parent-topic-id child-topic-id]
  (when-let [parent-pins (seq (get-pins parent-topic-id))]
    (jdbc/execute! ds
      (sql/format {:insert-into :topic_pins
                   :values (mapv (fn [row]
                                   {:topic_id child-topic-id
                                    :media_id (:topic_pins/media_id row)
                                    :placement (:topic_pins/placement row)
                                    :ord (:topic_pins/ord row)})
                             parent-pins)}))))

(defn- tree-order-items
  "Sort items in depth-first tree order."
  [items]
  (let [id-set (set (map :topics/id items))
        by-parent (group-by :topics/parent_id items)
        roots (->> items
                (filter #(not (id-set (:topics/parent_id %))))
                (sort-by (juxt :topics/page_number :topics/id)))]
    (loop [result []
           stack (vec (reverse roots))]
      (if (empty? stack)
        result
        (let [node (peek stack)
              children (->> (get by-parent (:topics/id node))
                         (sort-by (juxt :topics/page_number :topics/id))
                         reverse vec)]
          (recur (conj result node)
            (into (pop stack) children)))))))

(defn get-subset-review-queue
  "Get the subset review queue for a topic subtree.
   Filters out done items. Annotates :outstanding?.
   When include-reviewed-today? is false (default), also filters out items reviewed today."
  [user-id root-id & {:keys [include-reviewed-today?] :or {include-reviewed-today? false}}]
  (let [raw (get-subtree user-id root-id)
        now (java.sql.Timestamp. (System/currentTimeMillis))
        today (java.time.LocalDate/now)]
    (let [active (->> raw
                   (remove #(= "page" (:topics/kind %)))
                   (filter #(let [s (:topics/status %)] (or (= s "active") (nil? s)))))
          filtered (if include-reviewed-today?
                     active
                     (remove (fn [item]
                               (when-let [la (:topics/last_review_at item)]
                                 (= today (.. la toInstant (atZone (java.time.ZoneId/systemDefault)) toLocalDate))))
                       active))
          ordered (tree-order-items filtered)]
      ;; Slim to the fields the review session reads — get-subtree SELECTs
      ;; t.* (content + content_text included); shipping those for every
      ;; subtree topic is pure wire waste (TopicPage fetches content by id).
      (mapv (fn [item]
              (let [nra (:topics/next_review_at item)]
                (assoc (select-keys item [:topics/id :topics/parent_id :topics/kind
                                          :topics/title :topics/status :topics/page_number])
                  :outstanding? (or (nil? nra)
                                  (.before nra now)))))
        ordered))))

;; ---------------------------------------------------------------------------
;; Undo log — bounded per-user action history (12h window, ≤100 entries).
;; Forward (log) writes happen at delete time; reverse (restore) replays a
;; snapshot. Restores are idempotent via ON CONFLICT (id) DO NOTHING, so a
;; double-undo cannot resurrect a row twice.
;; ---------------------------------------------------------------------------

(defn- unqualify-row
  "Strip the table namespace from a next.jdbc row's keys (:flashcards/id → :id)
   so a stored snapshot is portable across the wire and back into an INSERT.
   JSONB column values (PGobject) are parsed to plain data — cheshire cannot
   encode PGobject when the snapshot itself is stored as JSONB."
  [row]
  (-> row
    (update-keys (comp keyword name))
    (update-vals pgobject->clj)))

(def ^:private flashcard-ts-cols
  "Timestamp columns that arrive as ISO strings after a JSONB round-trip and
   must be CAST back to timestamp on restore."
  #{:created_at :updated_at :anki_synced_at})

(defn- cast-timestamps
  "Wrap present, non-nil timestamp values as CAST(? AS timestamp) for honeysql."
  [row ts-cols]
  (reduce (fn [m k]
            (if (some? (get m k))
              (update m k (fn [v] [:cast v :timestamp]))
              m))
    row ts-cols))

(defn prune-undo-log!
  "Drop user-id's undo entries older than 12h or beyond the 100 most recent.
   Idempotent; called after every insert to keep the log bounded.
   Excludes 'delete-document' entries entirely: removing one would SET NULL its
   topics' staged_delete_id and resurface a document whose cards are already
   gone. Those entries are removed only by purge-staged-documents! (which
   hard-deletes the topics first)."
  [conn user-id]
  (jdbc/execute! conn
    ["DELETE FROM undo_log
      WHERE user_id = ?
        AND action_type <> 'delete-document'
        AND (occurred_at < now() - interval '12 hours'
             OR id IN (SELECT id FROM undo_log
                       WHERE user_id = ?
                         AND action_type <> 'delete-document'
                       ORDER BY occurred_at DESC
                       OFFSET 100))"
     user-id user-id]))

(defn insert-undo-entry-raw!
  "Insert an undo entry with an already-shaped snapshot value (vector OR map),
   on the given connectable (joins a caller's transaction). Returns the id.
   Does NOT prune — the caller prunes once its mutation is committed."
  [conn user-id action-type entity-type entity-refs snapshot]
  (-> (jdbc/execute-one! conn
        (sql/format {:insert-into :undo_log
                     :values [{:user_id user-id
                               :action_type action-type
                               :entity_type entity-type
                               :entity_refs (->jsonb (vec entity-refs))
                               :snapshot (->jsonb snapshot)}]
                     :returning [:id]}))
    :undo_log/id))

(defn insert-undo-entry!
  "Append an undo entry and prune the user's log. snapshot-rows is a seq of
   maps (next.jdbc rows or plain maps) restored verbatim on undo; their keys
   are unqualified before storage. Returns the new entry id.
   Not transactional with the caller's mutation: a failure here loses
   undoability for that action but never corrupts data."
  [user-id action-type entity-type entity-refs snapshot-rows]
  (let [id (insert-undo-entry-raw! ds user-id action-type entity-type entity-refs
             (mapv unqualify-row snapshot-rows))]
    (prune-undo-log! ds user-id)
    id))

(defn- parse-undo-row
  "Decode a raw undo_log row: keywordized unqualified keys with :entity_refs
   and :snapshot parsed back from JSONB."
  [row]
  (-> row
    (update :entity_refs pgobject->clj)
    (update :snapshot pgobject->clj)))

(defn get-undo-entries
  "Live (not-yet-undone) undo entries for user-id within the 12h window,
   newest first, capped at 100."
  [user-id]
  (mapv parse-undo-row
    (jdbc/execute! ds
      (sql/format {:select [:*]
                   :from [:undo_log]
                   :where [:and
                           [:= :user_id user-id]
                           [:is :undone_at nil]
                           [:> :occurred_at [:raw "now() - interval '12 hours'"]]]
                   :order-by [[:occurred_at :desc]]
                   :limit 100})
      {:builder-fn rs/as-unqualified-maps})))

(defn get-undo-entry
  "Single undo entry by id, or nil."
  [entry-id]
  (some-> (jdbc/execute-one! ds
            (sql/format {:select [:*] :from [:undo_log] :where [:= :id entry-id]})
            {:builder-fn rs/as-unqualified-maps})
    parse-undo-row))

(defn mark-undone!
  "Stamp undone_at on a live entry. Idempotent: a no-op if already undone.
   Returns the jdbc update count (0 when already undone)."
  [entry-id]
  (::jdbc/update-count
   (jdbc/execute-one! ds
     (sql/format {:update :undo_log
                  :set {:undone_at [:now]}
                  :where [:and [:= :id entry-id] [:is :undone_at nil]]}))))

(defn restore-flashcards-tx!
  "Re-insert deleted flashcards on the given connectable, preserving original
   ids. ON CONFLICT (id) DO NOTHING — a card still present is left untouched.
   Clears each card's Anki association (anki_note_id, anki_synced_at): the delete
   path removed the Anki notes, so a restored card must come back UNSYNCED rather
   than falsely rendering as pushed/synced."
  [conn snapshot-rows]
  (when (seq snapshot-rows)
    (jdbc/execute! conn
      (sql/format {:insert-into :flashcards
                   :values (mapv #(as-> % row
                                    (assoc row :anki_note_id nil :anki_synced_at nil)
                                    (cast-timestamps row flashcard-ts-cols)
                                    ;; JSONB round-trip left a plain map — re-wrap.
                                    (cond-> row
                                      (some? (:io_fields row))
                                      (update :io_fields ->jsonb)))
                                  snapshot-rows)
                   :on-conflict [:id]
                   :do-nothing true}))))

(defn restore-flashcards!
  "Re-insert deleted flashcards from a snapshot (non-transactional)."
  [snapshot-rows]
  (restore-flashcards-tx! ds snapshot-rows))

(defn restore-pin!
  "Re-insert a removed pin from its snapshot, preserving the original id.
   ON CONFLICT (id) DO NOTHING. Bypasses the ≤2 cap (set-pin!): undo restores
   exactly what was removed, so the cap cannot be exceeded."
  [snapshot-row]
  (when snapshot-row
    (jdbc/execute! ds
      (sql/format {:insert-into :topic_pins
                   :values [snapshot-row]
                   :on-conflict [:id]
                   :do-nothing true}))))

;; ---------------------------------------------------------------------------
;; Staged document deletion — soft-hide a topic subtree + its binary, hard-
;; delete its cards (snapshotted), reversible for 12h as one undo_log entry.
;; A background scheduler hard-deletes staged subtrees once the window elapses.
;; ---------------------------------------------------------------------------

(defn- subtree-topic-ids
  "Ids of root-id and all its descendants (via parent_id)."
  [conn root-id]
  (mapv :id
    (jdbc/execute! conn
      ["WITH RECURSIVE subtree(id) AS (
          SELECT id FROM topics WHERE id = ?
          UNION ALL
          SELECT c.id FROM topics c JOIN subtree s ON c.parent_id = s.id)
        SELECT id FROM subtree" root-id]
      {:builder-fn rs/as-unqualified-maps})))

(defn get-subtree-ids
  "Public: vector of topic-id and all its descendant ids. Used by the drag-and-
   drop nesting UI to grey out invalid (cycle-forming) drop targets."
  [topic-id]
  (subtree-topic-ids ds topic-id))

(defn get-flashcards-for-subtree
  "Flashcards for a topic and all its descendants (via parent_id). Same row
   shape as get-all-flashcards (coalesced page_number + card-group-selects) so
   the Anki-sync 'subtree' scope feeds the same downstream as 'self'/'document'.
   Reuses get-subtree-ids for the id set (always includes topic-id itself)."
  [topic-id]
  (if topic-id
    (mapv (comp strip-foreign-jsonb normalize-flashcard-row)
      (jdbc/execute! ds
        (sql/format {:select (into [[:f.*] [[:coalesce :t.page_number :parent.page_number] :page_number]]
                               card-group-selects)
                     :from [[:flashcards :f]]
                     :join [[:topics :t] [:= :f.topic_id :t.id]]
                     :left-join [[:topics :parent] [:= :t.parent_id :parent.id]
                                 [:occlusion_groups :og] [:= :f.occlusion_group_id :og.id]
                                 [:score_groups :sg] [:= :f.score_group_id :sg.id]]
                     :where [:in :f.topic_id (get-subtree-ids topic-id)]
                     :order-by [[:f.created_at :desc]]})))
    []))

(defn topic-scope-context
  "Scope-selector context for a topic: its kind and whether it has any
   non-staged child topic. {:kind <string> :has-children? <bool>}. nil topic-id
   → nil. Drives the Anki-sync scope label (kind) and the 'subtree' leaf gate."
  [topic-id]
  (when topic-id
    (let [row (jdbc/execute-one! ds
                (sql/format {:select [:kind
                                      [[:exists {:select [1]
                                                 :from [[:topics :c]]
                                                 :where [:and [:= :c.parent_id topic-id]
                                                         [:is :c.staged_delete_id nil]]}]
                                       :has_children]]
                             :from [:topics]
                             :where [:= :id topic-id]}))]
      {:kind (:topics/kind row)
       :has-children? (boolean (:has_children row))})))

(defn- subtree-owner
  "user_id of the root document above topic-id (root carries the owner;
   children may have NULL user_id). Returns nil if not found."
  [conn topic-id]
  (:user_id
   (jdbc/execute-one! conn
     ["WITH RECURSIVE anc(id, parent_id, user_id) AS (
         SELECT id, parent_id, user_id FROM topics WHERE id = ?
         UNION ALL
         SELECT t.id, t.parent_id, t.user_id FROM topics t JOIN anc a ON t.id = a.parent_id)
       SELECT user_id FROM anc WHERE parent_id IS NULL" topic-id]
     {:builder-fn rs/as-unqualified-maps})))

(defn owns-topic?
  "True iff `user-id` owns `topic-id` — the root document above it carries the
   owner. Works for root or child topic-ids (walks parent_id to the root).
   Pre: user-id server-authoritative (from the Ring session). Post: boolean.
   The security predicate for topic/group ownership checks outside db.clj."
  [user-id topic-id]
  (boolean (and user-id topic-id (= user-id (subtree-owner ds topic-id)))))

(defn occlusion-group-owner
  "Owner (root user_id) of occlusion `group-id`, resolved via any member
   flashcard's root topic, or nil. occlusion_groups carries no owner column."
  [group-id]
  (:user_id
   (jdbc/execute-one! ds
     ["SELECT root.user_id FROM flashcards f
       JOIN topics root ON f.root_topic_id = root.id
       WHERE f.occlusion_group_id = ? LIMIT 1" group-id]
     {:builder-fn rs/as-unqualified-maps})))

(defn score-group-owner
  "Owner (root user_id) of score `group-id`, resolved via any member flashcard's
   root topic, or nil. score_groups carries no owner column."
  [group-id]
  (:user_id
   (jdbc/execute-one! ds
     ["SELECT root.user_id FROM flashcards f
       JOIN topics root ON f.root_topic_id = root.id
       WHERE f.score_group_id = ? LIMIT 1" group-id]
     {:builder-fn rs/as-unqualified-maps})))

;; ---------------------------------------------------------------------------
;; Dismiss / Undismiss — remove a topic + its whole subtree from the Learning
;; Queue (SuperMemo Dismiss) while keeping it in the collection. Recursive by
;; design; orthogonal to status (Done survives a dismiss round-trip). Compare
;; done-topic!/restore-topic! (single-topic, status-column). One repetition
;; event is logged on the target only, not per descendant.
;; ---------------------------------------------------------------------------

(defn- set-subtree-dismissed!
  "Flip `dismissed` across topic-id + its whole subtree, logging one repetition
   `event-type` on topic-id. Shared body of dismiss-topic!/undismiss-topic!.
   Pre:  caller owns topic-id. Post: every subtree row has dismissed=`value`;
   exactly one `event-type` row on topic-id. Returns {:count n} | nil (not owned)."
  [user-id topic-id value event-type]
  (jdbc/with-transaction [tx ds]
    (when (= user-id (subtree-owner tx topic-id))
      (let [ids    (subtree-topic-ids tx topic-id)
            before (snapshot-topic-row tx topic-id)]
        (insert-repetition! tx topic-id event-type before)
        (jdbc/execute! tx
          (sql/format {:update :topics
                       :set {:dismissed value}
                       :where [:in :id ids]}))
        {:count (count ids)}))))

(defn dismiss-topic!
  "Dismiss topic-id + its whole subtree (dismissed=true). Reversible via
   undismiss-topic!. Returns {:count n} | nil when not owned by user-id."
  [user-id topic-id]
  (set-subtree-dismissed! user-id topic-id true "dismiss"))

(defn undismiss-topic!
  "Reverse a dismiss: clear dismissed across topic-id + its whole subtree.
   Leaves each row's status untouched (a done child stays done).
   Returns {:count n} | nil when not owned by user-id."
  [user-id topic-id]
  (set-subtree-dismissed! user-id topic-id false "undismiss"))

(defn topic-dismissed?
  "True iff topic-id is currently dismissed. Drives the viewer toolbar's
   Dismiss/Undismiss toggle label. False for a missing topic."
  [topic-id]
  (boolean (:dismissed (jdbc/execute-one! ds
                         ["SELECT dismissed FROM topics WHERE id = ?" topic-id]
                         {:builder-fn rs/as-unqualified-maps}))))

(defn- over-quota?
  "True when the user's usage now exceeds an explicit quota (nil quota ⇒ unlimited)."
  [conn user-id]
  (let [{:keys [usage_bytes quota_bytes]}
        (jdbc/execute-one! conn
          ["SELECT usage_bytes, quota_bytes FROM users WHERE id = ?" user-id]
          {:builder-fn rs/as-unqualified-maps})]
    (boolean (and quota_bytes usage_bytes (> usage_bytes quota_bytes)))))

(defn stage-topic-for-deletion!
  "Hide topic-id + its whole subtree (set staged_delete_id) and hard-delete the
   subtree's flashcards (snapshotted into one 'delete-document' undo entry).
   Frees usage_bytes immediately. Pre: caller passes a topic the user owns.
   Returns {:entry-id :card-count :note-ids :parent-id :freed-bytes}, or nil
   when the topic is not owned by user-id (caller bug / stale client)."
  [user-id topic-id]
  (jdbc/with-transaction [tx ds]
    (when (= user-id (subtree-owner tx topic-id))
      (let [ids (subtree-topic-ids tx topic-id)
            parent-id (:parent_id
                       (jdbc/execute-one! tx
                         ["SELECT parent_id FROM topics WHERE id = ?" topic-id]
                         {:builder-fn rs/as-unqualified-maps}))
            deleted (jdbc/execute! tx
                      (sql/format {:delete-from :flashcards
                                   :where [:in :topic_id ids]
                                   :returning [:*]}))
            note-ids (into [] (keep :flashcards/anki_note_id) deleted)
            entry-id (insert-undo-entry-raw! tx user-id "delete-document" "document"
                       [topic-id] {:cards (mapv unqualify-row deleted)})
            freed (subtree-file-bytes tx topic-id)]
        (jdbc/execute! tx
          (sql/format {:update :topics
                       :set {:staged_delete_id entry-id}
                       :where [:in :id ids]}))
        (when (pos? freed) (bump-user-usage! tx user-id (- freed)))
        (prune-undo-log! tx user-id)
        {:entry-id entry-id :card-count (count deleted)
         :note-ids note-ids :parent-id parent-id :freed-bytes freed}))))

(defn restore-staged-document!
  "Reverse a staged deletion: re-insert the cards, clear staged_delete_id on the
   subtree, re-add the freed usage_bytes. `entry` is a parsed undo_log row
   (:id, :entity_refs [topic-id], :snapshot {:cards [...]}).
   Returns {:over-quota? bool :card-count n}."
  [user-id entry]
  (jdbc/with-transaction [tx ds]
    (let [cards (:cards (:snapshot entry))
          topic-id (first (:entity_refs entry))]
      (restore-flashcards-tx! tx cards)
      (jdbc/execute! tx
        (sql/format {:update :topics
                     :set {:staged_delete_id nil}
                     :where [:= :staged_delete_id (:id entry)]}))
      (let [bytes (subtree-file-bytes tx topic-id)]
        (when (pos? bytes) (bump-user-usage! tx user-id bytes))
        {:over-quota? (over-quota? tx user-id) :card-count (count cards)}))))

;; ---------------------------------------------------------------------------
;; Topic re-parenting — custom nesting via drag-and-drop. A move is a single
;; parent_id update; the whole subtree follows (children reference the node,
;; not its ancestors). Cycle-guarded because the recursive CTEs above have no
;; visited check — a cycle would loop them forever, not just corrupt display.
;; ---------------------------------------------------------------------------

;; Kinds whose parent_id is structural, not user-arranged: a page stub's
;; identity is (parent_id = pdf-root, page_number), and get-page-text keys the
;; editor content off that pair — reparenting one orphans its content. These
;; may never be moved by reparent-topic!.
(def ^:private structural-kinds #{"page"})

(defn reparent-topic!
  "Move topic-id under new-parent-id (nil ⇒ promote to root) for user-id.
   Pre: user owns topic-id. Rejected (returns nil) when the move is not owned,
   would create a cycle (new-parent is topic-id or one of its descendants),
   targets another user's topic, new-parent already equals the current parent
   (no-op), or topic-id is a structural kind (page stub). On success writes one
   reversible 'move-topic' undo entry capturing the prior parent and returns
   {:entry-id _ :old-parent-id _ :new-parent-id _}."
  [user-id topic-id new-parent-id]
  (jdbc/with-transaction [tx ds]
    (when (= user-id (subtree-owner tx topic-id))
      (let [{old-parent-id :parent_id kind :kind}
            (jdbc/execute-one! tx
              ["SELECT parent_id, kind FROM topics WHERE id = ?" topic-id]
              {:builder-fn rs/as-unqualified-maps})
            forbidden (set (subtree-topic-ids tx topic-id))]
        (when (and (not (structural-kinds kind))
                (not= old-parent-id new-parent-id)
                (not (contains? forbidden new-parent-id))
                (or (nil? new-parent-id)
                  (= user-id (subtree-owner tx new-parent-id))))
          (let [entry-id (insert-undo-entry-raw! tx user-id "move-topic" "document"
                           [topic-id] {:topic-id topic-id :old-parent-id old-parent-id})]
            (jdbc/execute! tx
              (sql/format {:update :topics
                           :set {:parent_id new-parent-id}
                           :where [:= :id topic-id]}))
            (prune-undo-log! tx user-id)
            {:entry-id entry-id :old-parent-id old-parent-id :new-parent-id new-parent-id}))))))

(defn restore-topic-parent!
  "Undo a move: set topic's parent_id back to the snapshot's :old-parent-id.
   Idempotent — a plain UPDATE, safe to replay. `entry` is a parsed undo_log row."
  [entry]
  (let [{:keys [topic-id old-parent-id]} (:snapshot entry)]
    (when topic-id
      (jdbc/execute! ds
        (sql/format {:update :topics
                     :set {:parent_id old-parent-id}
                     :where [:= :id topic-id]})))))

(defn push-bibliography-to-descendants!
  "Copy topic-id's effective bibliography into every descendant extract as a
   fresh private sources row (overwrite-all). Excludes kind='page' descendants
   and topic-id itself, so each affected extract owns an independent copy.
   Pre:  user-id owns the subtree root above topic-id.
   Post: {:ok true :count n} — n descendants re-pointed to new source copies;
         {:ok false :error :no-source} when topic-id has no effective source;
         {:ok false :error :not-owned} when ownership fails.
   Invariant: one transaction; prior descendant source rows are left in place
   (sources are not garbage-collected here)."
  [user-id topic-id]
  (let [src-id (resolve-effective-source-id topic-id)]
    (if-not src-id
      {:ok false :error :no-source}
      (let [src      (get-source src-id)
            csl      (:sources/csl src)
            csl-type (:sources/csl_type src)
            url      (:sources/url src)
            title    (:sources/title src)]
        (jdbc/with-transaction [tx ds]
          (if-not (= user-id (subtree-owner tx topic-id))
            {:ok false :error :not-owned}
            (let [descendants (jdbc/execute! tx
                                ["WITH RECURSIVE subtree(id, kind) AS (
                                    SELECT id, kind FROM topics WHERE id = ?
                                    UNION ALL
                                    SELECT c.id, c.kind FROM topics c
                                      JOIN subtree s ON c.parent_id = s.id)
                                  SELECT id FROM subtree
                                   WHERE id <> ? AND (kind IS DISTINCT FROM 'page')"
                                 topic-id topic-id]
                                {:builder-fn rs/as-unqualified-maps})]
              (doseq [{:keys [id]} descendants]
                (let [new-src (jdbc/execute-one! tx
                                (sql/format {:insert-into :sources
                                             :values [{:user_id         user-id
                                                       :csl_type        csl-type
                                                       :csl             (->jsonb csl)
                                                       :url             url
                                                       :title           title
                                                       :container_title (when (map? csl)
                                                                          (:container-title csl))}]
                                             :returning [:id]}))]
                  (jdbc/execute! tx
                    ["UPDATE topics SET source_id = ? WHERE id = ?"
                     (:sources/id new-src) id])))
              {:ok true :count (count descendants)})))))))

(defn purge-staged-documents!
  "Hard-delete topic subtrees whose staging entry has aged past the 12h window,
   then drop those entries. CASCADE clears the binary + remaining rows.
   usage_bytes is untouched (already freed at stage time). Returns topics deleted.

   §4.7 7.1 — this is where video large objects are unlinked, NOT at staging
   time: staging is reversible for 12 h, and `restore-staged-document!` must be
   able to hand back a playable video. The OIDs are read before the DELETE (the
   topic_videos rows cascade away with the topics) and unlinked after it, in the
   same transaction."
  []
  (jdbc/with-transaction [tx ds]
    (let [oids (mapv :lo_oid
                 (jdbc/execute! tx
                   ["SELECT tv.lo_oid FROM topic_videos tv
                     JOIN topics t ON tv.topic_id = t.id
                     WHERE t.staged_delete_id IN
                       (SELECT id FROM undo_log
                        WHERE action_type = 'delete-document'
                          AND occurred_at < now() - interval '12 hours')"]
                   {:builder-fn rs/as-unqualified-maps}))
          deleted (jdbc/execute! tx
                    ["DELETE FROM topics
                      WHERE staged_delete_id IN
                        (SELECT id FROM undo_log
                         WHERE action_type = 'delete-document'
                           AND occurred_at < now() - interval '12 hours')"])
          n (or (some-> deleted first ::jdbc/update-count) 0)]
      (doseq [oid oids] (lo/unlink! tx oid))
      (jdbc/execute! tx
        ["DELETE FROM undo_log
          WHERE action_type = 'delete-document'
            AND occurred_at < now() - interval '12 hours'"])
      n)))

(defn purge-orphan-video-objects!
  "§4.7 7.4 — unlink every large object this role owns that no live row
   references: neither a `topic_videos` row nor an in-flight
   `video_upload_sessions` row.

   Catches the leaks no deletion path can: a crashed finalize between
   `lo_create` and the INSERT, a manual SQL delete, a restored-from-backup
   mismatch. Scoped to `lomowner = current_user` so a large object belonging to
   another role in the same database is never touched.

   Pre:  none. Post: returns the number unlinked. Safe to run repeatedly."
  []
  (jdbc/with-transaction [tx ds]
    (let [orphans (mapv :oid
                    (jdbc/execute! tx
                      ["SELECT m.oid FROM pg_largeobject_metadata m
                        WHERE m.lomowner = (SELECT oid FROM pg_roles WHERE rolname = current_user)
                          AND NOT EXISTS (SELECT 1 FROM topic_videos tv WHERE tv.lo_oid = m.oid)
                          AND NOT EXISTS (SELECT 1 FROM video_upload_sessions s WHERE s.lo_oid = m.oid)"]
                      {:builder-fn rs/as-unqualified-maps}))]
      (doseq [oid orphans] (lo/unlink! tx oid))
      (when (pos? (count orphans))
        (tel/log! {:level :warn :id ::orphan-large-objects :data {:count (count orphans)}}
          "Unlinked orphaned video large objects"))
      (count orphans))))

(defn reclaim-unowned-videos!
  "§4.7 7.3 — reclaim videos whose topic has no owner.

   `topics.user_id` is ON DELETE SET NULL, so deleting a user orphans their
   topics instead of cascading them: the topic_videos row survives, the large
   object stays referenced (so 7.4's detector will not see it), and nobody can
   reach or be billed for the bytes. Delete those topics outright, which
   cascades the row, then unlink.

   Prefer `delete-user!` — this is the backstop for a user removed by raw SQL."
  []
  (jdbc/with-transaction [tx ds]
    (let [oids (mapv :lo_oid
                 (jdbc/execute! tx
                   ["SELECT tv.lo_oid FROM topic_videos tv
                     JOIN topics t ON tv.topic_id = t.id
                     WHERE t.user_id IS NULL"]
                   {:builder-fn rs/as-unqualified-maps}))]
      (when (seq oids)
        (jdbc/execute! tx
          ["DELETE FROM topics WHERE user_id IS NULL AND id IN (SELECT topic_id FROM topic_videos)"])
        (doseq [oid oids] (lo/unlink! tx oid))
        (tel/log! {:level :warn :id ::unowned-videos :data {:count (count oids)}}
          "Reclaimed videos belonging to deleted users"))
      (count oids))))

(defn delete-user!
  "§4.7 7.3 — remove a user and everything they own. REPL/SQL admin path.

   Deletes the user's topics FIRST so `topic_videos` cascades while the OIDs are
   still reachable, unlinks them, then deletes the user row. Doing it in the
   other order would trip `topics.user_id ON DELETE SET NULL` and strand the
   bytes. Returns {:topics n :objects n}."
  [user-id]
  (jdbc/with-transaction [tx ds]
    (let [oids (mapv :lo_oid
                 (jdbc/execute! tx
                   ["SELECT tv.lo_oid FROM topic_videos tv
                     JOIN topics t ON tv.topic_id = t.id WHERE t.user_id = ?" user-id]
                   {:builder-fn rs/as-unqualified-maps}))
          deleted (jdbc/execute! tx ["DELETE FROM topics WHERE user_id = ?" user-id])]
      (doseq [oid oids] (lo/unlink! tx oid))
      (jdbc/execute! tx ["DELETE FROM users WHERE id = ?" user-id])
      (log/audit! {:id ::user-deleted :user-id user-id :action :delete
                   :entity :document :entity-id user-id})
      {:topics (or (some-> deleted first ::jdbc/update-count) 0)
       :objects (count oids)})))

(defonce ^:private purge-scheduler (atom nil))

(defonce ^{:doc "Extra jobs the hourly sweep runs after the staged-document purge.
   Registered by freememo.storage-meter and freememo.video-upload at namespace
   load so db.clj keeps no dependency on either. Each entry is [label thunk];
   a throwing thunk is logged and does not stop the others."}
  !sweep-jobs (atom []))

(defn register-sweep-job!
  "Add [label thunk] to the hourly sweep. Idempotent per label — re-registers
   replace, so a namespace reload does not double up the job."
  [label thunk]
  (swap! !sweep-jobs (fn [jobs] (conj (vec (remove #(= label (first %)) jobs)) [label thunk])))
  nil)

(defn- run-sweep!
  "One pass of the hourly sweep: staged purge, then the large-object reclaimers,
   then every registered job. Total — each step's failure is logged and the rest
   still run, because a wedged metering job must not stop deletions."
  []
  (doseq [[label thunk] (into [[::purge-staged-documents purge-staged-documents!]
                               [::reclaim-unowned-videos reclaim-unowned-videos!]
                               [::purge-orphan-video-objects purge-orphan-video-objects!]]
                         @!sweep-jobs)]
    (try (thunk)
      (catch Throwable t (tel/error! {:id label} t)))))

(defn start-purge-scheduler!
  "Start the hourly sweep (staged-document purge + large-object reclamation +
   registered jobs). Idempotent — once per process."
  []
  (when-not @purge-scheduler
    (let [exec (java.util.concurrent.Executors/newSingleThreadScheduledExecutor)]
      (.scheduleAtFixedRate exec run-sweep!
        1 60 java.util.concurrent.TimeUnit/MINUTES)
      (reset! purge-scheduler exec)
      (tel/log! :info "Hourly sweep scheduler started"))))

;; ---------------------------------------------------------------------------
;; Incremental Video (see plans/incremental-video.md)
;;
;; Byte ownership, stated once because three tables have to agree:
;;   users.usage_bytes = SUM(topic_files.file_size) + SUM(topic_videos.byte_size)
;; Bytes are RESERVED at upload init (against the declared total, via
;; quota/check-and-bump!) and only released by a deletion path or by an aborted/
;; reaped session. The large object and the reservation are created and undone
;; together — a path that does one without the other leaks.
;; ---------------------------------------------------------------------------

;; Casting note: Postgres has no implicit bigint → oid coercion, and pgjdbc
;; sends a Clojure long as int8. Every OID bind therefore goes through
;; CAST(CAST(? AS bigint) AS oid).
(def ^:private oid-param "CAST(CAST(? AS bigint) AS oid)")

(defn get-topic-video
  "topic_videos row for a topic, or nil. Unqualified keys."
  [topic-id]
  (jdbc/execute-one! ds
    ["SELECT topic_id, lo_oid, byte_size, mime_type, duration_ms, last_pos_ms
      FROM topic_videos WHERE topic_id = ?" topic-id]
    {:builder-fn rs/as-unqualified-maps}))

(defn get-topic-video-for-user
  "topic_videos row joined to its topic and scoped to `user-id`; nil when the
   topic is missing, staged for deletion, or owned by somebody else.
   The single ownership gate for playback and processing."
  [user-id topic-id]
  (jdbc/execute-one! ds
    ["SELECT tv.topic_id, tv.lo_oid, tv.byte_size, tv.mime_type,
             tv.duration_ms, tv.last_pos_ms, tv.remux_pending, t.title
      FROM topic_videos tv JOIN topics t ON tv.topic_id = t.id
      WHERE tv.topic_id = ? AND t.user_id = ? AND t.staged_delete_id IS NULL"
     topic-id user-id]
    {:builder-fn rs/as-unqualified-maps}))

(defn replace-video-bytes!
  "§12.4 2.1/2.2 — swap a video's stored bytes for `file`, atomically with its
   size, MIME and duration.

   Rewrites the SAME large object rather than allocating a new OID: every sweep
   job, the playback handler and `usage_bytes` all key off `lo_oid`, and
   `lo/write-file!` truncates before writing, so the object ends exactly
   `file`'s length. One transaction, because a size that disagrees with the
   object is a `Content-Length` that lies.

   Not gated by quota, matching `save-video-audio!`: the delta is a container
   rewrite of bytes the quota already accepted (measured +0.59 % on 192 MB).
   `usage_bytes` still moves, so any overage stays visible and is recovered on
   deletion.

   Pre:  topic-id has a topic_videos row; `file` is readable; the caller owns
         the topic (checked upstream — this function does not re-check).
   Post: {:ok true :size n}; remux_pending is FALSE, so the bytes are now
         immutable and cacheable.
   Invariant: byte_size = the object's real length = the delta applied to
              usage_bytes."
  [user-id topic-id ^java.io.File file mime-type duration-ms]
  (jdbc/with-transaction [tx ds]
    (let [row (jdbc/execute-one! tx
                ["SELECT lo_oid, byte_size FROM topic_videos
                  WHERE topic_id = ? FOR UPDATE" topic-id]
                {:builder-fn rs/as-unqualified-maps})]
      (if (nil? row)
        {:ok false :error "Video row not found"}
        (let [prior (long (:byte_size row))
              size (lo/write-file! tx (:lo_oid row) file)]
          (jdbc/execute! tx
            ["UPDATE topic_videos
              SET byte_size = ?, mime_type = ?, duration_ms = COALESCE(?, duration_ms),
                  remux_pending = FALSE
              WHERE topic_id = ?"
             size mime-type (when duration-ms (int duration-ms)) topic-id])
          (bump-user-usage! tx user-id (- size prior))
          {:ok true :size size})))))

(defn clear-video-remux-pending!
  "Mark a video's bytes settled — no remux will replace them.

   Called on every terminal outcome of the pipeline's normalization stage,
   including failure: after a remux that MP4 cannot accept, the uploaded bytes
   ARE the final bytes, and leaving the flag set would suppress caching forever
   for a file that never changes again."
  [topic-id]
  (jdbc/execute! ds
    ["UPDATE topic_videos SET remux_pending = FALSE WHERE topic_id = ?" topic-id]))

(defn set-video-duration!
  "Record the ffprobe-measured duration (§4.4 4.3)."
  [topic-id duration-ms]
  (jdbc/execute! ds
    ["UPDATE topic_videos SET duration_ms = ? WHERE topic_id = ?"
     (when duration-ms (int duration-ms)) topic-id]))

(defn save-video-position!
  "Persist the resume position (§4.8 8.7). Ownership-scoped so a forged topic-id
   writes nothing. Clamped at 0; the caller writes on pause/unmount, not per frame."
  [user-id topic-id pos-ms]
  (jdbc/execute! ds
    ["UPDATE topic_videos SET last_pos_ms = GREATEST(0, ?)
      WHERE topic_id = ? AND topic_id IN (SELECT id FROM topics WHERE user_id = ?)"
     (int (max 0 (or pos-ms 0))) topic-id user-id]))

(defn save-video-audio!
  "Store the ffmpeg-extracted MP3 as the video topic's role='audio' blob,
   replacing any previous one, and keep `usage_bytes` in step.

   Deliberately NOT gated by `quota/check-and-bump!`: the extraction is a
   derived artifact of an upload the quota already accepted, and it is ~2 % of
   the video's size. Refusing it here would leave a stored video that can never
   be played back on the waveform or transcribed — a worse state than being a
   few MB over. The counter is still incremented, so the overage is visible and
   is recovered on the next deletion.

   Post: exactly one role='audio' row for the topic; usage_bytes adjusted by the
   signed difference against whatever it replaced."
  [user-id topic-id ^bytes audio-bytes]
  (let [size (alength audio-bytes)]
    (jdbc/with-transaction [tx ds]
      (let [prior (or (:file_size (jdbc/execute-one! tx
                                    ["SELECT file_size FROM topic_files
                                      WHERE topic_id = ? AND role = 'audio'" topic-id]
                                    {:builder-fn rs/as-unqualified-maps}))
                    0)]
        (jdbc/execute! tx
          ["INSERT INTO topic_files (topic_id, role, file_data, file_size, mime_type)
            VALUES (?, 'audio', ?, ?, 'audio/mpeg')
            ON CONFLICT (topic_id, role)
            DO UPDATE SET file_data = excluded.file_data,
                          file_size = excluded.file_size,
                          mime_type = excluded.mime_type"
           topic-id audio-bytes size])
        (bump-user-usage! tx user-id (- size prior))
        {:ok true :size size}))))

;; ── Transcript (§4.4 / §4.8) ───────────────────────────────────────────────

(def ^:private transcript-insert-chunk
  "Rows per INSERT. 6 columns × 5000 rows stays far under Postgres' 65 535
   bind-parameter ceiling."
  5000)

(defn replace-video-transcript!
  "Replace a video's whole transcript with `segments`, each
   {:ord :start-ms :end-ms :text}. One transaction, so a partial transcript is
   never visible. Re-transcribing is therefore idempotent from the reader's side."
  [topic-id segments]
  (jdbc/with-transaction [tx ds]
    (jdbc/execute! tx ["DELETE FROM video_transcripts WHERE topic_id = ?" topic-id])
    (doseq [batch (partition-all transcript-insert-chunk segments)]
      (jdbc/execute! tx
        (sql/format {:insert-into :video_transcripts
                     :values (mapv (fn [s]
                                     {:topic_id topic-id
                                      :ord (int (:ord s))
                                      :start_ms (int (:start-ms s))
                                      :end_ms (int (:end-ms s))
                                      :text (sanitize-utf8 (:text s))})
                               batch)})))
    (count segments)))

(defn get-video-transcript-count
  "Segment count for a video — the virtual-scroll record count."
  [topic-id]
  (or (:c (jdbc/execute-one! ds
            ["SELECT COUNT(*) AS c FROM video_transcripts WHERE topic_id = ?" topic-id]
            {:builder-fn rs/as-unqualified-maps}))
    0))

(defn get-video-transcript
  "Every transcript segment for a video, in `ord` order.

   Read into a SERVER-FORM binding by the transcript pane, never into an e/defn
   return: the caller reads `(e/server (nth segs i nil))` per visible row, so a
   three-hour transcript stays server-side and only the scroll window crosses
   the wire (CLAUDE.md, 'e/defn Returns Materialize at the Call Site')."
  [topic-id]
  (jdbc/execute! ds
    ["SELECT id, ord, start_ms, end_ms, text FROM video_transcripts
      WHERE topic_id = ? ORDER BY ord" topic-id]
    {:builder-fn rs/as-unqualified-maps}))

(defn copy-video-transcript-to-content!
  "§4.8 8.6 — replace a video topic's `content` with its whole transcript.

   The one bridge between the reference transcript and the editable body. It is
   an explicit user action, not a default, because the design decision was that
   `content` starts empty and the user curates into it: auto-filling would make
   every video's content a wall of unedited speech-to-text that card generation
   then chews through.

   Post: {:ok true :segments n}, or {:ok false :error S} when there is no
   transcript to copy."
  [topic-id]
  (let [segs (get-video-transcript topic-id)]
    (if (empty? segs)
      {:ok false :error "No transcript yet"}
      (let [html (->> segs
                   (map :text)
                   (map str/trim)
                   (remove str/blank?)
                   (map #(str "<p>" (text/escape-html %) "</p>"))
                   (str/join))]
        (update-topic-content! topic-id html)
        {:ok true :segments (count segs)}))))

(defn get-video-transcript-overlapping
  "Segments of `topic-id` overlapping [start-ms, end-ms), in `ord` order.

   Overlap, not containment (§4.9 9.1): a marked range whose edges fall
   mid-segment must include both partial segments, or the extract loses the
   half-sentences at each end. Two segments overlap iff each starts before the
   other ends."
  [topic-id start-ms end-ms]
  (jdbc/execute! ds
    ["SELECT id, ord, start_ms, end_ms, text FROM video_transcripts
      WHERE topic_id = ? AND start_ms < ? AND end_ms > ? ORDER BY ord"
     topic-id (int end-ms) (int start-ms)]
    {:builder-fn rs/as-unqualified-maps}))

(defn get-video-segment
  "The [start_ms, end_ms) range an extract was cut from, or nil for an ordinary
   topic. Provenance only — it never rewrites the extract's content."
  [topic-id]
  (jdbc/execute-one! ds
    ["SELECT start_ms, end_ms FROM video_segments WHERE topic_id = ? ORDER BY id LIMIT 1"
     topic-id]
    {:builder-fn rs/as-unqualified-maps}))

(defn create-video-extract!
  "§4.9 — create an extract child of a video topic over [start-ms, end-ms).

   The extract is an ORDINARY topic: `content` is the overlapping transcript
   text copied in at creation, editable thereafter, and the `video_segments` row
   records where it came from. Nothing re-derives it, so the extract survives
   the video's bytes being reclaimed.

   Pre:  user-id owns video-topic-id; end-ms > start-ms >= 0.
   Post: {:success true :id N}, or {:success false :error S}.
   Throws: ex-info ::bad-range before touching the DB on a malformed range
           (mirrors score/validate-segment).

   Not atomic across the topic and its range row: `create-topic!` owns its own
   connection (audit trail, title validation), so a failure between the two
   leaves an extract with its text but no recorded range. Tolerable precisely
   because the range is provenance and nothing reads through it — the extract
   is complete and reviewable without it."
  [user-id video-topic-id start-ms end-ms title]
  (when-not (and (int? start-ms) (int? end-ms) (>= start-ms 0) (> end-ms start-ms))
    (throw (ex-info "Malformed video segment"
             {:type ::bad-range :start-ms start-ms :end-ms end-ms})))
  (if-not (get-topic-video-for-user user-id video-topic-id)
    {:success false :error "Video not found"}
    (let [segs (get-video-transcript-overlapping video-topic-id start-ms end-ms)
          content (if (seq segs)
                    (->> segs
                      (map :text)
                      (map str/trim)
                      (remove str/blank?)
                      (map #(str "<p>" (text/escape-html %) "</p>"))
                      (str/join))
                    "")
          inherited (clone-source! user-id (resolve-effective-source-id video-topic-id))
          created (create-topic! {:parent-id video-topic-id
                                  :user-id user-id
                                  :content content
                                  :kind "basic"
                                  :title title
                                  :source-id inherited})
          extract-id (:topics/id created)]
      (jdbc/execute! ds
        ["INSERT INTO video_segments (topic_id, start_ms, end_ms) VALUES (?, ?, ?)"
         extract-id (int start-ms) (int end-ms)])
      {:success true :id extract-id})))

;; ── Chunked upload sessions (§4.3) ─────────────────────────────────────────

(defn init-video-upload!
  "§4.3 3.1/3.1.1/3.1.2 — open an upload session.

   One transaction does three things that must not come apart: reserve
   `total-bytes` against the user's quota, create the large object, and record
   the session. A quota rejection therefore leaves no object behind, and a
   successful init has already paid for every byte the client may send.

   Gated by `quota/reserve-bytes!`, NOT `check-and-bump!`: the latter also
   enforces `upload_max_bytes`, which is the single-HTTP-request ceiling
   (100 MB by default) that chunking exists to get past. Applying it here
   rejected every video over 100 MB before a byte was sent. The per-upload
   bound for video is `video-http/max-video-bytes`, checked by the caller.

   Pre:  total-bytes > 0 and within the caller's per-upload ceiling; user-id
         exists; parent-id nil or a topic they own.
   Post: {:ok true :session-id S :lo-oid N}.
   Throws: quota/quota-error :over-quota — the tx aborts, unwinding both the
           reservation and the object."
  [user-id filename mime-type total-bytes parent-id]
  (jdbc/with-transaction [tx ds]
    (quota/reserve-bytes! tx user-id total-bytes)
    (let [oid (lo/create! tx)
          session-id (str (random-uuid))]
      (jdbc/execute! tx
        [(str "INSERT INTO video_upload_sessions
                 (id, user_id, lo_oid, total_bytes, filename, mime_type, parent_id)
               VALUES (?, ?, " oid-param ", ?, ?, ?, ?)")
         session-id user-id oid (long total-bytes) filename mime-type parent-id])
      {:ok true :session-id session-id :lo-oid oid})))

(defn get-video-upload-session
  "Session row scoped to its owner, or nil. Unqualified keys."
  [user-id session-id]
  (jdbc/execute-one! ds
    ["SELECT id, user_id, lo_oid, total_bytes, received_bytes, filename, mime_type, parent_id
      FROM video_upload_sessions WHERE id = ? AND user_id = ?" session-id user-id]
    {:builder-fn rs/as-unqualified-maps}))

(defn append-video-chunk!
  "§4.3 3.1.2/3.1.3 — append one chunk to a session's large object.

   Locks the session row FOR UPDATE, which serializes concurrent chunk POSTs
   for the same session: `lo/append!` writes at the object's current end, so
   two unserialized appenders would interleave bytes.

   Pre:  session exists and belongs to user-id.
   Post: {:ok true :received N :total N} with exactly one chunk in heap, or
         {:ok false :error S :code C} — :code \"overflow\" when the chunk would
         push past the declared total, \"not-found\" when the session is gone.
   Invariant: received_bytes always equals the large object's real size."
  [user-id session-id ^bytes buf n]
  (jdbc/with-transaction [tx ds]
    (let [row (jdbc/execute-one! tx
                ["SELECT lo_oid, total_bytes, received_bytes FROM video_upload_sessions
                  WHERE id = ? AND user_id = ? FOR UPDATE" session-id user-id]
                {:builder-fn rs/as-unqualified-maps})]
      (cond
        (nil? row)
        {:ok false :error "Upload session not found" :code "not-found"}

        (> (+ (:received_bytes row) (long n)) (:total_bytes row))
        {:ok false
         :error "Chunk exceeds the declared upload size"
         :code "overflow"}

        :else
        (let [new-size (lo/append! tx (:lo_oid row) buf n)]
          (jdbc/execute! tx
            ["UPDATE video_upload_sessions SET received_bytes = ?, updated_at = now()
              WHERE id = ?" new-size session-id])
          {:ok true :received new-size :total (:total_bytes row)})))))

(defn abort-video-upload!
  "§4.3 3.1.4 — compensate a failed or cancelled upload: unlink the object,
   refund the reservation, drop the session. Idempotent; unknown ids are no-ops.

   Refunds `total_bytes`, not `received_bytes`, because init reserved the
   declared total."
  [user-id session-id]
  (jdbc/with-transaction [tx ds]
    (when-let [row (jdbc/execute-one! tx
                     ["SELECT lo_oid, total_bytes FROM video_upload_sessions
                       WHERE id = ? AND user_id = ? FOR UPDATE" session-id user-id]
                     {:builder-fn rs/as-unqualified-maps})]
      (lo/unlink! tx (:lo_oid row))
      (jdbc/execute! tx ["DELETE FROM video_upload_sessions WHERE id = ?" session-id])
      (bump-user-usage! tx user-id (- (:total_bytes row)))
      {:ok true :refunded (:total_bytes row)})))

(defn finalize-video-upload!
  "§4.3 3.1 — turn a completed session into a `video` topic.

   Transfers ownership of the large object from the session row to a new
   `topic_videos` row; the session row is deleted, NOT compensated, because the
   bytes it reserved are now the topic's. When fewer bytes arrived than were
   declared, the difference is refunded here — the object's real size is what
   the topic owns.

   Pre:  the session's received_bytes equals its total_bytes.
   Post: {:ok true :topic-id N}, or {:ok false :error S} when incomplete.
   Invariant: exactly one row references the OID at every point."
  [user-id session-id]
  (jdbc/with-transaction [tx ds]
    (let [row (jdbc/execute-one! tx
                ["SELECT lo_oid, total_bytes, received_bytes, filename, mime_type, parent_id
                  FROM video_upload_sessions WHERE id = ? AND user_id = ? FOR UPDATE"
                 session-id user-id]
                {:builder-fn rs/as-unqualified-maps})]
      (cond
        (nil? row)
        {:ok false :error "Upload session not found"}

        (not= (:received_bytes row) (:total_bytes row))
        {:ok false :error (str "Upload incomplete: " (:received_bytes row)
                            " of " (:total_bytes row) " bytes")}

        :else
        (let [clean-name (input/prettify-title (input/sanitize-filename (:filename row)))
              _ (input/check-length! :title clean-name input/title-max)
              csl {:type "motion_picture" :title clean-name
                   :accessed {:date-parts (now-date-parts)}}
              source (jdbc/execute-one! tx
                       (sql/format {:insert-into :sources
                                    :values [{:user_id user-id
                                              :csl_type "motion_picture"
                                              :csl (->jsonb csl)
                                              :title clean-name}]
                                    :returning [:id]}))
              topic (jdbc/execute-one! tx
                      (sql/format {:insert-into :topics
                                   :values [{:user_id user-id
                                             :parent_id (:parent_id row)
                                             :kind "video"
                                             :title clean-name
                                             ;; "" not NULL: the editor mounts on
                                             ;; an empty body. Content starts
                                             ;; empty by design — the transcript
                                             ;; is a reference pane the user
                                             ;; curates from, not the content.
                                             :content ""
                                             :source_id (:sources/id source)}]
                                   :returning [:id]}))
              topic-id (:topics/id topic)]
          (jdbc/execute! tx
            [(str "INSERT INTO topic_videos (topic_id, lo_oid, byte_size, mime_type, remux_pending)
                   VALUES (?, " oid-param ", ?, ?, ?)")
             topic-id (:lo_oid row) (:received_bytes row) (:mime_type row)
             ;; §12.4 3.1 — anything that is not already an MP4 is a remux
             ;; candidate, so its bytes are not yet immutable and must not be
             ;; cached. Derived from the declared MIME because finalize has not
             ;; probed the file; the pipeline's probe is authoritative and
             ;; clears this either way.
             (not= "video/mp4" (:mime_type row))])
          (jdbc/execute! tx ["DELETE FROM video_upload_sessions WHERE id = ?" session-id])
          (audit-doc-created! user-id topic-id)
          {:ok true :topic-id topic-id})))))

(defn list-video-playlists
  "§14.3 3.1 — the user's playlists, newest first, for the import modal's picker.

   Deliberately NOT `get-root-topics`, which would answer the same question: it
   also selects `content` and a summed file size per root, and a title picker
   reads neither. Two columns is the whole contract.

   Staged-deleted playlists are excluded — offering one as an upload target
   would hang new videos under a subtree already on its way out.

   Post: [{:id N :title S}] ordered newest first; [] when the user has none."
  [user-id]
  (mapv (fn [r] {:id (:id r) :title (or (:title r) "Untitled")})
    (jdbc/execute! ds
      ["SELECT id, title FROM topics
        WHERE user_id = ? AND kind = 'video-playlist' AND staged_delete_id IS NULL
        ORDER BY created_at DESC, id DESC" user-id]
      {:builder-fn rs/as-unqualified-maps})))

(defn create-video-playlist!
  "§4.10 10.1 — the parent a multi-file upload hangs its videos under.
   An ordinary topic of kind `video-playlist` holding no bytes of its own."
  [user-id title]
  (let [clean-name (input/prettify-title (input/sanitize-filename title))]
    (input/check-length! :title clean-name input/title-max)
    (jdbc/with-transaction [tx ds]
      (let [topic (jdbc/execute-one! tx
                    (sql/format {:insert-into :topics
                                 :values [{:user_id user-id
                                           :kind "video-playlist"
                                           :title clean-name
                                           :content ""}]
                                 :returning [:id]}))]
        (audit-doc-created! user-id (:topics/id topic))
        (:topics/id topic)))))

(defn reap-stale-video-uploads!
  "§4.3 3.3 — abort sessions untouched for longer than `ttl-hours`.

   A browser tab closed mid-upload leaves both a large object and a byte
   reservation; nothing else reclaims either, so this runs in the hourly sweep.
   Returns the number reaped."
  [ttl-hours]
  (jdbc/with-transaction [tx ds]
    (let [stale (jdbc/execute! tx
                  [(str "SELECT id, user_id, lo_oid, total_bytes FROM video_upload_sessions
                         WHERE updated_at < now() - make_interval(hours => ?) FOR UPDATE")
                   (int ttl-hours)]
                  {:builder-fn rs/as-unqualified-maps})]
      (doseq [{:keys [id user_id lo_oid total_bytes]} stale]
        (lo/unlink! tx lo_oid)
        (jdbc/execute! tx ["DELETE FROM video_upload_sessions WHERE id = ?" id])
        (bump-user-usage! tx user_id (- total_bytes)))
      (when (seq stale)
        (tel/log! {:level :info :id ::reaped-video-uploads :data {:count (count stale)}}
          "Reaped abandoned video upload sessions"))
      (count stale))))

;; ── Storage metering (§4.6) ────────────────────────────────────────────────

(defn get-storage-meter-state
  "Everything one accrual pass needs about a user, read in one row:
   `usage_bytes`, `credit_balance_iqd`, `last_metered_at`,
   `storage_grace_started_at`, and whether they hold any video bytes at all."
  ([user-id] (get-storage-meter-state ds user-id))
  ([connectable user-id]
   (jdbc/execute-one! connectable
     ["SELECT u.usage_bytes, u.credit_balance_iqd, u.last_metered_at,
              u.storage_grace_started_at,
              EXISTS (SELECT 1 FROM topic_videos tv JOIN topics t ON tv.topic_id = t.id
                      WHERE t.user_id = u.id) AS has_video
       FROM users u WHERE u.id = ?" user-id]
     {:builder-fn rs/as-unqualified-maps})))

(defn set-storage-grace!
  "Open or close the storage grace window. `nil` clears it (a top-up); a
   timestamp opens it. Writes only on a real transition, so `storage-grace-days`
   counts from the FIRST time the balance hit zero, not from the latest sweep."
  [user-id ts]
  (if ts
    (jdbc/execute! ds
      ["UPDATE users SET storage_grace_started_at = ?
        WHERE id = ? AND storage_grace_started_at IS NULL" ts user-id])
    (jdbc/execute! ds
      ["UPDATE users SET storage_grace_started_at = NULL
        WHERE id = ? AND storage_grace_started_at IS NOT NULL" user-id])))

(defn storage-grace-started-at
  "When the user's storage grace window opened, or nil when not in grace.
   Read on the playback path (§4.6 6.3)."
  [user-id]
  (:storage_grace_started_at
   (jdbc/execute-one! ds
     ["SELECT storage_grace_started_at FROM users WHERE id = ?" user-id]
     {:builder-fn rs/as-unqualified-maps})))

(def ^:const micro-per-iqd
  "Denominator of the storage-rent carry. Rent accrues in millionths of an IQD
   and is debited in whole IQD once a millionth-sum crosses the unit."
  1000000)

(defn accrue-storage-charge!
  "§4.6 6.1 — accrue one user's storage rent and advance their meter, atomically.

   `micro-fn` receives {:usage-bytes :elapsed-ms} and returns the rent for that
   interval in MICRO-IQD. The carry (`users.storage_debt_micro`) is what makes
   the meter exact at any tick rate: an hourly pass over a small library prices
   well below 1 IQD, so without a carry every pass would round to zero and the
   meter would collect nothing. Whole IQD are debited as the carry crosses each
   unit; the remainder stays.

   Idempotent, which is the property the whole design hangs on: locking the row
   and re-reading `last_metered_at` inside the transaction means a second call
   an instant later sees elapsed ≈ 0, accrues ≈ 0, and debits nothing — and two
   concurrent sweeps cannot bill the same interval twice.

   `last_metered_at` always advances, so an interval is consumed exactly once
   whether or not it produced a whole-IQD charge.

   Pre:  micro-fn is pure and returns a non-negative long.
   Post: {:charged n :balance-after n :elapsed-ms n :carry-micro n}, or nil when
         the user is gone."
  [user-id micro-fn]
  (jdbc/with-transaction [tx ds]
    (when-let [row (jdbc/execute-one! tx
                     ["SELECT usage_bytes, credit_balance_iqd, last_metered_at, storage_debt_micro
                       FROM users WHERE id = ? FOR UPDATE" user-id]
                     {:builder-fn rs/as-unqualified-maps})]
      (let [now (java.time.Instant/now)
            last (some-> ^java.sql.Timestamp (:last_metered_at row) .toInstant)
            elapsed-ms (if last
                         (max 0 (- (.toEpochMilli now) (.toEpochMilli last)))
                         0)
            accrued (long (max 0 (micro-fn {:usage-bytes (or (:usage_bytes row) 0)
                                            :elapsed-ms elapsed-ms})))
            debt (+ (or (:storage_debt_micro row) 0) accrued)
            cost (quot debt micro-per-iqd)
            carry (rem debt micro-per-iqd)
            bal (or (:credit_balance_iqd row) 0)
            new-bal (- bal cost)]
        (jdbc/execute! tx
          ["UPDATE users SET last_metered_at = now(), storage_debt_micro = ? WHERE id = ?"
           carry user-id])
        (when (pos? cost)
          (jdbc/execute! tx
            ["UPDATE users SET credit_balance_iqd = ? WHERE id = ?" new-bal user-id])
          (jdbc/execute! tx
            (sql/format {:insert-into :credit_transactions
                         :values [{:user_id user-id
                                   :kind "debit"
                                   :amount_iqd (- cost)
                                   :balance_after new-bal
                                   :endpoint "storage"
                                   :model nil}]})))
        {:charged cost
         :balance-after (if (pos? cost) new-bal bal)
         :elapsed-ms elapsed-ms
         :carry-micro carry}))))

(defn users-due-for-metering
  "Ids of users whose meter is older than `stale-hours` AND who hold stored
   bytes. §4.6 6.4 / decision T1: a dormant account never triggers lazy accrual,
   so the sweep has to accrue for it or its bytes are stored free forever."
  [stale-hours]
  (mapv :id
    (jdbc/execute! ds
      ["SELECT id FROM users
        WHERE usage_bytes > 0
          AND (last_metered_at IS NULL
               OR last_metered_at < now() - make_interval(hours => ?))"
       (int stale-hours)]
      {:builder-fn rs/as-unqualified-maps})))

(defn users-with-expired-grace
  "Ids of users whose grace window opened more than `grace-days` ago and who
   still hold video bytes — the reclamation worklist (§4.6 6.4)."
  [grace-days]
  (mapv :id
    (jdbc/execute! ds
      ["SELECT DISTINCT u.id FROM users u
        JOIN topics t ON t.user_id = u.id
        JOIN topic_videos tv ON tv.topic_id = t.id
        WHERE u.storage_grace_started_at IS NOT NULL
          AND u.storage_grace_started_at < now() - make_interval(days => ?)"
       (int grace-days)]
      {:builder-fn rs/as-unqualified-maps})))

(defn reclaim-user-videos!
  "§4.6 6.5 / decision H3 — drop a user's video BYTES, keep everything else.

   Unlinks each large object and deletes the `topic_videos` rows, decrementing
   `usage_bytes` by exactly what was freed. The topic row, its transcript, its
   extracts, its cards and its schedule all survive: an extract's text was
   copied in at creation, so it reviews and generates cards unchanged — only
   playback is gone. This is the whole reason derivation is initial-only.

   Returns {:videos n :bytes n}."
  [user-id]
  (jdbc/with-transaction [tx ds]
    (let [rows (jdbc/execute! tx
                 ["SELECT tv.topic_id, tv.lo_oid, tv.byte_size FROM topic_videos tv
                   JOIN topics t ON tv.topic_id = t.id WHERE t.user_id = ? FOR UPDATE"
                  user-id]
                 {:builder-fn rs/as-unqualified-maps})
          freed (reduce + 0 (map :byte_size rows))]
      (doseq [{:keys [topic_id lo_oid]} rows]
        (lo/unlink! tx lo_oid)
        (jdbc/execute! tx ["DELETE FROM topic_videos WHERE topic_id = ?" topic_id]))
      (when (pos? freed) (bump-user-usage! tx user-id (- freed)))
      (when (seq rows)
        (tel/log! {:level :warn :id ::storage-reclaimed
                   :data {:user-id user-id :videos (count rows) :bytes freed}}
          "Reclaimed video bytes after storage grace expired"))
      {:videos (count rows) :bytes freed})))

(defn promote-to-paid-storage-tier!
  "§4.2 2.4 — raise a user's storage cap to the paid tier on their first
   completed credit order.

   `WHERE quota_bytes IS NULL` makes this both idempotent (a second purchase is
   a no-op) and safe against the admin override path — an operator-set
   `quota_bytes` is never overwritten by a purchase.
   Pre: called inside the order-completion transaction. Post: rows updated (0 or 1)."
  [connectable user-id paid-bytes]
  (when paid-bytes
    (jdbc/execute! connectable
      ["UPDATE users SET quota_bytes = ? WHERE id = ? AND quota_bytes IS NULL"
       (long paid-bytes) user-id])))

;; ---------------------------------------------------------------------------
;; Knowledge graph operations (see plans/knowledge-graph-quizzes.md)
;; ---------------------------------------------------------------------------

(defn- kg-aliases->vec
  "Normalize the aliases TEXT[] column (PgArray) to a Clojure vector — rows
   feed pr-str'd LLM prompts and Electric wires, neither of which can take a
   raw java.sql.Array."
  [rows]
  (mapv #(update % :aliases (fn [a] (some-> ^java.sql.Array a .getArray vec))) rows))

;; entity-batch-chunk-size caps rows/labels per statement so a large repo
;; distill's distinct-entity count never approaches Postgres' 65 535
;; bind-parameter limit — mirrors insert-kg-facts!'s chunking, one column
;; short of it since these rows are narrower.
(def ^:private entity-batch-chunk-size 20000)

(defn find-entity-link-candidates-batch
  "Batched find-entity-link-candidates: one query for every label in `labels`
   instead of a round trip per label (a VALUES list cross-joined against the
   user's kg_entities, ranked per label with a window function — same total
   work as the per-label loop, fewer round trips).
   Pre:  labels non-empty; entries need not be distinct.
   Post: {label [candidate...]}, same row shape as the single-label version
   (:id :label :aliases :sim, sim DESC, sim ≥ 0.25, ≤ `limit` per label). A
   label with zero qualifying candidates is absent from the map."
  [user-id labels limit]
  (when (seq labels)
    (let [distinct-labels (distinct labels)
          placeholders (str/join ", " (repeat (count distinct-labels) "(?)"))
          rows (jdbc/execute! ds
                 (vec (concat
                        [(str
                           "WITH query_labels(q_label) AS (VALUES " placeholders "),
                            scored AS (
                              SELECT q.q_label, e.id, e.label, e.aliases,
                                     GREATEST(
                                       similarity(e.label, q.q_label),
                                       COALESCE((SELECT MAX(similarity(a, q.q_label)) FROM unnest(e.aliases) a), 0)
                                     ) AS sim
                              FROM query_labels q
                              CROSS JOIN kg_entities e
                              WHERE e.user_id = ?
                            ),
                            ranked AS (
                              SELECT *, ROW_NUMBER() OVER (PARTITION BY q_label ORDER BY sim DESC) AS rn
                              FROM scored
                              WHERE sim >= 0.25
                            )
                            SELECT q_label, id, label, aliases, sim
                            FROM ranked
                            WHERE rn <= ?
                            ORDER BY q_label, sim DESC")]
                        distinct-labels
                        [user-id limit]))
                 {:builder-fn rs/as-unqualified-maps})]
      (->> rows
        kg-aliases->vec
        (reduce (fn [m {:keys [q_label] :as row}]
                  (update m q_label (fnil conj []) (dissoc row :q_label)))
          {})))))

(defn insert-kg-entities!
  "Batch-insert entities for `labels`. No ON CONFLICT — kg_entities has no
   UNIQUE(user_id,label) constraint (see get-or-create-kg-entities!); a race,
   or a caller that (like kg-extract's LLM-linker) inserts without an
   existence check, can produce a duplicate label — acceptable, matching the
   prior single-row insert-kg-entity!'s semantics.
   Pre:  labels non-empty; distinct within any one chunk (both callers dedupe
         before calling — a duplicate label in the same chunk would share one
         id in the result, since re-association below is by label).
   Post: vector of ids, positionally matching `labels`. RETURNING also pulls
   back `label` and re-associates each id to its input label by that value —
   NOT by trusting Postgres' VALUES/RETURNING row order, which SQL does not
   guarantee (reliable in practice for a single INSERT, but not a contract)."
  [user-id labels]
  (when (seq labels)
    (into []
      (mapcat (fn [chunk]
                (let [returned (jdbc/execute! ds
                                  (sql/format {:insert-into :kg_entities
                                               :values (mapv #(hash-map :user_id user-id :label (sanitize-utf8 %)) chunk)
                                               :returning [:id :label]})
                                  {:builder-fn rs/as-unqualified-maps})
                      id-by-label (into {} (map (juxt :label :id)) returned)]
                  (mapv (comp id-by-label sanitize-utf8) chunk))))
      (partition-all entity-batch-chunk-size labels))))

(defn get-or-create-kg-entities!
  "Batched exact get-or-create of entities by (user-id, label) — the
   DETERMINISTIC linker for code facts, whose labels are canonical
   fully-qualified names, so the prose path's fuzzy trigram/LLM merge
   (find-entity-link-candidates-batch) is neither needed nor wanted.
   One SELECT ... WHERE label IN (...) plus one multi-row INSERT for the
   labels not found, instead of a round trip per label. Same
   select-then-insert semantics as the old single-label get-or-create-kg-entity!
   (no UNIQUE(user_id,label) — a race could still insert a duplicate;
   acceptable, single-user ingest is low-concurrency).
   Pre:  labels non-empty; entries need not be distinct.
   Post: {label id} covering every distinct label in `labels`."
  [user-id labels]
  (when (seq labels)
    (let [clean (into {} (map (juxt identity sanitize-utf8)) (distinct labels))
          clean-vals (distinct (vals clean))
          existing (into []
                     (mapcat (fn [chunk]
                               (jdbc/execute! ds
                                 (sql/format {:select [:label :id]
                                              :from [:kg_entities]
                                              :where [:and [:= :user_id user-id] [:in :label chunk]]
                                              :order-by [:id]})
                                 {:builder-fn rs/as-unqualified-maps})))
                     (partition-all entity-batch-chunk-size clean-vals))
          found (reduce (fn [m {:keys [label id]}] (if (contains? m label) m (assoc m label id)))
                  {} existing)
          missing (vec (remove found clean-vals))
          id-by-clean (merge found (zipmap missing (insert-kg-entities! user-id missing)))]
      (into {} (map (fn [[raw c]] [raw (id-by-clean c)])) clean))))

(defn get-or-create-kg-predicate!
  "Upsert a predicate by (user-id, slug); new rows land status='approved' —
   the graph curates by exception (edit/reject in the facts browser), not by
   pre-approval (decision revised 2026-07-04, see plan doc).
   The no-op DO UPDATE makes RETURNING yield the id on conflict too.
   Pre:  slug matches the table's CHECK (callers slugify first — violation
         throws, blame: caller).
   Post: {:id int :status str} of the existing-or-new row."
  [user-id slug label]
  (jdbc/execute-one! ds
    ["INSERT INTO kg_predicates (user_id, slug, label, status) VALUES (?, ?, ?, 'approved')
      ON CONFLICT (user_id, slug) DO UPDATE SET slug = EXCLUDED.slug
      RETURNING id, status"
     user-id slug (sanitize-utf8 label)]
    {:builder-fn rs/as-unqualified-maps}))

(defn insert-kg-facts!
  "Batch insert facts. Rows: {:user_id :subject_entity_id :predicate_id
   :object_entity_id|:object_literal :graph_topic_id :page_number
   :source_model :status}. ON CONFLICT DO NOTHING — the partial unique
   indexes drop same-document s/p/o duplicates (including rows already
   reviewed: a re-distill never resurrects a rejected fact).
   Chunked to stay under Postgres' 65 535 bind-parameter cap per statement:
   one multi-row INSERT emits cols×rows params, and a repo distill can produce
   tens of thousands of facts (the prose path never approached the cap). Chunks
   are independent statements, not one transaction — acceptable because ON
   CONFLICT DO NOTHING + a re-distill make the insert idempotent, so a partial
   run self-heals on retry.
   Post: vector of inserted ids (shorter than rows when duplicates skipped)."
  [rows]
  (when (seq rows)
    (let [rows (map #(update % :object_literal sanitize-utf8) rows)
          cols (count (keys (first rows)))
          ;; 60 000 leaves margin below the 65 535 driver cap.
          per-chunk (max 1 (quot 60000 (max 1 cols)))]
      (into []
        (mapcat (fn [chunk]
                  (mapv :id
                    (jdbc/execute! ds
                      (sql/format {:insert-into :kg_facts
                                   :values chunk
                                   :on-conflict []
                                   :do-nothing true
                                   :returning [:id]})
                      {:builder-fn rs/as-unqualified-maps}))))
        (partition-all per-chunk rows)))))

(defn get-kg-facts
  "Facts for a root document, display-joined (subject/object/predicate labels).
   status nil = all; non-blank `query` filters case-insensitively across
   subject/predicate/object labels and the literal. Ordered by page, then id."
  [user-id graph-topic-id status query]
  (let [q (when-not (or (nil? query) (str/blank? query))
            (str "%" (str/trim query) "%"))]
    (jdbc/execute! ds
      (sql/format {:select [[:f.id :id] [:f.status :status]
                            [:f.page_number :page_number]
                            [:f.object_literal :object_literal]
                            [:f.subject_entity_id :subject_entity_id]
                            [:f.object_entity_id :object_entity_id]
                            [:f.predicate_id :predicate_id]
                            [:s.label :subject_label]
                            [:o.label :object_label]
                            [:p.label :predicate_label] [:p.slug :predicate_slug]]
                   :from [[:kg_facts :f]]
                   :join [[:kg_entities :s] [:= :s.id :f.subject_entity_id]
                          [:kg_predicates :p] [:= :p.id :f.predicate_id]]
                   :left-join [[:kg_entities :o] [:= :o.id :f.object_entity_id]]
                   :where [:and [:= :f.user_id user-id]
                           [:= :f.graph_topic_id graph-topic-id]
                           (when status [:= :f.status status])
                           (when q
                             [:or [:ilike :s.label q] [:ilike :p.label q]
                              [:ilike :o.label q] [:ilike :f.object_literal q]])]
                   :order-by [[:f.page_number :asc] [:f.id :asc]]})
      {:builder-fn rs/as-unqualified-maps})))

(defn kg-fact-ids-for-doc
  "Just the ids of a document's approved facts — the target set for \"select every
   fact in this document\", which by design ignores the view's search filter.
   Ids only: the display-joined rows get-kg-facts returns are far more than a
   selection needs, and the caller already holds rows for what it can see.
   Post: vector of ids, page order then id, matching get-kg-facts."
  [user-id graph-topic-id]
  (mapv :id
    (jdbc/execute! ds
      ["SELECT id FROM kg_facts
        WHERE user_id = ? AND graph_topic_id = ? AND status = 'approved'
        ORDER BY page_number NULLS LAST, id"
       user-id graph-topic-id]
      {:builder-fn rs/as-unqualified-maps})))

(defn get-kg-facts-context
  "Approved facts for assistant grounding — same display-join as get-kg-facts,
   with an optional inclusive page range and an optional row limit so a large
   document or code repo cannot return unbounded rows.
   Pre: graph-topic-id owned by user-id. Post: rows shaped like get-kg-facts,
   all status='approved'; page_number ∈ [start-page,end-page] when both given
   (NULL-page rows are excluded by the range); ≤ limit rows when limit given."
  [user-id graph-topic-id {:keys [start-page end-page limit]}]
  (jdbc/execute! ds
    (sql/format
      (cond-> {:select [[:f.id :id] [:f.status :status]
                        [:f.page_number :page_number]
                        [:f.object_literal :object_literal]
                        [:f.subject_entity_id :subject_entity_id]
                        [:f.object_entity_id :object_entity_id]
                        [:f.predicate_id :predicate_id]
                        [:s.label :subject_label]
                        [:o.label :object_label]
                        [:p.label :predicate_label] [:p.slug :predicate_slug]]
               :from [[:kg_facts :f]]
               :join [[:kg_entities :s] [:= :s.id :f.subject_entity_id]
                      [:kg_predicates :p] [:= :p.id :f.predicate_id]]
               :left-join [[:kg_entities :o] [:= :o.id :f.object_entity_id]]
               :where [:and [:= :f.user_id user-id]
                       [:= :f.graph_topic_id graph-topic-id]
                       [:= :f.status "approved"]
                       (when (and start-page end-page)
                         [:between :f.page_number start-page end-page])]
               :order-by [[:f.page_number :asc] [:f.id :asc]]}
        limit (assoc :limit limit)))
      {:builder-fn rs/as-unqualified-maps}))

(defn get-fact-mastery
  "Per-fact FSRS mastery signal, aggregated over the approved kg_questions linked
   to each fact via kg_question_facts. Shared by the assistant (weak-fact steering)
   and the facts→cards bridge (weak-first ordering).
   Pre:  fact-ids is a seq of kg_facts ids owned by user-id.
   Post: {fact-id {:tested? bool :due? bool :lapses int}} for every input id with
         ≥1 linked approved question. Ids with no linked approved question are
         OMITTED — the caller treats an absent id as untested (never quizzed).
         :tested? = some linked question reviewed ≥once; :due? = some introduced
         linked question is due now; :lapses = max lapses across linked questions."
  [user-id fact-ids]
  (if (empty? fact-ids)
    {}
    (into {}
      (map (juxt :fact_id
             (fn [r] {:tested? (boolean (:tested r))
                      :due? (boolean (:due r))
                      :lapses (or (:lapses r) 0)})))
      (jdbc/execute! ds
        (sql/format
          {:select [[:qf.fact_id :fact_id]
                    [[:raw "BOOL_OR(q.fsrs_reps > 0)"] :tested]
                    [[:raw "BOOL_OR(q.fsrs_due IS NOT NULL AND q.fsrs_due <= now())"] :due]
                    [[:raw "COALESCE(MAX(q.fsrs_lapses), 0)"] :lapses]]
           :from [[:kg_question_facts :qf]]
           :join [[:kg_questions :q] [:= :q.id :qf.question_id]]
           :where [:and [:= :q.user_id user-id]
                   [:= :q.status "approved"]
                   [:in :qf.fact_id (vec fact-ids)]]
           :group-by [:qf.fact_id]})
        {:builder-fn rs/as-unqualified-maps}))))

(defn kg-fact-stats
  "Per-document fact counts by status — feeds the Knowledge page badges.
   Post: {graph-topic-id {status-string n}}."
  [user-id]
  (reduce (fn [acc {:keys [graph_topic_id status n]}]
            (assoc-in acc [graph_topic_id status] n))
    {}
    (jdbc/execute! ds
      ["SELECT graph_topic_id, status, COUNT(*) AS n FROM kg_facts
        WHERE user_id = ? GROUP BY graph_topic_id, status"
       user-id]
      {:builder-fn rs/as-unqualified-maps})))

(defn list-kg-entities
  "Entities with their fact usage count; trigram-filtered when query is
   non-blank, else busiest first. Optional `source-id` (a graph_topic_id) keeps
   only entities that appear in a fact of that document. aliases normalized
   PgArray → vector so rows are Electric-wire safe."
  ([user-id query limit] (list-kg-entities user-id query limit nil))
  ([user-id query limit source-id]
   (let [blank? (or (nil? query) (str/blank? query))
         ;; Optional source filter: entity participates in a fact of the document.
         ;; No FK on kg_entities — reachable via kg_facts subject/object columns.
         src-sql (if source-id
                   " AND EXISTS (SELECT 1 FROM kg_facts fs
                       WHERE (fs.subject_entity_id = e.id OR fs.object_entity_id = e.id)
                         AND fs.graph_topic_id = ?)"
                   "")
         src-params (if source-id [source-id] [])
         base "SELECT e.id, e.label, e.aliases,
                 (SELECT COUNT(*) FROM kg_facts f
                  WHERE f.subject_entity_id = e.id OR f.object_entity_id = e.id) AS fact_count
               FROM kg_entities e WHERE e.user_id = ?"
         rows (if blank?
                (jdbc/execute! ds
                  (into [(str base src-sql
                           " ORDER BY fact_count DESC, e.label LIMIT ?")
                         user-id]
                    (concat src-params [limit]))
                  {:builder-fn rs/as-unqualified-maps})
                (jdbc/execute! ds
                  (into [(str base src-sql
                           " AND GREATEST(
                               similarity(e.label, ?),
                               COALESCE((SELECT MAX(similarity(a, ?)) FROM unnest(e.aliases) a), 0)
                             ) >= 0.2
                             ORDER BY GREATEST(
                               similarity(e.label, ?),
                               COALESCE((SELECT MAX(similarity(a, ?)) FROM unnest(e.aliases) a), 0)
                             ) DESC LIMIT ?")
                         user-id]
                    (concat src-params [query query query query limit]))
                  {:builder-fn rs/as-unqualified-maps}))]
     (kg-aliases->vec rows))))

(defn set-kg-fact-status!
  "Fact status transition. Approving also approves the fact's predicate;
   REJECTING retires every live question that covers the fact (the question's
   ground truth is gone — done-condition: reject never orphans a drawable
   question). Ownership enforced in SQL.
   Post: true iff the fact row changed."
  [user-id fact-id status]
  (jdbc/with-transaction [tx ds]
    (let [row (jdbc/execute-one! tx
                ["UPDATE kg_facts SET status = ?, reviewed_at = now()
                  WHERE id = ? AND user_id = ? RETURNING predicate_id"
                 status fact-id user-id]
                {:builder-fn rs/as-unqualified-maps})]
      (when (and row (= "approved" status))
        (jdbc/execute! tx
          ["UPDATE kg_predicates SET status = 'approved', reviewed_at = now()
            WHERE id = ? AND status = 'proposed'" (:predicate_id row)]))
      (when (and row (= "rejected" status))
        (jdbc/execute! tx
          ["UPDATE kg_questions SET status = 'retired'
            WHERE status = 'approved' AND id IN
              (SELECT question_id FROM kg_question_facts WHERE fact_id = ?)"
           fact-id]))
      (some? row))))

(defn reject-kg-question!
  "Reject a question and log an undo entry (so the toast can offer Undo).
   Returns the undo entry id, or nil when the question was missing or already
   rejected. The undo insert follows the mutation non-transactionally, matching
   insert-undo-entry!'s contract."
  [user-id question-id]
  (let [prev (jdbc/with-transaction [tx ds]
               (let [row (jdbc/execute-one! tx
                           ["SELECT status FROM kg_questions WHERE id = ? AND user_id = ?"
                            question-id user-id]
                           {:builder-fn rs/as-unqualified-maps})]
                 (when (and row (not= "rejected" (:status row)))
                   (jdbc/execute! tx
                     ["UPDATE kg_questions SET status = 'rejected' WHERE id = ? AND user_id = ?"
                      question-id user-id])
                   (:status row))))]
    (when prev
      (insert-undo-entry! user-id "reject-question" "question"
        [question-id]
        [{:question-id question-id :prev-status prev}]))))

(defn restore-rejected-question!
  "Undo a reject-question: restore the question's pre-reject status."
  [snapshot]
  (let [{:keys [question-id prev-status]} (first snapshot)]
    (jdbc/execute! ds
      ["UPDATE kg_questions SET status = ? WHERE id = ?" prev-status question-id])))

(defn reject-kg-fact!
  "Reject a fact and, like set-kg-fact-status!'s reject path, retire every live
   question that covers it — but snapshot the EXACT question ids retired so undo
   un-retires precisely those (never one already retired before this reject,
   never missing one). Returns the undo entry id, or nil when the fact was
   missing or already rejected."
  [user-id fact-id]
  (let [snap (jdbc/with-transaction [tx ds]
               (let [row (jdbc/execute-one! tx
                           ["SELECT status FROM kg_facts WHERE id = ? AND user_id = ?"
                            fact-id user-id]
                           {:builder-fn rs/as-unqualified-maps})]
                 (when (and row (not= "rejected" (:status row)))
                   ;; Capture the approved covering questions BEFORE retiring — these,
                   ;; and only these, are the ones this reject transitions to retired.
                   (let [qids (mapv :id
                                (jdbc/execute! tx
                                  ["SELECT id FROM kg_questions
                                    WHERE status = 'approved' AND id IN
                                      (SELECT question_id FROM kg_question_facts WHERE fact_id = ?)"
                                   fact-id]
                                  {:builder-fn rs/as-unqualified-maps}))]
                     (jdbc/execute! tx
                       ["UPDATE kg_facts SET status = 'rejected', reviewed_at = now()
                         WHERE id = ? AND user_id = ?" fact-id user-id])
                     (when (seq qids)
                       (jdbc/execute! tx
                         (sql/format {:update :kg_questions :set {:status "retired"}
                                      :where [:in :id qids]})))
                     {:fact-id fact-id :prev-status (:status row)
                      :retired-question-ids (vec qids)}))))]
    (when snap
      (insert-undo-entry! user-id "reject-fact" "fact" [fact-id] [snap]))))

(defn reject-kg-facts!
  "Reject many facts as ONE operation: one transaction, one undo entry.

   Not a loop over reject-kg-fact!. That would write one undo entry, one toast and
   one invalidation bump per fact — and the undo log is hard-capped at 100 rows per
   user, so a large selection would evict the user's unrelated undo history and
   truncate its own reversal.

   Per-fact precision is preserved, which is the whole difficulty: each fact keeps
   its own prev-status, and the retired question ids are recorded PER FACT rather
   than unioned, so undo un-retires exactly the questions this operation retired and
   never one that was already retired beforehand.
   Pre:  fact-ids from the user's selection; ownership re-checked in SQL.
   Post: {:rejected n :undo-id id-or-nil} — n counts only facts that were owned and
         not already rejected; :undo-id is nil iff n = 0. Already-rejected and
         foreign ids are skipped silently, exactly as the single-fact path skips
         them."
  [user-id fact-ids]
  (if (empty? fact-ids)
    {:rejected 0 :undo-id nil}
    (let [{:keys [snaps undo-id]}
          (jdbc/with-transaction [tx ds]
            (let [rows (jdbc/execute! tx
                         (sql/format {:select [:id :status] :from :kg_facts
                                      :where [:and [:= :user_id user-id]
                                              [:in :id (vec fact-ids)]
                                              [:not= :status "rejected"]]
                                      :for :update})
                         {:builder-fn rs/as-unqualified-maps})
                  ids (mapv :id rows)]
              (if (empty? ids)
                {:snaps [] :undo-id nil}
                ;; Capture each fact's approved covering questions BEFORE retiring
                ;; anything: after the UPDATE below there is no way to tell which
                ;; questions this operation retired from ones already retired.
                (let [links (jdbc/execute! tx
                              (sql/format {:select [:qf.fact_id :q.id]
                                           :from [[:kg_question_facts :qf]]
                                           :join [[:kg_questions :q] [:= :q.id :qf.question_id]]
                                           :where [:and [:in :qf.fact_id ids]
                                                   [:= :q.status "approved"]]})
                              {:builder-fn rs/as-unqualified-maps})
                      by-fact (reduce (fn [m {:keys [fact_id id]}]
                                        (update m fact_id (fnil conj []) id))
                                {} links)
                      snaps (mapv (fn [{:keys [id status]}]
                                    {:fact-id id :prev-status status
                                     :retired-question-ids (vec (get by-fact id []))})
                              rows)
                      qids (into [] (distinct) (map :id links))]
                  (jdbc/execute! tx
                    (sql/format {:update :kg_facts
                                 :set {:status "rejected" :reviewed_at [:now]}
                                 :where [:and [:= :user_id user-id] [:in :id ids]]}))
                  (when (seq qids)
                    (jdbc/execute! tx
                      (sql/format {:update :kg_questions :set {:status "retired"}
                                   :where [:in :id qids]})))
                  {:snaps snaps
                   :undo-id (insert-undo-entry-raw! tx user-id "bulk-reject-facts" "fact"
                              ids snaps)}))))]
      ;; Pruning is deliberately outside the transaction — insert-undo-entry-raw!
      ;; does not prune, and the caller owns it once the mutation has committed.
      (when undo-id (prune-undo-log! ds user-id))
      {:rejected (count snaps) :undo-id undo-id})))

(defn restore-rejected-facts!
  "Undo a bulk-reject-facts: restore every fact's own pre-reject status and set
   exactly the questions that operation retired back to approved.
   Pre:  snapshot is the vector of {:fact-id :prev-status :retired-question-ids}
         written by reject-kg-facts!.
   Post: each fact carries its own prior status again — not a single shared one."
  [snapshot]
  (jdbc/with-transaction [tx ds]
    (doseq [{:keys [fact-id prev-status]} snapshot]
      (jdbc/execute! tx
        ["UPDATE kg_facts SET status = ? WHERE id = ?" prev-status fact-id]))
    (let [qids (into [] (distinct) (mapcat :retired-question-ids snapshot))]
      (when (seq qids)
        (jdbc/execute! tx
          (sql/format {:update :kg_questions :set {:status "approved"}
                       :where [:in :id qids]}))))))

(defn restore-rejected-fact!
  "Undo a reject-fact: restore the fact's pre-reject status and set exactly the
   questions this reject retired back to approved."
  [snapshot]
  (let [{:keys [fact-id prev-status retired-question-ids]} (first snapshot)]
    (jdbc/with-transaction [tx ds]
      (jdbc/execute! tx
        ["UPDATE kg_facts SET status = ? WHERE id = ?" prev-status fact-id])
      (when (seq retired-question-ids)
        (jdbc/execute! tx
          (sql/format {:update :kg_questions :set {:status "approved"}
                       :where [:in :id retired-question-ids]}))))))

(defn update-kg-fact-literal!
  "Edit-then-approve for literal-object facts (entity edits go through
   entity rename/merge instead). Ownership enforced in SQL."
  [user-id fact-id literal]
  (pos? (::jdbc/update-count
          (jdbc/execute-one! ds
            ["UPDATE kg_facts SET object_literal = ?
              WHERE id = ? AND user_id = ? AND object_literal IS NOT NULL"
             (sanitize-utf8 literal) fact-id user-id]))))

(defn rename-kg-entity!
  "Rename an entity; the old label joins aliases (deduped)."
  [user-id entity-id new-label]
  (pos? (::jdbc/update-count
          (jdbc/execute-one! ds
            ["UPDATE kg_entities
              SET aliases = (SELECT ARRAY(SELECT DISTINCT x FROM unnest(aliases || label) x
                                          WHERE x <> ?)),
                  label = ?
              WHERE id = ? AND user_id = ?"
             (sanitize-utf8 new-label) (sanitize-utf8 new-label) entity-id user-id]))))

(defn merge-kg-entities!
  "Merge `absorbed-id` into `target-id`: facts repointed, labels/aliases
   unioned, absorbed row deleted. Facts whose repoint would collide with the
   per-document s/p/o unique indexes are deleted (they are duplicates by
   definition). All-or-nothing transaction.
   Pre:  both entities belong to user-id and differ (violation = caller bug).
   Post: no kg_facts row references absorbed-id; absorbed row gone."
  [user-id target-id absorbed-id]
  (jdbc/with-transaction [tx ds]
    (let [owned (jdbc/execute! tx
                  ["SELECT id FROM kg_entities WHERE user_id = ? AND id IN (?, ?)"
                   user-id target-id absorbed-id])]
      (when (or (= target-id absorbed-id) (not= 2 (count owned)))
        (throw (ex-info "merge-kg-entities!: entities must be two distinct rows owned by user"
                 {:user-id user-id :target target-id :absorbed absorbed-id})))
      ;; Duplicates-after-repoint die first, then the survivors repoint.
      (jdbc/execute! tx
        ["DELETE FROM kg_facts f WHERE f.subject_entity_id = ?
          AND EXISTS (SELECT 1 FROM kg_facts g
                      WHERE g.subject_entity_id = ? AND g.predicate_id = f.predicate_id
                        AND g.graph_topic_id = f.graph_topic_id
                        AND (g.object_entity_id = f.object_entity_id
                             OR (g.object_literal IS NOT NULL AND g.object_literal = f.object_literal)))"
         absorbed-id target-id])
      (jdbc/execute! tx
        ["DELETE FROM kg_facts f WHERE f.object_entity_id = ?
          AND EXISTS (SELECT 1 FROM kg_facts g
                      WHERE g.object_entity_id = ? AND g.predicate_id = f.predicate_id
                        AND g.graph_topic_id = f.graph_topic_id
                        AND g.subject_entity_id = f.subject_entity_id)"
         absorbed-id target-id])
      ;; Facts joining the pair in either direction (or absorbed to itself)
      ;; would become self-loops after repointing; drop them.
      (jdbc/execute! tx
        ["DELETE FROM kg_facts
          WHERE (subject_entity_id = ? AND object_entity_id IN (?, ?))
             OR (subject_entity_id = ? AND object_entity_id = ?)"
         absorbed-id target-id absorbed-id target-id absorbed-id])
      (jdbc/execute! tx
        ["UPDATE kg_facts SET subject_entity_id = ? WHERE subject_entity_id = ?"
         target-id absorbed-id])
      (jdbc/execute! tx
        ["UPDATE kg_facts SET object_entity_id = ? WHERE object_entity_id = ?"
         target-id absorbed-id])
      (jdbc/execute! tx
        ["UPDATE kg_entities t
          SET aliases = (SELECT ARRAY(SELECT DISTINCT x
                                      FROM unnest(t.aliases || a.aliases || a.label) x
                                      WHERE x <> t.label))
          FROM kg_entities a
          WHERE t.id = ? AND a.id = ?" target-id absorbed-id])
      (jdbc/execute! tx ["DELETE FROM kg_entities WHERE id = ?" absorbed-id])
      true)))

(defn get-kg-approved-predicates
  "The user's approved vocabulary, for the extraction prompt."
  [user-id]
  (jdbc/execute! ds
    ["SELECT slug, label FROM kg_predicates WHERE user_id = ? AND status = 'approved'
      ORDER BY label" user-id]
    {:builder-fn rs/as-unqualified-maps}))

(defn relink-kg-fact-object!
  "Point `fact-id`'s object at the entity named `label` — matched
   case-insensitively among the user's entities, created if absent. Fact-local:
   other facts referencing the old entity are untouched (rename-everywhere is
   rename-kg-entity!'s job). The old entity may become unreferenced; it stays
   visible in the entities browser for cleanup.
   Pre:  the fact is entity-valued and owned by user-id.
   Post: :done — object repointed; :duplicate — the target s/p/o already
         exists in this document (fact left unchanged); nil — fact not found /
         not entity-valued."
  [user-id fact-id label]
  (let [label (sanitize-utf8 (str/trim (str label)))]
    (when-not (str/blank? label)
      (jdbc/with-transaction [tx ds]
        (let [entity-id (or (:id (jdbc/execute-one! tx
                                   ["SELECT id FROM kg_entities
                                     WHERE user_id = ? AND lower(label) = lower(?) LIMIT 1"
                                    user-id label]
                                   {:builder-fn rs/as-unqualified-maps}))
                            (:id (jdbc/execute-one! tx
                                   ["INSERT INTO kg_entities (user_id, label) VALUES (?, ?) RETURNING id"
                                    user-id label]
                                   {:builder-fn rs/as-unqualified-maps})))]
          (try
            (when (pos? (::jdbc/update-count
                          (jdbc/execute-one! tx
                            ["UPDATE kg_facts SET object_entity_id = ?
                              WHERE id = ? AND user_id = ? AND object_entity_id IS NOT NULL
                                AND subject_entity_id <> ?"
                             entity-id fact-id user-id entity-id])))
              :done)
            (catch org.postgresql.util.PSQLException e
              (if (= "23505" (.getSQLState e)) ; unique_violation on the s/p/o index
                :duplicate
                (throw e)))))))))

;; ── Question bank ───────────────────────────────────────────────────────────

(defn create-kg-question!
  "Insert a question and its fact links in one transaction.
   Pre:  fact-ids non-empty, all owned by user-id (generation queries only the
         user's facts — violation = caller bug).
   Post: the new question id."
  [user-id kind question answer fact-ids source-model]
  (jdbc/with-transaction [tx ds]
    (let [qid (:id (jdbc/execute-one! tx
                     ["INSERT INTO kg_questions (user_id, kind, question, reference_answer, source_model)
                       VALUES (?, ?, ?, ?, ?) RETURNING id"
                      user-id kind (sanitize-utf8 question) (sanitize-utf8 answer) source-model]
                     {:builder-fn rs/as-unqualified-maps}))]
      (jdbc/execute! tx
        (sql/format {:insert-into :kg_question_facts
                     :values (mapv (fn [fid] {:question_id qid :fact_id fid}) fact-ids)}))
      qid)))

(defn get-kg-questions
  "The user's questions with covered-fact counts; non-blank `query` filters
   question + answer text case-insensitively. status nil = all live views
   should pass \"approved\". Optional opts {:source-id gid :entity-id eid}:
   source-id keeps questions with ANY linked fact in that document; entity-id
   keeps questions with a linked fact touching that entity (subject or object).
   Neither is a stored column on kg_questions — both route via kg_question_facts
   → kg_facts. Newest first.

   The bank is the unflag/unsuspend surface, so a suspended question MUST stay
   visible here — this is the one question query that does not apply
   drawable-question-where. Opts {:flagged? true} / {:suspended? true} narrow to
   the corresponding worklist; false and nil both mean \"don't filter\"."
  ([user-id status query] (get-kg-questions user-id status query nil))
  ([user-id status query {:keys [source-id entity-id flagged? suspended?]}]
   (let [q (when-not (or (nil? query) (str/blank? query))
             (str "%" (str/trim query) "%"))]
     (jdbc/execute! ds
       (sql/format {:select [[:q.id :id] [:q.kind :kind] [:q.question :question]
                             [:q.reference_answer :reference_answer]
                             [:q.status :status]
                             [:q.flagged :flagged] [:q.suspended :suspended]
                             [[:raw "(SELECT COUNT(*) FROM kg_question_facts qf WHERE qf.question_id = q.id)"]
                              :fact_count]]
                    :from [[:kg_questions :q]]
                    :where [:and [:= :q.user_id user-id]
                            (when status [:= :q.status status])
                            (when flagged? [:= :q.flagged true])
                            (when suspended? [:= :q.suspended true])
                            (when q [:or [:ilike :q.question q]
                                     [:ilike :q.reference_answer q]])
                            (when source-id
                              [:exists {:select [1] :from [[:kg_question_facts :qf]]
                                        :join [[:kg_facts :f] [:= :f.id :qf.fact_id]]
                                        :where [:and [:= :qf.question_id :q.id]
                                                [:= :f.graph_topic_id source-id]]}])
                            (when entity-id
                              [:exists {:select [1] :from [[:kg_question_facts :qf2]]
                                        :join [[:kg_facts :f2] [:= :f2.id :qf2.fact_id]]
                                        :where [:and [:= :qf2.question_id :q.id]
                                                [:or [:= :f2.subject_entity_id entity-id]
                                                     [:= :f2.object_entity_id entity-id]]]}])]
                    :order-by [[:q.id :desc]]})
       {:builder-fn rs/as-unqualified-maps}))))

(defn fact-clusters-without-question
  "The atomic generator's work list, clustered by ambiguity.

   A cluster is every approved fact of the document sharing one
   (subject_entity_id, predicate_id) — the direction in which a question has more
   than one true answer: one subject, one predicate, N objects, so \"what does S
   P?\" is answerable N ways and is unlearnable as a single item. A cluster of size
   1 is unambiguous and yields one direct question.

   :members carries EVERY approved sibling, covered or not, because the generator
   must know the ambiguity is over N to avoid naming one member in another's
   question. :targets are the members with no live atomic question yet — the only
   ones a question is written for. A cluster whose targets are all covered is
   omitted entirely.

   Post: [{:subject_label :predicate_label
           :members [{:id :object_label :object_literal :object_entity_id
                      :page_number}]
           :targets #{fact-id}}], :targets non-empty, ordered by the cluster's
         lowest fact id so two runs over unchanged data agree."
  [user-id graph-topic-id]
  (let [rows (jdbc/execute! ds
               ["SELECT f.id, f.subject_entity_id, f.predicate_id, f.object_entity_id,
                        f.object_literal, f.page_number,
                        s.label AS subject_label, p.label AS predicate_label,
                        o.label AS object_label,
                        EXISTS (SELECT 1 FROM kg_question_facts qf
                                JOIN kg_questions kq ON kq.id = qf.question_id
                                WHERE qf.fact_id = f.id AND kq.kind = 'atomic'
                                  AND kq.status = 'approved') AS covered
                 FROM kg_facts f
                 JOIN kg_entities s ON s.id = f.subject_entity_id
                 JOIN kg_predicates p ON p.id = f.predicate_id
                 LEFT JOIN kg_entities o ON o.id = f.object_entity_id
                 WHERE f.user_id = ? AND f.graph_topic_id = ? AND f.status = 'approved'
                 ORDER BY f.id"
                user-id graph-topic-id]
               {:builder-fn rs/as-unqualified-maps})]
    (->> (group-by (juxt :subject_entity_id :predicate_id) rows)
      (keep (fn [[_key members]]
              (let [targets (into #{} (comp (remove :covered) (map :id)) members)]
                (when (seq targets)
                  {:subject_label (:subject_label (first members))
                   :predicate_label (:predicate_label (first members))
                   :members (mapv #(select-keys % [:id :object_label :object_literal
                                                   :object_entity_id :page_number])
                              members)
                   :targets targets}))))
      (sort-by #(reduce min (map :id (:members %))))
      vec)))

(defn kg-entity-discriminators
  "Approved facts touching each of `entity-ids`, as compact prompt rows keyed by
   entity id — the material that lets a question ask for one member of an ambiguous
   cluster by a property it alone has, instead of asking for \"one of\" the group.

   One query for the whole batch: called once per generation batch, never per
   object. An entity is absent from the result when it has no approved facts at all
   — the caller reads that as \"undiscriminable\" and omits the member.

   Post: {entity-id [{:fact_id :s :p :o}]}, ≤ `per-entity` rows each, lowest fact
         id first."
  [user-id entity-ids per-entity]
  (if (empty? entity-ids)
    {}
    (let [ids (vec (distinct entity-ids))
          wanted (set ids)
          rows (jdbc/execute! ds
                 (sql/format
                   {:select [[:f.id :fact_id] :f.subject_entity_id :f.object_entity_id
                             [:s.label :s] [:p.label :p]
                             [[:coalesce :o.label :f.object_literal] :o]]
                    :from [[:kg_facts :f]]
                    :join [[:kg_entities :s] [:= :s.id :f.subject_entity_id]
                           [:kg_predicates :p] [:= :p.id :f.predicate_id]]
                    :left-join [[:kg_entities :o] [:= :o.id :f.object_entity_id]]
                    :where [:and [:= :f.user_id user-id] [:= :f.status "approved"]
                            [:or [:in :f.subject_entity_id ids]
                             [:in :f.object_entity_id ids]]]
                    :order-by [[:f.id :asc]]})
                 {:builder-fn rs/as-unqualified-maps})
          row->prompt #(select-keys % [:fact_id :s :p :o])]
      ;; A fact can touch two wanted entities; it discriminates both.
      (reduce (fn [acc row]
                (reduce (fn [a eid]
                          (if (and (wanted eid) (< (count (get a eid)) per-entity))
                            (update a eid (fnil conj []) (row->prompt row))
                            a))
                  acc
                  [(:subject_entity_id row) (:object_entity_id row)]))
        {} rows))))

(defn entity-fact-neighborhood
  "Approved facts touching an entity (as subject or object), labeled with
   provenance and entity ids — input for synthesis generation AND the concept
   popover (whose rows link onward through the ids).
   Post: rows {:id :subject_label :predicate_label :object_label
   :object_literal :page_number :doc_title :subject_entity_id
   :object_entity_id}."
  [user-id entity-id]
  (jdbc/execute! ds
    ["SELECT f.id, s.label AS subject_label, p.label AS predicate_label,
             o.label AS object_label, f.object_literal, f.page_number,
             t.title AS doc_title, f.subject_entity_id, f.object_entity_id
      FROM kg_facts f
      JOIN kg_entities s ON s.id = f.subject_entity_id
      JOIN kg_predicates p ON p.id = f.predicate_id
      JOIN topics t ON t.id = f.graph_topic_id
      LEFT JOIN kg_entities o ON o.id = f.object_entity_id
      WHERE f.user_id = ? AND f.status = 'approved'
        AND (f.subject_entity_id = ? OR f.object_entity_id = ?)
      ORDER BY f.id"
     user-id entity-id entity-id]
    {:builder-fn rs/as-unqualified-maps}))

(defn kg-entity-card
  "Entity + its fact neighborhood — the concept popover's payload.
   Post: {:id :label :aliases [str] :facts [...]} (wire-safe) or nil."
  [user-id entity-id]
  (when-let [e (jdbc/execute-one! ds
                 ["SELECT id, label, aliases FROM kg_entities
                   WHERE id = ? AND user_id = ?" entity-id user-id]
                 {:builder-fn rs/as-unqualified-maps})]
    (assoc (first (kg-aliases->vec [e]))
      :facts (entity-fact-neighborhood user-id entity-id))))

(defn kg-graph-elements
  "The edges-only concept graph for the Graph tab. Node = an entity that
   participates in ≥1 approved entity→entity fact; edge = such a fact.
   Literal-object facts are excluded (no second node) — they reach the UI only
   through kg-entity-card's side panel. Degree is derived from the edge list by
   the caller (freememo.kg-graph), not per-node subqueries.
   Post: {:nodes [{:id :label}] :edges [{:s :t :p :topic}] :predicates
   [{:id :label}] :docs [{:id :title}]} — all wire-safe scalars."
  [user-id]
  (let [edges (jdbc/execute! ds
                ["SELECT subject_entity_id AS s, object_entity_id AS t,
                         predicate_id AS p, graph_topic_id AS topic
                  FROM kg_facts
                  WHERE user_id = ? AND status = 'approved'
                    AND object_entity_id IS NOT NULL"
                 user-id]
                {:builder-fn rs/as-unqualified-maps})
        nodes (jdbc/execute! ds
                ["SELECT id, label FROM kg_entities
                  WHERE user_id = ? AND id IN (
                    SELECT subject_entity_id FROM kg_facts
                      WHERE user_id = ? AND status = 'approved' AND object_entity_id IS NOT NULL
                    UNION
                    SELECT object_entity_id FROM kg_facts
                      WHERE user_id = ? AND status = 'approved' AND object_entity_id IS NOT NULL)"
                 user-id user-id user-id]
                {:builder-fn rs/as-unqualified-maps})
        predicates (jdbc/execute! ds
                     ["SELECT DISTINCT p.id, p.label FROM kg_predicates p
                       JOIN kg_facts f ON f.predicate_id = p.id
                       WHERE f.user_id = ? AND f.status = 'approved'
                         AND f.object_entity_id IS NOT NULL"
                      user-id]
                     {:builder-fn rs/as-unqualified-maps})
        docs (jdbc/execute! ds
               ["SELECT DISTINCT t.id, t.title FROM topics t
                 JOIN kg_facts f ON f.graph_topic_id = t.id
                 WHERE f.user_id = ? AND f.status = 'approved'
                   AND f.object_entity_id IS NOT NULL"
                user-id]
               {:builder-fn rs/as-unqualified-maps})]
    {:nodes nodes :edges edges :predicates predicates :docs docs}))

(defn read-graph-layout
  "Cached render payload for a Graph-tab scope, or nil on cold cache.
   Post: {:version <long> :payload {...}} | nil."
  [user-id scope]
  (when-let [row (jdbc/execute-one! ds
                   ["SELECT version, payload FROM kg_graph_layout
                     WHERE user_id = ? AND scope = ?" user-id scope]
                   {:builder-fn rs/as-unqualified-maps})]
    {:version (:version row) :payload (pgobject->clj (:payload row))}))

(defn write-graph-layout!
  "Upsert the positioned render payload for (user, scope) at version.
   Post: exactly one row for (user, scope); its version/payload are the args."
  [user-id scope version payload]
  (jdbc/execute! ds
    ["INSERT INTO kg_graph_layout (user_id, scope, version, payload, computed_at)
      VALUES (?, ?, ?, ?, now())
      ON CONFLICT (user_id, scope)
      DO UPDATE SET version = EXCLUDED.version, payload = EXCLUDED.payload,
                    computed_at = now()"
     user-id scope version (->jsonb payload)]))

(defn set-kg-question-status!
  "reject (curation) or retire. Ownership enforced in SQL.
   Post: true iff the row changed."
  [user-id question-id status]
  (pos? (::jdbc/update-count
          (jdbc/execute-one! ds
            ["UPDATE kg_questions SET status = ? WHERE id = ? AND user_id = ?"
             status question-id user-id]))))

(defn set-kg-question-flags!
  "Set a question's curation flags; nil field = keep current. Ownership enforced in
   SQL. Writes NO FSRS column by design (Anki parity): a suspension neither freezes
   nor shifts the schedule, so a question unsuspended after its due date returns as
   overdue rather than resurfacing on a fabricated timeline.
   Post: true iff the row exists and is owned; the question's fsrs_* columns are
         bit-identical across any suspend/unsuspend round-trip."
  [user-id question-id {:keys [flagged suspended]}]
  (pos? (::jdbc/update-count
          (jdbc/execute-one! ds
            ["UPDATE kg_questions
              SET flagged = COALESCE(?, flagged),
                  suspended = COALESCE(?, suspended)
              WHERE id = ? AND user_id = ?"
             flagged suspended question-id user-id]))))

(defn update-kg-question!
  "Edit question text and/or reference answer (nil field = keep current)."
  [user-id question-id question answer]
  (pos? (::jdbc/update-count
          (jdbc/execute-one! ds
            ["UPDATE kg_questions
              SET question = COALESCE(?, question),
                  reference_answer = COALESCE(?, reference_answer)
              WHERE id = ? AND user_id = ?"
             (some-> question sanitize-utf8) (some-> answer sanitize-utf8)
             question-id user-id]))))

;; ── Quiz/exam sessions ──────────────────────────────────────────────────────

(defn- sql-int-array->vec [a]
  (some->> ^java.sql.Array a .getArray (mapv long)))

;; Write-side counterparts to sql-int-array->vec. A bare ARRAY[] has no inferable
;; element type in Postgres, so the EMPTY case must carry an explicit cast. The
;; failure mode is nasty enough to be worth centralising: it is a runtime SQL error
;; on the empty path only, so a writer that is only ever tested with a non-empty
;; array looks correct and then throws in production the first time it is handed
;; nothing — inside an Electric server thunk that kills the session.

(defn- int-array-value
  "HoneySQL value for an INTEGER[] column.
   Post: an expression valid for both empty and non-empty `ids`."
  [ids]
  (if (seq ids) [:array (vec ids)] [:raw "'{}'::integer[]"]))

(defn- text-array-value
  "HoneySQL value for a TEXT[] column; elements are UTF-8 sanitised.
   Post: an expression valid for both empty and non-empty `strs`."
  [strs]
  (if (seq strs) [:array (mapv sanitize-utf8 strs)] [:raw "'{}'::text[]"]))

(defn- drawable-question-where
  "SQL predicate for \"this question may be drawn into a quiz or review\": approved
   curation status AND not suspended. The single definition shared by every draw
   site (kg-questions-per-doc, draw-kg-questions, draw-fsrs-due-queue) so the scope
   picker's counts can never drift from what a draw actually returns, and a future
   withholding rule is added in one place.
   Pre:  `alias` is the kg_questions alias in the enclosing query (no user input).
   Post: a SQL fragment safe to AND into a WHERE clause."
  [alias]
  (str alias ".status = 'approved' AND NOT " alias ".suspended"))

(defn kg-questions-per-doc
  "Drawable-question count per document, for the quiz scope picker. Counts exactly
   what a draw over that document would be allowed to return, suspensions included.
   Post: {graph-topic-id n} (a question spanning docs counts in each)."
  [user-id]
  (into {}
    (map (juxt :graph_topic_id :n))
    (jdbc/execute! ds
      [(str "SELECT f.graph_topic_id, COUNT(DISTINCT q.id) AS n
             FROM kg_questions q
             JOIN kg_question_facts qf ON qf.question_id = q.id
             JOIN kg_facts f ON f.id = qf.fact_id
             WHERE q.user_id = ? AND " (drawable-question-where "q")
        " GROUP BY f.graph_topic_id")
       user-id]
      {:builder-fn rs/as-unqualified-maps})))

(defn draw-kg-questions
  "Random draw of up to n drawable questions ALL of whose facts lie inside the
   scope documents (a question must be answerable from the chosen material).
   Post: vector of question ids, ≤ n, random order; no suspended question."
  [user-id scope-topic-ids n]
  (mapv :id
    (jdbc/execute! ds
      (sql/format {:select [:q.id]
                   :from [[:kg_questions :q]]
                   :where [:and [:= :q.user_id user-id]
                           [:raw (drawable-question-where "q")]
                           [:exists {:select [1] :from [[:kg_question_facts :qf]]
                                     :join [[:kg_facts :f] [:= :f.id :qf.fact_id]]
                                     :where [:and [:= :qf.question_id :q.id]
                                             [:in :f.graph_topic_id scope-topic-ids]]}]
                           [:not [:exists {:select [1] :from [[:kg_question_facts :qf]]
                                           :join [[:kg_facts :f] [:= :f.id :qf.fact_id]]
                                           :where [:and [:= :qf.question_id :q.id]
                                                   [:not-in :f.graph_topic_id scope-topic-ids]]}]]]
                   :order-by [[[:raw "random()"]]]
                   :limit n})
      {:builder-fn rs/as-unqualified-maps})))

(defn create-kg-session!
  "Freeze a draw into a session row. Pre: question-ids non-empty (callers
   refuse an empty draw — violation = caller bug). Post: session id."
  [user-id kind scope-topic-ids question-ids time-limit-seconds]
  (:id (jdbc/execute-one! ds
         (sql/format {:insert-into :kg_sessions
                      :values [{:user_id user-id :kind kind
                                :scope_topic_ids [:array (vec scope-topic-ids)]
                                :question_ids [:array (vec question-ids)]
                                :time_limit_seconds time-limit-seconds}]
                      :returning [:id]})
         {:builder-fn rs/as-unqualified-maps})))

(defn get-active-kg-session
  "Latest unfinished session of `kind`, wire-lean, or nil.
   Post: {:id :kind :question-ids [long] :answered n :time-limit-seconds
          :elapsed-seconds n} — :answered counts SAVED answers (graded or
   not), so an exam resumes at the first unanswered question even when
   grading hasn't run. Timestamps stay server-side; the client gets derived
   numbers only."
  [user-id kind]
  (when-let [row (jdbc/execute-one! ds
                   ["SELECT id, kind, question_ids, time_limit_seconds,
                       EXTRACT(EPOCH FROM (now() - started_at))::bigint AS elapsed_seconds,
                       (SELECT COUNT(*) FROM kg_answers a
                        WHERE a.session_id = s.id) AS answered
                     FROM kg_sessions s
                     WHERE user_id = ? AND kind = ? AND finished_at IS NULL
                     ORDER BY id DESC LIMIT 1"
                    user-id kind]
                   {:builder-fn rs/as-unqualified-maps})]
    {:id (:id row)
     :kind (:kind row)
     :question-ids (sql-int-array->vec (:question_ids row))
     :answered (long (:answered row))
     :time-limit-seconds (:time_limit_seconds row)
     :elapsed-seconds (long (:elapsed_seconds row))}))

(defn get-kg-question-for-session
  "Everything one quiz turn needs: the question, its facts with document +
   page provenance, and the highlight lexicon (labels + aliases of every
   entity in the linked facts). Wire-lean. nil when not found/not owned."
  [user-id question-id]
  (when-let [q (jdbc/execute-one! ds
                 ["SELECT id, kind, question, reference_answer, flagged, suspended
                   FROM kg_questions
                   WHERE id = ? AND user_id = ?" question-id user-id]
                 {:builder-fn rs/as-unqualified-maps})]
    (let [facts (jdbc/execute! ds
                  ["SELECT f.id, s.label AS subject_label, p.label AS predicate_label,
                       o.label AS object_label, f.object_literal, f.page_number,
                       t.title AS doc_title, f.graph_topic_id,
                       f.subject_entity_id, f.object_entity_id
                    FROM kg_question_facts qf
                    JOIN kg_facts f ON f.id = qf.fact_id
                    JOIN kg_entities s ON s.id = f.subject_entity_id
                    JOIN kg_predicates p ON p.id = f.predicate_id
                    JOIN topics t ON t.id = f.graph_topic_id
                    LEFT JOIN kg_entities o ON o.id = f.object_entity_id
                    WHERE qf.question_id = ?" question-id]
                  {:builder-fn rs/as-unqualified-maps})
          entity-ids (into #{} (comp (mapcat (juxt :subject_entity_id :object_entity_id))
                                 (remove nil?))
                       facts)
          entities (when (seq entity-ids)
                     (kg-aliases->vec
                       (jdbc/execute! ds
                         (sql/format {:select [:id :label :aliases]
                                      :from [:kg_entities]
                                      :where [:in :id (vec entity-ids)]})
                         {:builder-fn rs/as-unqualified-maps})))
          keywords (into []
                     (comp (mapcat (fn [{:keys [label aliases]}] (cons label aliases)))
                       (remove str/blank?)
                       (distinct))
                     entities)]
      {:id (:id q) :kind (:kind q)
       :question (:question q) :reference-answer (:reference_answer q)
       :flagged (boolean (:flagged q)) ; the in-quiz Flag toggle renders from this
       :suspended (boolean (:suspended q))
       :facts facts ; entity ids stay on rows — feedback links through them
       :entities (or entities [])
       :keywords keywords})))

(defn kg-facts-labeled
  "The given facts with labels and provenance — same row shape as
   entity-fact-neighborhood, so FactLine renders them unchanged. Used by the review
   detail to show missed facts. Ownership enforced in SQL.
   Post: labeled rows for the owned subset of `fact-ids`, lowest id first (a
         cascade-deleted fact simply drops out)."
  [user-id fact-ids]
  (if (empty? fact-ids)
    []
    (jdbc/execute! ds
      (sql/format
        {:select [[:f.id :id] [:s.label :subject_label] [:p.label :predicate_label]
                  [:o.label :object_label] [:f.object_literal :object_literal]
                  [:f.page_number :page_number] [:t.title :doc_title]
                  [:f.graph_topic_id :graph_topic_id]
                  [:f.subject_entity_id :subject_entity_id]
                  [:f.object_entity_id :object_entity_id]]
         :from [[:kg_facts :f]]
         :join [[:kg_entities :s] [:= :s.id :f.subject_entity_id]
                [:kg_predicates :p] [:= :p.id :f.predicate_id]
                [:topics :t] [:= :t.id :f.graph_topic_id]]
         :left-join [[:kg_entities :o] [:= :o.id :f.object_entity_id]]
         :where [:and [:= :f.user_id user-id] [:in :f.id (vec fact-ids)]]
         :order-by [[:f.id :asc]]})
      {:builder-fn rs/as-unqualified-maps})))

(defn record-kg-answer!
  "Persist the user's answer text before grading (an LLM failure must not
   lose what they typed). Idempotent per (session, question).

   question_text is snapshotted here rather than passed in by each of the three
   call sites, and is deliberately NOT touched by the upsert: it records the
   wording as FIRST asked, so re-answering within a session cannot rewrite the
   record the history list renders.
   Post: answer row id; question_text set on insert and unchanged thereafter."
  [user-id session-id question-id position user-answer]
  (:id (jdbc/execute-one! ds
         ["INSERT INTO kg_answers (session_id, question_id, position, user_answer, question_text)
           SELECT s.id, ?, ?, ?, (SELECT q.question FROM kg_questions q WHERE q.id = ?)
           FROM kg_sessions s
           WHERE s.id = ? AND s.user_id = ?
           ON CONFLICT (session_id, question_id)
             DO UPDATE SET user_answer = EXCLUDED.user_answer
           RETURNING id"
          question-id position (sanitize-utf8 user-answer) question-id session-id user-id]
         {:builder-fn rs/as-unqualified-maps})))

(defn grade-kg-answer!
  "Write the grading verdict. Only freememo.kg-grade calls this — it owns the
   ⊆-validation of missed-fact-ids and matched-keywords."
  [answer-id verdict explanation missed-fact-ids matched-keywords]
  (jdbc/execute-one! ds
    (sql/format {:update :kg_answers
                 :set {:verdict verdict
                       :explanation (sanitize-utf8 explanation)
                       :missed_fact_ids (int-array-value missed-fact-ids)
                       :matched_keywords (text-array-value matched-keywords)
                       :graded_at [:now]}
                 :where [:= :id answer-id]}))
  answer-id)

(defn finish-kg-session!
  "Close a session (idempotent). Post: true iff this call closed it."
  [user-id session-id]
  (pos? (::jdbc/update-count
          (jdbc/execute-one! ds
            ["UPDATE kg_sessions SET finished_at = now()
              WHERE id = ? AND user_id = ? AND finished_at IS NULL"
             session-id user-id]))))

(defn kg-session-verdict-counts
  "Graded-verdict tally for a session's summary. Post: {verdict-string n}."
  [user-id session-id]
  (into {}
    (map (juxt :verdict :n))
    (jdbc/execute! ds
      ["SELECT a.verdict, COUNT(*) AS n FROM kg_answers a
        JOIN kg_sessions s ON s.id = a.session_id
        WHERE s.id = ? AND s.user_id = ? AND a.verdict IS NOT NULL
        GROUP BY a.verdict"
       session-id user-id]
      {:builder-fn rs/as-unqualified-maps})))

;; ---------------------------------------------------------------------------
;; FSRS review scheduling (freememo.fsrs). The Review quiz is the sole writer;
;; custom quiz/exam never touch these. "Today" is server-local (CURRENT_DATE),
;; matching every other due-query in this file.
;; ---------------------------------------------------------------------------

(defn fsrs-daily-counts
  "Today's FSRS tallies for cap enforcement, from the append-only log.
   Post: {:new-today n :reviews-today n} — new = first-ever reviews
   (reps_before = 0); reviews = reviews of cards already in Review state."
  [user-id]
  (let [row (jdbc/execute-one! ds
              ["SELECT COUNT(*) FILTER (WHERE reps_before = 0)  AS new_today,
                       COUNT(*) FILTER (WHERE state_before = 2) AS reviews_today
                FROM kg_reviews
                WHERE user_id = ? AND reviewed_at::date = CURRENT_DATE"
               user-id]
              {:builder-fn rs/as-unqualified-maps})]
    {:new-today (long (:new_today row)) :reviews-today (long (:reviews_today row))}))

;; --- Card quiz items -------------------------------------------------------
;; A quiz item is [:question id] or [:card flashcard-id ord]. The draw, the client
;; queue, apply-fsrs-review! and the review log all speak this one shape
;; (plans/cards-in-quiz-queue.md §1.5).

(defn- card-quiz-where
  "SQL predicate for \"this flashcard yields quiz items\". Occlusion, score and
   overlapping cards carry no typed answer, so the quiz never asks them.
   Pre:  `alias` is the flashcards alias in the enclosing query (no user input).
   Post: a SQL fragment safe to AND into a WHERE clause."
  [alias]
  (str alias ".kind IN ('basic','cloze')"))

(defn- live-ord?
  "Whether `ord` still exists in this card's text — the guard that keeps a stale
   schedule row (its deletion edited away) out of a draw. A basic card has exactly
   ord 0; a cloze card's ords come from freememo.cloze, the single scan.
   Pre:  `kind` is 'basic' or 'cloze' (card-quiz-where guarantees it)."
  [kind cloze-text ord]
  (if (= "basic" kind)
    (zero? ord)
    (boolean (some #{ord} (cloze/ords cloze-text)))))

(defn- card-refs
  "Rows of {:flashcard_id :ord :kind :cloze} → [:card id ord] refs, dropping any
   whose ord no longer exists in the card text.
   Post: order preserved; every ref's ord is live."
  [rows]
  (into []
    (comp (filter #(live-ord? (:kind %) (:cloze %) (:ord %)))
      (map (fn [r] [:card (:flashcard_id r) (long (:ord r))])))
    rows))

(defn- due-card-items
  "Card items whose schedule is due, for one tier. Not SQL-limited: the ord-liveness
   filter runs in Clojure, so a LIMIT here could silently under-fill. The caller's
   cap bounds the result instead.
   Post: refs ordered by due ascending."
  [user-id where]
  (card-refs
    (jdbc/execute! ds
      [(str "SELECT cs.flashcard_id, cs.ord, f.kind, f.cloze
             FROM card_schedules cs
             JOIN flashcards f ON f.id = cs.flashcard_id
             JOIN topics root ON root.id = f.root_topic_id
             WHERE root.user_id = ? AND " (card-quiz-where "f")
        " AND " where
        " ORDER BY cs.due ASC")
       user-id]
      {:builder-fn rs/as-unqualified-maps})))

(defn- fresh-card-items
  "Card items never yet reviewed, up to `room`.

   Two passes, cheap first. An untouched card (no schedule row at all) yields every
   ord it has, so a SQL LIMIT over untouched cards is safe and usually fills the
   room on its own. Only when it does not do we scan partially-reviewed cloze cards,
   whose fresh ords need the text parsed.
   Post: ≤ room refs; none has a card_schedules row."
  [user-id room]
  (let [untouched
        (into []
          (mapcat (fn [{:keys [id kind cloze]}]
                    (if (= "basic" kind)
                      [[:card id 0]]
                      (map (fn [n] [:card id n]) (cloze/ords cloze)))))
          (jdbc/execute! ds
            [(str "SELECT f.id, f.kind, f.cloze FROM flashcards f
                   JOIN topics root ON root.id = f.root_topic_id
                   WHERE root.user_id = ? AND " (card-quiz-where "f")
              " AND NOT EXISTS (SELECT 1 FROM card_schedules cs
                                WHERE cs.flashcard_id = f.id)
                ORDER BY f.id LIMIT ?")
             user-id room]
            {:builder-fn rs/as-unqualified-maps}))]
    (if (>= (count untouched) room)
      (into [] (take room) untouched)
      (let [partial-refs
            (into []
              (mapcat (fn [{:keys [id cloze ords]}]
                        (let [seen (set (sql-int-array->vec ords))]
                          (into [] (comp (remove seen) (map (fn [n] [:card id n])))
                            (cloze/ords cloze)))))
              (jdbc/execute! ds
                ["SELECT f.id, f.cloze, array_agg(cs.ord) AS ords
                  FROM flashcards f
                  JOIN topics root ON root.id = f.root_topic_id
                  JOIN card_schedules cs ON cs.flashcard_id = f.id
                  WHERE root.user_id = ? AND f.kind = 'cloze'
                  GROUP BY f.id, f.cloze
                  ORDER BY f.id"
                 user-id]
                {:builder-fn rs/as-unqualified-maps}))]
        (into [] (take room) (concat untouched partial-refs))))))

(defn- alternate
  "Lazily take from `a` and `b` in turn; when one runs out the other supplies the
   rest. Post: a seq of (count a) + (count b) items, each input's order preserved."
  [a b]
  (lazy-seq
    (cond
      (empty? a) b
      (empty? b) a
      :else (cons (first a) (cons (first b) (alternate (rest a) (rest b)))))))

(defn draw-review-queue
  "Ordered quiz items for a Review sitting, mixing questions and card items.

   Three tiers in order: learning/relearning due now (uncapped), then Review items
   due today (capped at review-limit − reviews so far), then never-introduced items
   (capped at new-limit − new so far). Inside each capped tier, questions and cards
   alternate, so a card bank several times the size of the question bank cannot
   crowd questions out (plans/cards-in-quiz-queue.md D11). One budget spans both
   types — fsrs_daily_counts reads the single kg_reviews log.

   The client holds this as a live queue and re-enqueues items still due today.
   Post: vector of item refs, order learning → review → new; no suspended question
         and no stale cloze ord in any tier."
  [user-id new-limit review-limit]
  (let [{:keys [new-today reviews-today]} (fsrs-daily-counts user-id)
        review-room (max 0 (- review-limit reviews-today))
        new-room    (max 0 (- new-limit new-today))
        q (fn [where order limit]
            (mapv (fn [r] [:question (:id r)])
              (jdbc/execute! ds
                (into [(str "SELECT q.id FROM kg_questions q
                             WHERE q.user_id = ? AND " (drawable-question-where "q")
                            " AND " where
                            " ORDER BY " order (when limit " LIMIT ?"))]
                  (cond-> [user-id] limit (conj limit)))
                {:builder-fn rs/as-unqualified-maps})))
        ;; Uncapped, so alternating cannot starve either type — it only mixes them.
        learning (alternate
                   (q "q.fsrs_state IN (1,3) AND q.fsrs_due IS NOT NULL AND q.fsrs_due <= now()"
                     "q.fsrs_due ASC" nil)
                   (due-card-items user-id
                     "cs.state IN (1,3) AND cs.due IS NOT NULL AND cs.due <= now()"))
        reviews  (if (pos? review-room)
                   (take review-room
                     (alternate
                       (q "q.fsrs_state = 2 AND q.fsrs_due IS NOT NULL AND q.fsrs_due::date <= CURRENT_DATE"
                         "q.fsrs_due ASC" review-room)
                       (due-card-items user-id
                         "cs.state = 2 AND cs.due IS NOT NULL AND cs.due::date <= CURRENT_DATE")))
                   [])
        news     (if (pos? new-room)
                   (take new-room
                     (alternate (q "q.fsrs_due IS NULL" "q.id ASC" new-room)
                       (fresh-card-items user-id new-room)))
                   [])]
    (vec (concat learning reviews news))))

(defn- load-question-memory!
  "The question's FSRS state, row-locked. nil when it is not this user's.
   Post: {:state :step :stability :difficulty :reps :lapses :gap_secs} or nil."
  [tx user-id question-id]
  (jdbc/execute-one! tx
    ["SELECT fsrs_state AS state, fsrs_step AS step, fsrs_stability AS stability,
             fsrs_difficulty AS difficulty, fsrs_reps AS reps, fsrs_lapses AS lapses,
             EXTRACT(EPOCH FROM (now() - fsrs_last_review)) AS gap_secs
      FROM kg_questions
      WHERE id = ? AND user_id = ? FOR UPDATE"
     question-id user-id]
    {:builder-fn rs/as-unqualified-maps}))

(defn- load-card-memory!
  "The card item's FSRS state, row-locked, creating the row on first review.

   A missing row means never introduced, so it is inserted at fsrs/new-card state
   before the advance — this is where §2.3's lazy materialization happens. Ownership
   comes from flashcards.root_topic_id → topics; nil when the card is not this
   user's, in which case nothing is written.
   Post: the same shape load-question-memory! returns, or nil."
  [tx user-id card-id ord]
  (when (= user-id (:user_id (jdbc/execute-one! tx
                               ["SELECT root.user_id FROM flashcards f
                                 JOIN topics root ON root.id = f.root_topic_id
                                 WHERE f.id = ?" card-id]
                               {:builder-fn rs/as-unqualified-maps})))
    (jdbc/execute-one! tx
      ["INSERT INTO card_schedules (flashcard_id, ord, state, step)
        VALUES (?, ?, ?, ?)
        ON CONFLICT (flashcard_id, ord) DO NOTHING"
       card-id ord (:state fsrs/new-card) (:step fsrs/new-card)])
    (jdbc/execute-one! tx
      ["SELECT state, step, stability, difficulty, reps, lapses,
               EXTRACT(EPOCH FROM (now() - last_review)) AS gap_secs
        FROM card_schedules
        WHERE flashcard_id = ? AND ord = ? FOR UPDATE"
       card-id ord]
      {:builder-fn rs/as-unqualified-maps})))

(defn apply-fsrs-review!
  "Advance one quiz item's FSRS schedule by `rating` and append a kg_reviews row,
   in one transaction. Postgres now() is the transaction clock, so the elapsed gap
   and the new due date are computed against a single consistent instant.

   `item-ref` is [:question id] or [:card flashcard-id ord]. Both types advance by
   the same freememo.fsrs step and land in the same log; only the state table and
   the wording snapshot differ.

   The Review flow writes no kg_answers row, so this row IS its answer history:
   `record` carries {:verdict :explanation :user-answer :missed-fact-ids
   :grade-source} and the item's current wording is snapshotted by subselect. On the
   self-graded arm there is no verdict, explanation or answer — :grade-source 'self'
   is what tells that row apart from one written before grade_source existed.
   Pre:  the item belongs to user-id (else returns nil, no write); rating ∈ 1..4.
   Post: the item's FSRS state advanced; exactly one kg_reviews row inserted;
         returns {:state :reps :lapses :scheduled-days :due-today?}."
  [user-id item-ref rating record scheduler enable-fuzzing]
  (let [[item-kind a b] item-ref
        card-id (when (= :card item-kind) a)
        ord (when (= :card item-kind) b)
        question-id (when (= :question item-kind) a)]
    (jdbc/with-transaction [tx ds]
      (when-let [row (if card-id
                       (load-card-memory! tx user-id card-id ord)
                       (load-question-memory! tx user-id question-id))]
        (let [state-before (long (:state row))
              reps-before  (long (:reps row))
              gap          (:gap_secs row)
              days-since   (when gap (long (Math/floor (/ (double gap) 86400.0))))
              elapsed      (max 0 (or days-since 0))
              card {:state state-before
                    :step (some-> (:step row) long)
                    :stability (some-> (:stability row) double)
                    :difficulty (some-> (:difficulty row) double)}
              res (fsrs/review-card scheduler card rating elapsed days-since)
              state-after (:state res)
              base-secs (:interval-seconds res)
              secs (if (and enable-fuzzing (= 2 state-after) (>= base-secs 86400))
                     (* 86400 (fsrs/fuzz-interval-days scheduler (quot base-secs 86400) (rand)))
                     base-secs)
              scheduled-days (quot secs 86400)
              lapse? (and (= 2 state-before) (= 1 rating))
              upd (if card-id
                    (jdbc/execute-one! tx
                      ["UPDATE card_schedules
                        SET state = ?, step = ?, stability = ?, difficulty = ?,
                            reps = reps + 1, lapses = lapses + ?, last_review = now(),
                            due = now() + make_interval(secs => ?)
                        WHERE flashcard_id = ? AND ord = ?
                        RETURNING (due::date <= CURRENT_DATE) AS due_today"
                       state-after (:step res) (:stability res) (:difficulty res)
                       (if lapse? 1 0) (double secs) card-id ord]
                      {:builder-fn rs/as-unqualified-maps})
                    (jdbc/execute-one! tx
                      ["UPDATE kg_questions
                        SET fsrs_state = ?, fsrs_step = ?, fsrs_stability = ?,
                            fsrs_difficulty = ?, fsrs_reps = fsrs_reps + 1,
                            fsrs_lapses = fsrs_lapses + ?, fsrs_last_review = now(),
                            fsrs_due = now() + make_interval(secs => ?)
                        WHERE id = ? AND user_id = ?
                        RETURNING (fsrs_due::date <= CURRENT_DATE) AS due_today"
                       state-after (:step res) (:stability res) (:difficulty res)
                       (if lapse? 1 0) (double secs) question-id user-id]
                      {:builder-fn rs/as-unqualified-maps}))]
          (jdbc/execute-one! tx
            (sql/format
              {:insert-into :kg_reviews
               :values [{:user_id user-id :rating rating
                         :question_id question-id
                         :flashcard_id card-id :ord ord
                         :grade_source (or (:grade-source record) "ai")
                         :verdict (:verdict record)
                         :state_before state-before :state_after state-after
                         :reps_before reps-before
                         :stability_after (:stability res) :difficulty_after (:difficulty res)
                         :elapsed_days elapsed :scheduled_days scheduled-days
                         :user_answer (some-> (:user-answer record) sanitize-utf8)
                         :explanation (some-> (:explanation record) sanitize-utf8)
                         :missed_fact_ids (int-array-value (:missed-fact-ids record))
                         ;; The wording as asked. A cloze card stores its RAW text —
                         ;; with `ord` alongside, history can re-derive the exact
                         ;; prompt, and a later edit cannot rewrite the record.
                         :question_text (if card-id
                                          {:select [[[:coalesce :question :cloze]]]
                                           :from [:flashcards] :where [:= :id card-id]}
                                          {:select [:question] :from [:kg_questions]
                                           :where [:= :id question-id]})
                         :reviewed_at [:now]}]}))
          {:state state-after
           :reps (inc reps-before)
           :lapses (+ (long (:lapses row)) (if lapse? 1 0))
           :scheduled-days scheduled-days
           :due-today? (boolean (:due_today upd))})))))

(defn fsrs-review-history-daily
  "Per-day Review tallies over the last `days` days (server-local).
   Post: [{:day date :reviews n :good n :again n :new n}] newest first."
  [user-id days]
  (jdbc/execute! ds
    ["SELECT reviewed_at::date AS day,
             COUNT(*)                          AS reviews,
             COUNT(*) FILTER (WHERE rating = 3) AS good,
             COUNT(*) FILTER (WHERE rating = 1) AS again,
             COUNT(*) FILTER (WHERE reps_before = 0) AS new
      FROM kg_reviews
      WHERE user_id = ? AND reviewed_at >= CURRENT_DATE - make_interval(days => ?::int)
      GROUP BY day ORDER BY day DESC"
     user-id days]
    {:builder-fn rs/as-unqualified-maps}))

(defn review-due-count
  "How many items the next Review sitting would draw — the dashboard's due tile.
   Runs the same draw under the same caps, so the tile can never disagree with the
   queue Start opens. Counts questions and card items together.
   Post: a non-negative count."
  [user-id new-limit review-limit]
  (count (draw-review-queue user-id new-limit review-limit)))

(defn get-card-item
  "One card quiz item, as the review turn needs it.

   `prompt` is what the learner sees: a basic card's question, or the cloze text with
   this ord hidden and every other deletion revealed. `answer` is what it hides. Both
   are stored HTML — the caller renders them through freememo.math.
   Pre:  ord is live for this card (the draw guarantees it).
   Post: {:card-id :ord :kind :prompt :answer} or nil when not found, not owned, or
         the ord no longer exists in the card text."
  [user-id card-id ord]
  (when-let [c (jdbc/execute-one! ds
                 ["SELECT f.id, f.kind, f.question, f.answer, f.cloze
                   FROM flashcards f
                   JOIN topics root ON root.id = f.root_topic_id
                   WHERE f.id = ? AND root.user_id = ?"
                  card-id user-id]
                 {:builder-fn rs/as-unqualified-maps})]
    (when (live-ord? (:kind c) (:cloze c) ord)
      {:card-id (:id c) :ord ord :kind (:kind c)
       :prompt (if (= "basic" (:kind c))
                 (:question c)
                 (cloze/mask-ord (:cloze c) ord))
       :answer (if (= "basic" (:kind c))
                 (:answer c)
                 (cloze/answer-for-ord (:cloze c) ord))})))

(defn kg-question-bank-counts
  "Bank-wide tallies for the quiz dashboard tiles. Counts live (approved) questions
   only — a rejected or retired question is not part of the bank.
   Post: {:live n :flagged n :suspended n}; :flagged and :suspended overlap, since
         the two flags are independent."
  [user-id]
  (let [r (jdbc/execute-one! ds
            ["SELECT COUNT(*) AS live,
                     COUNT(*) FILTER (WHERE flagged)   AS flagged,
                     COUNT(*) FILTER (WHERE suspended) AS suspended
              FROM kg_questions
              WHERE user_id = ? AND status = 'approved'"
             user-id]
            {:builder-fn rs/as-unqualified-maps})]
    {:live (long (:live r)) :flagged (long (:flagged r))
     :suspended (long (:suspended r))}))

(defn- review-row-prompt
  "The wording to show for one kg_reviews row.

   A question row's snapshot is already display text. A CARD row's snapshot is the
   card's raw text, so a cloze snapshot still carries its {{cN::…}} markers and must
   be masked back to the prompt that ord was asked as. ord 0 means a basic card —
   cloze numbering is a gap-free 1..max, so 0 can only be the basic ord.
   Post: a string, or nil when the row predates the snapshot column."
  [{:keys [flashcard_id ord question_text]}]
  (cond
    (nil? question_text) nil
    (nil? flashcard_id) question_text
    (zero? (long ord)) question_text
    :else (cloze/mask-ord question_text (long ord))))

(defn fsrs-review-log
  "One row per graded review, newest first — the Reviews tab's list. Covers both
   item types: a card row carries :flashcard_id + :ord instead of :question_id.

   :prompt is the wording as asked; the LIST renders that snapshot so a later edit
   cannot rewrite the record, while the detail reads the live item by id. A nil
   :prompt marks a row written before the snapshot column existed — \"not
   recorded\", which the UI must not present as \"unchanged\".
   Post: ≤ `limit` rows {:id :question_id :flashcard_id :ord :prompt :question_text
         :verdict :rating :grade_source :reviewed_at}, newest first."
  [user-id limit]
  (into []
    (map (fn [r] (assoc r :prompt (review-row-prompt r))))
    (jdbc/execute! ds
      ["SELECT r.id, r.question_id, r.flashcard_id, r.ord, r.question_text,
               r.verdict, r.rating, r.grade_source,
               to_char(r.reviewed_at, 'YYYY-MM-DD HH24:MI') AS reviewed_at
        FROM kg_reviews r
        WHERE r.user_id = ?
        ORDER BY r.reviewed_at DESC, r.id DESC
        LIMIT ?"
       user-id limit]
      {:builder-fn rs/as-unqualified-maps})))

(defn kg-review-detail
  "One review as the answer view needs it: what was answered, how it was graded, the
   wording as asked, and the item's wording NOW (by id, so an edit is visible here
   even though the list keeps the snapshot).

   The joins are LEFT joins because exactly one of them matches: a question row has
   no flashcard and a card row has no question. A card row's live wording comes from
   get-card-item, so an edited deletion shows here the same way an edited question
   does; a card whose ord was edited away has no live wording at all.
   Post: {:review {…} :missed-facts [labeled rows]} or nil when not owned/found;
         :question_text may be nil (pre-column row); :live_question is nil only for a
         card item whose ord is gone."
  [user-id review-id]
  (when-let [r (jdbc/execute-one! ds
                 ["SELECT r.id, r.question_id, r.flashcard_id, r.ord, r.verdict,
                          r.rating, r.grade_source, r.user_answer,
                          r.explanation, r.question_text, r.missed_fact_ids,
                          to_char(r.reviewed_at, 'YYYY-MM-DD HH24:MI') AS reviewed_at,
                          q.question AS live_question, q.reference_answer,
                          q.flagged, q.suspended
                   FROM kg_reviews r
                   LEFT JOIN kg_questions q ON q.id = r.question_id
                   WHERE r.id = ? AND r.user_id = ?"
                  review-id user-id]
                 {:builder-fn rs/as-unqualified-maps})]
    (let [missed (or (sql-int-array->vec (:missed_fact_ids r)) [])
          card (when-let [cid (:flashcard_id r)]
                 (get-card-item user-id cid (long (:ord r))))]
      {:review (assoc r :missed_fact_ids missed
                 :prompt (review-row-prompt r)
                 :live_question (or (:live_question r) (:prompt card))
                 :reference_answer (or (:reference_answer r) (:answer card))
                 :flagged (boolean (:flagged r))
                 :suspended (boolean (:suspended r)))
       :missed-facts (kg-facts-labeled user-id missed)})))

(defn fsrs-question-states
  "Per-question FSRS snapshot for the history browser — reviewed questions,
   most-lapsed first. Post: ≤ `limit` rows with schedule state + due date."
  [user-id limit]
  (jdbc/execute! ds
    ["SELECT id, question, fsrs_state, fsrs_reps, fsrs_lapses,
             fsrs_stability, fsrs_due
      FROM kg_questions
      WHERE user_id = ? AND status = 'approved' AND fsrs_reps > 0
      ORDER BY fsrs_lapses DESC, fsrs_reps DESC
      LIMIT ?"
     user-id limit]
    {:builder-fn rs/as-unqualified-maps}))

(defn kg-fact-alternates
  "Approved facts that share a predicate+object or subject+predicate with any
   of `fact-ids` (excluding them) — the grader's 'also true' context, so an
   answer confirmed by a sibling fact is never marked wrong.
   Post: ≤ 12 labeled rows, same shape as entity-fact-neighborhood."
  [user-id fact-ids]
  (when (seq fact-ids)
    (jdbc/execute! ds
      (sql/format
        {:select [[:f.id :id] [:s.label :subject_label] [:p.label :predicate_label]
                  [:o.label :object_label] [:f.object_literal :object_literal]]
         :from [[:kg_facts :f]]
         :join [[:kg_entities :s] [:= :s.id :f.subject_entity_id]
                [:kg_predicates :p] [:= :p.id :f.predicate_id]]
         :left-join [[:kg_entities :o] [:= :o.id :f.object_entity_id]]
         :where [:and [:= :f.user_id user-id] [:= :f.status "approved"]
                 [:not-in :f.id (vec fact-ids)]
                 [:exists {:select [1] :from [[:kg_facts :lf]]
                           :where [:and [:in :lf.id (vec fact-ids)]
                                   [:= :lf.predicate_id :f.predicate_id]
                                   [:or
                                    [:and [:!= :lf.object_entity_id nil]
                                     [:= :lf.object_entity_id :f.object_entity_id]]
                                    [:= :lf.subject_entity_id :f.subject_entity_id]]]}]]
         :limit 12})
      {:builder-fn rs/as-unqualified-maps})))

;; ── Exam grading + session history ─────────────────────────────────────────

(defn kg-ungraded-answers
  "Saved-but-ungraded answers of a session, in sitting order — the exam
   grader's work list. Ownership enforced via the session join."
  [user-id session-id]
  (jdbc/execute! ds
    ["SELECT a.question_id, a.position, a.user_answer
      FROM kg_answers a JOIN kg_sessions s ON s.id = a.session_id
      WHERE s.id = ? AND s.user_id = ? AND a.verdict IS NULL
      ORDER BY a.position"
     session-id user-id]
    {:builder-fn rs/as-unqualified-maps}))

(defn list-kg-sessions
  "Finished sessions, newest first, with verdict tallies — the history list.
   Post: [{:id :kind :total :started :correct :partial :incorrect}]."
  [user-id]
  (jdbc/execute! ds
    ["SELECT s.id, s.kind, cardinality(s.question_ids) AS total,
             to_char(s.started_at, 'YYYY-MM-DD HH24:MI') AS started,
             COUNT(a.verdict) FILTER (WHERE a.verdict = 'correct')   AS correct,
             COUNT(a.verdict) FILTER (WHERE a.verdict = 'partial')   AS partial,
             COUNT(a.verdict) FILTER (WHERE a.verdict = 'incorrect') AS incorrect
      FROM kg_sessions s
      LEFT JOIN kg_answers a ON a.session_id = s.id
      WHERE s.user_id = ? AND s.finished_at IS NOT NULL
      GROUP BY s.id ORDER BY s.id DESC LIMIT 100"
     user-id]
    {:builder-fn rs/as-unqualified-maps}))

(defn kg-session-detail
  "One finished (or being-reviewed) session with its per-question record.

   Both wordings are returned: :question_text is the snapshot as asked (nil on rows
   written before that column existed) and :question is the question NOW. The list
   renders the snapshot, the detail renders live, and their difference is what raises
   the edited-since marker.
   Post: {:session {:id :kind :total :started} :answers [{:position :question
   :question_text :reference_answer :user_answer :verdict :explanation}]} or nil
   when not owned/found. Verdict nil = saved but ungraded (grading failure or skip);
   a question skipped via Suspend & Skip has no row at all, so it counts toward
   :total and shows as unanswered."
  [user-id session-id]
  (when-let [s (jdbc/execute-one! ds
                 ["SELECT id, kind, cardinality(question_ids) AS total,
                     to_char(started_at, 'YYYY-MM-DD HH24:MI') AS started
                   FROM kg_sessions WHERE id = ? AND user_id = ?"
                  session-id user-id]
                 {:builder-fn rs/as-unqualified-maps})]
    {:session s
     :answers (jdbc/execute! ds
                ["SELECT a.position, a.user_answer, a.verdict, a.explanation,
                     a.question_text, a.question_id,
                     q.question, q.reference_answer
                  FROM kg_answers a JOIN kg_questions q ON q.id = a.question_id
                  WHERE a.session_id = ? ORDER BY a.position"
                 session-id]
                {:builder-fn rs/as-unqualified-maps})}))
