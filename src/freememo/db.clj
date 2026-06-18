(ns freememo.db
  "Database connection and schema management for PostgreSQL.
   Unified topics model — all entities (documents, pages, extracts) are topics
   in a parent_id tree."
  (:require
   [next.jdbc :as jdbc]
   [next.jdbc.result-set :as rs]
   [honey.sql :as sql]
   [cheshire.core :as json]
   [taoensso.telemere :as tel]
   [clojure.string :as str]
   [freememo.csl-util :as csl]
   [freememo.html-cleaner :as cleaner]
   [freememo.input-check :as input]
   [freememo.quota :as quota]
   [freememo.config :as config]
   [freememo.text :as text])
  (:import [org.postgresql.util PGobject]))

(declare sanitize-utf8)
(declare migrate-to-topics!)
(declare backfill-content-text!)
(declare backfill-sources!)
(declare backfill-pdf-sources!)
(declare run-grandfather-migration!)
(declare start-purge-scheduler!)

;; ---------------------------------------------------------------------------
;; Connection configuration
;; ---------------------------------------------------------------------------

(def db-config
  {:dbtype "postgresql"
   :host (or (System/getenv "DB_HOST") "localhost")
   :port (Integer/parseInt (or (System/getenv "DB_PORT") "5432"))
   :dbname (or (System/getenv "DB_NAME") "cardmaker")
   :user (or (System/getenv "DB_USER") "cardmaker")
   :password (or (System/getenv "DB_PASSWORD") "dev")})

;; HikariCP datasource (connection pool)
(defonce ds (jdbc/get-datasource db-config))

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

(defn setup-schema []
  (tel/log! :info "Setting up database schema")

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
        CHECK (event_type IN ('advance','touch','postpone','done','restore','priority-change')),
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

  ;; Search: plain-text column derived from HTML content, trigram GIN index
  (jdbc/execute! ds ["ALTER TABLE topics ADD COLUMN IF NOT EXISTS content_text text"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_topics_content_text_trgm
                      ON topics USING GIN (content_text gin_trgm_ops)"])

  ;; Topic files (binary storage, split from old documents.file_data)
  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS topic_files (
      id SERIAL PRIMARY KEY,
      topic_id INTEGER NOT NULL UNIQUE REFERENCES topics(id) ON DELETE CASCADE,
      file_data BYTEA NOT NULL,
      file_size INTEGER,
      mime_type TEXT
    )"])

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

  ;; Undo log — bounded per-user action history for the Undo feature.
  ;; entity_refs/snapshot are JSONB; occurred_at drives the 12h window,
  ;; undone_at IS NULL marks an entry still reversible. Hard-capped to the
  ;; most recent 100 live-or-not rows per user by prune-undo-log!.
  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS undo_log (
      id BIGSERIAL PRIMARY KEY,
      user_id BIGINT NOT NULL REFERENCES users(id) ON DELETE CASCADE,
      action_type TEXT NOT NULL CHECK (action_type IN
        ('delete-card','bulk-delete-cards','remove-pin','reset-prompt','delete-document')),
      entity_type TEXT NOT NULL CHECK (entity_type IN ('flashcard','pin','setting','document')),
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
                        ('delete-card','bulk-delete-cards','remove-pin','reset-prompt','delete-document'))"])
  (jdbc/execute! ds ["ALTER TABLE undo_log DROP CONSTRAINT IF EXISTS undo_log_entity_type_check"])
  (jdbc/execute! ds ["ALTER TABLE undo_log ADD CONSTRAINT undo_log_entity_type_check
                      CHECK (entity_type IN ('flashcard','pin','setting','document'))"])

  ;; Staged-delete marker: a topic (and its whole subtree) hidden pending the
  ;; 12h undo window points at the undo_log entry that staged it. ON DELETE
  ;; SET NULL — see prune/purge: only the time-based purge removes a
  ;; delete-document entry, and it hard-deletes the topics first.
  (jdbc/execute! ds ["ALTER TABLE topics ADD COLUMN IF NOT EXISTS staged_delete_id BIGINT
                      REFERENCES undo_log(id) ON DELETE SET NULL"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_topics_staged_delete
                      ON topics(staged_delete_id) WHERE staged_delete_id IS NOT NULL"])

  ;; One-time grandfather credit grant (idempotent; no-op when credits disabled)
  (run-grandfather-migration!)

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

(defn set-setting [user-id key value]
  (jdbc/execute! ds
    (sql/format {:insert-into :settings
                 :values [{:user_id user-id :key key :value value}]
                 :on-conflict [:user_id :key]
                 :do-update-set {:value value
                                 :updated_at [:now]}})))

(defn delete-setting [user-id key]
  (jdbc/execute! ds
    (sql/format {:delete-from :settings
                 :where [:and
                         [:= :user_id user-id]
                         [:= :key key]]})))

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

(defn upsert-google-user [google-id email username]
  (jdbc/execute-one! ds
    (sql/format {:insert-into :users
                 :values [{:google_id google-id :email email :username username}]
                 :on-conflict [:google_id]
                 :do-update-set {:email email}
                 :returning [:id :username]})))

(defn insert-user-event! [user-id event-type]
  (jdbc/execute! ds
    (sql/format {:insert-into :user_events
                 :values [{:user_id user-id :event_type event-type}]})))

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
              (:source-id attrs) (assoc :source_id (:source-id attrs)))]
    (jdbc/execute-one! ds
      (sql/format {:insert-into :topics
                   :values [row]
                   :returning [:*]}))))

(defn get-topic
  "Get a topic by ID. Excludes topics staged for deletion (hidden)."
  [id]
  (jdbc/execute-one! ds
    (sql/format {:select [:*]
                 :from [:topics]
                 :where [:and [:= :id id] [:is :staged_delete_id nil]]})))

(defn get-topic-for-user
  "Get a topic by ID, scoped to a user. Excludes topics staged for deletion."
  [user-id id]
  (jdbc/execute-one! ds
    (sql/format {:select [:*]
                 :from [:topics]
                 :where [:and [:= :id id] [:= :user_id user-id] [:is :staged_delete_id nil]]})))

(defn get-root-topics
  "Get all root topics for a user (parent_id IS NULL). Replaces get-documents.
   Includes file_size from topic_files or content length, plus bibliography
   fields joined from `sources` (NULL when topic has no source_id)."
  [user-id]
  (let [topics (jdbc/execute! ds
                 (sql/format {:select [:t/id :t/title :t/kind
                                       :s/url :s/title :s/csl_type :s/container_title
                                       :t/status :t/priority :t/created_at :t/content
                                       :tf/file_size]
                              :from [[:topics :t]]
                              :left-join [[:topic_files :tf] [:= :t/id :tf/topic_id]
                                          [:sources :s] [:= :t/source_id :s/id]]
                              :where [:and [:= :t/user_id user-id] [:= :t/parent_id nil]
                                      [:is :t/staged_delete_id nil]]
                              :order-by [[:t/created_at :desc]]}))]
    (mapv (fn [t]
            (let [size (or (:topic_files/file_size t)
                         (when-let [c (:topics/content t)]
                           (count (.getBytes c "UTF-8"))))]
              (-> t
                (assoc :topics/file_size size)
                (dissoc :topics/content :topic_files/file_size))))
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
  "Sum of file_size across the topic subtree rooted at id (including id).
   Returns 0 when nothing is found."
  [connectable id]
  (or (:sum (jdbc/execute-one! connectable
              ["WITH RECURSIVE subtree AS (
                  SELECT id FROM topics WHERE id = ?
                  UNION ALL
                  SELECT c.id FROM topics c
                  JOIN subtree s ON c.parent_id = s.id
                )
                SELECT COALESCE(SUM(tf.file_size), 0) AS sum
                FROM topic_files tf
                JOIN subtree s ON tf.topic_id = s.id"
               id]))
    0))

(defn delete-topic!
  "Delete a topic by ID. CASCADE handles children + flashcards.
   Decrements usage_bytes by the subtree's total file_size atomically.
   Looks up owner from the row before delete (so the counter belongs to the right user)."
  [id]
  (jdbc/with-transaction [tx ds]
    (let [owner (:users_id (jdbc/execute-one! tx
                             ["SELECT user_id AS users_id FROM topics WHERE id = ?" id]))
          freed (subtree-file-bytes tx id)
          result (jdbc/execute! tx
                   (sql/format {:delete-from :topics
                                :where [:= :id id]}))]
      (when (and owner (pos? freed))
        (bump-user-usage! tx owner (- freed)))
      result)))

(defn delete-topic-for-user!
  "Delete a topic scoped to user. Decrements usage_bytes by the subtree's
   total file_size atomically."
  [user-id id]
  (jdbc/with-transaction [tx ds]
    (let [freed (subtree-file-bytes tx id)
          result (jdbc/execute! tx
                   (sql/format {:delete-from :topics
                                :where [:and [:= :id id] [:= :user_id user-id]]}))]
      (when (pos? freed)
        (bump-user-usage! tx user-id (- freed)))
      result)))

(defn batch-create-topics!
  "Batch insert child topics under parent-id. items is a seq of maps with :content.
   Returns count of created items."
  [parent-id items]
  (when (seq items)
    (let [parent (get-topic parent-id)
          user-id (:topics/user_id parent)
          rows (mapv (fn [item]
                       (let [sanitized (sanitize-utf8 (:content item))]
                         {:user_id user-id
                          :parent_id parent-id
                          :kind "basic"
                          :title (let [raw (or (:content item) "")]
                                   (-> raw
                                     (str/replace #"<[^>]+>" "")
                                     (subs 0 (min 80 (count (str/replace raw #"<[^>]+>" ""))))
                                     str/trim
                                     (#(if (str/blank? %) "Extract" %))))
                          :content sanitized
                          :content_text (text/strip-html sanitized)
                          :status "active"
                          :priority 50}))
                 items)]
      (jdbc/execute! ds
        (sql/format {:insert-into :topics
                     :values rows}))
      (count items))))

;; ---------------------------------------------------------------------------
;; File operations
;; ---------------------------------------------------------------------------

(defn get-topic-file
  "Get the binary file data for a topic (PDF/EPUB)."
  [topic-id]
  (jdbc/execute-one! ds
    (sql/format {:select [:file_data :file_size :mime_type]
                 :from [:topic_files]
                 :where [:= :topic_id topic-id]})))

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
    (some-> row (update :sources/csl pgobject->clj))))

(defn attach-source-to-topic!
  "Set topics.source_id for a topic owned by user-id. Returns update count."
  [user-id topic-id source-id]
  (jdbc/execute-one! ds
    ["UPDATE topics SET source_id = ? WHERE id = ? AND user_id = ?"
     source-id topic-id user-id]))

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
               ["SELECT id, user_id, title FROM topics
                 WHERE kind = 'pdf' AND source_id IS NULL"]
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
          (doseq [n (range 1 (inc page-count))]
            (jdbc/execute! tx
              ["INSERT INTO topics (user_id, parent_id, kind, title, page_number)
                VALUES (?, ?, 'page', ?, ?)
                ON CONFLICT (parent_id, page_number) WHERE page_number IS NOT NULL
                DO NOTHING"
               user-id topic-id (str "Page " n) n])))
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
        topic))))

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
        ;; Create chapter children
        (let [chapter-ids (when (seq chapters)
                            (mapv (fn [i ch]
                                    (let [sanitized (sanitize-utf8 (:html ch))
                                          result (jdbc/execute-one! tx
                                                   (sql/format {:insert-into :topics
                                                                :values [{:user_id user-id
                                                                          :parent_id topic-id
                                                                          :kind "basic"
                                                                          :title (input/prettify-title
                                                                                   (or (:title ch) (str "Chapter " (inc i))))
                                                                          :content sanitized
                                                                          :content_text (text/strip-html sanitized)
                                                                          :page_number (inc i)
                                                                          :status "active"
                                                                          :priority 50}]
                                                                :returning [:id]}))]
                                      (:topics/id result)))
                              (range (count chapters))
                              chapters))]
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
    {:topic-id (:topics/id topic)}))

;; ---------------------------------------------------------------------------
;; Page-specific operations (kind='page' child topics)
;; ---------------------------------------------------------------------------

(defn create-page-stubs!
  "Batch-insert empty page topics for a parent. ON CONFLICT DO NOTHING."
  [parent-id page-count user-id]
  (when (pos? page-count)
    (doseq [n (range 1 (inc page-count))]
      (jdbc/execute! ds
        ["INSERT INTO topics (user_id, parent_id, kind, title, page_number)
          VALUES (?, ?, 'page', ?, ?)
          ON CONFLICT (parent_id, page_number) WHERE page_number IS NOT NULL
          DO NOTHING"
         user-id parent-id (str "Page " n) n]))))

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

(defn insert-flashcards!
  "Batch insert flashcards. Rows should include :topic_id and :root_topic_id.
   Uses ON CONFLICT DO NOTHING to prevent duplicates."
  [rows]
  (when (seq rows)
    (doseq [row rows]
      (input/check-length! :question (:question row) input/card-max)
      (input/check-length! :answer (:answer row) input/card-max)
      (input/check-length! :cloze (:cloze row) input/card-max))
    (jdbc/execute! ds
      (sql/format {:insert-into :flashcards
                   :values rows
                   :on-conflict []
                   :do-nothing true}))))

(defn get-flashcards
  "Get all flashcards for a specific topic (page or extract).
   Includes page_number from the topic hierarchy (direct or via parent)."
  [topic-id]
  (if topic-id
    (jdbc/execute! ds
      (sql/format {:select [[:f.*] [[:coalesce :t.page_number :parent.page_number] :page_number]]
                   :from [[:flashcards :f]]
                   :join [[:topics :t] [:= :f.topic_id :t.id]]
                   :left-join [[:topics :parent] [:= :t.parent_id :parent.id]]
                   :where [:= :f.topic_id topic-id]
                   :order-by [[:f.created_at :desc]]}))
    []))

(defn get-all-flashcards
  "Get all flashcards under a root topic.
   Includes page_number from the topic hierarchy (direct or via parent)."
  [root-topic-id]
  (jdbc/execute! ds
    (sql/format {:select [[:f.*] [[:coalesce :t.page_number :parent.page_number] :page_number]]
                 :from [[:flashcards :f]]
                 :join [[:topics :t] [:= :f.topic_id :t.id]]
                 :left-join [[:topics :parent] [:= :t.parent_id :parent.id]]
                 :where [:= :f.root_topic_id root-topic-id]
                 :order-by [[:f.created_at :desc]]})))

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
            (assoc :topic_title (:topics/topic_title row)
              :root_title (:topics/root_title row))
            (dissoc :topics/topic_title :topics/root_title)))
    (jdbc/execute! ds
      (sql/format {:select [[:f.*]
                            [[:coalesce :t.page_number :parent.page_number] :page_number]
                            [:t.title :topic_title]
                            [:root.title :root_title]
                            [[:to_char :f.created_at [:inline "Mon DD"]] :formatted_date]]
                   :from [[:flashcards :f]]
                   :join [[:topics :t] [:= :f.topic_id :t.id]
                          [:topics :root] [:= :f.root_topic_id :root.id]]
                   :left-join [[:topics :parent] [:= :t.parent_id :parent.id]]
                   :where [:= :root.user_id user-id]
                   :order-by [[:f.created_at :desc]]}))))

(defn get-flashcards-by-ids
  "Flashcards owned by user-id (via root topic) with ids in card-ids.
   Full rows plus :page_number — the inputs for bulk push (build-update-fields
   reads page_number for the source anchor) and bulk pull. Empty ids → []."
  [user-id card-ids]
  (if (empty? card-ids)
    []
    (jdbc/execute! ds
      (sql/format {:select [[:f.*]
                            [[:coalesce :t.page_number :parent.page_number] :page_number]]
                   :from [[:flashcards :f]]
                   :join [[:topics :t] [:= :f.topic_id :t.id]
                          [:topics :root] [:= :f.root_topic_id :root.id]]
                   :left-join [[:topics :parent] [:= :t.parent_id :parent.id]]
                   :where [:and
                           [:= :root.user_id user-id]
                           [:in :f.id (vec card-ids)]]}))))

(defn delete-user-flashcards!
  "Bulk delete of user-id's flashcards. Ownership enforced via the root
   topic join. Returns the full deleted rows (RETURNING f.*) so callers can
   both read :flashcards/anki_note_id and snapshot the rows for undo."
  [user-id card-ids]
  (if (empty? card-ids)
    []
    (jdbc/execute! ds
      (into [(str "DELETE FROM flashcards f USING topics root
                   WHERE f.root_topic_id = root.id AND root.user_id = ?
                     AND f.id IN (" (str/join "," (repeat (count card-ids) "?")) ")
                   RETURNING f.*")
             user-id]
        card-ids))))

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
  "Delete a single flashcard by ID. Returns the full deleted row (RETURNING *)
   so callers can read :flashcards/anki_note_id and snapshot it for undo."
  [card-id]
  (jdbc/execute-one! ds
    ["DELETE FROM flashcards WHERE id = ? RETURNING *" card-id]))

(defn update-flashcard!
  "Update flashcard content fields. Sets updated_at for sync tracking."
  [card-id fields]
  (jdbc/execute-one! ds
    (sql/format {:update :flashcards
                 :set (assoc fields :updated_at [:now])
                 :where [:= :id card-id]})))

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
  "Set anki_note_id and anki_synced_at for a flashcard."
  [card-id anki-note-id]
  (jdbc/execute-one! ds
    ["UPDATE flashcards SET anki_note_id = ?, anki_synced_at = CURRENT_TIMESTAMP WHERE id = ?"
     anki-note-id card-id]))

(defn set-anki-note-ids
  "Bulk set anki_note_id + anki_synced_at. Takes [[card-id note-id] ...]."
  [pairs]
  (doseq [[card-id note-id] pairs]
    (set-anki-note-id card-id note-id)))

(defn mark-anki-synced
  "Update anki_synced_at to now for a flashcard."
  [card-id]
  (jdbc/execute-one! ds
    ["UPDATE flashcards SET anki_synced_at = CURRENT_TIMESTAMP WHERE id = ?"
     card-id]))

(defn mark-cards-exported
  "Mark cards as exported (sets anki_synced_at). Used after CSV export."
  [card-ids]
  (when (seq card-ids)
    (jdbc/execute! ds
      (into [(str "UPDATE flashcards SET anki_synced_at = CURRENT_TIMESTAMP WHERE id IN ("
               (str/join "," (repeat (count card-ids) "?"))
               ")")]
        card-ids))))

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

(defn advance-topic!
  "Advance a topic's review schedule using A-Factor algorithm.
   Logs 'advance' event with pre-mutation snapshot."
  [id]
  (jdbc/with-transaction [tx ds]
    (when-let [before (snapshot-topic-row tx id)]
      (insert-repetition! tx id "advance" before)
      (jdbc/execute-one! tx
        ["UPDATE topics
          SET interval_days = COALESCE(interval_days, 1.0) * COALESCE(a_factor, 2.0),
              next_review_at = NOW() + (COALESCE(interval_days, 1.0) * COALESCE(a_factor, 2.0)) * INTERVAL '1 day',
              last_review_at = NOW()
          WHERE id = ?"
         id]))))

(defn update-topic-priority!
  "Update a topic's priority (0=highest, 100=lowest).
   Logs 'priority-change' event only when new value differs from current (no-op skipped)."
  [id priority]
  (jdbc/with-transaction [tx ds]
    (when-let [before (snapshot-topic-row tx id)]
      (when (not= (:priority before) priority)
        (insert-repetition! tx id "priority-change" before)
        (jdbc/execute-one! tx
          ["UPDATE topics SET priority = ? WHERE id = ?" priority id])))))

(defn postpone-topic!
  "Postpone a topic by N days without changing interval/a-factor.
   Logs 'postpone' event with pre-mutation snapshot."
  [id days]
  (jdbc/with-transaction [tx ds]
    (when-let [before (snapshot-topic-row tx id)]
      (insert-repetition! tx id "postpone" before)
      (jdbc/execute-one! tx
        ["UPDATE topics
          SET next_review_at = NOW() + ? * INTERVAL '1 day',
              last_review_at = NOW()
          WHERE id = ?"
         (double days) id]))))

(defn done-topic!
  "Mark a topic as done. Logs 'done' event with pre-mutation snapshot."
  [id]
  (jdbc/with-transaction [tx ds]
    (when-let [before (snapshot-topic-row tx id)]
      (insert-repetition! tx id "done" before)
      (jdbc/execute-one! tx
        ["UPDATE topics SET status = 'done' WHERE id = ?" id]))))

(defn restore-topic!
  "Restore a done topic back to active queue. Logs 'restore' event."
  [id]
  (jdbc/with-transaction [tx ds]
    (when-let [before (snapshot-topic-row tx id)]
      (insert-repetition! tx id "restore" before)
      (jdbc/execute-one! tx
        ["UPDATE topics SET status = 'active', next_review_at = NULL WHERE id = ?" id]))))

(defn touch-topic!
  "Update last_review_at without advancing the interval (subset-review soft rep).
   Logs 'touch' event with pre-mutation snapshot."
  [id]
  (jdbc/with-transaction [tx ds]
    (when-let [before (snapshot-topic-row tx id)]
      (insert-repetition! tx id "touch" before)
      (jdbc/execute-one! tx
        ["UPDATE topics SET last_review_at = NOW() WHERE id = ?" id]))))

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
   Joins `sources` to surface bibliography fields (source_url, source_title,
   source_csl_type, source_container); NULL when the topic has no source_id."
  [user-id]
  (try
    (jdbc/execute! ds
      ["SELECT t.id, t.parent_id, t.kind, t.title, t.priority,
              t.next_review_at, t.interval_days, t.a_factor,
              t.status,
              s.url, s.title, s.csl_type, s.container_title
       FROM topics t
       LEFT JOIN sources s ON s.id = t.source_id
       WHERE t.user_id = ?
         AND t.kind != 'page'
         AND t.staged_delete_id IS NULL
         AND (t.next_review_at::date <= CURRENT_DATE OR t.next_review_at IS NULL)
         AND (t.status = 'active' OR t.status IS NULL)
       ORDER BY t.priority ASC, t.next_review_at ASC NULLS FIRST, t.id ASC" user-id])
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
                     AND (status = 'active' OR status IS NULL)"
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
                     AND (status = 'active' OR status IS NULL)"
                  user-id])]
    (or (:total result) 0)))

(defn get-full-queue
  "All topics with scheduling info. No date filter.
   Joins `sources` to surface bibliography fields (source_url, source_title,
   source_csl_type, source_container); NULL when the topic has no source_id."
  [user-id]
  (try
    (jdbc/execute! ds
      ["SELECT t.id, t.parent_id, t.kind, t.title, t.priority, t.next_review_at,
              t.interval_days, t.status, t.content,
              s.url, s.title, s.csl_type, s.container_title
       FROM topics t
       LEFT JOIN sources s ON s.id = t.source_id
       WHERE t.user_id = ? AND t.kind != 'page' AND t.staged_delete_id IS NULL
       ORDER BY CASE WHEN t.status = 'active' OR t.status IS NULL THEN 0
                     WHEN t.status = 'done' THEN 1 ELSE 2 END,
                t.priority ASC, t.next_review_at ASC NULLS FIRST, t.id ASC" user-id])
    (catch Exception e
      (tel/error! {:id ::get-full-queue} e)
      [])))

(defn get-queue-summary
  "Aggregate queue stats in a single SQL query."
  [user-id]
  (try
    (let [result (jdbc/execute-one! ds
                   ["SELECT COUNT(*) AS total,
                           COUNT(*) FILTER (WHERE status = 'done') AS inactive,
                           COUNT(*) FILTER (WHERE (status = 'active' OR status IS NULL)
                                             AND (next_review_at IS NULL
                                                  OR next_review_at <= CURRENT_TIMESTAMP)) AS due_today,
                           COUNT(*) FILTER (WHERE (status = 'active' OR status IS NULL)
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
  "Topic counts per day for the next N days."
  [user-id days]
  (jdbc/execute! ds
    ["SELECT DATE(next_review_at) AS review_date, COUNT(*) AS count
      FROM topics
      WHERE user_id = ?
        AND kind != 'page'
        AND staged_delete_id IS NULL
        AND next_review_at BETWEEN NOW() AND NOW() + ? * INTERVAL '1 day'
        AND (status = 'active' OR status IS NULL)
      GROUP BY DATE(next_review_at)
      ORDER BY review_date"
     user-id days]))

(defn get-inactive-topics
  "Get all non-active (done) topics for a user."
  [user-id]
  (try
    (jdbc/execute! ds
      ["SELECT id, parent_id, kind, title, created_at, status
       FROM topics
       WHERE user_id = ?
         AND kind != 'page'
         AND staged_delete_id IS NULL
         AND status IN ('dismissed', 'done')
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
                          [:t.page_number :page_number] [:t.last_review_at :last_review_at]
                          :s.title :s.csl_type :s.container_title
                          [[:coalesce :tf.file_size [:octet_length [:coalesce :t.content ""]]] :file_size]
                          [[:to_char :t.created_at [:inline "Mon DD"]] :formatted_date]]
                 :from [[:topics :t]]
                 :left-join [[:topic_files :tf] [:= :tf.topic_id :t.id]
                             [:sources :s] [:= :t.source_id :s.id]]
                 :where [:and [:= :t.user_id user-id] [:is :t.staged_delete_id nil]]
                 :order-by [[:t.parent_id :asc-nulls-first] [:t.page_number :asc-nulls-first] [:t.created_at :asc]]})))

(defn get-root-topic-id
  "Walk up parent_id chain to find the root topic. Returns the root topic's ID."
  [topic-id]
  (let [result (jdbc/execute-one! ds
                 ["WITH RECURSIVE ancestors AS (
                     SELECT id, parent_id FROM topics WHERE id = ?
                     UNION ALL
                     SELECT t.id, t.parent_id FROM topics t
                     JOIN ancestors a ON t.id = a.parent_id
                   )
                   SELECT id FROM ancestors WHERE parent_id IS NULL"
                  topic-id])]
    (:id result)))

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
   Charges quota only on actual insert."
  [{:keys [user-id kind ^bytes bytes mime-type source-url]}]
  (let [sha256 (bytes-sha256 bytes)
        byte-size (alength bytes)]
    (if-let [existing (find-media-by-sha user-id sha256)]
      (:media/id existing)
      (jdbc/with-transaction [tx ds]
        (quota/check-and-bump! tx user-id byte-size)
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
  "Delete a pin row by id. Returns the full deleted row (RETURNING *) so the
   caller can snapshot it for undo."
  [pin-id]
  (jdbc/execute-one! ds
    (sql/format {:delete-from :topic_pins
                 :where [:= :id pin-id]
                 :returning [:*]})))

(defn update-pin-placement!
  "Update a pin's placement. Throws on invalid value."
  [pin-id new-placement]
  (when-not (#{"front" "back"} new-placement)
    (throw (ex-info "Invalid placement" {:placement new-placement})))
  (jdbc/execute-one! ds
    (sql/format {:update :topic_pins
                 :set {:placement new-placement}
                 :where [:= :id pin-id]})))

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
   so a stored snapshot is portable across the wire and back into an INSERT."
  [row]
  (update-keys row (comp keyword name)))

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
   ids. ON CONFLICT (id) DO NOTHING — a card still present is left untouched."
  [conn snapshot-rows]
  (when (seq snapshot-rows)
    (jdbc/execute! conn
      (sql/format {:insert-into :flashcards
                   :values (mapv #(cast-timestamps % flashcard-ts-cols) snapshot-rows)
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

(defn purge-staged-documents!
  "Hard-delete topic subtrees whose staging entry has aged past the 12h window,
   then drop those entries. CASCADE clears the binary + remaining rows.
   usage_bytes is untouched (already freed at stage time). Returns topics deleted."
  []
  (jdbc/with-transaction [tx ds]
    (let [deleted (jdbc/execute! tx
                    ["DELETE FROM topics
                      WHERE staged_delete_id IN
                        (SELECT id FROM undo_log
                         WHERE action_type = 'delete-document'
                           AND occurred_at < now() - interval '12 hours')"])
          n (or (some-> deleted first ::jdbc/update-count) 0)]
      (jdbc/execute! tx
        ["DELETE FROM undo_log
          WHERE action_type = 'delete-document'
            AND occurred_at < now() - interval '12 hours'"])
      n)))

(defonce ^:private purge-scheduler (atom nil))

(defn start-purge-scheduler!
  "Start the hourly staged-document purge. Idempotent — once per process."
  []
  (when-not @purge-scheduler
    (let [exec (java.util.concurrent.Executors/newSingleThreadScheduledExecutor)]
      (.scheduleAtFixedRate exec
        (fn [] (try (purge-staged-documents!)
                 (catch Throwable t (tel/error! {:id ::purge-staged-documents} t))))
        1 60 java.util.concurrent.TimeUnit/MINUTES)
      (reset! purge-scheduler exec)
      (tel/log! :info "Staged-document purge scheduler started"))))
