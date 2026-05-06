(ns freememo.db
  "Database connection and schema management for PostgreSQL.
   Unified topics model — all entities (documents, pages, extracts) are topics
   in a parent_id tree."
  (:require
   [next.jdbc :as jdbc]
   [next.jdbc.result-set :as rs]
   [honey.sql :as sql]
   [taoensso.telemere :as tel]
   [clojure.string :as str]
   [freememo.input-check :as input]
   [freememo.text :as text]))

(declare sanitize-utf8)
(declare migrate-to-topics!)
(declare backfill-content-text!)

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
  ;; Backfill `usage_bytes` once, when the column is first added.
  (let [had-usage? (some? (jdbc/execute-one! ds
                            ["SELECT 1 FROM information_schema.columns
                              WHERE table_name = 'users' AND column_name = 'usage_bytes'"]))]
    (jdbc/execute! ds ["ALTER TABLE users ADD COLUMN IF NOT EXISTS usage_bytes BIGINT NOT NULL DEFAULT 0"])
    (jdbc/execute! ds ["ALTER TABLE users ADD COLUMN IF NOT EXISTS quota_bytes BIGINT"])
    (when-not had-usage?
      (tel/log! :info "Backfilling users.usage_bytes from topic_files.file_size")
      (jdbc/execute! ds
        ["UPDATE users SET usage_bytes = COALESCE(
            (SELECT SUM(tf.file_size)
             FROM topic_files tf JOIN topics t ON tf.topic_id = t.id
             WHERE t.user_id = users.id), 0)"])))

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
      source_reference TEXT,
      status TEXT DEFAULT 'active',
      priority INTEGER DEFAULT 50,
      interval_days REAL DEFAULT 1.0,
      a_factor REAL DEFAULT 2.0,
      next_review_at TIMESTAMP,
      last_review_at TIMESTAMP,
      review_count INTEGER DEFAULT 0,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )"])

  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_topics_parent ON topics(parent_id)"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_topics_user ON topics(user_id)"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_topics_next_review ON topics(next_review_at)"])
  (jdbc/execute! ds ["CREATE UNIQUE INDEX IF NOT EXISTS idx_topics_page
                      ON topics(parent_id, page_number) WHERE page_number IS NOT NULL"])

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
  (jdbc/execute! ds ["ALTER TABLE flashcards ADD COLUMN IF NOT EXISTS source_reference TEXT"])
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

  ;; Backfill source_reference for markdown topics that lack it
  (jdbc/execute! ds ["UPDATE topics SET source_reference = title
                      WHERE kind = 'markdown' AND source_reference IS NULL"])

  ;; Backfill content_text for existing rows (idempotent)
  (backfill-content-text!)

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
        ["INSERT INTO topics (id, user_id, parent_id, kind, title, content, source_url, source_reference,
                              status, priority, interval_days, a_factor, next_review_at, last_review_at,
                              review_count, created_at)
          SELECT id, user_id, NULL,
                 CASE source_type WHEN 'topic' THEN 'basic' WHEN 'pdf' THEN 'pdf'
                                  WHEN 'epub' THEN 'epub' WHEN 'web' THEN 'web'
                                  ELSE COALESCE(source_type, 'basic') END,
                 COALESCE(filename, 'Untitled'), html_content, source_url, source_reference,
                 COALESCE(status, 'active'), COALESCE(priority, 50),
                 COALESCE(interval_days, 1.0), COALESCE(a_factor, 2.0),
                 next_review_at, last_review_at, COALESCE(review_count, 0), uploaded_at
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
                              p.last_review_at, p.review_count, p.created_at, d.user_id
                       FROM pages p JOIN documents d ON p.document_id = d.id
                       ORDER BY p.id"]
                    {:builder-fn rs/as-unqualified-maps})]
        (doseq [p pages]
          (let [new-topic (jdbc/execute-one! tx
                            ["INSERT INTO topics (user_id, parent_id, kind, title, content, page_number,
                                                  status, priority, interval_days, a_factor,
                                                  next_review_at, last_review_at, review_count, created_at)
                              VALUES (?, ?, 'page', ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
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
                             (or (:review_count p) 0)
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
                            ci.next_review_at, ci.last_review_at, ci.review_count,
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
                                                   :review_count (or (:review_count ci) 0)
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
                            ci.last_review_at, ci.review_count, ci.created_at, d.user_id
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
                                                   :review_count (or (:review_count ci) 0)
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
   :source-url :source-reference :status :priority
   Returns the created row with :topics/id."
  [attrs]
  (input/check-length! :title (:title attrs) input/title-max)
  (let [sanitized (when (:content attrs) (sanitize-utf8 (:content attrs)))
        row (cond-> {:kind (or (:kind attrs) "basic")
                     :title (or (:title attrs) "Untitled")
                     :status (or (:status attrs) "active")
                     :priority (or (:priority attrs) 50)}
              (:user-id attrs) (assoc :user_id (:user-id attrs))
              (:parent-id attrs) (assoc :parent_id (:parent-id attrs))
              sanitized (assoc :content sanitized
                          :content_text (text/strip-html sanitized))
              (:page-number attrs) (assoc :page_number (:page-number attrs))
              (:source-url attrs) (assoc :source_url (:source-url attrs))
              (:source-reference attrs) (assoc :source_reference (:source-reference attrs)))]
    (jdbc/execute-one! ds
      (sql/format {:insert-into :topics
                   :values [row]
                   :returning [:*]}))))

(defn get-topic
  "Get a topic by ID."
  [id]
  (jdbc/execute-one! ds
    (sql/format {:select [:*]
                 :from [:topics]
                 :where [:= :id id]})))

(defn get-topic-for-user
  "Get a topic by ID, scoped to a user (checks user_id on the root)."
  [user-id id]
  (jdbc/execute-one! ds
    (sql/format {:select [:*]
                 :from [:topics]
                 :where [:and [:= :id id] [:= :user_id user-id]]})))

(defn get-root-topics
  "Get all root topics for a user (parent_id IS NULL). Replaces get-documents.
   Includes file_size from topic_files or content length."
  [user-id]
  (let [topics (jdbc/execute! ds
                 (sql/format {:select [:t/id :t/title :t/kind :t/source_url :t/source_reference
                                       :t/status :t/priority :t/created_at :t/content
                                       :tf/file_size]
                              :from [[:topics :t]]
                              :left-join [[:topic_files :tf] [:= :t/id :tf/topic_id]]
                              :where [:and [:= :t/user_id user-id] [:= :t/parent_id nil]]
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
                 :where [:= :parent_id parent-id]
                 :order-by [[:page_number :asc-nulls-last] [:created_at :asc]]})))

(defn update-topic-content!
  "Update the content of a topic."
  [id content]
  (let [sanitized (sanitize-utf8 content)]
    (jdbc/execute-one! ds
      (sql/format {:update :topics
                   :set {:content sanitized
                         :content_text (text/strip-html sanitized)}
                   :where [:= :id id]}))))

(defn update-topic-source!
  "Update source_reference on a topic.
   Also bumps updated_at on all flashcards under this root topic
   so Anki push detects the source change."
  [id source-reference]
  (jdbc/execute-one! ds
    (sql/format {:update :topics
                 :set {:source_reference source-reference}
                 :where [:= :id id]}))
  (jdbc/execute-one! ds
    ["UPDATE flashcards SET updated_at = CURRENT_TIMESTAMP WHERE root_topic_id = ?" id]))

(defn get-topic-source
  "Get the source_reference for a topic."
  [id]
  (:topics/source_reference
   (jdbc/execute-one! ds
     (sql/format {:select [:source_reference]
                  :from [:topics]
                  :where [:= :id id]}))))

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
;; Compound creation helpers
;; ---------------------------------------------------------------------------

(defn create-pdf-topic!
  "Create a PDF root topic with file data and page stubs.
   Increments users.usage_bytes by file-size atomically.
   Returns the result with :topics/id."
  [user-id filename file-bytes file-size page-count]
  (input/check-length! :title filename input/title-max)
  (jdbc/with-transaction [tx ds]
    (let [topic (jdbc/execute-one! tx
                  (sql/format {:insert-into :topics
                               :values [{:user_id user-id
                                         :kind "pdf"
                                         :title filename
                                         :source_reference filename}]
                               :returning [:id]}))
          topic-id (:topics/id topic)]
      ;; Store file
      (jdbc/execute! tx
        ["INSERT INTO topic_files (topic_id, file_data, file_size, mime_type)
          VALUES (?, ?, ?, ?)"
         topic-id file-bytes file-size "application/pdf"])
      (bump-user-usage! tx user-id file-size)
      ;; Create page stubs
      (when (pos? page-count)
        (doseq [n (range 1 (inc page-count))]
          (jdbc/execute! tx
            ["INSERT INTO topics (user_id, parent_id, kind, title, page_number)
              VALUES (?, ?, 'page', ?, ?)
              ON CONFLICT (parent_id, page_number) WHERE page_number IS NOT NULL
              DO NOTHING"
             user-id topic-id (str "Page " n) n])))
      topic)))

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

(defn create-web-topic!
  "Create a web article topic. Returns topic-id."
  [user-id title html-content source-url]
  (input/check-length! :title title input/title-max)
  (let [source-ref (str title (when source-url (str " — " source-url)))
        sanitized (sanitize-utf8 html-content)
        topic (jdbc/execute-one! ds
                (sql/format {:insert-into :topics
                             :values [{:user_id user-id
                                       :kind "web"
                                       :title (or title "Web Article")
                                       :content sanitized
                                       :content_text (text/strip-html sanitized)
                                       :source_url source-url
                                       :source_reference source-ref}]
                             :returning [:id]}))]
    (:topics/id topic)))

(defn create-markdown-topic!
  "Create a Markdown-imported topic. Returns topic-id."
  [user-id title html-content]
  (input/check-length! :title title input/title-max)
  (let [sanitized (sanitize-utf8 html-content)
        topic (jdbc/execute-one! ds
                (sql/format {:insert-into :topics
                             :values [{:user_id user-id
                                       :kind "markdown"
                                       :title (or title "Untitled Markdown")
                                       :content sanitized
                                       :content_text (text/strip-html sanitized)
                                       :source_reference (or title "Untitled Markdown")}]
                             :returning [:id]}))]
    (:topics/id topic)))

(defn create-epub-topic!
  "Create an EPUB root topic with file and chapter children.
   chapters is a vec of {:html :title} maps.
   Increments users.usage_bytes by file-size atomically.
   Returns {:topic-id N :chapter-ids [...]}"
  [user-id title file-bytes file-size chapters]
  (input/check-length! :title title input/title-max)
  (jdbc/with-transaction [tx ds]
    (let [topic (jdbc/execute-one! tx
                  (sql/format {:insert-into :topics
                               :values [{:user_id user-id
                                         :kind "epub"
                                         :title (or title "Untitled EPUB")
                                         :source_reference (or title "Untitled EPUB")}]
                               :returning [:id]}))
          topic-id (:topics/id topic)]
      ;; Store file
      (jdbc/execute! tx
        ["INSERT INTO topic_files (topic_id, file_data, file_size, mime_type)
          VALUES (?, ?, ?, ?)"
         topic-id file-bytes file-size "application/epub+zip"])
      (bump-user-usage! tx user-id file-size)
      ;; Create chapter children
      (let [chapter-ids (when (seq chapters)
                          (mapv (fn [i ch]
                                  (let [sanitized (sanitize-utf8 (:html ch))
                                        result (jdbc/execute-one! tx
                                                 (sql/format {:insert-into :topics
                                                              :values [{:user_id user-id
                                                                        :parent_id topic-id
                                                                        :kind "basic"
                                                                        :title (or (:title ch) (str "Chapter " (inc i)))
                                                                        :content sanitized
                                                                        :content_text (text/strip-html sanitized)
                                                                        :page_number (inc i)
                                                                        :status "active"
                                                                        :priority 50}]
                                                              :returning [:id]}))]
                                    (:topics/id result)))
                            (range (count chapters))
                            chapters))]
        {:topic-id topic-id :chapter-ids (vec (remove nil? chapter-ids))}))))

(defn create-standalone-topic!
  "Create a standalone empty topic. Returns {:topic-id N}."
  [user-id title]
  (input/check-length! :title title input/title-max)
  (let [topic (jdbc/execute-one! ds
                (sql/format {:insert-into :topics
                             :values [{:user_id user-id
                                       :kind "basic"
                                       :title (or title "New Topic")
                                       :content ""
                                       :content_text ""
                                       :source_reference (or title "New Topic")}]
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
                 :where [:and [:= :parent_id parent-id] [:= :kind "page"]]
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

(defn delete-flashcard!
  "Delete a single flashcard by ID. Returns the deleted row."
  [card-id]
  (jdbc/execute-one! ds
    ["DELETE FROM flashcards WHERE id = ? RETURNING id, anki_note_id" card-id]))

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
   :total_items, :done_items, :total_cards, :synced_cards."
  [user-id]
  (jdbc/execute! ds
    ["SELECT t.id, t.title, t.kind, t.created_at,
             CASE WHEN t.kind = 'pdf' THEN COALESCE(ps.total_pages, 0)
                  ELSE COALESCE(ds.total_items, 0)
             END AS total_items,
             CASE WHEN t.kind = 'pdf' THEN COALESCE(ps.done_pages, 0)
                  ELSE COALESCE(ds.done_items, 0)
             END AS done_items,
             COALESCE(cs.total_cards, 0)  AS total_cards,
             COALESCE(cs.synced_cards, 0) AS synced_cards
      FROM topics t
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
      WHERE t.user_id = ? AND t.parent_id IS NULL
      ORDER BY t.created_at DESC"
     user-id]))

;; ---------------------------------------------------------------------------
;; Scheduling (unified — no topic-type dispatch)
;; ---------------------------------------------------------------------------

(defn advance-topic!
  "Advance a topic's review schedule using A-Factor algorithm."
  [id]
  (jdbc/execute-one! ds
    ["UPDATE topics
      SET interval_days = COALESCE(interval_days, 1.0) * COALESCE(a_factor, 2.0),
          next_review_at = NOW() + (COALESCE(interval_days, 1.0) * COALESCE(a_factor, 2.0)) * INTERVAL '1 day',
          last_review_at = NOW(),
          review_count = COALESCE(review_count, 0) + 1
      WHERE id = ?"
     id]))

(defn update-topic-priority!
  "Update a topic's priority (0=highest, 100=lowest)."
  [id priority]
  (jdbc/execute-one! ds
    ["UPDATE topics SET priority = ? WHERE id = ?" priority id]))

(defn postpone-topic!
  "Postpone a topic by N days without changing interval/a-factor."
  [id days]
  (jdbc/execute-one! ds
    ["UPDATE topics
      SET next_review_at = NOW() + ? * INTERVAL '1 day',
          last_review_at = NOW()
      WHERE id = ?"
     (double days) id]))

(defn done-topic!
  "Mark a topic as done."
  [id]
  (jdbc/execute-one! ds
    ["UPDATE topics SET status = 'done' WHERE id = ?" id]))

(defn restore-topic!
  "Restore a done topic back to active queue."
  [id]
  (jdbc/execute-one! ds
    ["UPDATE topics SET status = 'active', next_review_at = NULL WHERE id = ?" id]))

(defn touch-topic!
  "Update last_review_at without advancing the interval."
  [id]
  (jdbc/execute-one! ds
    ["UPDATE topics
      SET last_review_at = NOW(), review_count = COALESCE(review_count, 0) + 1
      WHERE id = ?"
     id]))

;; ---------------------------------------------------------------------------
;; Queue queries (single SELECT, no UNION ALL)
;; ---------------------------------------------------------------------------

(defn get-learning-queue
  "Due topics for incremental reading. Single query, no UNION."
  [user-id]
  (try
    (jdbc/execute! ds
      ["SELECT id, parent_id, kind, title, content, priority, next_review_at,
              interval_days, a_factor, review_count, status, source_url, source_reference
       FROM topics
       WHERE user_id = ?
         AND kind != 'page'
         AND (next_review_at::date <= CURRENT_DATE OR next_review_at IS NULL)
         AND (status = 'active' OR status IS NULL)
       ORDER BY priority ASC, next_review_at ASC NULLS FIRST, id ASC" user-id])
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
                     AND (status = 'active' OR status IS NULL)"
                  user-id])]
    (or (:total result) 0)))

(defn get-full-queue
  "All topics with scheduling info. No date filter."
  [user-id]
  (try
    (jdbc/execute! ds
      ["SELECT id, parent_id, kind, title, priority, next_review_at,
              interval_days, status, content, source_url
       FROM topics
       WHERE user_id = ? AND kind != 'page'
       ORDER BY CASE WHEN status = 'active' OR status IS NULL THEN 0
                     WHEN status = 'done' THEN 1 ELSE 2 END,
                priority ASC, next_review_at ASC NULLS FIRST, id ASC" user-id])
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
                    WHERE user_id = ? AND kind != 'page'" user-id])]
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
   Includes page topics so parent chain (PDF → page → extract) is intact."
  [user-id]
  (jdbc/execute! ds
    (sql/format {:select [[:t.id :id] [:t.parent_id :parent_id] [:t.title :title]
                          [:t.kind :kind] [:t.status :status] [:t.created_at :created_at]
                          [:t.page_number :page_number] [:t.last_review_at :last_review_at]
                          [[:coalesce :tf.file_size [:octet_length [:coalesce :t.content ""]]] :file_size]
                          [[:to_char :t.created_at [:inline "Mon DD"]] :formatted_date]]
                 :from [[:topics :t]]
                 :left-join [[:topic_files :tf] [:= :tf.topic_id :t.id]]
                 :where [:= :t.user_id user-id]
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
      WHERE t.user_id = ? OR t.user_id IS NULL
      ORDER BY t.parent_id ASC NULLS FIRST, t.page_number ASC NULLS LAST, t.id ASC"
     root-id user-id]))

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
      (mapv (fn [item]
              (let [nra (:topics/next_review_at item)]
                (assoc item :outstanding?
                  (or (nil? nra)
                    (.before nra now)))))
        ordered))))
