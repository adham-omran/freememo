(ns electric-starter-app.db
  "Database connection and schema management for PostgreSQL."
  (:require
    [next.jdbc :as jdbc]
    [honey.sql :as sql]
    [clojure.string :as str]))

;; Connection configuration
(def db-config
  {:dbtype   "postgresql"
   :host     (or (System/getenv "DB_HOST") "localhost")
   :port     (Integer/parseInt (or (System/getenv "DB_PORT") "5432"))
   :dbname   (or (System/getenv "DB_NAME") "cardmaker")
   :user     (or (System/getenv "DB_USER") "cardmaker")
   :password (or (System/getenv "DB_PASSWORD") "dev")})

;; HikariCP datasource (connection pool)
(defonce ds (jdbc/get-datasource db-config))

;; Schema setup
(defn setup-schema []
  (println "Setting up database schema...")

  ;; Enable pgcrypto for password hashing
  (jdbc/execute! ds ["CREATE EXTENSION IF NOT EXISTS pgcrypto"])

  ;; Create users table (before settings, since settings references it)
  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS users (
      id SERIAL PRIMARY KEY,
      username TEXT NOT NULL UNIQUE,
      password_hash TEXT NOT NULL,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )"])

  ;; Google OAuth migrations: add google_id and email columns, allow NULL password_hash
  (jdbc/execute! ds ["ALTER TABLE users ADD COLUMN IF NOT EXISTS google_id TEXT UNIQUE"])
  (jdbc/execute! ds ["ALTER TABLE users ADD COLUMN IF NOT EXISTS email TEXT"])
  (jdbc/execute! ds ["ALTER TABLE users ALTER COLUMN password_hash DROP NOT NULL"])

  ;; Migrate settings: drop old global table, recreate with user_id
  ;; Check if settings table exists and lacks user_id column
  (let [has-user-id (jdbc/execute-one! ds
                      ["SELECT column_name FROM information_schema.columns
                        WHERE table_name = 'settings' AND column_name = 'user_id'"])]
    (when-not has-user-id
      (println "Migrating settings table to per-user...")
      (jdbc/execute! ds ["DROP TABLE IF EXISTS settings"])
      (jdbc/execute! ds ["
        CREATE TABLE IF NOT EXISTS settings (
          user_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,
          key TEXT NOT NULL,
          value TEXT NOT NULL,
          updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
          PRIMARY KEY (user_id, key)
        )"])))

  ;; Create documents table
  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS documents (
      id SERIAL PRIMARY KEY,
      filename TEXT NOT NULL,
      file_data BYTEA NOT NULL,
      file_size INTEGER NOT NULL,
      mime_type TEXT NOT NULL,
      uploaded_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )"])

  ;; Add user_id column to documents (nullable — existing orphaned docs survive)
  (jdbc/execute! ds ["ALTER TABLE documents ADD COLUMN IF NOT EXISTS user_id INTEGER REFERENCES users(id) ON DELETE SET NULL"])

  ;; Web import: extend documents for non-PDF content
  (jdbc/execute! ds ["ALTER TABLE documents ALTER COLUMN file_data DROP NOT NULL"])
  (jdbc/execute! ds ["ALTER TABLE documents ALTER COLUMN file_size DROP NOT NULL"])
  (jdbc/execute! ds ["ALTER TABLE documents ALTER COLUMN mime_type DROP NOT NULL"])
  (jdbc/execute! ds ["ALTER TABLE documents ADD COLUMN IF NOT EXISTS source_type TEXT DEFAULT 'pdf'"])
  (jdbc/execute! ds ["ALTER TABLE documents ADD COLUMN IF NOT EXISTS source_url TEXT"])
  (jdbc/execute! ds ["ALTER TABLE documents ADD COLUMN IF NOT EXISTS html_content TEXT"])

  ;; Create pages table for OCR text storage
  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS pages (
      id SERIAL PRIMARY KEY,
      document_id INTEGER NOT NULL REFERENCES documents(id) ON DELETE CASCADE,
      page_number INTEGER NOT NULL,
      text TEXT,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      UNIQUE(document_id, page_number)
    )"])

  ;; Add is_done column to pages table (migration)
  (jdbc/execute! ds ["ALTER TABLE pages ADD COLUMN IF NOT EXISTS is_done BOOLEAN DEFAULT NULL"])

  ;; Add priority column to pages table (migration)
  (jdbc/execute! ds ["ALTER TABLE pages ADD COLUMN IF NOT EXISTS priority INTEGER DEFAULT 50"])

  ;; Create flashcards table
  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS flashcards (
      id SERIAL PRIMARY KEY,
      document_id INTEGER NOT NULL REFERENCES documents(id) ON DELETE CASCADE,
      page_number INTEGER NOT NULL,
      kind TEXT NOT NULL,
      question TEXT,
      answer TEXT,
      cloze TEXT,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      UNIQUE(document_id, page_number, kind, question, cloze)
    )"])

  (jdbc/execute! ds ["
    CREATE INDEX IF NOT EXISTS idx_flashcards_document_page
      ON flashcards(document_id, page_number)"])

  ;; Create content_items table (must exist before flashcards FK reference)
  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS content_items (
      id SERIAL PRIMARY KEY,
      document_id INTEGER NOT NULL REFERENCES documents(id) ON DELETE CASCADE,
      page_number INTEGER NOT NULL,
      kind TEXT NOT NULL DEFAULT 'html',
      content TEXT NOT NULL,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_content_items_document_page
                      ON content_items(document_id, page_number)"])

  ;; Link flashcards to their parent extract (NULL = page-level card from OcrPage)
  (jdbc/execute! ds ["ALTER TABLE flashcards ADD COLUMN IF NOT EXISTS content_item_id INTEGER
                      REFERENCES content_items(id) ON DELETE CASCADE"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_flashcards_content_item ON flashcards(content_item_id)"])

  ;; Source reference columns
  (jdbc/execute! ds ["ALTER TABLE documents ADD COLUMN IF NOT EXISTS source_reference TEXT"])
  (jdbc/execute! ds ["ALTER TABLE flashcards ADD COLUMN IF NOT EXISTS source_reference TEXT"])

  ;; Backfill source_reference on documents from filename + URL
  (jdbc/execute! ds ["UPDATE documents
                      SET source_reference = filename || CASE WHEN source_url IS NOT NULL THEN ' — ' || source_url ELSE '' END
                      WHERE source_reference IS NULL OR source_reference = ''"])
  ;; Backfill source_reference on existing cards from their parent document
  (jdbc/execute! ds ["UPDATE flashcards f
                      SET source_reference = d.source_reference
                      FROM documents d
                      WHERE f.document_id = d.id
                        AND (f.source_reference IS NULL OR f.source_reference = '')
                        AND d.source_reference IS NOT NULL
                        AND d.source_reference != ''"])

  ;; Anki sync columns on flashcards
  (jdbc/execute! ds ["ALTER TABLE flashcards ADD COLUMN IF NOT EXISTS anki_note_id BIGINT DEFAULT NULL"])
  (jdbc/execute! ds ["ALTER TABLE flashcards ADD COLUMN IF NOT EXISTS anki_synced_at TIMESTAMP DEFAULT NULL"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_flashcards_anki_note_id ON flashcards(anki_note_id) WHERE anki_note_id IS NOT NULL"])

  ;; Track card edits for sync-after-edit detection
  (jdbc/execute! ds ["ALTER TABLE flashcards ADD COLUMN IF NOT EXISTS updated_at TIMESTAMP DEFAULT NULL"])

  ;; Scheduling columns for incremental reading (pages)
  (jdbc/execute! ds ["ALTER TABLE pages ADD COLUMN IF NOT EXISTS interval_days REAL DEFAULT 1.0"])
  (jdbc/execute! ds ["ALTER TABLE pages ADD COLUMN IF NOT EXISTS a_factor REAL DEFAULT 2.0"])
  (jdbc/execute! ds ["ALTER TABLE pages ADD COLUMN IF NOT EXISTS next_review_at TIMESTAMP DEFAULT NULL"])
  (jdbc/execute! ds ["ALTER TABLE pages ADD COLUMN IF NOT EXISTS last_review_at TIMESTAMP DEFAULT NULL"])
  (jdbc/execute! ds ["ALTER TABLE pages ADD COLUMN IF NOT EXISTS review_count INTEGER DEFAULT 0"])

  ;; Scheduling columns for incremental reading (content_items)
  (jdbc/execute! ds ["ALTER TABLE content_items ADD COLUMN IF NOT EXISTS priority INTEGER DEFAULT 50"])
  (jdbc/execute! ds ["ALTER TABLE content_items ADD COLUMN IF NOT EXISTS interval_days REAL DEFAULT 1.0"])
  (jdbc/execute! ds ["ALTER TABLE content_items ADD COLUMN IF NOT EXISTS a_factor REAL DEFAULT 2.0"])
  (jdbc/execute! ds ["ALTER TABLE content_items ADD COLUMN IF NOT EXISTS next_review_at TIMESTAMP DEFAULT NULL"])
  (jdbc/execute! ds ["ALTER TABLE content_items ADD COLUMN IF NOT EXISTS last_review_at TIMESTAMP DEFAULT NULL"])
  (jdbc/execute! ds ["ALTER TABLE content_items ADD COLUMN IF NOT EXISTS review_count INTEGER DEFAULT 0"])

  ;; Parent-child hierarchy for extract-from-extract (SuperMemo-style)
  (jdbc/execute! ds ["ALTER TABLE content_items ADD COLUMN IF NOT EXISTS parent_content_item_id INTEGER
                      REFERENCES content_items(id) ON DELETE CASCADE"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_content_items_parent ON content_items(parent_content_item_id)"])

  ;; Dismissed flag for removing topics from queue without deleting (legacy)
  (jdbc/execute! ds ["ALTER TABLE documents ADD COLUMN IF NOT EXISTS dismissed BOOLEAN DEFAULT false"])
  (jdbc/execute! ds ["ALTER TABLE content_items ADD COLUMN IF NOT EXISTS dismissed BOOLEAN DEFAULT false"])

  ;; Status column: 'active', 'done', 'dismissed' (replaces boolean dismissed)
  (jdbc/execute! ds ["ALTER TABLE documents ADD COLUMN IF NOT EXISTS status TEXT DEFAULT 'active'"])
  (jdbc/execute! ds ["ALTER TABLE content_items ADD COLUMN IF NOT EXISTS status TEXT DEFAULT 'active'"])
  ;; Backfill from legacy dismissed column
  (jdbc/execute! ds ["UPDATE documents SET status = 'dismissed' WHERE dismissed = true AND (status IS NULL OR status = 'active')"])
  (jdbc/execute! ds ["UPDATE content_items SET status = 'dismissed' WHERE dismissed = true AND (status IS NULL OR status = 'active')"])

  ;; Indexes for review queue queries
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_pages_next_review ON pages(next_review_at)"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_content_items_next_review ON content_items(next_review_at)"])

  ;; Scheduling columns for documents (incremental reading at document level)
  (jdbc/execute! ds ["ALTER TABLE documents ADD COLUMN IF NOT EXISTS priority INTEGER DEFAULT 50"])
  (jdbc/execute! ds ["ALTER TABLE documents ADD COLUMN IF NOT EXISTS interval_days REAL DEFAULT 1.0"])
  (jdbc/execute! ds ["ALTER TABLE documents ADD COLUMN IF NOT EXISTS a_factor REAL DEFAULT 2.0"])
  (jdbc/execute! ds ["ALTER TABLE documents ADD COLUMN IF NOT EXISTS next_review_at TIMESTAMP DEFAULT NULL"])
  (jdbc/execute! ds ["ALTER TABLE documents ADD COLUMN IF NOT EXISTS last_review_at TIMESTAMP DEFAULT NULL"])
  (jdbc/execute! ds ["ALTER TABLE documents ADD COLUMN IF NOT EXISTS review_count INTEGER DEFAULT 0"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_documents_next_review ON documents(next_review_at)"])

  (println "Database ready."))

;; Query functions
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

;; Document queries
(defn save-document [user-id filename file-bytes file-size mime-type]
  (jdbc/execute! ds
    (sql/format {:insert-into :documents
                 :values [{:user_id user-id
                           :filename filename
                           :file_data file-bytes
                           :file_size file-size
                           :mime_type mime-type
                           :source_reference filename}]
                 :returning [:id]})))

(defn save-web-document
  "Save a web article as a document with a single page containing the HTML."
  [user-id title html-content source-type source-url]
  (let [source-ref (str title (when source-url (str " — " source-url)))
        doc (first (jdbc/execute! ds
                     (sql/format {:insert-into :documents
                                  :values [{:user_id user-id
                                            :filename title
                                            :source_type source-type
                                            :source_url source-url
                                            :html_content html-content
                                            :source_reference source-ref}]
                                  :returning [:id]})))
        doc-id (:documents/id doc)]
    (when doc-id
      ;; Create single page with the HTML content
      (jdbc/execute! ds
        (sql/format {:insert-into :pages
                     :values [{:document_id doc-id
                               :page_number 1
                               :text html-content}]
                     :on-conflict [:document_id :page_number]
                     :do-nothing true})))
    doc-id))

(defn get-documents [user-id]
  (let [docs (jdbc/execute! ds
               (sql/format {:select [:id :filename :file_size :uploaded_at :source_type :html_content :status]
                            :from [:documents]
                            :where [:= :user_id user-id]
                            :order-by [[:uploaded_at :desc]]}))]
    (mapv (fn [d]
            (let [size (or (:documents/file_size d)
                           (when-let [html (:documents/html_content d)]
                             (count (.getBytes html "UTF-8"))))]
              (-> d
                  (assoc :documents/file_size size)
                  (dissoc :documents/html_content))))
          docs)))

(defn delete-document [user-id id]
  (jdbc/execute! ds
    (sql/format {:delete-from :documents
                 :where [:and
                         [:= :id id]
                         [:= :user_id user-id]]})))

(defn get-documents-by-id
  "Retrieve a document by its ID, scoped to user."
  [user-id id]
  (jdbc/execute! ds
    (sql/format {:select [:*]
                 :from [:documents]
                 :where [:and
                         [:= :id id]
                         [:= :user_id user-id]]})))

(defn update-document-source [document-id source-reference]
  (jdbc/execute-one! ds
    (sql/format {:update :documents
                 :set {:source_reference source-reference}
                 :where [:= :id document-id]})))

(defn get-document-source [document-id]
  (:documents/source_reference
    (jdbc/execute-one! ds
      (sql/format {:select [:source_reference]
                   :from [:documents]
                   :where [:= :id document-id]}))))

;; Page queries
(defn save-page-text
  "Save or update OCR text for a specific page of a document.
   Uses UPSERT pattern (ON CONFLICT DO UPDATE)."
  [document-id page-number text]
  (jdbc/execute! ds
    ["INSERT INTO pages (document_id, page_number, text, updated_at)
      VALUES (?, ?, ?, CURRENT_TIMESTAMP)
      ON CONFLICT (document_id, page_number)
      DO UPDATE SET text = excluded.text, updated_at = CURRENT_TIMESTAMP"
     document-id page-number text]))

(defn create-page-stubs
  "Batch-insert empty page rows for a newly uploaded document.
   Uses ON CONFLICT DO NOTHING so existing rows (from OCR) are never overwritten."
  [document-id page-count]
  (when (pos? page-count)
    (jdbc/execute! ds
      (sql/format {:insert-into :pages
                   :columns [:document_id :page_number]
                   :values (mapv (fn [n] [document-id n]) (range 1 (inc page-count)))
                   :on-conflict [:document_id :page_number]
                   :do-nothing true}))))

(defn get-page-text
  "Retrieve OCR text for a specific page."
  [document-id page-number]
  (jdbc/execute-one! ds
    (sql/format {:select [:*]
                 :from [:pages]
                 :where [:and
                         [:= :document_id document-id]
                         [:= :page_number page-number]]})))

(defn list-pages
  "List all pages with OCR text for a document."
  [document-id]
  (jdbc/execute! ds
    (sql/format {:select [:*]
                 :from [:pages]
                 :where [:= :document_id document-id]
                 :order-by [[:page_number :asc]]})))

(defn get-page-done-status
  "Get the is_done status for a specific page. Returns boolean."
  [document-id page-number]
  (let [result (jdbc/execute-one! ds
                 (sql/format {:select [:is_done]
                              :from [:pages]
                              :where [:and
                                      [:= :document_id document-id]
                                      [:= :page_number page-number]]}))]
    (if result
      (or (:pages/is_done result) false)  ; NULL or false -> false, true -> true
      false)))  ; Page doesn't exist yet -> false

(defn get-page-priority
  "Get the priority for a specific page. Returns integer (default 50)."
  [document-id page-number]
  (let [result (jdbc/execute-one! ds
                 (sql/format {:select [:priority]
                              :from [:pages]
                              :where [:and
                                      [:= :document_id document-id]
                                      [:= :page_number page-number]]}))]
    (or (:pages/priority result) 50)))

(defn set-page-priority
  "Set the priority for a specific page (0=highest, 100=lowest).
   Upserts the row so it works even without extracted text."
  [document-id page-number priority]
  (jdbc/execute! ds
    ["INSERT INTO pages (document_id, page_number, priority, updated_at)
      VALUES (?, ?, ?, CURRENT_TIMESTAMP)
      ON CONFLICT (document_id, page_number)
      DO UPDATE SET priority = excluded.priority, updated_at = CURRENT_TIMESTAMP"
     document-id page-number priority]))

(defn get-reading-queue
  "Returns all non-done pages for a user's documents, sorted by priority."
  [user-id]
  (jdbc/execute! ds
    (sql/format {:select [:p/document_id :p/page_number :p/priority :d/filename]
                 :from [[:pages :p]]
                 :join [[:documents :d] [:= :p/document_id :d/id]]
                 :where [:and [:= :d/user_id user-id]
                               [:or [:= :p/is_done false] [:= :p/is_done nil]]]
                 :order-by [[:p/priority :asc] [:d/id :asc] [:p/page_number :asc]]})))

(defn toggle-page-done
  "Toggle the is_done status for a specific page.
   Upserts the row so it works even if text has never been extracted."
  [document-id page-number]
  (jdbc/execute-one! ds
    ["INSERT INTO pages (document_id, page_number, is_done, updated_at)
      VALUES (?, ?, true, CURRENT_TIMESTAMP)
      ON CONFLICT (document_id, page_number)
      DO UPDATE SET
        is_done = CASE WHEN pages.is_done = true THEN false ELSE true END,
        updated_at = CURRENT_TIMESTAMP"
     document-id page-number]))

;; Flashcard queries
(defn insert-flashcards
  "Batch insert flashcards. Uses ON CONFLICT DO NOTHING to prevent duplicates."
  [rows]
  (when (seq rows)
    (jdbc/execute! ds
      (sql/format {:insert-into :flashcards
                   :values rows
                   :on-conflict []
                   :do-nothing true}))))

(defn get-flashcards
  "Get ALL flashcards for a page (used by sync, export). Includes extract cards."
  [document-id page-number]
  (jdbc/execute! ds
    (sql/format {:select [:*]
                 :from [:flashcards]
                 :where [:and
                         [:= :document_id document-id]
                         [:= :page_number page-number]]
                 :order-by [[:created_at :asc]]})))

(defn get-flashcards-by-content-item
  "Get all flashcards for a specific content item (extract)."
  [content-item-id]
  (jdbc/execute! ds
    (sql/format {:select [:*]
                 :from [:flashcards]
                 :where [:= :content_item_id content-item-id]
                 :order-by [[:created_at :asc]]})))

(defn get-all-flashcards
  "Get all flashcards for a document (all pages)."
  [document-id]
  (jdbc/execute! ds
    (sql/format {:select [:*]
                 :from [:flashcards]
                 :where [:= :document_id document-id]
                 :order-by [[:page_number :asc] [:created_at :asc]]})))

(defn delete-flashcard
  "Delete a single flashcard by ID. Returns the deleted row (including anki_note_id)."
  [card-id]
  (jdbc/execute-one! ds
    ["DELETE FROM flashcards WHERE id = ? RETURNING id, anki_note_id" card-id]))

(defn get-anki-note-ids-for-document
  "Get all anki_note_ids for a document's flashcards (for bulk Anki cleanup before cascade delete)."
  [document-id]
  (->> (jdbc/execute! ds
         ["SELECT anki_note_id FROM flashcards WHERE document_id = ? AND anki_note_id IS NOT NULL"
          document-id])
       (mapv :flashcards/anki_note_id)))

(defn get-anki-note-ids-for-content-item
  "Get all anki_note_ids for an extract's flashcards (for bulk Anki cleanup before cascade delete)."
  [content-item-id]
  (->> (jdbc/execute! ds
         ["SELECT anki_note_id FROM flashcards WHERE content_item_id = ? AND anki_note_id IS NOT NULL"
          content-item-id])
       (mapv :flashcards/anki_note_id)))

(defn update-flashcard
  "Update flashcard content fields. Sets updated_at for sync tracking."
  [card-id fields]
  (jdbc/execute-one! ds
    (sql/format {:update :flashcards
                 :set (assoc fields :updated_at [:now])
                 :where [:= :id card-id]})))

(defn get-context-pages
  "Get text from previous N pages for context. Returns pages in ascending order."
  [document-id start-page end-page]
  (jdbc/execute! ds
    (sql/format {:select [:page_number :text]
                 :from [:pages]
                 :where [:and
                         [:= :document_id document-id]
                         [:>= :page_number start-page]
                         [:<= :page_number end-page]]
                 :order-by [[:page_number :asc]]})))

;; Google OAuth user queries
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

;; Content item queries

(defn sanitize-utf8
  "Re-encode string through UTF-8 to strip invalid byte sequences."
  [s]
  (when s
    (String. (.getBytes (str s) "UTF-8") "UTF-8")))

(defn save-content-item
  ([document-id page-number kind content]
   (save-content-item document-id page-number kind content nil))
  ([document-id page-number kind content parent-content-item-id]
   (jdbc/execute-one! ds
     (sql/format {:insert-into :content_items
                  :values [(cond-> {:document_id document-id
                                    :page_number page-number
                                    :kind kind
                                    :content (sanitize-utf8 content)}
                             parent-content-item-id
                             (assoc :parent_content_item_id parent-content-item-id))]
                  :returning [:id]}))))

(defn get-content-items [document-id page-number]
  (jdbc/execute! ds
    (sql/format {:select [:*]
                 :from [:content_items]
                 :where [:and [:= :document_id document-id]
                               [:= :page_number page-number]]
                 :order-by [[:created_at :asc]]})))

(defn get-content-item-by-id [id]
  (jdbc/execute-one! ds
    (sql/format {:select [:*] :from [:content_items] :where [:= :id id]})))

(defn update-content-item-content [id content]
  (jdbc/execute-one! ds
    (sql/format {:update :content_items
                 :set {:content (sanitize-utf8 content)}
                 :where [:= :id id]})))

(defn get-all-content-items
  "Get all content items for a user across all documents, newest first."
  [user-id]
  (jdbc/execute! ds
    (sql/format {:select [:ci/id :ci/document_id :ci/page_number :ci/content :ci/created_at :d/filename]
                 :from [[:content_items :ci]]
                 :join [[:documents :d] [:= :ci/document_id :d/id]]
                 :where [:= :d/user_id user-id]
                 :order-by [[:ci/created_at :desc]]})))

(defn get-knowledge-tree
  "Fetch all content_items with parent references for building the knowledge tree."
  [user-id]
  (jdbc/execute! ds
    (sql/format {:select [:ci/id :ci/document_id :ci/page_number :ci/content
                          :ci/parent_content_item_id :ci/status :ci/created_at]
                 :from [[:content_items :ci]]
                 :join [[:documents :d] [:= :ci/document_id :d/id]]
                 :where [:= :d/user_id user-id]
                 :order-by [[:ci/document_id :asc] [:ci/page_number :asc] [:ci/created_at :asc]]})))

;; Anki sync functions
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
  "Count unsynced cards, scoped to extract, page, or document.
   - content-item-id provided: count only that extract's cards
   - page-number provided (no content-item-id): count all cards on the page
   - neither: count all cards in document"
  [document-id page-number content-item-id]
  (let [base-where "document_id = ? AND (anki_synced_at IS NULL OR (updated_at IS NOT NULL AND updated_at > anki_synced_at))"
        [sql & params]
        (cond
          content-item-id
          [(str "SELECT COUNT(*) AS cnt FROM flashcards WHERE " base-where " AND content_item_id = ?")
           document-id content-item-id]

          page-number
          [(str "SELECT COUNT(*) AS cnt FROM flashcards WHERE " base-where " AND page_number = ?")
           document-id page-number]

          :else
          [(str "SELECT COUNT(*) AS cnt FROM flashcards WHERE " base-where)
           document-id])
        result (jdbc/execute-one! ds (into [sql] params))]
    (or (:cnt result) 0)))

;; Learning queue functions
(defn get-learning-queue
  "Unified queue of due documents and extracts for incremental reading."
  [user-id]
  (try
    (jdbc/execute! ds
      ["SELECT 'document' AS topic_type, d.id, d.id AS document_id, 0 AS page_number,
               d.priority, d.next_review_at, d.interval_days, d.a_factor, d.review_count,
               NULL AS content, d.filename
        FROM documents d
        WHERE d.user_id = ?
          AND (d.next_review_at <= NOW() OR d.next_review_at IS NULL)
          AND (d.status = 'active' OR d.status IS NULL)
        UNION ALL
        SELECT 'extract', ci.id, ci.document_id, ci.page_number, ci.priority,
               ci.next_review_at, ci.interval_days, ci.a_factor, ci.review_count,
               ci.content, d.filename
        FROM content_items ci JOIN documents d ON ci.document_id = d.id
        WHERE d.user_id = ?
          AND (ci.next_review_at <= NOW() OR ci.next_review_at IS NULL)
          AND (ci.status = 'active' OR ci.status IS NULL)
        ORDER BY priority ASC, next_review_at ASC NULLS FIRST"
       user-id user-id])
    (catch Exception e
      (println "ERROR get-learning-queue:" (.getMessage e))
      [])))

(defn get-learning-queue-count
  "Count of due topics without fetching content."
  [user-id]
  (let [result (jdbc/execute-one! ds
                 ["SELECT
                     (SELECT COUNT(*) FROM documents d
                      WHERE d.user_id = ?
                        AND (d.next_review_at <= NOW() OR d.next_review_at IS NULL)
                        AND (d.status = 'active' OR d.status IS NULL))
                   + (SELECT COUNT(*) FROM content_items ci JOIN documents d ON ci.document_id = d.id
                      WHERE d.user_id = ?
                        AND (ci.next_review_at <= NOW() OR ci.next_review_at IS NULL)
                        AND (ci.status = 'active' OR ci.status IS NULL))
                   AS total"
                  user-id user-id])]
    (or (:total result) 0)))

(defn get-total-topic-count
  "Count ALL topics (due and not due, excluding dismissed)."
  [user-id]
  (let [result (jdbc/execute-one! ds
                 ["SELECT
                     (SELECT COUNT(*) FROM documents d
                      WHERE d.user_id = ?
                        AND (d.status = 'active' OR d.status IS NULL))
                   + (SELECT COUNT(*) FROM content_items ci JOIN documents d ON ci.document_id = d.id
                      WHERE d.user_id = ?
                        AND (ci.status = 'active' OR ci.status IS NULL))
                   AS total"
                  user-id user-id])]
    (or (:total result) 0)))

(defn get-full-queue
  "Get ALL topics (documents + extracts) ordered by priority. No date filter."
  [user-id]
  (try
    (jdbc/execute! ds
      ["SELECT * FROM (
        SELECT 'document' AS topic_type, d.id, d.filename AS title, d.priority,
               d.next_review_at, d.interval_days, d.status,
               d.source_type, NULL AS content
        FROM documents d WHERE d.user_id = ?
        UNION ALL
        SELECT 'extract', ci.id, d.filename AS title, ci.priority,
               ci.next_review_at, ci.interval_days, ci.status,
               NULL AS source_type, SUBSTRING(ci.content FROM 1 FOR 100) AS content
        FROM content_items ci JOIN documents d ON ci.document_id = d.id
        WHERE d.user_id = ?
       ) q ORDER BY CASE WHEN status = 'active' OR status IS NULL THEN 0 WHEN status = 'done' THEN 1 ELSE 2 END, priority ASC, next_review_at ASC NULLS FIRST"
       user-id user-id])
    (catch Exception e
      (println "ERROR get-full-queue:" (.getMessage e))
      [])))

(defn get-review-calendar
  "Get topic counts per day for the next N days."
  [user-id days]
  (jdbc/execute! ds
    ["SELECT review_date, SUM(cnt) AS count FROM (
        SELECT DATE(next_review_at) AS review_date, COUNT(*) AS cnt
        FROM documents WHERE user_id = ?
          AND next_review_at BETWEEN NOW() AND NOW() + ? * INTERVAL '1 day'
          AND (status = 'active' OR status IS NULL)
        GROUP BY DATE(next_review_at)
        UNION ALL
        SELECT DATE(ci.next_review_at), COUNT(*)
        FROM content_items ci JOIN documents d ON ci.document_id = d.id
        WHERE d.user_id = ?
          AND ci.next_review_at BETWEEN NOW() AND NOW() + ? * INTERVAL '1 day'
          AND (ci.status = 'active' OR ci.status IS NULL)
        GROUP BY DATE(ci.next_review_at)
      ) sub GROUP BY review_date ORDER BY review_date"
     user-id days user-id days]))

(defn advance-topic
  "Advance a topic's review schedule using A-Factor algorithm."
  [topic-type id]
  (let [table (case topic-type
                "document" "documents"
                "page" "pages"
                "extract" "content_items")]
    (jdbc/execute-one! ds
      [(str "UPDATE " table "
             SET interval_days = COALESCE(interval_days, 1.0) * COALESCE(a_factor, 2.0),
                 next_review_at = NOW() + (COALESCE(interval_days, 1.0) * COALESCE(a_factor, 2.0)) * INTERVAL '1 day',
                 last_review_at = NOW(),
                 review_count = COALESCE(review_count, 0) + 1
             WHERE id = ?")
       id])))

(defn update-topic-priority
  "Update a topic's priority (0=highest, 100=lowest)."
  [topic-type id priority]
  (let [table (case topic-type
                "document" "documents"
                "extract" "content_items")]
    (jdbc/execute-one! ds
      [(str "UPDATE " table " SET priority = ? WHERE id = ?")
       priority id])))

(defn postpone-topic
  "Postpone a topic by N days without changing its interval/a-factor."
  [topic-type id days]
  (let [table (case topic-type
                "document" "documents"
                "extract" "content_items")]
    (jdbc/execute-one! ds
      [(str "UPDATE " table "
             SET next_review_at = NOW() + ? * INTERVAL '1 day',
                 last_review_at = NOW()
             WHERE id = ?")
       (double days) id])))

(defn dismiss-topic
  "Remove a topic from the review queue without deleting it."
  [topic-type id]
  (let [table (case topic-type
                "document" "documents"
                "extract" "content_items")]
    (jdbc/execute-one! ds
      [(str "UPDATE " table " SET status = 'dismissed', dismissed = true WHERE id = ?")
       id])))

(defn done-topic
  "Mark a topic as fully processed (extracted/carded everything useful)."
  [topic-type id]
  (let [table (case topic-type
                "document" "documents"
                "extract" "content_items")]
    (jdbc/execute-one! ds
      [(str "UPDATE " table " SET status = 'done' WHERE id = ?")
       id])))

(defn delete-content-item
  "Delete a content item (extract) and cascade to children + flashcards."
  [id]
  (jdbc/execute! ds
    (sql/format {:delete-from :content_items
                 :where [:= :id id]})))

(defn get-inactive-topics
  "Get all non-active (dismissed + done) documents and extracts for a user."
  [user-id]
  (try
    (jdbc/execute! ds
      ["SELECT 'document' AS topic_type, d.id, d.filename AS title, d.uploaded_at, d.status
        FROM documents d
        WHERE d.user_id = ? AND d.status IN ('dismissed', 'done')
        UNION ALL
        SELECT 'extract', ci.id, SUBSTRING(ci.content FROM 1 FOR 100) AS title, ci.created_at AS uploaded_at, ci.status
        FROM content_items ci JOIN documents d ON ci.document_id = d.id
        WHERE d.user_id = ? AND ci.status IN ('dismissed', 'done')
        ORDER BY uploaded_at DESC"
       user-id user-id])
    (catch Exception e
      (println "ERROR get-dismissed-topics:" (.getMessage e))
      [])))

(defn restore-topic
  "Restore a dismissed/done topic back to the active review queue."
  [topic-type id]
  (let [table (case topic-type
                "document" "documents"
                "extract" "content_items")]
    (jdbc/execute-one! ds
      [(str "UPDATE " table " SET status = 'active', dismissed = false, next_review_at = NULL WHERE id = ?")
       id])))
