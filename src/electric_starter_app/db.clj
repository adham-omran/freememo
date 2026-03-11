(ns electric-starter-app.db
  "Database connection and schema management for PostgreSQL."
  (:require
    [next.jdbc :as jdbc]
    [honey.sql :as sql]))

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

  ;; Anki sync columns on flashcards
  (jdbc/execute! ds ["ALTER TABLE flashcards ADD COLUMN IF NOT EXISTS anki_note_id BIGINT DEFAULT NULL"])
  (jdbc/execute! ds ["ALTER TABLE flashcards ADD COLUMN IF NOT EXISTS anki_synced_at TIMESTAMP DEFAULT NULL"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_flashcards_anki_note_id ON flashcards(anki_note_id) WHERE anki_note_id IS NOT NULL"])

  ;; Create content_items table
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
                           :mime_type mime-type}]
                 :returning [:id]})))

(defn get-documents [user-id]
  (jdbc/execute! ds
    (sql/format {:select [:id :filename :file_size :uploaded_at]
                 :from [:documents]
                 :where [:= :user_id user-id]
                 :order-by [[:uploaded_at :desc]]})))

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
  "Get all flashcards for a specific document page."
  [document-id page-number]
  (jdbc/execute! ds
    (sql/format {:select [:*]
                 :from [:flashcards]
                 :where [:and
                         [:= :document_id document-id]
                         [:= :page_number page-number]]
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
  "Delete a single flashcard by ID."
  [card-id]
  (jdbc/execute! ds
    (sql/format {:delete-from :flashcards
                 :where [:= :id card-id]})))

(defn update-flashcard
  "Update flashcard content fields. Returns update count."
  [card-id fields]
  (jdbc/execute-one! ds
    (sql/format {:update :flashcards
                 :set fields
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
(defn save-content-item [document-id page-number kind content]
  (jdbc/execute-one! ds
    (sql/format {:insert-into :content_items
                 :values [{:document_id document-id
                           :page_number page-number
                           :kind kind
                           :content content}]
                 :returning [:id]})))

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
                 :set {:content content}
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
