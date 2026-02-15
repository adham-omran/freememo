(ns electric-starter-app.db
  "Database connection and schema management for PostgreSQL."
  (:require
    [next.jdbc :as jdbc]
    [honey.sql :as sql]))

;; Connection configuration
(def db-config
  {:dbtype "postgresql"
   :host "localhost"
   :port 5432
   :dbname "cardmaker"
   :user "cardmaker"
   :password "dev"})

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
