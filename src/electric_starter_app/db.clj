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
  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS settings (
      key TEXT PRIMARY KEY,
      value TEXT NOT NULL,
      updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )"])

  ;; Seed default values if table is empty
  (let [count (-> (jdbc/execute-one! ds ["SELECT COUNT(*) FROM settings"])
                  :count)]
    (when (zero? count)
      (println "Seeding default settings...")
      (jdbc/execute! ds (sql/format
        {:insert-into :settings
         :values [{:key "openai_api_key" :value ""}
                  {:key "model" :value "gpt-4o"}
                  {:key "card_count" :value "20"}]}))))

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

  (println "Database ready."))

;; Query functions
(defn get-setting [key]
  (-> (jdbc/execute-one! ds
        (sql/format {:select [:value]
                     :from [:settings]
                     :where [:= :key key]}))
      :settings/value))

(defn set-setting [key value]
  (jdbc/execute! ds
    (sql/format {:insert-into :settings
                 :values [{:key key :value value}]
                 :on-conflict [:key]
                 :do-update-set {:value value
                                 :updated_at [:now]}})))

;; Document queries
(defn save-document [filename file-bytes file-size mime-type]
  (jdbc/execute! ds
    (sql/format {:insert-into :documents
                 :values [{:filename filename
                           :file_data file-bytes
                           :file_size file-size
                           :mime_type mime-type}]
                 :returning [:id]})))

(defn get-documents []
  (jdbc/execute! ds
    (sql/format {:select [:id :filename :file_size :uploaded_at]
                 :from [:documents]
                 :order-by [[:uploaded_at :desc]]})))

(defn delete-document [id]
  (jdbc/execute! ds
    (sql/format {:delete-from :documents
                 :where [:= :id id]})))

(defn get-documents-by-id
  "Retrieve a document by its ID."
  [id]
  (jdbc/execute! ds
    (sql/format {:select [:*]
                 :from [:documents]
                 :where [:= :id id]})))

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
