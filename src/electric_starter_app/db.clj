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
