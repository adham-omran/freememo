(ns freememo.account-export-test
  "The export's row JSON, against a live database. Tests skip when PostgreSQL
   is down."
  (:require [clojure.test :refer [deftest is testing]]
            [cheshire.core :as json]
            [clojure.java.io :as io]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]
            [freememo.account-export :as export]
            [freememo.account-transfer-format :as fmt]
            [freememo.db :as db])
  (:import [java.io File FileOutputStream]
           [java.util.zip ZipFile]))

(def ^:private opts {:builder-fn rs/as-unqualified-lower-maps})

(defn- db-up? []
  (try (jdbc/execute-one! db/ds ["SELECT 1"]) true (catch Exception _ false)))

(defn- one [sql & params] (jdbc/execute-one! db/ds (into [sql] params) opts))

(defn- entity-lines [^ZipFile zip table]
  (let [entry (.getEntry zip (fmt/entity-path table))]
    (mapv json/parse-string (line-seq (io/reader (.getInputStream zip entry))))))

(deftest an-account-with-a-pdf-over-128-mib-exports
  ;; to_jsonb renders bytea as hex, two characters per byte, and a jsonb string
  ;; holds at most 268435455 bytes. 135 MB of file_data is over that limit.
  (when (db-up?)
    (let [u (:id (one "INSERT INTO users (username) VALUES (?) RETURNING id" (str "export-" (System/nanoTime))))
          t (:id (one "INSERT INTO topics (user_id, title, kind, content) VALUES (?, 'big', 'pdf', '<p>c</p>') RETURNING id" u))
          f (:id (one "INSERT INTO topic_files (topic_id, file_data, file_size, mime_type)
                       SELECT ?, decode(repeat('ab', 135000000), 'hex'), 135000000, 'application/pdf'
                       RETURNING id" t))
          out (File/createTempFile "export" ".zip")]
      (try
        (with-open [os (FileOutputStream. out)]
          (export/write-archive! db/ds u os "http://test"))
        (with-open [zip (ZipFile. out)]
          (testing "the file row carries its columns and not its bytes"
            (let [[row :as rows] (entity-lines zip :topic_files)]
              (is (= 1 (count rows)))
              (is (= {"id" f "topic_id" t "mime_type" "application/pdf" "file_size" 135000000}
                     (select-keys row ["id" "topic_id" "mime_type" "file_size"])))
              (is (not (contains? row "file_data")))))
          (testing "the bytes are their own entry"
            (is (= 135000000 (.getSize (.getEntry zip (fmt/blob-path :topic_files t "main"))))))
          (testing "a row holds what to_jsonb(t) held, less its drop-keys"
            (let [[row] (entity-lines zip :topics)
                  expected (json/parse-string
                             (:r (one "SELECT CAST(to_jsonb(t) - 'content_preview'::text AS text) AS r
                                       FROM topics t WHERE id = ?" t)))]
              (is (= expected row)))))
        (finally
          (.delete out)
          (jdbc/execute! db/ds ["DELETE FROM topic_files WHERE id = ?" f]))))))
