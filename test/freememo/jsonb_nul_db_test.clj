(ns freememo.jsonb-nul-db-test
  "A JSONB write never fails on NUL (db/->jsonb). PostgreSQL rejects \\u0000
   in JSONB, so the bibliography of a PDF with a NUL in its title was never
   stored (production, 2026-10-14). Skips when the database is down."
  (:require [clojure.test :refer [deftest is]]
            [freememo.db :as db]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]))

(defn- db-up? []
  (try (jdbc/execute-one! db/ds ["SELECT 1"]) true (catch Exception _ false)))

(deftest a-csl-with-nul-is-stored-without-it
  (if-not (db-up?)
    (is true "PostgreSQL is unavailable; integration assertion skipped")
    (let [user-id (:id (jdbc/execute-one! db/ds
                         ["INSERT INTO users (username, email) VALUES (?, ?) RETURNING id"
                          (str "jsonb-nul-test-" (random-uuid)) (str (random-uuid) "@example.test")]
                         {:builder-fn rs/as-unqualified-maps}))]
      (try
        (let [source (db/create-source! {:user-id user-id :csl-type "document"
                                         :csl {:type "document" :title "Title\u0000 here"
                                               :author [{:family "A\u0000"}]}
                                         :title "Title here"})]
          (is (= {:type "document" :title "Title here" :author [{:family "A"}]}
                (:sources/csl (db/get-source (:sources/id source))))))
        (finally
          (jdbc/execute! db/ds ["DELETE FROM users WHERE id = ?" user-id]))))))
