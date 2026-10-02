(ns freememo.reading-positions-db-test
  "plans/reading-position-memory.md §4 1: the `reading_positions` table and
   its two queries. Skips with the database down."
  (:require [clojure.test :refer [deftest is testing]]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]
            [freememo.db :as db]))

(defn- db-up? []
  (try (jdbc/execute-one! db/ds ["SELECT 1"]) true (catch Exception _ false)))

(defn- insert! [sql & params]
  (:id (jdbc/execute-one! db/ds (into [sql] params)
         {:builder-fn rs/as-unqualified-maps})))

(defn- user! [label]
  (insert! "INSERT INTO users (username) VALUES (?) RETURNING id"
    (str "reading-position-test-" label "-" (System/nanoTime))))

(defn- rows-for [topic-id]
  (:n (jdbc/execute-one! db/ds
        ["SELECT count(*) AS n FROM reading_positions WHERE topic_id = ?" topic-id]
        {:builder-fn rs/as-unqualified-maps})))

(deftest a-position-round-trips-for-its-owner-only
  (if-not (db-up?)
    (is true "PostgreSQL is unavailable; integration assertion skipped")
    (let [_ (#'db/setup-schema-reading-positions!)
          owner (user! "owner")
          other (user! "other")]
      (try
        (let [topic (insert! "INSERT INTO topics (user_id, kind, title) VALUES (?, 'wikipedia', 'Article') RETURNING id"
                      owner)
              anchor {:i 12 :fp "The second section begins" :frac 0.25}]
          (testing "the owner's write lands and reads back as plain data"
            (is (= 1 (db/save-reading-position! owner topic "editor" anchor 0.4)))
            (let [row (db/get-reading-position owner topic "editor")]
              (is (map? (:anchor row)))
              (is (= anchor (:anchor row)))
              (is (< (abs (- 0.4 (:fraction row))) 1e-6))))
          (testing "a second write replaces the first"
            (db/save-reading-position! owner topic "editor" (assoc anchor :i 13) 0.5)
            (is (= 13 (get-in (db/get-reading-position owner topic "editor") [:anchor :i])))
            (is (= 1 (rows-for topic))))
          (testing "each surface keeps its own position"
            (db/save-reading-position! owner topic "code-source" {:i 3 :fp "" :frac 0} 0.1)
            (is (= 3 (get-in (db/get-reading-position owner topic "code-source") [:anchor :i])))
            (is (= 13 (get-in (db/get-reading-position owner topic "editor") [:anchor :i]))))
          (testing "another user can neither write nor read it"
            (is (= 0 (db/save-reading-position! other topic "editor" {:i 0 :fp "" :frac 0} 0)))
            (is (nil? (db/get-reading-position other topic "editor")))
            (is (= 13 (get-in (db/get-reading-position owner topic "editor") [:anchor :i]))))
          (testing "an unknown surface is refused by the schema"
            (is (thrown? Exception (db/save-reading-position! owner topic "pdf" anchor 0.1))))
          (testing "the rows go with their topic"
            (jdbc/execute! db/ds ["DELETE FROM topics WHERE id = ?" topic])
            (is (= 0 (rows-for topic)))))
        (finally
          (jdbc/execute! db/ds ["DELETE FROM topics WHERE user_id IN (?, ?)" owner other])
          (jdbc/execute! db/ds ["DELETE FROM users WHERE id IN (?, ?)" owner other]))))))
