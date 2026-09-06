(ns freememo.document-summary-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [freememo.db :as db]
   [next.jdbc :as jdbc]
   [next.jdbc.result-set :as rs]))

(defn- db-up? []
  (try
    (jdbc/execute-one! db/ds ["SELECT 1"])
    true
    (catch Exception _ false)))

(defn- insert-user! []
  (let [tag (str "document-summary-" (System/nanoTime))]
    (:id (jdbc/execute-one! db/ds
           ["INSERT INTO users (username, email) VALUES (?, ?) RETURNING id"
            tag (str tag "@example.test")]
           {:builder-fn rs/as-unqualified-maps}))))

(defn- insert-topic! [user-id parent-id kind title status]
  (:id (jdbc/execute-one! db/ds
         ["INSERT INTO topics (user_id, parent_id, kind, title, status)
           VALUES (?, ?, ?, ?, ?) RETURNING id"
          user-id parent-id kind title status]
         {:builder-fn rs/as-unqualified-maps})))

(defn- status-by-id [user-id]
  (into {} (map (juxt :topics/id identity)) (db/get-document-status user-id)))

(deftest ^:db document-summary-follows-source-writes
  (if-not (db-up?)
    (println "SKIP document-summary: no PostgreSQL database is available.")
    (do
      (db/setup-schema)
      (let [user-id (insert-user!)]
        (try
          (let [root-a (insert-topic! user-id nil "basic" "Root A" "active")
                child (insert-topic! user-id root-a "basic" "Child" "done")
                root-b (insert-topic! user-id nil "basic" "Root B" "active")]
            (testing "topic writes update item progress"
              (let [rows (status-by-id user-id)]
                (is (= 2 (:total_items (get rows root-a))))
                (is (= 1 (:done_items (get rows root-a))))))

            (testing "card writes update sync progress"
              (let [card-id (:id (jdbc/execute-one! db/ds
                                  ["INSERT INTO flashcards
                                      (topic_id, root_topic_id, kind, question, answer)
                                    VALUES (?, ?, 'basic', 'Question', 'Answer')
                                    RETURNING id"
                                   child root-a]
                                  {:builder-fn rs/as-unqualified-maps}))]
                (let [row (get (status-by-id user-id) root-a)]
                  (is (= 1 (:total_cards row)))
                  (is (= 0 (:synced_cards row))))
                (jdbc/execute! db/ds
                  ["UPDATE flashcards
                    SET anki_note_id = 42,
                        local_current_digest = ?,
                        local_baseline_digest = ?
                    WHERE id = ?"
                   (apply str (repeat 64 "a"))
                   (apply str (repeat 64 "a"))
                   card-id])
                (is (= 1 (:synced_cards (get (status-by-id user-id) root-a))))))

            (testing "moving a subtree refreshes both roots"
              (jdbc/execute! db/ds
                ["UPDATE topics SET parent_id = ? WHERE id = ?" root-b child])
              (let [rows (status-by-id user-id)]
                (is (= 1 (:total_items (get rows root-a))))
                (is (= 0 (:done_items (get rows root-a))))
                (is (= 2 (:total_items (get rows root-b))))
                (is (= 1 (:done_items (get rows root-b)))))))
          (finally
            (jdbc/execute! db/ds ["DELETE FROM users WHERE id = ?" user-id])))))))
