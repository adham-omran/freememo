(ns freememo.assistant-epub-reference-db-test
  "The two reads behind an @ reference to an EPUB. Skips with the database
   down."
  (:require [clojure.test :refer [deftest is testing]]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]
            [freememo.db :as db]))

(defn- db-up? []
  (try (jdbc/execute-one! db/ds ["SELECT 1"]) true (catch Exception _ false)))

(defn- insert! [sql & params]
  (:id (jdbc/execute-one! db/ds (into [sql] params)
         {:builder-fn rs/as-unqualified-maps})))

(deftest the-reads-see-only-chapters-and-their-done-state
  (if-not (db-up?)
    (is true "PostgreSQL is unavailable; integration assertion skipped")
    (let [user-id (insert! "INSERT INTO users (username) VALUES (?) RETURNING id"
                    (str "assistant-epub-ref-test-" (System/nanoTime)))]
      (try
        (let [book (insert! "INSERT INTO topics (user_id, kind, title) VALUES (?, 'epub', 'Book') RETURNING id"
                     user-id)
              chapter! (fn [n status]
                         (insert! "INSERT INTO topics (user_id, kind, title, parent_id, page_number, status, content)
                                   VALUES (?, 'basic', ?, ?, ?, ?, ?) RETURNING id"
                           user-id (str "Chapter " n) book n status (str "<p>" n "</p>")))
              ids (mapv chapter! [1 2 3 4] ["done" "done" "active" "active"])]
          (insert! "INSERT INTO topics (user_id, kind, title, parent_id) VALUES (?, 'basic', 'A note', ?) RETURNING id"
            user-id book)
          (testing "the first chapter not done is where the learner is"
            (is (= 3 (db/get-epub-reading-chapter book))))
          (testing "a child with no chapter index is not a chapter"
            (is (= (subvec ids 1 3) (map :topics/id (db/get-epub-chapters book 2 3))))
            (is (= [1 2 3 4] (map :topics/page_number (db/get-epub-chapters book 0 99)))))
          (testing "a finished book has no reading chapter"
            (jdbc/execute! db/ds ["UPDATE topics SET status = 'done' WHERE parent_id = ?" book])
            (is (nil? (db/get-epub-reading-chapter book)))))
        (finally
          (jdbc/execute! db/ds ["DELETE FROM topics WHERE user_id = ?" user-id])
          (jdbc/execute! db/ds ["DELETE FROM users WHERE id = ?" user-id]))))))
