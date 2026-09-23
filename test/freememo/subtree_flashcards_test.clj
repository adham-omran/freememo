(ns freememo.subtree-flashcards-test
  "DB-backed checks for `db/get-flashcards-for-subtree`, the query the card
   table runs for every non-root topic.

   The defect it answers: a PDF page's card table read \"No cards yet\" while an
   extract made from that page held four cards, because the table asked only
   for cards whose topic IS the page.

   These need a real Postgres because the property under test is a recursive
   CTE — where it descends and where it stops. The tests SKIP (not fail) when no
   database is reachable, matching `anki-settings-snapshot-test`.

   Every test creates its own user and removes its rows afterwards, so nothing
   here depends on or disturbs the dev collection."
  (:require [clojure.test :refer [deftest is testing]]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]
            [freememo.db :as db]))

(defn- db-up? []
  (try (jdbc/execute-one! db/ds ["SELECT 1"]) true (catch Exception _ false)))

(defn- insert-id! [sql-params]
  (:id (jdbc/execute-one! db/ds sql-params {:builder-fn rs/as-unqualified-maps})))

(defn- new-user! []
  (let [tag (str "subtree-cards-" (System/nanoTime))]
    (insert-id! ["INSERT INTO users (username, email) VALUES (?, ?) RETURNING id"
                 tag (str tag "@example.test")])))

(defn- new-pdf! [uid]
  (insert-id! ["INSERT INTO topics (user_id, title, kind) VALUES (?, 'subtree pdf', 'pdf')
                RETURNING id" uid]))

(defn- new-page! [uid root-id n]
  (insert-id! ["INSERT INTO topics (user_id, parent_id, title, kind, page_number)
                VALUES (?, ?, ?, 'page', ?) RETURNING id" uid root-id (str "Page " n) n]))

(defn- new-extract! [uid parent-id]
  (insert-id! ["INSERT INTO topics (user_id, parent_id, title, kind)
                VALUES (?, ?, 'subtree extract', 'basic') RETURNING id" uid parent-id]))

(defn- new-card! [topic-id root-id q]
  (insert-id! ["INSERT INTO flashcards (topic_id, root_topic_id, kind, question, answer)
                VALUES (?, ?, 'basic', ?, 'a') RETURNING id" topic-id root-id q]))

(defn- remove-user! [uid root-id]
  (jdbc/execute! db/ds ["DELETE FROM topics WHERE id = ?" root-id])
  (jdbc/execute! db/ds ["DELETE FROM users WHERE id = ?" uid]))

(defn- card-ids [rows]
  (set (map :flashcards/id rows)))

(deftest ^:db a-page-lists-the-cards-of-its-extracts
  (if-not (db-up?)
    (println "SKIP subtree-flashcards: no Postgres reachable.")
    (let [uid (new-user!)
          root (new-pdf! uid)]
      (try
        (let [page-70 (new-page! uid root 70)
              page-71 (new-page! uid root 71)
              extract (new-extract! uid page-70)
              nested (new-extract! uid extract)
              other (new-extract! uid page-71)
              on-page (new-card! page-70 root "on the page")
              on-extract (new-card! extract root "on the extract")
              on-nested (new-card! nested root "on an extract of the extract")
              on-other (new-card! other root "on the next page's extract")
              on-root (new-card! root root "on the root")]
          (testing "the page's own query misses the extract's cards — the defect"
            (is (= #{on-page} (card-ids (db/get-flashcards page-70)))))
          (testing "the subtree reaches extracts and extracts of extracts"
            (is (= #{on-page on-extract on-nested}
                  (card-ids (db/get-flashcards-for-subtree page-70)))))
          (testing "the subtree stops at the topic: no sibling page, no parent"
            (is (not (contains? (card-ids (db/get-flashcards-for-subtree page-70)) on-other)))
            (is (not (contains? (card-ids (db/get-flashcards-for-subtree page-70)) on-root))))
          (testing "an extract lists its own descendants too"
            (is (= #{on-extract on-nested}
                  (card-ids (db/get-flashcards-for-subtree extract)))))
          (testing "an extract's card carries its page's number through the parent"
            (is (= 70 (->> (db/get-flashcards-for-subtree page-70)
                        (filter #(= on-extract (:flashcards/id %)))
                        first :page_number)))))
        (finally (remove-user! uid root))))))

(deftest ^:db a-nil-topic-reads-nothing
  (is (= [] (db/get-flashcards-for-subtree nil))))
