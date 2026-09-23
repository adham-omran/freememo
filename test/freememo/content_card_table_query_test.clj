(ns freememo.content-card-table-query-test
  "The card table's query, `get-cards-for-topic*`, on a PDF page.

   It lives in its own namespace, apart from `freememo.subtree-flashcards-test`,
   because reaching it means LOADING the Electric namespace. A load failure
   there must not take the database query tests down with it — the same split
   as `freememo.curriculum-exam-render-test`.

   The defect: a page's table read \"No cards yet\" while an extract made from
   that page held four cards. The query asked only for cards whose topic IS the
   page, and a page's cards live mostly on its extracts.

   SKIPS (not fails) when no database is reachable. Removes its rows afterwards."
  (:require [clojure.test :refer [deftest is testing]]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]
            [freememo.db :as db]
            [freememo.content-card-table :as table]))

(defn- db-up? []
  (try (jdbc/execute-one! db/ds ["SELECT 1"]) true (catch Exception _ false)))

(defn- insert-id! [sql-params]
  (:id (jdbc/execute-one! db/ds sql-params {:builder-fn rs/as-unqualified-maps})))

(defn- member-ids [result]
  (set (mapcat :member-ids (:cards result))))

(deftest ^:db a-page-table-lists-its-extracts-cards
  (if-not (db-up?)
    (println "SKIP content-card-table-query: no Postgres reachable.")
    (let [tag (str "card-table-query-" (System/nanoTime))
          uid (insert-id! ["INSERT INTO users (username, email) VALUES (?, ?) RETURNING id"
                           tag (str tag "@example.test")])
          root (insert-id! ["INSERT INTO topics (user_id, title, kind)
                             VALUES (?, 'card table pdf', 'pdf') RETURNING id" uid])]
      (try
        (let [page (insert-id! ["INSERT INTO topics (user_id, parent_id, title, kind, page_number)
                                 VALUES (?, ?, 'Page 70', 'page', 70) RETURNING id" uid root])
              extract (insert-id! ["INSERT INTO topics (user_id, parent_id, title, kind)
                                    VALUES (?, ?, 'Parsing is the problem', 'basic')
                                    RETURNING id" uid page])
              cards (mapv #(insert-id! ["INSERT INTO flashcards
                                          (topic_id, root_topic_id, kind, question, answer)
                                         VALUES (?, ?, 'basic', ?, 'a') RETURNING id"
                                        extract root %])
                      ["q1" "q2" "q3" "q4"])]
          (testing "the page lists the four cards held by its extract"
            (let [result (table/get-cards-for-topic* 0 page uid)]
              (is (:success result))
              (is (= (set cards) (member-ids result)))))
          (testing "the extract still lists its own cards"
            (is (= (set cards) (member-ids (table/get-cards-for-topic* 0 extract uid)))))
          (testing "the root still lists every card in the document"
            (is (= (set cards) (member-ids (table/get-cards-for-topic* 0 root uid))))))
        (finally
          (jdbc/execute! db/ds ["DELETE FROM topics WHERE id = ?" root])
          (jdbc/execute! db/ds ["DELETE FROM users WHERE id = ?" uid]))))))
