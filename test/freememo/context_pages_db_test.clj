(ns freememo.context-pages-db-test
  "Database coverage for page-context reads used by card generation and the assistant.

   NEEDS POSTGRESQL. `freememo.db` creates its connection pool at namespace load."
  (:require [clojure.test :refer [deftest is]]
            [next.jdbc :as jdbc]
            [freememo.db :as db]))

(deftest context-page-query-reads-page-content
  (let [root-id (:topics/id
                 (jdbc/execute-one! db/ds
                   ["INSERT INTO topics (kind, title) VALUES ('pdf', 'context-pages-db-test') RETURNING id"]))
        page-id (:topics/id
                 (jdbc/execute-one! db/ds
                   ["INSERT INTO topics (parent_id, kind, title, content, page_number)
                     VALUES (?, 'page', 'Page 1', '<p>body 1</p>', 1) RETURNING id"
                    root-id]))]
    (try
      (is (= [[1 "<p>body 1</p>"]]
             (mapv (juxt :topics/page_number :topics/content)
               (db/get-context-pages root-id 1 1))))
      (finally
        (jdbc/execute! db/ds ["DELETE FROM topics WHERE id IN (?, ?)" page-id root-id])))))
