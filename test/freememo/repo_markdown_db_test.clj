(ns freememo.repo-markdown-db-test
  "The path lookup the rendered Markdown view calls
   (plans/markdown-rendered-code-pane.md §1.5). Skips with the database down."
  (:require [clojure.test :refer [deftest is]]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]
            [freememo.db :as db]))

(defn- db-up? []
  (try (jdbc/execute-one! db/ds ["SELECT 1"]) true (catch Exception _ false)))

(defn- insert-topic! [row]
  (:id (jdbc/execute-one! db/ds
         ["INSERT INTO topics (user_id, kind, title, parent_id, repo_path)
           VALUES (?, ?, ?, ?, ?) RETURNING id"
          (:user-id row) (:kind row) (:title row) (:parent-id row) (:repo-path row)]
         {:builder-fn rs/as-unqualified-maps})))

;; `repo-markdown/repo-link-paths` returns a SET. `distinct` destructures its
;; input with `nth`, which a set does not support, so the lookup threw on every
;; Markdown file with a forge origin. Inside the code pane's `e/Offload`, that
;; ended the session's Electric process with nothing in the server log.
(deftest the-lookup-takes-the-set-the-renderer-passes
  (if-not (db-up?)
    (is true "PostgreSQL is unavailable; integration assertion skipped")
    (let [user-id (:id (jdbc/execute-one! db/ds
                         ["INSERT INTO users (username) VALUES (?) RETURNING id"
                          (str "repo-md-db-test-" (System/nanoTime))]
                         {:builder-fn rs/as-unqualified-maps}))]
      (try
        (let [commit-id (insert-topic! {:user-id user-id :kind "commit" :title "c"})
              file-id (insert-topic! {:user-id user-id :kind "code" :title "a.md"
                                      :parent-id commit-id :repo-path "docs/a.md"})]
          (is (= {"docs/a.md" file-id}
                 (db/repo-topic-ids-by-path commit-id #{"docs/a.md" "docs/missing.md"}))))
        (finally
          (jdbc/execute! db/ds ["DELETE FROM topics WHERE user_id = ?" user-id])
          (jdbc/execute! db/ds ["DELETE FROM users WHERE id = ?" user-id]))))))
