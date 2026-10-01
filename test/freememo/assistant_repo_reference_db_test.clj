(ns freememo.assistant-repo-reference-db-test
  "The query behind the assistant's @ picker for repo files. Skips with the
   database down."
  (:require [clojure.test :refer [deftest is testing]]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]
            [freememo.db :as db]))

(defn- db-up? []
  (try (jdbc/execute-one! db/ds ["SELECT 1"]) true (catch Exception _ false)))

(defn- insert-user! []
  (:id (jdbc/execute-one! db/ds
         ["INSERT INTO users (username) VALUES (?) RETURNING id"
          (str "assistant-repo-ref-test-" (System/nanoTime))]
         {:builder-fn rs/as-unqualified-maps})))

(defn- insert-topic! [user-id kind title parent-id repo-path]
  (:id (jdbc/execute-one! db/ds
         ["INSERT INTO topics (user_id, kind, title, parent_id, repo_path)
           VALUES (?, ?, ?, ?, ?) RETURNING id"
          user-id kind title parent-id repo-path]
         {:builder-fn rs/as-unqualified-maps})))

(defn- insert-source! [topic-id source]
  (jdbc/execute-one! db/ds
    ["INSERT INTO topic_code (topic_id, source, byte_size, lang) VALUES (?, ?, ?, ?)"
     topic-id source (count source) "markdown"]))

(defn- repo!
  "A repository with an older and a newer commit. Post: the ids by name."
  [user-id title]
  (let [repo (insert-topic! user-id "repo" title nil nil)
        old-commit (insert-topic! user-id "commit" "aaaaaaa" repo "")
        old-file (insert-topic! user-id "code" "a.md" old-commit "docs/a.md")
        new-commit (insert-topic! user-id "commit" "bbbbbbb" repo "")
        docs (insert-topic! user-id "dir" "docs" new-commit "docs")
        new-file (insert-topic! user-id "code" "a.md" docs "docs/a.md")
        readme (insert-topic! user-id "code" "README.md" new-commit "README.md")
        _empty-leaf (insert-topic! user-id "code" "logo.png" new-commit "logo.png")]
    (doseq [id [old-file new-file readme]]
      (insert-source! id "# A"))
    {:new-file new-file :readme readme}))

(deftest the-picker-reads-only-the-newest-commit-of-the-users-repositories
  (if-not (db-up?)
    (is true "PostgreSQL is unavailable; integration assertion skipped")
    (let [user-id (insert-user!)
          other-id (insert-user!)]
      (try
        (let [{:keys [new-file readme]} (repo! user-id "adham/grid")
              _ (repo! other-id "other/grid")]
          (testing "the older commit's copy of a path is not offered"
            (is (= [{:id new-file :label "adham/grid/docs/a.md"}]
                  (db/get-referenceable-repo-files user-id "a.md" 50))))
          (testing "the repository title matches; a shallower path comes first;
                    an empty leaf never comes"
            (is (= [readme new-file]
                  (map :id (db/get-referenceable-repo-files user-id "grid" 50)))))
          (testing "the limit bounds the list"
            (is (= [readme]
                  (map :id (db/get-referenceable-repo-files user-id "grid" 1)))))
          (testing "another user's repository never matches"
            (is (= [] (db/get-referenceable-repo-files user-id "other/" 50)))))
        (finally
          (jdbc/execute! db/ds ["DELETE FROM topics WHERE user_id IN (?, ?)" user-id other-id])
          (jdbc/execute! db/ds ["DELETE FROM users WHERE id IN (?, ?)" user-id other-id]))))))
