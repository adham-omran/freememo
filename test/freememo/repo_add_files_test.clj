(ns freememo.repo-add-files-test
  "Adding to an imported repository's selection, through the clone and the
   database (plans/repo-selection-widening.md §5.2). Skips when PostgreSQL is
   down. The forge is a file:// fixture, so nothing leaves the machine."
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.db :as db]
            [freememo.git-clone :as git]
            [freememo.git-clone-test :as fixture]
            [freememo.repo-import :as repo-import]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]))

(def ^:private commits
  [{"README.md" "# Fixture\n"
    "src/main/core.clj" "(ns core)\n"
    "src/main/notes.md" "# Notes\n"
    "src/test/core_test.clj" "(ns core-test)\n"
    "docs/guide.md" "# Guide\n"
    :message "feat: first"}])

(defn- db-up? []
  (try (jdbc/execute-one! db/ds ["SELECT 1"]) true (catch Exception _ false)))

(defn- create-user! []
  (:id (jdbc/execute-one! db/ds
         ["INSERT INTO users (username, email) VALUES (?, ?) RETURNING id"
          (str "repo-add-files-test-" (random-uuid)) (str (random-uuid) "@example.test")]
         {:builder-fn rs/as-unqualified-maps})))

(def ^:private form {:forge "gitlab" :host "gitlab.example.com" :repository "owner/name"})

(defn- rows
  "Post: {repo-path {:id :parent-path :title :source}} under `commit-id`."
  [commit-id]
  (into {}
    (map (fn [r] [(:repo_path r) (dissoc r :repo_path)]))
    (jdbc/execute! db/ds
      ["WITH RECURSIVE sub AS (SELECT id FROM topics WHERE id = ?
                               UNION ALL SELECT t.id FROM topics t JOIN sub ON t.parent_id = sub.id)
        SELECT t.id, t.repo_path, t.title, p.repo_path AS parent_path, tc.source
          FROM sub JOIN topics t ON t.id = sub.id JOIN topics p ON p.id = t.parent_id
          LEFT JOIN topic_code tc ON tc.topic_id = t.id
         WHERE t.kind IN ('dir', 'code')"
       commit-id]
      {:builder-fn rs/as-unqualified-maps})))

(deftest add-files-widens-the-newest-commit
  (if-not (db-up?)
    (is true "PostgreSQL is unavailable; integration assertion skipped")
    (let [{repo :dir} (fixture/fixture-repo! commits)
          user (create-user!)
          root (fixture/temp-dir "clone-root")]
      (try
        (with-redefs [git/protocols #{"https" "file"}
                      git/clone-root (fn [] root)
                      git/remote-url (fn [_ _] (str "file://" repo))]
          (let [staged (repo-import/stage-repository! user form)
                imported (repo-import/import-staged! user (:stage-id staged)
                           {:dirs {"src/main" true} :extensions #{".clj"}})
                commit-id (:topic-id imported)
                repo-id (:id (jdbc/execute-one! db/ds ["SELECT parent_id AS id FROM topics WHERE id = ?" commit-id]
                               {:builder-fn rs/as-unqualified-maps}))
                before (rows commit-id)
                wider {:dirs {"src" true} :extensions #{".clj" ".md"}}]
            (is (:ok imported) (str (:error imported)))
            (is (nil? (get-in before ["src/main/notes.md" :source])) "an empty leaf before")

            (testing "§3.1: a narrower selection is refused and writes nothing"
              (let [r (repo-import/add-repository-files! user repo-id commit-id
                        {:dirs {"docs" true} :extensions #{".clj"}})]
                (is (not (:ok r)))
                (is (= before (rows commit-id)))))

            (testing "§5.2.6: a stale commit is refused"
              (let [r (repo-import/add-repository-files! user repo-id (inc commit-id) wider)]
                (is (not (:ok r)))
                (is (re-find #"newer commit" (:error r)))))

            (testing "§5.2: adding src and .md"
              (let [r (repo-import/add-repository-files! user repo-id commit-id wider)
                    after (rows commit-id)]
                (is (:ok r) (str (:error r)))
                (is (= commit-id (:topic-id r)))
                (testing "D2: src/main keeps its id, moves under src, and is retitled"
                  (is (= (get-in before ["src/main" :id]) (get-in after ["src/main" :id])))
                  (is (= "src" (get-in after ["src/main" :parent_path])))
                  (is (= "main" (get-in after ["src/main" :title]))))
                (testing "new files are inserted with their source"
                  (is (= "(ns core-test)\n" (get-in after ["src/test/core_test.clj" :source]))))
                (testing "D3: the old empty .md file is filled"
                  (is (= (get-in before ["src/main/notes.md" :id]) (get-in after ["src/main/notes.md" :id])))
                  (is (= "# Notes\n" (get-in after ["src/main/notes.md" :source]))))
                (testing "nothing outside src is added"
                  (is (not (contains? after "docs/guide.md"))))
                (testing "the stored selection is the wider one"
                  (is (= wider (:selection (db/get-repo-import user repo-id)))))))

            (testing "§5.2.2: the same selection again adds nothing and says so"
              (let [r (repo-import/add-repository-files! user repo-id commit-id wider)]
                (is (not (:ok r)))
                (is (re-find #"adds no files" (:error r)))))

            (testing "D4: a staged directory keeps its ordinal, and its paths are not brought back"
              (let [src-id (get-in (rows commit-id) ["src" :id])
                    test-file-id (get-in (rows commit-id) ["src/test/core_test.clj" :id])]
                ;; src is the commit's only child, at ordinal 1. The root's new
                ;; children are docs and README.md; docs sorts first.
                (db/stage-topic-for-deletion! user src-id)
                (let [r (repo-import/add-repository-files! user repo-id commit-id
                          {:dirs {"" true} :extensions #{".clj" ".md"}})
                      after (rows commit-id)]
                  (is (:ok r) (str (:error r)))
                  (is (= "# Guide\n" (get-in after ["docs/guide.md" :source])))
                  (is (= "# Fixture\n" (get-in after ["README.md" :source])))
                  (is (= test-file-id (get-in after ["src/test/core_test.clj" :id]))
                    "the staged file is the same row, not a new one"))))

            (testing "§5.1.3: with no live commit, Add files refuses"
              (db/delete-topic-for-user! user commit-id)
              (let [r (repo-import/add-files-target user repo-id)]
                (is (not (:ok r)))
                (is (re-find #"no imported commit" (:error r)))))))
        (finally
          (jdbc/execute! db/ds ["DELETE FROM users WHERE id = ?" user]))))))
