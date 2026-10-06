(ns freememo.repo-import-clone-test
  "The snapshot import on the clone (plans/commit-range-walkthrough.md §6.2).

   The first test needs no database: it compares what the removed archive path
   read from a forge zip with what the clone path reads, over one fixture
   repository (§6.2.1). The others import through the clone into PostgreSQL
   and skip when it is down."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [freememo.archive :as archive]
            [freememo.db :as db]
            [freememo.git-clone :as git]
            [freememo.git-clone-test :as fixture]
            [freememo.repo-import :as repo-import]
            [freememo.repo-tree :as tree]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs])
  (:import [java.io File]
           [java.nio.file Files]))

(def ^:private commits
  [{"README.md" "# Fixture\n\nSee [the spec](docs/spec.md).\n"
    "docs/spec.md" "# Spec\n\nA port declares a signature.\n"
    "src/ir.zig" "const Op = enum { cycle, hold };\n"
    "src/deep/nested/x.zig" "pub fn x() void {}\n"
    "bin/run.sh" "#!/bin/sh\necho run\n"
    "assets/blob.bin" (String. (byte-array [0 1 2 3 0 255]) "ISO-8859-1")
    :message "feat: first"}])

(defn- archive-read
  "What the removed archive path read: the zip's single top directory, every
   regular file under it, and each file's bytes."
  [^File zip]
  (let [dir (archive/extract-to-temp-dir! zip {:prefix "fixture" :escaping-entry :refuse})]
    (try
      (let [children (vec (.listFiles dir))
            root (if (and (= 1 (count children)) (.isDirectory ^File (first children)))
                   (first children) dir)
            base (.toPath ^File root)]
        (into {}
          (comp (filter #(.isFile ^File %))
            (map (fn [^File f]
                   [(str/replace (str (.relativize base (.toPath f))) File/separator "/")
                    (vec (Files/readAllBytes (.toPath f)))])))
          (file-seq root)))
      (finally (archive/delete-dir! dir)))))

(defn- clone-read
  "What the clone path reads for commit `sha`: `ls-tree`, then the blobs."
  [^File dir sha]
  (let [entries (git/ls-tree dir sha)]
    (git/prefetch-blobs! dir sha (map :oid entries) {:what "the fixture"})
    (git/with-blob-reader* dir
      (fn [read-blob]
        (into {} (map (fn [{:keys [path oid]}] [path (vec (read-blob oid))])) entries)))))

(deftest the-clone-reads-what-the-archive-read
  (let [{repo :dir [sha] :shas} (fixture/fixture-repo! commits)
        zip (io/file (fixture/temp-dir "zip") "repo.zip")
        _ (let [p (.start (doto (ProcessBuilder. ["git" "archive" "--format=zip"
                                                  (str "--prefix=owner-name-" (subs sha 0 7) "/")
                                                  "-o" (str zip) sha])
                            (.directory repo)))]
            (.waitFor p))
        dir (io/file (fixture/temp-dir "clones") "1.git")]
    (let [from-archive (archive-read zip)
          from-clone (fixture/with-file-protocol
                       (git/clone! dir (str "file://" repo) {:what "the fixture"})
                       (clone-read dir sha))]
      (testing "§6.2.1: the same paths give the same topic tree"
        (is (= (tree/tree (keys from-archive)) (tree/tree (keys from-clone)))))
      (testing "§6.2.1: each path has the same bytes"
        (is (= from-archive from-clone))))))

;; ── Through the database ───────────────────────────────────────────

(defn- db-up? []
  (try (jdbc/execute-one! db/ds ["SELECT 1"]) true (catch Exception _ false)))

(defn- create-user! []
  (:id (jdbc/execute-one! db/ds
         ["INSERT INTO users (username, email) VALUES (?, ?) RETURNING id"
          (str "repo-clone-test-" (random-uuid)) (str (random-uuid) "@example.test")]
         {:builder-fn rs/as-unqualified-maps})))

(defmacro ^:private with-fixture-forge
  "Run `body` with a user bound to `user-sym`, a temp clone root, and every
   remote URL pointing at `repo-dir`."
  [[user-sym repo-dir] & body]
  `(if-not (db-up?)
     (is true "PostgreSQL is unavailable; integration assertion skipped")
     (let [~user-sym (create-user!)
           root# (fixture/temp-dir "clone-root")]
       (try
         (with-redefs [git/protocols #{"https" "file"}
                       git/clone-root (fn [] root#)
                       git/remote-url (fn [_# _#] (str "file://" ~repo-dir))]
           ~@body)
         (finally
           (jdbc/execute! db/ds ["DELETE FROM users WHERE id = ?" ~user-sym]))))))

(def ^:private form {:forge "gitlab" :host "gitlab.example.com" :repository "owner/name"})

(def ^:private selection {:dirs {"" true} :extensions #{".md" ".zig"}})

(defn- sources [commit-id]
  (into {}
    (map (juxt :repo_path :source))
    (jdbc/execute! db/ds
      ["WITH RECURSIVE sub AS (SELECT id FROM topics WHERE id = ?
                               UNION ALL SELECT t.id FROM topics t JOIN sub ON t.parent_id = sub.id)
        SELECT t.repo_path, tc.source FROM sub JOIN topics t ON t.id = sub.id
          JOIN topic_code tc ON tc.topic_id = t.id"
       commit-id]
      {:builder-fn rs/as-unqualified-maps})))

(deftest an-import-and-its-updates-run-on-the-clone
  (let [{repo :dir} (fixture/fixture-repo! commits)]
    (with-fixture-forge [user repo]
      (let [staged (repo-import/stage-repository! user form)
            _ (is (:ok staged) (str (:error staged)))
            r (repo-import/import-staged! user (:stage-id staged) selection)
            repo-id (:id (jdbc/execute-one! db/ds ["SELECT parent_id AS id FROM topics WHERE id = ?"
                                                   (:topic-id r)]
                           {:builder-fn rs/as-unqualified-maps}))]
        (is (:ok r) (str (:error r)))
        (testing "the selected files are stored from the clone"
          (is (= {"README.md" "# Fixture\n\nSee [the spec](docs/spec.md).\n"
                  "docs/spec.md" "# Spec\n\nA port declares a signature.\n"
                  "src/ir.zig" "const Op = enum { cycle, hold };\n"
                  "src/deep/nested/x.zig" "pub fn x() void {}\n"}
                (sources (:topic-id r)))))
        (testing "§6.1.2: the import leaves exactly one clone, at the node's id"
          (is (git/clone? (git/clone-dir repo-id)))
          (is (= [(str repo-id ".git")]
                (vec (remove #(str/starts-with? % ".") (.list ^File (git/clone-root)))))))
        (testing "§6.2.2: an unchanged ref adds nothing"
          (let [u (repo-import/update-repository! user repo-id)]
            (is (:unchanged? u))
            (is (= (:topic-id r) (:topic-id u)))))
        (testing "an update adds the new commit"
          (spit (io/file repo "docs/spec.md") "# Spec\n\nA port declares a type.\n")
          (fixture/sh! repo "git" "commit" "-q" "-am" "docs: type")
          (let [u (repo-import/update-repository! user repo-id)]
            (is (:ok u) (str (:error u)))
            (is (not (:unchanged? u)))
            (is (= "# Spec\n\nA port declares a type.\n" (get (sources (:topic-id u)) "docs/spec.md")))))
        (testing "§6.1.4: a missing clone is made again"
          (git/delete-clone! repo-id)
          (is (not (git/clone? (git/clone-dir repo-id))))
          (let [u (repo-import/update-repository! user repo-id)]
            (is (:unchanged? u))
            (is (git/clone? (git/clone-dir repo-id)))))))))

(defn- import-fixture!
  "Import the fixture for `user`. Post: the repository node id."
  [user]
  (let [staged (repo-import/stage-repository! user form)
        r (repo-import/import-staged! user (:stage-id staged) selection)]
    (:id (jdbc/execute-one! db/ds ["SELECT parent_id AS id FROM topics WHERE id = ?" (:topic-id r)]
           {:builder-fn rs/as-unqualified-maps}))))

(deftest deleting-a-repository-deletes-its-clone
  (let [{repo :dir} (fixture/fixture-repo! commits)]
    (with-fixture-forge [user repo]
      (testing "§6.1.5: the hard-delete path"
        (let [repo-id (import-fixture! user)]
          (is (git/clone? (git/clone-dir repo-id)))
          (db/delete-topic-for-user! user repo-id)
          (is (not (.exists (git/clone-dir repo-id))))))
      (testing "§6.1.5: staging keeps the clone, the purge removes it"
        (let [repo-id (import-fixture! user)
              {:keys [entry-id]} (db/stage-topic-for-deletion! user repo-id)]
          (is (git/clone? (git/clone-dir repo-id)) "the undo window keeps the clone")
          (jdbc/execute! db/ds ["UPDATE undo_log SET occurred_at = now() - interval '13 hours'
                                  WHERE id = ?" entry-id])
          (db/purge-staged-documents!)
          (is (not (.exists (git/clone-dir repo-id)))))))))
