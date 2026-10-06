(ns freememo.git-clone-test
  "The git transport against a local fixture repository
   (plans/commit-range-walkthrough.md §6.1). No database and no network:
   `file://` is allowed here only, through `with-file-protocol`."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [freememo.git-clone :as git])
  (:import [java.io File]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn sh! [^File dir & args]
  (let [p (-> (ProcessBuilder. ^java.util.List (vec args))
            (.directory dir)
            (.redirectErrorStream true))
        env (.environment p)]
    (.putAll env {"GIT_CONFIG_NOSYSTEM" "1" "GIT_CONFIG_GLOBAL" "/dev/null"
                  "GIT_AUTHOR_NAME" "Fixture" "GIT_AUTHOR_EMAIL" "f@example.com"
                  "GIT_COMMITTER_NAME" "Fixture" "GIT_COMMITTER_EMAIL" "f@example.com"
                  "GIT_AUTHOR_DATE" "2026-10-01T10:00:00Z" "GIT_COMMITTER_DATE" "2026-10-01T10:00:00Z"})
    (let [proc (.start p)
          out (slurp (.getInputStream proc))]
      (.waitFor proc)
      (when-not (zero? (.exitValue proc))
        (throw (ex-info (str "fixture command failed: " args "\n" out) {})))
      (str/trim out))))

(defn temp-dir ^File [prefix]
  (.toFile (Files/createTempDirectory prefix (make-array FileAttribute 0))))

(defn fixture-repo!
  "A work tree with `commits`, each a map of path -> text (nil deletes) and a
   :message. Post: {:dir File :shas [sha …]}. The repository serves blob
   filters and blob wants, as a forge does."
  [commits]
  (let [dir (temp-dir "fixture-repo")]
    (sh! dir "git" "init" "-q" "-b" "main")
    (sh! dir "git" "config" "uploadpack.allowFilter" "true")
    (sh! dir "git" "config" "uploadpack.allowAnySHA1InWant" "true")
    {:dir dir
     :shas (mapv (fn [{:keys [message] :as c}]
                   (doseq [[path text] (dissoc c :message)]
                     (let [f (io/file dir path)]
                       (if (nil? text)
                         (sh! dir "git" "rm" "-q" path)
                         (do (io/make-parents f) (spit f text) (sh! dir "git" "add" path)))))
                   (sh! dir "git" "commit" "-q" "--allow-empty" "-m" message)
                   (sh! dir "git" "rev-parse" "HEAD"))
             commits)}))

(defmacro with-file-protocol [& body]
  `(with-redefs [git/protocols #{"https" "file"}] ~@body))

(defn- url [{:keys [dir]}] (str "file://" (.getAbsolutePath ^File dir)))

(deftest a-clone-holds-history-and-no-blobs
  (let [repo (fixture-repo! [{"a.md" "# A\n" "src/x.zig" "const x = 1;\n" :message "feat: first"}
                             {"a.md" "# A\n\nMore.\n" :message "docs: more\n\nWhy it matters."}])
        dir (io/file (temp-dir "clones") "1.git")]
    (with-file-protocol
      (git/clone! dir (url repo) {:what "the fixture"})
      (testing "the clone is bare and has every commit"
        (is (git/clone? dir))
        (is (= (last (:shas repo)) (:sha (git/resolve-ref dir nil "the fixture"))))
        (is (= "main" (:ref (git/resolve-ref dir nil "the fixture")))))
      (testing "ls-tree lists files without fetching them"
        (let [files (git/ls-tree dir (last (:shas repo)))]
          (is (= #{"a.md" "src/x.zig"} (set (map :path files))))
          (is (= 2 (count (git/missing-blobs dir (last (:shas repo)) (map :oid files))))
            "no blob is on disk yet")
          (git/prefetch-blobs! dir (last (:shas repo)) (map :oid files) {:what "the fixture"})
          (is (empty? (git/missing-blobs dir (last (:shas repo)) (map :oid files))))
          (is (= 2 (count (git/blob-sizes dir (map :oid files)))))
          (is (= "# A\n\nMore.\n"
                (String. (git/blob dir (:oid (first (filter #(= "a.md" (:path %)) files)))) "UTF-8")))))
      (testing "the log carries message, body and parents"
        (let [[c1 c2] (:shas repo)
              [entry] (git/log dir ["--first-parent" (str c1 ".." c2)])]
          (is (= c2 (:sha entry)))
          (is (= [c1] (:parents entry)))
          (is (= "docs: more" (:subject entry)))
          (is (= "Why it matters." (:body entry)))
          (is (git/ancestor? dir c1 c2))
          (is (not (git/ancestor? dir c2 c1)))))
      (testing "a diff fetches what it needs"
        (let [[c1 c2] (:shas repo)]
          (is (str/includes? (git/diff dir c1 c2 {}) "+More.")))))))

(deftest file-urls-are-refused-outside-tests
  (let [repo (fixture-repo! [{"a.md" "x\n" :message "one"}])
        dir (io/file (temp-dir "clones") "2.git")]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"HTTPS only|git failed"
          (git/clone! dir (url repo) {:what "the fixture"})))
    (is (not (.exists dir)) "a refused clone leaves nothing")))

(deftest the-token-never-reaches-disk
  (let [repo (fixture-repo! [{"a.md" "x\n" :message "one"}])
        dir (io/file (temp-dir "clones") "3.git")
        token "glpat-secret-token-value"]
    (with-file-protocol
      (git/clone! dir (url repo) {:what "the fixture" :auth (git/auth-header "gitlab" token)})
      (git/fetch! dir (url repo) {:what "the fixture" :auth (git/auth-header "gitlab" token)}))
    (let [config (slurp (io/file dir "config"))]
      (is (not (str/includes? config token)))
      (is (not (str/includes? config "extraHeader"))))))

(deftest auth-headers-differ-per-forge
  (is (nil? (git/auth-header "github" "")))
  (doseq [forge ["github" "gitlab" "forgejo"]]
    (is (str/starts-with? (git/auth-header forge "t") "Authorization: Basic "))))

(deftest an-http-origin-is-refused
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"HTTPS only"
        (git/remote-url {:scheme "http" :host "example.com"} "o/n"))))

(deftest operations-on-one-clone-run-one-after-the-other
  (let [dir (io/file (temp-dir "clones") "4.git")
        !inside (atom 0)
        !max (atom 0)
        run (fn [] (git/with-lock dir
                     (swap! !max max (swap! !inside inc))
                     (Thread/sleep 50)
                     (swap! !inside dec)))]
    (run! deref [(future (run)) (future (run)) (future (run))])
    (is (= 1 @!max))))
