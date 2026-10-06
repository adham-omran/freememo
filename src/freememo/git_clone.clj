(ns freememo.git-clone
  "The blob-less bare clone of each imported repository
   (plans/commit-range-walkthrough.md §6.1).

   One clone per repository node, at `<clone-root>/<repo-node-id>.git`. It is a
   CACHE: a missing clone is made again on first use (§6.1.4), so a repository
   imported before clones existed, or one that arrived through account import,
   needs nothing special.

   Every git call goes through `git!`, which owns four rules:
     1. HTTPS only. `protocol.allow=never` and `protocol.https.allow=always`
        come from `protocols`, so `file://`, `ssh://` and `ext::` never reach
        the network (§6.1.2.1). A probe rebinds `protocols` to add `file`.
     2. The token travels in an `http.extraHeader` given through the
        `GIT_CONFIG_*` environment of that one call. It is never an argument,
        so no process listing shows it, and never config, so the clone's
        `config` file never holds it (§6.1.2.2).
     3. The user's and the system's git config are ignored, so no credential
        helper stores the token and no `url.insteadOf` rewrites the remote.
     4. A timeout, and a `cancelled?` fn read while git runs. Unlike an HTTP
        read, a git process CAN be stopped, so cancelling here is real.

   `with-lock` serializes the git operations on one clone (§6.1.2.3).

   Failures are `ex-info` with `:type :freememo.forge/forge-failure`, the type
   the import services already show to the user as is."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [freememo.config :as config]
            [freememo.forge :as forge]
            [taoensso.telemere :as tel])
  (:import [java.io ByteArrayOutputStream File InputStream OutputStream]
           [java.nio.charset StandardCharsets]
           [java.nio.file Files LinkOption]
           [java.util Base64]
           [java.util.concurrent ConcurrentHashMap TimeUnit]
           [java.util.concurrent.locks ReentrantLock]))

;; ── Configuration ──────────────────────────────────────────────────

(defn clone-root
  "The directory that holds every clone. FORGE_CLONE_ROOT, or
   `:deployment :forge-clone-root` in config.edn, else a directory under the
   JVM's temp dir. Production mounts one volume here for both app colors
   (§6.1.3), so a deploy that swaps colors finds the clones."
  ^File []
  (io/file (or (config/env-or-config "FORGE_CLONE_ROOT" [:deployment :forge-clone-root])
             (str (System/getProperty "java.io.tmpdir") File/separator "freememo-clones"))))

(def max-clone-bytes
  "Largest clone kept on disk (§6.1.6). FORGE_MAX_CLONE_BYTES overrides the
   2 GiB default. A clone past it is deleted and the import refused."
  (or (some-> (System/getenv "FORGE_MAX_CLONE_BYTES") parse-long)
    (* 2 1024 1024 1024)))

(def ^:private network-timeout-ms
  "Longest clone or fetch. A blob-less clone of a large history moves commits
   and trees only, so this is generous."
  (* 15 60 1000))

(def ^:private local-timeout-ms
  "Longest local read: ls-tree, log, diff over objects already on disk. A diff
   whose blobs are missing fetches them, which `network-timeout-ms` bounds."
  (* 2 60 1000))

(def ^:private max-output-bytes
  "Largest stdout one call may return. A diff of one commit past this is not
   something a reader can follow either."
  (* 256 1024 1024))

(def protocols
  "The transports git may use, as `protocol.<name>.allow=always`. Everything
   else is `never`. Only a test or a probe adds \"file\"."
  #{"https"})

;; ── Running git ────────────────────────────────────────────────────

(defn- failure [msg data]
  (ex-info msg (assoc data :type :freememo.forge/forge-failure)))

(defn- config-env
  "The `GIT_CONFIG_*` environment for one call: the protocol rules, no
   redirects, no credential helper, and the auth header when given."
  [auth-header]
  (let [pairs (cond-> [["protocol.allow" "never"]
                       ["http.followRedirects" "false"]
                       ["credential.helper" ""]
                       ["core.askPass" ""]
                       ["gc.auto" "0"]]
                :always (into (map (fn [p] [(str "protocol." p ".allow") "always"]) protocols))
                auth-header (conj ["http.extraHeader" auth-header]))]
    (into {"GIT_CONFIG_COUNT" (str (count pairs))}
      (mapcat (fn [i [k v]] [[(str "GIT_CONFIG_KEY_" i) k] [(str "GIT_CONFIG_VALUE_" i) v]])
        (range) pairs))))

(defn- drain
  "A future that reads `in` to the end. Past `limit` bytes it keeps reading
   and drops the rest, so git never blocks on a full pipe, and marks the
   result `:truncated?`."
  [^InputStream in limit]
  (future
    (let [out (ByteArrayOutputStream.)
          buf (byte-array 65536)]
      (loop [total 0 truncated? false]
        (let [n (.read in buf)]
          (if (neg? n)
            {:bytes (.toByteArray out) :truncated? truncated?}
            (let [room (max 0 (- limit total))]
              (.write out buf 0 (int (min n room)))
              (recur (+ total n) (or truncated? (> n room))))))))))

(defn git!
  "Run `git` with `args`.

   `opts`:
     :dir         the git dir (`--git-dir`), or nil for a call that makes one
     :auth        an HTTP header line, or nil
     :stdin       bytes or a string written to git's stdin, or nil
     :timeout-ms  default `local-timeout-ms`
     :cancelled?  a fn read every 200 ms; true stops git
   Post: {:exit n :out bytes :err string}. Never throws for a non-zero exit;
         the caller reads it. Throws ::forge-failure on a timeout, a cancel, or
         output past `max-output-bytes`, after the process is destroyed.
   Invariant: neither the args nor a log line carry `:auth`."
  [args {:keys [dir auth stdin timeout-ms cancelled?] :or {timeout-ms local-timeout-ms}}]
  (let [cmd (cond-> ["git"]
              dir (into ["--git-dir" (str dir)])
              :always (into args))
        pb (doto (ProcessBuilder. ^java.util.List cmd)
             (.redirectInput (if stdin
                               java.lang.ProcessBuilder$Redirect/PIPE
                               (java.lang.ProcessBuilder$Redirect/from (io/file "/dev/null")))))
        env (.environment pb)]
    (doseq [k ["GIT_DIR" "GIT_WORK_TREE" "GIT_INDEX_FILE" "GIT_ASKPASS" "SSH_ASKPASS"]]
      (.remove env k))
    (.putAll env (merge {"GIT_TERMINAL_PROMPT" "0"
                         "GIT_CONFIG_NOSYSTEM" "1"
                         "GIT_CONFIG_GLOBAL" "/dev/null"
                         "LC_ALL" "C"}
                   (config-env auth)))
    (let [proc (.start pb)
          out (drain (.getInputStream proc) max-output-bytes)
          err (drain (.getErrorStream proc) 65536)
          deadline (+ (System/currentTimeMillis) timeout-ms)
          stop! (fn [msg reason]
                  (.destroyForcibly proc)
                  (.waitFor proc 5 TimeUnit/SECONDS)
                  (throw (failure msg {:reason reason})))]
      (when stdin
        (future
          (try
            (with-open [^OutputStream os (.getOutputStream proc)]
              (.write os ^bytes (if (string? stdin) (.getBytes ^String stdin StandardCharsets/UTF_8) stdin)))
            (catch java.io.IOException _ nil))))
      (loop []
        (when-not (.waitFor proc 200 TimeUnit/MILLISECONDS)
          (cond
            (and cancelled? (cancelled?)) (stop! "The git operation was cancelled." :cancelled)
            (> (System/currentTimeMillis) deadline)
            (stop! (str "git took longer than " (quot timeout-ms 60000) " minutes.") :too-slow)
            :else (recur))))
      (let [{:keys [bytes truncated?]} @out]
        (when truncated?
          (throw (failure "The git output is too large to read." {:reason :too-large})))
        {:exit (.exitValue proc)
         :out bytes
         :err (String. ^bytes (:bytes @err) StandardCharsets/UTF_8)}))))

(defn- out-str [{:keys [out]}] (String. ^bytes out StandardCharsets/UTF_8))

(defn- user-message
  "Post: a message a user can act on for git's `stderr`, which names no token."
  [err what]
  (let [e (str/lower-case (str err))]
    (cond
      (or (str/includes? e "authentication failed") (str/includes? e "could not read username")
        (str/includes? e "401") (str/includes? e "403"))
      (str "The forge refused access to " what ". A private repository needs a token in Settings → Git Forges.")
      (or (str/includes? e "not found") (str/includes? e "404"))
      (str (str/capitalize what) " was not found.")
      (str/includes? e "unknown revision")
      (str "That commit is not in " what ".")
      (or (str/includes? e "could not resolve host") (str/includes? e "failed to connect")
        (str/includes? e "connection timed out"))
      (str "Could not reach the forge for " what ".")
      (str/includes? e "transport '")
      "FreeMemo reads repositories over HTTPS only."
      :else (str "git failed for " what ": " (first (str/split-lines (str/trim (str err))))))))

(defn- git-ok!
  "`git!`, then ::forge-failure for a non-zero exit. Post: the result."
  [args opts what]
  (let [r (git! args opts)]
    (when-not (zero? (:exit r))
      (tel/log! {:level :warn :id ::git-failed
                 :data {:args (first args) :exit (:exit r) :err (subs (:err r) 0 (min 500 (count (:err r))))}})
      (throw (failure (user-message (:err r) what) {:reason :git :exit (:exit r)})))
    r))

;; ── Remotes and credentials ────────────────────────────────────────

(defn remote-url
  "The HTTPS clone URL of repository `path` on `origin`.
   Throws ::forge-failure for an `http` origin (§6.1.2.1) or an address the
   forge gate refuses. A probe rebinds this fn to give a `file://` URL."
  [origin path]
  (when-not (= "https" (:scheme origin))
    (throw (failure "FreeMemo reads repositories over HTTPS only." {:reason :scheme})))
  (let [url (str (forge/origin-url origin) "/" path ".git")]
    (when-not (forge/address-permitted? url)
      (throw (failure (str "FreeMemo does not connect to " (:host origin)
                        ". The address is private or not valid.")
               {:reason :address})))
    url))

(defn auth-header
  "The `Authorization` header line for `token` on `forge`, or nil for no token.
   Each forge reads a token from HTTP Basic auth, with a different user name.
   P (§8, unknown 2): GitHub takes `x-access-token`, GitLab `oauth2`, and
   Forgejo the token as the user name with `x-oauth-basic` as the password."
  [forge token]
  (when-not (str/blank? token)
    (let [pair (case forge
                 "github" (str "x-access-token:" token)
                 "gitlab" (str "oauth2:" token)
                 "forgejo" (str token ":x-oauth-basic"))]
      (str "Authorization: Basic "
        (.encodeToString (Base64/getEncoder) (.getBytes pair StandardCharsets/UTF_8))))))

;; ── Locks and paths ────────────────────────────────────────────────

(defonce ^:private ^ConcurrentHashMap locks (ConcurrentHashMap.))

(defn with-lock*
  "Run `f` while holding the lock of git dir `dir` (§6.1.2.3). A second
   operation on the same clone waits for the first."
  [^File dir f]
  (let [^ReentrantLock lock (.computeIfAbsent locks (.getAbsolutePath dir)
                              (reify java.util.function.Function
                                (apply [_ _] (ReentrantLock.))))]
    (.lock lock)
    (try (f) (finally (.unlock lock)))))

(defmacro with-lock [dir & body] `(with-lock* ~dir (fn [] ~@body)))

(defn clone-dir
  "Post: the git dir of repository node `repo-id`'s clone. It may not exist."
  ^File [repo-id]
  (io/file (clone-root) (str repo-id ".git")))

(defn clone?
  "Post: true when `dir` holds a git dir."
  [^File dir]
  (.isFile (io/file dir "HEAD")))

(defn- dir-bytes [^File dir]
  (if (.exists dir)
    (with-open [paths (Files/walk (.toPath dir) (make-array java.nio.file.FileVisitOption 0))]
      (reduce (fn [acc ^java.nio.file.Path p]
                (if (Files/isRegularFile p (make-array LinkOption 0))
                  (+ acc (Files/size p))
                  acc))
        0 (iterator-seq (.iterator paths))))
    0))

(defn delete-dir!
  "Remove git dir `dir` and everything under it. Never follows a link out of
   it, and never throws. Post: nil."
  [^File dir]
  (try
    (when (Files/exists (.toPath dir) (make-array LinkOption 0))
      (with-open [paths (Files/walk (.toPath dir) (make-array java.nio.file.FileVisitOption 0))]
        (doseq [^java.nio.file.Path p (reverse (iterator-seq (.iterator paths)))]
          (try (Files/deleteIfExists p) (catch Exception _ nil)))))
    (catch Exception e
      (tel/log! {:level :warn :id ::delete-failed :data {:dir (str dir) :error (ex-message e)}})))
  nil)

(defn delete-clone!
  "Remove repository node `repo-id`'s clone (§6.1.5). Post: nil."
  [repo-id]
  (let [dir (clone-dir repo-id)]
    (with-lock dir (delete-dir! dir))))

(defn- check-size!
  "Throw ::forge-failure, after deleting `dir`, when the clone is past
   `max-clone-bytes` (§6.1.6)."
  [^File dir what]
  (let [n (dir-bytes dir)]
    (when (> n max-clone-bytes)
      (delete-dir! dir)
      (throw (failure (str (str/capitalize what) " is larger than "
                        (quot max-clone-bytes (* 1024 1024)) " MB without file contents, "
                        "so FreeMemo does not keep it.")
               {:reason :too-large :bytes n})))))

;; ── Clone and fetch ────────────────────────────────────────────────

(def ^:private refspecs ["+refs/heads/*:refs/heads/*" "+refs/tags/*:refs/tags/*"])

(defn clone!
  "Make a blob-less bare clone of `url` at `dir`.
   Pre : nothing is at `dir`. Post: `dir` is a clone whose branches and tags
         match the remote's, or nothing is at `dir` and ::forge-failure is
         thrown. The clone is made beside `dir` and renamed into place, so a
         failed or cancelled clone leaves nothing behind."
  [^File dir url {:keys [auth cancelled? what]}]
  (let [tmp (io/file (.getParentFile dir) (str ".tmp-" (random-uuid) ".git"))]
    (.mkdirs (.getParentFile dir))
    (try
      (git-ok! ["clone" "--bare" "--filter=blob:none" "--quiet" url (str tmp)]
        {:auth auth :cancelled? cancelled? :timeout-ms network-timeout-ms} what)
      (check-size! tmp what)
      (Files/move (.toPath tmp) (.toPath dir)
        (into-array java.nio.file.CopyOption [java.nio.file.StandardCopyOption/ATOMIC_MOVE]))
      dir
      (finally (delete-dir! tmp)))))

(defn fetch!
  "Bring `dir`'s branches and tags up to the remote's. An unchanged remote
   transfers no objects (§6.2.2). Post: nil, or ::forge-failure."
  [^File dir url {:keys [auth cancelled? what]}]
  (git-ok! (into ["fetch" "--prune" "--quiet" "--filter=blob:none" url] refspecs)
    {:dir dir :auth auth :cancelled? cancelled? :timeout-ms network-timeout-ms} what)
  (check-size! dir what)
  nil)

(defn ensure-fresh!
  "Clone `url` at `dir` when no clone is there (§6.1.4), else fetch.
   Pre : the caller holds `dir`'s lock. Post: `dir`."
  [^File dir url opts]
  (if (clone? dir)
    (do (fetch! dir url opts) dir)
    (do (delete-dir! dir) (clone! dir url opts))))

;; ── Reading ────────────────────────────────────────────────────────

(def ^:private sha-re #"[0-9a-f]{40}|[0-9a-f]{64}")

(defn resolve-ref
  "The commit `ref` names in `dir`, or the default branch's when `ref` is blank.
   Post: {:sha S :ref R}. Throws ::forge-failure when nothing matches."
  [^File dir ref what]
  (let [ref (or (some-> ref str/trim not-empty)
              (let [r (git! ["symbolic-ref" "--short" "HEAD"] {:dir dir})]
                (when (zero? (:exit r)) (not-empty (str/trim (out-str r)))))
              (throw (failure (str (str/capitalize what) " has no default branch. It may be empty.")
                       {:reason :empty})))
        r (git! ["rev-parse" "--verify" "--quiet" "--end-of-options" (str ref "^{commit}")] {:dir dir})
        sha (str/trim (out-str r))]
    (if (and (zero? (:exit r)) (re-matches sha-re sha))
      {:sha sha :ref ref}
      (throw (failure (str "Ref " ref " in " what " does not name a commit.")
               {:reason :no-commit})))))

(defn ls-tree
  "Every regular file of commit `sha`, without fetching a blob.
   Post: [{:path :oid}], in git's order. Symbolic links and submodules are
         left out: neither has text a reader can study."
  [^File dir sha]
  (let [out (out-str (git-ok! ["ls-tree" "-r" "-z" "--full-tree" sha] {:dir dir} "the commit"))]
    (into []
      (keep (fn [entry]
              (let [[meta path] (str/split entry #"\t" 2)
                    [mode type oid] (str/split meta #" ")]
                (when (and path (= "blob" type) (#{"100644" "100755"} mode))
                  {:path path :oid oid}))))
      (str/split out #"\u0000"))))

(defn missing-blobs
  "The blobs of `oids` that commit `sha` holds and `dir` lacks. Nothing is
   fetched: `rev-list --missing=print` names a missing object instead.
   Post: a vector of oids."
  [^File dir sha oids]
  (let [wanted (set oids)
        listed (git-ok! ["rev-list" "--objects" "--no-walk" "--missing=print" "--no-object-names" sha]
                 {:dir dir} "the commit")]
    (into []
      (keep (fn [line]
              (when (str/starts-with? line "?")
                (let [oid (subs line 1)] (when (wanted oid) oid)))))
      (str/split-lines (out-str listed)))))

(defn prefetch-blobs!
  "Fetch the blobs of `oids` that commit `sha` holds and `dir` lacks, in one
   request, the way git's own lazy fetch does. Post: nil. A clone that has
   every blob fetches nothing."
  [^File dir sha oids {:keys [auth cancelled? what]}]
  (let [missing (missing-blobs dir sha oids)]
    (when (seq missing)
      (git-ok! ["-c" "fetch.negotiationAlgorithm=noop" "fetch" "origin" "--no-tags"
                "--no-write-fetch-head" "--recurse-submodules=no" "--filter=blob:none" "--stdin"]
        {:dir dir :auth auth :cancelled? cancelled? :timeout-ms network-timeout-ms
         :stdin (str (str/join "\n" missing) "\n")}
        what))
    nil))

(defn blob-sizes
  "Post: {oid size} for the blobs of `oids`.
   Pre : the blobs are on disk (`prefetch-blobs!`). A missing one would be
         fetched alone, one request each."
  [^File dir oids]
  (let [r (git! ["cat-file" "--batch-check=%(objectname) %(objectsize)"]
            {:dir dir :stdin (str (str/join "\n" oids) "\n")})]
    (into {}
      (keep (fn [line]
              (let [[oid size] (str/split line #" ")]
                (when-let [n (some-> size parse-long)] [oid n]))))
      (str/split-lines (out-str r)))))

(defn- read-line-bytes
  "Post: the bytes of `in` up to the next newline, as a string, or nil at the
   end of the stream."
  [^java.io.BufferedInputStream in]
  (let [out (ByteArrayOutputStream.)]
    (loop []
      (let [b (.read in)]
        (cond
          (neg? b) (when (pos? (.size out)) (String. (.toByteArray out) StandardCharsets/UTF_8))
          (= 10 b) (String. (.toByteArray out) StandardCharsets/UTF_8)
          :else (do (.write out b) (recur)))))))

(defn with-blob-reader*
  "Call `f` with a fn of one oid that returns the blob's bytes, or nil when
   the blob is absent, all through ONE `git cat-file --batch` process. A
   process per file would cost more than the reads for a large tree.
   Pre : the blobs are on disk (`prefetch-blobs!`).
   Post: `f`'s value. The process is gone on every path.
   Invariant: heap holds one blob at a time."
  [^File dir f]
  (let [pb (doto (ProcessBuilder. ^java.util.List ["git" "--git-dir" (str dir) "cat-file" "--batch"])
             (.redirectError (java.lang.ProcessBuilder$Redirect/to (io/file "/dev/null"))))
        _ (.putAll (.environment pb) (merge {"GIT_TERMINAL_PROMPT" "0" "GIT_CONFIG_NOSYSTEM" "1"
                                             "GIT_CONFIG_GLOBAL" "/dev/null"}
                                       (config-env nil)))
        proc (.start pb)
        out (.getOutputStream proc)
        in (java.io.BufferedInputStream. (.getInputStream proc) 65536)
        read-blob (fn [oid]
                    (.write out (.getBytes (str oid "\n") StandardCharsets/UTF_8))
                    (.flush out)
                    (let [header (read-line-bytes in)
                          [_ type size] (some-> header (str/split #" "))]
                      (if (and (= "blob" type) size)
                        (let [n (parse-long size)
                              buf (byte-array n)]
                          (loop [off 0]
                            (when (< off n)
                              (let [k (.read in buf off (- n off))]
                                (when (neg? k) (throw (ex-info "cat-file ended early" {})))
                                (recur (+ off k)))))
                          (.read in)
                          buf)
                        (do (when (and type (not= "missing" type))
                              ;; Some other object: skip its body and newline.
                              (when-let [n (some-> size parse-long)]
                                (.skipNBytes in (inc n))))
                            nil))))]
    (try
      (f read-blob)
      (finally
        (try (.close out) (catch Exception _ nil))
        (when-not (.waitFor proc 5 TimeUnit/SECONDS)
          (.destroyForcibly proc))))))

(defn blob
  "Post: the bytes of blob `oid` in `dir`, or nil when it is absent."
  ^bytes [^File dir oid]
  (let [r (git! ["cat-file" "blob" oid] {:dir dir})]
    (when (zero? (:exit r)) (:out r))))

(defn file-at
  "Post: the bytes of `path` at commit `sha`, or nil when the commit has no
   such file. A missing blob is fetched on demand through the clone's
   promisor remote, so `auth` must be given for a private repository."
  ^bytes [^File dir sha path {:keys [auth]}]
  (let [r (git! ["cat-file" "blob" (str sha ":" path)]
            {:dir dir :auth auth :timeout-ms network-timeout-ms})]
    (when (zero? (:exit r)) (:out r))))

(defn ancestor?
  "Post: true when commit `a` is an ancestor of commit `b`, or equal to it."
  [^File dir a b]
  (zero? (:exit (git! ["merge-base" "--is-ancestor" a b] {:dir dir}))))

(defn commit-exists?
  "Post: true when `sha` names a commit in `dir`."
  [^File dir sha]
  (zero? (:exit (git! ["cat-file" "-e" (str sha "^{commit}")] {:dir dir}))))

(def ^:private field-sep "\u001f")
(def ^:private record-sep "\u001e")

(defn log
  "Commits of `dir` as `git log` gives them for `args`.
   Post: [{:sha :parents [..] :author :date :subject :body}]."
  [^File dir args]
  (let [fmt (str/join field-sep ["%H" "%P" "%an" "%aI" "%s" "%b"])
        out (out-str (git-ok! (into ["log" (str "--format=" record-sep fmt)] args)
                       {:dir dir} "the commit history"))]
    (into []
      (keep (fn [rec]
              (let [[sha parents author date subject body] (str/split rec (re-pattern field-sep) -1)]
                (when (and sha (re-matches sha-re (str/trim sha)))
                  {:sha (str/trim sha)
                   :parents (vec (remove str/blank? (str/split (str parents) #" ")))
                   :author author :date date :subject subject
                   :body (str/trim (str body))}))))
      (str/split out (re-pattern record-sep)))))

(defn changed-paths
  "The paths commit `sha` changes against its first parent, or against the
   empty tree when it has none. Trees only, so no blob is fetched.
   Pre : `parents` are `sha`'s parents, as `log` gives them.
   Post: a vector of paths."
  [^File dir sha parents]
  (let [args (if-let [p (first parents)]
               ["diff-tree" "-r" "--name-only" "--no-renames" p sha]
               ["diff-tree" "-r" "--root" "--name-only" "--no-renames" "--no-commit-id" sha])]
    (into [] (remove str/blank?)
      (str/split-lines (out-str (git-ok! args {:dir dir} "the commit's files"))))))

(defn diff
  "The patch from commit `from` to commit `to`, with renames found (§6.4.3).
   Missing blobs are fetched in one batch by git's own diff prefetch.
   Post: the patch text."
  [^File dir from to {:keys [auth]}]
  (out-str (git-ok! ["diff-tree" "-r" "-p" "-M" "--no-color" "--no-ext-diff" "--full-index"
                     "--src-prefix=a/" "--dst-prefix=b/" from to]
             {:dir dir :auth auth :timeout-ms network-timeout-ms} "the commit diff")))

(defn attributes
  "The `linguist-vendored`, `linguist-generated` and `diff` attributes of
   `paths` at commit `sha`, read from that commit's `.gitattributes`.
   Post: {path #{:vendored :generated :binary}}; a path with none is absent."
  [^File dir sha paths {:keys [auth]}]
  (if (empty? paths)
    {}
    (let [r (git! ["check-attr" (str "--source=" sha) "-z" "--stdin"
                   "linguist-vendored" "linguist-generated" "diff"]
              {:dir dir :auth auth :timeout-ms network-timeout-ms
               :stdin (str (str/join "\u0000" paths) "\u0000")})
          parts (str/split (out-str r) #"\u0000")]
      (if-not (zero? (:exit r))
        {}
        (reduce (fn [acc [path attr value]]
                  (let [flag (case attr
                               "linguist-vendored" (when (#{"set" "true"} value) :vendored)
                               "linguist-generated" (when (#{"set" "true"} value) :generated)
                               "diff" (when (= "unset" value) :binary)
                               nil)]
                    (if flag (update acc path (fnil conj #{}) flag) acc)))
          {} (partition 3 parts))))))
