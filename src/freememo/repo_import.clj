(ns freememo.repo-import
  "Import one commit of a git forge repository as a topic tree, and update a
   repository to a newer commit (plans/git-forge-integration.md §4.3, §4.6).

   Two phases, because the picker needs the whole tree before any row exists:

     stage-repository!  resolve the ref, download the archive, extract it, and
                        record the directory as an upload session
     import-staged!     apply the user's selection, create the rows, release
                        the session

   `update-repository!` runs both at once with the selection stored on the
   repository node, so an update shows no picker (§4.6 6.2).

   The rows: one `repo` node per repository URL, one `commit` node per sha under
   it, one `dir` node per selected directory, one `code` node per file inside a
   selected directory. Only `code` nodes are review subjects; the other three
   are structure (`db/structural-kinds`). A file's source goes to `topic_code`,
   never to `topics.content`, which holds the user's notes and starts empty.

   Activity (§4.3 3.8): `:importing-repos` maps a work key to its phase while
   work runs, and `:repo-import-errors` keeps the failure the reporter reads
   when it closes the row."
  (:require [clojure.string :as str]
            [next.jdbc :as jdbc]
            [freememo.archive :as archive]
            [freememo.commands :as commands]
            [freememo.db :as db]
            [freememo.forge :as forge]
            [freememo.forge-kinds :as kinds]
            [freememo.forge-tokens :as tokens]
            [freememo.input-check :as input]
            [freememo.priority :as priority]
            [freememo.repo-tree :as tree]
            [freememo.user-state :as us]
            [taoensso.telemere :as tel])
  (:import [java.io File]
           [java.nio ByteBuffer]
           [java.nio.charset CharacterCodingException CodingErrorAction StandardCharsets]
           [java.nio.file Files]))

;; ── Limits ─────────────────────────────────────────────────────────

(def extract-limits
  "The bounds of `epub/default-limits` (§4.2 2.4). An entry that escapes the
   extraction directory fails the whole archive instead of being skipped: no
   forge produces one, so an archive that holds one is hostile."
  {:prefix "forge-import"
   :max-entries 50000
   :max-entry-bytes (* 64 1024 1024)
   :max-inflation-ratio 10
   :max-total-bytes (* 8 1024 1024 1024)
   :escaping-entry :refuse})

(def max-source-bytes
  "Largest file whose source is stored. A larger file becomes an empty leaf:
   the pane would highlight it slowly and a card could not use it whole."
  (* 1024 1024))

(def max-import-source-bytes
  "Most source text one commit subtree stores. The import charges no quota
   (§4.3 3.7), so this is what bounds the database growth of one click. Files
   past it become empty leaves and the report counts them."
  (* 512 1024 1024))

(def ^:private statement-source-bytes
  "Source bytes per INSERT statement."
  (* 16 1024 1024))

;; ── Activity ───────────────────────────────────────────────────────

(defn- work-key [origin path]
  (str (:host origin) "/" path))

(defn phase-in
  "Post: the phase of the running import or update of `repo-url` in `phases` —
   the value of `:importing-repos` — or nil when none runs. The work key
   encoding stays in this namespace; a reader passes the URL."
  [phases repo-url]
  (when-let [{:keys [origin path]} (some-> repo-url forge/parse-repository-input)]
    (get phases (work-key origin path))))

(defn- set-phase! [user-id key phase]
  (swap! (us/get-atom user-id :importing-repos) assoc key phase))

(defn- run-as-activity
  "Run `f` while `key` is published in `:importing-repos`.

   Post: `f`'s value. A result carrying `:ok false`, or a throw, records its
         message in `:repo-import-errors` BEFORE membership drops, so the
         reporter closes the row as failed; any other result clears the entry.
   Invariant: membership is removed on every path."
  [user-id key phase f]
  (let [errors (us/get-atom user-id :repo-import-errors)]
    (set-phase! user-id key phase)
    (us/work-started! user-id)
    (try
      (let [r (f)]
        (if (false? (:ok r))
          (swap! errors assoc key (:error r))
          (swap! errors dissoc key))
        r)
      (catch Throwable t
        (swap! errors assoc key (or (.getMessage t) "Import failed"))
        (throw t))
      (finally
        (swap! (us/get-atom user-id :importing-repos) dissoc key)))))

(defn- failure-message
  "Post: a message for the user. An unexpected throw is logged here, once."
  [^Throwable t]
  (let [type (:type (ex-data t))]
    (cond
      (forge/failure? t) (.getMessage t)
      (= :freememo.archive/archive-too-large type)
      "The repository archive is too large to extract."
      (= :freememo.archive/unsafe-entry type)
      "The archive holds a path outside the repository, so FreeMemo refused it."
      :else
      (do (tel/error! {:id ::repo-import-failed} t)
          "The import failed. The server log has the details."))))

;; ── The extracted tree ─────────────────────────────────────────────

(defn- archive-root
  "The directory the repository's paths are relative to. All three forges wrap a
   commit in one top-level directory named after the repository and the sha,
   and that wrapper is not part of the repository."
  ^File [^File dir]
  (let [children (vec (.listFiles dir))]
    (if (and (= 1 (count children)) (.isDirectory ^File (first children)))
      (first children)
      dir)))

(defn- file-paths
  "Post: every regular file under `root`, as repository-relative `/` paths."
  [^File root]
  (let [base (.toPath root)]
    (->> (file-seq root)
      (filter #(.isFile ^File %))
      (mapv #(str/replace (str (.relativize base (.toPath ^File %))) File/separator "/")))))

(defn- fetch-tree!
  "Download and extract the archive of `sha`.
   Post: the extracted directory. The caller owns it and MUST delete it. The
         archive file is gone on every path (§4.3 3.7)."
  ^File [user-id key forge origin token path sha]
  (set-phase! user-id key "downloading")
  (let [zip (forge/download-archive! forge origin token path sha)]
    (try
      (set-phase! user-id key "extracting")
      (archive/extract-to-temp-dir! zip extract-limits)
      (finally
        (.delete ^File zip)))))

(defn- utf8-text
  "Post: the text of `bytes` when it is valid UTF-8 and holds no NUL, else nil.
   PostgreSQL TEXT cannot store NUL, and a binary file shown as text helps no
   one, so both become empty leaves."
  [^bytes bytes]
  (when-not (loop [i 0]
              (cond
                (= i (alength bytes)) false
                (zero? (aget bytes i)) true
                :else (recur (inc i))))
    (try
      (str (.decode (doto (.newDecoder StandardCharsets/UTF_8)
                      (.onMalformedInput CodingErrorAction/REPORT)
                      (.onUnmappableCharacter CodingErrorAction/REPORT))
             (ByteBuffer/wrap bytes)))
      (catch CharacterCodingException _ nil))))

;; ── Rows ───────────────────────────────────────────────────────────

(defn- clamp-title [s]
  (let [s (if (str/blank? s) "(unnamed)" s)]
    (if (> (count s) input/title-max) (subs s 0 input/title-max) s)))

(defn- commit-title
  "`a1b2c3d · main`. The sha is the identity (frame: a branch moves); the ref
   records what the user asked for, and is dropped when it is the sha itself."
  [sha ref]
  (str (subs sha 0 7)
    (when (and ref (not (str/starts-with? sha ref)))
      (str " · " ref))))

(defn- partition-by-bytes
  "Post: a lazy seq of vectors that concatenate to `rows`. Each holds at most
   `limit` bytes of `:byte_size`, unless one row alone exceeds it."
  [limit rows]
  (lazy-seq
    (when (seq rows)
      (loop [batch [] bytes 0 rs rows]
        (let [r (first rs)]
          (if (and r (or (empty? batch) (<= (+ bytes (long (:byte_size r))) limit)))
            (recur (conj batch r) (+ bytes (long (:byte_size r))) (rest rs))
            (cons batch (partition-by-bytes limit rs))))))))

(defn- normalize-selection
  "Pre : `selection` came from the client.
   Post: {:dirs {String Boolean} :extensions #{String}}; anything else dropped."
  [{:keys [dirs extensions]}]
  {:dirs (into {} (keep (fn [[k v]] (when (and (string? k) (boolean? v)) [k v]))) dirs)
   :extensions (into #{} (filter #(and (string? %) (or (= "" %) (str/starts-with? % "."))))
                 extensions)})

(defn- create-commit-subtree!
  "Write one commit subtree, and the repository node when it does not exist.

   Pre : `root` holds the extracted commit. `source` is
         {:forge :repo-url :path :ref :sha}. `selection` is normalized.
         `repo-id` is the existing repository node, or nil for a first import.
   Post: {:ok true :topic-id commit-id :repo-id N :report {…}}, or
         {:ok false :error S} with nothing written — when a node cap refuses
         (§4.3 3.9, 3.9.1) or the sha is already a child of the repository.
   Invariant: every row commits in one transaction."
  [user-id ^File root source selection repo-id]
  (let [{:keys [forge repo-url path ref sha]} source
        commit-tree (tree/tree (file-paths root))
        plan (tree/import-plan commit-tree (:dirs selection))
        nodes (tree/subtree-nodes plan (:dirs selection))
        new-count (+ (count nodes) 1 (if repo-id 0 1))
        existing-count (if repo-id (db/count-subtree-topics repo-id) 0)]
    (cond
      (> new-count tree/max-selection-nodes)
      {:ok false
       :error (str "This selection creates " new-count " topics. One import may create "
                tree/max-selection-nodes ". Select fewer directories.")}

      (> (+ existing-count new-count) tree/max-repository-nodes)
      {:ok false
       :error (str "This commit adds " new-count " topics to a repository that holds "
                existing-count ". One repository may hold " tree/max-repository-nodes ".")}

      :else
      (let [;; The repository id first, so ids ascend from root to leaf.
            repo-id* (or repo-id (first (db/reserve-topic-ids! 1)))
            [commit-id & node-ids] (db/reserve-topic-ids! (inc (count nodes)))
            id-by-path (zipmap (map :path nodes) node-ids)
            base {:user_id user-id :status "active" :priority priority/priority-default}
            topic-rows (cond-> []
                         (nil? repo-id)
                         (conj (assoc base :id repo-id* :kind "repo"
                                 :title (clamp-title path) :repo_url repo-url))
                         :always
                         (conj (assoc base :id commit-id :kind "commit" :parent_id repo-id*
                                 :title (commit-title sha ref) :commit_sha sha :repo_path ""))
                         :always
                         (into (map (fn [{:keys [kind path parent title ordinal]}]
                                      (assoc base :id (id-by-path path) :kind kind
                                        :parent_id (if parent (id-by-path parent) commit-id)
                                        :title (clamp-title title) :repo_path path
                                        :page_number ordinal)))
                           nodes))
            extensions (:extensions selection)
            !report (atom {:directories (count (:dirs plan)) :files (count (:files plan))
                           :sources-stored 0 :binary-files 0 :oversize-files 0
                           :over-budget-files 0})
            !budget (atom 0)
            ;; Lazy, and realized once inside the transaction, so at most one
            ;; statement's worth of source is in heap at a time.
            code-rows (keep (fn [p]
                              (let [f (File. root ^String p)
                                    len (.length f)]
                                (cond
                                  (> len max-source-bytes)
                                  (do (swap! !report update :oversize-files inc) nil)

                                  (> (+ @!budget len) max-import-source-bytes)
                                  (do (swap! !report update :over-budget-files inc) nil)

                                  :else
                                  (if-let [text (utf8-text (Files/readAllBytes (.toPath f)))]
                                    (do (swap! !budget + len)
                                        (swap! !report update :sources-stored inc)
                                        {:topic_id (id-by-path p) :source text
                                         :byte_size len :lang (tree/language-for p)})
                                    (do (swap! !report update :binary-files inc) nil)))))
                        (filter #(contains? extensions (tree/extension %)) (:files plan)))]
        (jdbc/with-transaction [tx db/ds]
          (if (and repo-id (db/find-commit-node tx repo-id sha))
            {:ok false :error (str "Commit " (subs sha 0 7) " is already imported.")}
            (do
              (db/insert-topic-rows! tx topic-rows)
              (doseq [batch (partition-by-bytes statement-source-bytes code-rows)]
                (db/insert-topic-code-rows! tx batch))
              (when (nil? repo-id)
                (db/insert-repo-selection! tx repo-id* forge ref selection))
              {:ok true :topic-id commit-id :repo-id repo-id*
               :report (assoc @!report :topics-created new-count)})))))))

;; ── Services ───────────────────────────────────────────────────────

(defn- read-import-form
  "Post: {:ok true :forge :origin :path :ref} or {:ok false :error S}. A host in
   a pasted URL wins over the host field."
  [{:keys [forge host repository ref]}]
  (let [parsed (forge/parse-repository-input repository)
        origin (or (:origin parsed)
                 (forge/parse-origin (or (some-> host str/trim not-empty)
                                       (kinds/default-host forge))))]
    (cond
      (not (contains? kinds/forge-ids forge))
      {:ok false :error "Choose GitHub, GitLab or Forgejo."}

      (nil? parsed)
      {:ok false :error "Enter a repository as owner/name, or paste its URL."}

      (nil? origin)
      {:ok false :error "Enter the forge host, for example gitlab.com."}

      (not (forge/valid-repository-path? forge (:path parsed)))
      {:ok false :error (str "\"" (:path parsed) "\" is not a " (kinds/forge-label forge)
                          " repository path. Use owner/name.")}

      :else
      {:ok true :forge forge :origin origin :path (:path parsed)
       :ref (some-> ref str/trim not-empty)})))

(defn stage-repository!
  "Phase one of an import: fetch the commit and hold its tree for the picker.

   Pre : `form` is the import form, {:forge :host :repository :ref}.
   Post: {:ok true :stage-id S :source {…} :file-count N}, or
         {:ok false :error S}, which also carries :topic-id when the repository
         is already imported. On success an upload session owns the extracted
         tree, nothing is charged to quota, and no topic exists yet. On failure
         nothing remains on disk or in upload_sessions.
   A repository that is already imported is refused before any download: its
   selection cannot change (frame), and Update on its node is the path."
  [user-id form]
  (let [req (read-import-form form)]
    (if-not (:ok req)
      req
      (let [{:keys [forge origin path ref]} req
            repo-url (str (forge/origin-url origin) "/" path)
            key (work-key origin path)]
        (if-let [existing (db/find-repo-node user-id repo-url)]
          {:ok false :topic-id existing
           :error (str path " is already imported. Open it and press Update to add a newer commit.")}
          (try
            (run-as-activity user-id key "resolving"
              (fn []
                (let [token (tokens/token user-id forge (:host origin))
                      {:keys [sha ref]} (forge/resolve-commit forge origin token path ref)
                      dir (fetch-tree! user-id key forge origin token path sha)]
                  (try
                    (let [source {:forge forge :origin-url (forge/origin-url origin)
                                  :repo-url repo-url :path path :ref ref :sha sha}]
                      {:ok true
                       :file-count (count (file-paths (archive-root dir)))
                       :source source
                       :stage-id (db/open-forge-stage! user-id path dir source)})
                    (catch Throwable t
                      (archive/delete-dir! dir)
                      (throw t))))))
            (catch Throwable t
              {:ok false :error (failure-message t)})))))))

(defn staged-tree
  "Post: {:source {…} :tree tree} for a stage `user-id` owns, or nil when the
   stage is gone."
  [user-id stage-id]
  (when-let [{:keys [dir forge-source]} (db/get-forge-stage user-id stage-id)]
    {:source forge-source
     :tree (tree/tree (file-paths (archive-root dir)))}))

(defonce ^:private !importing-stages
  ;; Stage ids `import-staged!` holds. `cancel-stage!` leaves these alone, so a
  ;; picker frame that unmounts while its import runs cannot delete the tree the
  ;; import is reading.
  (atom #{}))

(defn import-staged!
  "Phase two: create the rows `selection` names, then release the stage.

   Pre : `stage-id` came from `stage-repository!` for `user-id`; `selection` is
         {:dirs :extensions} from the picker.
   Post: {:ok true :topic-id commit-id :report {…}} or {:ok false :error S}.
         The stage is released on EVERY path. A failed import downloads again,
         which costs less than a session that holds a repository on disk for six
         hours."
  [user-id stage-id selection]
  (swap! !importing-stages conj stage-id)
  (try
    (if-let [{:keys [dir forge-source]} (db/get-forge-stage user-id stage-id)]
      (let [{:keys [origin-url repo-url path]} forge-source]
        (try
          (run-as-activity user-id (work-key (forge/parse-origin origin-url) path) "creating topics"
            (fn []
              (if (db/find-repo-node user-id repo-url)
                {:ok false :error (str path " was imported while this picker was open.")}
                (let [r (create-commit-subtree! user-id (archive-root dir) forge-source
                          (normalize-selection selection) nil)]
                  (when (:ok r)
                    (commands/bump! user-id :import-document))
                  r))))
          (catch Throwable t
            {:ok false :error (failure-message t)})))
      {:ok false :error "This download expired. Start the import again."})
    (finally
      (db/release-upload-session! user-id stage-id)
      (swap! !importing-stages disj stage-id))))

(defn cancel-stage!
  "Post: the stage and its directory are gone, unless `import-staged!` holds the
   stage — that call releases it when it ends. Returns nil."
  [user-id stage-id]
  (when (and stage-id (not (contains? @!importing-stages stage-id)))
    (db/release-upload-session! user-id stage-id))
  nil)

(defn update-repository!
  "Add the commit the repository's ref names now, with the stored selection
   (§4.6 6.2).

   Pre : `repo-id` is a `repo` topic `user-id` owns.
   Post: {:ok true :topic-id commit-id :sha S :report {…}} for a new commit;
         {:ok true :unchanged? true :topic-id commit-id :sha S} when that sha is
         already a child (§4.6 6.3); or {:ok false :error S}. Nothing is written
         on failure, and the extracted tree is gone on every path."
  [user-id repo-id]
  (if-let [{:keys [repo-url forge ref selection]} (db/get-repo-import user-id repo-id)]
    (let [{:keys [origin path]} (forge/parse-repository-input repo-url)
          key (work-key origin path)]
      (try
        (run-as-activity user-id key "resolving"
          (fn []
            (let [token (tokens/token user-id forge (:host origin))
                  {:keys [sha]} (forge/resolve-commit forge origin token path ref)]
              (if-let [existing (db/find-commit-node repo-id sha)]
                {:ok true :unchanged? true :topic-id existing :sha sha}
                (let [dir (fetch-tree! user-id key forge origin token path sha)]
                  (try
                    (set-phase! user-id key "creating topics")
                    (let [r (create-commit-subtree! user-id (archive-root dir)
                              {:forge forge :repo-url repo-url :path path :ref ref :sha sha}
                              selection repo-id)]
                      (when (:ok r)
                        (commands/bump! user-id :import-document))
                      (assoc r :sha sha))
                    (finally
                      (archive/delete-dir! dir))))))))
        (catch Throwable t
          {:ok false :error (failure-message t)})))
    {:ok false :error "This topic is not an imported repository."}))
