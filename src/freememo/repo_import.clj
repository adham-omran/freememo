(ns freememo.repo-import
  "Import one commit of a git forge repository as a topic tree, and update a
   repository to a newer commit (plans/git-forge-integration.md §4.3, §4.6).

   Bytes arrive through a blob-less bare clone (`freememo.git-clone`,
   plans/commit-range-walkthrough.md §6.2). The forge's REST API serves the
   picker's repository list only.

   Two phases, because the picker needs the whole tree before any row exists:

     stage-repository!  clone the history, resolve the ref, and record the
                        staging clone as an upload session
     import-staged!     apply the user's selection, fetch those files, create
                        the rows, keep the clone as the repository's, and
                        release the session

   `update-repository!` runs both at once with the selection stored on the
   repository node, so an update shows no picker (§4.6 6.2).

   The rows: one `repo` node per repository URL, one `commit` node per sha under
   it, one `dir` node per selected directory, one `code` node per file inside a
   selected directory. Only `code` nodes are review subjects; the other three
   are structure (`db/structural-kinds`). A file's source goes to `topic_code`,
   never to `topics.content`, which holds the user's notes and starts empty.

   Activity (§4.3 3.8): `:importing-repos` maps a work key to its phase while
   work runs, and `:repo-import-errors` keeps the failure the reporter reads
   when it closes the row.

   Cancel (plans/repo-import-progress.md): `:repo-import-cancelled` marks a
   work key. A git process reads it and stops; the insert transaction checks it
   per batch."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [next.jdbc :as jdbc]
            [freememo.activity :as activity]
            [freememo.commands :as commands]
            [freememo.db :as db]
            [freememo.forge :as forge]
            [freememo.forge-kinds :as kinds]
            [freememo.forge-tokens :as tokens]
            [freememo.git-clone :as git]
            [freememo.input-check :as input]
            [freememo.priority :as priority]
            [freememo.repo-tree :as tree]
            [freememo.toasts :as toasts]
            [freememo.user-state :as us]
            [taoensso.telemere :as tel])
  (:import [java.io File]
           [java.nio ByteBuffer]
           [java.nio.charset CharacterCodingException CodingErrorAction StandardCharsets]
           [java.nio.file Files StandardCopyOption]))

;; ── Limits ─────────────────────────────────────────────────────────

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

(defn- cancelled?
  "Post: true when the user abandoned the run of `key`."
  [user-id key]
  (contains? @(us/get-atom user-id :repo-import-cancelled) key))

(defn- throw-if-cancelled!
  "Post: nil, or a throw when the run of `key` is abandoned. `run-as-activity`
   turns the throw into the cancelled result."
  [user-id key]
  (when (cancelled? user-id key)
    (throw (ex-info "Repository import cancelled" {:type ::cancelled}))))

(defn- run-as-activity
  "Run `f` while `key` is published in `:importing-repos`.

   Post: `f`'s value. A throw after the run was marked cancelled returns
         {:ok false :cancelled? true} and records no error: the user chose it.
         Otherwise a result carrying `:ok false`, or a throw, records its
         message in `:repo-import-errors` BEFORE membership drops, so the
         reporter closes the row as failed; any other result clears the entry.
   Invariant: membership, the canceller and the mark are removed on every path,
         in that order, so a stale mark cannot cancel the next run of `key`."
  [user-id key phase f]
  (let [errors (us/get-atom user-id :repo-import-errors)
        cancellers (us/get-atom user-id :repo-import-cancellers)
        marks (us/get-atom user-id :repo-import-cancelled)]
    (swap! marks disj key)
    (set-phase! user-id key phase)
    (swap! cancellers assoc key (fn [] (swap! marks conj key)))
    ;; After the canceller: discovery reads cancellability when it opens the row.
    (us/work-started! user-id)
    (try
      (let [r (f)]
        (if (false? (:ok r))
          (swap! errors assoc key (:error r))
          (swap! errors dissoc key))
        r)
      (catch Throwable t
        (if (cancelled? user-id key)
          (do (swap! errors dissoc key)
              {:ok false :cancelled? true})
          (do (swap! errors assoc key (or (.getMessage t) "Import failed"))
              (throw t))))
      (finally
        (swap! (us/get-atom user-id :importing-repos) dissoc key)
        (swap! cancellers dissoc key)
        (swap! marks disj key)))))

(defn- failure-message
  "Post: a message for the user. An unexpected throw is logged here, once."
  [^Throwable t]
  (do
    (cond
      (forge/failure? t) (.getMessage t)
      :else
      (do (tel/error! {:id ::repo-import-failed} t)
          "The import failed. The server log has the details."))))

;; ── The clone ────────────────────────────────────────────────────

(defn- git-context
  "What a git call on `forge`'s repository `path` needs: the clone URL, the
   auth header of the user's token, and the cancel mark of `key`.
   Post: {:url :auth :cancelled? :what}. Throws ::forge-failure for an origin
   the transport refuses (HTTPS only, the address gate)."
  [user-id key forge origin path]
  {:url (git/remote-url origin path)
   :auth (git/auth-header forge (tokens/token user-id forge (:host origin)))
   :cancelled? #(cancelled? user-id key)
   :what path})

(defn- stage-dir
  "A fresh staging clone path beside the clones, so the import can rename it
   into place on the same filesystem."
  ^File []
  (io/file (git/clone-root) (str ".stage-" (random-uuid) ".git")))

(defn- adopt-stage!
  "Make staging clone `dir` repository node `repo-id`'s clone. A clone already
   there wins and `dir` is left for the session release to delete. A failed
   rename costs nothing: a clone is a cache (§6.1.4)."
  [^File dir repo-id]
  (let [target (git/clone-dir repo-id)]
    (git/with-lock target
      (when-not (git/clone? target)
        (try
          (Files/move (.toPath dir) (.toPath target)
            (into-array java.nio.file.CopyOption [StandardCopyOption/ATOMIC_MOVE]))
          (catch Exception e
            (tel/log! {:level :warn :id ::adopt-stage-failed
                       :data {:repo-id repo-id :error (ex-message e)}})))))))

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

   Pre : `git-ctx` is `git-context`'s value plus :dir, a clone holding commit
         `sha`. `source` is {:forge :repo-url :path :ref :sha}. `selection` is
         normalized. `repo-id` is the existing repository node, or nil for a
         first import.
   Post: {:ok true :topic-id commit-id :repo-id N :report {…}}, or
         {:ok false :error S} with nothing written — when a node cap refuses
         (§4.3 3.9, 3.9.1) or the sha is already a child of the repository.
   Invariant: every row commits in one transaction.
   Invariant: `check-cancelled!` runs before the transaction and before each
         source batch; when it throws, the transaction writes nothing.
   Invariant: only the selected files' blobs are fetched, in one request, and
         one blob at a time is in heap."
  [user-id git-ctx source selection repo-id check-cancelled!]
  (let [{:keys [forge repo-url path ref sha]} source
        dir (:dir git-ctx)
        entries (git/ls-tree dir sha)
        oid-of (into {} (map (juxt :path :oid)) entries)
        commit-tree (tree/tree (map :path entries))
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
      (let [extensions (:extensions selection)
            wanted (filterv #(contains? extensions (tree/extension %)) (:files plan))
            _ (set-phase! user-id (:key git-ctx) "downloading")
            _ (git/prefetch-blobs! dir sha (map oid-of wanted) git-ctx)
            size-of (git/blob-sizes dir (map oid-of wanted))
            ;; The repository id first, so ids ascend from root to leaf.
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
            !report (atom {:directories (count (:dirs plan)) :files (count (:files plan))
                           :sources-stored 0 :binary-files 0 :oversize-files 0
                           :over-budget-files 0})
            !budget (atom 0)
            ;; Lazy, and realized once inside the transaction, so at most one
            ;; statement's worth of source is in heap at a time.
            code-rows (fn [read-blob]
                        (keep (fn [p]
                              (let [len (long (get size-of (oid-of p) 0))]
                                (cond
                                  (> len max-source-bytes)
                                  (do (swap! !report update :oversize-files inc) nil)

                                  (> (+ @!budget len) max-import-source-bytes)
                                  (do (swap! !report update :over-budget-files inc) nil)

                                  :else
                                  (if-let [text (some-> (read-blob (oid-of p)) utf8-text)]
                                    (do (swap! !budget + len)
                                        (swap! !report update :sources-stored inc)
                                        {:topic_id (id-by-path p) :source text
                                         :byte_size len :lang (tree/language-for p)})
                                    (do (swap! !report update :binary-files inc) nil)))))
                          wanted))]
        (check-cancelled!)
        (set-phase! user-id (:key git-ctx) "creating topics")
        (git/with-blob-reader* dir
          (fn [read-blob]
            (jdbc/with-transaction [tx db/ds]
              (if (and repo-id (db/find-commit-node tx repo-id sha))
                {:ok false :error (str "Commit " (subs sha 0 7) " is already imported.")}
                (do
                  (db/insert-topic-rows! tx topic-rows)
                  (doseq [batch (partition-by-bytes statement-source-bytes (code-rows read-blob))]
                    (check-cancelled!)
                    (db/insert-topic-code-rows! tx batch))
                  (when (nil? repo-id)
                    (db/insert-repo-selection! tx repo-id* forge ref selection))
                  {:ok true :topic-id commit-id :repo-id repo-id*
                   :report (assoc @!report :topics-created new-count)})))))))))

(defn clone-context
  "What a reader of repository node `repo-id`'s clone needs: its git dir,
   remote URL and auth header (plans/commit-range-walkthrough.md §6.1.4).
   A missing clone is made again here; an existing one is NOT fetched, so a
   read costs no network unless `fresh?`.
   Pre : `repo-id` is a `repo` topic `user-id` owns.
   Post: {:dir :url :auth :what}, or nil when the node is not an imported
         repository. Throws ::forge-failure when a clone or fetch fails."
  ([user-id repo-id] (clone-context user-id repo-id false))
  ([user-id repo-id fresh?]
   (when-let [{:keys [repo-url forge]} (db/get-repo-import user-id repo-id)]
     (let [{:keys [origin path]} (forge/parse-repository-input repo-url)
           dir (git/clone-dir repo-id)
           ctx {:dir dir :what path
                :url (git/remote-url origin path)
                :auth (git/auth-header forge (tokens/token user-id forge (:host origin)))}]
       (git/with-lock dir
         (cond
           fresh? (git/ensure-fresh! dir (:url ctx) ctx)
           (not (git/clone? dir)) (do (git/delete-dir! dir) (git/clone! dir (:url ctx) ctx))))
       ctx))))

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

(defn form-work-key
  "Post: the work key a download of `form` publishes under, or nil when the form
   does not name a repository. The key encoding stays in this namespace."
  [form]
  (let [{:keys [ok origin path]} (read-import-form form)]
    (when ok (work-key origin path))))

(defn source-work-key
  "Post: the work key of the import of a staged `source` ({:origin-url :path})."
  [{:keys [origin-url path]}]
  (work-key (forge/parse-origin origin-url) path))

(defn stage-repository!
  "Phase one of an import: fetch the commit and hold its tree for the picker.

   Pre : `form` is the import form, {:forge :host :repository :ref}.
   Post: {:ok true :stage-id S :source {…} :file-count N}, or
         {:ok false :error S}, which also carries :topic-id when the repository
         is already imported. On success an upload session owns the extracted
         tree, nothing is charged to quota, and no topic exists yet. On failure
         nothing remains on disk or in upload_sessions. A cancelled download
         returns {:ok false :cancelled? true} and leaves nothing either.
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
            (run-as-activity user-id key "cloning"
              (fn []
                (let [ctx (git-context user-id key forge origin path)
                      dir (stage-dir)]
                  (try
                    (git/with-lock dir (git/clone! dir (:url ctx) ctx))
                    (set-phase! user-id key "resolving")
                    (let [{:keys [sha ref]} (git/resolve-ref dir ref path)
                          _ (throw-if-cancelled! user-id key)
                          source {:forge forge :origin-url (forge/origin-url origin)
                                  :repo-url repo-url :path path :ref ref :sha sha}
                          file-count (count (git/ls-tree dir sha))
                          stage-id (db/open-forge-stage! user-id path dir source)]
                      ;; A cancel that lands after the stage opened: no picker
                      ;; will mount to release it.
                      (when (cancelled? user-id key)
                        (db/release-upload-session! user-id stage-id)
                        (throw-if-cancelled! user-id key))
                      {:ok true :file-count file-count :source source :stage-id stage-id})
                    (catch Throwable t
                      (git/delete-dir! dir)
                      (throw t))))))
            (catch Throwable t
              {:ok false :error (failure-message t)})))))))

(defn staged-tree
  "Post: {:source {…} :tree tree} for a stage `user-id` owns, or nil when the
   stage is gone."
  [user-id stage-id]
  (when-let [{:keys [dir forge-source]} (db/get-forge-stage user-id stage-id)]
    {:source forge-source
     :tree (tree/tree (map :path (git/ls-tree dir (:sha forge-source))))}))

(defonce ^:private !importing-stages
  ;; Stage ids `import-staged!` holds. `cancel-stage!` leaves these alone, so a
  ;; picker frame that unmounts while its import runs cannot delete the tree the
  ;; import is reading.
  (atom #{}))

(defn import-staged!
  "Phase two: create the rows `selection` names, then release the stage.

   Pre : `stage-id` came from `stage-repository!` for `user-id`; `selection` is
         {:dirs :extensions} from the picker.
   Post: {:ok true :topic-id commit-id :path S :report {…}},
         {:ok false :cancelled? true} with nothing written, or
         {:ok false :error S}. The stage is released on EVERY path. A failed import downloads again,
         which costs less than a session that holds a repository on disk for six
         hours."
  [user-id stage-id selection]
  (swap! !importing-stages conj stage-id)
  (try
    (if-let [{:keys [dir forge-source]} (db/get-forge-stage user-id stage-id)]
      (let [{:keys [repo-url path]} forge-source
            key (source-work-key forge-source)]
        (try
          (run-as-activity user-id key "creating topics"
            (fn []
              (if (db/find-repo-node user-id repo-url)
                {:ok false :error (str path " was imported while this picker was open.")}
                (let [origin (forge/parse-origin (:origin-url forge-source))
                      ctx (assoc (git-context user-id key (:forge forge-source) origin path)
                            :dir dir :key key)
                      r (git/with-lock dir
                          (create-commit-subtree! user-id ctx forge-source
                            (normalize-selection selection) nil
                            #(throw-if-cancelled! user-id key)))]
                  (when (:ok r)
                    (adopt-stage! dir (:repo-id r))
                    (commands/bump! user-id :import-document))
                  (assoc r :path path)))))
          (catch Throwable t
            {:ok false :error (failure-message t)})))
      {:ok false :error "This download expired. Start the import again."})
    (finally
      (db/release-upload-session! user-id stage-id)
      (swap! !importing-stages disj stage-id))))

;; ── Background import ──────────────────────────────────────────────
;; The picker waits on a run it does not own, so Close can unmount it while the
;; import continues (plans/repo-import-progress.md §3).

(def ^:private settled-run-ttl-ms
  "How long a settled run stays readable for a late Close."
  (* 60 60 1000))

(defonce ^:private !import-runs
  ;; stage id -> {:user-id :delivery promise :backgrounded? :result :settled-at}.
  ;; The result and the Close mark are each written with `swap-vals!`, so
  ;; whichever lands second sees the other and pushes the one toast.
  (atom {}))

(defn- push-result-toast!
  "Post: one toast for `r`, or none for a cancelled run — the user chose it."
  [user-id {:keys [ok cancelled? topic-id path report error]}]
  (cond
    cancelled? nil
    ok (toasts/push! user-id
         {:level :info
          :message (str "Imported " path ": " (:topics-created report) " topics.")
          :actions [{:label "Open" :nav [:viewer {:type :topic :topic-id topic-id}]}]})
    :else (toasts/push! user-id {:level :error :message error})))

(defn- settle-run!
  "Record `r` for `stage-id`, hand it to the waiting picker, and toast it when
   the picker already closed."
  [stage-id r]
  (let [[old _] (swap-vals! !import-runs update stage-id assoc
                  :result r :settled-at (System/currentTimeMillis))
        run (get old stage-id)]
    (deliver (:delivery run) r)
    (when (:backgrounded? run)
      (push-result-toast! (:user-id run) r))))

(defn- without-expired-runs [runs now]
  (into {} (remove (fn [[_ {:keys [settled-at]}]]
                     (and settled-at (> (- now settled-at) settled-run-ttl-ms))))
    runs))

(defn start-import!
  "Run `import-staged!` for `stage-id` on a server thread, once.

   Pre : as `import-staged!`.
   Post: a promise of `import-staged!`'s result. A second call for the same
         stage — an e/Offload restart — returns the same promise and starts
         nothing. A stage another user's run holds delivers a refusal.
   Invariant: the stage is held before this returns, so a picker that unmounts
         at once cannot release the tree the run reads."
  [user-id stage-id selection]
  (let [fresh {:user-id user-id :delivery (promise)}
        run (get (swap! !import-runs
                   (fn [runs]
                     (let [runs (without-expired-runs runs (System/currentTimeMillis))]
                       (if (contains? runs stage-id) runs (assoc runs stage-id fresh)))))
              stage-id)]
    (cond
      (not= user-id (:user-id run))
      (doto (promise) (deliver {:ok false :error "This download expired. Start the import again."}))

      (identical? run fresh)
      (do (swap! !importing-stages conj stage-id)
          (future
            (settle-run! stage-id
              (try (import-staged! user-id stage-id selection)
                (catch Throwable t {:ok false :error (failure-message t)}))))
          (:delivery run))

      :else (:delivery run))))

(defn background-import!
  "The picker waiting on `stage-id`'s run closed.
   Post: the run's result reaches the user as a toast exactly once — now if it
         already settled, else when it settles. Nothing for a run `user-id`
         does not own. Returns nil."
  [user-id stage-id]
  (let [[old _] (swap-vals! !import-runs
                  (fn [runs]
                    (if (= user-id (get-in runs [stage-id :user-id]))
                      (assoc-in runs [stage-id :backgrounded?] true)
                      runs)))
        run (get old stage-id)]
    (when (and (= user-id (:user-id run)) (not (:backgrounded? run)) (contains? run :result))
      (push-result-toast! user-id (:result run)))
    nil))

(defn cancel-import!
  "Abandon the running download or import published under `key`.
   Post: `activity/cancel-work!`'s result. The run stops at its next check and
         its Activity row reads cancelled."
  [user-id key]
  (when key
    (activity/cancel-work! user-id "repo-import" key)))

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
         already a child (§4.6 6.3); {:ok false :cancelled? true}; or
         {:ok false :error S}. Nothing is written on failure or cancel. The
         clone is fetched, or made again when it is missing (§6.1.4), and an
         unchanged ref transfers no objects (§6.2.2)."
  [user-id repo-id]
  (if-let [{:keys [repo-url forge ref selection]} (db/get-repo-import user-id repo-id)]
    (let [{:keys [origin path]} (forge/parse-repository-input repo-url)
          key (work-key origin path)]
      (try
        (run-as-activity user-id key "fetching"
          (fn []
            (let [dir (git/clone-dir repo-id)
                  ctx (assoc (git-context user-id key forge origin path) :dir dir :key key)]
              (git/with-lock dir
                (git/ensure-fresh! dir (:url ctx) ctx)
                (set-phase! user-id key "resolving")
                (let [{:keys [sha]} (git/resolve-ref dir ref path)]
                  (if-let [existing (db/find-commit-node repo-id sha)]
                    {:ok true :unchanged? true :topic-id existing :sha sha}
                    (let [r (create-commit-subtree! user-id ctx
                              {:forge forge :repo-url repo-url :path path :ref ref :sha sha}
                              selection repo-id #(throw-if-cancelled! user-id key))]
                      (when (:ok r)
                        (commands/bump! user-id :import-document))
                      (assoc r :sha sha))))))))
        (catch Throwable t
          {:ok false :error (failure-message t)})))
    {:ok false :error "This topic is not an imported repository."}))
