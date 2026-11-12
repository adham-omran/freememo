(ns freememo.sync-client
  "The desktop half of row sync (plans/electron-local-db-sync.md §2).

   One sync runs these steps (2.3):
   0. top up id blocks below 20 % (1.2.5),
   1. `meta`: protocol, epoch and license (2.3.2),
   2. graves both ways,
   3. pending rows up, then the server's corrections back,
   4. rows down since the checkpoint, skipping this sync's own uploads,
   5. apply, in one transaction per download,
   6. sanity check of row counts, once retried (2.6),
   7. store the new checkpoint.

   A crash at any step loses nothing: an upload is marked synced only after
   the server committed it, and the checkpoint moves only after the download
   committed (6.3). Rerunning a step applies the same rows again, and the
   apply is idempotent.

   Syncs are single-flight. They start at launch, every five minutes, when the
   connection returns, before quit, and from Sync now (2.11)."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [cheshire.core :as json]
            [clj-http.client :as http]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]
            [taoensso.telemere :as tel]
            [freememo.account-transfer-format :as fmt]
            [freememo.commands :as commands]
            [freememo.db :as db]
            [freememo.desktop-link :as link]
            [freememo.desktop-version :as desktop-version]
            [freememo.largeobj :as lo]
            [freememo.sync-apply :as apply]
            [freememo.sync-schema :as schema]
            [freememo.sync-server :as scope])
  (:import [java.io File]
           [java.security DigestInputStream MessageDigest]
           [java.util.concurrent Executors ScheduledExecutorService ThreadFactory TimeUnit]))

(def ^:private opts {:builder-fn rs/as-unqualified-lower-maps})

(def interval-minutes "2.11" 5)
(def batch-bytes "Upload batch size, well under the server's small-request cap." (* 4 1024 1024))
(def batch-rows 500)
(def pending-key-cap "The sanity check is skipped above this many pending rows." 1000)

;; ── Status ─────────────────────────────────────────────────────────

(defonce ^:private !status
  (atom {:state :idle :last-sync-at nil :error nil :license nil}))

(defn status
  "What Settings and Electron show: the sync state, the last error, the
   cached license (13.6), and download progress.
   `:library-ready` is true once a download has committed, the first one
   included. Electron keeps the app closed until then, so nobody works in an
   empty library that is about to be replaced.
   `:progress` holds `:downloaded` rows and `:downloaded-bytes`, `:uploaded`
   rows, and during a full download the totals `:total-rows` and
   `:total-bytes`.
   The license cache is what an offline launch reads; the server stays the
   authority for every operation."
  []
  (let [row (jdbc/execute-one! db/ds ["SELECT checkpoint, last_sync_at, last_error, license,
                                              full_sync_required FROM sync_state"] opts)]
    (merge {:library-ready (pos? (or (:checkpoint row) 0))
            :last-sync-at (some-> (:last_sync_at row) str)
            :error (:last_error row)
            :full-sync-required (:full_sync_required row)
            :license (some-> (:license row) str json/parse-string)
            :online (link/online?)
            :pending-conflicts (:n (jdbc/execute-one! db/ds ["SELECT count(*) AS n FROM sync_conflicts
                                                              WHERE resolved_at IS NULL"] opts))}
      (select-keys @!status [:state :progress]))))

(declare local-user-id)

(defn sync-state
  "Post: the sync state keyword (`:idle`, `:update-required`, ...), or nil
         before the first sync."
  []
  (:state @!status))

(defn- set-state!
  "Move the sync state machine and refresh Settings (:desktop-mutations)."
  [state & {:as more}]
  (swap! !status merge {:state state} more)
  (when-let [user-id (local-user-id)]
    (commands/bump-channels! user-id #{:desktop-mutations})))

(def ^:private jsonb-columns #{:license :full_download})

(defn- record!
  "Write columns of the one sync_state row."
  [cols]
  (let [sets (str/join ", " (map #(str (name %) (if (jsonb-columns %) " = ?::jsonb" " = ?")) (keys cols)))]
    (jdbc/execute! db/ds (into [(str "UPDATE sync_state SET " sets)] (vals cols)))))

;; ── HTTP ───────────────────────────────────────────────────────────

(defn protocol-refusal?
  "Pre:  `body` is a parsed JSON map, a JSON string, or anything else.
   Post: true when it is the body of `desktop-auth/protocol-refusal`:
         an `error` of `update-required` or `server-outdated` and a numeric
         `protocol`. Any other 409 is not a protocol refusal."
  [body]
  (let [m (cond
            (map? body) body
            (string? body) (try (json/parse-string body) (catch Exception _ nil)))]
    (boolean (and (map? m)
               (contains? #{"update-required" "server-outdated"} (get m "error"))
               (number? (get m "protocol"))))))

(defn note-update-required!
  "Show \"Update to keep syncing\": the server refused the Sync Protocol of
   this install. Every path that can see the refusal calls this."
  []
  (set-state! :update-required))

(defn- call!
  "One request to freememo.net. Answers {:status :body}.
   Post: a refused connection marks the link offline and throws ::offline.
         A 401 throws ::unauthorized: the token was revoked (3.2).
         A protocol refusal (409, `protocol-refusal?`) throws ::update-required."
  [method path & {:keys [json-body file query]}]
  (try
    (let [resp (http/request
                 (cond-> {:method method
                          :url (str (link/origin) path)
                          :headers (assoc (desktop-version/request-headers)
                                     "Authorization" (str "Bearer " (link/token)))
                          :throw-exceptions false
                          :connection-timeout 15000
                          :socket-timeout 120000}
                   json-body (assoc :body (json/generate-string json-body) :content-type :json)
                   file (assoc :body file :content-type "application/octet-stream"
                          :length (.length ^File file))
                   query (assoc :query-params query)))
          body (let [b (:body resp)]
                 (if (and (string? b) (str/starts-with? (str (get-in resp [:headers "content-type"])) "application/json"))
                   (json/parse-string b)
                   b))]
      (link/set-online! true)
      (when (= 401 (:status resp))
        (throw (ex-info "This device was signed out of FreeMemo" {:type ::unauthorized})))
      (when (and (= 409 (:status resp)) (protocol-refusal? body))
        (throw (ex-info (str (get body "error")) {:type ::update-required})))
      {:status (:status resp) :body body})
    (catch java.io.IOException e
      (link/set-online! false)
      (throw (ex-info "FreeMemo is offline" {:type ::offline} e)))))

(defn- ok-body! [{:keys [status body]} step]
  (when-not (= 200 status)
    (throw (ex-info (str "Sync step " step " failed: HTTP " status " " (pr-str body))
             {:type ::step-failed :step step :status status :body body})))
  body)

(defn- download-blob!
  "Fetch one blob of a server row into a temp file."
  ^File [{:strs [t k]} col]
  (let [file (File/createTempFile "fm-blob" ".bin")
        resp (http/get (str (link/origin) "/api/sync/blob")
               {:headers (assoc (desktop-version/request-headers)
                           "Authorization" (str "Bearer " (link/token)))
                :query-params {"t" t "k" (json/generate-string k) "c" col}
                :as :stream :throw-exceptions false :socket-timeout 600000})]
    (when (= 409 (:status resp))
      (let [b (with-open [in ^java.io.InputStream (:body resp)] (slurp in))]
        (when (protocol-refusal? b)
          (throw (ex-info "update-required" {:type ::update-required})))))
    (when-not (= 200 (:status resp))
      (throw (ex-info (str "Blob download failed: HTTP " (:status resp))
               {:type ::step-failed :step :blob :status (:status resp)})))
    (with-open [in ^java.io.InputStream (:body resp)] (io/copy in file))
    (swap! !status update-in [:progress :downloaded-bytes] (fnil + 0) (.length file))
    file))

(defn- fetch-blobs
  "Download every blob the rows name. Returns {[row-key col] file}."
  [rows]
  (into {} (for [row rows [col _] (get row "b")]
             [[(get row "t") (get row "k") col] (download-blob! row col)])))

(defn- blob-lookup [files]
  (fn [row col] (get files [(get row "t") (get row "k") col])))

(defn- delete-files! [files]
  (doseq [^File f (vals files)] (.delete f)))

;; ── 0. Id blocks (1.2.5) ───────────────────────────────────────────

(defn ensure-id-blocks!
  "Add a block while fewer than `needed` ids are left, and at least 20 % of a
   block. A signed-in desktop asks the server, and offline it keeps minting
   until the block runs out (1.2.4).
   Post: at least `needed` unspent ids, or a thrown ::offline."
  ([] (ensure-id-blocks! 0))
  ([needed]
   (loop []
     (let [left (:n (jdbc/execute-one! db/ds ["SELECT coalesce(sum(block_end - next_id), 0) AS n
                                               FROM local_id_blocks"] opts))]
       (when (< left (max needed (quot schema/block-size 5)))
         (let [{:strs [start end]} (ok-body! (call! :post "/api/desktop/id-block" :json-body {}) :id-block)]
           (jdbc/execute! db/ds ["INSERT INTO local_id_blocks (block_start, block_end, next_id)
                                  VALUES (?, ?, ?) ON CONFLICT (block_start) DO NOTHING"
                                 start end start])
           (recur)))))))

;; ── Local reads ────────────────────────────────────────────────────

(defn- local-user-id []
  (:user_id (jdbc/execute-one! db/ds ["SELECT user_id FROM sync_state"] opts)))

(defn- sync-row []
  (jdbc/execute-one! db/ds ["SELECT * FROM sync_state"] opts))

(defn- pending-rows
  "Every pending row of one table, as wire rows."
  [conn table]
  (apply/read-rows conn table "t.usn = -1" []))

(defn- local-counts
  "2.6 — synced rows per table in the account's scope, and the keys of the
   pending ones, which the server subtracts from its own counts."
  [conn user-id]
  (reduce
    (fn [acc table]
      (let [{:keys [where]} (scope/sync-scope table)
            meta (apply/table-meta conn table)
            params (scope/scope-params table user-id)
            n (:n (jdbc/execute-one! conn (into [(format "SELECT count(*) AS n FROM %s t WHERE (%s) AND t.usn <> -1"
                                                   (name table) where)] params) opts))
            pending (mapv #(json/parse-string (:k %))
                      (jdbc/execute! conn (into [(format "SELECT jsonb_build_object(%s)::text AS k
                                                          FROM %s t WHERE (%s) AND t.usn = -1"
                                                   (str/join ", " (mapcat (fn [c] [(str "'" c "'") (str "t." c)]) (:key meta)))
                                                   (name table) where)] params) opts))]
        (-> acc (assoc-in [:counts (name table)] n)
          (cond-> (seq pending) (assoc-in [:pending (name table)] pending)))))
    {:counts {} :pending {}}
    fmt/synced-tables))

(defn pending-edit-count
  "How many edits are not yet synced: the rows with `usn = -1` in the account's
   scope (the same rows `local-counts` lists as pending). The installer gate
   (`freememo.installer-gate`) reads it."
  []
  (if-let [user-id (local-user-id)]
    (transduce (map count) + 0 (vals (:pending (local-counts db/ds user-id))))
    0))

;; ── 2. Graves ──────────────────────────────────────────────────────

(defn- graves-step!
  "Send local deletions, apply the server's. Returns the server's `to`."
  [user-id checkpoint]
  (let [local (mapv (fn [g] {"t" (:table_name g) "k" (json/parse-string (:row_key g))})
                (jdbc/execute! db/ds ["SELECT table_name, row_key::text AS row_key FROM sync_graves
                                       WHERE usn = -1"] opts))
        {:strs [graves to]} (ok-body! (call! :post "/api/sync/graves"
                                        :json-body {:checkpoint checkpoint :graves local
                                                   })
                              :graves)
        result (apply/with-apply-tx db/ds
                 (fn [tx]
                   (doseq [g local]
                     (jdbc/execute! tx ["DELETE FROM sync_graves WHERE table_name = ? AND row_key = ?::jsonb
                                         AND usn = -1" (get g "t") (json/generate-string (get g "k"))]))
                   (apply/apply-graves! tx {:peer :desktop :user-id user-id} graves)))]
    {:to to :touched (:touched result)}))

;; ── 3. Upload ──────────────────────────────────────────────────────

(defn- sha256-file [^File f]
  (let [md (MessageDigest/getInstance "SHA-256")]
    (with-open [in (DigestInputStream. (io/input-stream f) md)]
      (io/copy in (java.io.OutputStream/nullOutputStream)))
    (apply str (map #(format "%02x" %) (.digest md)))))

(defn- export-blob!
  "Write one local blob to a temp file."
  ^File [conn {:strs [t k]} col]
  (let [table (apply/synced-table t)
        meta (apply/table-meta conn table)
        kind (get-in schema/blob-columns [table col])
        file (File/createTempFile "fm-up" ".bin")
        v (:v (jdbc/execute-one! conn [(format "SELECT t.%s AS v FROM %s t WHERE (%s) =
                                                 (SELECT %s FROM jsonb_populate_record(null::%s, ?::jsonb) k)"
                                         col t (str/join ", " (map #(str "t." %) (:key meta)))
                                         (str/join ", " (map #(str "k." %) (:key meta))) t)
                                        (json/generate-string k)] opts))]
    (if (= kind :bytea)
      (io/copy ^bytes v file)
      (jdbc/with-transaction [tx db/ds] (lo/copy-to-file! tx v file)))
    file))

(defn- upload-blobs!
  "PUT each blob a batch names, and name it by its SHA-256 in the row."
  [rows]
  (mapv (fn [row]
          (reduce (fn [row [col _]]
                    (let [f (export-blob! db/ds row col)
                          sha (sha256-file f)]
                      (try
                        (ok-body! (call! :put (str "/api/sync/blob/" sha) :file f) :blob-put)
                        (finally (.delete f)))
                      (assoc-in row ["b" col] {"sha256" sha})))
            row (get row "b")))
    rows))

(defn- batches
  "Rows grouped into requests under `batch-bytes` and `batch-rows`."
  [rows]
  (->> rows
    (reduce (fn [[cur size out] row]
              (let [n (count (json/generate-string (get row "r")))]
                (if (and (seq cur) (or (> (+ size n) batch-bytes) (>= (count cur) batch-rows)))
                  [[row] n (conj out cur)]
                  [(conj cur row) (+ size n) out])))
      [[] 0 []])
    ((fn [[cur _ out]] (cond-> out (seq cur) (conj cur))))))

(def ^:private kept-local
  "Rejections that leave the local row pending for the next sync: the row is
   sound, only the server could not take it now."
  #(or (= "storage-quota" %) (str/starts-with? % "invalid:")))

(defn- parent-missing-keys
  "The rows an apply refused only because their parent had not arrived."
  [result]
  (into #{} (keep #(when (= "parent-deleted" (get % "reason")) [(get % "t") (get % "k")]))
    (:rejected result)))

(defn- settle-upload!
  "Take back what the server decided about one uploaded batch.
   Pre:  the server committed the batch as `xid`.
   Post: rows the server applied are no longer pending, unless they changed
         again meanwhile (their mtime moved). Rejected rows are in the conflict
         report, and gone locally unless `kept-local` (D8). Merged ids point at
         the server's row (2.9.1). Corrections replace the local copies.
         With `wait-for-parents?`, a row refused only for a missing parent
         stays pending instead, because its parent may be in a later batch.
         Returns the keys of those waiting rows."
  [user-id sent {:strs [xid rejected remaps corrections]} wait-for-parents?]
  (let [waiting (if wait-for-parents? (parent-missing-keys {:rejected rejected}) #{})
        rejected-keys (set (map (juxt #(get % "t") #(get % "k")) rejected))
        rejected (remove #(waiting [(get % "t") (get % "k")]) rejected)
        files (fetch-blobs corrections)]
    (try
      (apply/with-apply-tx db/ds
        (fn [tx]
          (doseq [{:strs [t k r]} sent
                  :when (not (rejected-keys [t k]))
                  :let [meta (apply/table-meta tx (apply/synced-table t))]]
            (jdbc/execute! tx [(format "UPDATE %s t SET usn = ? WHERE (%s) =
                                          (SELECT %s FROM jsonb_populate_record(null::%s, ?::jsonb) k)
                                        AND t.usn = -1 AND t.mtime = ?::timestamptz"
                                 t (str/join ", " (map #(str "t." %) (:key meta)))
                                 (str/join ", " (map #(str "k." %) (:key meta))) t)
                               xid (json/generate-string k) (get r "mtime")]))
          (doseq [{:strs [t k r reason]} rejected]
            (jdbc/execute! tx ["INSERT INTO sync_conflicts (table_name, row_key, row_data, reason)
                                VALUES (?, ?::jsonb, ?::jsonb, ?)"
                               t (json/generate-string k) (json/generate-string r) reason])
            (when-not (kept-local reason)
              (let [meta (apply/table-meta tx (apply/synced-table t))
                    match (format "(%s) = (SELECT %s FROM jsonb_populate_record(null::%s, ?::jsonb) k)"
                            (str/join ", " (map #(str "t." %) (:key meta)))
                            (str/join ", " (map #(str "k." %) (:key meta))) t)]
                ;; usn 0 first, so the grave trigger does not report it twice.
                (jdbc/execute! tx [(format "UPDATE %s t SET usn = 0 WHERE %s" t match) (json/generate-string k)])
                (jdbc/execute! tx [(format "DELETE FROM %s t WHERE %s" t match) (json/generate-string k)]))))
          (doseq [[t m] remaps [from to] m]
            (apply/rewrite-local-references! tx (apply/synced-table t) (parse-long from) to))
          (apply/apply-rows! tx {:peer :desktop :user-id user-id :blob-file (blob-lookup files)}
            corrections)))
      waiting
      (finally (delete-files! files)))))

(defn- upload-round!
  "Upload `rows` in batches. Returns {:xids :waiting}, where `waiting` holds
   the keys of rows refused only for a missing parent, left pending when
   `wait-for-parents?`."
  [user-id base rows wait-for-parents?]
  (reduce (fn [acc batch]
            (let [batch (upload-blobs! batch)
                  answer (ok-body! (call! :post "/api/sync/upload"
                                     :json-body {:base base :rows batch})
                           :upload)
                  waiting (settle-upload! user-id batch answer wait-for-parents?)]
              (swap! !status update-in [:progress :uploaded] (fnil + 0) (count batch))
              (-> acc (update :xids conj (get answer "xid")) (update :waiting into waiting))))
    {:xids [] :waiting #{}}
    (batches rows)))

(defn- upload-step!
  "Upload every pending row in FK order. Returns this sync's upload xids.
   A child whose parent is in a later batch is refused the first time, since
   the server applies one batch per transaction. It waits, pending, and goes
   again in the next round, when its parent is there. A round that applies
   none of the waiting rows is the last: what it still refuses goes to the
   conflict report."
  [user-id base]
  (loop [rows (into [] (mapcat #(pending-rows db/ds %)) fmt/synced-tables)
         xids []]
    (let [{round-xids :xids waiting :waiting} (upload-round! user-id base rows true)
          xids (into xids round-xids)
          again (into [] (keep (fn [[t k]] (apply/read-row db/ds (apply/synced-table t) k))) waiting)]
      (cond
        (empty? again) xids
        (< (count again) (count rows)) (recur again xids)
        :else (into xids (:xids (upload-round! user-id base again false)))))))

;; ── 4, 5. Download ─────────────────────────────────────────────────

(defn- blob-keys [rows]
  (for [row rows [col _] (get row "b")] [(get row "t") (get row "k") col]))

(defn- download-step!
  "Download and apply every row in [from, to) except this sync's uploads.
   A row whose parent sits in a later page, such as a forked chat whose fork
   message downloads after every chat, waits with its files and applies with
   that later page. Only a parent that never arrives makes it a refusal.
   Post: one transaction holds the whole download, so an interrupted one
         leaves nothing applied and the checkpoint unmoved. Returns {:to
         :touched}.
   With `on-commit`, a page that leaves no row waiting also ends its
   transaction. `(on-commit tx {:cursor c})` runs in that transaction first,
   so whatever it records commits with exactly the rows before `c`, and a
   download stopped later resumes from `c`. `cursor` starts the download
   there."
  [user-id from {:keys [to exclude graves-from cursor on-commit]}]
  (loop [cursor cursor to to touched #{}]
    (let [first? (when-not cursor true)
          segment
          (apply/with-apply-tx db/ds
            (fn [tx]
              (loop [cursor cursor to to touched touched first? first? waiting [] waiting-files {}]
                (let [page (ok-body! (call! :post "/api/sync/download"
                                       :json-body (cond-> {:from from :exclude (or exclude []) :cursor cursor
                                                          }
                                                    to (assoc :to to)
                                                    graves-from (assoc :graves_from graves-from)))
                             :download)
                      {:strs [rows graves]} page
                      next-cursor (get page "cursor")
                      to (get page "to")
                      files (merge waiting-files (fetch-blobs rows))
                      batch (into waiting rows)
                      g (when first? (apply/apply-graves! tx {:peer :desktop :user-id user-id} (or graves [])))
                      r (apply/apply-rows! tx {:peer :desktop :user-id user-id :blob-file (blob-lookup files)} batch)
                      missing (parent-missing-keys r)
                      still-waiting (if next-cursor (filterv #(missing [(get % "t") (get % "k")]) batch) [])
                      kept-files (select-keys files (blob-keys still-waiting))
                      refused (if next-cursor
                                (remove #(missing [(get % "t") (get % "k")]) (:rejected r))
                                (:rejected r))
                      touched (into touched (concat (:touched g) (:touched r)))]
                  (delete-files! (apply dissoc files (keys kept-files)))
                  (when (seq refused)
                    (tel/log! {:level :warn :id ::download-rejected :data {:rejected (map #(dissoc % "r") refused)}}
                      "Some downloaded rows did not apply"))
                  (swap! !status update-in [:progress :downloaded] (fnil + 0) (count rows))
                  (commands/bump-channels! user-id #{:desktop-mutations})
                  (cond
                    (nil? next-cursor) {:to to :touched touched}

                    (and on-commit (empty? still-waiting))
                    (do (on-commit tx {:cursor next-cursor})
                        {:to to :touched touched :cursor next-cursor})

                    :else (recur next-cursor to touched false still-waiting kept-files))))))]
      (if-let [next-cursor (:cursor segment)]
        (recur next-cursor (:to segment) (:touched segment))
        segment))))

;; ── 6. Sanity (2.6) ────────────────────────────────────────────────

(defn- sane? [user-id]
  (let [{:keys [counts pending]} (local-counts db/ds user-id)]
    (if (> (reduce + (map count (vals pending))) pending-key-cap)
      true
      (get (ok-body! (call! :post "/api/sync/finish"
                       :json-body {:counts counts :pending pending})
             :finish)
        "ok"))))

;; ── Full sync (2.8) ────────────────────────────────────────────────

(defn- local-empty? []
  (every? (fn [table]
            (nil? (jdbc/execute-one! db/ds [(str "SELECT 1 FROM " (name table) " LIMIT 1")])))
    fmt/synced-tables))

(defn- stored-full-download
  "The full download a stopped sync left, or nil (2.8.1)."
  [state]
  (some-> (:full_download state) str json/parse-string))

(defn full-download!
  "Replace this desktop's data with the account's (Anki's download). Pending
   local rows go to the conflict report on the way out, so nothing is lost
   without a trace.
   The download commits page by page. Each commit stores its cursor in
   `sync_state.full_download`, so a download stopped by a crash, a quit or a
   lost connection resumes there (2.8.1). It does not restart from zero:
   an account of 200000 rows takes long enough that a laptop sleeps in it.
   `resume` is a stored download to continue, whose epoch the caller checked
   against the server's. nil starts afresh.
   Post: the checkpoint is the download's `to` and nothing is stored to
         resume. While the download runs, the checkpoint is 0, so the
         library is not ready (`status`)."
  ([] (full-download! nil))
  ([resume]
   (let [user-id (local-user-id)
         job (or resume
               (let [{:strs [to scm rows bytes]} (ok-body! (call! :post "/api/sync/full/download"
                                                             :json-body {})
                                                   :full-download)
                     job {"to" to "scm" scm "rows" rows "bytes" bytes
                          "cursor" nil "downloaded" 0 "downloaded_bytes" 0}]
                 (apply/with-apply-tx db/ds
                   (fn [tx]
                     (doseq [table (reverse fmt/synced-tables)]
                       (jdbc/execute! tx [(str "DELETE FROM " (name table))]))
                     (jdbc/execute! tx ["DELETE FROM sync_graves"])
                     (jdbc/execute! tx ["UPDATE sync_state SET checkpoint = 0, full_download = ?::jsonb"
                                        (json/generate-string job)])))
                 job))
         {:strs [to scm rows bytes]} job]
     ;; An older server reports no totals. Leave them out rather than nil.
     (swap! !status assoc :progress (cond-> {:downloaded (get job "downloaded" 0)
                                             :downloaded-bytes (get job "downloaded_bytes" 0)}
                                      (number? rows) (assoc :total-rows rows)
                                      (number? bytes) (assoc :total-bytes bytes)))
     (when resume
       (tel/log! {:level :info :id ::full-download-resumed
                  :data {:downloaded (get job "downloaded") :cursor (get job "cursor")}}
         "Resuming a stopped full download"))
     (let [{:keys [to touched]}
           (download-step! user-id 0
             {:to to
              :cursor (get job "cursor")
              :on-commit (fn [tx {:keys [cursor]}]
                           (let [{:keys [downloaded downloaded-bytes]} (:progress @!status)]
                             (jdbc/execute! tx ["UPDATE sync_state SET full_download = ?::jsonb"
                                                (json/generate-string
                                                  (assoc job "cursor" cursor
                                                    "downloaded" (or downloaded 0)
                                                    "downloaded_bytes" (or downloaded-bytes 0)))])))})]
       (record! {:checkpoint to :scm scm :full_sync_required false :last_error nil :full_download nil
                 :last_sync_at (java.sql.Timestamp/from (java.time.Instant/now))})
       (commands/bump-channels! user-id (apply/channels-for (into touched fmt/synced-tables)))))))

(defn full-upload!
  "Replace the account's data on freememo.net with this desktop's (Anki's
   upload). Every other desktop of the account must full-sync next."
  []
  (let [user-id (local-user-id)
        {:strs [base scm]} (ok-body! (call! :post "/api/sync/full/upload"
                                       :json-body {})
                             :full-upload)]
    (apply/with-apply-tx db/ds
      (fn [tx]
        (doseq [table fmt/synced-tables]
          (jdbc/execute! tx [(str "UPDATE " (name table) " SET usn = -1")]))
        (jdbc/execute! tx ["DELETE FROM sync_graves"])))
    (let [xids (upload-step! user-id base)
          {:keys [to]} (download-step! user-id base {:exclude xids})]
      (record! {:checkpoint to :scm scm :full_sync_required false :last_error nil
                :last_sync_at (java.sql.Timestamp/from (java.time.Instant/now))}))))

;; ── One sync ───────────────────────────────────────────────────────

(defn- incremental-sync!
  [user-id {:strs [sync]}]
  (let [checkpoint (:checkpoint (sync-row))
        full? (= "full" sync)
        {graves-to :to graves-touched :touched} (if full? (graves-step! user-id checkpoint) {:to checkpoint})
        xids (if full? (upload-step! user-id graves-to) [])
        {:keys [to touched]} (download-step! user-id checkpoint {:exclude xids :graves-from graves-to})
        touched (into (set graves-touched) touched)]
    (record! {:checkpoint to})
    (when (seq touched)
      (commands/bump-channels! user-id (apply/channels-for touched)))
    ;; 2.6 — once more before calling it a mismatch, because a write on the
    ;; web between the download and the count looks the same as lost data.
    (when-not (sane? user-id)
      (let [{retry-to :to retry-touched :touched} (download-step! user-id to {})]
        (record! {:checkpoint retry-to})
        (when (seq retry-touched)
          (commands/bump-channels! user-id (apply/channels-for retry-touched)))
        (when-not (sane? user-id)
          (record! {:full_sync_required true})
          (throw (ex-info "The device and freememo.net disagree. A full sync is required."
                   {:type ::full-sync-required})))))))

(defn- run-sync! []
  (let [user-id (local-user-id)
        state (sync-row)]
    (ensure-id-blocks!)
    (let [{:keys [status body]} (call! :post "/api/sync/meta"
                                  :json-body {:protocol desktop-version/sync-protocol
                                             })]
      (case (long status)
        409 (do (set-state! :update-required) (record! {:last_error (get body "error")}))
        200 (let [license (get body "license")]
              (record! {:license (json/generate-string license)})
              (swap! !status assoc :license license)
              (cond
                (= "none" (get body "sync"))
                (set-state! :license-refused)

                ;; 2.8.1 — a stopped full download continues. Its rows are
                ;; only a prefix of the account, so when the epoch moved, it
                ;; starts again instead of asking which copy to keep.
                (stored-full-download state)
                (let [job (stored-full-download state)]
                  (full-download! (when (= (get job "scm") (get body "scm")) job))
                  (set-state! :idle))

                (or (:full_sync_required state) (not= (:scm state) (get body "scm")))
                (cond
                  (local-empty?) (do (full-download!) (set-state! :idle))
                  ;; An epoch change with local data: Anki asks, and so does this.
                  :else (set-state! :full-sync-choice))

                :else
                (do (incremental-sync! user-id body)
                    (record! {:last_sync_at (java.sql.Timestamp/from (java.time.Instant/now))
                              :last_error nil})
                    (set-state! :idle))))
        (throw (ex-info (str "Sync meta failed: HTTP " status) {:type ::step-failed :status status}))))))

(defn- fail!
  "Show the failed `state`, with `message` as the last error when it is
   non-nil. The error is written before the state moves: moving the state is
   what refreshes Settings, and it must find the error already there.
   Post: `status` answers `state`, even when the local database cannot
         record the error."
  [state message]
  (try (when message (record! {:last_error message}))
    (catch Throwable e (tel/error! {:id ::record-failed} e)))
  (try (set-state! state)
    (catch Throwable e
      (swap! !status assoc :state state)
      (tel/error! {:id ::state-failed} e)))
  (link/sync-failed!))

(defonce ^:private !lock (Object.))
(defonce ^:private !running (atom false))
;; True between `stop-scheduler!` and the next `start-scheduler!`: no sync starts.
(defonce ^:private !stopped (atom false))

(defn sync-now!
  "Run one sync unless one is already running.
   Post: returns the status after the sync. Errors are recorded and shown,
         never thrown, because the callers are a timer, Electron and a button."
  []
  (when (and (not @!stopped) (compare-and-set! !running false true))
    (try
      (set-state! :syncing :progress {})
      (locking !lock (run-sync!))
      (catch clojure.lang.ExceptionInfo e
        (let [kind (:type (ex-data e))]
          (when-not (= kind ::offline)
            (tel/log! {:level :warn :id ::sync-failed :data (dissoc (ex-data e) :body)} (.getMessage e)))
          (fail! (case kind ::offline :offline ::unauthorized :signed-out ::update-required :update-required :error)
            (when-not (= kind ::offline) (.getMessage e)))))
      ;; Throwable, not Exception: a StackOverflowError that escaped here left
      ;; the state at :syncing, so the download window froze on its last
      ;; progress with no message.
      (catch Throwable e
        (tel/error! {:id ::sync-failed} e)
        (fail! :error (or (.getMessage e) (str (class e)))))
      (finally (reset! !running false))))
  (status))

(defn note-offline!
  "The network check found freememo.net unreachable. Show the offline state,
   unless a sync runs now: that sync ends in its own state."
  []
  (when-not @!running
    (try (set-state! :offline)
      (catch Throwable e (tel/error! {:id ::note-offline-failed} e)))))

(defn choose-full-sync!
  "The user's answer to a full-sync choice: :download or :upload."
  [direction]
  (when (and (not @!stopped) (compare-and-set! !running false true))
    (try
      (set-state! :syncing)
      (locking !lock
        (case direction :download (full-download!) :upload (full-upload!)))
      (set-state! :idle)
      (catch Throwable e
        (tel/error! {:id ::full-sync-failed} e)
        (fail! :error (or (.getMessage e) (str (class e)))))
      (finally (reset! !running false))))
  (status))

(defn resume-after-sign-in!
  "A sign-in gave the link a new token (`freememo.local-library`). Show the
   syncing state at once, so a page that reads it does not still see
   :signed-out, then sync in the background."
  []
  (set-state! :syncing)
  (future (sync-now!))
  nil)

;; ── Scheduling (2.11) ──────────────────────────────────────────────

(defonce ^:private !scheduler (atom nil))
(defonce ^:private !online-listener? (atom false))

(defn start-scheduler!
  "2.11 — sync now, every five minutes, and whenever the link comes back
   online (2.11.1). Quit-time sync is Electron's call to /api/desktop/sync-now.
   Also lets syncs run again after `stop-scheduler!`."
  []
  (reset! !stopped false)
  (when-not @!scheduler
    (let [^ScheduledExecutorService ex (Executors/newSingleThreadScheduledExecutor
                                         (reify ThreadFactory
                                           (newThread [_ r] (doto (Thread. r "freememo-sync") (.setDaemon true)))))]
      (reset! !scheduler ex)
      ;; A task that throws is never run again by scheduleWithFixedDelay, so
      ;; the timer would stop for the life of the process.
      (let [run (fn [] (try (sync-now!) (catch Throwable e (tel/error! {:id ::scheduled-sync-failed} e))))]
        (.scheduleWithFixedDelay ex ^Runnable run 2 (* 60 interval-minutes) TimeUnit/SECONDS)
        ;; One listener for the life of the process: it uses the current executor.
        (when (compare-and-set! !online-listener? false true)
          (link/on-online! (fn []
                             (when-let [^ScheduledExecutorService ex @!scheduler]
                               (.execute ex ^Runnable (fn [] (try (sync-now!)
                                                                  (catch Throwable e (tel/error! {:id ::scheduled-sync-failed} e))))))))))))) 

(defn stop-scheduler!
  "Stop the timer and wait for a sync that runs now. A library removal calls
   this before it drops the schema.
   Post: no sync runs and none starts, until `start-scheduler!`."
  []
  (reset! !stopped true)
  (when-let [^ScheduledExecutorService ex (first (reset-vals! !scheduler nil))]
    (.shutdown ex))
  ;; `sync-now!` and `choose-full-sync!` set `!running` before they take the lock.
  (let [deadline (+ (System/currentTimeMillis) 120000)]
    (while (and @!running (< (System/currentTimeMillis) deadline))
      (Thread/sleep 50)))
  (locking !lock nil)
  nil)

;; ── Conflict report (§5) ───────────────────────────────────────────

(def ^:private table-labels
  {"topics" "Document" "flashcards" "Card" "media" "Image" "sources" "Source"})

(defn- conflict-summary [{:keys [id table_name reason title question]}]
  (let [plain-question (some-> question (str/replace #"<[^>]*>" ""))]
    {:id id
     :label (str (get table-labels table_name table_name) ": "
              (or (not-empty title)
                (when plain-question (subs plain-question 0 (min 80 (count plain-question))))
                "(untitled)"))
     :reason (case reason
               "deleted-elsewhere" "Its document was deleted on another device."
               "parent-deleted" "Its parent was deleted on another device."
               "storage-quota" "Your freememo.net storage is full."
               reason)}))

(defn open-conflicts
  "Read all unresolved conflict summaries for non-UI callers."
  []
  (mapv conflict-summary
    (jdbc/execute! db/ds ["SELECT id, table_name, reason, row_data->>'title' AS title,
                                  row_data->>'question' AS question
                           FROM sync_conflicts WHERE resolved_at IS NULL ORDER BY id DESC"] opts)))

(defn open-conflict-count
  "Count unresolved conflicts without loading their contents."
  []
  (:count (jdbc/execute-one! db/ds
            ["SELECT count(*)::int AS count FROM sync_conflicts WHERE resolved_at IS NULL"] opts)))

(defn open-conflict-window
  "Read one bounded range of unresolved conflict summaries."
  [offset limit]
  (mapv conflict-summary
    (jdbc/execute! db/ds
      ["SELECT id, table_name, reason, row_data->>'title' AS title,
               left(row_data->>'question', 160) AS question
          FROM sync_conflicts WHERE resolved_at IS NULL ORDER BY id DESC LIMIT ? OFFSET ?"
       (min 100 (max 0 (long limit))) (max 0 (long offset))] opts)))

(defn restore-conflict!
  "5.2 — put a left-out row back as a new local write, so the next sync
   uploads it. A topic goes under `parent-id` (nil: top level). A card goes
   into the document `parent-id`. Other rows keep their own parent.
   The row keeps its id: its id came from this device's block, so it is free,
   and its children in the report then restore under it unchanged.
   Post: {:ok true}, or {:ok false :error msg} with nothing written."
  [conflict-id parent-id]
  (try
    (jdbc/with-transaction [tx db/ds]
      (let [{:keys [table_name row_data]} (jdbc/execute-one! tx ["SELECT table_name, row_data::text AS row_data
                                                                  FROM sync_conflicts
                                                                  WHERE id = ? AND resolved_at IS NULL FOR UPDATE"
                                                                 conflict-id] opts)
            table (apply/synced-table table_name)
            meta (apply/table-meta tx table)
            row (json/parse-string row_data)
            row (case table
                  :topics (assoc row "parent_id" parent-id "user_id" (local-user-id))
                  :flashcards (if parent-id
                                (assoc row "topic_id" parent-id
                                  "root_topic_id" (:r (jdbc/execute-one! tx ["SELECT freememo_root_topic_id(?) AS r"
                                                                             parent-id] opts)))
                                row)
                  row)
            cols (filterv #(contains? row %)
                   (concat (:sent meta) (keys (:blob meta))))]
        (when (and (= table :flashcards) (nil? parent-id))
          (throw (ex-info "Choose the document the card goes into." {:type ::restore-refused})))
        (jdbc/execute! tx [(format "INSERT INTO %s (%s) SELECT %s FROM jsonb_populate_record(null::%s, ?::jsonb) r"
                             table_name (str/join ", " cols) (str/join ", " (map #(str "r." %) cols)) table_name)
                           (json/generate-string row)])
        (jdbc/execute! tx ["UPDATE sync_conflicts SET resolved_at = now(), resolution = 'restored' WHERE id = ?"
                           conflict-id])
        {:ok true}))
    (catch clojure.lang.ExceptionInfo e {:ok false :error (.getMessage e)})
    (catch java.sql.SQLException e
      {:ok false :error (if (= "23503" (.getSQLState e))
                          "Its parent is missing. Restore the parent first, or choose a document."
                          (.getMessage e))})))

(defn discard-conflict!
  "Drop a left-out row from the report for good."
  [conflict-id]
  (jdbc/execute! db/ds ["UPDATE sync_conflicts SET resolved_at = now(), resolution = 'discarded'
                         WHERE id = ? AND resolved_at IS NULL" conflict-id])
  {:ok true})
