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
   connection returns, before quit, and from Sync now (2.11).

   A local account (§15) syncs nothing. It picks its own id blocks. After its
   sign-in, the first syncs register those blocks and merge the library into
   the account (15.5.7, 15.6, 15.7)."
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
   rows, and during a full download or a merge the totals `:total-rows` and
   `:total-bytes`. A local account reports the state `:local` (15.2.2).
   The license cache is what an offline launch reads; the server stays the
   authority for every operation."
  []
  (let [row (jdbc/execute-one! db/ds ["SELECT checkpoint, last_sync_at, last_error, license,
                                              full_sync_required, merge_pending FROM sync_state"] opts)]
    (merge {:library-ready (pos? (or (:checkpoint row) 0))
            :last-sync-at (some-> (:last_sync_at row) str)
            :error (:last_error row)
            :full-sync-required (:full_sync_required row)
            :merge-pending (boolean (:merge_pending row))
            :license (some-> (:license row) str json/parse-string)
            :online (link/online?)
            :pending-conflicts (:n (jdbc/execute-one! db/ds ["SELECT count(*) AS n FROM sync_conflicts
                                                              WHERE resolved_at IS NULL"] opts))}
      (select-keys @!status [:state :progress])
      (when (link/local-account?) {:state :local}))))

(declare local-user-id)

(defn- set-state!
  "Move the sync state machine and refresh Settings (:desktop-mutations)."
  [state & {:as more}]
  (swap! !status merge {:state state} more)
  (when-let [user-id (local-user-id)]
    (commands/bump-channels! user-id #{:desktop-mutations})))

(defn- record!
  "Write columns of the one sync_state row."
  [cols]
  (let [sets (str/join ", " (map #(str (name %) (if (= :license %) " = ?::jsonb" " = ?")) (keys cols)))]
    (jdbc/execute! db/ds (into [(str "UPDATE sync_state SET " sets)] (vals cols)))))

;; ── HTTP ───────────────────────────────────────────────────────────

(defn- call!
  "One request to freememo.net. Answers {:status :body}.
   Post: a refused connection marks the link offline and throws ::offline.
         A 401 throws ::unauthorized: the token was revoked (3.2)."
  [method path & {:keys [json-body file query]}]
  (try
    (let [resp (http/request
                 (cond-> {:method method
                          :url (str (link/origin) path)
                          :headers {"Authorization" (str "Bearer " (link/token))
                                    "X-FreeMemo-Major" (str desktop-version/desktop-major)}
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
               {:headers {"Authorization" (str "Bearer " (link/token))
                          "X-FreeMemo-Major" (str desktop-version/desktop-major)}
                :query-params {"t" t "k" (json/generate-string k) "c" col}
                :as :stream :throw-exceptions false :socket-timeout 600000})]
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
   until the block runs out (1.2.4). A local account picks a random offline
   block itself (15.3.2).
   Post: at least `needed` unspent ids, or a thrown ::offline."
  ([] (ensure-id-blocks! 0))
  ([needed]
   (loop []
     (let [left (:n (jdbc/execute-one! db/ds ["SELECT coalesce(sum(block_end - next_id), 0) AS n
                                               FROM local_id_blocks"] opts))]
       (when (< left (max needed (quot schema/block-size 5)))
         (let [[{:keys [start end]} origin]
               (if (link/local-account?)
                 [(schema/random-offline-block) "offline"]
                 (let [{:strs [start end]} (ok-body! (call! :post "/api/desktop/id-block" :json-body {}) :id-block)]
                   [{:start start :end end} "server"]))]
           (jdbc/execute! db/ds ["INSERT INTO local_id_blocks (block_start, block_end, next_id, origin)
                                  VALUES (?, ?, ?, ?) ON CONFLICT (block_start) DO NOTHING"
                                 start end start origin])
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

;; ── 2. Graves ──────────────────────────────────────────────────────

(defn- graves-step!
  "Send local deletions, apply the server's. Returns the server's `to`."
  [user-id checkpoint]
  (let [local (mapv (fn [g] {"t" (:table_name g) "k" (json/parse-string (:row_key g))})
                (jdbc/execute! db/ds ["SELECT table_name, row_key::text AS row_key FROM sync_graves
                                       WHERE usn = -1"] opts))
        {:strs [graves to]} (ok-body! (call! :post "/api/sync/graves"
                                        :json-body {:checkpoint checkpoint :graves local
                                                    :major desktop-version/desktop-major})
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
                                     :json-body {:base base :rows batch :major desktop-version/desktop-major})
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
         :touched}."
  [user-id from {:keys [to exclude graves-from]}]
  (apply/with-apply-tx db/ds
    (fn [tx]
      (loop [cursor nil to to touched #{} first? true waiting [] waiting-files {}]
        (let [{:strs [rows graves cursor] :as page}
              (ok-body! (call! :post "/api/sync/download"
                          :json-body (cond-> {:from from :exclude (or exclude []) :cursor cursor
                                              :major desktop-version/desktop-major}
                                       to (assoc :to to)
                                       graves-from (assoc :graves_from graves-from)))
                :download)
              to (get page "to")
              files (merge waiting-files (fetch-blobs rows))
              batch (into waiting rows)
              g (when first? (apply/apply-graves! tx {:peer :desktop :user-id user-id} (or graves [])))
              r (apply/apply-rows! tx {:peer :desktop :user-id user-id :blob-file (blob-lookup files)} batch)
              missing (parent-missing-keys r)
              still-waiting (if cursor (filterv #(missing [(get % "t") (get % "k")]) batch) [])
              kept-files (select-keys files (blob-keys still-waiting))
              refused (if cursor
                        (remove #(missing [(get % "t") (get % "k")]) (:rejected r))
                        (:rejected r))
              touched (into touched (concat (:touched g) (:touched r)))]
          (delete-files! (apply dissoc files (keys kept-files)))
          (when (seq refused)
            (tel/log! {:level :warn :id ::download-rejected :data {:rejected (map #(dissoc % "r") refused)}}
              "Some downloaded rows did not apply"))
          (swap! !status update-in [:progress :downloaded] (fnil + 0) (count rows))
          (commands/bump-channels! user-id #{:desktop-mutations})
          (if cursor
            (recur cursor to touched false still-waiting kept-files)
            {:to to :touched touched}))))))

;; ── 6. Sanity (2.6) ────────────────────────────────────────────────

(defn- sane? [user-id]
  (let [{:keys [counts pending]} (local-counts db/ds user-id)]
    (if (> (reduce + (map count (vals pending))) pending-key-cap)
      true
      (get (ok-body! (call! :post "/api/sync/finish"
                       :json-body {:counts counts :pending pending :major desktop-version/desktop-major})
             :finish)
        "ok"))))

;; ── Full sync (2.8) ────────────────────────────────────────────────

(defn- local-empty? []
  (every? (fn [table]
            (nil? (jdbc/execute-one! db/ds [(str "SELECT 1 FROM " (name table) " LIMIT 1")])))
    fmt/synced-tables))

(defn full-download!
  "Replace this desktop's data with the account's (Anki's download). Pending
   local rows go to the conflict report on the way out, so nothing is lost
   without a trace."
  []
  (let [user-id (local-user-id)
        {:strs [to scm rows bytes]} (ok-body! (call! :post "/api/sync/full/download"
                                                :json-body {:major desktop-version/desktop-major})
                                      :full-download)]
    ;; An older server reports no totals. Leave them out rather than nil.
    (swap! !status assoc :progress (cond-> {}
                                     (number? rows) (assoc :total-rows rows)
                                     (number? bytes) (assoc :total-bytes bytes)))
    (apply/with-apply-tx db/ds
      (fn [tx]
        (doseq [table (reverse fmt/synced-tables)]
          (jdbc/execute! tx [(str "DELETE FROM " (name table))]))
        (jdbc/execute! tx ["DELETE FROM sync_graves"])))
    (let [{:keys [to touched]} (download-step! user-id 0 {:to to})]
      (record! {:checkpoint to :scm scm :full_sync_required false :last_error nil
                :last_sync_at (java.sql.Timestamp/from (java.time.Instant/now))})
      (commands/bump-channels! user-id (apply/channels-for (into touched fmt/synced-tables))))))

(defn full-upload!
  "Replace the account's data on freememo.net with this desktop's (Anki's
   upload). Every other desktop of the account must full-sync next."
  []
  (let [user-id (local-user-id)
        {:strs [base scm]} (ok-body! (call! :post "/api/sync/full/upload"
                                       :json-body {:major desktop-version/desktop-major})
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

;; ── Merge of a local account (15.5.7, 15.6, 15.7) ─────────────────

(defn- pending-count []
  (reduce + (for [table fmt/synced-tables]
              (:n (jdbc/execute-one! db/ds [(str "SELECT count(*) AS n FROM " (name table) " WHERE usn = -1")] opts)))))

(defn- register-offline-blocks!
  "15.5.7 — tell freememo.net which offline blocks this library used, and
   renumber the rows of each block it refused (15.5.9).
   Pre:  `merge_pending` is set, so the re-key closed every offline block.
   Post: every offline block left in `local_id_blocks` is the account's on the
         server. Safe to rerun: the server accepts a block it already holds."
  []
  (let [blocks (jdbc/execute! db/ds ["SELECT block_start, block_end FROM local_id_blocks
                                      WHERE origin = 'offline' ORDER BY block_start"] opts)]
    (when (seq blocks)
      (let [{:strs [refused]} (ok-body! (call! :post "/api/desktop/offline-blocks"
                                          :json-body {:blocks (mapv (fn [b] {:start (:block_start b) :end (:block_end b)})
                                                                blocks)})
                                :offline-blocks)]
        (doseq [{:strs [start end]} refused]
          (let [used (reduce + (for [table fmt/synced-tables
                                     :when (= ["id"] (:key (apply/table-meta db/ds table)))]
                                 (:n (jdbc/execute-one! db/ds [(format "SELECT count(*) AS n FROM %s WHERE id >= ? AND id < ?"
                                                                 (name table)) start end] opts))))]
            (ensure-id-blocks! used)
            (let [moved (apply/with-apply-tx db/ds
                          (fn [tx]
                            (let [n (apply/renumber-block! tx {:start start :end end})]
                              (jdbc/execute! tx ["DELETE FROM local_id_blocks WHERE block_start = ?" start])
                              n)))]
              (tel/log! {:level :warn :id ::offline-block-refused :data {:start start :rows moved}}
                "freememo.net refused an offline id block; its rows took new ids"))))))))

(defn- close-merge! []
  (ok-body! (call! :post "/api/desktop/merge-done" :json-body {}) :merge-done)
  (record! {:merge_pending false}))

(declare incremental-sync!)

(defn- merge-sync!
  "15.6.1 — the first sync of a re-keyed local account, with a license that
   allows sync: adopt the account's epoch with no wipe, then sync from 0, so
   the local rows go up and the account's rows come down. Both keep their ids.
   Post: `merge_pending` is clear, and the token's merge window is closed."
  [user-id body]
  (let [{:strs [rows bytes]} (ok-body! (call! :post "/api/sync/full/download"
                                         :json-body {:major desktop-version/desktop-major})
                               :merge-totals)]
    ;; 15.6.5 — that route only reports the window and the totals. It changes
    ;; nothing on the server, and this sync wipes nothing here.
    (swap! !status assoc :progress (cond-> {}
                                     (number? rows) (assoc :total-rows rows)
                                     (number? bytes) (assoc :total-bytes bytes))))
  (record! {:scm (get body "scm") :checkpoint 0 :full_sync_required false})
  (incremental-sync! user-id body)
  (close-merge!))

(defn- merge-upload-only!
  "15.7.3 — the merge of an account that no license lets sync: upload only,
   inside the token's merge window. No graves and no download, because both
   apply server data, and no sanity check, which needs a download.
   Post: once no pending row is left, the window is closed and `merge_pending`
         is clear. The epoch is recorded, so a later license makes the next
         sync an incremental one from checkpoint 0 (15.7.4), not a full-sync
         choice."
  [user-id body]
  (record! {:scm (get body "scm") :full_sync_required false})
  (upload-step! user-id 0)
  (when (zero? (pending-count))
    (close-merge!)))

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
        state (sync-row)
        merging? (:merge_pending state)]
    (ensure-id-blocks!)
    (when merging? (register-offline-blocks!))
    (let [{:keys [status body]} (call! :post "/api/sync/meta"
                                  :json-body {:protocol desktop-version/sync-protocol
                                              :major desktop-version/desktop-major})]
      (case (long status)
        409 (do (set-state! :update-required) (record! {:last_error (get body "error")}))
        200 (let [license (get body "license")]
              (record! {:license (json/generate-string license)})
              (swap! !status assoc :license license)
              (cond
                (and merging? (= "full" (get body "sync")))
                (do (merge-sync! user-id body)
                    (record! {:last_sync_at (java.sql.Timestamp/from (java.time.Instant/now))
                              :last_error nil})
                    (set-state! :idle))

                (and merging? (get license "merge-upload?"))
                (do (merge-upload-only! user-id body)
                    (set-state! :license-refused))

                ;; 15.7.5 — a closed window: the rows wait for a license.
                merging?
                (set-state! :license-refused)

                (= "none" (get body "sync"))
                (set-state! :license-refused)

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

(defonce ^:private !lock (Object.))
(defonce ^:private !running (atom false))

(defn sync-now!
  "Run one sync unless one is already running.
   Post: returns the status after the sync. Errors are recorded and shown,
         never thrown, because the callers are a timer, Electron and a button."
  []
  ;; 15.2.2 — a local account has nothing to sync with.
  (when (and (not (link/local-account?)) (compare-and-set! !running false true))
    (try
      (set-state! :syncing :progress {})
      (locking !lock (run-sync!))
      ;; The error is written before the state moves: moving the state is
      ;; what refreshes Settings, and it must find the error already there.
      (catch clojure.lang.ExceptionInfo e
        (let [kind (:type (ex-data e))]
          (when-not (= kind ::offline)
            (record! {:last_error (.getMessage e)})
            (tel/log! {:level :warn :id ::sync-failed :data (dissoc (ex-data e) :body)} (.getMessage e)))
          (set-state! (case kind ::offline :offline ::unauthorized :signed-out :error))))
      (catch Exception e
        (record! {:last_error (or (.getMessage e) (str (class e)))})
        (tel/error! {:id ::sync-failed} e)
        (set-state! :error))
      (finally (reset! !running false))))
  (status))

(defn choose-full-sync!
  "The user's answer to a full-sync choice: :download or :upload."
  [direction]
  (when (compare-and-set! !running false true)
    (try
      (set-state! :syncing)
      (locking !lock
        (case direction :download (full-download!) :upload (full-upload!)))
      (set-state! :idle)
      (catch Exception e
        (record! {:last_error (or (.getMessage e) (str (class e)))})
        (tel/error! {:id ::full-sync-failed} e)
        (set-state! :error))
      (finally (reset! !running false))))
  (status))

;; ── Scheduling (2.11) ──────────────────────────────────────────────

(defonce ^:private !scheduler (atom nil))

(defn start-scheduler!
  "2.11 — sync now, every five minutes, and whenever the link comes back
   online (2.11.1). Quit-time sync is Electron's call to /api/desktop/sync-now."
  []
  (when-not @!scheduler
    (let [^ScheduledExecutorService ex (Executors/newSingleThreadScheduledExecutor
                                         (reify ThreadFactory
                                           (newThread [_ r] (doto (Thread. r "freememo-sync") (.setDaemon true)))))]
      (reset! !scheduler ex)
      (.scheduleWithFixedDelay ex ^Runnable sync-now! 2 (* 60 interval-minutes) TimeUnit/SECONDS)
      (link/on-online! #(.execute ex ^Runnable sync-now!)))))

(defn start-local-block-timer!
  "15.2.2, 15.3.2 — a local account syncs nothing, so no sync scheduler runs.
   This checks its offline id blocks at boot, before the first request, and
   once a minute after, so an import never finds them spent.
   Post: at least 20 % of a block is unspent when this returns."
  []
  (ensure-id-blocks!)
  (when-not @!scheduler
    (let [^ScheduledExecutorService ex (Executors/newSingleThreadScheduledExecutor
                                         (reify ThreadFactory
                                           (newThread [_ r] (doto (Thread. r "freememo-id-blocks") (.setDaemon true)))))]
      (reset! !scheduler ex)
      (.scheduleWithFixedDelay ex
        ^Runnable #(try (ensure-id-blocks!)
                        (catch Exception e (tel/error! {:id ::offline-block-failed} e)))
        60 60 TimeUnit/SECONDS))))

;; ── Conflict report (§5) ───────────────────────────────────────────

(def ^:private table-labels
  {"topics" "Document" "flashcards" "Card" "media" "Image" "sources" "Source"})

(defn open-conflicts
  "5.2 — the rows a sync left out, newest first, with a label a person reads."
  []
  (mapv (fn [{:keys [id table_name reason title question]}]
          {:id id
           :label (str (get table-labels table_name table_name) ": "
                    (or (not-empty title) (some-> question (str/replace #"<[^>]*>" "") (subs 0 (min 80 (count question))))
                      "(untitled)"))
           :reason (case reason
                     "deleted-elsewhere" "Its document was deleted on another device."
                     "parent-deleted" "Its parent was deleted on another device."
                     "storage-quota" "Your freememo.net storage is full."
                     reason)})
    (jdbc/execute! db/ds ["SELECT id, table_name, reason, row_data->>'title' AS title,
                                  row_data->>'question' AS question
                           FROM sync_conflicts WHERE resolved_at IS NULL ORDER BY id DESC"] opts)))

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
