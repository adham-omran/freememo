(ns freememo.sync-server
  "The freememo.net side of desktop row sync (plans/electron-local-db-sync.md
   §2). Every route takes the desktop token (3.2) and answers JSON.

   | Route                       | Step (2.3) | Operation (13.4) |
   |-----------------------------|------------|------------------|
   | POST /api/sync/meta         | 1          | :meta            |
   | POST /api/sync/graves       | 2          | :graves          |
   | PUT  /api/sync/blob/<sha>   | 3          | :upload          |
   | POST /api/sync/upload       | 3          | :upload          |
   | POST /api/sync/download     | 4          | :download        |
   | GET  /api/sync/blob         | 4          | :blob            |
   | POST /api/sync/finish       | 6, 7       | :finish          |
   | POST /api/sync/full/upload  | 2.8        | :full-upload     |
   | POST /api/sync/full/download| 2.8        | :full-download   |

   **The usn window (2.2).** A row's usn is the id of the transaction that
   last wrote it. A download serves `from ≤ usn < to`, where `to` is the
   snapshot xmin: every transaction below it has finished, so no row below
   `to` can still appear. The client stores `to` as its next `from`.

   **Scope (2.10).** Every query is limited to the token's account through
   `sync-scope`, the export scopes without their staged-delete filter."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [cheshire.core :as json]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]
            [taoensso.telemere :as tel]
            [freememo.account-transfer-format :as fmt]
            [freememo.commands :as commands]
            [freememo.db :as db]
            [freememo.desktop-auth :as desktop-auth]
            [freememo.desktop-license :as license]
            [freememo.desktop-version :as desktop-version]
            [freememo.largeobj :as lo]
            [freememo.quota :as quota]
            [freememo.sync-apply :as apply]
            [freememo.sync-schema :as schema])
  (:import [java.io File]
           [java.security DigestInputStream MessageDigest]))

(def ^:private opts {:builder-fn rs/as-unqualified-lower-maps})

(def page-rows
  "Rows per download page."
  500)

;; ── Scope ──────────────────────────────────────────────────────────

(defn sync-scope
  "The rows of `table` that belong to the account: the export scope (2.10)
   without its staged-delete filter. A staged topic still exists, and its rows
   must keep syncing until the purge turns it into graves.
   Post: {:where sql :params n}; `t` is the table alias."
  [table]
  (let [{:keys [where params]} (or (fmt/entity-scopes table)
                                   (throw (ex-info (str "no scope for " (name table))
                                            {:type ::missing-scope :table table})))]
    {:where (str/replace where #"\s+AND\s+(?:\w+\.)?staged_delete_id IS NULL" "")
     :params params}))

(defn scope-params [table user-id]
  (repeat (:params (sync-scope table)) user-id))

(defn snapshot-xmin
  "Every transaction id below this has finished (2.2)."
  [conn]
  (:x (jdbc/execute-one! conn ["SELECT pg_snapshot_xmin(pg_current_snapshot())::text::bigint AS x"] opts)))

;; ── Request plumbing ───────────────────────────────────────────────

(defn- json-response [status body]
  {:status status :headers {"Content-Type" "application/json"} :body (json/generate-string body)})

(defn- body-json [request]
  (some-> (:body request) slurp not-empty json/parse-string))

(defn with-access
  "Run `(f holder body access)` when the token is valid and its account may
   perform `operation` (13.4). Every route goes through here, so no route
   carries its own license check.
   Post: 401 for a missing or revoked token, 403 with the reason code for a
         refused operation (13.4.3)."
  [request operation f]
  (if-let [holder (desktop-auth/bearer-token request)]
    (let [body (when (#{:post} (:request-method request)) (body-json request))
          major (or (some-> (get body "major") long)
                  (some-> (get-in request [:headers "x-freememo-major"]) parse-long)
                  desktop-version/desktop-major)
          access (license/token-access db/ds (:user-id holder) (:token-id holder) major)]
      (if (license/allows? access operation)
        (f holder body access)
        (json-response 403 {:error (license/reason access) :license access})))
    (json-response 401 {:error "unauthorized"})))

;; ── 1. meta (2.3, 2.3.2) ───────────────────────────────────────────

(defn meta-handler
  "Step 1. A protocol mismatch refuses the sync and asks for an update. The
   answer always carries the license, so the desktop can cache it (13.6) and
   stop before graves when sync is refused (2.3.2)."
  [request]
  (with-access request :meta
    (fn [{:keys [user-id]} body access]
      (let [protocol (get body "protocol")]
        (if (not= protocol desktop-version/sync-protocol)
          (json-response 409 {:error (if (and (number? protocol)
                                           (< protocol desktop-version/sync-protocol))
                                       "update-required" "server-outdated")
                              :protocol desktop-version/sync-protocol})
          (let [scm (:scm (jdbc/execute-one! db/ds ["INSERT INTO desktop_accounts (user_id, scm)
                                                     VALUES (?, md5(random()::text))
                                                     ON CONFLICT (user_id) DO UPDATE SET scm = desktop_accounts.scm
                                                     RETURNING scm" user-id] opts))]
            (json-response 200 {:protocol desktop-version/sync-protocol
                                :scm scm
                                :license access
                                :sync (name (:sync access))})))))))

;; ── 2. graves ──────────────────────────────────────────────────────

(defn- server-graves [conn user-id from to]
  (mapv (fn [g] {"t" (:table_name g) "k" (json/parse-string (str (:row_key g)))})
    (jdbc/execute! conn ["SELECT table_name, row_key::text AS row_key FROM sync_graves
                          WHERE user_id = ? AND usn >= ? AND usn < ?" user-id from to] opts)))

(defn graves-handler
  "Step 2. Apply the desktop's deletions, then answer the server's deletions
   since `checkpoint`. Graves move before rows, so a delete wins over an edit
   made elsewhere (6.2).
   Post: {:graves [...] :to xmin}. The desktop uploads with `base = to`."
  [request]
  (with-access request :graves
    (fn [{:keys [user-id]} body _]
      (let [result (apply/with-apply-tx db/ds
                     #(apply/apply-graves! % {:peer :server :user-id user-id} (get body "graves" [])))
            to (snapshot-xmin db/ds)]
        (when (seq (:touched result))
          (commands/bump-channels! user-id (apply/channels-for (:touched result))))
        (json-response 200 {:graves (server-graves db/ds user-id (get body "checkpoint" 0) to)
                            :to to
                            :deleted (:deleted result)})))))

;; ── 3. blobs up, rows up ───────────────────────────────────────────

(def ^:private blob-root
  (io/file (System/getProperty "java.io.tmpdir") "freememo-sync-blobs"))

(defn- blob-file ^File [user-id sha]
  (when (and (string? sha) (re-matches #"[0-9a-f]{64}" sha))
    (io/file blob-root (str user-id) sha)))

(def ^:private max-blob-bytes (* 8 1024 1024 1024))

(defn blob-put-handler
  "PUT /api/sync/blob/<sha256>. Stores one blob for an upload to name.
   Post: the file exists only when its bytes hash to the name. A blob larger
         than the account's free storage is refused before it is read."
  [request]
  (with-access request :upload
    (fn [{:keys [user-id]} _ _]
      (let [sha (last (str/split (:uri request) #"/"))
            target (blob-file user-id sha)
            length (some-> (get-in request [:headers "content-length"]) parse-long)
            remaining (quota/remaining-bytes db/ds user-id)]
        (cond
          (nil? target) (json-response 400 {:error "bad-sha256"})
          (nil? length) (json-response 411 {:error "length-required"})
          (or (> length max-blob-bytes) (and remaining (> length remaining)))
          (json-response 413 {:error "storage-quota"})
          :else
          (let [tmp (File/createTempFile "blob" ".part" (doto (.getParentFile target) .mkdirs))
                md (MessageDigest/getInstance "SHA-256")]
            (with-open [in (DigestInputStream. (:body request) md)]
              (io/copy in tmp))
            (if (= sha (apply str (map #(format "%02x" %) (.digest md))))
              (do (.renameTo tmp target) (json-response 200 {:ok true}))
              (do (.delete tmp) (json-response 400 {:error "hash-mismatch"})))))))))

(defn upload-handler
  "Step 3. Apply the desktop's pending rows in one transaction (2.4).
   Post: {:xid :rejected :remaps :corrections}. `xid` is this transaction,
         which the next download skips (2.3.1). `corrections` are the rows the
         desktop must take back because the server's state differs from what
         it sent: a merged column, a remapped id, a reminted ordinal, a
         history row (D4)."
  [request]
  (with-access request :upload
    (fn [{:keys [user-id]} body _]
      (let [base (long (get body "base" 0))
            ctx {:peer :server :user-id user-id :base-usn base
                 :blob-file (fn [row col] (let [f (blob-file user-id (get-in row ["b" col "sha256"]))]
                                            (when (and f (.exists f)) f)))}
            {:keys [xid result corrections]}
            (apply/with-apply-tx db/ds
              (fn [tx]
                (let [result (apply/apply-rows! tx ctx (get body "rows" []))]
                  {:xid (:x (jdbc/execute-one! tx ["SELECT pg_current_xact_id()::text::bigint AS x"] opts))
                   :result result
                   :corrections (into [] (keep (fn [[table k]] (apply/read-row tx table k)))
                                  (:corrections result))})))]
        (doseq [row (get body "rows" []) [_ b] (get row "b")]
          (some-> (blob-file user-id (get b "sha256")) .delete))
        (when (seq (:touched result))
          (commands/bump-channels! user-id (apply/channels-for (:touched result))))
        (json-response 200 {:xid xid
                            :applied (:applied result)
                            :rejected (:rejected result)
                            :remaps (into {} (for [[t m] (:remaps result)]
                                               [(name t) (into {} (for [[a b] m] [(str a) b]))]))
                            :corrections corrections})))))

;; ── 4. rows down ───────────────────────────────────────────────────

(defn- table-page
  "Up to `limit` rows of one table in the usn window, after `after-key`."
  [conn user-id table from to exclude after-key limit]
  (let [{:keys [where]} (sync-scope table)
        meta (apply/table-meta conn table)
        key-cols (:key meta)
        key-list (str/join ", " (map #(str "t." %) key-cols))
        after (when after-key
                (format " AND (%s) > (SELECT %s FROM jsonb_populate_record(null::%s, ?::jsonb) k)"
                  key-list (str/join ", " (map #(str "k." %) key-cols)) (name table)))]
    (apply/read-rows conn table
      (str "(" where ") AND t.usn >= ? AND t.usn < ? AND NOT (t.usn = ANY(?))"
        after " ORDER BY " key-list " LIMIT " limit)
      (concat (scope-params table user-id) [from to (long-array exclude)]
        (when after-key [(json/generate-string after-key)])))))

(defn download-handler
  "Step 4, one page. The first page fixes `to`; later pages pass it back, so a
   multi-page download reads one consistent window.
   Post: {:to :rows :graves :cursor}. `cursor` is nil after the last page.
         Graves come with the first page, for the window after the graves
         step's `to`; a download from 0 (a full download) carries none."
  [request]
  (with-access request :download
    (fn [{:keys [user-id]} body _]
      (let [from (long (get body "from" 0))
            to (or (some-> (get body "to") long) (snapshot-xmin db/ds))
            exclude (mapv long (get body "exclude" []))
            [start-index after-key] (or (some-> (get body "cursor") ((juxt #(get % "table") #(get % "after"))))
                                      [0 nil])
            tables fmt/synced-tables]
        (loop [i start-index after after-key rows []]
          (cond
            (>= i (count tables))
            (json-response 200 {:to to :rows rows :cursor nil
                                :graves (when (and (nil? (get body "cursor")) (pos? from))
                                          (server-graves db/ds user-id (get body "graves_from" from) to))})

            (>= (count rows) page-rows)
            (json-response 200 {:to to :rows rows :cursor {:table i :after after}
                                :graves (when (and (nil? (get body "cursor")) (pos? from))
                                          (server-graves db/ds user-id (get body "graves_from" from) to))})

            :else
            (let [table (nth tables i)
                  page (table-page db/ds user-id table from to exclude after (- page-rows (count rows)))]
              (if (< (count page) (- page-rows (count rows)))
                (recur (inc i) nil (into rows page))
                (recur i (get (peek page) "k") (into rows page))))))))))

(defn blob-get-handler
  "GET /api/sync/blob?t=&k=&c= — the bytes of one blob column, streamed.
   Post: 404 unless the row exists and the account owns it (2.10)."
  [request]
  (with-access request :blob
    (fn [{:keys [user-id]} _ _]
      (let [{:strs [t k c]} (:params request)
            table (apply/synced-table t)
            kind (get-in schema/blob-columns [table c])
            key-map (some-> k json/parse-string)]
        (if-not (and kind (map? key-map))
          (json-response 400 {:error "bad-blob-ref"})
          (let [meta (apply/table-meta db/ds table)
                row (jdbc/execute-one! db/ds
                      [(format "SELECT %s AS v, freememo_sync_owner(?, ?::jsonb) AS owner
                                FROM %s t WHERE (%s) = (SELECT %s FROM jsonb_populate_record(null::%s, ?::jsonb) k)"
                         (if (= kind :bytea) (str "t." c) (str "t." c "::bigint"))
                         (name table)
                         (str/join ", " (map #(str "t." %) (:key meta)))
                         (str/join ", " (map #(str "k." %) (:key meta)))
                         (name table))
                       t k k] opts)]
            (cond
              (or (nil? row) (nil? (:v row)) (not= (some-> (:owner row) long) (long user-id)))
              (json-response 404 {:error "not-found"})

              (= kind :bytea)
              {:status 200 :headers {"Content-Type" "application/octet-stream"}
               :body (io/input-stream ^bytes (:v row))}

              :else
              (let [size (with-open [conn (jdbc/get-connection db/ds)]
                           (jdbc/with-transaction [tx conn] (lo/size tx (:v row))))]
                {:status 200
                 :headers {"Content-Type" "application/octet-stream"
                           "Content-Length" (str size)}
                 :body (lo/open-range-stream! db/ds (:v row) 0 size)}))))))))

;; ── 6, 7. sanity and finish (2.6) ──────────────────────────────────

(defn server-counts
  "Rows per synced table in the account's scope, minus the rows the desktop
   still holds as pending edits (it does not count those)."
  [conn user-id pending]
  (into {}
    (for [table fmt/synced-tables
          :let [{:keys [where]} (sync-scope table)
                meta (apply/table-meta conn table)
                keys-json (json/generate-string (get pending (name table) []))
                n (:n (jdbc/execute-one! conn
                        (into [(format "SELECT count(*) AS n FROM %s t WHERE (%s)
                                        AND NOT EXISTS (SELECT 1 FROM jsonb_array_elements(?::jsonb) p
                                                        WHERE (%s) = (SELECT %s FROM jsonb_populate_record(null::%s, p.value) k))"
                                 (name table) where
                                 (str/join ", " (map #(str "t." %) (:key meta)))
                                 (str/join ", " (map #(str "k." %) (:key meta)))
                                 (name table))]
                          (concat (scope-params table user-id) [keys-json]))
                        opts))]]
      [(name table) n])))

(defn finish-handler
  "Steps 6 and 7. Compare the desktop's row counts with the server's.
   Post: {:ok true} or {:ok false :mismatches {table [desktop server]}}. The
         desktop retries once and then marks a full sync required (2.6)."
  [request]
  (with-access request :finish
    (fn [{:keys [user-id]} body _]
      (let [counts (get body "counts" {})
            server (server-counts db/ds user-id (get body "pending" {}))
            mismatches (into {} (for [[t n] server
                                      :let [d (get counts t)]
                                      :when (and (some? d) (not= (long d) (long n)))]
                                  [t [d n]]))]
        (when (seq mismatches)
          (tel/log! {:level :warn :id ::sanity-mismatch :data {:user-id user-id :mismatches mismatches}}
            "Desktop sync sanity check found different row counts"))
        (json-response 200 (if (empty? mismatches) {:ok true} {:ok false :mismatches mismatches}))))))

;; ── Full sync (2.8) ────────────────────────────────────────────────

(defn full-upload-handler
  "Replace the account's server data with the desktop's (Anki's upload).
   Deletes every synced row in scope, frees the storage it held, and starts a
   new sync epoch, so every other desktop must full-sync next (2.8).
   Post: {:base :scm}. The desktop then uploads every row with `base`, which
         lies above the graves this wipe wrote, so none of them refuses a row."
  [request]
  (with-access request :full-upload
    (fn [{:keys [user-id]} _ _]
      (let [{:keys [base scm]}
            (apply/with-apply-tx db/ds
              (fn [tx]
                (doseq [table (reverse fmt/synced-tables)
                        :let [{:keys [where]} (sync-scope table)]]
                  (jdbc/execute! tx (into [(format "DELETE FROM %s t WHERE %s" (name table) where)]
                                      (scope-params table user-id))))
                (jdbc/execute! tx ["UPDATE users SET usage_bytes = 0 WHERE id = ?" user-id])
                {:base (inc (:x (jdbc/execute-one! tx ["SELECT pg_current_xact_id()::text::bigint AS x"] opts)))
                 :scm (:scm (jdbc/execute-one! tx ["UPDATE desktop_accounts SET scm = md5(random()::text)
                                                    WHERE user_id = ? RETURNING scm" user-id] opts))}))]
        (commands/bump-channels! user-id (apply/channels-for fmt/synced-tables))
        (json-response 200 {:base base :scm scm})))))

(def ^:private blob-size-columns
  {:media "byte_size" :topic_files "file_size" :topic_videos "byte_size" :topic_epubs "byte_size"})

(defn account-bytes
  "The bytes of every file the account stores, as a full download moves them."
  [conn user-id]
  (reduce + (for [[table col] blob-size-columns
                  :let [{:keys [where]} (sync-scope table)]]
              (or (:n (jdbc/execute-one! conn
                        (into [(format "SELECT sum(t.%s)::bigint AS n FROM %s t WHERE %s" col (name table) where)]
                          (scope-params table user-id))
                        opts))
                  0))))

(defn full-download-handler
  "Start a full download (Anki's download). The desktop clears its own data
   and downloads from usn 0. This route fixes the window and the epoch, and
   says how much is coming, so the desktop can show progress.
   Post: {:to :scm :rows :bytes}."
  [request]
  (with-access request :full-download
    (fn [{:keys [user-id]} _ _]
      (json-response 200 {:to (snapshot-xmin db/ds)
                          :scm (:scm (jdbc/execute-one! db/ds ["SELECT scm FROM desktop_accounts WHERE user_id = ?"
                                                                user-id] opts))
                          :rows (reduce + (vals (server-counts db/ds user-id {})))
                          :bytes (account-bytes db/ds user-id)}))))
