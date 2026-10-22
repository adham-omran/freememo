(ns freememo.sync-apply
  "Reading and applying sync rows, shared by both peers
   (plans/electron-local-db-sync.md §2.4–2.9).

   One `apply-rows!` has two callers: the server's upload route and the
   desktop's download (2.4.1). They differ only in `ctx`:

   | key        | server                          | desktop                  |
   |------------|---------------------------------|--------------------------|
   | :peer      | :server                         | :desktop                 |
   | :user-id   | the token's account             | the local account        |
   | :base-usn  | the client's checkpoint (2.4.2) | unused                   |
   | :blob-file | the uploaded blob for a row     | the downloaded blob      |

   A wire row is `{\"t\" table \"k\" key \"r\" row \"b\" blobs}`. `r` holds
   column values as `to_jsonb` writes them, so `jsonb_populate_record` reads
   them back without a type table here. `b` names the blob columns whose bytes
   travel separately (2.7).

   **Blame.** The wire is untrusted on the server. A row naming an unknown
   table, another account's row or an id outside the account's blocks is
   rejected, never thrown, because a buggy or hostile client is routine input
   there. A schema fact missing from `freememo.sync-schema` throws, because
   that is a bug in this build."
  (:require [clojure.string :as str]
            [clojure.walk :as walk]
            [cheshire.core :as json]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]
            [freememo.account-import :as import]
            [freememo.account-transfer-format :as fmt]
            [freememo.db :as db]
            [freememo.largeobj :as lo]
            [freememo.quota :as quota]
            [freememo.sync-schema :as schema])
  (:import [java.nio.file Files]
           [java.sql Connection]
           [org.postgresql.util PGobject PSQLException]))

;; ── Table metadata ─────────────────────────────────────────────────

(def ^:private synced-by-name
  (into {} (map (juxt name identity)) fmt/synced-tables))

(def ^:private table-rank
  (into {} (map-indexed (fn [i t] [t i])) fmt/synced-tables))

(defn synced-table
  "The synced table a wire name denotes, or nil."
  [table-name]
  (synced-by-name table-name))

(def ^:private !table-meta (atom {}))

(defn table-meta
  "Facts about one synced table, read from the catalog once per process.
   `:sent` is every column a payload may carry: all columns except generated,
   peer-local, derived and blob columns (1.6)."
  [conn table]
  (or (@!table-meta table)
      (let [cols (schema/columns conn table)
            key-cols (schema/primary-key conn table)
            blob (schema/blob-columns table {})
            excluded (set (concat (map :name (filter :generated? cols))
                            (schema/peer-local-columns table)
                            (schema/derived-columns table)
                            (keys blob)))
            sent (into [] (comp (map :name) (remove excluded)) cols)
            group (schema/schedule-groups table)
            schedule-cols (when group
                            (set (case (:columns group)
                                   :all (remove (set (concat key-cols ["usn" "mtime"])) sent)
                                   :fsrs (filter #(str/starts-with? % "fsrs_") sent)
                                   (:columns group))))
            meta {:table table
                  :name (name table)
                  :class (fmt/sync-classes table)
                  :key key-cols
                  :types (into {} (map (juxt :name :type)) cols)
                  :sent (set sent)
                  :sent-cols sent
                  :excluded excluded
                  :blob blob
                  :schedule (when group {:clock (:clock group) :columns schedule-cols})
                  :watermark (set (schema/watermark-columns table))}]
        (swap! !table-meta assoc table meta)
        meta)))

(defn- row-json
  "SQL for the `:sent` columns of the row aliased `t`, as one jsonb object.
   It names the columns it keeps. `to_jsonb(t) - excluded` would build the
   whole row first, and `to_jsonb` renders a `bytea` as a hex string of two
   characters per byte. A jsonb string holds at most 268435455 bytes, so one
   PDF over 128 MiB in `topic_files.file_data` failed every read of its page,
   although the column was never sent."
  [sent-cols]
  (format "(SELECT to_jsonb(s) FROM (SELECT %s) s)"
    (str/join ", " (map #(str "t.\"" % "\"") sent-cols))))

(defn- ->json ^PGobject [v]
  (doto (PGobject.) (.setType "jsonb") (.setValue (json/generate-string v))))

(defn- key-match
  "SQL that matches the row whose key a ?::jsonb parameter holds."
  [{:keys [name key]}]
  (format "(%s) = (SELECT %s FROM jsonb_populate_record(null::%s, ?::jsonb) k)"
    (str/join ", " (map #(str "t." %) key))
    (str/join ", " (map #(str "k." %) key))
    name))

;; ── Reading rows (2.2, 2.3.1) ──────────────────────────────────────

(defn read-rows
  "Wire rows of `table` that `where` selects, with `t` as the table alias.
   Pre:  `params` bind the placeholders in `where`.
   Post: each row's `r` holds only `:sent` columns; `b` maps each blob column
         that holds a value to {}."
  [conn table where params]
  (let [{:keys [name key sent-cols blob]} (table-meta conn table)
        flags (for [[c _] blob] (format ", (t.%s IS NOT NULL) AS has_%s" c c))
        sql (format "SELECT %s::text AS r%s FROM %s t WHERE %s"
              (row-json sent-cols) (apply str flags) name where)]
    (mapv (fn [rec]
            (let [r (json/parse-string (:r rec))]
              {"t" name
               "k" (select-keys r key)
               "r" r
               "b" (into {} (for [[c _] blob :when (get rec (keyword (str "has_" c)))] [c {}]))}))
      (jdbc/execute! conn (into [sql] params) {:builder-fn rs/as-unqualified-lower-maps}))))

(defn read-row
  "The wire row for one key, or nil."
  [conn table key-map]
  (let [meta (table-meta conn table)]
    (first (read-rows conn table (key-match meta) [(->json key-map)]))))

;; ── Apply transaction ──────────────────────────────────────────────

(defn with-apply-tx
  "Run `(f tx)` in one transaction with the sync bypass on (1.5).
   Post: every write `f` makes keeps the payload's usn and mtime on a desktop,
         and its mtime on the server. Nothing outside `f` sees the bypass,
         because the setting is transaction-local. Every document summary
         that `f`'s writes affect is refreshed once, before the commit
         (`freememo_summaries_affected` in `freememo.db`)."
  [ds f]
  (jdbc/with-transaction [tx ds]
    (jdbc/execute! tx ["SELECT set_config('freememo.sync_apply', 'on', true)"])
    (let [result (f tx)]
      (jdbc/execute! tx ["SELECT freememo_flush_deferred_summaries()"])
      result)))

;; ── Ownership (2.10) ───────────────────────────────────────────────

(defn- owner-of [tx table key-map]
  (:owner (jdbc/execute-one! tx ["SELECT freememo_sync_owner(?, ?::jsonb) AS owner"
                                 (name table) (->json key-map)]
            {:builder-fn rs/as-unqualified-lower-maps})))

(defn- foreign-parent
  "The first FK column of `r` that names a row another account owns, or nil.
   A parent that does not exist yet is not foreign: the FK decides that one."
  [tx user-id table r]
  (some (fn [[col space]]
          (let [v (get r (name col))]
            (when (and (some? v) (contains? synced-by-name (name space)))
              (let [owner (owner-of tx space {"id" v})]
                (when (and owner (not= (long owner) (long user-id))) (name col))))))
    (import/fk-columns table)))

(defn- id-in-blocks? [tx user-id id]
  (some? (jdbc/execute-one! tx ["SELECT 1 FROM id_blocks
                                 WHERE user_id = ? AND ? >= block_start AND ? < block_end"
                                user-id id id])))

(defn- grave-of [tx table key-map]
  (jdbc/execute-one! tx ["SELECT usn, user_id FROM sync_graves
                          WHERE table_name = ? AND row_key = ?::jsonb"
                         (name table) (->json key-map)]
    {:builder-fn rs/as-unqualified-lower-maps}))

(defn- server-refusal
  "Why the server must not apply this row, or nil (2.4.2, 2.10).
   Pre: `existing` is the row this key names on the server, or nil."
  [tx {:keys [user-id base-usn]} meta key-map r existing]
  (let [grave (grave-of tx (:table meta) key-map)
        id (get key-map "id")
        row-user (get r "user_id")]
    (cond
      (and grave (>= (:usn grave) base-usn)) "deleted-elsewhere"
      (and existing (not= (long (:owner existing -1)) (long user-id))) "not-yours"
      (and row-user (not= (long row-user) (long user-id))) "not-yours"
      (and (nil? existing) (= ["id"] (:key meta)) (number? id)
           (if (>= id schema/client-id-floor)
             (not (id-in-blocks? tx user-id id))
             (not (and grave (= (:user_id grave) user-id)))))
      "unknown-id"
      :else (when-let [col (foreign-parent tx user-id (:table meta) r)]
              (str "not-yours:" col)))))

;; ── Reference rewriting (2.9.1) ────────────────────────────────────

(defn- rewrite-payload
  "`r` with every reference to a merged id rewritten through `remaps`,
   `{space {from to}}`. Covers FK columns, media URLs in HTML, and the media
   ids inside score geometry, the places account import rewrites."
  [r table remaps]
  (if (empty? remaps)
    r
    (let [media-map (get remaps :media {})
          r (reduce (fn [r [col space]]
                      (let [c (name col) v (get r c)]
                        (if-let [to (and (some? v) (get-in remaps [space v]))]
                          (assoc r c to)
                          r)))
              r (import/fk-columns table))]
      (if (empty? media-map)
        r
        (as-> r r
          (reduce (fn [r col] (update r (name col) import/rewrite-media-urls media-map))
            r (filter #(contains? r (name %)) (import/html-columns table)))
          (reduce (fn [r col]
                    (update r (name col)
                      #(walk/postwalk (fn [x] (if (string? x) (import/rewrite-media-urls x media-map) x)) %)))
            r (filter #(contains? r (name %)) (import/jsonb-html-columns table)))
          (if (and (#{:score_groups :card_versions} table) (map? (get r "geometry")))
            (update r "geometry"
              #(walk/stringify-keys (import/remap-score-geometry (walk/keywordize-keys %) media-map)))
            r))))))

(defn rewrite-local-references!
  "Desktop: point every local reference to `from` at `to`, in `space` (a
   table keyword), then delete the `from` row without a grave or a conflict.
   Pre:  `tx` is an apply transaction, so the rewrites keep each row's usn.
   Post: no FK column, media URL or score geometry names `from`."
  [tx space from to]
  (doseq [[table cols] import/fk-columns
          [col s] cols
          :when (= s space)
          :when (contains? synced-by-name (name table))]
    (jdbc/execute! tx [(format "UPDATE %s SET %s = ? WHERE %s = ?" (name table) (name col) (name col))
                       to from]))
  (when (= space :media)
    (let [pattern (str "/api/media/" from "(?![0-9])")
          replacement (str "/api/media/" to)]
      (doseq [[table cols] import/html-columns col cols]
        (jdbc/execute! tx [(format "UPDATE %s SET %s = regexp_replace(%s, ?, ?, 'g') WHERE %s ~ ?"
                             (name table) (name col) (name col) (name col))
                           pattern replacement pattern]))
      (doseq [[table cols] (assoc import/jsonb-html-columns :score_groups [:geometry]
                             :card_versions (conj (vec (import/jsonb-html-columns :card_versions)) :geometry))
              col cols
              :let [geometry? (= :geometry col)
                    pat (if geometry? (str "\"media-id\": " from "(?![0-9])") pattern)
                    rep (if geometry? (str "\"media-id\": " to) replacement)]]
        (jdbc/execute! tx [(format "UPDATE %s SET %s = regexp_replace(%s::text, ?, ?, 'g')::jsonb
                                    WHERE %s::text ~ ?" (name table) (name col) (name col) (name col))
                           pat rep pat]))))
  (jdbc/execute! tx [(format "UPDATE %s SET usn = 0 WHERE id = ?" (name space)) from])
  (jdbc/execute! tx [(format "DELETE FROM %s WHERE id = ?" (name space)) from]))

;; ── Upsert ─────────────────────────────────────────────────────────

(defn- incoming-wins-sql
  "When an incoming column value replaces the stored one (D4, class 2).
   Server: the client had seen the stored row (t.usn < base), or the incoming
   write is newer. Desktop: the stored row is not a pending local edit, or the
   incoming write is newer. Judging \"seen\" by usn first keeps a sequential
   edit from losing to a device clock that runs behind."
  [peer base-usn]
  (case peer
    :server (format "(t.usn < %d OR EXCLUDED.mtime > t.mtime)" (long base-usn))
    :desktop "(t.usn <> -1 OR EXCLUDED.mtime > t.mtime)"))

(defn- set-clause [meta peer base-usn col]
  (let [{:keys [schedule watermark]} meta
        clock (:clock schedule)]
    (cond
      ;; Desktop: a pending local row stays pending, so whatever survived of
      ;; it uploads next sync. Re-uploading a merged row is safe: the server
      ;; merges it again.
      (= col "usn") (if (= peer :desktop)
                      "usn = CASE WHEN t.usn = -1 THEN -1 ELSE EXCLUDED.usn END"
                      "usn = t.usn")
      (= col "mtime") "mtime = GREATEST(t.mtime, EXCLUDED.mtime)"
      (watermark col) (format "%s = GREATEST(t.%s, EXCLUDED.%s)" col col col)
      (and schedule ((:columns schedule) col))
      (format "%s = CASE WHEN EXCLUDED.%s IS NOT NULL AND (t.%s IS NULL OR EXCLUDED.%s > t.%s)
                         THEN EXCLUDED.%s ELSE t.%s END"
        col clock clock clock clock col col)
      :else (format "%s = CASE WHEN %s THEN EXCLUDED.%s ELSE t.%s END"
              col (incoming-wins-sql peer base-usn) col col))))

(defn- upsert-sql
  "INSERT … ON CONFLICT for one payload. Only the payload's columns are named
   (2.4.3), so a column this build does not send keeps the receiver's value."
  [meta cols blob-cols peer base-usn]
  (let [{:keys [name key blob class]} meta
        insert-cols (concat cols blob-cols)
        exprs (concat (map #(str "r." %) cols)
                (map #(if (= :large-object (blob %)) "?::oid" "?::bytea") blob-cols))
        update-cols (remove (set key) insert-cols)
        lo-col (some (fn [[c kind]] (when (= kind :large-object) c)) blob)]
    (str "INSERT INTO " name " AS t (" (str/join ", " insert-cols) ") "
      "SELECT " (str/join ", " exprs) " FROM jsonb_populate_record(null::" name ", ?::jsonb) r "
      "ON CONFLICT (" (str/join ", " key) ") "
      (if (or (= 1 class) (empty? update-cols))
        "DO NOTHING"
        (str "DO UPDATE SET " (str/join ", " (map #(set-clause meta peer base-usn %) update-cols))))
      " RETURNING " (if lo-col (str "t." lo-col " AS lo_oid") "1 AS ok"))))

(defn- existing-row
  "The stored row for `key-map`, locked FOR UPDATE, with what the merge needs:
   its owner (server), whether it changed concurrently with this payload, and
   whether the payload is newer. nil when no row holds the key."
  [tx ctx meta key-map r]
  (let [{:keys [peer]} ctx
        {:keys [name sent-cols blob]} meta
        lo-col (some (fn [[c kind]] (when (= kind :large-object) c)) blob)
        size-col (get {:topic_files "file_size" :topic_videos "byte_size" :topic_epubs "byte_size"}
                   (:table meta))
        server? (= peer :server)
        sql (format "SELECT %s AS owner, %s AS concurrent,
                            (?::timestamptz > t.mtime) AS incoming_newer,
                            %s::text AS r%s%s
                     FROM %s t WHERE %s FOR UPDATE"
              (if server? (format "freememo_sync_owner('%s', ?::jsonb)" name) "NULL::bigint")
              (if server? (format "(t.usn >= %d)" (long (:base-usn ctx))) "(t.usn = -1)")
              (row-json sent-cols)
              (if lo-col (str ", t." lo-col " AS old_oid") "")
              (if size-col (str ", t." size-col " AS old_size") "")
              name (key-match meta))
        params (concat (when server? [(->json key-map)])
                 [(get r "mtime" "1970-01-01T00:00:00Z")]
                 [(->json key-map)])]
    (some-> (jdbc/execute-one! tx (into [sql] params) {:builder-fn rs/as-unqualified-lower-maps})
      (update :r json/parse-string))))

;; ── History (D4: the loser goes to history) ────────────────────────

(def ^:private history
  {:flashcards {:columns ["question" "answer" "cloze" "overlapping" "io_fields"]
                :table :card_versions
                :sql "INSERT INTO card_versions (root_topic_id, scope_type, scope_id, kind, question,
                                                 answer, cloze, overlapping, io_fields)
                      SELECT r.root_topic_id, 'card', r.id, r.kind, r.question, r.answer,
                             r.cloze, r.overlapping, r.io_fields
                      FROM jsonb_populate_record(null::flashcards, ?::jsonb) r RETURNING id"}
   :topics {:columns ["title" "content"]
            :table :topic_versions
            :sql "INSERT INTO topic_versions (topic_id, title, content)
                  SELECT r.id, r.title, r.content
                  FROM jsonb_populate_record(null::topics, ?::jsonb) r RETURNING id"}})

(defn- save-loser!
  "When two peers edited the same row's content concurrently, write the losing
   rendition to its history table. Returns the history row's key, or nil.
   Pre: `existing` was concurrent (its :concurrent flag is true)."
  [tx table existing incoming]
  (when-let [{:keys [columns sql] hist :table} (history table)]
    (let [stored (:r existing)
          merged (merge stored incoming)
          content #(select-keys % columns)]
      (when (not= (content stored) (content merged))
        (let [loser (if (:incoming_newer existing) stored merged)
              id (:id (jdbc/execute-one! tx [sql (->json loser)] {:builder-fn rs/as-unqualified-lower-maps}))]
          [hist {"id" id}])))))

;; ── Blobs (2.7) ────────────────────────────────────────────────────

(defn- blob-values
  "For each blob column the row carries, the SQL parameter: bytes for bytea,
   a new large object's OID for an OID column. Returns [cols values new-oids],
   or {:missing col} when a named blob has no file."
  [^Connection tx ctx meta row]
  (reduce (fn [[cols vals oids] [col _]]
            (let [kind (get-in meta [:blob col])
                  file ((:blob-file ctx) row col)]
              (cond
                (nil? file) (reduced {:missing col})
                (= kind :bytea) [(conj cols col) (conj vals (Files/readAllBytes (.toPath ^java.io.File file))) oids]
                :else (let [oid (lo/create! tx)]
                        (lo/write-file! tx oid file)
                        [(conj cols col) (conj vals oid) (conj oids oid)]))))
    [[] [] []]
    (get row "b")))

(def ^:private size-columns
  {:topic_files "file_size" :topic_videos "byte_size" :topic_epubs "byte_size"})

(defn- reserve-storage!
  "Server: charge the account's storage for bytes this row adds (CLAUDE.md,
   usage_bytes counts topic_files, topic_videos and topic_epubs). Throws the
   quota error when the account is full."
  [tx user-id table r existing]
  (when-let [col (size-columns table)]
    (let [new-size (or (get r col) 0)
          delta (- new-size (or (:old_size existing) 0))]
      (cond
        (pos? delta) (quota/reserve-bytes! tx user-id delta)
        (neg? delta) (jdbc/execute! tx ["UPDATE users SET usage_bytes = GREATEST(0, usage_bytes + ?)
                                         WHERE id = ?" delta user-id])))))

;; ── One row ────────────────────────────────────────────────────────

(defn- reject [state row reason]
  (update state :rejected conj {"t" (get row "t") "k" (get row "k") "r" (get row "r") "reason" reason}))

(def ^:private natural-keys
  "Class 4 (1.6): the columns a second row may not repeat."
  {:media ["user_id" "sha256"] :kg_predicates ["user_id" "slug"]})

(defn- natural-key-holder [tx table r]
  (let [cols (natural-keys table)]
    (:id (jdbc/execute-one! tx
           (into [(format "SELECT id FROM %s WHERE %s" (name table)
                    (str/join " AND " (map #(str % " = ?") cols)))]
             (map #(get r %) cols))
           {:builder-fn rs/as-unqualified-lower-maps}))))

(defn- remint-mask-ordinal!
  "Class 5: give a flashcard a fresh ordinal from its group's watermark."
  [tx group-id]
  (:o (jdbc/execute-one! tx ["UPDATE occlusion_groups SET next_ordinal = next_ordinal + 1
                              WHERE id = ? RETURNING next_ordinal - 1 AS o" group-id]
        {:builder-fn rs/as-unqualified-lower-maps})))

(declare apply-row! finish!)

(defn- on-unique-violation
  "A unique constraint other than the key refused the row."
  [tx ctx state row r ^PSQLException e]
  (let [table (synced-table (get row "t"))
        constraint (some-> e .getServerErrorMessage .getConstraint)
        incoming-id (get r "id")]
    (cond
      ;; Class 4: the same natural key under a second id.
      (natural-keys table)
      (let [holder (natural-key-holder tx table r)]
        (cond
          (or (nil? holder) (= holder incoming-id)) (reject state row (str "duplicate:" constraint))
          (= :server (:peer ctx))
          (-> state (assoc-in [:remaps table incoming-id] holder) (update :applied inc))
          :else
          ;; Desktop: the server's row wins, and the local duplicate merges into it.
          (do (rewrite-local-references! tx table holder incoming-id)
              (apply-row! tx ctx (update state :touched conj table) row))))

      ;; Class 5: two peers drew the same mask ordinal.
      (and (= "uq_flashcards_mask_ordinal" constraint) (get r "occlusion_group_id"))
      (if (= :server (:peer ctx))
        (let [o (remint-mask-ordinal! tx (get r "occlusion_group_id"))]
          (apply-row! tx ctx
            (update state :corrections conj [:occlusion_groups {"id" (get r "occlusion_group_id")}])
            (-> row (assoc-in ["r" "mask_ordinal"] o) (assoc ::reminted true))))
        ;; Desktop: the server's card keeps its ordinal. The local card moves.
        (let [local (:id (jdbc/execute-one! tx ["SELECT id FROM flashcards
                                                 WHERE occlusion_group_id = ? AND mask_ordinal = ?"
                                                (get r "occlusion_group_id") (get r "mask_ordinal")]
                           {:builder-fn rs/as-unqualified-lower-maps}))
              o (remint-mask-ordinal! tx (get r "occlusion_group_id"))]
          (jdbc/execute! tx ["UPDATE flashcards SET mask_ordinal = ? WHERE id = ?" o local])
          (apply-row! tx ctx state row)))

      :else (reject state row (str "duplicate:" constraint)))))

(defn- apply-row!
  "Apply one wire row. Returns the next state, or ::retry when a parent row is
   missing and a later pass may supply it (2.4)."
  [^Connection tx {:keys [peer user-id base-usn] :as ctx} state row]
  (if-let [table (synced-table (get row "t"))]
    (let [meta (table-meta tx table)
          r (-> (get row "r") (rewrite-payload table (:remaps state)) (select-keys (:sent meta)))
          key-map (select-keys r (:key meta))]
      (if (not= (count key-map) (count (:key meta)))
        (reject state row "invalid:key")
        (let [existing (existing-row tx ctx meta key-map r)
              refusal (when (= peer :server) (server-refusal tx ctx meta key-map r existing))]
          (if refusal
            (reject state row refusal)
            (let [sp (.setSavepoint tx)]
              (try
                (let [blobs (blob-values tx ctx meta row)]
                  (if (:missing blobs)
                    (do (.rollback tx sp) (reject state row (str "invalid:blob-missing:" (:missing blobs))))
                    (let [[blob-cols blob-vals new-oids] blobs
                          cols (vec (remove (set blob-cols) (keys r)))
                          _ (when (= peer :server) (reserve-storage! tx user-id table r existing))
                          result (jdbc/execute-one! tx
                                   (-> [(upsert-sql meta cols blob-cols peer base-usn)]
                                     (into blob-vals) (conj (->json r)))
                                   {:builder-fn rs/as-unqualified-lower-maps})
                          final-oid (:lo_oid result)]
                      ;; A large object that lost its slot is garbage now.
                      (when (and (:old_oid existing) (not= (:old_oid existing) final-oid))
                        (lo/unlink! tx (:old_oid existing)))
                      (doseq [oid new-oids :when (not= oid final-oid)] (lo/unlink! tx oid))
                      (.releaseSavepoint tx sp)
                      (let [loser (when (:concurrent existing) (save-loser! tx table existing r))]
                        (cond-> (-> state (update :applied inc) (update :touched conj table))
                          (= table :flashcards) (update :restamp conj (get r "id"))
                          loser (update :corrections conj loser)
                          (or (:concurrent existing) (::reminted row) (seq (:remaps state)))
                          (update :corrections conj [table key-map]))))))
                (catch PSQLException e
                  (.rollback tx sp)
                  (case (.getSQLState e)
                    "23503" ::retry
                    "23505" (on-unique-violation tx ctx state row r e)
                    (reject state row (str "invalid:" (.getSQLState e) ":" (.getMessage e)))))
                (catch clojure.lang.ExceptionInfo e
                  (.rollback tx sp)
                  (if (quota/quota-error? (ex-data e))
                    (reject state row "storage-quota")
                    (throw e)))))))))
    (reject state row "invalid:table")))

;; ── Batches ────────────────────────────────────────────────────────

(def empty-result
  {:applied 0 :rejected [] :corrections #{} :remaps {} :touched #{} :restamp #{}})

(defn apply-rows!
  "Apply wire `rows` in FK order (2.4), with retry passes for parents that
   arrive later in the same batch, such as a forked chat's message.
   Pre:  `tx` is from `with-apply-tx`.
   Post: every row is applied, merged, or in `:rejected` with a reason. A row
         whose parent never arrived is rejected as `parent-deleted` (2.4.4).
         `:corrections` lists the keys whose stored state differs from what
         the sender holds; `:remaps` lists class 4 merges, `{table {from to}}`."
  [tx ctx rows]
  (loop [pending (sort-by #(table-rank (synced-table (get % "t")) -1) rows)
         state empty-result]
    (let [{:keys [state retry]}
          (reduce (fn [{:keys [state retry]} row]
                    (let [next-state (apply-row! tx ctx state row)]
                      (if (= ::retry next-state)
                        {:state state :retry (conj retry row)}
                        {:state next-state :retry retry})))
            {:state state :retry []}
            pending)]
      (cond
        (empty? retry) (finish! tx state)
        (< (count retry) (count pending)) (recur retry state)
        :else (finish! tx (reduce #(reject %1 %2 "parent-deleted") state retry))))))

(defn- finish!
  "Recompute class 6 columns for what the batch wrote (2.5)."
  [tx state]
  (when (seq (:restamp state))
    (db/restamp-card-digests! tx (vec (:restamp state))))
  state)

(defn apply-graves!
  "Delete the rows `graves` name (1.4). The server deletes only rows the
   account owns and returns their storage. Graves for absent rows are no-ops.
   Pre:  `tx` is from `with-apply-tx`.
   Post: returns {:deleted n :touched #{table}}."
  [tx {:keys [peer user-id]} graves]
  (reduce
    (fn [acc grave]
      (let [table (synced-table (get grave "t"))
            meta (some->> table (table-meta tx))
            key-map (get grave "k")]
        (if (or (nil? meta) (not= (set (keys key-map)) (set (:key meta))))
          acc
          (let [owned? (or (= peer :desktop)
                         (= (some-> (owner-of tx table key-map) long) (long user-id)))
                present (jdbc/execute-one! tx [(format "SELECT %s FROM %s t WHERE %s"
                                                 (case table
                                                   :topics "t.staged_delete_id IS NULL AS live"
                                                   "true AS live")
                                                 (:name meta) (key-match meta))
                                                (->json key-map)]
                          {:builder-fn rs/as-unqualified-lower-maps})]
            (if-not (and owned? present)
              acc
              (do
                (when (= peer :server)
                  (let [bytes (case table
                                :topics (when (:live present) (db/subtree-file-bytes tx (get key-map "id")))
                                (:topic_files :topic_videos :topic_epubs)
                                (:s (jdbc/execute-one! tx [(format "SELECT %s AS s FROM %s t WHERE %s"
                                                             (size-columns table) (:name meta) (key-match meta))
                                                           (->json key-map)]
                                      {:builder-fn rs/as-unqualified-lower-maps}))
                                nil)]
                    (when (and bytes (pos? bytes))
                      (jdbc/execute! tx ["UPDATE users SET usage_bytes = GREATEST(0, usage_bytes - ?)
                                          WHERE id = ?" bytes user-id]))))
                (jdbc/execute! tx [(format "DELETE FROM %s t WHERE %s" (:name meta) (key-match meta))
                                   (->json key-map)])
                (-> acc (update :deleted inc) (update :touched conj table))))))))
    {:deleted 0 :touched #{}}
    graves))

;; ── Invalidation (2.5) ─────────────────────────────────────────────

(defn channels-for
  "The reactivity channels a write to `tables` invalidates, for
   `commands/bump-channels!`."
  [tables]
  (into #{}
    (mapcat (fn [table]
              (let [t (name table)]
                (cond
                  (= t "topics") [:refresh :tree-mutations :meta-refresh :queue-mutations]
                  (#{"flashcards" "card_schedules" "card_versions" "occlusion_groups" "score_groups"} t)
                  [:card-mutations :refresh :schedule-mutations]
                  (= t "settings") [:settings-refresh]
                  (= t "topic_pins") [:pin-mutations]
                  (= t "topic_repetitions") [:queue-mutations]
                  (= t "study_schedules") [:schedule-mutations]
                  (str/starts-with? t "assistant_") [:assistant-mutations]
                  (str/starts-with? t "curricul") [:curriculum-mutations]
                  (str/starts-with? t "question_") [:question-mutations]
                  (str/starts-with? t "walkthrough") [:walkthrough-mutations]
                  (or (str/starts-with? t "kg_") (#{"known_marks" "flashcard_entities"} t))
                  [:kg-mutations]
                  :else [:refresh]))))
    tables))
