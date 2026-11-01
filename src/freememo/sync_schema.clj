(ns freememo.sync-schema
  "The schema half of desktop row sync (plans/electron-local-db-sync.md §1).

   `install!` runs at the end of `db/setup-schema` on both peers. It:
   1. converts every id and foreign key column to BIGINT (1.1),
   2. caps every server sequence below the client id range (1.2.2),
   3. adds `usn` and `mtime` to every synced table (1.3),
   4. installs the stamp and grave triggers (1.4, 1.5),
   5. on a desktop, makes each synced id column draw from an id block (1.2.1).

   It also owns the per-table facts the apply needs and the catalog cannot
   give: who owns a row, which columns form a schedule, which hold blobs.

   This namespace takes a connectable and never requires `freememo.db`, so
   `db` can call it without a cycle."
  (:require [clojure.string :as str]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]
            [taoensso.telemere :as tel]
            [freememo.account-transfer-format :as fmt]
            [freememo.peer :as peer]))

;; ── Id ranges (1.2) ────────────────────────────────────────────────

(def client-id-floor
  "2^40. Server sequences stay below it. Desktop id blocks start at it."
  1099511627776)

(def block-size
  "Ids in one block (D2)."
  1000000)

(def offline-id-floor
  "2^50. A local account picks its own blocks at or above it (15.3.1), and
   the server cuts its blocks below it (15.3.5)."
  1125899906842624)

(def offline-id-end
  "2^53. The end of the offline range: every id below it is exact as a
   JavaScript number (15.3.4)."
  9007199254740992)

(defn random-offline-block
  "15.3.1 — a block at a random multiple of `block-size` in
   [offline-id-floor, offline-id-end), from a cryptographic random source.
   Post: {:start :end}, with offline-id-floor <= start < end <= offline-id-end."
  []
  (let [slots (quot (- offline-id-end offline-id-floor) block-size)
        n (mod (.nextLong (java.security.SecureRandom.)) slots)
        start (+ offline-id-floor (* n block-size))]
    {:start start :end (+ start block-size)}))

;; ── Per-table facts ────────────────────────────────────────────────

(def key-overrides
  "Synced tables whose row identity is not their primary key.
   question_pin_tombstones has no primary key, only UNIQUE (map_id, label)."
  {:question_pin_tombstones ["map_id" "label"]})

(def owner-paths
  "How the server finds the account that owns a row, for its grave (1.4).
   Each entry is a list of alternatives, and the first non-NULL one wins.
   `\"user_id\"` reads the row's own column. `[fk parent]` asks for the owner
   of the parent row that `fk` names, through its `id`.

   Invariant: every synced table has an entry. `install!` asserts it."
  {:sources ["user_id"]
   :media ["user_id"]
   :topics ["user_id" ["parent_id" :topics]]
   :poem_packages ["user_id" ["topic_id" :topics]]
   :topic_files [["topic_id" :topics]]
   :topic_videos [["topic_id" :topics]]
   :topic_epubs [["topic_id" :topics]]
   :topic_code [["topic_id" :topics]]
   :topic_youtube [["topic_id" :topics]]
   :repo_selections [["topic_id" :topics]]
   :video_transcripts [["topic_id" :topics]]
   :video_segments [["topic_id" :topics]]
   :topic_pins [["topic_id" :topics]]
   :reading_positions [["topic_id" :topics]]
   :neoreader_highlights ["user_id"]
   :occlusion_groups [["image_media_id" :media]]
   :score_groups [["clip_media_id" :media]]
   :flashcards [["root_topic_id" :topics] ["topic_id" :topics]]
   :card_schedules [["flashcard_id" :flashcards]]
   :card_versions [["root_topic_id" :topics]]
   :topic_versions [["topic_id" :topics]]
   :topic_repetitions ["user_id"]
   :study_schedules ["user_id"]
   :curricula ["user_id"]
   :curriculum_blocks [["curriculum_id" :curricula]]
   :curriculum_materials [["block_id" :curriculum_blocks]]
   :curriculum_exam_forms ["user_id"]
   :curriculum_exam_sittings ["user_id"]
   :curriculum_grade_revisions [["sitting_id" :curriculum_exam_sittings]]
   :curriculum_unlock_overrides [["curriculum_id" :curricula]]
   :walkthroughs ["user_id"]
   :walkthrough_concepts [["walkthrough_id" :walkthroughs]]
   :walkthrough_hunks [["walkthrough_id" :walkthroughs]]
   :assistant_chats ["user_id"]
   :assistant_messages [["chat_id" :assistant_chats]]
   :assistant_runs [["chat_id" :assistant_chats]]
   :assistant_checkpoints [["chat_id" :assistant_chats]]
   :assistant_evidence [["run_id" :assistant_runs]]
   :kg_entities ["user_id"]
   :kg_predicates ["user_id"]
   :kg_facts ["user_id"]
   :kg_page_extractions ["user_id"]
   :kg_questions ["user_id"]
   :kg_question_facts [["question_id" :kg_questions]]
   :kg_reviews ["user_id"]
   :question_maps ["user_id"]
   :question_roots [["map_id" :question_maps]]
   :question_pins [["map_id" :question_maps]]
   :question_pin_tombstones [["map_id" :question_maps]]
   :known_marks [["entity_id" :kg_entities]]
   :flashcard_entities [["entity_id" :kg_entities] ["flashcard_id" :flashcards]]
   :settings ["user_id"]})

(def schedule-groups
  "Class 3 columns (1.6). Within a group the later review wins, judged by the
   group's clock column, whatever the row's mtime says. `:columns :all` means
   every synced column of the table."
  {:topics {:clock "last_review_at"
            :columns ["interval_days" "a_factor" "next_review_at" "last_review_at"]}
   :kg_questions {:clock "fsrs_last_review" :columns :fsrs}
   :card_schedules {:clock "last_review" :columns :all}})

(def watermark-columns
  "Class 5 counters (1.6). They take the maximum of both peers."
  {:occlusion_groups ["next_ordinal"]})

(def peer-local-columns
  "Columns that are never sent (class 7 inside a synced table). Each names a
   peer-local row, so the receiver keeps its own value (2.4.3)."
  {:topics ["staged_delete_id"]
   :curricula ["staged_delete_id"]
   :assistant_chats ["staged_delete_id"]})

(def derived-columns
  "Class 6 columns inside a synced table. Never sent; the apply recomputes
   them (2.5). Generated columns are excluded by the catalog instead."
  {:flashcards ["local_current_digest"]})

(def blob-columns
  "Columns whose bytes move as a separate transfer (2.7). `:bytea` is stored
   inline, `:large-object` is an OID local to one database (2.7.2)."
  {:media {"bytes" :bytea}
   :topic_files {"file_data" :bytea}
   :topic_videos {"lo_oid" :large-object}
   :topic_epubs {"lo_oid" :large-object}})

;; ── Catalog reads ──────────────────────────────────────────────────

(defn- rows [conn sql-params]
  (jdbc/execute! conn sql-params {:builder-fn rs/as-unqualified-lower-maps}))

(defn primary-key
  "The row identity of `table` as column names, in index order."
  [conn table]
  (or (key-overrides table)
      (let [ks (mapv :attname
                 (rows conn ["SELECT a.attname
                              FROM pg_index i
                              JOIN pg_attribute a ON a.attrelid = i.indrelid
                                                 AND a.attnum = ANY (i.indkey)
                              WHERE i.indrelid = ?::regclass AND i.indisprimary
                              ORDER BY array_position(i.indkey::int2[], a.attnum)"
                             (name table)]))]
        (when (empty? ks)
          (throw (ex-info (str "synced table " (name table) " has no primary key")
                   {:type ::no-row-identity :table table})))
        ks)))

(defn columns
  "Every column of `table`: `{:name :type :generated?}`, in table order."
  [conn table]
  (mapv (fn [r] {:name (:attname r) :type (:type r) :generated? (:generated r)})
    (rows conn ["SELECT a.attname, format_type(a.atttypid, a.atttypmod) AS type,
                        a.attgenerated <> '' AS generated
                 FROM pg_attribute a
                 WHERE a.attrelid = ?::regclass AND a.attnum > 0 AND NOT a.attisdropped
                 ORDER BY a.attnum"
                (name table)])))

;; ── 1.1 BIGINT ids ─────────────────────────────────────────────────

(def ^:private extra-id-columns
  "int4 columns that hold ids without a key constraint naming them."
  [["assistant_evidence" "document_topic_id"]
   ["assistant_evidence" "page_topic_id"]])

(def ^:private id-array-columns
  "int4[] columns whose elements are ids."
  [["kg_sessions" "scope_topic_ids"]
   ["kg_sessions" "question_ids"]
   ["kg_answers" "missed_fact_ids"]
   ["kg_reviews" "missed_fact_ids"]])

(defn- int4-key-columns
  "Every int4 column in a primary or foreign key, as [table column], plus the
   extras. Already-converted columns drop out, so a rerun converts nothing."
  [conn]
  (let [keyed (rows conn ["SELECT DISTINCT c.relname AS tbl, a.attname AS col
                           FROM pg_constraint k
                           JOIN pg_class c ON c.oid = k.conrelid
                           JOIN pg_namespace n ON n.oid = c.relnamespace
                           JOIN pg_attribute a ON a.attrelid = k.conrelid
                                              AND a.attnum = ANY (k.conkey)
                           WHERE n.nspname = 'public' AND k.contype IN ('p', 'f')
                             AND a.atttypid = 'int4'::regtype"])
        typed (fn [[t c] type]
                (seq (rows conn ["SELECT 1 FROM information_schema.columns
                                  WHERE table_schema = 'public' AND table_name = ?
                                    AND column_name = ? AND udt_name = ?" t c type])))]
    {:scalars (into (set (map (juxt :tbl :col) keyed))
                (filter #(typed % "int4") extra-id-columns))
     :arrays (filterv #(typed % "_int4") id-array-columns)}))

(defn- convert-ids-to-bigint!
  "1.1 — one ALTER TABLE per table, so each table is rewritten once.
   Post: no int4 key column and no int4[] id column remains."
  [conn]
  (let [{:keys [scalars arrays]} (int4-key-columns conn)
        clauses (concat (for [[t c] scalars] [t (format "ALTER COLUMN %s TYPE bigint" c)])
                  (for [[t c] arrays]
                    [t (format "ALTER COLUMN %s TYPE bigint[] USING %s::bigint[]" c c)]))]
    (doseq [[t cs] (group-by first clauses)]
      (let [started (System/nanoTime)]
        (jdbc/execute! conn [(str "ALTER TABLE " t " " (str/join ", " (map second cs)))])
        (tel/log! {:level :info :id ::bigint-converted
                   :data {:table t :columns (count cs)
                          :ms (quot (- (System/nanoTime) started) 1000000)}}
          (str "Converted " t " ids to BIGINT"))))))

;; ── 1.2.2 Sequence caps ────────────────────────────────────────────

(defn- cap-sequences!
  "Every sequence except the block counter stops at 2^40 − 1, so a server id
   can never enter the range desktop blocks are cut from.
   Pre: no sequence has passed the cap. A violation throws, by design."
  [conn]
  (doseq [{:keys [seq last_value max_value data_type]}
          (rows conn ["SELECT sequencename AS seq, last_value, max_value, data_type::text
                       FROM pg_sequences
                       WHERE schemaname = 'public' AND sequencename <> 'id_block_seq'"])
          :when (or (not= "bigint" data_type) (not= (dec client-id-floor) max_value))]
    (when (and last_value (>= last_value client-id-floor))
      (throw (ex-info (str "sequence " seq " is already in the client id range")
               {:type ::sequence-in-client-range :sequence seq :last-value last_value})))
    (jdbc/execute! conn [(format "ALTER SEQUENCE %s AS bigint MAXVALUE %d" seq
                           (dec client-id-floor))])))

;; ── SQL functions ──────────────────────────────────────────────────

(defn- peer-functions-sql
  "Functions both triggers read. The peer is baked into the function body,
   because a database belongs to one peer for its whole life."
  []
  [(format "CREATE OR REPLACE FUNCTION freememo_sync_peer() RETURNS text
            LANGUAGE sql IMMUTABLE AS $$ SELECT '%s'::text $$"
     (name peer/role))
   "CREATE OR REPLACE FUNCTION freememo_sync_applying() RETURNS boolean
    LANGUAGE sql STABLE AS $$
      SELECT coalesce(current_setting('freememo.sync_apply', true), '') = 'on'
    $$"
   ;; Server: the writing transaction's id. Desktop: -1, which means pending.
   "CREATE OR REPLACE FUNCTION freememo_sync_usn() RETURNS bigint
    LANGUAGE sql VOLATILE AS $$
      SELECT CASE freememo_sync_peer()
               WHEN 'server' THEN pg_current_xact_id()::text::bigint
               ELSE -1 END
    $$"
   ;; 1.2.1 — the desktop's id source. 1.2.4 — refuses once every block is spent.
   "CREATE OR REPLACE FUNCTION freememo_next_id() RETURNS bigint
    LANGUAGE plpgsql VOLATILE AS $$
    DECLARE v bigint;
    BEGIN
      FOR attempt IN 1..3 LOOP
        UPDATE local_id_blocks b SET next_id = b.next_id + 1
         WHERE b.block_start = (SELECT block_start FROM local_id_blocks
                                 WHERE next_id < block_end
                                 ORDER BY block_start LIMIT 1)
           AND b.next_id < b.block_end
        RETURNING b.next_id - 1 INTO v;
        IF v IS NOT NULL THEN RETURN v; END IF;
        EXIT WHEN NOT EXISTS (SELECT 1 FROM local_id_blocks WHERE next_id < block_end);
      END LOOP;
      RAISE EXCEPTION 'FreeMemo cannot create anything new on this device until it syncs. Connect to the internet and choose Sync now.'
        USING ERRCODE = 'P0001', HINT = 'freememo-id-blocks-exhausted';
    END $$"
   ;; Reads the key columns named in TG_ARGV off a row, as a JSON object.
   "CREATE OR REPLACE FUNCTION freememo_sync_row_key(p_row anyelement, p_cols text[])
    RETURNS jsonb LANGUAGE plpgsql STABLE AS $$
    DECLARE k jsonb := '{}'::jsonb; v jsonb; c text;
    BEGIN
      FOREACH c IN ARRAY p_cols LOOP
        EXECUTE format('SELECT to_jsonb(($1).%I)', c) USING p_row INTO v;
        k := k || jsonb_build_object(c, v);
      END LOOP;
      RETURN k;
    END $$"])

(defn- owner-alternative-sql [alt]
  (if (string? alt)
    (format "%s::bigint" alt)
    (let [[fk parent] alt]
      (format "freememo_sync_owner('%s', jsonb_build_object('id', %s))" (name parent) fk))))

(defn- owner-function-sql
  "freememo_sync_owner(table, key): the account that owns a row. A live row is
   resolved through `owner-paths`. A row deleted earlier in this transaction is
   resolved through its grave, which its own BEFORE DELETE trigger wrote. That
   is what lets a cascaded child find the owner of a parent already gone."
  [conn]
  (let [arms (for [table fmt/synced-tables
                   :let [ks (primary-key conn table)
                         types (into {} (map (juxt :name :type)) (columns conn table))
                         alts (or (owner-paths table)
                                (throw (ex-info (str "no owner path for " (name table))
                                         {:type ::missing-owner-path :table table})))
                         where (str/join " AND "
                                 (for [k ks] (format "%s = (p_key->>'%s')::%s" k k (types k))))]]
               (format "WHEN '%s' THEN SELECT coalesce(%s), true INTO v, hit FROM %s WHERE %s;"
                 (name table) (str/join ", " (map owner-alternative-sql alts))
                 (name table) where))]
    (str "CREATE OR REPLACE FUNCTION freememo_sync_owner(p_table text, p_key jsonb)
          RETURNS bigint LANGUAGE plpgsql STABLE AS $$
          DECLARE v bigint; hit boolean;
          BEGIN
            CASE p_table
            " (str/join "\n" arms) "
            ELSE RAISE EXCEPTION 'freememo_sync_owner: % is not synced', p_table;
            END CASE;
            IF hit THEN RETURN v; END IF;
            SELECT g.user_id INTO v FROM sync_graves g
             WHERE g.table_name = p_table AND g.row_key = p_key;
            RETURN v;
          END $$")))

(def ^:private trigger-functions-sql
  [;; 1.5 — BEFORE INSERT OR UPDATE. 1.5.1: the server always stamps usn, and
   ;; keeps a payload's mtime only while applying. The desktop keeps both
   ;; while applying, because they are the server's values.
   ;; 1.4.1 — an insert takes back the grave of its key.
   "CREATE OR REPLACE FUNCTION freememo_sync_stamp() RETURNS trigger
    LANGUAGE plpgsql AS $$
    BEGIN
      IF TG_OP = 'INSERT' THEN
        DELETE FROM sync_graves
         WHERE table_name = TG_TABLE_NAME
           AND row_key = freememo_sync_row_key(NEW, TG_ARGV);
      END IF;
      IF freememo_sync_peer() = 'desktop' THEN
        IF freememo_sync_applying() THEN RETURN NEW; END IF;
        NEW.usn := -1;
        NEW.mtime := clock_timestamp();
      ELSE
        NEW.usn := pg_current_xact_id()::text::bigint;
        IF NOT freememo_sync_applying() THEN NEW.mtime := clock_timestamp(); END IF;
      END IF;
      RETURN NEW;
    END $$"
   ;; 1.4, 1.5.3 — BEFORE DELETE, so a cascade parent writes its grave before
   ;; its children are deleted, and a child can resolve its owner through it.
   ;; A desktop apply writes no grave: the delete came from the server. A
   ;; pending local row it deletes goes to the conflict report instead (D8).
   ;; An applied delete also frees a large object, which the app's own delete
   ;; paths do by hand (CLAUDE.md, \"Large objects need an explicit unlink\").
   "CREATE OR REPLACE FUNCTION freememo_sync_grave() RETURNS trigger
    LANGUAGE plpgsql AS $$
    DECLARE k jsonb := freememo_sync_row_key(OLD, TG_ARGV);
            pending boolean := freememo_sync_peer() = 'desktop' AND OLD.usn = -1;
    BEGIN
      IF freememo_sync_applying() THEN
        IF pending THEN
          INSERT INTO sync_conflicts (table_name, row_key, row_data, reason)
          VALUES (TG_TABLE_NAME, k, to_jsonb(OLD), 'deleted-elsewhere');
        ELSIF TG_TABLE_NAME IN ('topic_videos', 'topic_epubs') THEN
          -- Nested, because OLD.lo_oid fails to resolve on any other table.
          IF EXISTS (SELECT 1 FROM pg_largeobject_metadata WHERE oid = OLD.lo_oid) THEN
            PERFORM lo_unlink(OLD.lo_oid);
          END IF;
        END IF;
        IF freememo_sync_peer() = 'desktop' THEN RETURN OLD; END IF;
      END IF;
      INSERT INTO sync_graves (table_name, row_key, user_id, usn)
      VALUES (TG_TABLE_NAME, k,
              CASE freememo_sync_peer() WHEN 'server'
                   THEN freememo_sync_owner(TG_TABLE_NAME, k) END,
              freememo_sync_usn())
      ON CONFLICT (table_name, row_key) DO UPDATE
        SET user_id = EXCLUDED.user_id, usn = EXCLUDED.usn, deleted_at = now();
      RETURN OLD;
    END $$"])

;; ── Per-table installation ─────────────────────────────────────────

(defn- sql-string-list [xs]
  (str/join ", " (map #(str "'" % "'") xs)))

(defn- install-table!
  "1.3 columns and index, and 1.5 triggers, on one synced table."
  [conn table]
  (let [t (name table)
        ks (primary-key conn table)]
    (jdbc/execute! conn [(format "ALTER TABLE %s
                                    ADD COLUMN IF NOT EXISTS usn BIGINT NOT NULL DEFAULT 0,
                                    ADD COLUMN IF NOT EXISTS mtime TIMESTAMPTZ NOT NULL
                                      DEFAULT '1970-01-01T00:00:00Z'" t)])
    (jdbc/execute! conn [(format "CREATE INDEX IF NOT EXISTS idx_%s_usn ON %s(usn)" t t)])
    (jdbc/execute! conn [(format "CREATE OR REPLACE TRIGGER freememo_sync_stamp
                                    BEFORE INSERT OR UPDATE ON %s FOR EACH ROW
                                    EXECUTE FUNCTION freememo_sync_stamp(%s)"
                           t (sql-string-list ks))])
    (jdbc/execute! conn [(format "CREATE OR REPLACE TRIGGER freememo_sync_grave
                                    BEFORE DELETE ON %s FOR EACH ROW
                                    EXECUTE FUNCTION freememo_sync_grave(%s)"
                           t (sql-string-list ks))])))

(defn- point-ids-at-blocks!
  "1.2.1 — on a desktop, a synced table's serial id column draws from
   freememo_next_id(). Peer-local tables keep their own sequences, because
   their ids never leave this database."
  [conn]
  (doseq [table fmt/synced-tables
          :let [t (name table)
                default (:column_default
                         (first (rows conn ["SELECT column_default FROM information_schema.columns
                                             WHERE table_schema = 'public'
                                               AND table_name = ? AND column_name = 'id'" t])))]
          :when (and default (str/starts-with? default "nextval("))]
    (jdbc/execute! conn [(format "ALTER TABLE %s ALTER COLUMN id SET DEFAULT freememo_next_id()" t)])))

(defn install!
  "Everything in §1 that the schema owns. Idempotent: every step either checks
   the catalog first or uses CREATE OR REPLACE / IF NOT EXISTS.
   Pre:  every table in `fmt/synced-tables` exists.
   Post: the triggers stamp every synced write and grave every synced delete."
  [conn]
  (convert-ids-to-bigint! conn)
  (cap-sequences! conn)
  (doseq [sql (peer-functions-sql)] (jdbc/execute! conn [sql]))
  (jdbc/execute! conn [(owner-function-sql conn)])
  (doseq [sql trigger-functions-sql] (jdbc/execute! conn [sql]))
  (doseq [table fmt/synced-tables] (install-table! conn table))
  (when (peer/desktop?) (point-ids-at-blocks! conn))
  (tel/log! {:level :info :id ::installed
             :data {:peer peer/role :tables (count fmt/synced-tables)}}
    "Row sync schema installed"))
