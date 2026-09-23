(ns freememo.account-transfer-format
  "The on-disk shape of an account transfer archive, shared by export and import.

   plans/user-data-export-import.md §5.1, §5.2. A user exports a whole account
   from one FreeMemo instance and imports it into another. This namespace owns
   the vocabulary both halves must agree on: the format version, the manifest
   keys, the entity insert order, and the path of every file in the archive.
   It performs no I/O and touches no database, so both peers and the tests read
   the same constants.

   It also owns `entity-scopes`, which only the export half reads. That is a
   deliberate departure from \"both halves\": the scope map is a set of SQL
   strings keyed by `entity-tables`, and the two are one fact split in two. They
   sit together, and `every-exported-table-has-a-scope` reads both from here.

   Named after `freememo.supermemo-format`, which plays the same role for a
   SuperMemo collection.

   **Blame split.** A function that receives archive content returns nil or an
   `{:ok false}` map, because a malformed archive is routine input rather than a
   caller bug. A function that receives values the export path chose throws,
   because a bad table name or an unroundtrippable role is a caller bug."
  (:require [clojure.string :as str]))

;; ── Version ────────────────────────────────────────────────────────

(def format-version
  "Version this build writes into every manifest. Bump it when a change would
   make an older importer read the archive wrongly, not when a change is
   additive and an older importer can ignore it."
  1)

(def supported-versions
  "Versions this build will import. §5.1.3 refuses anything else outright,
   rather than importing the part it recognises."
  #{1})

;; ── Manifest ───────────────────────────────────────────────────────

(def manifest-path
  "§5.1.1 — the archive's first entry, so an importer can refuse a wrong
   version before reading anything else."
  "manifest.edn")

(def required-manifest-keys
  "§5.1.2. `:export-id` scopes every source id in the archive (§5.1.6), so two
   exports of one account never collide in the import ledger. `:tiers` records
   which optional content this file carries, because the answer is not
   recoverable from the entity counts alone."
  #{:format-version :export-id :exported-at :source-instance :counts :tiers})

(defn validate-manifest
  "Whether `manifest` may be imported.

   Pre : `manifest` is whatever the EDN reader produced from `manifest-path`.
         Any value is acceptable input, including nil.
   Post: {:ok true}, or {:ok false :error S} where S names the specific defect.
         Never throws — a malformed manifest is archive content, not a caller
         bug, and §5.1.2 requires the message to name the missing key."
  [manifest]
  (cond
    (not (map? manifest))
    {:ok false :error "The manifest is not a map. This is not a FreeMemo export."}

    :else
    (let [missing (sort (remove #(contains? manifest %) required-manifest-keys))
          version (:format-version manifest)]
      (cond
        (seq missing)
        {:ok false :error (str "The manifest is missing "
                            (str/join ", " (map #(str \: (name %)) missing))
                            ".")}

        (not (contains? supported-versions version))
        {:ok false :error (str "Unsupported export format version " (pr-str version)
                            ". This build reads "
                            (str/join ", " (sort supported-versions)) ".")}

        :else {:ok true}))))

;; ── Entity tables ──────────────────────────────────────────────────

(def entity-tables
  "§5.2.1 — the 40 exported tables, in the order an import must insert them.

   Invariant: the order is forced by foreign keys. A table may not appear
   before every table it references. Three constraints are easy to lose:
   `flashcards` follows `topics` and both group tables; `kg_reviews` follows
   `flashcards` AND `kg_questions`, because its CHECK requires exactly one of
   them (db.clj:1006); `settings` is last, because its keys embed topic ids and
   the rewrite needs the finished topic map (§5.4.8)."
  [:sources
   :media
   :topics
   :poem_packages
   :topic_files
   :topic_videos
   :topic_epubs
   :topic_code
   :repo_selections
   :video_transcripts
   :video_segments
   :topic_pins
   :neoreader_highlights
   :occlusion_groups
   :score_groups
   :flashcards
   :card_schedules
   :card_versions
   :topic_repetitions
   :study_schedules
   :curricula
   :curriculum_blocks
   :curriculum_materials
   :curriculum_exam_forms
   :curriculum_exam_sittings
   :curriculum_grade_revisions
   :curriculum_unlock_overrides
   :assistant_chats
   :assistant_messages
   :assistant_runs
   :assistant_checkpoints
   :assistant_evidence
   :kg_entities
   :kg_predicates
   :kg_facts
   :kg_page_extractions
   :kg_questions
   :kg_question_facts
   :kg_reviews
   :settings])

(def excluded-tables
  "§5.2.2 — the tables an export deliberately omits, each with its reason.

   This set exists to be asserted against the schema. A table added to
   `db.clj` with no entry in either list is data silently dropped from every
   export, and `account_transfer_format_test` is what catches that."
  {:users "account identity — the target instance owns its own user row"
   :app_migrations "deployment state: the one-time data migrations this database
                    has run. It is global, not per user, and the target keeps its
                    own list"
   :user_events "analytics of the source deployment"
   :credit_orders "billing of the source deployment"
   :credit_transactions "billing of the source deployment"
   :undo_log "a 12-hour operational window, capped at 100 rows per user"
   :upload_sessions "in-flight transport state"
   :kg_staged_triples "in-flight extraction state — pass-1 triples a distill run
                       has not linked into kg_facts yet. Safe to drop ONLY because
                       kg_page_extractions.fact_count is NULL until linking
                       finishes: kg-extract/classify-page reads a digest-matching
                       row with no fact count and no staged rows as needing
                       extraction, so the pages arrive eligible again rather than
                       marked done with no facts"
   :kg_graph_layout "a render cache that a version compare invalidates"
   :document_summaries "a maintained read model over topics and flashcards, not
                        content. Statement triggers refresh it on every topic and
                        flashcard write, so an import rebuilds it row by row as
                        the rows it summarizes arrive, and `setup-schema` rebuilds
                        every root at boot. Carrying it would import counts the
                        target immediately recomputes"
   :anki_card_states "a cache of FSRS state replayed out of Anki, and Anki is
                      the source of truth for it (study-scheduler-spec §3.5).
                      Its keys are one collection's Anki card ids, which the
                      target instance re-derives on the next Schedule refresh"
   :anki_deck_configs "the deck config the cache above was derived under —
                       re-read from Anki on the next Schedule refresh"
   :kg_sessions "quiz session transcript — kg_reviews holds the durable record"
   :kg_answers "quiz session transcript — kg_reviews holds the durable record"
   :account_imports "import bookkeeping — target-instance state (§5.5.7)"
   :account_import_ids "import bookkeeping — target-instance state (§5.5.7)"
   :anki_note_tombstones "note ids of ONE Anki collection, owed a deletion the
                          source instance may not have completed. Carrying them
                          would tell the target instance to delete notes in a
                          collection it has never synced with"
   :activities "in-flight and historical operation state of ONE deployment, the
                same reason `upload_sessions` is excluded. An in-flight row
                carries a `work_key` naming work no process on the target is
                doing, so it would import already stale and read as an
                operation the user never ran"})

;; ── Export scope ───────────────────────────────────────────────────
;;
;; These are SQL strings, not queries — nothing here executes anything, and
;; only `freememo.account-export` reads them. They live here beside
;; `entity-tables`, and `every-exported-table-has-a-scope` checks the two
;; against each other. Loading the writer does not need a database either:
;; `freememo.db` starts its pool at the first query, and
;; `freememo.account-export` loads with the port refusing connections (measured
;; 2026-09-14). So if the scope map moves, move the test's require with it.

(def owned-topics
  "Subquery for the topics an export covers: the user's own, minus anything
   staged for deletion.

   A staged topic is hidden pending the 12-hour undo window, and `undo_log` is
   excluded from the archive (§5.2.2), so exporting one would resurrect deleted
   content on the target with no way to finish deleting it.
   `freememo.db/stage-topic-for-deletion!` marks the whole subtree, not only its
   root (db.clj:5141-5156), so this flat test needs no recursive walk.

   Public because the blob queries in `freememo.account-export` scope through it
   too, and those stay beside the writer — they read bytes, which is I/O this
   namespace does not do."
  "SELECT id FROM topics WHERE user_id = ? AND staged_delete_id IS NULL")

(def ^:private owned-flashcards
  "Subquery for the flashcards an export covers.
   `topic_id` is the owning topic and `root_topic_id` the document. The COALESCE
   covers rows the unified-topics migration left with a null `topic_id`
   (db.clj:1394-1397)."
  (str "SELECT id FROM flashcards WHERE COALESCE(topic_id, root_topic_id) IN ("
    owned-topics ")"))

(def ^:private owned-kg-facts
  "Subquery for the kg_facts an export covers. Scoped through `graph_topic_id`
   as well as `user_id`, so a fact distilled from a staged-deleted document stays
   behind with it."
  (str "SELECT id FROM kg_facts WHERE user_id = ? AND graph_topic_id IN ("
    owned-topics ")"))

(def ^:private owned-curriculum-exam-forms
  "Subquery for forms whose subject survives the same export."
  (str "SELECT f.id FROM curriculum_exam_forms f WHERE f.user_id = ? AND ("
    "f.block_id IN (SELECT b.id FROM curriculum_blocks b JOIN curricula c"
    " ON c.id = b.curriculum_id WHERE c.user_id = ? AND c.staged_delete_id IS NULL) OR "
    "f.material_id IN (SELECT m.id FROM curriculum_materials m"
    " JOIN curriculum_blocks b ON b.id = m.block_id"
    " JOIN curricula c ON c.id = b.curriculum_id WHERE c.user_id = ?"
    " AND c.staged_delete_id IS NULL"
    " AND m.topic_id IN (" owned-topics ")))"))

(def entity-scopes
  "Per-table scope, used to build both the `count(*)` and the row query.

   Each entry is {:where SQL :order SQL :params N :drop-keys [column …]}.
   `:params` is how many times the user id appears in `:where`, because every
   placeholder in every clause binds that one value. `:drop-keys` names columns
   removed from the row JSON — the blob columns, whose bytes travel as their own
   archive entries, and DERIVED columns, which the target recomputes from a
   column the archive does carry.

   Invariant: every member of `entity-tables` has an entry here, and this map
   has no key that is not a member. `account-export/scope-of` throws on a miss,
   which surfaces as a crashed WebSocket on the Settings page rather than a
   zero-row export — `every-exported-table-has-a-scope` is what catches it
   first.

   Invariant: no clause may admit a row another user owns, and no clause may
   admit a row referencing an entity this same map excludes. The second half is
   the easier one to break: `kg_question_facts` is scoped through BOTH exported
   questions and exported facts, because scoping it through questions alone would
   emit a link to a fact that `owned-kg-facts` left behind."
  {:sources {:where "t.user_id = ?" :params 1 :order "t.id"}

   :media {:where "t.user_id = ?" :params 1 :order "t.id" :drop-keys ["bytes"]}

   ;; content_preview is GENERATED from content_text, which this archive does
   ;; carry, so the target recomputes it on insert. Exporting it would add ~14 MB
   ;; of bytes the importer must drop anyway (account-import/dropped-columns).
   :topics {:where "t.user_id = ? AND t.staged_delete_id IS NULL"
            :params 1 :order "t.id" :drop-keys ["content_preview"]}

   ;; Scoped through the topic alone, not through `user_id` as well. The column
   ;; exists and is NOT NULL, but `owned-topics` already binds the user, and a
   ;; second clause binding the same value would only add a placeholder to keep
   ;; in step with `:params`.
   :poem_packages {:where (str "t.topic_id IN (" owned-topics ")")
                   :params 1 :order "t.id"}

   :topic_files {:where (str "t.topic_id IN (" owned-topics ")")
                 :params 1 :order "t.id" :drop-keys ["file_data"]}

   :topic_videos {:where (str "t.topic_id IN (" owned-topics ")")
                  :params 1 :order "t.topic_id"}

   :topic_epubs {:where (str "t.topic_id IN (" owned-topics ")")
                 :params 1 :order "t.topic_id"}

   ;; source_preview is GENERATED from source, which this archive does carry,
   ;; so the target recomputes it on insert. Postgres refuses an INSERT that
   ;; names a generated column, which is why account-import/dropped-columns
   ;; drops it as well — the same pair topics/content_preview forms.
   :topic_code {:where (str "t.topic_id IN (" owned-topics ")")
                :params 1 :order "t.topic_id" :drop-keys ["source_preview"]}

   :repo_selections {:where (str "t.topic_id IN (" owned-topics ")")
                     :params 1 :order "t.topic_id"}

   :video_transcripts {:where (str "t.topic_id IN (" owned-topics ")")
                       :params 1 :order "t.id"}

   :video_segments {:where (str "t.topic_id IN (" owned-topics ")")
                    :params 1 :order "t.id"}

   :topic_pins {:where (str "t.topic_id IN (" owned-topics ")")
                :params 1 :order "t.id"}

   ;; Scoped through the document as well as the user, so a highlight whose
   ;; book is staged for deletion stays behind with it. `chapter_topic_id` is
   ;; deliberately NOT scoped: it is nullable with ON DELETE SET NULL, and
   ;; import omits it from `required-fk-columns`, so an untranslatable value
   ;; arrives as null — the same outcome the source schema produces. Gating on
   ;; it would drop a whole highlight to protect a nullable pointer.
   :neoreader_highlights {:where (str "t.user_id = ? AND t.root_topic_id IN ("
                                   owned-topics ")")
                          :params 2 :order "t.id"}

   ;; Reached through the cards that use them, so a group orphaned by
   ;; reconcile-occlusion-group! is not exported and cannot dangle.
   :occlusion_groups {:where (str "t.id IN (SELECT occlusion_group_id FROM flashcards"
                               " WHERE occlusion_group_id IS NOT NULL AND id IN ("
                               owned-flashcards "))")
                      :params 1 :order "t.id"}

   :score_groups {:where (str "t.id IN (SELECT score_group_id FROM flashcards"
                           " WHERE score_group_id IS NOT NULL AND id IN ("
                           owned-flashcards "))")
                  :params 1 :order "t.id"}

   :flashcards {:where (str "t.id IN (" owned-flashcards ")") :params 1 :order "t.id"}

   :card_schedules {:where (str "t.flashcard_id IN (" owned-flashcards ")")
                    :params 1 :order "t.flashcard_id, t.ord"}

   :card_versions {:where (str "t.root_topic_id IN (" owned-topics ")")
                   :params 1 :order "t.id"}

   :topic_repetitions {:where (str "t.user_id = ? AND t.topic_id IN (" owned-topics ")")
                       :params 2 :order "t.id"}

   ;; Scoped through the document as well as the user: a schedule over a
   ;; staged-deleted document has nothing to forecast on the target, and its
   ;; `root_topic_id` is `required-fk-columns`, so it would be dropped there
   ;; anyway — dropping it here costs the import a reported row it cannot use.
   ;; `excluded_pages` holds page NUMBERS, not topic ids, so it needs no
   ;; translation and appears in no `fk-columns` entry.
   :study_schedules {:where (str "t.user_id = ? AND t.root_topic_id IN (" owned-topics ")")
                     :params 2 :order "t.id"}

   ;; A staged curriculum owns an undo_log reference that an account archive
   ;; does not carry. Excluding it keeps both the graph and the live marker
   ;; intact in its current account until Undo or purge resolves it.
   :curricula {:where "t.user_id = ? AND t.staged_delete_id IS NULL"
               :params 1 :order "t.id" :drop-keys ["staged_delete_id"]}

   :curriculum_blocks
   {:where "t.curriculum_id IN (SELECT id FROM curricula
                                 WHERE user_id = ? AND staged_delete_id IS NULL)"
    :params 1 :order "t.id"}

   :curriculum_materials
   {:where (str "t.block_id IN (SELECT b.id FROM curriculum_blocks b"
             " JOIN curricula c ON c.id = b.curriculum_id"
             " WHERE c.user_id = ? AND c.staged_delete_id IS NULL)"
             " AND t.topic_id IN (" owned-topics ")")
    :params 2 :order "t.id"}

   :curriculum_exam_forms {:where (str "t.id IN (" owned-curriculum-exam-forms ")")
                           :params 4 :order "t.id"}

   :curriculum_exam_sittings
   {:where (str "t.user_id = ? AND t.form_id IN (" owned-curriculum-exam-forms ")")
    :params 5 :order "t.id"}

   :curriculum_grade_revisions
   {:where (str "t.sitting_id IN (SELECT s.id FROM curriculum_exam_sittings s"
             " WHERE s.user_id = ? AND s.form_id IN (" owned-curriculum-exam-forms "))")
    :params 5 :order "t.id"}

   :curriculum_unlock_overrides
   {:where "t.curriculum_id IN (SELECT id FROM curricula
                                 WHERE user_id = ? AND staged_delete_id IS NULL)"
    :params 1 :order "t.id"}

   :assistant_chats {:where (str "t.user_id = ? AND t.root_topic_id IN (" owned-topics ")")
                     :params 2 :order "t.id"}

   :assistant_messages {:where (str "t.chat_id IN (SELECT id FROM assistant_chats"
                                 " WHERE user_id = ? AND root_topic_id IN ("
                                 owned-topics "))")
                        :params 2 :order "t.id"}

   :assistant_runs {:where (str "t.chat_id IN (SELECT id FROM assistant_chats"
                             " WHERE user_id = ? AND root_topic_id IN ("
                             owned-topics "))")
                    :params 2 :order "t.id"}

   :assistant_checkpoints {:where (str "t.chat_id IN (SELECT id FROM assistant_chats"
                                    " WHERE user_id = ? AND root_topic_id IN ("
                                    owned-topics "))")
                           :params 2 :order "t.id"}

   :assistant_evidence {:where (str "t.run_id IN (SELECT r.id FROM assistant_runs r"
                                 " JOIN assistant_chats c ON c.id = r.chat_id"
                                 " WHERE c.user_id = ? AND c.root_topic_id IN ("
                                 owned-topics "))")
                        :params 2 :order "t.id"}

   :kg_entities {:where "t.user_id = ?" :params 1 :order "t.id"}

   :kg_predicates {:where "t.user_id = ?" :params 1 :order "t.id"}

   :kg_facts {:where (str "t.id IN (" owned-kg-facts ")") :params 2 :order "t.id"}

   ;; Scoped through BOTH topic columns rather than through `graph_topic_id`
   ;; alone. A page of an owned document is itself owned today, because
   ;; `stage-topic-for-deletion!` marks the whole subtree — but that is a fact
   ;; about another function, and the second invariant above has to hold locally.
   ;;
   ;; `content_digest` and `digest_version` are deliberately NOT dropped on
   ;; import. Content travels with the account, so the digest still matches on
   ;; arrival and the skip holds; dropping them would re-extract, and re-bill, the
   ;; importer's whole account. `kg_staged_triples` IS dropped (§5.2.2), which is
   ;; what keeps an archive taken mid-run honest.
   :kg_page_extractions {:where (str "t.user_id = ? AND t.graph_topic_id IN ("
                                  owned-topics ") AND t.page_topic_id IN ("
                                  owned-topics ")")
                         :params 3 :order "t.id"}

   :kg_questions {:where "t.user_id = ?" :params 1 :order "t.id"}

   :kg_question_facts {:where (str "t.question_id IN"
                               " (SELECT id FROM kg_questions WHERE user_id = ?)"
                               " AND t.fact_id IN (" owned-kg-facts ")")
                       :params 3 :order "t.question_id, t.fact_id"}

   :kg_reviews {:where "t.user_id = ?" :params 1 :order "t.id"}

   :settings {:where "t.user_id = ?" :params 1 :order "t.key"}})

(def ^:private entity-table-set (set entity-tables))

;; ── Entity file paths ──────────────────────────────────────────────

(def ^:private entity-dir "entities/")
(def ^:private entity-suffix ".ndjson")

(defn entity-path
  "Archive path of `table`'s rows. §5.1.4 — one newline-delimited JSON file per
   table, so an import streams row by row instead of holding a table in heap.

   JSON rather than EDN because PostgreSQL owns the encoding at both ends:
   export selects `to_jsonb(t)` and writes the result verbatim, and import feeds
   a row back through `jsonb_populate_record`, which reads the TARGET's own
   column types. No JVM type map sits in between, so none can drift. A
   `timestamptz` keeps its offset and an `integer[]` stays an array without this
   namespace knowing which columns are which.

   Pre : `table` is a member of `entity-tables`.
   Post: a path string under `entities/`.
   Throws ex-info {:type ::unknown-entity-table} when the precondition fails —
   the caller chose this table, so a miss is the caller's bug."
  [table]
  (when-not (contains? entity-table-set table)
    (throw (ex-info (str "Not an exported table: " (pr-str table))
             {:type ::unknown-entity-table :table table})))
  (str entity-dir (name table) entity-suffix))

(defn table-of-entity-path
  "The table `path` holds rows for, or nil.

   Pre : `path` is an archive entry name, from untrusted content.
   Post: a member of `entity-tables`, or nil when `path` is not an entity file
         or names a table this build does not import. nil rather than a throw:
         an unknown entry is what a hostile or newer archive looks like."
  [path]
  (when (and (string? path)
          (str/starts-with? path entity-dir)
          (str/ends-with? path entity-suffix))
    (let [table (keyword (subs path (count entity-dir)
                           (- (count path) (count entity-suffix))))]
      (when (contains? entity-table-set table) table))))

;; ── Blob paths ─────────────────────────────────────────────────────

(def ^:private blob-dir "blobs/")

(def blob-kinds
  "§5.1.5 — the three blob mechanisms, named after the table that owns each.
   `:topic_files` is `bytea` keyed on (topic_id, role) and is the only kind
   carrying a role. `:media` is `bytea` deduped on (user_id, sha256).
   `:topic_videos` and `:topic_epubs` are PostgreSQL large objects, because
   `bytea` caps at 1 GB and cannot be range-read (db.clj:1018). They are two
   kinds rather than one because each is keyed on its own table's topic_id and
   restored by its own writer."
  #{:topic_files :media :topic_videos :topic_epubs})

(def ^:private role-pattern
  "A role that survives a path round trip. `topic_files.role` is free TEXT with
   a 'main' default and 'audio' for score recordings (db.clj:259), so nothing in
   the schema stops a future value holding a dot or a slash. Export refuses such
   a value rather than writing a path it cannot parse back, or one that escapes
   the blob directory."
  #"[a-z0-9-]+")

(defn blob-path
  "Archive path of one blob.

   Pre : `kind` is a member of `blob-kinds`. `source-id` is a positive integer.
         `role` is a string matching `role-pattern` when `kind` is
         `:topic_files`, and nil for every other kind.
   Post: a path string under `blobs/`, parsed back by `parse-blob-path` to the
         same three values.
   Throws ex-info {:type ::invalid-blob-path} on any precondition violation.
   The export path chose these values, so a violation is its bug."
  [kind source-id role]
  (let [bad (cond
              (not (contains? blob-kinds kind)) (str "unknown blob kind " (pr-str kind))
              (not (and (integer? source-id) (pos? source-id)))
              (str "source id must be a positive integer, got " (pr-str source-id))
              (= kind :topic_files)
              (when-not (and (string? role) (re-matches role-pattern role))
                (str "role must match " role-pattern ", got " (pr-str role)))
              (some? role) (str "only :topic_files carries a role, got " (pr-str role)))]
    (when bad
      (throw (ex-info (str "Cannot build a blob path: " bad)
               {:type ::invalid-blob-path :kind kind :source-id source-id :role role}))))
  (str blob-dir (name kind) "/" source-id
    (when (= kind :topic_files) (str "." role))))

(defn parse-blob-path
  "The blob `path` names, or nil.

   Pre : `path` is an archive entry name, from untrusted content.
   Post: {:kind k :source-id n :role r} with `r` nil for every kind but
         `:topic_files`, or nil when `path` is not a blob this build recognises.
         nil rather than a throw, for the reason `table-of-entity-path` gives."
  [path]
  (when (and (string? path) (str/starts-with? path blob-dir))
    (let [[kind-str file & more] (str/split (subs path (count blob-dir)) #"/")
          kind (keyword kind-str)]
      (when (and (nil? more) (some? file) (contains? blob-kinds kind))
        (if (= kind :topic_files)
          (let [dot (str/last-index-of file ".")]
            (when dot
              (let [source-id (parse-long (subs file 0 dot))
                    role (subs file (inc dot))]
                (when (and source-id (pos? source-id) (re-matches role-pattern role))
                  {:kind kind :source-id source-id :role role}))))
          (when-let [source-id (parse-long file)]
            (when (pos? source-id)
              {:kind kind :source-id source-id :role nil})))))))
