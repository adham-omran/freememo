(ns freememo.account-transfer-format
  "The on-disk shape of an account transfer archive, shared by export and import.

   plans/user-data-export-import.md §5.1, §5.2. A user exports a whole account
   from one FreeMemo instance and imports it into another. This namespace owns
   the vocabulary both halves must agree on: the format version, the manifest
   keys, the entity insert order, and the path of every file in the archive.
   It performs no I/O and touches no database, so both peers and the tests read
   the same constants.

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
  "§5.2.1 — the 24 exported tables, in the order an import must insert them.

   Invariant: the order is forced by foreign keys. A table may not appear
   before every table it references. Three constraints are easy to lose:
   `flashcards` follows `topics` and both group tables; `kg_reviews` follows
   `flashcards` AND `kg_questions`, because its CHECK requires exactly one of
   them (db.clj:1006); `settings` is last, because its keys embed topic ids and
   the rewrite needs the finished topic map (§5.4.8)."
  [:sources
   :media
   :topics
   :topic_files
   :topic_videos
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
   :assistant_chats
   :assistant_messages
   :kg_entities
   :kg_predicates
   :kg_facts
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
   :user_events "analytics of the source deployment"
   :credit_orders "billing of the source deployment"
   :credit_transactions "billing of the source deployment"
   :undo_log "a 12-hour operational window, capped at 100 rows per user"
   :upload_sessions "in-flight transport state"
   :kg_graph_layout "a render cache that a version compare invalidates"
   :kg_sessions "quiz session transcript — kg_reviews holds the durable record"
   :kg_answers "quiz session transcript — kg_reviews holds the durable record"
   :account_imports "import bookkeeping — target-instance state (§5.5.7)"
   :account_import_ids "import bookkeeping — target-instance state (§5.5.7)"})

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
   `:topic_videos` is a PostgreSQL large object, because `bytea` caps at 1 GB
   and cannot be range-read (db.clj:1018)."
  #{:topic_files :media :topic_videos})

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
