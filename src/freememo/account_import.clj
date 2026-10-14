(ns freememo.account-import
  "Import an account transfer archive into this instance, additively.

   plans/user-data-export-import.md §5.4, §5.5, §5.6. `freememo.account-export`
   wrote the archive and `freememo.account-transfer-format` owns its shape. This
   namespace owns id remapping, the ledger, and the phase split.

   **Every id in the archive is a source id and must be translated.** Four
   classes, and only the first is visible to a schema walk: foreign keys
   (`fk-columns`), id columns with no foreign key (`card_versions/scope_id` and
   the two copied media ids), ids inside JSONB (`score_groups/geometry`), and
   `/api/media/<id>` URLs inside HTML text. A fifth hides in `settings`, whose
   KEYS embed topic ids.

   **PostgreSQL reconstructs every row.** A remapped row goes back through
   `jsonb_populate_record(null::<table>, ?::jsonb)`, so the TARGET's column types
   decide how each value is read. No JVM type map sits in between. The column
   list comes from `information_schema`, never from the archive — an archive key
   that names no real column is dropped, which is both the schema-tolerance
   mechanism and the reason a hostile key cannot reach SQL. `import-table!`
   reports what it dropped, because silently losing a column is how an archive
   from a newer build would lose data with no signal.

   **Three phases, not two.** The plan says metadata then blobs. Two blob columns
   are `NOT NULL` (`topic_files.file_data`, `topic_videos.lo_oid`), so their rows
   cannot exist before their bytes — which is what makes §5.5.5's gap detection a
   missing ROW rather than an empty column. `media.bytes` is `NOT NULL` too, but
   the media id map must exist before phase 1 rewrites any HTML, so media lands
   first, outside the transaction, and is compensated on failure. That is the
   shape `supermemo-import/import-collection!` already uses for the same reason.

   Phase 0  media rows and bytes. Compensated by deletion and a refund on failure.
   Phase 1  every other metadata table, in one transaction.
   Phase 2  topic_files and topic_videos rows plus bytes, one commit each.

   **A resume repeats phase 2 only.** Phase 1 is atomic, so its rows are either
   all present or all absent — there is no partial metadata state for a rerun to
   fill. `account_imports.metadata_done_at` set with `blobs_done_at` still null is
   exactly what a resumable run looks like."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [freememo.account-transfer-format :as fmt]
            [freememo.largeobj :as lo]
            [freememo.quota :as quota]
            [freememo.user-state :as us]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]
            [taoensso.telemere :as tel])
  (:import [java.io File]
           [java.security MessageDigest]
           [java.util UUID]
           [org.postgresql.util PGobject]))

;; ── Remap specification ────────────────────────────────────────────

(def ^:private user-scoped-tables
  "Tables carrying a `user_id` that must point at the importing user.
   An archive's own user id is provenance only and is never trusted."
  #{:sources :media :topics :poem_packages :topic_repetitions :study_schedules
    :curricula :curriculum_exam_forms :curriculum_exam_sittings :assistant_chats
    :kg_entities :kg_predicates :kg_facts :kg_questions :kg_reviews :settings
    :question_maps :walkthroughs})

(def ^:private fk-columns
  "column → id space, per table. The id space is the table whose ledger map
   translates the value.

   `topic_files` and `topic_videos` are absent on purpose: phase 2 builds their
   rows directly and knows its own topic id."
  {:topics {:parent_id :topics :source_id :sources}
   :poem_packages {:topic_id :topics}
   ;; Both are phase-1 tables, unlike the blob tables above: their rows travel
   ;; as ordinary JSON, so the topic id in each needs translating like any other.
   :topic_code {:topic_id :topics}
   :repo_selections {:topic_id :topics}
   :topic_pins {:topic_id :topics :media_id :media}
   :reading_positions {:topic_id :topics}
   :neoreader_highlights {:root_topic_id :topics :chapter_topic_id :topics}
   :occlusion_groups {:image_media_id :media}
   :score_groups {:clip_media_id :media}
   :flashcards {:topic_id :topics :root_topic_id :topics
                :occlusion_group_id :occlusion_groups :score_group_id :score_groups}
   :card_schedules {:flashcard_id :flashcards}
   :card_versions {:root_topic_id :topics
                   :occlusion_image_media_id :media
                   :score_clip_media_id :media}
   :topic_repetitions {:topic_id :topics}
   ;; `excluded_pages` is deliberately absent: it holds page NUMBERS, not topic
   ;; ids. An id inside an array is the case this map cannot translate — it
   ;; rewrites scalars — so the column type is what keeps the schedule honest
   ;; on the target rather than an entry here.
   :study_schedules {:root_topic_id :topics}
   :curriculum_blocks {:curriculum_id :curricula}
   :curriculum_materials {:block_id :curriculum_blocks :topic_id :topics}
   :curriculum_exam_forms {:block_id :curriculum_blocks
                           :material_id :curriculum_materials}
   :curriculum_exam_sittings {:form_id :curriculum_exam_forms}
   :curriculum_grade_revisions {:sitting_id :curriculum_exam_sittings}
   :curriculum_unlock_overrides {:curriculum_id :curricula
                                 :from_block_id :curriculum_blocks
                                 :to_block_id :curriculum_blocks}
   ;; A fork's two ids are translated by `link-imported-forks!`, after the
   ;; messages exist. `table-specific-remap` holds both back from the insert.
   :assistant_chats {:root_topic_id :topics
                     :parent_chat_id :assistant_chats
                     :fork_message_id :assistant_messages}
   :assistant_messages {:chat_id :assistant_chats}
   :assistant_runs {:chat_id :assistant_chats
                    :user_message_id :assistant_messages
                    :assistant_message_id :assistant_messages}
   :assistant_checkpoints {:chat_id :assistant_chats :run_id :assistant_runs
                           :covered_from_message_id :assistant_messages
                           :covered_through_message_id :assistant_messages
                           :for_chat_id :assistant_chats}
   :assistant_evidence {:run_id :assistant_runs
                        :document_topic_id :topics :page_topic_id :topics
                        :walkthrough_id :walkthroughs}
   :video_transcripts {:topic_id :topics}
   :video_segments {:topic_id :topics}
   :kg_facts {:subject_entity_id :kg_entities :predicate_id :kg_predicates
              :object_entity_id :kg_entities :graph_topic_id :topics}
   :kg_page_extractions {:graph_topic_id :topics :page_topic_id :topics}
   :kg_question_facts {:question_id :kg_questions :fact_id :kg_facts}
   :kg_reviews {:question_id :kg_questions :flashcard_id :flashcards}
   :question_maps {:repo_topic_id :topics}
   :question_roots {:map_id :question_maps :entity_id :kg_entities}
   :question_pins {:map_id :question_maps :snapshot_topic_id :topics
                   :root_id :question_roots}
   :question_pin_tombstones {:map_id :question_maps}
   :known_marks {:entity_id :kg_entities :as_of_repo_topic_id :topics
                 :proof_run_id :assistant_runs}
   :flashcard_entities {:flashcard_id :flashcards :entity_id :kg_entities}
   :walkthroughs {:repo_topic_id :topics}
   :walkthrough_concepts {:walkthrough_id :walkthroughs}
   :walkthrough_hunks {:walkthrough_id :walkthroughs :concept_id :walkthrough_concepts}})

(def ^:private required-fk-columns
  "Foreign keys whose row is worthless without them. A row whose value here does
   not translate is dropped and reported, rather than inserted with a null that
   the schema forbids or a dangling id the schema cannot see."
  {:poem_packages #{:topic_id}
   :topic_pins #{:topic_id :media_id}
   ;; Half of the primary key. A position for no topic is no position.
   :reading_positions #{:topic_id}
   ;; topic_id is the PRIMARY KEY of both tables. A row whose topic did not
   ;; translate has no identity at all, so it is dropped and reported.
   :topic_code #{:topic_id}
   :repo_selections #{:topic_id}
   :occlusion_groups #{:image_media_id}
   :score_groups #{:clip_media_id}
   :card_schedules #{:flashcard_id}
   :card_versions #{:root_topic_id}
   :topic_repetitions #{:topic_id}
   :study_schedules #{:root_topic_id}
   :curriculum_blocks #{:curriculum_id}
   :curriculum_materials #{:block_id :topic_id}
   :curriculum_exam_sittings #{:form_id}
   :curriculum_grade_revisions #{:sitting_id}
   :curriculum_unlock_overrides #{:curriculum_id :from_block_id :to_block_id}
   :assistant_chats #{:root_topic_id}
   :assistant_messages #{:chat_id}
   :assistant_runs #{:chat_id :user_message_id :assistant_message_id}
   :assistant_checkpoints #{:chat_id :run_id :covered_from_message_id
                            :covered_through_message_id}
   :assistant_evidence #{:run_id}
   :video_transcripts #{:topic_id}
   :video_segments #{:topic_id}
   :neoreader_highlights #{:root_topic_id}
   :kg_facts #{:subject_entity_id :predicate_id :graph_topic_id}
   ;; Both are NOT NULL and both identify the row's subject. An extraction record
   ;; whose page did not translate would claim a page of the wrong document, and
   ;; the UNIQUE on page_topic_id makes that a hard failure rather than a wrong
   ;; skip — dropping the row instead re-extracts one page.
   :kg_page_extractions #{:graph_topic_id :page_topic_id}
   :kg_question_facts #{:question_id :fact_id}
   ;; A map is its document key, and roots and pins are nothing without their
   ;; map. `entity_id`, `snapshot_topic_id` and `root_id` are nullable with ON
   ;; DELETE SET NULL, so an untranslatable value arrives as null, the same
   ;; outcome the source schema produces.
   :question_maps #{:repo_topic_id}
   :question_roots #{:map_id}
   :question_pins #{:map_id}
   :question_pin_tombstones #{:map_id}
   ;; Half of the primary key.
   :known_marks #{:entity_id}
   ;; The whole primary key: a link is nothing without both ends.
   :flashcard_entities #{:flashcard_id :entity_id}
   ;; A walkthrough is its repository's range, and its concepts and hunks
   ;; are nothing without it. A hunk with no concept would break the
   ;; one-concept-per-hunk invariant (§6.5.2).
   :walkthroughs #{:repo_topic_id}
   :walkthrough_concepts #{:walkthrough_id}
   :walkthrough_hunks #{:walkthrough_id :concept_id}})

(def ^:private dropped-columns
  "Columns removed before insert. `id` everywhere — the target's sequence issues
   it. `staged_delete_id` because `undo_log` is not imported (§5.2.2), so any
   value would dangle and would also hide the topic, curriculum or chat on arrival.

   An entry REPLACES the `#{:id}` default, so every entry must name `:id` too.

   `neoreader_highlights.quote_hash` and `topics.content_preview` are GENERATED
   columns. Postgres refuses an INSERT that names one, and the column filter
   cannot see the difference — it exists in `information_schema` like any other.
   A generated column in any future table needs its own entry here for the same
   reason. `entity-scopes` also drops `content_preview` from the exported row
   JSON, so a current archive does not carry it; this entry is what protects the
   import if that `:drop-keys` is ever removed."
  ;; `poem_packages` has NO entry, so the `#{:id}` default applies. Its other
  ;; seven columns are all carried: the four JSONB documents ARE the poem, and
  ;; `created_at` records when the package was imported, which stays true on the
  ;; target. It holds no generated column and no single-collection state.
  {:topics #{:id :staged_delete_id :content_preview}
   ;; `source_preview` is GENERATED, the same case as content_preview above.
   ;; `:id` is named because an entry REPLACES the default, even though this
   ;; table's key is topic_id and it has no id column of its own.
   :topic_code #{:id :source_preview}
   :curricula #{:id :staged_delete_id}
   :assistant_chats #{:id :staged_delete_id}
   :neoreader_highlights #{:id :quote_hash}
   ;; The reconcile baselines and the conflict flag describe ONE Anki
   ;; collection. On the target they would claim a card is in step with a
   ;; collection it has never met. Dropped, the card arrives with no baseline:
   ;; `anki-reconcile/classify` reads that as `:needs-baseline`, and since the
   ;; target's Anki does not hold the note, the first run classifies it
   ;; `:create` and pushes it fresh.
   ;;
   ;; `anki_note_id` is deliberately NOT dropped here — that is pre-existing
   ;; behaviour and changing it is outside this work. The residual risk it
   ;; carries: if the target collection happens to hold a note with the same id,
   ;; the first backfill adopts it.
   ;;
   ;; `local_current_digest` is NOT dropped, and that is load-bearing. It is a
   ;; derived column, so dropping it looks right — but the baselines beside it ARE
   ;; dropped, and `db/unsynced-sql` reads the PAIR. With current NULL and baseline
   ;; NULL, `NULL IS DISTINCT FROM NULL` is false, so an imported card that carries
   ;; an `anki_note_id` would read as SYNCED while the reconcile classified it
   ;; `:needs-baseline` and then `:create` — the count-versus-runner disagreement
   ;; this predicate exists to remove. Carried, it holds the source's digest
   ;; against a NULL baseline, which reads unsynced.
   ;;
   ;; The value is safe to carry because it is a pure function of the row's own
   ;; content: the target recomputes the same string from the same columns. Nothing
   ;; refills it on arrival — `backfill-local-card-digests!` runs at boot, not after
   ;; an import.
   ;;
   ;; `anki_header_text` and `anki_bibliography_html` are carried for the same
   ;; reason: the digest covers them, so dropping them would make the carried
   ;; digest disagree with the carried content.
   ;; `baseline_digest_version` goes with the baselines it describes. Keeping it
   ;; while dropping them would leave a row claiming a current-generation baseline
   ;; and holding none — harmless, because `classify` tests the nil baselines
   ;; first, but a lie in the data either way.
   :flashcards #{:id :anki_baseline_digest :local_baseline_digest
                 :baseline_digest_version
                 :anki_baseline_mod :anki_conflict_at}})

(def ^:private html-columns
  "Plain-HTML columns holding `/api/media/<id>` URLs (§5.4.7).
   `assistant_messages.content` has no code path that writes such a URL, but an
   assistant reply can echo one out of injected document context.

   `flashcards.anki_bibliography_html` holds HTML and is deliberately absent. It
   is built by `anki-sync-helpers/format-bibliography-html`, whose only URL is the
   citation's own `csl :URL` — a web address, never `/api/media/`. Listing it would
   claim a property it does not have, and the push path rewrites the column on
   every run anyway."
  {:topics [:content]
   :flashcards [:question :answer :cloze]
   :card_versions [:question :answer :cloze]
   :assistant_messages [:content]})

(def ^:private id-bearing-tables
  "Tables whose new ids the ledger persists.

   Deliberately narrow. Phase 1 keeps every table's map in memory, and the only
   maps a later phase or a resume needs are the two that name a blob: a
   `topic_files` or `topic_videos` blob is keyed by its topic's source id, and a
   `media` blob by its own. Persisting all 23 maps would roughly double the row
   count of an import for data nothing reads."
  #{:topics :media})

(def ^:private natural-key-lookup
  "Tables with a per-user unique key that an additive merge can collide with.

   Each entry is [SQL columns…]: the SQL binds the user id followed by each named
   column, and returns the existing row's id. On a hit the source id maps onto
   that row and NO insert happens (§5.4.5).

   `kg_predicates` is the only member. Its `UNIQUE (user_id, slug)` (db.clj:765)
   makes a second import of the same vocabulary fail outright, and the predicate
   vocabulary is shared reference data — an imported fact should point at the
   predicate the user already approved, not at a second copy of it.

   `media` has the same shape and is handled in phase 0, where the key is a
   digest of bytes this map cannot see."
  {:kg_predicates ["SELECT id FROM kg_predicates WHERE user_id = ? AND slug = ?" :slug]})

(def ^:private conflict-clauses
  "Per-table `ON CONFLICT` tails.

   `settings` is keyed on `(user_id, key)`, so importing into a user who already
   has preferences collides on every global key. DO NOTHING keeps the target's
   own value: an additive import must not silently retune the instance the user is
   importing INTO. Per-topic keys carry a freshly mapped topic id and never
   collide, so they still arrive."
  {:settings "ON CONFLICT DO NOTHING"})

(def ^:private settings-key-prefixes
  "Settings key prefixes whose suffix is a topic id (§5.4.8, settings.clj).
   The panel keys use `pane-scope-id`. It resolves to the document root topic
   id, so one translation serves all pages."
  ["last_page_" "pdf_layout_" "pdf_fit_" "pdf_focus_" "focus_reader_" "extract_style_"
   "ocr_model_" "card_model_"
   "goal_" "assistant_model_" "anki_preset_" "hierarchy_open_" "pins_open_"
   "assistant_tab_" "left_panel_tab_"
   ;; `right_panel_tab_<pane-scope-id>` is the poem reader's tab choice, saved
   ;; the way `left_panel_tab_` already is (settings.clj). Same suffix, same
   ;; translation — without the entry the restored preference names another
   ;; user's topic and the tab silently reverts.
   "right_panel_tab_"
   ;; The per-document Anki header and tags (settings.clj). Each is two rows
   ;; keyed on the root topic id, with no account-wide row behind them, so an
   ;; untranslated suffix does not degrade to a global default — it points at
   ;; another user's topic and the setting is simply lost. The header pair was
   ;; missing here from the day it was written; the tag pair arrived with
   ;; plans/anki-tags-per-document.md.
   "anki_use_header_" "anki_header_text_"
   "anki_use_tags_" "anki_tags_"
   ;; `boox_file_<root topic id>` binds an EPUB document to a Google Drive file
   ;; (freememo.neoreader-sync/file-id-setting-prefix). The id needs the same
   ;; translation as every other per-topic key. The file id itself travels
   ;; unchanged and is harmless on the target: the app holds no `drive.file`
   ;; grant for it there, so a sync asks the user to pick the file again.
   "boox_file_"
   ;; `code_view_<code topic id>`: rendered or source, for one repo Markdown
   ;; file (plans/markdown-rendered-code-pane.md §3).
   "code_view_"
   ;; `editor_hidden_<PDF root or document root>`: the viewer's hidden editor
   ;; pane (plans/editor-pane-toggle.md §1.3).
   "editor_hidden_"])

;; ── Media URL rewriting (§5.4.7) ───────────────────────────────────

(def ^:private media-url-pattern #"/api/media/(\d+)")

(defn rewrite-media-urls
  "Translate every `/api/media/<id>` in `html` through `media-map`.

   Pre : `html` is a string or nil. `media-map` maps source media id to new id.
   Post: the same string with each id replaced, or nil for nil input. An id the
         map does not hold is left as it stands — the archive references media it
         did not carry, and a dead link beats a link to another topic's image."
  [html media-map]
  (when html
    (str/replace html media-url-pattern
      (fn [[whole id-str]]
        (if-let [new-id (get media-map (parse-long id-str))]
          (str "/api/media/" new-id)
          whole)))))

(defn- rewrite-value-html
  "Rewrite media URLs anywhere inside `v`, walking maps and vectors.
   Covers `io_fields` (a map of HTML fields) and `overlapping` (`:items` of HTML
   plus a materialized `:fields` map) without naming their shapes."
  [v media-map]
  (cond
    (string? v) (rewrite-media-urls v media-map)
    (map? v) (into {} (map (fn [[k x]] [k (rewrite-value-html x media-map)])) v)
    (vector? v) (mapv #(rewrite-value-html % media-map) v)
    :else v))

;; ── Target schema ──────────────────────────────────────────────────

(defn- table-columns
  "The set of column names `table` actually has on this instance, as keywords.

   This is the boundary that keeps archive content out of SQL: the insert's
   column list is built from this set, never from the archive's keys. It is also
   what lets an archive from a different build import — an unknown key is
   dropped and reported instead of failing the row."
  [ds table]
  (into #{}
    (map (comp keyword :column_name))
    (jdbc/execute! ds
      ["SELECT column_name FROM information_schema.columns
        WHERE table_schema = 'public' AND table_name = ?" (name table)]
      {:builder-fn rs/as-unqualified-maps})))

;; ── Row remapping ──────────────────────────────────────────────────

(defn- remap-row
  "Translate one archive row for `table`.

   Pre : `row` is the parsed JSON object for one source row, keys keywordized.
         `maps` holds one source-id-to-new-id map per id space, for every space
         this table's columns reference. `user-id` is the importing user.
   Post: {:row M} ready for insert, or {:drop reason} when a required foreign key
         does not translate.
   Invariant: no source id survives in the result. A column named in
         `fk-columns` is either translated, nil, or the row is dropped."
  [table row {:keys [maps user-id media-map default-parent-id]}]
  (let [fks (get fk-columns table)
        required (get required-fk-columns table #{})
        drop-reason (volatile! nil)
        translated
        (reduce-kv
          (fn [acc col space]
            (let [v (get acc col ::absent)]
              (cond
                (or (= v ::absent) (nil? v)) acc
                :else
                (if-let [new-id (get (get maps space) v)]
                  (assoc acc col new-id)
                  (do (when (contains? required col)
                        (vreset! drop-reason
                          (str (name col) " → no imported " (name space) " " v)))
                    (assoc acc col nil))))))
          row
          fks)]
    (if-let [reason @drop-reason]
      {:drop reason}
      {:row (cond-> translated
              ;; user_id is set, never carried: the archive's value names a user
              ;; on another instance.
              (contains? user-scoped-tables table) (assoc :user_id user-id)
              ;; A source root topic has no parent. It attaches under the
              ;; destination the user picked (§5.4.11).
              (and (= table :topics) (nil? (:parent_id translated)))
              (assoc :parent_id default-parent-id)
              true (as-> r (reduce (fn [m col]
                                     (if (contains? m col)
                                       (assoc m col (rewrite-media-urls (get m col) media-map))
                                       m))
                             r (get html-columns table []))))})))

(defn- remap-card-versions-scope
  "Translate `card_versions.scope_id`, which carries no foreign key (§5.4.4).

   `scope_type` selects the id space. The value may already point at nothing on
   the source — `reconcile-occlusion-group!` deletes flashcard rows whose mask
   ordinal disappears — so an untranslatable scope means the row is dropped.
   Keeping it would attach one card's history to an unrelated new card.

   Pre : `row` has been through `remap-row`. Post: {:row M} or {:drop reason}."
  [row maps]
  (let [space (case (:scope_type row)
                "card" :flashcards
                "occlusion_group" :occlusion_groups
                "score_group" :score_groups
                nil)
        old (:scope_id row)]
    (cond
      (nil? space) {:drop (str "unknown scope_type " (pr-str (:scope_type row)))}
      (nil? old) {:drop "scope_id is null"}
      :else (if-let [new-id (get (get maps space) old)]
              {:row (assoc row :scope_id new-id)}
              {:drop (str "scope_id → no imported " (name space) " " old)}))))

(defn- remap-score-geometry
  "Translate the `:media-id` on every rect inside `score_groups.geometry`
   (§5.4.6, db.clj:469-471). A rect whose media did not import keeps its value,
   which renders as a missing crop rather than another topic's image."
  [geometry media-map]
  ;; Only a shape this recognises is touched. An `update` with an `(or pages [])`
  ;; default would ADD a :pages key to geometry that had none, editing data whose
  ;; shape we do not understand.
  (if-not (sequential? (:pages geometry))
    geometry
    (update geometry :pages
      (fn [pages]
        (mapv (fn [page]
                (if-not (sequential? (:rects page))
                  page
                  (update page :rects
                    (fn [rects]
                      (mapv (fn [rect]
                              (if-let [new-id (get media-map (:media-id rect))]
                                (assoc rect :media-id new-id)
                                rect))
                        rects)))))
          pages)))))

;; ── Insert ─────────────────────────────────────────────────────────

(defn- ->jsonb
  "Wrap a Clojure value as a JSONB parameter."
  [v]
  (doto (PGobject.) (.setType "jsonb") (.setValue (json/generate-string v))))

(defn- insert-row!
  "Insert one remapped row and return its new id, or nil for a table with no
   `id` column.

   Pre : `row`'s keys are a subset of `columns`, which came from
         `information_schema` — never from the archive.
   Post: exactly one row inserted, carrying only the columns `columns` holds. Any
         other key in `row` is silently absent — `import-table!` is what reports
         it, because it owns the per-table report. The row's values are read by
         `jsonb_populate_record` against the TARGET's column types, so a
         `timestamptz` keeps its offset and an `integer[]` stays an array with no
         type map in this namespace.
   Invariant: the emitted column list is derived from `columns`, so no archive
         content ever reaches the SQL text."
  [tx table row columns returning-id?]
  (let [cols (filterv #(contains? columns %) (keys row))
        col-sql (str/join ", " (map name cols))
        sql (str "INSERT INTO " (name table) " (" col-sql ") SELECT " col-sql
              " FROM jsonb_populate_record(null::" (name table) ", ?::jsonb)"
              (when-let [c (get conflict-clauses table)] (str " " c))
              (when returning-id? " RETURNING id"))]
    (if returning-id?
      (:id (jdbc/execute-one! tx [sql (->jsonb (select-keys row cols))]
             {:builder-fn rs/as-unqualified-maps}))
      (do (jdbc/execute-one! tx [sql (->jsonb (select-keys row cols))]) nil))))

;; ── Archive reading ────────────────────────────────────────────────

(defn read-manifest
  "Parse and validate the archive's manifest.
   Pre : `dir` is an extracted archive directory.
   Post: {:ok true :manifest M} or {:ok false :error S}. Never throws on a
         malformed or absent manifest — that is archive content."
  [^File dir]
  (let [f (io/file dir fmt/manifest-path)]
    (if-not (.exists f)
      {:ok false :error (str "The archive has no " fmt/manifest-path
                          ". This is not a FreeMemo export.")}
      (let [parsed (try (read-string (slurp f))
                     (catch Exception e {::unreadable (.getMessage e)}))]
        (if (::unreadable parsed)
          {:ok false :error (str "The manifest could not be read: "
                              (::unreadable parsed))}
          (let [v (fmt/validate-manifest parsed)]
            (if (:ok v) {:ok true :manifest parsed} v)))))))

(defn- entity-rows
  "Lazily read `table`'s rows from the archive.
   Post: a sequence of parsed JSON objects with keywordized keys, empty when the
         entity file is absent. One line is held at a time."
  [^File dir table]
  (let [f (io/file dir (fmt/entity-path table))]
    (when (.exists f)
      (->> (line-seq (io/reader f))
        (remove str/blank?)
        (map #(json/parse-string % true))))))

;; ── Ledger (§5.5) ──────────────────────────────────────────────────

(defn find-import
  "The newest import run for this user and export id, or nil.
   Post: a row with :id :copy_ordinal :metadata_done_at :blobs_done_at
         :parent_topic_id."
  [ds user-id export-id]
  (jdbc/execute-one! ds
    ["SELECT id, copy_ordinal, parent_topic_id, metadata_done_at, blobs_done_at
      FROM account_imports WHERE user_id = ? AND export_id = ?
      ORDER BY copy_ordinal DESC LIMIT 1" user-id export-id]
    {:builder-fn rs/as-unqualified-maps}))

(defn- create-import!
  "Open a ledger run. Post: its id. `copy_ordinal` is one past the newest run for
   this export, so a deliberate second copy never collides with a resume."
  [tx user-id manifest parent-topic-id]
  (let [next-ordinal (or (some-> (jdbc/execute-one! tx
                                   ["SELECT max(copy_ordinal) AS m FROM account_imports
                                     WHERE user_id = ? AND export_id = ?"
                                    user-id (:export-id manifest)]
                                   {:builder-fn rs/as-unqualified-maps})
                           :m inc)
                       0)]
    (:id (jdbc/execute-one! tx
           ["INSERT INTO account_imports
             (user_id, export_id, copy_ordinal, format_version, source_instance,
              exported_at, parent_topic_id)
             VALUES (?, ?, ?, ?, ?, ?, ?) RETURNING id"
            user-id (:export-id manifest) next-ordinal (:format-version manifest)
            (:source-instance manifest) (str (:exported-at manifest)) parent-topic-id]
           {:builder-fn rs/as-unqualified-maps}))))

(defn- record-ids!
  "Persist a source-id-to-new-id map for one id space.
   Pre : `space` is a member of `id-bearing-tables`."
  [tx import-id space id-map]
  (when (seq id-map)
    (jdbc/execute-batch! tx
      "INSERT INTO account_import_ids (import_id, source_kind, source_id, new_id)
       VALUES (?, ?, ?, ?) ON CONFLICT DO NOTHING"
      (mapv (fn [[old new]] [import-id (name space) old new]) id-map)
      {})))

(defn- load-ids
  "The persisted map for one id space of an existing run.
   Post: {source-id new-id}, empty when nothing was recorded."
  [ds import-id space]
  (into {}
    (map (juxt :source_id :new_id))
    (jdbc/execute! ds
      ["SELECT source_id, new_id FROM account_import_ids
        WHERE import_id = ? AND source_kind = ?" import-id (name space)]
      {:builder-fn rs/as-unqualified-maps})))

;; ── Phase 0: media (§5.4.9) ────────────────────────────────────────

(defn- sha256-hex
  "Hex SHA-256 of `f`, read in 256 KiB steps.
   Recomputed from the bytes rather than trusted from the archive row: the sha is
   what dedupe keys on, so a wrong one would attach a topic to another image."
  [^File f]
  (let [md (MessageDigest/getInstance "SHA-256")
        buf (byte-array 262144)]
    (with-open [in (java.io.FileInputStream. f)]
      (loop []
        (let [n (.read in buf)]
          (when (pos? n) (.update md buf 0 n) (recur)))))
    (str/join (map #(format "%02x" %) (.digest md)))))

(defn- import-media!
  "Phase 0. Insert media rows with their bytes, deduping against what the user
   already holds.

   Pre : `dir` is an extracted archive. Nothing else has been written yet.
   Post: {:media-map M :created-ids v :skipped v}. `:created-ids` names the rows
         THIS call inserted, so the caller can compensate them on a later
         failure — a deduped row belongs to the user already and must not be
         deleted.
   Invariant: heap high-water is one media blob.
   Blame: a media row whose blob entry is absent is skipped and reported. HTML
         referencing it keeps its original URL and renders a missing image."
  [ds user-id ^File dir]
  (reduce
    (fn [acc row]
      (let [source-id (:id row)
            blob (io/file dir (fmt/blob-path :media source-id nil))]
        (if-not (.exists blob)
          (update acc :skipped conj {:media source-id :reason "blob entry absent"})
          (let [sha (sha256-hex blob)
                existing (jdbc/execute-one! ds
                           ["SELECT id FROM media WHERE user_id = ? AND sha256 = ?"
                            user-id sha]
                           {:builder-fn rs/as-unqualified-maps})]
            (if existing
              (assoc-in acc [:media-map source-id] (:id existing))
              (let [size (.length blob)
                    new-id (jdbc/with-transaction [tx ds]
                             ;; reserve-bytes!, not check-and-bump!: the
                             ;; per-upload cap is a single-request bound, and an
                             ;; import is not a request (quota.clj:155).
                             (quota/reserve-bytes! tx user-id size)
                             (:id (jdbc/execute-one! tx
                                    ["INSERT INTO media
                                      (user_id, kind, bytes, mime_type, sha256, byte_size, source_url)
                                      VALUES (?, ?, ?, ?, ?, ?, ?) RETURNING id"
                                     user-id (or (:kind row) "image")
                                     (java.nio.file.Files/readAllBytes (.toPath blob))
                                     (or (:mime_type row) "application/octet-stream")
                                     sha size (:source_url row)]
                                    {:builder-fn rs/as-unqualified-maps})))]
                (-> acc
                  (assoc-in [:media-map source-id] new-id)
                  (update :created-ids conj new-id))))))))
    {:media-map {} :created-ids [] :skipped []}
    (entity-rows dir :media)))

(defn- compensate-media!
  "Delete the media rows this import created and refund their bytes.
   Pre : `ids` are rows THIS import inserted, never deduped ones.
   Post: those rows are gone and `usage_bytes` is back where it started, so a
         failed import costs the user no storage."
  [ds user-id ids]
  (when (seq ids)
    (try
      (jdbc/with-transaction [tx ds]
        (let [freed (or (:s (jdbc/execute-one! tx
                              [(str "SELECT COALESCE(sum(byte_size),0) AS s FROM media"
                                 " WHERE user_id = ? AND id = ANY(?)")
                               user-id (int-array ids)]
                              {:builder-fn rs/as-unqualified-maps})) 0)]
          (jdbc/execute! tx ["DELETE FROM media WHERE user_id = ? AND id = ANY(?)"
                             user-id (int-array ids)])
          (jdbc/execute! tx ["UPDATE users SET usage_bytes = GREATEST(0, usage_bytes - ?)
                              WHERE id = ?" (long freed) user-id])))
      (catch Exception e
        (tel/error! {:id ::media-compensation-failed :data {:user-id user-id}} e)))))

;; ── Phase 1: metadata ──────────────────────────────────────────────

(def ^:private jsonb-html-columns
  "JSONB columns whose payload holds HTML with media URLs (§5.4.7).
   `io_fields` is a map of six HTML fields; `overlapping` holds `:items` of HTML
   and a materialized `:fields` map."
  ;; `poem_packages.notes` holds HTML and is deliberately absent (poem-reader-spec
  ;; row 2.3.4). Poem notes are quoted from Shamela pages and carry no
  ;; `/api/media/` URL — the package format forbids one (format doc V7) and the
  ;; importer rejects a package that breaks it, so there is no URL here to rewrite.
  {:flashcards [:io_fields :overlapping]
   :card_versions [:io_fields :overlapping]})

(defn- topic-depth
  "Depth of `id` in the ARCHIVE's tree, by walking `parent-of`.
   Pre : `parent-of` maps a source topic id to its source parent id.
   Post: 0 for a root. A cycle terminates at the guard and reports as the tree's
         size, which sorts it last — an import must not hang on a malformed
         archive."
  [parent-of id limit]
  (loop [cur id d 0]
    (let [p (get parent-of cur)]
      (cond (nil? p) d
            (>= d limit) limit
            :else (recur p (inc d))))))

(defn- ordered-topic-rows
  "Archive topic rows sorted parent-before-child.

   Id order is NOT safe: `topic_move` can reparent a topic under a NEWER parent,
   so a child's id may be smaller than its parent's. Depth order is."
  [rows]
  (let [parent-of (into {} (map (juxt :id :parent_id)) rows)
        n (count rows)]
    (sort-by #(topic-depth parent-of (:id %) n) rows)))

(defn- remap-settings-key
  "Translate a settings key whose suffix is a topic id (§5.4.8).
   Post: the rewritten key, or nil when the embedded topic did not import — the
         row is then dropped, because a preference pointing at another topic is
         worse than an absent preference."
  [k topic-map]
  (if-let [prefix (some #(when (str/starts-with? k %) %) settings-key-prefixes)]
    (let [old (parse-long (subs k (count prefix)))]
      (when-let [new-id (get topic-map old)]
        (str prefix new-id)))
    k))

(defn- remap-settings-row
  "Translate one settings row. `last_document`'s VALUE is a topic id."
  [row topic-map]
  (let [k (remap-settings-key (:key row) topic-map)]
    (cond
      (nil? k) {:drop (str "settings key " (:key row) " → topic not imported")}
      (= k "last_document")
      (if-let [new-id (get topic-map (parse-long (str (:value row))))]
        {:row (assoc row :key k :value (str new-id))}
        {:drop "last_document → topic not imported"})
      :else {:row (assoc row :key k)})))

(defn- table-specific-remap
  "The remaps unique to one table, applied after `remap-row`.
   Post: {:row M} or {:drop reason}."
  [tx table row maps media-map]
  (let [with-jsonb-html
        (reduce (fn [m col]
                  (if (contains? m col)
                    (assoc m col (rewrite-value-html (get m col) media-map))
                    m))
          row (get jsonb-html-columns table []))]
    (case table
      ;; A fork arrives as a root. Chats import before messages, so its fork
      ;; message has no id here yet, and the pair CHECK refuses a parent
      ;; without one. `link-imported-forks!` sets both after the messages.
      :assistant_chats
      {:row (-> with-jsonb-html
              (assoc :provider_session_id (str (UUID/randomUUID)))
              (dissoc :parent_chat_id :fork_message_id))}
      :assistant_runs
      (let [session-id (:provider_session_id
                         (jdbc/execute-one! tx
                           ["SELECT provider_session_id FROM assistant_chats WHERE id = ?"
                            (:chat_id with-jsonb-html)]
                           {:builder-fn rs/as-unqualified-maps}))
            interrupted? (#{"queued" "streaming" "stop-requested"}
                           (:status with-jsonb-html))
            remapped (cond-> (-> with-jsonb-html
                               (assoc :run_uuid (str (UUID/randomUUID))
                                      :retry_of_run_uuid nil
                                      :provider_session_id session-id)
                               (update :request_snapshot
                                 #(if (map? %) (assoc % :session_id session-id) %)))
                       interrupted?
                       (assoc :status "failed"
                              :error_text "Interrupted during account export."
                              :failed_at (str (java.time.LocalDateTime/now))))]
        (when (and interrupted? (:assistant_message_id remapped))
          (jdbc/execute! tx
            ["UPDATE assistant_messages SET content = ? WHERE id = ?"
             (:response_text remapped) (:assistant_message_id remapped)]))
        {:row remapped})
      :card_versions (remap-card-versions-scope with-jsonb-html maps)
      :score_groups {:row (update with-jsonb-html :geometry
                            #(remap-score-geometry % media-map))}
      :curriculum_exam_forms
      {:row
       (reduce
         (fn [form-row column]
           (if-let [value (get form-row column)]
             (assoc form-row column
               (walk/postwalk
                 (fn [node]
                   (if (map? node)
                     (cond-> node
                       (contains? node :material_id)
                       (update :material_id #(get-in maps [:curriculum_materials %]))
                       (contains? node :topic_id)
                       (update :topic_id #(get-in maps [:topics %])))
                     node))
                 value))
             form-row))
         with-jsonb-html [:blueprint :questions])}
      :settings (remap-settings-row with-jsonb-html (get maps :topics))
      {:row with-jsonb-html})))

(defn- link-imported-forks!
  "Make each imported fork a fork again (plans/assistant-chat-forks.md
   §4.1.4). `table-specific-remap` inserted every chat as a root, because a
   chat's fork message is imported after it.
   Pre:  `archive-chats` are the archive's assistant_chats rows. `maps` holds
         the :assistant_chats and :assistant_messages id maps of this import.
   Post: a chat whose parent and fork message both imported has both set. Any
         other chat stays a root, with its own transcript intact."
  [tx archive-chats maps]
  (doseq [{:keys [id parent_chat_id fork_message_id]} archive-chats
          :when (and parent_chat_id fork_message_id)
          :let [chat-id (get-in maps [:assistant_chats id])
                parent-id (get-in maps [:assistant_chats parent_chat_id])
                message-id (get-in maps [:assistant_messages fork_message_id])]
          :when (and chat-id parent-id message-id)]
    (jdbc/execute! tx ["UPDATE assistant_chats SET parent_chat_id = ?, fork_message_id = ?
                         WHERE id = ?"
                       parent-id message-id chat-id])))

(def ^:private phase-1-tables
  "Tables phase 1 inserts. `media` landed in phase 0, and the two blob tables
   cannot exist before their bytes (see the namespace docstring)."
  (into [] (remove #{:media :topic_files :topic_videos :topic_epubs}) fmt/entity-tables))

(defn- import-table!
  "Insert one table's rows inside the metadata transaction.
   Post: {:map M :inserted N :dropped v} — `:map` is source id to new id for
         this table, empty for a table with no `id` column."
  [tx table rows columns maps media-map user-id parent-topic-id]
  (let [returning? (contains? columns :id)
        [lookup-sql & lookup-cols] (get natural-key-lookup table)
        ;; Keys the archive carries that this instance has no column for.
        ;; Computed once from the first row, not per row: every row of a table
        ;; comes from one `to_jsonb`, so they all carry the same keys.
        ;; Reported rather than swallowed — an archive from a newer build loses
        ;; these columns, and this is the only signal the user gets.
        unknown-columns (->> (keys (first rows))
                          (remove #(contains? columns %))
                          (remove (get dropped-columns table #{:id}))
                          sort vec)
        result
        (reduce
      (fn [acc row]
        (let [base (apply dissoc row (get dropped-columns table #{:id}))
              r1 (remap-row table base {:maps (assoc maps table (:map acc))
                                        :user-id user-id
                                        :media-map media-map
                                        :default-parent-id parent-topic-id})]
          (if (:drop r1)
            (update acc :dropped conj {:table table :id (:id row) :reason (:drop r1)})
            (let [r2 (table-specific-remap tx table (:row r1)
                       (assoc maps table (:map acc)) media-map)]
              (if (:drop r2)
                (update acc :dropped conj {:table table :id (:id row) :reason (:drop r2)})
                (let [merged-id (when lookup-sql
                                  (:id (jdbc/execute-one! tx
                                         (into [lookup-sql user-id]
                                           (map #(get (:row r2) %)) lookup-cols)
                                         {:builder-fn rs/as-unqualified-maps})))]
                  (if merged-id
                    (cond-> (update acc :merged inc)
                      (:id row) (assoc-in [:map (:id row)] merged-id))
                    (let [new-id (insert-row! tx table (:row r2) columns returning?)]
                      (cond-> (update acc :inserted inc)
                        (and returning? new-id (:id row))
                        (assoc-in [:map (:id row)] new-id))))))))))
          {:map {} :inserted 0 :merged 0 :dropped []}
          rows)]
    (cond-> result
      (seq unknown-columns) (assoc :unknown-columns unknown-columns))))

;; ── Phase 2: blobs (§5.5.3, §5.5.5) ────────────────────────────────

(defn- topic-file-gaps
  "Archive `topic_files` entries with no row on the target.

   Gap detection is a left join on the unique `(topic_id, role)` index
   (db.clj:261) and reads no `file_data` — §5.5.5. A missing blob IS a missing
   row, which is why phase 2 creates the row and its bytes together."
  [ds ^File dir topic-map]
  (keep (fn [row]
          (when-let [new-topic (get topic-map (:topic_id row))]
            (let [role (or (:role row) "main")]
              (when-not (jdbc/execute-one! ds
                          ["SELECT 1 FROM topic_files WHERE topic_id = ? AND role = ?"
                           new-topic role])
                (assoc row ::new-topic-id new-topic ::role role
                  ::blob (io/file dir (fmt/blob-path :topic_files (:topic_id row) role)))))))
    (entity-rows dir :topic_files)))

(defn- video-gaps
  "Archive `topic_videos` entries with no row on the target.
   `topic_videos` is keyed on `topic_id`, so the same left-join rule applies."
  [ds ^File dir topic-map]
  (keep (fn [row]
          (when-let [new-topic (get topic-map (:topic_id row))]
            (when-not (jdbc/execute-one! ds
                        ["SELECT 1 FROM topic_videos WHERE topic_id = ?" new-topic])
              (assoc row ::new-topic-id new-topic
                ::blob (io/file dir (fmt/blob-path :topic_videos (:topic_id row) nil))))))
    (entity-rows dir :topic_videos)))

(defn- epub-gaps
  "Archive `topic_epubs` entries with no row on the target.
   `topic_epubs` is keyed on `topic_id`, so the same left-join rule applies."
  [ds ^File dir topic-map]
  (keep (fn [row]
          (when-let [new-topic (get topic-map (:topic_id row))]
            (when-not (jdbc/execute-one! ds
                        ["SELECT 1 FROM topic_epubs WHERE topic_id = ?" new-topic])
              (assoc row ::new-topic-id new-topic
                ::blob (io/file dir (fmt/blob-path :topic_epubs (:topic_id row) nil))))))
    (entity-rows dir :topic_epubs)))

(defn- import-topic-file!
  "Insert one `topic_files` row with its bytes, in one transaction.
   Pre : `::blob` exists. Post: the row and its bytes are committed and
         `usage_bytes` includes them (§5.6.7).
   Invariant: heap high-water is this one value — `bytea` has no range read."
  [ds user-id gap]
  (let [^File blob (::blob gap)
        size (.length blob)]
    (jdbc/with-transaction [tx ds]
      (quota/reserve-bytes! tx user-id size)
      (jdbc/execute-one! tx
        ["INSERT INTO topic_files (topic_id, file_data, file_size, mime_type, role)
          VALUES (?, ?, ?, ?, ?)"
         (::new-topic-id gap)
         (java.nio.file.Files/readAllBytes (.toPath blob))
         size (:mime_type gap) (::role gap)]))))

(defn- import-video!
  "Create one large object from a ZIP entry and insert its `topic_videos` row.
   Pre : `::blob` exists. Post: both committed together, so a `lo_oid` never
         names an object no row owns and `usage_bytes` includes `byte_size`.
   Invariant: the bytes stream from the file — never through heap."
  [ds user-id gap]
  (let [^File blob (::blob gap)
        size (.length blob)]
    (jdbc/with-transaction [tx ds]
      (quota/reserve-bytes! tx user-id size)
      (let [oid (lo/create! tx)]
        (lo/write-file! tx oid blob)
        (jdbc/execute-one! tx
          ["INSERT INTO topic_videos
            (topic_id, lo_oid, byte_size, mime_type, duration_ms, last_pos_ms, remux_pending)
            VALUES (?, ?, ?, ?, ?, ?, false)"
           (::new-topic-id gap) oid size
           (or (:mime_type gap) "video/mp4") (:duration_ms gap)
           (or (:last_pos_ms gap) 0)])))))

(defn- import-epub!
  "Create one large object from a ZIP entry and insert its `topic_epubs` row.
   Pre : `::blob` exists. Post: both committed together, so a `lo_oid` never
         names an object no row owns and `usage_bytes` includes `byte_size`.
   Invariant: the bytes stream from the file — never through heap, which is the
         whole reason an EPUB is stored this way."
  [ds user-id gap]
  (let [^File blob (::blob gap)
        size (.length blob)]
    (jdbc/with-transaction [tx ds]
      (quota/reserve-bytes! tx user-id size)
      (let [oid (lo/create! tx)]
        (lo/write-file! tx oid blob)
        (jdbc/execute-one! tx
          ["INSERT INTO topic_epubs (topic_id, lo_oid, byte_size) VALUES (?, ?, ?)"
           (::new-topic-id gap) oid size])))))

(defn- run-phase-2!
  "Write every missing blob. Returns what it wrote and what it could not.
   Post: {:topic_files N :topic_videos N :topic_epubs N :failed v :orphaned v}. `:orphaned`
         names archive blobs whose topic did not import, `:failed` those whose
         write threw — a quota refusal is the expected member.
   Invariant: one blob per transaction, so an interruption leaves every blob
         already written in place and the rest still detectable as gaps."
  [ds user-id ^File dir topic-map]
  (let [files (topic-file-gaps ds dir topic-map)
        videos (video-gaps ds dir topic-map)
        epubs (epub-gaps ds dir topic-map)
        write! (fn [acc kind gap import!]
                 (cond
                   (not (.exists ^File (::blob gap)))
                   (update acc :orphaned conj {kind (:topic_id gap) :reason "blob entry absent"})
                   :else
                   (try (import! ds user-id gap)
                     (update acc kind inc)
                     (catch Exception e
                       (update acc :failed conj
                         {kind (:topic_id gap) :reason (.getMessage e)})))))]
    (as-> {:topic_files 0 :topic_videos 0 :topic_epubs 0 :failed [] :orphaned []} acc
      (reduce #(write! %1 :topic_files %2 import-topic-file!) acc files)
      (reduce #(write! %1 :topic_videos %2 import-video!) acc videos)
      (reduce #(write! %1 :topic_epubs %2 import-epub!) acc epubs))))

;; ── Orchestration ──────────────────────────────────────────────────

(defn- resumable?
  "Whether `run` finished its metadata phase but not its blobs."
  [run]
  (and run (some? (:metadata_done_at run)) (nil? (:blobs_done_at run))))

(defn- complete?
  [run]
  (and run (some? (:blobs_done_at run))))

(defn- stamp-report!
  [ds import-id report]
  (jdbc/execute-one! ds
    ["UPDATE account_imports SET report = ? WHERE id = ?" (->jsonb report) import-id]))

(defn- import-archive!*
  "Import an extracted account archive for `user-id`.

   Pre  : `dir` holds an archive `freememo.archive/extract-to-temp-dir!` produced.
          The caller owns `dir` and deletes it afterwards, on every path.
          `parent-topic-id` is a topic this user owns, or nil for the tree root.
          `user-id` is the authenticated user — nothing in the archive names who
          imports it.
   Post : {:ok true :import-id N :resumed? bool :report M} when the run reached
          the end of phase 2, or {:ok false :error S} when it did not.
          On a phase-1 failure NOTHING persists: the transaction rolls back and
          phase 0's media rows are deleted and their bytes refunded, so a failed
          import costs the user no storage.
   Invariant: a source id never reaches a stored column. Every id is translated,
          nulled, or its row is dropped and named in `:report`.
   Blame  : {:ok false} with an :error naming a manifest defect is the archive's
          fault. A quota refusal appears in `:report :blobs :failed`, because the
          metadata is legitimately imported by then."
  [ds user-id ^File dir parent-topic-id {:keys [allow-second-copy?]}]
  (let [m (read-manifest dir)]
    (if-not (:ok m)
      m
      (let [manifest (:manifest m)
            existing (find-import ds user-id (:export-id manifest))]
        (cond
          (and (complete? existing) (not allow-second-copy?))
          {:ok false :already-imported? true
           :error (str "This export was already imported on "
                    (:blobs_done_at existing)
                    ". Import it again only if you want a second copy.")}

          ;; §5.5.4 — resume: phase 1 is atomic, so only blobs can be missing.
          (and (resumable? existing) (not allow-second-copy?))
          (let [topic-map (load-ids ds (:id existing) :topics)
                blobs (run-phase-2! ds user-id dir topic-map)
                done? (and (empty? (:failed blobs)) (empty? (:orphaned blobs)))]
            (when done?
              (jdbc/execute-one! ds ["UPDATE account_imports SET blobs_done_at = now()
                                      WHERE id = ?" (:id existing)]))
            (stamp-report! ds (:id existing) {:blobs blobs :resumed true})
            {:ok true :import-id (:id existing) :resumed? true
             :report {:blobs blobs}})

          :else
          (let [media (import-media! ds user-id dir)]
            (try
              (let [{:keys [import-id maps dropped inserted]}
                    (jdbc/with-transaction [tx ds]
                      (let [import-id (create-import! tx user-id manifest parent-topic-id)
                            columns (into {} (map (fn [t] [t (table-columns tx t)]))
                                      fmt/entity-tables)]
                        (loop [tables phase-1-tables
                               maps {:media (:media-map media)}
                               dropped []
                               inserted {}]
                          (if-let [table (first tables)]
                            (let [rows (cond->> (entity-rows dir table)
                                         (= table :topics) ordered-topic-rows)
                                  r (import-table! tx table rows (get columns table)
                                      maps (:media-map media) user-id parent-topic-id)]
                              (recur (rest tables)
                                (assoc maps table (:map r))
                                (into dropped (:dropped r))
                                (assoc inserted table
                                  (cond-> {:inserted (:inserted r)}
                                    (pos? (:merged r)) (assoc :merged (:merged r))
                                    ;; A column this instance does not have. The
                                    ;; archive came from a build with a wider
                                    ;; schema, and that data is lost — silently,
                                    ;; unless it is said here.
                                    (seq (:unknown-columns r))
                                    (assoc :unknown-columns (:unknown-columns r))))))
                            (do
                              (link-imported-forks! tx (entity-rows dir :assistant_chats) maps)
                              (doseq [space id-bearing-tables]
                                (record-ids! tx import-id space
                                  (if (= space :media) (:media-map media) (get maps space))))
                              (jdbc/execute-one! tx
                                ["UPDATE account_imports SET metadata_done_at = now()
                                  WHERE id = ?" import-id])
                              {:import-id import-id :maps maps
                               :dropped dropped :inserted inserted})))))
                    blobs (run-phase-2! ds user-id dir (get maps :topics))
                    report {:inserted inserted :dropped dropped :blobs blobs
                            :media {:created (count (:created-ids media))
                                    :deduped (- (count (:media-map media))
                                               (count (:created-ids media)))
                                    :skipped (:skipped media)}}]
                (when (and (empty? (:failed blobs)) (empty? (:orphaned blobs)))
                  (jdbc/execute-one! ds ["UPDATE account_imports SET blobs_done_at = now()
                                          WHERE id = ?" import-id]))
                (stamp-report! ds import-id report)
                ;; A schema-width mismatch is the one report line that means data
                ;; was lost rather than skipped, so it gets its own level.
                (when-let [wider (seq (keep (fn [[t v]]
                                              (when (:unknown-columns v)
                                                [t (:unknown-columns v)]))
                                        inserted))]
                  (tel/log! {:level :warn :id ::archive-schema-wider-than-target
                             :data {:user-id user-id :import-id import-id
                                    :columns (into {} wider)}}
                    "The archive carries columns this instance has no place for"))
                (tel/log! {:level :info :id ::account-imported
                           :data {:user-id user-id :import-id import-id
                                  :export-id (:export-id manifest)
                                  :rows (reduce + (map :inserted (vals inserted)))
                                  :dropped (count dropped) :blobs blobs}}
                  "Account import finished")
                {:ok true :import-id import-id :resumed? false :report report})
              (catch Exception e
                (compensate-media! ds user-id (:created-ids media))
                (tel/error! {:id ::account-import-failed
                             :data {:user-id user-id :export-id (:export-id manifest)}} e)
                {:ok false :error (str "The import failed and nothing was kept: "
                                    (.getMessage e))}))))))))

(defn import-archive!
  "`import-archive!*`, published in the Activity monitor for its duration.

   The membership lives here rather than at `web-import`'s two call sites: both
   would have to remember it, and a third caller would silently report nothing.
   `with-in-flight` supplies the `finally`, so a throw mid-import still closes
   the activity instead of leaving it to time out as interrupted.

   A thin delegate rather than a wrapper around the body, deliberately — the
   body is 100 lines of nested transaction handling, and re-indenting it to add
   one enclosing form is a large diff over code this change does not touch."
  [ds user-id ^File dir parent-topic-id opts]
  (us/with-in-flight user-id :importing-accounts "account"
    (import-archive!* ds user-id dir parent-topic-id opts)))
