(ns freememo.curriculum
  "Personal curriculum persistence, progression, and schedule derivation.

   This namespace owns the domain boundary. All public mutations authorize the
   curriculum through user-id. Only a passing block exam completes a block.
   Unlock overrides change access, not completion."
  (:require
   [cheshire.core :as json]
   [clojure.string :as str]
   [freememo.commands :as commands]
   [freememo.db :as db]
   [freememo.toasts :as toasts]
   [freememo.user-state :as us]
   [next.jdbc :as jdbc]
   [next.jdbc.result-set :as rs]
   [taoensso.telemere :as tel])
  (:import
   [java.security MessageDigest]
   [java.time LocalDate ZoneId]))

(def ^:private opts {:builder-fn rs/as-unqualified-maps})
(def default-threshold 70)
(def default-daily-minutes 60)
(def default-material-exam-minutes 45)
(def default-block-exam-minutes 60)

(defn- bump! [user-id]
  (commands/bump! user-id :curriculum-change))

(defn- pg-json [value]
  (doto (org.postgresql.util.PGobject.)
    (.setType "jsonb")
    (.setValue (json/generate-string value))))

(defn- read-json [value]
  (cond
    (nil? value) nil
    (string? value) (json/parse-string value true)
    (instance? org.postgresql.util.PGobject value)
    (json/parse-string (.getValue ^org.postgresql.util.PGobject value) true)
    :else value))

(defn- study-weekdays->vec [study-weekdays]
  (let [values (if (instance? java.sql.Array study-weekdays)
                 (.getArray ^java.sql.Array study-weekdays)
                 study-weekdays)]
    (mapv int (or values []))))

(defn- curriculum-row->map
  "Convert database-specific schedule controls to plain Electric values."
  [row]
  (some-> row
    (update :study_weekdays study-weekdays->vec)
    (update :target_date #(some-> % str))))

(defn- own-curriculum [connectable user-id curriculum-id]
  (curriculum-row->map
    (jdbc/execute-one! connectable
      ["SELECT * FROM curricula
         WHERE id = ? AND user_id = ? AND staged_delete_id IS NULL"
       curriculum-id user-id]
      opts)))

(defn curriculum-title
  "The title of one curriculum the user owns, or nil.

   Pre: curriculum-id is an integer.
   Post: nil for a missing id, another user's curriculum, and one staged for
   deletion — the scope `own-curriculum` checks, without its other columns."
  [user-id curriculum-id]
  (:title
    (jdbc/execute-one! db/ds
      ["SELECT title FROM curricula
         WHERE id = ? AND user_id = ? AND staged_delete_id IS NULL"
       curriculum-id user-id]
      opts)))

(defn- own-block [connectable user-id block-id]
  (jdbc/execute-one! connectable
    ["SELECT b.* FROM curriculum_blocks b
       JOIN curricula c ON c.id = b.curriculum_id
      WHERE b.id = ? AND c.user_id = ? AND c.staged_delete_id IS NULL" block-id user-id]
    opts))

(defn- own-material [connectable user-id material-id]
  (jdbc/execute-one! connectable
    ["SELECT m.*, b.curriculum_id FROM curriculum_materials m
       JOIN curriculum_blocks b ON b.id = m.block_id
       JOIN curricula c ON c.id = b.curriculum_id
      WHERE m.id = ? AND c.user_id = ? AND c.staged_delete_id IS NULL" material-id user-id]
    opts))

(defn- own-form [connectable user-id form-id]
  (some-> (jdbc/execute-one! connectable
            ["SELECT f.* FROM curriculum_exam_forms f
               LEFT JOIN curriculum_materials m ON m.id = f.material_id
               JOIN curriculum_blocks b ON b.id = COALESCE(f.block_id, m.block_id)
               JOIN curricula c ON c.id = b.curriculum_id
              WHERE f.id = ? AND f.user_id = ? AND c.staged_delete_id IS NULL"
             form-id user-id]
            opts)
    (update :blueprint read-json)
    (update :questions read-json)
    (update :source_versions read-json)
    (update :accepted_source_versions read-json)))

(defn- require-owned! [row entity id]
  (or row
    (throw (ex-info (str (str/capitalize (name entity)) " not found")
             {:type ::not-found :entity entity :id id}))))

(defn list-curricula
  "Return the user's curricula with progress counts."
  [_refresh user-id]
  (mapv curriculum-row->map
    (jdbc/execute! db/ds
      ["SELECT c.*,
               count(b.id)::int AS block_count,
               count(b.completed_at)::int AS completed_block_count
          FROM curricula c
          LEFT JOIN curriculum_blocks b ON b.curriculum_id = c.id
         WHERE c.user_id = ? AND c.staged_delete_id IS NULL
         GROUP BY c.id
         ORDER BY c.updated_at DESC" user-id]
      opts)))

(defn create-curriculum!
  "Create a personal curriculum.
   Pre: title is not blank, and at least one schedule control is present.
   Post: returns its id."
  [user-id {:keys [title target-date daily-minutes study-weekdays]}]
  (let [title (str/trim (str title))
        weekdays (vec (sort (distinct (map int study-weekdays))))]
    (when (str/blank? title)
      (throw (ex-info "Enter a curriculum name." {:type ::invalid-curriculum})))
    (when-not (or (some? target-date) (some? daily-minutes) (seq weekdays))
      (throw (ex-info "Set a target date, daily study time, or study days."
               {:type ::invalid-curriculum})))
    (let [row (jdbc/execute-one! db/ds
                ["INSERT INTO curricula
                    (user_id, title, target_date, daily_minutes, study_weekdays)
                  VALUES (?, ?, ?::date, ?, ?::smallint[])
                  RETURNING id"
                 user-id title target-date daily-minutes
                 (into-array Short (map short weekdays))]
                opts)]
      (bump! user-id)
      (:id row))))

(defn update-curriculum!
  "Update title and schedule controls after ownership validation."
  [user-id curriculum-id {:keys [title target-date daily-minutes study-weekdays]}]
  (require-owned! (own-curriculum db/ds user-id curriculum-id) :curriculum curriculum-id)
  (let [title (str/trim (str title))
        weekdays (vec (sort (distinct (map int study-weekdays))))]
    (when (str/blank? title)
      (throw (ex-info "Enter a curriculum name." {:type ::invalid-curriculum})))
    (when-not (or (some? target-date) (some? daily-minutes) (seq weekdays))
      (throw (ex-info "Set a target date, daily study time, or study days."
               {:type ::invalid-curriculum})))
    (jdbc/execute-one! db/ds
      ["UPDATE curricula
          SET title = ?, target_date = ?::date, daily_minutes = ?,
              study_weekdays = ?::smallint[], updated_at = now()
        WHERE id = ? AND user_id = ?"
       title target-date daily-minutes
       (into-array Short (map short weekdays)) curriculum-id user-id])
    (bump! user-id)))

(defn- curriculum-form-work-keys
  "Return every running-form key the curriculum can own.

   The keys match curriculum-exam/form-generation-work-key. This namespace
   must not require that namespace because it already depends on this one."
  [connectable curriculum-id]
  (let [blocks (jdbc/execute! connectable
                 ["SELECT id FROM curriculum_blocks WHERE curriculum_id = ?"
                  curriculum-id] opts)
        materials (jdbc/execute! connectable
                    ["SELECT m.id FROM curriculum_materials m
                        JOIN curriculum_blocks b ON b.id = m.block_id
                       WHERE b.curriculum_id = ?" curriculum-id] opts)]
    (into #{}
      (concat (map #(str "block/" (:id %)) blocks)
              (map #(str "material/" (:id %)) materials)))))

(defn- cancel-curriculum-form-generations!
  "Ask every running form generation for curriculum-id not to commit.

   The canceller is an interruption attempt. The cancelled set and the durable
   deletion entry are the commit guards, so an HTTP call that ignores interrupt
   cannot write a late form."
  [user-id work-keys]
  (let [cancelled (us/get-atom user-id :exam-form-generation-cancelled)
        cancellers (us/get-atom user-id :exam-form-generation-cancellers)]
    (swap! cancelled into work-keys)
    (doseq [work-key work-keys]
      (when-some [cancel (get @cancellers work-key)]
        (cancel)))))

(defn stage-curriculum-for-deletion!
  "Hide a curriculum for the 12-hour recovery window.

   Pre: curriculum-id names a live curriculum owned by user-id.
   Post: one undo entry and one staged root marker commit atomically; blocks,
   materials, forms, sittings, revisions, overrides, source topics, and credit
   ledger rows stay unchanged.
   Invariant: only the marker hides this graph. The hourly purge owns the later
   physical cascade, and an entry cannot exist without its marked root."
  [user-id curriculum-id]
  (let [{:keys [entry-id title work-keys] :as staged}
        (jdbc/with-transaction [tx db/ds]
          (let [curriculum (require-owned!
                             (jdbc/execute-one! tx
                               ["SELECT title FROM curricula
                                  WHERE id = ? AND user_id = ?
                                    AND staged_delete_id IS NULL FOR UPDATE"
                                curriculum-id user-id] opts)
                             :curriculum curriculum-id)
                entry-id (db/insert-undo-entry-raw!
                           tx user-id "delete-curriculum" "curriculum" [curriculum-id]
                           {:title (:title curriculum)})
                staged-row (jdbc/execute-one! tx
                             ["UPDATE curricula
                                  SET staged_delete_id = ?, updated_at = now()
                                WHERE id = ? AND user_id = ?
                                  AND staged_delete_id IS NULL
                              RETURNING title"
                              entry-id curriculum-id user-id] opts)]
            (require-owned! staged-row :curriculum curriculum-id)
            (db/prune-undo-log! tx user-id)
            {:entry-id entry-id
             :title (:title staged-row)
             :work-keys (curriculum-form-work-keys tx curriculum-id)}))]
    ;; This is after the database commit: a failed stage must never cancel work.
    (cancel-curriculum-form-generations! user-id work-keys)
    (commands/bump-channels! user-id
      #{:curriculum-mutations :schedule-mutations :undo-mutations})
    (toasts/push! user-id
      {:level :success
       :message (str "Deleted \"" title "\". You can undo this for 12 hours.")
       :dedup? false
       :actions [{:label "Undo" :undo-id entry-id}]})
    staged))

(defn restore-staged-curriculum!
  "Clear one owned staged marker.

   Pre: entry is a live delete-curriculum entry for user-id.
   Post: the curriculum graph is visible without rewriting retained rows.
   Invariant: restoration clears only the matching entry marker, so a newer
   deletion cannot be undone by an older action."
  [user-id entry]
  (let [curriculum-id (first (:entity_refs entry))]
    (jdbc/with-transaction [tx db/ds]
      (let [row (jdbc/execute-one! tx
                  ["UPDATE curricula
                      SET staged_delete_id = NULL, updated_at = now()
                    WHERE id = ? AND user_id = ? AND staged_delete_id = ?
                  RETURNING id, title"
                   curriculum-id user-id (:id entry)] opts)]
        (require-owned! row :curriculum curriculum-id)
        row))))

(defn- resequence!
  "Write `ids` as positions 0, 1, 2 … under one parent.

   Pre: `ids` is exactly the parent's current child ids. The caller checks that.
   Post: every child holds its index in `ids`.
   Invariant: no statement writes a negative position, and no two children of
   one parent hold the same position at any point inside the transaction.

   The two passes exist because `UNIQUE (parent, position)` rejects a direct
   swap. The park range starts ABOVE the highest position in use, because
   `CHECK (position >= 0)` rejects a negative one. Parking below zero is what
   this used to do, so every reorder raised
   `curriculum_materials_position_check` and rolled back — a button that
   looked dead."
  [tx table parent-column parent-id ids]
  (let [park (inc (or (:max (jdbc/execute-one! tx
                              [(str "SELECT max(position) AS max FROM " table
                                 " WHERE " parent-column " = ?") parent-id]
                              opts))
                    0))]
    (doseq [[position id] (map-indexed vector ids)]
      (jdbc/execute-one! tx
        [(str "UPDATE " table " SET position = ? WHERE id = ? AND " parent-column " = ?")
         (+ park position) id parent-id]))
    (doseq [[position id] (map-indexed vector ids)]
      (jdbc/execute-one! tx
        [(str "UPDATE " table " SET position = ? WHERE id = ? AND " parent-column " = ?")
         position id parent-id]))))

(defn add-block!
  "Append a block. The first block starts unlocked and focused."
  [user-id curriculum-id title]
  (require-owned! (own-curriculum db/ds user-id curriculum-id) :curriculum curriculum-id)
  (let [title (str/trim (str title))]
    (when (str/blank? title)
      (throw (ex-info "Enter a block name." {:type ::invalid-block})))
    (let [block-id
          (jdbc/with-transaction [tx db/ds]
            (let [{:keys [position]} (jdbc/execute-one! tx
                                       ["SELECT count(*)::int AS position
                                           FROM curriculum_blocks WHERE curriculum_id = ?"
                                        curriculum-id] opts)
                  first? (zero? position)
                  row (jdbc/execute-one! tx
                        ["INSERT INTO curriculum_blocks
                            (curriculum_id, position, title, unlocked_at, focused_at)
                          VALUES (?, ?, ?, CASE WHEN ? THEN now() END,
                                          CASE WHEN ? THEN now() END)
                          RETURNING id"
                         curriculum-id position title first? first?] opts)]
              (jdbc/execute-one! tx ["UPDATE curricula SET updated_at = now() WHERE id = ?"
                                     curriculum-id])
              (:id row)))]
      (bump! user-id)
      block-id)))

(defn rename-block!
  "Give one block a new name.

   Pre: `block-id` belongs to `user-id`, and `title` holds a character that is
   not white space.
   Post: the block holds the trimmed title, and the curriculum records the
   change time. The two writes share one transaction, so a reader never sees a
   renamed block under a stale curriculum time.
   Invariant: the block keeps its position, its unlock state and its
   materials. A name is the only thing this writes."
  [user-id block-id title]
  (let [title (str/trim (str title))]
    (when (str/blank? title)
      (throw (ex-info "Enter a block name." {:type ::invalid-block})))
    (jdbc/with-transaction [tx db/ds]
      (let [block (require-owned! (own-block tx user-id block-id) :block block-id)]
        (jdbc/execute-one! tx
          ["UPDATE curriculum_blocks SET title = ? WHERE id = ?" title block-id])
        (jdbc/execute-one! tx
          ["UPDATE curricula SET updated_at = now() WHERE id = ?" (:curriculum_id block)])))
    (bump! user-id)
    :renamed))

(defn- stepped
  "Move one id one step through a vector of ids.

   Pre: `ids` holds `id` at most once. `direction` is -1 or 1.
   Post: `id` and its neighbour exchange places. A step off either end, or an
   id the vector does not hold, returns `ids` unchanged — a stale click is
   nothing to do, not an error."
  [ids id direction]
  (let [index (.indexOf ^java.util.List ids id)
        target (+ index direction)]
    (if (or (neg? index) (neg? target) (>= target (count ids)))
      ids
      (assoc ids index (nth ids target) target id))))

(defn- ids-in-order [connectable table parent-column parent-id]
  (mapv :id (jdbc/execute! connectable
              [(str "SELECT id FROM " table " WHERE " parent-column " = ? ORDER BY position")
               parent-id] opts)))

(defn move-block!
  "Move one block one step through its curriculum.

   Pre: the user owns the block. `direction` is -1 (earlier) or 1 (later).
   Post: the block and its neighbour exchange positions, or nothing changes at
   an end.
   Invariant: the caller sends a block and a direction, NEVER a computed order.
   An order computed on the client is a function of the very list this write
   rewrites, so an Electric re-read restarts the call with the new order and
   steps the block back — an oscillation whose parity, not the click, decides
   where the block lands."
  [user-id block-id direction]
  (jdbc/with-transaction [tx db/ds]
    (let [block (require-owned! (own-block tx user-id block-id) :block block-id)
          curriculum-id (:curriculum_id block)
          ids (ids-in-order tx "curriculum_blocks" "curriculum_id" curriculum-id)]
      (resequence! tx "curriculum_blocks" "curriculum_id" curriculum-id
        (stepped ids block-id direction))
      (jdbc/execute-one! tx ["UPDATE curricula SET updated_at = now() WHERE id = ?"
                             curriculum-id])))
  (bump! user-id))

(defn documents-for-picker
  "Return supported personal source materials."
  [_refresh user-id]
  (jdbc/execute! db/ds
    ["SELECT id, title, kind FROM topics
       WHERE user_id = ? AND parent_id IS NULL AND staged_delete_id IS NULL
         AND kind IN ('pdf','markdown','web','wikipedia')
       ORDER BY lower(title)" user-id]
    opts))

(defn- estimate-material-minutes [tx topic-id]
  (let [{:keys [words]} (jdbc/execute-one! tx
                          ["WITH RECURSIVE tree AS (
                              SELECT id, content_text FROM topics WHERE id = ?
                              UNION ALL
                              SELECT t.id, t.content_text FROM topics t
                              JOIN tree p ON t.parent_id = p.id
                            )
                            SELECT COALESCE(sum(array_length(
                              regexp_split_to_array(trim(COALESCE(content_text,'')), '\\s+'), 1)), 0)::int
                              AS words FROM tree" topic-id] opts)]
    (max 15 (int (Math/ceil (/ (double words) 180.0))))))

(defn add-material!
  "Append a supported Library material to a block."
  [user-id block-id topic-id estimated-minutes]
  (let [material-id
        (jdbc/with-transaction [tx db/ds]
          (let [block (require-owned! (own-block tx user-id block-id) :block block-id)
                topic (jdbc/execute-one! tx
                        ["SELECT id, kind FROM topics
                          WHERE id = ? AND user_id = ? AND staged_delete_id IS NULL"
                         topic-id user-id] opts)]
            ;; `poem` is deliberately absent (poem-reader-spec row 2.4.2). A
            ;; curriculum material is read start to finish and marked studied;
            ;; a poem is read one verse at a time and has no such flow in v1.
            ;; Adding it here also needs a row in
            ;; `curriculum-page/material-kind-filters`.
            (when-not (contains? #{"pdf" "markdown" "web" "wikipedia"} (:kind topic))
              (throw (ex-info "Pick a PDF, Markdown document, or web article."
                       {:type ::unsupported-material})))
            (let [{:keys [position]} (jdbc/execute-one! tx
                                     ["SELECT count(*)::int AS position
                                        FROM curriculum_materials WHERE block_id = ?" block-id]
                                     opts)
                  minutes (or estimated-minutes (estimate-material-minutes tx topic-id))
                  row (jdbc/execute-one! tx
                        ["INSERT INTO curriculum_materials
                            (block_id, topic_id, position, estimated_minutes)
                          VALUES (?, ?, ?, ?) RETURNING id"
                         block-id topic-id position minutes] opts)]
              (jdbc/execute-one! tx ["UPDATE curricula SET updated_at = now() WHERE id = ?"
                                     (:curriculum_id block)])
              (:id row))))]
    (bump! user-id)
    material-id))

(defn move-material!
  "Move one material one step through its block.

   Pre: the user owns the material. `direction` is -1 (earlier) or 1 (later).
   Post: the material and its neighbour exchange positions, or nothing changes
   at an end.
   Invariant: as `move-block!` — a direction, never a computed order."
  [user-id material-id direction]
  (jdbc/with-transaction [tx db/ds]
    (let [material (require-owned! (own-material tx user-id material-id)
                     :material material-id)
          block-id (:block_id material)
          ids (ids-in-order tx "curriculum_materials" "block_id" block-id)]
      (resequence! tx "curriculum_materials" "block_id" block-id
        (stepped ids material-id direction))))
  (bump! user-id))

(defn mark-material-studied!
  "Record that the learner finished the source material."
  [user-id material-id]
  (require-owned! (own-material db/ds user-id material-id) :material material-id)
  (jdbc/execute-one! db/ds
    ["UPDATE curriculum_materials SET studied_at = COALESCE(studied_at, now())
      WHERE id = ?" material-id])
  (bump! user-id))

(defn update-material-estimate!
  "Replace the learner's material-duration estimate."
  [user-id material-id estimated-minutes]
  (require-owned! (own-material db/ds user-id material-id) :material material-id)
  (when-not (and (integer? estimated-minutes) (pos? estimated-minutes))
    (throw (ex-info "Estimated minutes must be positive."
             {:type ::invalid-estimate})))
  (jdbc/execute-one! db/ds
    ["UPDATE curriculum_materials SET estimated_minutes = ? WHERE id = ?"
     estimated-minutes material-id])
  (bump! user-id))

(defn focus-block!
  "Make one available block the schedule focus."
  [user-id block-id]
  (jdbc/with-transaction [tx db/ds]
    (let [{:keys [curriculum_id unlocked_at]}
          (require-owned! (own-block tx user-id block-id) :block block-id)]
      (when-not unlocked_at
        (throw (ex-info "Unlock this block before focusing it." {:type ::locked-block})))
      (jdbc/execute-one! tx
        ["UPDATE curriculum_blocks SET focused_at = NULL WHERE curriculum_id = ?"
         curriculum_id])
      (jdbc/execute-one! tx
        ["UPDATE curriculum_blocks SET focused_at = now() WHERE id = ?" block-id])))
  (bump! user-id))

(defn unlock-next-block!
  "Unlock the next ordered block without completing the current block."
  [user-id block-id]
  (jdbc/with-transaction [tx db/ds]
    (let [{:keys [curriculum_id position]}
          (require-owned! (own-block tx user-id block-id) :block block-id)
          next-block (jdbc/execute-one! tx
                       ["SELECT id FROM curriculum_blocks
                         WHERE curriculum_id = ? AND position > ?
                         ORDER BY position LIMIT 1" curriculum_id position] opts)]
      (when next-block
        (jdbc/execute-one! tx
          ["INSERT INTO curriculum_unlock_overrides
              (curriculum_id, from_block_id, to_block_id)
            VALUES (?, ?, ?) ON CONFLICT DO NOTHING"
           curriculum_id block-id (:id next-block)])
        (jdbc/execute-one! tx
          ["UPDATE curriculum_blocks SET unlocked_at = COALESCE(unlocked_at, now())
            WHERE id = ?" (:id next-block)])
        (jdbc/execute-one! tx
          ["UPDATE curriculum_blocks SET focused_at = NULL WHERE curriculum_id = ?"
           curriculum_id])
        (jdbc/execute-one! tx
          ["UPDATE curriculum_blocks SET focused_at = now() WHERE id = ?"
           (:id next-block)]))))
  (bump! user-id))

(defn- sha256 [text]
  (let [digest (.digest (MessageDigest/getInstance "SHA-256")
                 (.getBytes (str text) "UTF-8"))]
    (apply str (map #(format "%02x" (bit-and % 0xff)) digest))))

(defn material-evidence
  "Return the current evidence and content digest for one owned material."
  [connectable user-id material-id]
  (let [{:keys [topic_id] :as material}
        (require-owned! (own-material connectable user-id material-id) :material material-id)
        rows (jdbc/execute! connectable
               ["WITH RECURSIVE tree AS (
                   SELECT id, parent_id, title, kind, page_number, content_text
                     FROM topics WHERE id = ? AND user_id = ?
                   UNION ALL
                   SELECT t.id, t.parent_id, t.title, t.kind, t.page_number, t.content_text
                     FROM topics t JOIN tree p ON t.parent_id = p.id
                    WHERE t.user_id = ? AND t.staged_delete_id IS NULL
                 )
                 SELECT * FROM tree ORDER BY page_number NULLS FIRST, id"
                topic_id user-id user-id] opts)
        evidence (mapv (fn [{:keys [id title kind page_number content_text]}]
                         {:topic-id id :title title :kind kind :page page_number
                          :text (or content_text "")}) rows)
        digest (sha256 (pr-str evidence))]
    {:material material :evidence evidence :digest digest}))

(defn subject-evidence
  "Return source-bounded evidence for a material or block exam."
  [connectable user-id {:keys [material-id block-id]}]
  (when (= (some? material-id) (some? block-id))
    (throw (ex-info "Choose exactly one exam subject." {:type ::invalid-subject})))
  (let [materials (if material-id
                    [(require-owned! (own-material connectable user-id material-id)
                       :material material-id)]
                    (let [block (require-owned! (own-block connectable user-id block-id)
                                  :block block-id)]
                      (jdbc/execute! connectable
                        ["SELECT m.*, b.curriculum_id FROM curriculum_materials m
                          JOIN curriculum_blocks b ON b.id = m.block_id
                         WHERE m.block_id = ? ORDER BY m.position" (:id block)] opts)))
        bundles (mapv #(material-evidence connectable user-id (:id %)) materials)]
    {:materials (mapv (fn [{:keys [material evidence digest]}]
                        {:material-id (:id material)
                         :topic-id (:topic_id material)
                         :digest digest
                         :evidence evidence}) bundles)
     :source-versions (mapv :digest bundles)}))

(defn- subject-column+id
  "Return the exam-form column and the subject id.
   Pre: subject names exactly one of material-id or block-id."
  [{:keys [material-id block-id]}]
  (when (= (some? material-id) (some? block-id))
    (throw (ex-info "Choose exactly one exam subject." {:type ::invalid-subject})))
  (if material-id ["material_id" material-id] ["block_id" block-id]))

(defn- curriculum-deleted-since?
  "True when the curriculum was staged after operation-started-at.

   This reads deletion history, not the current staged marker. An Undo can make
   a curriculum live again, but it must not authorize an older model result."
  [connectable user-id curriculum-id operation-started-at]
  (boolean (:deleted
             (jdbc/execute-one! connectable
               ["SELECT EXISTS (
                    SELECT 1 FROM undo_log
                     WHERE user_id = ?
                       AND action_type = 'delete-curriculum'
                       AND entity_refs @> ?::jsonb
                       AND occurred_at >= ?) AS deleted"
                user-id (json/generate-string [curriculum-id]) operation-started-at]
               opts))))

(defn- lock-subject-curriculum!
  "Lock a subject's parent curriculum for a form commit.

   The lock orders staging against the final insert. If staging won, the
   durable history check rejects the result; if this insert won, staging starts
   after the result and retains that completed work for recovery."
  [connectable user-id subject]
  (let [[_ subject-id] (subject-column+id subject)
        material-id (:material-id subject)]
    (jdbc/execute-one! connectable
      (if material-id
        ["SELECT c.id AS curriculum_id, c.staged_delete_id
            FROM curriculum_materials m
            JOIN curriculum_blocks b ON b.id = m.block_id
            JOIN curricula c ON c.id = b.curriculum_id
           WHERE m.id = ? AND c.user_id = ? FOR UPDATE OF c" subject-id user-id]
        ["SELECT c.id AS curriculum_id, c.staged_delete_id
            FROM curriculum_blocks b
            JOIN curricula c ON c.id = b.curriculum_id
           WHERE b.id = ? AND c.user_id = ? FOR UPDATE OF c" subject-id user-id])
      opts)))

(defn next-form-version [connectable user-id subject]
  (let [{:keys [material-id]} subject
        [column id] (subject-column+id subject)]
    (require-owned! (if material-id
                      (own-material connectable user-id id)
                      (own-block connectable user-id id))
      (if material-id :material :block) id)
    (inc (:version (jdbc/execute-one! connectable
                     [(str "SELECT COALESCE(max(version), 0)::int AS version
                              FROM curriculum_exam_forms WHERE " column " = ?") id]
                     opts)))))

(defn save-draft-form!
  "Persist one validated five-question draft.

   Returns nil when a deletion happened after operation-started-at. This keeps
   a late model result from landing even if Undo has since restored the graph."
  [user-id subject {:keys [form-kind threshold blueprint questions source-versions cost
                           operation-started-at]}]
  (when-not (= 5 (count questions))
    (throw (ex-info "An exam form must contain five questions."
             {:type ::invalid-form :question-count (count questions)})))
  (jdbc/with-transaction [tx db/ds]
    (let [curriculum (lock-subject-curriculum! tx user-id subject)]
      (when-not (and operation-started-at curriculum
                     (curriculum-deleted-since?
                       tx user-id (:curriculum_id curriculum) operation-started-at))
        (let [version (next-form-version tx user-id subject)
              row (jdbc/execute-one! tx
                    ["INSERT INTO curriculum_exam_forms
                        (user_id, block_id, material_id, version, form_kind,
                         threshold, blueprint, questions, source_versions,
                         generation_cost_credits)
                      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?) RETURNING id"
                     user-id (:block-id subject) (:material-id subject) version
                     (or form-kind "standard") (or threshold default-threshold)
                     (pg-json blueprint) (pg-json questions) (pg-json source-versions) cost]
                    opts)]
          (bump! user-id)
          (:id row))))))

(defn- questions-with-prompts
  "Return the stored questions carrying the reader's prompts, in order.

   Pre: prompts holds five strings, none of them blank.
   Post: rubric and evidence survive untouched, because only :prompt is
   replaced. The client therefore never sends a question map."
  [questions prompts]
  (when-not (= 5 (count prompts))
    (throw (ex-info "An exam form must contain five questions."
             {:type ::invalid-form :prompt-count (count prompts)})))
  (let [prompts (mapv #(str/trim (str %)) prompts)]
    (when (some str/blank? prompts)
      (throw (ex-info "Every question needs text." {:type ::invalid-form})))
    (mapv #(assoc %1 :prompt %2) questions prompts)))

(defn- moved
  "Return items with the entry at index swapped one step along direction."
  [items index direction]
  (let [items (vec items)
        target (+ index direction)]
    (if (or (neg? target) (>= target (count items)))
      items
      (assoc items index (nth items target) target (nth items index)))))

(defn- require-threshold! [threshold]
  (when-not (and (integer? threshold) (<= 1 threshold 100))
    (throw (ex-info "Threshold must be from 1 to 100."
             {:type ::invalid-threshold :threshold threshold}))))

(defn save-form-edits!
  "Persist edited prompts and a pass threshold.

   Pre: the user owns form-id. prompts holds five non-blank strings, in the
   order the reader sees them. The threshold is from 1 to 100.
   Post: a draft is written in place, and its own id comes back. An approved
   form yields a NEW draft version whose id comes back, and the approved
   version stays byte-for-byte as it was.
   Invariant: an approved form is never mutated. That is what lets a past
   sitting keep rendering the questions it was taken against."
  [user-id form-id {:keys [threshold prompts]}]
  (require-threshold! threshold)
  (let [id (jdbc/with-transaction [tx db/ds]
             (let [form (require-owned! (own-form tx user-id form-id) :exam-form form-id)
                   questions (questions-with-prompts (:questions form) prompts)]
               (if (= "draft" (:status form))
                 (do (jdbc/execute-one! tx
                       ["UPDATE curriculum_exam_forms
                           SET threshold = ?, questions = ? WHERE id = ?"
                        threshold (pg-json questions) form-id])
                     form-id)
                 (let [subject {:material-id (:material_id form)
                                :block-id (:block_id form)}
                       version (next-form-version tx user-id subject)]
                   (:id (jdbc/execute-one! tx
                          ["INSERT INTO curriculum_exam_forms
                              (user_id, block_id, material_id, version, form_kind,
                               threshold, blueprint, questions, source_versions,
                               accepted_source_versions)
                            VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?) RETURNING id"
                           user-id (:block_id form) (:material_id form) version
                           (:form_kind form) threshold
                           (pg-json (:blueprint form)) (pg-json questions)
                           (pg-json (:source_versions form))
                           (pg-json (:accepted_source_versions form))]
                          opts))))))]
    (bump! user-id)
    id))

(defn reorder-draft-question!
  "Write the live prompts, then move one question one step.

   Pre: the user owns a DRAFT form. prompts holds the five prompts the reader
   currently sees, in the current order.
   Post: the stored questions carry those prompts in the new order.
   Invariant: a move never reverts an unsaved prompt. The field keys are
   positional, so writing the prompts first is what keeps them with their own
   question across the swap."
  [user-id form-id index direction prompts]
  (jdbc/with-transaction [tx db/ds]
    (let [form (require-owned! (own-form tx user-id form-id) :exam-form form-id)]
      (when-not (= "draft" (:status form))
        (throw (ex-info "Approved exam forms are immutable." {:type ::approved-form})))
      (jdbc/execute-one! tx
        ["UPDATE curriculum_exam_forms SET questions = ? WHERE id = ?"
         (pg-json (moved (questions-with-prompts (:questions form) prompts)
                    index direction))
         form-id])))
  (bump! user-id))

(defn record-form-generation-charge!
  "Attach the actual completed-call charge to a persisted form."
  [user-id form-id charge]
  (try
    (jdbc/execute-one! db/ds
      ["UPDATE curriculum_exam_forms SET generation_cost_credits = ?
        WHERE id = ? AND user_id = ?" charge form-id user-id])
    charge
    (catch Exception error
      (tel/error! {:id ::record-form-charge
                   :data {:user-id user-id :form-id form-id}} error)
      nil)))

(defn form-stale?
  "Whether current material evidence differs from the accepted evidence."
  [connectable user-id form]
  (let [subject {:material-id (:material_id form) :block-id (:block_id form)}
        current (:source-versions (subject-evidence connectable user-id subject))
        accepted (or (:accepted_source_versions form) (:source_versions form))]
    (not= current accepted)))

(defn approve-form!
  "Freeze a draft, or explicitly reapprove an outdated approved form.

   Pre: the user owns form-id. For a draft, edits may carry the prompts and
   threshold the reader currently sees.
   Post: the form is approved and holds those edits. Reapproving an approved
   form re-accepts current evidence and writes no prompts, which is this
   function's second job.
   Invariant: an approve never discards an unsaved prompt. Committing the edits
   and freezing the form happen in one transaction, or neither happens."
  ([user-id form-id] (approve-form! user-id form-id nil))
  ([user-id form-id {:keys [threshold prompts]}]
   (when (some? threshold) (require-threshold! threshold))
   (jdbc/with-transaction [tx db/ds]
     (let [form (require-owned! (own-form tx user-id form-id) :exam-form form-id)
           subject {:material-id (:material_id form) :block-id (:block_id form)}
           current (:source-versions (subject-evidence tx user-id subject))]
       (when (and (= "draft" (:status form)) (seq prompts))
         (jdbc/execute-one! tx
           ["UPDATE curriculum_exam_forms
               SET threshold = ?, questions = ? WHERE id = ?"
            (or threshold (:threshold form))
            (pg-json (questions-with-prompts (:questions form) prompts)) form-id]))
       (jdbc/execute-one! tx
         ["UPDATE curriculum_exam_forms
             SET status = 'approved', approved_at = COALESCE(approved_at, now()),
                 accepted_source_versions = ? WHERE id = ?"
          (pg-json current) form-id])))
   (bump! user-id)))

(defn list-forms
  "Return forms for one subject with a computed stale flag."
  [user-id subject]
  (let [[column id] (subject-column+id subject)]
    (mapv (fn [row]
            (let [form (-> row
                         (update :blueprint read-json)
                         (update :questions read-json)
                         (update :source_versions read-json)
                         (update :accepted_source_versions read-json))]
              (assoc form :stale? (and (= "approved" (:status form))
                                    (form-stale? db/ds user-id form)))))
      (jdbc/execute! db/ds
        [(str "SELECT * FROM curriculum_exam_forms
                WHERE user_id = ? AND " column " = ? ORDER BY version DESC")
         user-id id] opts))))

(defn form-summary
  "Reduce one subject's forms to what a chip and a route need.

   Pre: forms are that subject's rows, newest version first, with their
   source-version JSON already read.
   Post: returns nil for a subject with no form. Nil is what an exam chip
   reads to offer generation.
   Invariant: this is the ONLY definition of current and sittable. The current
   form is the highest version, whatever its status. The sittable form is the
   highest APPROVED version. A subject with no approved version cannot be sat."
  [connectable user-id forms]
  (when-let [current (first forms)]
    (let [approved (first (filter #(= "approved" (:status %)) forms))
          stale? (boolean (and approved (form-stale? connectable user-id approved)))]
      {:form-id (:id current)
       :version (:version current)
       :status (:status current)
       :form-kind (:form_kind current)
       :threshold (:threshold current)
       :version-count (count forms)
       :approved-form-id (:id approved)
       :stale? stale?
       :sittable? (and (some? approved) (not stale?))})))

(defn- subject-key [row]
  (if-let [id (:material_id row)] [:material id] [:block (:block_id row)]))

(defn exam-summaries
  "Return one exam summary per subject in a curriculum, keyed by subject.

   One statement reads every form the curriculum owns, because a per-chip
   query would repeat the evidence walk once for each material.
   Post: keys are [:material id] and [:block id]. A subject with no form is
   absent, so `get` answers nil."
  [connectable user-id curriculum-id]
  (let [rows (mapv #(-> %
                      (update :source_versions read-json)
                      (update :accepted_source_versions read-json))
               (jdbc/execute! connectable
                 ["SELECT f.id, f.material_id, f.block_id, f.version, f.status,
                          f.form_kind, f.threshold, f.source_versions,
                          f.accepted_source_versions
                     FROM curriculum_exam_forms f
                     LEFT JOIN curriculum_materials m ON m.id = f.material_id
                     LEFT JOIN curriculum_blocks mb ON mb.id = m.block_id
                     LEFT JOIN curriculum_blocks b ON b.id = f.block_id
                    WHERE f.user_id = ?
                      AND COALESCE(mb.curriculum_id, b.curriculum_id) = ?
                    ORDER BY f.version DESC" user-id curriculum-id] opts))]
    (into {}
      (map (fn [[key subject-forms]] [key (form-summary connectable user-id subject-forms)]))
      (group-by subject-key rows))))

(defn start-sitting!
  "Start an approved, current form."
  [user-id form-id]
  (let [form (require-owned! (own-form db/ds user-id form-id) :exam-form form-id)]
    (when-not (= "approved" (:status form))
      (throw (ex-info "Approve this exam form before taking it." {:type ::draft-form})))
    (when (form-stale? db/ds user-id form)
      (throw (ex-info "Regenerate or reapprove this outdated exam form."
               {:type ::stale-form})))
    (:id (jdbc/execute-one! db/ds
           ["INSERT INTO curriculum_exam_sittings (form_id, user_id)
             VALUES (?, ?) RETURNING id" form-id user-id] opts))))

(def ^:private blank-answer-draft
  "The shape an answer draft starts from. Five slots, in question order."
  (vec (repeat 5 {:answer "" :do-not-know false})))

(defn- answers-complete? [answers]
  (and (= 5 (count answers))
    (every? #(or (not (str/blank? (str (:answer %)))) (:do-not-know %)) answers)))

(defn- merge-answer-slots
  "Return the draft with each slot written at its own index.
   Pre: every slot carries an index from 0 to 4. A bad index is a caller bug."
  [draft slots]
  (reduce (fn [answers {:keys [index answer do-not-know]}]
            (when-not (and (integer? index) (<= 0 index 4))
              (throw (ex-info "The answer index must be from 0 to 4."
                       {:type ::invalid-answer-index :index index})))
            (assoc answers index {:answer (str answer)
                                  :do-not-know (boolean do-not-know)}))
    (vec (or draft blank-answer-draft))
    slots))

(defn save-answer-draft!
  "Merge answer slots into an open sitting's draft.

   Pre: the user owns sitting-id. Each slot carries an index from 0 to 4, an
   answer, and a do-not-know flag.
   Post: returns :saved, or :already-closed when the sitting is submitted or
   cancelled. Throws only when the user owns no such sitting.
   Invariant: a closed sitting never changes. :already-closed is an OUTCOME and
   not an exception, because a focus change fired by the Submit click lands
   beside the finalize and must not read as a failure. A cancelled sitting is
   closed for the same reason a submitted one is: nothing may be written to a
   sitting the learner has walked away from."
  [user-id sitting-id slots]
  (let [outcome
        (jdbc/with-transaction [tx db/ds]
          (let [sitting (require-owned!
                          (jdbc/execute-one! tx
                            ["SELECT s.id, s.submitted_at, s.cancelled_at, s.draft_answers
                                FROM curriculum_exam_sittings s
                                JOIN curriculum_exam_forms f ON f.id = s.form_id
                                LEFT JOIN curriculum_materials m ON m.id = f.material_id
                                JOIN curriculum_blocks b ON b.id = COALESCE(f.block_id, m.block_id)
                               JOIN curricula c ON c.id = b.curriculum_id
                               WHERE s.id = ? AND s.user_id = ?
                                 AND c.staged_delete_id IS NULL FOR UPDATE OF s"
                             sitting-id user-id] opts)
                          :sitting sitting-id)]
            (if (or (:submitted_at sitting) (:cancelled_at sitting))
              :already-closed
              (do (jdbc/execute-one! tx
                    ["UPDATE curriculum_exam_sittings SET draft_answers = ? WHERE id = ?"
                     (pg-json (merge-answer-slots (read-json (:draft_answers sitting))
                                slots))
                     sitting-id])
                  :saved))))]
    outcome))

(defn finalize-sitting!
  "Freeze the answer draft and close the sitting.

   Pre: the user owns an open sitting whose draft answers are all complete.
   Post: answers equals the draft, submitted_at is set, and :submitted comes
   back. A submitted or cancelled sitting yields :already-closed with nothing
   changed.
   Invariant: no answer text crosses the wire at finalize time, because the
   text is already on the server. A cancelled sitting can never be finalized,
   so no grade can follow a sitting the learner abandoned."
  [user-id sitting-id]
  (let [outcome
        (jdbc/with-transaction [tx db/ds]
          (let [sitting (require-owned!
                          (jdbc/execute-one! tx
                            ["SELECT s.id, s.submitted_at, s.cancelled_at, s.draft_answers
                                FROM curriculum_exam_sittings s
                                JOIN curriculum_exam_forms f ON f.id = s.form_id
                                LEFT JOIN curriculum_materials m ON m.id = f.material_id
                                JOIN curriculum_blocks b ON b.id = COALESCE(f.block_id, m.block_id)
                               JOIN curricula c ON c.id = b.curriculum_id
                               WHERE s.id = ? AND s.user_id = ?
                                 AND c.staged_delete_id IS NULL FOR UPDATE OF s"
                             sitting-id user-id] opts)
                          :sitting sitting-id)]
            (if (or (:submitted_at sitting) (:cancelled_at sitting))
              :already-closed
              (let [answers (read-json (:draft_answers sitting))]
                (when-not (answers-complete? answers)
                  (throw (ex-info "Answer every question or select “I do not know.”"
                           {:type ::incomplete-sitting})))
                (jdbc/execute-one! tx
                  ["UPDATE curriculum_exam_sittings
                      SET answers = ?, submitted_at = now() WHERE id = ?"
                   (pg-json answers) sitting-id])
                :submitted))))]
    (when (= :submitted outcome) (bump! user-id))
    outcome))

(defn cancel-sitting!
  "Close an open sitting without freezing its answers.

   Pre: the user owns the sitting.
   Post: :cancelled when this call closed it. :already-closed when the sitting
   was already submitted, already cancelled, or archived — nothing changes and
   no exception is raised, because a second click is not a caller error.
   Invariant: a SUBMITTED sitting can never be cancelled. Its answers are
   frozen and a grade may already cite them, so cancelling would orphan a
   revision and a block completion."
  [user-id sitting-id]
  (let [updated (jdbc/execute-one! db/ds
                  ["UPDATE curriculum_exam_sittings SET cancelled_at = now()
                     WHERE id = ? AND user_id = ?
                       AND submitted_at IS NULL AND cancelled_at IS NULL
                       AND EXISTS (
                         SELECT 1
                           FROM curriculum_exam_forms f
                           LEFT JOIN curriculum_materials m ON m.id = f.material_id
                           JOIN curriculum_blocks b ON b.id = COALESCE(f.block_id, m.block_id)
                           JOIN curricula c ON c.id = b.curriculum_id
                          WHERE f.id = curriculum_exam_sittings.form_id
                            AND c.staged_delete_id IS NULL)
                     RETURNING id" sitting-id user-id] opts)]
    (bump! user-id)
    (if updated :cancelled :already-closed)))

(defn archive-sitting!
  "Take a cancelled sitting off the sittings list.

   Pre: the user owns the sitting and it is cancelled.
   Post: :archived when this call hid it. :not-cancelled when the sitting is
   open or submitted — a submitted sitting is the record of a graded attempt
   and must stay visible. :already-archived when it was already hidden.
   Invariant: archiving hides a row and deletes nothing. The grade revisions
   and the answers stay, so an account export still carries the whole history."
  [user-id sitting-id]
  (let [sitting (jdbc/execute-one! db/ds
                  ["SELECT s.cancelled_at, s.archived_at
                      FROM curriculum_exam_sittings s
                      JOIN curriculum_exam_forms f ON f.id = s.form_id
                      LEFT JOIN curriculum_materials m ON m.id = f.material_id
                      JOIN curriculum_blocks b ON b.id = COALESCE(f.block_id, m.block_id)
                      JOIN curricula c ON c.id = b.curriculum_id
                     WHERE s.id = ? AND s.user_id = ?
                       AND c.staged_delete_id IS NULL" sitting-id user-id] opts)]
    (cond
      (nil? sitting) (throw (ex-info "Sitting not found"
                              {:type ::not-found :entity :sitting :id sitting-id}))
      (some? (:archived_at sitting)) :already-archived
      (nil? (:cancelled_at sitting)) :not-cancelled
      :else (do (jdbc/execute-one! db/ds
                  ["UPDATE curriculum_exam_sittings SET archived_at = now()
                     WHERE id = ? AND user_id = ?
                       AND EXISTS (
                         SELECT 1
                           FROM curriculum_exam_forms f
                           LEFT JOIN curriculum_materials m ON m.id = f.material_id
                           JOIN curriculum_blocks b ON b.id = COALESCE(f.block_id, m.block_id)
                           JOIN curricula c ON c.id = b.curriculum_id
                          WHERE f.id = curriculum_exam_sittings.form_id
                            AND c.staged_delete_id IS NULL)" sitting-id user-id])
              (bump! user-id)
              :archived))))

(defn sitting-for-grade
  "Return one submitted sitting with its frozen form."
  [user-id sitting-id]
  (some-> (jdbc/execute-one! db/ds
            ["SELECT s.*, f.questions, f.blueprint, f.threshold, f.block_id,
                     f.material_id, f.source_versions
                FROM curriculum_exam_sittings s
                JOIN curriculum_exam_forms f ON f.id = s.form_id
                LEFT JOIN curriculum_materials m ON m.id = f.material_id
                JOIN curriculum_blocks b ON b.id = COALESCE(f.block_id, m.block_id)
                JOIN curricula c ON c.id = b.curriculum_id
               WHERE s.id = ? AND s.user_id = ? AND s.submitted_at IS NOT NULL
                 AND c.staged_delete_id IS NULL"
             sitting-id user-id] opts)
    (update :answers read-json)
    (update :questions read-json)
    (update :blueprint read-json)
    (update :source_versions read-json)))

(defn record-grade-revision!
  "Append one successful AI grade and apply progression atomically.

   Returns nil when a curriculum deletion occurred after operation-started-at.
   Completion never reverses after a later lower revision."
  [user-id sitting-id {:keys [raw-score criteria explanation cost operation-started-at]}]
  (let [criterion-scores (mapcat #(vals (:criteria %)) criteria)]
    (when-not (and (= 5 (count criteria)) (= 25 (count criterion-scores))
                (= raw-score (reduce + criterion-scores)))
      (throw (ex-info "The raw score must equal all 25 criterion scores."
               {:type ::invalid-grade-revision}))))
  (let [revision-id
        (jdbc/with-transaction [tx db/ds]
          (let [sitting (jdbc/execute-one! tx
                          ["SELECT s.*, f.threshold, f.block_id, f.material_id,
                                  c.id AS curriculum_id, c.staged_delete_id
                              FROM curriculum_exam_sittings s
                              JOIN curriculum_exam_forms f ON f.id = s.form_id
                              LEFT JOIN curriculum_materials m ON m.id = f.material_id
                              JOIN curriculum_blocks b ON b.id = COALESCE(f.block_id, m.block_id)
                              JOIN curricula c ON c.id = b.curriculum_id
                             WHERE s.id = ? AND s.user_id = ?
                               AND s.submitted_at IS NOT NULL FOR UPDATE OF c"
                           sitting-id user-id] opts)]
            (cond
              (and operation-started-at sitting
                   (curriculum-deleted-since?
                     tx user-id (:curriculum_id sitting) operation-started-at))
              nil

              (:staged_delete_id sitting)
              (throw (ex-info "Sitting not found"
                       {:type ::not-found :entity :sitting :id sitting-id}))

              :else
              (let [sitting (require-owned! sitting :sitting sitting-id)
                    passed (>= raw-score (:threshold sitting))
                    revision (inc (:revision (jdbc/execute-one! tx
                                              ["SELECT COALESCE(max(revision), 0)::int AS revision
                                                 FROM curriculum_grade_revisions WHERE sitting_id = ?"
                                               sitting-id] opts)))
                    row (jdbc/execute-one! tx
                          ["INSERT INTO curriculum_grade_revisions
                              (sitting_id, revision, raw_score, passed, criteria,
                               explanation, grading_cost_credits)
                            VALUES (?, ?, ?, ?, ?, ?, ?) RETURNING id"
                           sitting-id revision raw-score passed (pg-json criteria) explanation cost]
                          opts)]
                (when (and passed (:block_id sitting))
                  (let [block (jdbc/execute-one! tx
                                ["UPDATE curriculum_blocks
                                    SET completed_at = COALESCE(completed_at, now()), focused_at = NULL
                                  WHERE id = ? RETURNING curriculum_id, position"
                                 (:block_id sitting)] opts)
                        next-block (jdbc/execute-one! tx
                                     ["SELECT id FROM curriculum_blocks
                                       WHERE curriculum_id = ? AND position > ?
                                       ORDER BY position LIMIT 1"
                                      (:curriculum_id block) (:position block)] opts)]
                    (when next-block
                      (jdbc/execute-one! tx
                        ["UPDATE curriculum_blocks
                            SET unlocked_at = COALESCE(unlocked_at, now()),
                                focused_at = CASE
                                  WHEN NOT EXISTS (
                                    SELECT 1 FROM curriculum_blocks focused
                                     WHERE focused.curriculum_id = ?
                                       AND focused.focused_at IS NOT NULL)
                                  THEN now() ELSE focused_at END
                          WHERE id = ?" (:curriculum_id block) (:id next-block)]))))
                (:id row)))))]
    (when revision-id (bump! user-id))
    revision-id))

(defn record-grade-charge!
  "Attach the actual completed-call charge to a persisted grade revision."
  [user-id revision-id charge]
  (try
    (jdbc/execute-one! db/ds
      ["UPDATE curriculum_grade_revisions g SET grading_cost_credits = ?
         FROM curriculum_exam_sittings s
        WHERE g.id = ? AND g.sitting_id = s.id AND s.user_id = ?"
       charge revision-id user-id])
    charge
    (catch Exception error
      (tel/error! {:id ::record-grade-charge
                   :data {:user-id user-id :revision-id revision-id}} error)
      nil)))

(defn record-self-correction!
  "Record learner reflection without changing progression."
  [user-id sitting-id score reason]
  (let [row (jdbc/execute-one! db/ds
              ["UPDATE curriculum_exam_sittings
                  SET self_correction_score = ?, self_correction_reason = ?
                WHERE id = ? AND user_id = ?
                  AND EXISTS (
                    SELECT 1
                      FROM curriculum_exam_forms f
                      LEFT JOIN curriculum_materials m ON m.id = f.material_id
                      JOIN curriculum_blocks b ON b.id = COALESCE(f.block_id, m.block_id)
                      JOIN curricula c ON c.id = b.curriculum_id
                     WHERE f.id = curriculum_exam_sittings.form_id
                       AND c.staged_delete_id IS NULL)
              RETURNING id"
               score reason sitting-id user-id] opts)]
    (require-owned! row :sitting sitting-id)
    (bump! user-id)))

(defn latest-grade [user-id sitting-id]
  (some-> (jdbc/execute-one! db/ds
            ["SELECT g.* FROM curriculum_grade_revisions g
              JOIN curriculum_exam_sittings s ON s.id = g.sitting_id
             WHERE g.sitting_id = ? AND s.user_id = ?
             ORDER BY g.revision DESC LIMIT 1" sitting-id user-id] opts)
    (update :criteria read-json)))

(defn grade-history
  "Return immutable AI revisions, newest first."
  [user-id sitting-id]
  (mapv #(update % :criteria read-json)
    (jdbc/execute! db/ds
      ["SELECT g.* FROM curriculum_grade_revisions g
        JOIN curriculum_exam_sittings s ON s.id = g.sitting_id
       WHERE g.sitting_id = ? AND s.user_id = ?
       ORDER BY g.revision DESC" sitting-id user-id] opts)))

(defn- completed-subjects [user-id]
  (jdbc/execute! db/ds
    ["SELECT DISTINCT f.material_id, f.block_id
       FROM curriculum_grade_revisions g
       JOIN curriculum_exam_sittings s ON s.id = g.sitting_id
       JOIN curriculum_exam_forms f ON f.id = s.form_id
      WHERE s.user_id = ? AND g.passed" user-id] opts))

(defn curriculum-detail
  "Return one curriculum with nested blocks, materials, readiness, and forms.

   Post: nil when the curriculum is absent or belongs to another account.
   A READER does not blame its caller for a row that is gone. A live page
   holds the id of a curriculum that another act can delete under it, so
   `require-owned!` here reported the delete as a server exception, which
   Electric propagates as a reactor failure and a closed WebSocket. Writers
   keep throwing, because a writer that names a row it does not own IS a
   caller bug."
  [_refresh user-id curriculum-id]
  (let [curriculum (own-curriculum db/ds user-id curriculum-id)
        blocks (jdbc/execute! db/ds
                 ["SELECT * FROM curriculum_blocks WHERE curriculum_id = ? ORDER BY position"
                  curriculum-id] opts)
        materials (jdbc/execute! db/ds
                    ["SELECT m.*, t.title, t.kind,
                            count(a.anki_card_id)::int AS anki_cards,
                            COALESCE(avg(a.stability), 0)::double precision AS anki_stability
                       FROM curriculum_materials m
                       JOIN curriculum_blocks b ON b.id = m.block_id
                       JOIN topics t ON t.id = m.topic_id
                       LEFT JOIN flashcards f ON f.root_topic_id = m.topic_id
                       LEFT JOIN anki_card_states a ON a.user_id = ?
                                                    AND a.flashcard_id = f.id
                      WHERE b.curriculum_id = ?
                      GROUP BY m.id, t.title, t.kind
                      ORDER BY m.block_id, m.position" user-id curriculum-id] opts)
        material-scores (jdbc/execute! db/ds
                          ["SELECT DISTINCT ON (f.material_id)
                                  f.material_id, g.raw_score
                             FROM curriculum_grade_revisions g
                             JOIN curriculum_exam_sittings s ON s.id = g.sitting_id
                             JOIN curriculum_exam_forms f ON f.id = s.form_id
                             JOIN curriculum_materials m ON m.id = f.material_id
                             JOIN curriculum_blocks b ON b.id = m.block_id
                            WHERE s.user_id = ? AND b.curriculum_id = ?
                            ORDER BY f.material_id, g.created_at DESC"
                           user-id curriculum-id] opts)
        score-by-material (into {} (map (juxt :material_id :raw_score)) material-scores)
        passed (completed-subjects user-id)
        passed-materials (into #{} (keep :material_id) passed)
        summaries (exam-summaries db/ds user-id curriculum-id)]
    (when curriculum
      (assoc curriculum
        :complete? (and (seq blocks) (every? :completed_at blocks))
        :blocks
        (mapv (fn [block]
                (let [block-materials (filterv #(= (:id block) (:block_id %)) materials)]
                  (assoc block
                    :materials (mapv #(assoc %
                                        :exam-passed? (contains? passed-materials (:id %))
                                        :latest-exam-score (get score-by-material (:id %))
                                        :exam (get summaries [:material (:id %)]))
                                 block-materials)
                    :exam (get summaries [:block (:id block)]))))
          blocks)))))

;; ── Exam page read model ───────────────────────────────────────────────────

(defn- subject-form-rows
  "Return one subject's form rows, newest version first, without questions."
  [connectable user-id subject]
  (let [[column id] (subject-column+id subject)]
    (mapv #(-> %
             (update :source_versions read-json)
             (update :accepted_source_versions read-json))
      (jdbc/execute! connectable
        [(str "SELECT id, material_id, block_id, version, status, form_kind,
                      threshold, source_versions, accepted_source_versions
                 FROM curriculum_exam_forms
                WHERE user_id = ? AND " column " = ? ORDER BY version DESC")
         user-id id] opts))))

(defn- form-breadcrumb
  "Resolve the curriculum, block, and material names from a form id alone.

   The curriculum id in the exam URL is display context. This read is what
   makes it so, and what lets a cold load render the trail."
  [connectable user-id form-id]
  (jdbc/execute-one! connectable
    ["SELECT c.id AS curriculum_id, c.title AS curriculum_title,
             b.id AS block_id, b.title AS block_title,
             m.id AS material_id, t.title AS material_title
        FROM curriculum_exam_forms f
        LEFT JOIN curriculum_materials m ON m.id = f.material_id
        LEFT JOIN topics t ON t.id = m.topic_id
        LEFT JOIN curriculum_blocks b ON b.id = COALESCE(f.block_id, m.block_id)
        JOIN curricula c ON c.id = b.curriculum_id
       WHERE f.id = ? AND f.user_id = ?" form-id user-id] opts))

(defn- charge->long
  "Return a recorded charge as a plain long.

   The column is NUMERIC, so JDBC hands back a BigDecimal that no Electric
   transfer can carry. Credits are whole IQD, so nothing is lost."
  [value]
  (some-> value bigdec (.setScale 0 java.math.RoundingMode/HALF_UP) .longValue))

(defn- grade-row->map
  "Return one grade revision in a shape Electric can transfer.
   Post: the charge is a long and the timestamp is a string."
  [row]
  (some-> row
    (update :grading_cost_credits charge->long)
    (update :created_at #(some-> % str))))

(defn- sitting-rows
  "Return every sitting of one form, newest first, with server-derived elapsed."
  [connectable user-id form-id]
  (mapv (fn [row]
          (-> row
            (update :started_at #(some-> % str))
            (update :submitted_at #(some-> % str))
            (update :cancelled_at #(some-> % str))
            (update :draft_answers read-json)))
    (jdbc/execute! connectable
      ["SELECT s.id, s.started_at, s.submitted_at, s.cancelled_at, s.draft_answers,
               GREATEST(0, floor(EXTRACT(EPOCH FROM
                 (COALESCE(s.submitted_at, s.cancelled_at, now())
                  - s.started_at))))::int
                 AS elapsed_seconds,
               g.raw_score, g.passed
          FROM curriculum_exam_sittings s
          LEFT JOIN LATERAL (SELECT raw_score, passed
                               FROM curriculum_grade_revisions
                              WHERE sitting_id = s.id
                              ORDER BY revision DESC LIMIT 1) g ON true
         WHERE s.user_id = ? AND s.form_id = ? AND s.archived_at IS NULL
         ORDER BY s.started_at DESC" user-id form-id] opts)))

(defn exam-page-model
  "Return everything the exam page renders for one form.

   Pre: the user owns form-id. sitting-id is nil, or a sitting of that form.
   Post: returns nil when the user owns no such form, which is what renders
   Not found. Elapsed seconds are derived on the SERVER from started_at, so a
   reload 20 minutes into a sitting shows 20 minutes and no clock is compared.
   Invariant: the blueprint here carries no evidence packets. Those hold whole
   source passages, and `form-evidence-packets` fetches them only when a reader
   opens them."
  [_refresh user-id form-id sitting-id]
  (when-let [form (own-form db/ds user-id form-id)]
    (let [subject {:material-id (:material_id form) :block-id (:block_id form)}
          rows (subject-form-rows db/ds user-id subject)
          sittings (sitting-rows db/ds user-id form-id)
          open (first (filter #(and (nil? (:submitted_at %)) (nil? (:cancelled_at %)))
                        sittings))
          chosen (when sitting-id (first (filter #(= sitting-id (:id %)) sittings)))]
      {:form {:id (:id form)
              :version (:version form)
              :status (:status form)
              :form-kind (:form_kind form)
              :threshold (:threshold form)
              :questions (:questions form)
              :assessed-outcomes (vec (:assessed_outcomes (:blueprint form)))
              :unassessed-outcomes (vec (:unassessed_outcomes (:blueprint form)))
              :evidence-packet-count (count (:evidence_packets (:blueprint form)))
              :generation-cost (charge->long (:generation_cost_credits form))
              :stale? (and (= "approved" (:status form))
                        (form-stale? db/ds user-id form))}
       :subject subject
       :breadcrumb (form-breadcrumb db/ds user-id form-id)
       :summary (form-summary db/ds user-id rows)
       :versions (mapv #(select-keys % [:id :version :status :threshold]) rows)
       :sittings (mapv #(dissoc % :draft_answers) sittings)
       :open-sitting (when open
                       {:id (:id open)
                        :elapsed-seconds (:elapsed_seconds open)
                        :draft-answers (or (:draft_answers open) blank-answer-draft)})
       :sitting (when chosen
                  {:id (:id chosen)
                   :submitted-at (:submitted_at chosen)
                   :cancelled? (some? (:cancelled_at chosen))
                   :elapsed-seconds (:elapsed_seconds chosen)
                   :draft-answers (or (:draft_answers chosen) blank-answer-draft)
                   :submitted? (some? (:submitted_at chosen))
                   :grade (grade-row->map (latest-grade user-id (:id chosen)))
                   :history (mapv grade-row->map
                              (grade-history user-id (:id chosen)))})})))

(defn form-evidence-packets
  "Return one form's blueprint evidence packets, on demand.
   Pre: the user owns form-id."
  [_refresh user-id form-id]
  (vec (:evidence_packets
         (:blueprint (require-owned! (own-form db/ds user-id form-id)
                       :exam-form form-id)))))

(defn- allowed-date? [weekdays ^LocalDate date]
  (or (empty? weekdays) (contains? weekdays (.getValue (.getDayOfWeek date)))))

(defn- available-dates [start weekdays]
  (filter #(allowed-date? weekdays %) (iterate #(.plusDays ^LocalDate % 1) start)))

(defn allocate-work
  "Allocate ordered work without exceeding the daily minute limit.
   A task may span days. Several tasks may share one day."
  [tasks ^LocalDate start weekdays daily-minutes]
  {:pre [(pos? daily-minutes)]}
  (loop [remaining (seq tasks)
         dates (available-dates start (set weekdays))
         capacity daily-minutes
         result []]
    (if-let [task (first remaining)]
      (let [minutes (min capacity (:minutes task))
            rest-minutes (- (:minutes task) minutes)
            capacity' (- capacity minutes)
            result' (conj result (assoc task :date (str (first dates))
                                   :minutes minutes))
            remaining' (if (pos? rest-minutes)
                         (cons (assoc task :minutes rest-minutes) (rest remaining))
                         (rest remaining))]
        (if (zero? capacity')
          (recur remaining' (rest dates) daily-minutes result')
          (recur remaining' dates capacity' result')))
      result)))

(defn build-schedule
  "Derive the current plan from incomplete work.
   Work is sequential. Focus-block work comes before other available blocks.

   Post: nil when the curriculum is absent, for the reason
   `curriculum-detail` states. The bindings above the guard are safe on an
   absent curriculum: `blocks` is nil, so `tasks` is empty and
   `allocate-work` returns without forcing its infinite date sequence."
  [_refresh user-id curriculum-id]
  (let [{:keys [target_date daily_minutes study_weekdays blocks] :as detail}
        (curriculum-detail 0 user-id curriculum-id)
        weekdays (set study_weekdays)
        focus-id (:id (first (filter :focused_at blocks)))
        available (filterv :unlocked_at blocks)
        ordered (sort-by (juxt #(if (= focus-id (:id %)) 0 1) :position) available)
        tasks (vec
                (mapcat
                  (fn [block]
                    (when-not (:completed_at block)
                      (concat
                        (mapcat (fn [material]
                                  (cond-> []
                                    (nil? (:studied_at material))
                                    (conj {:kind :study :block-id (:id block)
                                           :material-id (:id material)
                                           :title (str "Study “" (:title material) "”")
                                           :minutes (:estimated_minutes material)})
                                    (and (pos? (:anki_cards material))
                                      (< (:anki_stability material) 7.0))
                                    (conj {:kind :anki-review :block-id (:id block)
                                           :material-id (:id material)
                                           :title (str "Anki readiness review: “"
                                                    (:title material) "”")
                                           :minutes (max 10
                                                      (int (Math/ceil
                                                             (* 1.5 (:anki_cards material)))))})
                                    (and (:latest-exam-score material)
                                      (not (:exam-passed? material)))
                                    (conj {:kind :targeted-review :block-id (:id block)
                                           :material-id (:id material)
                                           :title (str "Review weak exam outcomes: “"
                                                    (:title material) "”")
                                           :minutes (max 15
                                                      (- 70 (:latest-exam-score material)))})
                                    (not (:exam-passed? material))
                                    (conj {:kind :material-exam :block-id (:id block)
                                           :material-id (:id material)
                                           :title (str "Material exam: “" (:title material) "”")
                                           :minutes default-material-exam-minutes})))
                          (:materials block))
                        [{:kind :block-exam :block-id (:id block)
                          :title (str "Block exam: “" (:title block) "”")
                          :minutes default-block-exam-minutes}])))
                  ordered))
        total-minutes (reduce + 0 (map :minutes tasks))
        start (LocalDate/now (ZoneId/systemDefault))
        target (some-> target_date str LocalDate/parse)
        dates-to-target (when target
                          (take-while #(not (.isAfter ^LocalDate % target))
                            (available-dates start weekdays)))
        available-days (count dates-to-target)
        derived-daily (if daily_minutes daily_minutes
                        (if (pos? available-days)
                          (max 1 (int (Math/ceil (/ total-minutes (double available-days)))))
                          default-daily-minutes))
        allocations (allocate-work tasks start weekdays derived-daily)
        earliest (some-> allocations last :date LocalDate/parse)
        feasible? (or (nil? target) (nil? earliest) (not (.isAfter earliest target)))]
    (when detail
      {:curriculum (select-keys detail [:id :title :target_date :daily_minutes :study_weekdays])
       :tasks allocations
       :total-minutes total-minutes
       :daily-minutes derived-daily
       :feasible? feasible?
       :earliest-date (some-> earliest str)
       :target-date (some-> target str)
       :adjustment (when-not feasible?
                     {:earliest-date (str earliest)
                      :required-daily-minutes (if (pos? available-days)
                                                (int (Math/ceil (/ total-minutes
                                                                   (double available-days))))
                                                total-minutes)})})))
