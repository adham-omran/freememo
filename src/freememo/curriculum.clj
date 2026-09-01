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

(defn- own-curriculum [connectable user-id curriculum-id]
  (jdbc/execute-one! connectable
    ["SELECT * FROM curricula WHERE id = ? AND user_id = ?" curriculum-id user-id]
    opts))

(defn- own-block [connectable user-id block-id]
  (jdbc/execute-one! connectable
    ["SELECT b.* FROM curriculum_blocks b
       JOIN curricula c ON c.id = b.curriculum_id
      WHERE b.id = ? AND c.user_id = ?" block-id user-id]
    opts))

(defn- own-material [connectable user-id material-id]
  (jdbc/execute-one! connectable
    ["SELECT m.*, b.curriculum_id FROM curriculum_materials m
       JOIN curriculum_blocks b ON b.id = m.block_id
       JOIN curricula c ON c.id = b.curriculum_id
      WHERE m.id = ? AND c.user_id = ?" material-id user-id]
    opts))

(defn- own-form [connectable user-id form-id]
  (some-> (jdbc/execute-one! connectable
            ["SELECT * FROM curriculum_exam_forms WHERE id = ? AND user_id = ?"
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
  (jdbc/execute! db/ds
    ["SELECT c.*,
             count(b.id)::int AS block_count,
             count(b.completed_at)::int AS completed_block_count
        FROM curricula c
        LEFT JOIN curriculum_blocks b ON b.curriculum_id = c.id
       WHERE c.user_id = ?
       GROUP BY c.id
       ORDER BY c.updated_at DESC" user-id]
    opts))

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

(defn- resequence! [tx table parent-column parent-id ids]
  (doseq [[position id] (map-indexed vector ids)]
    (jdbc/execute-one! tx
      [(str "UPDATE " table " SET position = ? WHERE id = ? AND " parent-column " = ?")
       (- -100000 position) id parent-id]))
  (doseq [[position id] (map-indexed vector ids)]
    (jdbc/execute-one! tx
      [(str "UPDATE " table " SET position = ? WHERE id = ? AND " parent-column " = ?")
       position id parent-id])))

(defn add-block!
  "Append a block. The first block starts unlocked and focused."
  [user-id curriculum-id title]
  (require-owned! (own-curriculum db/ds user-id curriculum-id) :curriculum curriculum-id)
  (let [title (str/trim (str title))]
    (when (str/blank? title)
      (throw (ex-info "Enter a block name." {:type ::invalid-block})))
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
        (bump! user-id)
        (:id row)))))

(defn reorder-blocks!
  "Replace the complete block order.
   Pre: ids are exactly the curriculum's current block ids."
  [user-id curriculum-id ids]
  (jdbc/with-transaction [tx db/ds]
    (require-owned! (own-curriculum tx user-id curriculum-id) :curriculum curriculum-id)
    (let [current (mapv :id (jdbc/execute! tx
                              ["SELECT id FROM curriculum_blocks
                                 WHERE curriculum_id = ? ORDER BY position" curriculum-id]
                              opts))]
      (when-not (= (set current) (set ids))
        (throw (ex-info "Block order does not match this curriculum."
                 {:type ::invalid-order})))
      (resequence! tx "curriculum_blocks" "curriculum_id" curriculum-id ids)
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
  (jdbc/with-transaction [tx db/ds]
    (let [block (require-owned! (own-block tx user-id block-id) :block block-id)
          topic (jdbc/execute-one! tx
                  ["SELECT id, kind FROM topics
                    WHERE id = ? AND user_id = ? AND staged_delete_id IS NULL"
                   topic-id user-id] opts)]
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
        (bump! user-id)
        (:id row)))))

(defn reorder-materials!
  "Replace the complete material order inside one block."
  [user-id block-id ids]
  (jdbc/with-transaction [tx db/ds]
    (require-owned! (own-block tx user-id block-id) :block block-id)
    (let [current (mapv :id (jdbc/execute! tx
                              ["SELECT id FROM curriculum_materials
                                WHERE block_id = ? ORDER BY position" block-id] opts))]
      (when-not (= (set current) (set ids))
        (throw (ex-info "Material order does not match this block."
                 {:type ::invalid-order})))
      (resequence! tx "curriculum_materials" "block_id" block-id ids)))
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

(defn next-form-version [connectable user-id subject]
  (let [{:keys [material-id block-id]} subject
        [column id] (if material-id ["material_id" material-id] ["block_id" block-id])]
    (require-owned! (if material-id
                      (own-material connectable user-id id)
                      (own-block connectable user-id id))
      (if material-id :material :block) id)
    (inc (:version (jdbc/execute-one! connectable
                     [(str "SELECT COALESCE(max(version), 0)::int AS version
                              FROM curriculum_exam_forms WHERE " column " = ?") id]
                     opts)))))

(defn save-draft-form!
  "Persist one validated five-question draft."
  [user-id subject {:keys [form-kind threshold blueprint questions source-versions cost]}]
  (when-not (= 5 (count questions))
    (throw (ex-info "An exam form must contain five questions."
             {:type ::invalid-form :question-count (count questions)})))
  (jdbc/with-transaction [tx db/ds]
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
      (:id row))))

(defn update-draft-form!
  "Replace editable draft fields. Approved forms are immutable."
  [user-id form-id {:keys [threshold blueprint questions]}]
  (let [form (require-owned! (own-form db/ds user-id form-id) :exam-form form-id)]
    (when-not (= "draft" (:status form))
      (throw (ex-info "Approved exam forms are immutable." {:type ::approved-form})))
    (when-not (= 5 (count questions))
      (throw (ex-info "An exam form must contain five questions."
               {:type ::invalid-form})))
    (jdbc/execute-one! db/ds
      ["UPDATE curriculum_exam_forms
          SET threshold = ?, blueprint = ?, questions = ? WHERE id = ?"
       threshold (pg-json blueprint) (pg-json questions) form-id])
    (bump! user-id)))

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
  "Freeze a draft, or explicitly reapprove an outdated approved form."
  [user-id form-id]
  (jdbc/with-transaction [tx db/ds]
    (let [form (require-owned! (own-form tx user-id form-id) :exam-form form-id)
          subject {:material-id (:material_id form) :block-id (:block_id form)}
          current (:source-versions (subject-evidence tx user-id subject))]
      (jdbc/execute-one! tx
        ["UPDATE curriculum_exam_forms
            SET status = 'approved', approved_at = COALESCE(approved_at, now()),
                accepted_source_versions = ? WHERE id = ?"
         (pg-json current) form-id])))
  (bump! user-id))

(defn list-forms
  "Return forms for one subject with a computed stale flag."
  [user-id subject]
  (let [[column id] (if-let [id (:material-id subject)] ["material_id" id]
                      ["block_id" (:block-id subject)])]
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

(defn submit-sitting!
  "Freeze all five answers before grading.
   Pre: each answer has text or explicit do-not-know true."
  [user-id sitting-id answers]
  (when-not (and (= 5 (count answers))
              (every? #(or (not (str/blank? (str (:answer %)))) (:do-not-know %)) answers))
    (throw (ex-info "Answer every question or select “I do not know.”"
             {:type ::incomplete-sitting})))
  (let [row (jdbc/execute-one! db/ds
              ["UPDATE curriculum_exam_sittings
                  SET answers = ?, submitted_at = now()
                WHERE id = ? AND user_id = ? AND submitted_at IS NULL
                RETURNING id"
               (pg-json answers) sitting-id user-id] opts)]
    (when-not row
      (require-owned!
        (jdbc/execute-one! db/ds
          ["SELECT id FROM curriculum_exam_sittings
            WHERE id = ? AND user_id = ? AND submitted_at IS NOT NULL"
           sitting-id user-id] opts)
        :sitting sitting-id))
    (bump! user-id)))

(defn sitting-for-grade
  "Return one submitted sitting with its frozen form."
  [user-id sitting-id]
  (some-> (jdbc/execute-one! db/ds
            ["SELECT s.*, f.questions, f.blueprint, f.threshold, f.block_id,
                     f.material_id, f.source_versions
                FROM curriculum_exam_sittings s
                JOIN curriculum_exam_forms f ON f.id = s.form_id
               WHERE s.id = ? AND s.user_id = ? AND s.submitted_at IS NOT NULL"
             sitting-id user-id] opts)
    (update :answers read-json)
    (update :questions read-json)
    (update :blueprint read-json)
    (update :source_versions read-json)))

(defn record-grade-revision!
  "Append one successful AI grade and apply progression atomically.
   Completion never reverses after a later lower revision."
  [user-id sitting-id {:keys [raw-score criteria explanation cost]}]
  (let [criterion-scores (mapcat #(vals (:criteria %)) criteria)]
    (when-not (and (= 5 (count criteria)) (= 25 (count criterion-scores))
                (= raw-score (reduce + criterion-scores)))
      (throw (ex-info "The raw score must equal all 25 criterion scores."
               {:type ::invalid-grade-revision}))))
  (jdbc/with-transaction [tx db/ds]
    (let [sitting (some-> (jdbc/execute-one! tx
                            ["SELECT s.*, f.threshold, f.block_id, f.material_id
                                FROM curriculum_exam_sittings s
                                JOIN curriculum_exam_forms f ON f.id = s.form_id
                               WHERE s.id = ? AND s.user_id = ? AND s.submitted_at IS NOT NULL"
                             sitting-id user-id] opts)
                    (require-owned! :sitting sitting-id))
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
                 sitting-id revision raw-score passed (pg-json criteria) explanation cost] opts)]
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
      (bump! user-id)
      (:id row))))

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
                WHERE id = ? AND user_id = ? RETURNING id"
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
  "Return one curriculum with nested blocks, materials, readiness, and forms."
  [_refresh user-id curriculum-id]
  (let [curriculum (require-owned! (own-curriculum db/ds user-id curriculum-id)
                     :curriculum curriculum-id)
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
        passed-materials (into #{} (keep :material_id) passed)]
    (assoc curriculum
      :complete? (and (seq blocks) (every? :completed_at blocks))
      :blocks
      (mapv (fn [block]
              (let [block-materials (filterv #(= (:id block) (:block_id %)) materials)]
                (assoc block
                  :materials (mapv #(assoc %
                                      :exam-passed? (contains? passed-materials (:id %))
                                      :latest-exam-score (get score-by-material (:id %)))
                               block-materials)
                  :forms (list-forms user-id {:block-id (:id block)}))))
        blocks))))

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
   Work is sequential. Focus-block work comes before other available blocks."
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
                                              total-minutes)})}))
