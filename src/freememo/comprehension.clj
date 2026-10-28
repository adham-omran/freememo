(ns freememo.comprehension
  "Persistent diagnostic cycles with frozen sources and learner-selected follow-up."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [freememo.assessment :as assessment]
            [freememo.db :as db]
            [freememo.kg-extract :as kg-extract]
            [freememo.user-state :as us]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs])
  (:import [java.security MessageDigest]
           [org.postgresql.util PGobject]))

(def ^:private opts {:builder-fn rs/as-unqualified-maps})

(defn ensure-schema!
  "Create cycle storage after the main schema exists. Existing records stay unchanged."
  []
  (doseq [statement
          ["CREATE TABLE IF NOT EXISTS comprehension_cycles (
              id BIGSERIAL PRIMARY KEY,
              user_id BIGINT NOT NULL REFERENCES users(id) ON DELETE CASCADE,
              topic_id BIGINT REFERENCES topics(id) ON DELETE SET NULL,
              title TEXT NOT NULL,
              target_minutes INTEGER NOT NULL CHECK (target_minutes BETWEEN 5 AND 180),
              materials JSONB NOT NULL,
              facts JSONB NOT NULL DEFAULT '[]',
              form JSONB NOT NULL,
              draft_answers JSONB NOT NULL DEFAULT '{}',
              draft_kind TEXT NOT NULL DEFAULT 'initial',
              parent_gap_id BIGINT,
              created_at TIMESTAMPTZ NOT NULL DEFAULT now())"
           "CREATE INDEX IF NOT EXISTS idx_comprehension_cycles_user
              ON comprehension_cycles(user_id, id DESC)"
           "CREATE TABLE IF NOT EXISTS comprehension_submissions (
              id BIGSERIAL PRIMARY KEY,
              cycle_id BIGINT NOT NULL REFERENCES comprehension_cycles(id) ON DELETE CASCADE,
              answer_digest TEXT NOT NULL,
              answers JSONB NOT NULL,
              revision_kind TEXT NOT NULL CHECK
                (revision_kind IN ('initial','transcription-correction','answer-revision')),
              scan_assignments JSONB NOT NULL DEFAULT '[]',
              status TEXT NOT NULL DEFAULT 'pending' CHECK
                (status IN ('pending','grading','graded','failed')),
              grade JSONB,
              error TEXT,
              grading_token UUID,
              grading_started_at TIMESTAMPTZ,
              created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
              UNIQUE(cycle_id, answer_digest))"
           "CREATE TABLE IF NOT EXISTS comprehension_gaps (
              id BIGSERIAL PRIMARY KEY,
              submission_id BIGINT NOT NULL REFERENCES comprehension_submissions(id) ON DELETE CASCADE,
              question_id TEXT NOT NULL,
              criterion TEXT NOT NULL,
              category TEXT NOT NULL CHECK (category IN ('recall','connection','reasoning','uncertain')),
              reason TEXT NOT NULL,
              evidence JSONB NOT NULL,
              fact_ids JSONB NOT NULL DEFAULT '[]',
              status TEXT NOT NULL DEFAULT 'identified' CHECK
                (status IN ('identified','studied','reassessed')),
              reassessment_submission_id BIGINT REFERENCES comprehension_submissions(id) ON DELETE SET NULL,
              created_at TIMESTAMPTZ NOT NULL DEFAULT now())"
           "CREATE TABLE IF NOT EXISTS comprehension_followups (
              id BIGSERIAL PRIMARY KEY,
              gap_id BIGINT NOT NULL REFERENCES comprehension_gaps(id) ON DELETE CASCADE,
              kind TEXT NOT NULL CHECK (kind IN ('source-review','targeted-questions')),
              evidence JSONB NOT NULL,
              questions JSONB NOT NULL DEFAULT '[]',
              session_id BIGINT REFERENCES kg_sessions(id) ON DELETE SET NULL,
              completed_at TIMESTAMPTZ,
              created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
              UNIQUE(gap_id, kind))"
           "CREATE TABLE IF NOT EXISTS comprehension_scans (
              id BIGSERIAL PRIMARY KEY,
              cycle_id BIGINT NOT NULL REFERENCES comprehension_cycles(id) ON DELETE CASCADE,
              filename TEXT NOT NULL,
              content_type TEXT NOT NULL,
              original_bytes BYTEA NOT NULL,
              page_count INTEGER NOT NULL CHECK (page_count BETWEEN 1 AND 20),
              suggestions JSONB,
              uncertainties JSONB NOT NULL DEFAULT '[]',
              accepted_assignments JSONB,
              accepted_at TIMESTAMPTZ,
              transcription_token UUID,
              transcription_started_at TIMESTAMPTZ,
              status TEXT NOT NULL DEFAULT 'uploaded' CHECK
                (status IN ('uploaded','transcribing','review','accepted','failed')),
              error TEXT,
              created_at TIMESTAMPTZ NOT NULL DEFAULT now())"
           "CREATE INDEX IF NOT EXISTS idx_comprehension_submissions_cycle
              ON comprehension_submissions(cycle_id, id DESC)"
           "CREATE INDEX IF NOT EXISTS idx_comprehension_scans_cycle
              ON comprehension_scans(cycle_id, id)"
           "ALTER TABLE comprehension_scans ADD COLUMN IF NOT EXISTS transcription_token UUID"
           "ALTER TABLE comprehension_scans ADD COLUMN IF NOT EXISTS transcription_started_at TIMESTAMPTZ"
           "DO $$ DECLARE column_row record; BEGIN
              FOR column_row IN
                SELECT table_name, column_name FROM information_schema.columns
                 WHERE table_schema = 'public' AND data_type = 'integer'
                   AND table_name IN ('comprehension_cycles','comprehension_submissions',
                     'comprehension_gaps','comprehension_followups','comprehension_scans')
                   AND (column_name = 'id' OR column_name LIKE '%\\_id' ESCAPE '\\')
              LOOP
                EXECUTE format('ALTER TABLE %I ALTER COLUMN %I TYPE bigint',
                  column_row.table_name, column_row.column_name);
              END LOOP;
            END $$"]]
    (jdbc/execute! db/ds [statement])))

(defn- encoded [value]
  (doto (PGobject.) (.setType "jsonb") (.setValue (json/generate-string value))))

(defn- decoded [value]
  (if (instance? PGobject value) (json/parse-string-strict (.getValue ^PGobject value) true) value))

(defn- decode-row [row]
  (when row (update-vals row decoded)))

(defn- string-keys [answers]
  (into {} (map (fn [[k v]] [(if (keyword? k) (name k) (str k)) v])) answers))

(defn- owned-cycle [connectable user-id cycle-id]
  (decode-row (jdbc/execute-one! connectable
                ["SELECT * FROM comprehension_cycles WHERE id = ? AND user_id = ?"
                 cycle-id user-id] opts)))

(defn- require-cycle [connectable user-id cycle-id]
  (or (owned-cycle connectable user-id cycle-id)
      (throw (ex-info "This study cycle is not available." {:type ::not-found}))))

(defn- owned-gap [connectable user-id gap-id]
  (some-> (jdbc/execute-one! connectable
            ["SELECT g.*, s.cycle_id, s.answers AS original_answers, s.grade AS original_grade,
                     c.form AS original_form FROM comprehension_gaps g
                JOIN comprehension_submissions s ON s.id = g.submission_id
                JOIN comprehension_cycles c ON c.id = s.cycle_id
               WHERE g.id = ? AND c.user_id = ?" gap-id user-id] opts)
    decode-row
    ((fn [gap]
       (let [question (first (filter #(= (:question_id gap) (:id %))
                              (get-in gap [:original_form :questions])))]
         (-> gap
           (assoc :original_answer (get (string-keys (:original_answers gap)) (:question_id gap))
             :original_prompt (:prompt question) :original-cycle-id (:cycle_id gap))
           (dissoc :original_answers :original_form)))))))

(defn- require-gap [connectable user-id gap-id]
  (or (owned-gap connectable user-id gap-id)
      (throw (ex-info "This learning gap is not available." {:type ::not-found}))))

(defn- result [f]
  (try (f)
       (catch Exception e {:ok false :error (or (ex-message e) "The study operation failed.")})))

(defn with-work!
  "Publish server work and its outcome. Pre: user-id comes from the server session."
  [user-id member f]
  (let [membership (us/get-atom user-id :comprehension-work)
        claimed (locking membership
                  (when-not (contains? @membership member)
                    (swap! (us/get-atom user-id :comprehension-errors) dissoc member)
                    (swap! membership conj member)
                    true))]
    (if-not claimed
      {:ok false :error "This study operation is already running. Refresh to inspect its result."}
      (do
        (us/work-started! user-id)
        (try
          (let [response (result f)]
            (when-not (:ok response)
              (swap! (us/get-atom user-id :comprehension-errors) assoc member (:error response)))
            response)
          (finally (swap! membership disj member)))))))

(defn list-materials
  "Return owned source roots. Post: no other user's source appears."
  [user-id]
  (jdbc/execute! db/ds
    ["SELECT t.id, t.title,
        (SELECT count(*) FROM kg_facts f WHERE f.graph_topic_id = t.id
          AND f.user_id = ? AND f.status = 'approved') AS fact_count
       FROM topics t WHERE user_id = ? AND parent_id IS NULL
       AND staged_delete_id IS NULL ORDER BY lower(title), id" user-id user-id] opts))

(defn extract-facts!
  "Reuse the existing fact extractor after checking source ownership."
  [user-id topic-id]
  (with-work! user-id [:extract topic-id]
    #(do
       (when-not (db/owns-topic? user-id topic-id)
         (throw (ex-info "Choose material from your account." {})))
       (let [response (kg-extract/distill-document! user-id topic-id)]
         (if (:success response)
           (assoc response :ok true)
           {:ok false :error (:error response)})))))

(defn list-cycles [user-id]
  (jdbc/execute! db/ds
    ["SELECT c.id, c.topic_id, c.title, c.target_minutes, c.parent_gap_id, c.created_at,
        (SELECT s.status FROM comprehension_submissions s
          WHERE s.cycle_id = c.id ORDER BY s.id DESC LIMIT 1) AS status,
        (SELECT count(*) FROM comprehension_gaps g JOIN comprehension_submissions s
          ON s.id = g.submission_id WHERE s.cycle_id = c.id) AS gap_count
       FROM comprehension_cycles c WHERE c.user_id = ? ORDER BY c.id DESC" user-id] opts))

(defn cycle-detail
  "Return one owned cycle with immutable attempts, scans, gaps, and follow-up history."
  [user-id cycle-id]
  (when-let [cycle (owned-cycle db/ds user-id cycle-id)]
    (let [submissions (mapv #(update (decode-row %) :answers string-keys)
                        (jdbc/execute! db/ds
                          ["SELECT * FROM comprehension_submissions WHERE cycle_id = ? ORDER BY id"
                           cycle-id] opts))
          scans (mapv decode-row
                  (jdbc/execute! db/ds
                    ["SELECT id, cycle_id, filename, content_type, page_count, suggestions,
                        uncertainties, accepted_assignments, accepted_at, status, error, created_at
                       FROM comprehension_scans WHERE cycle_id = ? ORDER BY id" cycle-id] opts))
          gaps (mapv (fn [row]
                       (assoc (decode-row row) :followups
                         (mapv decode-row (jdbc/execute! db/ds
                                           ["SELECT * FROM comprehension_followups WHERE gap_id = ? ORDER BY id"
                                            (:id row)] opts))))
                 (jdbc/execute! db/ds
                   ["SELECT g.* FROM comprehension_gaps g
                       JOIN comprehension_submissions s ON s.id = g.submission_id
                      WHERE s.cycle_id = ? ORDER BY g.id" cycle-id] opts))]
      (cond-> (assoc cycle :answers (string-keys (:draft_answers cycle))
                :submissions submissions :scans scans :gaps gaps)
        (:parent_gap_id cycle)
        (assoc :parent-gap (owned-gap db/ds user-id (:parent_gap_id cycle)))))))

(defn- source-snapshot [user-id topic-id]
  (when-not (db/owns-topic? user-id topic-id)
    (throw (ex-info "Choose material from your account." {:type ::not-found})))
  (let [sections (jdbc/execute! db/ds
                   ["WITH RECURSIVE tree AS (
                       SELECT id, title, content_text, page_number FROM topics
                        WHERE id = ? AND staged_delete_id IS NULL AND (user_id = ? OR user_id IS NULL)
                       UNION ALL
                       SELECT t.id, t.title, t.content_text, t.page_number
                         FROM topics t JOIN tree p ON t.parent_id = p.id
                        WHERE t.staged_delete_id IS NULL AND (t.user_id = ? OR t.user_id IS NULL))
                      SELECT * FROM tree ORDER BY page_number NULLS FIRST, id" topic-id user-id user-id] opts)
        title (:title (first (filter #(= topic-id (:id %)) sections)))
        evidence (mapv (fn [{:keys [id title content_text page_number]}]
                         {:topic-id id :title title :page page_number
                          :text (or content_text "")}) sections)
        facts (jdbc/execute! db/ds
                ["WITH RECURSIVE tree AS (
                    SELECT id FROM topics WHERE id = ? AND staged_delete_id IS NULL
                      AND (user_id = ? OR user_id IS NULL)
                    UNION ALL SELECT t.id FROM topics t JOIN tree p ON t.parent_id = p.id
                     WHERE t.staged_delete_id IS NULL AND (t.user_id = ? OR t.user_id IS NULL))
                   SELECT f.id, f.graph_topic_id, f.page_number, s.label AS subject_label,
                          p.label AS predicate_label, o.label AS object_label, f.object_literal
                     FROM kg_facts f JOIN tree t ON t.id = f.graph_topic_id
                     JOIN kg_entities s ON s.id = f.subject_entity_id
                     JOIN kg_predicates p ON p.id = f.predicate_id
                     LEFT JOIN kg_entities o ON o.id = f.object_entity_id
                    WHERE f.user_id = ? AND f.status = 'approved' ORDER BY f.id"
                 topic-id user-id user-id user-id] opts)]
    (when-not (some #(not (str/blank? (:text %))) evidence)
      (throw (ex-info "This material has no text. Scan or import its text first."
               {:type ::insufficient-evidence})))
    {:title (or title "Study cycle")
     :materials [{:material-id topic-id :title title :evidence evidence}]
     :facts facts}))

(defn- minutes! [minutes]
  (when-not (and (integer? minutes) (<= 5 minutes 180))
    (throw (ex-info "Choose an estimated duration from 5 through 180 minutes." {})))
  minutes)

(defn- insert-cycle! [user-id topic-id title minutes materials facts form parent-gap-id]
  (:id (jdbc/execute-one! db/ds
         ["INSERT INTO comprehension_cycles
            (user_id, topic_id, title, target_minutes, materials, facts, form, parent_gap_id)
          VALUES (?, ?, ?, ?, ?, ?, ?, ?) RETURNING id"
          user-id topic-id title minutes (encoded materials) (encoded facts)
          (encoded form) parent-gap-id] opts)))

(defn start-cycle!
  "Freeze owned sources before generation. Post: a saved form never follows later source edits."
  [user-id topic-id minutes]
  (with-work! user-id [:generate topic-id]
    #(let [minutes (minutes! minutes)
           {:keys [title materials facts]} (source-snapshot user-id topic-id)
           form (assessment/generate! user-id {:materials materials :facts facts}
                  {:target-minutes minutes})
           cycle-id (insert-cycle! user-id topic-id title minutes materials facts
                      (select-keys form [:blueprint :questions :evidence_packets]) nil)]
       {:ok true :cycle-id cycle-id})))

(defn normalize-answers!
  "Validate answer keys at the boundary. Missing answers remain explicit empty responses."
  [form answers]
  (when-not (map? answers)
    (throw (ex-info "Answers must identify their exam questions." {})))
  (let [answers (string-keys answers)
        question-ids (set (map :id (:questions form)))]
    (when-not (every? question-ids (keys answers))
      (throw (ex-info "An answer refers to a different exam question." {})))
    (when-not (every? #(and (string? %) (<= (count %) 50000)) (vals answers))
      (throw (ex-info "Each answer must be text with at most 50,000 characters." {})))
    (merge (zipmap question-ids (repeat "")) answers)))

(defn save-draft-answers! [user-id cycle-id answers]
  (result
    #(let [cycle (require-cycle db/ds user-id cycle-id)
           answers (normalize-answers! (:form cycle) answers)
           previous (jdbc/execute-one! db/ds
                      ["SELECT id FROM comprehension_submissions WHERE cycle_id = ? LIMIT 1" cycle-id] opts)]
       (jdbc/execute-one! db/ds
         ["UPDATE comprehension_cycles SET draft_answers = ?, draft_kind = ?
            WHERE id = ? AND user_id = ?"
          (encoded answers)
          (cond
            (= answers (string-keys (:draft_answers cycle))) (:draft_kind cycle)
            previous "answer-revision"
            :else "initial")
          cycle-id user-id])
       {:ok true :answers answers})))

(defn- answer-digest [answers scan-assignments]
  (let [bytes (.getBytes (json/generate-string [(into (sorted-map) answers) scan-assignments]) "UTF-8")]
    (apply str (map #(format "%02x" (bit-and 255 %))
                 (.digest (MessageDigest/getInstance "SHA-256") bytes)))))

(defn- prepare-submission! [user-id cycle-id answers]
  (jdbc/with-transaction [tx db/ds]
    (let [cycle (require-cycle tx user-id cycle-id)
          answers (normalize-answers! (:form cycle) answers)
          unreviewed (:n (jdbc/execute-one! tx
                           ["SELECT count(*) AS n FROM comprehension_scans
                              WHERE cycle_id = ? AND status <> 'accepted'" cycle-id] opts))
          scans (mapv decode-row
                  (jdbc/execute! tx
                    ["SELECT id, accepted_assignments FROM comprehension_scans
                       WHERE cycle_id = ? AND status = 'accepted' ORDER BY id" cycle-id] opts))
          previous (jdbc/execute-one! tx
                     ["SELECT id FROM comprehension_submissions
                        WHERE cycle_id = ? LIMIT 1" cycle-id] opts)
          revision (if previous
                     (if (= "transcription-correction" (:draft_kind cycle))
                       "transcription-correction" "answer-revision")
                     "initial")]
      (when (pos? unreviewed)
        (throw (ex-info "Review and accept each uploaded transcript before submitting answers." {})))
      (jdbc/execute-one! tx
        ["UPDATE comprehension_cycles SET draft_answers = ? WHERE id = ?"
         (encoded answers) cycle-id])
      (let [submission (decode-row
                         (jdbc/execute-one! tx
                           ["INSERT INTO comprehension_submissions
                              (cycle_id, answer_digest, answers, revision_kind, scan_assignments)
                            VALUES (?, ?, ?, ?, ?) ON CONFLICT (cycle_id, answer_digest)
                            DO UPDATE SET answer_digest = EXCLUDED.answer_digest RETURNING *"
                            cycle-id (answer-digest answers scans) (encoded answers)
                            revision (encoded scans)] opts))]
        {:cycle cycle :submission submission}))))

(defn- save-grade! [user-id cycle submission-id token grade]
  (jdbc/with-transaction [tx db/ds]
    (let [saved (jdbc/execute-one! tx
                  ["UPDATE comprehension_submissions SET status = 'graded', grade = ?, error = NULL
                     WHERE id = ? AND grading_token = ? AND status = 'grading' RETURNING id"
                   (encoded grade) submission-id token] opts)]
      (when-not saved
        (throw (ex-info "This grading run no longer owns the answer submission." {})))
      (doseq [{:keys [question_id criterion category reason evidence fact_ids]} (:gaps grade)]
        (jdbc/execute-one! tx
          ["INSERT INTO comprehension_gaps
             (submission_id, question_id, criterion, category, reason, evidence, fact_ids)
           VALUES (?, ?, ?, ?, ?, ?, ?)"
           submission-id question_id (name criterion) (name category) reason
           (encoded evidence) (encoded (or fact_ids []))]))
      (when-let [gap-id (:parent_gap_id cycle)]
        (require-gap tx user-id gap-id)
        (jdbc/execute-one! tx
          ["UPDATE comprehension_gaps SET status = 'reassessed', reassessment_submission_id = ?
             WHERE id = ?" submission-id gap-id]))
      (jdbc/execute-one! tx
        ["UPDATE comprehension_cycles SET draft_kind = 'answer-revision'
           WHERE id = ? AND user_id = ?" (:id cycle) user-id]))))

(defn- preserve-transcription-uncertainty [grade submission]
  (let [uncertain-questions (into #{}
                              (comp (mapcat :accepted_assignments)
                                (filter :uncertain) (map :question_id))
                              (when-not (= "answer-revision" (:revision_kind submission))
                                (:scan_assignments submission)))
        uncertain-questions (into uncertain-questions
                              (keep (fn [[question-id answer]]
                                      (when (re-find #"(?i)\[\s*(unclear|illegible|unreadable)[^\]]*\]" answer)
                                        question-id)))
                              (string-keys (:answers submission)))]
    (cond-> grade
      (seq uncertain-questions)
      (assoc :uncertain_questions (vec (sort uncertain-questions))
        :gaps (mapv (fn [gap]
                      (if (contains? uncertain-questions (:question_id gap))
                        (assoc gap :category "uncertain"
                          :reason (str "The transcript contains unreadable text. Confirm the original answer before interpreting this result. "
                                    (:reason gap)))
                        gap)) (:gaps grade))))))

(defn submit-answers!
  "Save answers before grading. Identical submissions retry the same attempt without duplicate gaps."
  [user-id cycle-id answers]
  (with-work! user-id [:grade cycle-id]
    #(let [{:keys [cycle submission]} (prepare-submission! user-id cycle-id answers)
           submission-id (:id submission)]
       (if (= "graded" (:status submission))
         {:ok true :cycle-id cycle-id :submission-id submission-id :grade (:grade submission)}
         (let [token (random-uuid)
               claimed (jdbc/execute-one! db/ds
                         ["UPDATE comprehension_submissions
                            SET status = 'grading', grading_token = ?, grading_started_at = now(), error = NULL
                            WHERE id = ? AND (status IN ('pending','failed') OR
                              (status = 'grading' AND (grading_started_at IS NULL OR
                                grading_started_at < now() - interval '20 minutes'))) RETURNING id"
                          token submission-id] opts)]
           (when-not claimed
             (throw (ex-info "These answers are already being graded. Refresh to inspect their result." {})))
           (try
             (let [form (assoc (:form cycle) :evidence_packets (:materials cycle))
                   grade (-> (assessment/grade! user-id form (string-keys (:answers submission)))
                           (preserve-transcription-uncertainty submission))]
               (save-grade! user-id cycle submission-id token grade)
               {:ok true :cycle-id cycle-id :submission-id submission-id :grade grade})
             (catch Exception e
               (jdbc/execute-one! db/ds
                 ["UPDATE comprehension_submissions SET status = 'failed', error = ?
                    WHERE id = ? AND grading_token = ? AND status = 'grading'"
                  (ex-message e) submission-id token])
               (throw e))))))))

(defn- practice-session! [tx user-id cycle fact-ids]
  (when (seq fact-ids)
    (let [placeholders (str/join "," (repeat (count fact-ids) "?"))
          questions (jdbc/execute! tx
                      (into [(str "SELECT DISTINCT q.id FROM kg_questions q
                                    JOIN kg_question_facts qf ON qf.question_id = q.id
                                    JOIN kg_facts f ON f.id = qf.fact_id
                                   WHERE q.user_id = ? AND f.user_id = ?
                                     AND q.status = 'approved' AND NOT q.suspended
                                     AND f.status = 'approved' AND qf.fact_id IN (" placeholders ")
                                   ORDER BY q.id LIMIT 10") user-id user-id] fact-ids) opts)]
      (when (seq questions)
        (:id (jdbc/execute-one! tx
               ["INSERT INTO kg_sessions (user_id, kind, scope_topic_ids, question_ids)
                 VALUES (?, 'quiz', CAST(? AS bigint[]), CAST(? AS bigint[])) RETURNING id"
                user-id (str "{" (str/join "," (keep :material-id (:materials cycle))) "}")
                (str "{" (str/join "," (map :id questions)) "}")] opts))))))

(defn save-followup!
  "Select work from an owned gap. Study completion never claims mastery or changes FSRS."
  [user-id gap-id kind]
  (result
    #(do
       (when-not (contains? #{"source-review" "targeted-questions"} kind)
         (throw (ex-info "Choose source review or practice questions." {})))
       (jdbc/with-transaction [tx db/ds]
         (let [gap (require-gap tx user-id gap-id)
               locked (jdbc/execute-one! tx
                        ["SELECT id FROM comprehension_gaps WHERE id = ? FOR UPDATE" gap-id] opts)
               cycle (require-cycle tx user-id (:cycle_id gap))
               questions (filterv (fn [q] (= (:id q) (:question_id gap)))
                           (get-in cycle [:form :questions]))
               existing (jdbc/execute-one! tx
                          ["SELECT * FROM comprehension_followups WHERE gap_id = ? AND kind = ?"
                           gap-id kind] opts)]
           (if existing
             {:ok true :followup (decode-row existing)}
             (let [session-id (when (= kind "targeted-questions")
                                (practice-session! tx user-id cycle (:fact_ids gap)))
                   followup (decode-row
                              (jdbc/execute-one! tx
                                ["INSERT INTO comprehension_followups (gap_id, kind, evidence, questions, session_id)
                                  VALUES (?, ?, ?, ?, ?) ON CONFLICT (gap_id, kind)
                                  DO UPDATE SET kind = EXCLUDED.kind RETURNING *"
                                 gap-id kind (encoded (:evidence gap))
                                 (encoded (if (= kind "targeted-questions") questions [])) session-id] opts))]
               {:ok true :followup followup})))))))

(defn finish-followup!
  "Record completed study. Post: the gap remains open to reassessment."
  [user-id followup-id]
  (result
    #(jdbc/with-transaction [tx db/ds]
       (let [row (jdbc/execute-one! tx
                   ["SELECT f.id, f.gap_id FROM comprehension_followups f
                      JOIN comprehension_gaps g ON g.id = f.gap_id
                      JOIN comprehension_submissions s ON s.id = g.submission_id
                      JOIN comprehension_cycles c ON c.id = s.cycle_id
                     WHERE f.id = ? AND c.user_id = ?" followup-id user-id] opts)]
         (when-not row (throw (ex-info "This study activity is not available." {})))
         (jdbc/execute-one! tx
           ["UPDATE comprehension_followups SET completed_at = COALESCE(completed_at, now()) WHERE id = ?"
            followup-id])
         (jdbc/execute-one! tx
           ["UPDATE comprehension_gaps SET status = 'studied'
              WHERE id = ? AND status = 'identified'" (:gap_id row)])
         {:ok true}))))

(defn reassess!
  "Generate new evidence for an owned gap from its frozen source and learning target."
  [user-id gap-id minutes]
  (with-work! user-id [:reassess gap-id]
    #(let [minutes (minutes! minutes)
           gap (require-gap db/ds user-id gap-id)
           cycle (require-cycle db/ds user-id (:cycle_id gap))
           question (first (filter (fn [q] (= (:id q) (:question_id gap)))
                             (get-in cycle [:form :questions])))
           target {:reason (:reason gap) :criterion (:criterion gap)
                   :question (:prompt question) :outcomes (:outcomes question)
                   :evidence (:evidence gap) :fact_ids (:fact_ids gap)}
           form (assessment/generate! user-id {:materials (:materials cycle) :facts (:facts cycle)}
                  {:target-minutes minutes :target-outcomes [target]})
           cycle-id (insert-cycle! user-id (:topic_id cycle) (:title cycle) minutes
                      (:materials cycle) (:facts cycle)
                      (select-keys form [:blueprint :questions :evidence_packets]) gap-id)]
       {:ok true :cycle-id cycle-id})))
