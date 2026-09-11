(ns freememo.curriculum-exam
  "Evidence-bound exam form generation and grading."
  (:require
   [cheshire.core :as json]
   [clojure.string :as str]
   [freememo.cards :as cards]
   [freememo.credits :as credits]
   [freememo.curriculum :as curriculum]
   [freememo.db :as db]
   [freememo.kg-llm :as llm]
   [freememo.llm-json :as llm-json]
   [freememo.toasts :as toasts]
   [freememo.user-state :as us]
   [missionary.core :as m]
   [next.jdbc :as jdbc]
   [next.jdbc.result-set :as rs]
   [taoensso.telemere :as tel])
  (:import
   [java.text Normalizer Normalizer$Form]
   [java.time Instant]
   [java.util.concurrent ExecutorService Executors TimeUnit]
   [missionary Cancelled]))

(def question-roles #{"explain" "compare" "apply" "evaluate" "synthesize"})
(def criterion-keys
  [:factual_accuracy :reasoning :source_evidence :integration :completeness])

(defn- non-blank? [value]
  (and (string? value) (not (str/blank? value))))

(defn- normalize-question [index question]
  (let [role (some-> (:role question) name str/lower-case)
        rubric (:rubric question)
        evidence (:evidence question)]
    (when-not (contains? question-roles role)
      (throw (ex-info "The model returned an unsupported question role."
               {:type ::invalid-form :question index :field :role})))
    (when-not (non-blank? (:prompt question))
      (throw (ex-info "The model returned a blank exam question."
               {:type ::invalid-form :question index :field :prompt})))
    (when-not (and (map? rubric) (every? #(non-blank? (get rubric %)) criterion-keys))
      (throw (ex-info "The model returned an incomplete question rubric."
               {:type ::invalid-form :question index :field :rubric})))
    (when-not (and (vector? evidence) (seq evidence)
                (every? #(and (integer? (:material_id %))
                           (integer? (:topic_id %))
                           (non-blank? (:quote %))) evidence))
      (throw (ex-info "The model returned a question without source evidence."
               {:type ::invalid-form :question index :field :evidence})))
    ;; The grading guide repeats the required rubric. Some structured-output
    ;; providers omit this redundant field. Preserve the complete rubric as the
    ;; guide instead of discarding a valid five-question form.
    (assoc question
      :id (or (:id question) (str (random-uuid)))
      :role role
      :grading_guide (if (non-blank? (:grading_guide question))
                       (:grading_guide question)
                       (str/join " " (map rubric criterion-keys))))))

(defn validate-generated-form!
  "Validate the model contract and return a normalized form.
   An explicit insufficiency becomes a typed failure with user guidance."
  [form]
  (when (:insufficient form)
    (throw (ex-info
             (str (or (:reason form) "The source cannot support five essay questions.")
               " Add material, merge it with another block, or remove this exam subject.")
             {:type ::insufficient-evidence})))
  (let [questions (:questions form)
        blueprint (:blueprint form)]
    (when-not (= 5 (count questions))
      (throw (ex-info "The source must support exactly five essay questions."
               {:type ::invalid-form :question-count (count questions)})))
    (when-not (map? blueprint)
      (throw (ex-info "The model returned no exam blueprint."
               {:type ::invalid-form :field :blueprint})))
    (when-not (and (vector? (:assessed_outcomes blueprint))
                (vector? (:unassessed_outcomes blueprint)))
      (throw (ex-info "The exam blueprint must account for assessed and unassessed outcomes."
               {:type ::invalid-form :field :blueprint-outcomes})))
    (let [questions (mapv normalize-question (range 1 6) questions)]
      (when-not (= 5 (count (set (map (comp str/lower-case str/trim :prompt) questions))))
        (throw (ex-info "The source must support five distinct essay questions."
                 {:type ::invalid-form :field :duplicate-questions})))
      {:blueprint blueprint :questions questions})))

(def ^:private citation-punctuation
  {\‘ "'" \’ "'" \‚ "'" \‛ "'" \ʼ "'"
   \“ "\"" \” "\"" \„ "\"" \‟ "\""
   \‐ "-" \‑ "-" \‒ "-" \– "-" \— "-" \― "-" \− "-"
   \… "..."})

(defn- normalize-citation-text
  "Return a comparison-only form of source evidence.

   Pre: text is a source string or a model citation string.
   Post: Unicode compatibility forms, typographic punctuation, and whitespace
   have canonical forms.
   Invariant: persisted source text and citations remain unchanged. The caller
   must still require exact containment inside the declared material and topic.
   A failed comparison is blamed on the model citation, not the frozen source."
  [text]
  (-> (Normalizer/normalize text Normalizer$Form/NFKC)
    (str/escape citation-punctuation)
    (str/replace #"[\p{Z}\s]+" " ")
    str/trim))

(defn- validate-citations!
  "Require each citation to occur in its declared frozen source section.

   Pre: materials contain frozen evidence and questions contain normalized form
   data from the model.
   Post: returns questions when every normalized quote is an exact substring.
   Invariant: normalization permits formatting changes only. It does not permit
   paraphrases, cross-topic matches, or invented evidence. A mismatch is blamed
   on the generated form."
  [materials questions]
  (let [source-text (into {}
                      (mapcat
                        (fn [{:keys [material-id evidence]}]
                          (map (fn [{:keys [topic-id text]}]
                                 [[material-id topic-id]
                                  (normalize-citation-text text)]) evidence))
                        materials))]
    (doseq [[question-index question] (map-indexed vector questions)
            {:keys [material_id topic_id quote]} (:evidence question)]
      (when-not (some-> (get source-text [material_id topic_id])
                  (str/includes? (normalize-citation-text quote)))
        (throw (ex-info "The model cited text that is not in the frozen source."
                 {:type ::invalid-form :question (inc question-index)
                  :material-id material_id :topic-id topic_id}))))
    questions))

(defn- evidence-payload [bundles]
  (let [per-material-budget (max 12000 (quot 120000 (max 1 (count bundles))))]
    (mapv
      (fn [{:keys [material-id topic-id digest evidence]}]
        (let [[sections _]
              (reduce
                (fn [[rows remaining] {:keys [topic-id title kind page text]}]
                  (let [take-count (min 5000 remaining (count text))]
                    [(conj rows
                       (cond-> {:topic_id topic-id :title title :kind kind :page page}
                         (pos? take-count) (assoc :text (subs text 0 take-count))
                         (< take-count (count text)) (assoc :text_omitted true)))
                     (- remaining take-count)]))
                [[] per-material-budget] evidence)]
          {:material_id material-id
           :topic_id topic-id
           :digest digest
           :sections sections}))
      bundles)))

(defn- latest-weaknesses [user-id block-id]
  (let [row (jdbc/execute-one! db/ds
              ["SELECT g.criteria
                  FROM curriculum_grade_revisions g
                  JOIN curriculum_exam_sittings s ON s.id = g.sitting_id
                  JOIN curriculum_exam_forms f ON f.id = s.form_id
                 WHERE s.user_id = ? AND f.block_id = ? AND NOT g.passed
                 ORDER BY g.created_at DESC LIMIT 1" user-id block-id]
              {:builder-fn rs/as-unqualified-maps})
        raw (:criteria row)
        parsed (cond
                 (nil? raw) nil
                 (string? raw) (json/parse-string raw true)
                 (instance? org.postgresql.util.PGobject raw)
                 (json/parse-string (.getValue ^org.postgresql.util.PGobject raw) true)
                 :else raw)]
    (vec (distinct (mapcat :weaknesses parsed)))))

(defn form-generation-work-key
  "Return the activity key for one material or block exam subject.

   Pre: subject contains exactly one positive material or block id.
   Post: returns `material/<id>` or `block/<id>`.
   Invariant: the key identifies one exam subject inside one user's state."
  [{:keys [material-id block-id]}]
  (when (= (some? material-id) (some? block-id))
    (throw (ex-info "Choose exactly one exam subject." {:type ::invalid-subject})))
  (let [[kind id] (if material-id ["material" material-id] ["block" block-id])]
    (when-not (and (integer? id) (pos? id))
      (throw (ex-info "The exam subject id must be positive."
               {:type ::invalid-subject :id id})))
    (str kind "/" id)))

(defn- generation-cancelled?
  [user-id work-key]
  (contains? @(us/get-atom user-id :exam-form-generation-cancelled) work-key))

(defn generate-form!
  "Generate and persist one five-question draft.

   Pre: the user owns subject. The caller owns an invalid or unowned subject.
   Post: each returned model cost is charged once. A successful result identifies
   the saved draft and its charge.
   Invariant: output validation cannot suppress a returned model cost. A cancel
   after the model call prevents form persistence. This function owns charging
   and form persistence."
  [user-id subject form-kind threshold]
  (try
    (let [operation-started-at (Instant/now)
          work-key (form-generation-work-key subject)
          {:keys [materials source-versions]}
          (curriculum/subject-evidence db/ds user-id subject)
          retry? (= "targeted-retry" form-kind)
          weaknesses (when retry? (latest-weaknesses user-id (:block-id subject)))
          {:keys [api-key entry model-fields]} (llm/resolve-model+gate! user-id :synthesis)
          payload {:exam_kind (if (:material-id subject) "material" "block")
                   :form_kind (or form-kind "standard")
                   :question_count 5
                   :threshold (or threshold curriculum/default-threshold)
                   :weaknesses weaknesses
                   :evidence (evidence-payload materials)}
          {:keys [parsed cost]} (llm/chat! api-key model-fields
                                  (cards/load-prompt-template "curriculum-exam-form.md")
                                  (llm-json/write-prompt-input payload)
                                  :form :map
                                  {:feature :curriculum-exam-form :user-id user-id})
          charge (credits/record-cost-charge! user-id :curriculum.exam-form
                   (:id entry) cost)
          {:keys [blueprint questions]} (validate-generated-form! parsed)
          questions (validate-citations! materials questions)]
      ;; OpenRouter ignores thread interruption during its socket read. This
      ;; check prevents a cancelled request from saving its late result.
      (if (generation-cancelled? user-id work-key)
        {:success false :cancelled? true :charged-credits charge}
        (let [form-id (curriculum/save-draft-form! user-id subject
                        {:form-kind (or form-kind "standard")
                         :threshold (or threshold curriculum/default-threshold)
                         :blueprint (assoc blueprint :evidence_packets (:evidence payload))
                         :questions questions
                         :source-versions source-versions
                         :cost nil
                         :operation-started-at operation-started-at})]
          (if form-id
            (do (curriculum/record-form-generation-charge! user-id form-id charge)
                {:success true :form-id form-id :charged-credits charge})
            {:success false :cancelled? true :charged-credits charge}))))
    (catch Exception error
      (when-not (llm/insufficient-credits? error)
        (tel/error! {:id ::generate-form :data {:user-id user-id :subject subject}} error))
      {:success false
       :error (ex-message (llm/root-cause error))
       :error-type (cond
                     (llm/insufficient-credits? error) :insufficient-credits
                     (= ::insufficient-evidence (:type (ex-data error))) :insufficient-evidence
                     :else :generation-failed)})))

(defonce ^:private form-generation-executor (Executors/newFixedThreadPool 2))

(def ^:private form-generation-timeout-ms
  (* 20 60 1000))

(def ^:private form-generation-shutdown-wait-ms
  (+ form-generation-timeout-ms 30000))

(defn- drain-form-generation-executor!
  "Stop accepting form-generation work and wait for accepted work.

   Pre: executor owns form-generation tasks. The caller owns an invalid timeout.
   Post: returns :drained when accepted tasks finish, :timed-out after forced
   cancellation, or :interrupted when the waiting thread is interrupted.
   Invariant: accepted work is not cancelled before the timeout. This function
   owns an early cancellation or an executor left accepting work."
  [^ExecutorService executor timeout-ms]
  (.shutdown executor)
  (try
    (if (.awaitTermination executor timeout-ms TimeUnit/MILLISECONDS)
      :drained
      (do
        (.shutdownNow executor)
        :timed-out))
    (catch InterruptedException _
      (.shutdownNow executor)
      (.interrupt (Thread/currentThread))
      :interrupted)))

(defn await-form-generation-shutdown!
  "Drain accepted exam form work during production shutdown.

   Pre: the JVM has started shutdown. The production hook owns call timing.
   Post: returns after all accepted work finishes or the shutdown wait expires.
   Invariant: the wait exceeds the task timeout. This function owns a shorter
   wait that lets deployment discard an accepted model call."
  []
  (let [outcome (drain-form-generation-executor!
                  form-generation-executor form-generation-shutdown-wait-ms)]
    (tel/log! {:level (if (= :drained outcome) :info :warn)
               :id ::form-generation-shutdown
               :data {:outcome outcome}}
      "Exam form generation shutdown finished")
    outcome))

(defn- finish-form-generation!
  "Clear the live owner state and publish the result.

   Pre: result is the terminal task result for work-key.
   Post: live membership and its canceller are absent.
   Invariant: an error is stored before membership is removed."
  [user-id work-key result]
  (when (and (not (:success result))
          (not (:cancelled? result)))
    (swap! (us/get-atom user-id :exam-form-generation-errors)
      assoc work-key (:error result)))
  (swap! (us/get-atom user-id :exam-form-generation-cancellers) dissoc work-key)
  (swap! (us/get-atom user-id :exam-form-generations) disj work-key)
  (cond
    (:success result)
    (do
      (tel/log! {:level :info :id ::form-generation-complete
                 :data {:user-id user-id :work-key work-key
                        :form-id (:form-id result)}}
        "Exam form generation completed")
      (toasts/push! user-id
        {:level :success :message "The exam form is ready."}))

    (:cancelled? result)
    nil

    :else
    (do
      (tel/log! {:level :warn :id ::form-generation-failed
                 :data {:user-id user-id :work-key work-key
                        :error-type (:error-type result)}}
        "Exam form generation failed")
      (toasts/push! user-id
        {:level :error
         :message (or (:error result) "Exam form generation failed.")
         :actions (if (= :insufficient-credits (:error-type result))
                    [{:label "Top up credits" :nav :settings}]
                    [])})))
  nil)

(defn start-form-generation!
  "Start one server-owned exam form generation task.

   Pre: the user owns subject. The threshold is from 1 to 100.
   Post: returns after task submission. The model result arrives independently.
   Invariant: at most one task is published for each user and exam subject."
  [user-id subject form-kind threshold]
  (try
    (when-not (and (integer? threshold) (<= 1 threshold 100))
      (throw (ex-info "The exam threshold must be from 1 to 100."
               {:type ::invalid-threshold :threshold threshold})))
    (let [work-key (form-generation-work-key subject)
          generations (us/get-atom user-id :exam-form-generations)
          [before _after] (swap-vals! generations conj work-key)]
      (if (contains? before work-key)
        {:success true :started false :work-key work-key}
        (let [cancelled (us/get-atom user-id :exam-form-generation-cancelled)
              cancellers (us/get-atom user-id :exam-form-generation-cancellers)
              cancel-task (atom nil)
              owner-cancel
              (fn []
                (swap! cancelled conj work-key)
                (when-some [cancel @cancel-task] (cancel)))]
          (try
            (swap! cancelled disj work-key)
            (swap! (us/get-atom user-id :exam-form-generation-errors) dissoc work-key)
            (swap! cancellers assoc work-key owner-cancel)
            (tel/log! {:level :info :id ::form-generation-started
                       :data {:user-id user-id :work-key work-key}}
              "Exam form generation started")
            (us/work-started! user-id)
            (let [cancel
                  ((m/timeout
                     (m/via form-generation-executor
                       (generate-form! user-id subject form-kind threshold))
                     form-generation-timeout-ms
                     {:success false
                      :timed-out? true
                      :error-type :generation-timeout
                      :error "Exam form generation timed out after 20 minutes."})
                   (fn [result]
                     (when (:timed-out? result)
                       (swap! cancelled conj work-key))
                     (finish-form-generation! user-id work-key result))
                   (fn [error]
                     (if (or (instance? Cancelled error)
                           (generation-cancelled? user-id work-key))
                       (do
                         (swap! cancellers dissoc work-key)
                         (swap! generations disj work-key))
                       (finish-form-generation!
                         user-id work-key
                         {:success false :error (ex-message error)
                          :error-type :generation-failed}))))]
              (reset! cancel-task cancel)
              (when (generation-cancelled? user-id work-key) (cancel))
              {:success true :started true :work-key work-key})
            (catch Throwable error
              (swap! cancellers dissoc work-key)
              (swap! generations disj work-key)
              (throw error))))))
    (catch Exception error
      {:success false :error (ex-message error)})))

(defn- criterion-score [question-index scores criterion]
  (let [score (get scores criterion)]
    (when-not (and (integer? score) (<= 0 score 4))
      (throw (ex-info "The model returned an invalid criterion score."
               {:type ::invalid-grade :question question-index :criterion criterion})))
    score))

(defn validate-grade!
  "Validate all 25 scores and compute the authoritative raw score."
  [grade blueprint]
  (let [question-grades (:question_grades grade)
        source-texts (for [packet (:evidence_packets blueprint)
                           section (:sections packet)
                           :when (string? (:text section))]
                       (:text section))]
    (when-not (= 5 (count question-grades))
      (throw (ex-info "The model must grade all five questions."
               {:type ::invalid-grade :question-count (count question-grades)})))
    (when-not (non-blank? (:explanation grade))
      (throw (ex-info "The model returned no overall grade explanation."
               {:type ::invalid-grade})))
    (let [normalized
          (mapv
            (fn [index question-grade]
              (let [scores (:criteria question-grade)
                    deductions (:deductions question-grade)]
                (when-not (and (vector? deductions)
                            (every? #(and (non-blank? (:criterion %))
                                       (non-blank? (:reason %))
                                       (non-blank? (:citation %))
                                       (some (fn [text]
                                               (str/includes? text (:citation %)))
                                         source-texts)) deductions))
                  (throw (ex-info "Every deduction must cite frozen evidence."
                           {:type ::invalid-grade :question index})))
                (let [normalized-scores
                      (into {} (map (fn [criterion]
                                     [criterion (criterion-score index scores criterion)]))
                                 criterion-keys)
                      deducted-criteria (into #{} (map #(keyword (:criterion %))) deductions)]
                  (doseq [[criterion score] normalized-scores
                          :when (< score 4)]
                    (when-not (contains? deducted-criteria criterion)
                      (throw (ex-info "Every lost point needs an evidence citation."
                               {:type ::invalid-grade :question index
                                :criterion criterion}))))
                  (assoc question-grade :criteria normalized-scores))))
            (range 1 6) question-grades)
          raw-score (reduce + (mapcat #(vals (:criteria %)) normalized))]
      {:raw-score raw-score
       :criteria normalized
       :explanation (:explanation grade)})))

(defn grade-sitting!
  "Grade one submitted sitting and append an immutable revision.

   Pre: the user owns a submitted sitting. The caller owns an invalid id.
   Post: each returned model cost is charged once. Valid output adds one Grade
   Revision. Invalid output adds no revision.
   Invariant: output validation cannot suppress a returned model cost. A failed
   call changes no sitting, answer, or progression data. This function owns the
   charge and Grade Revision transition."
  [user-id sitting-id]
  (try
    (let [operation-started-at (Instant/now)
          sitting (or (curriculum/sitting-for-grade user-id sitting-id)
                    (throw (ex-info "Submitted exam sitting not found."
                             {:type ::missing-sitting})))
          {:keys [api-key entry model-fields]} (llm/resolve-model+gate! user-id :grade)
          payload {:questions (:questions sitting)
                   :blueprint (:blueprint sitting)
                   :answers (:answers sitting)
                   :threshold (:threshold sitting)}
          {:keys [parsed cost]} (llm/chat! api-key model-fields
                                  (cards/load-prompt-template "curriculum-exam-grade.md")
                                  (llm-json/write-prompt-input payload)
                                  :grade :map
                                  {:feature :curriculum-exam-grade :user-id user-id})
          charge (credits/record-cost-charge! user-id :curriculum.exam-grade
                   (:id entry) cost)
          grade (validate-grade! parsed (:blueprint sitting))
          revision-id (curriculum/record-grade-revision! user-id sitting-id
                        (assoc grade :cost nil :operation-started-at operation-started-at))]
      (if revision-id
        (do (curriculum/record-grade-charge! user-id revision-id charge)
            {:success true :revision-id revision-id :charged-credits charge
             :raw-score (:raw-score grade)
             :passed (>= (:raw-score grade) (:threshold sitting))})
        {:success false :cancelled? true :charged-credits charge}))
    (catch Exception error
      (when-not (llm/insufficient-credits? error)
        (tel/error! {:id ::grade-sitting
                     :data {:user-id user-id :sitting-id sitting-id}} error))
      {:success false
       :error (ex-message (llm/root-cause error))
       :error-type (if (llm/insufficient-credits? error)
                     :insufficient-credits :grading-failed)})))
