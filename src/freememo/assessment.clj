(ns freememo.assessment
  "Source-bound mixed assessments and diagnostic grading."
  (:require [clojure.string :as str]
            [freememo.cards :as cards]
            [freememo.credits :as credits]
            [freememo.kg-llm :as llm]
            [freememo.llm-json :as llm-json])
  (:import [java.text Normalizer Normalizer$Form]))

(def formats #{"fill-in-the-blank" "multiple-choice" "short-essay" "long-essay"})
(def criterion-keys [:factual_accuracy :reasoning :source_evidence :integration :completeness])
(def gap-categories #{"recall" "connection" "reasoning" "uncertain"})
(defn- nonblank? [x] (and (string? x) (not (str/blank? x))))
(defn- fail! [message fields] (throw (ex-info message (assoc fields :type ::invalid-assessment))))

(defn- normalize-typography
  "Normalize punctuation and whitespace without changing Unicode notation.
   Pre: text is a string. Post: spacing and quotation marks use a common form.
   Invariant: this function preserves letter case and numeric script positions."
  [text]
  (-> text
      (str/escape {\‘ "'" \’ "'" \“ "\"" \” "\"" \– "-" \— "-" \… "..."})
      (str/replace #"[\p{Z}\s]+" " ") str/trim))

(defn normalize-text [text]
  (normalize-typography (Normalizer/normalize (str text) Normalizer$Form/NFKC)))

(defn- normalize-objective-answer
  "Normalize answer typography without folding compatibility characters.
   Pre: text is an answer or an option label. Post: canonical spellings compare equally.
   Invariant: superscripts, subscripts, and letter case retain their meaning."
  [text]
  (normalize-typography (Normalizer/normalize (str text) Normalizer$Form/NFC)))

(defn evidence-materials
  "Read current material packets and legacy section packets."
  [packets]
  (mapv (fn [packet]
          {:material-id (or (:material-id packet) (:material_id packet))
           :title (:title packet)
           :evidence (mapv (fn [section]
                             (assoc section :topic-id (or (:topic-id section) (:topic_id section))))
                       (or (:evidence packet) (:sections packet)))}) packets))

(defn validate-citations!
  "Require citations in their declared frozen material and section.
   Pre: materials are frozen source packets. Post: every citation has a source.
   Invariant: model text cannot change the frozen source."
  ([materials citations] (validate-citations! materials citations {}))
  ([materials citations fields]
   (let [sources (into {} (mapcat (fn [{:keys [material-id evidence]}]
                                          (map (fn [{:keys [topic-id text]}]
                                                 [[material-id topic-id] (normalize-text text)]) evidence)) materials))]
     (doseq [{:keys [material_id topic_id quote]} citations]
       (when-not (and (integer? material_id) (integer? topic_id) (nonblank? quote)
                     (some-> (get sources [material_id topic_id])
                             (str/includes? (normalize-text quote))))
         (fail! "A quoted passage does not match the material word for word."
           (assoc fields :material-id material_id :topic-id topic_id)))))
   citations))

(defn- normalize-question [index question valid-fact-ids]
  (let [format (or (:format question) "long-essay")
        points (or (:points question) 20)
        rubric (:rubric question)
        objective? (contains? #{"fill-in-the-blank" "multiple-choice"} format)
        options (:options question)]
    (when-not (and (contains? formats format) (nonblank? (:prompt question))
                  (integer? points) (<= 1 points 1000))
      (fail! "A question has no text, an unknown format, or no points." {:question index}))
    (when-not (and (vector? (:evidence question)) (seq (:evidence question)))
      (fail! "A question quotes nothing from the material." {:question index}))
    (when (and (not objective?)
               (not (and (map? rubric) (seq rubric)
                         (every? #(and (contains? (set criterion-keys) (key %)) (nonblank? (val %))) rubric))))
      (fail! "An essay question has no marking criteria." {:question index}))
    (when (and (= format "fill-in-the-blank")
               (not (and (vector? (:accepted_answers question)) (seq (:accepted_answers question))
                         (every? nonblank? (:accepted_answers question)))))
      (fail! "A fill-in-the-blank question has no accepted answers." {:question index}))
    (when (and (= format "multiple-choice")
               (not (and (vector? options) (<= 2 (count options) 8)
                         (every? #(and (nonblank? (:id %)) (nonblank? (:text %))) options)
                         (= (count options) (count (set (map #(str/lower-case (normalize-objective-answer (:id %))) options))))
                         (= (count options) (count (set (map #(normalize-objective-answer (:text %)) options))))
                         (some #(= (:id %) (:correct_option_id question)) options))))
      (fail! "A multiple-choice question has repeated options or no correct option." {:question index}))
    (assoc question :id (str (or (:id question) (str "q" index))) :format format :points points
      :rubric (or rubric {})
      :fact_ids (vec (filter valid-fact-ids (:fact_ids question)))
      :grading_guide (or (:grading_guide question) (str/join " " (vals rubric))))))

(defn- validate-blueprint! [blueprint questions evidence]
  (let [used (set (map :format questions))
        omitted (set (map :format (:omitted_formats blueprint)))
        ids (set (map :id questions))]
    (when-not (= (set (remove used formats)) omitted)
      (fail! "The model did not say why it left out a question format." {:field :omitted-formats}))
    (doseq [outcome (:assessed_outcomes blueprint)]
      (when-not (and (nonblank? (:id outcome)) (nonblank? (:statement outcome))
                    (seq (:question_ids outcome)) (every? ids (:question_ids outcome))
                    (seq (:evidence outcome)))
        (fail! "A learning outcome is not linked to its questions and quotes." {:field :assessed-outcomes}))
      (when evidence (validate-citations! (:materials evidence) (:evidence outcome))))
    (when-not (and (seq (:assessed_outcomes blueprint))
                   (= ids (set (mapcat :question_ids (:assessed_outcomes blueprint)))))
      (fail! "A question is not linked to a learning outcome." {:field :assessed-outcomes}))
    (doseq [outcome (:unassessed_outcomes blueprint)]
      (when-not (and (nonblank? (:statement outcome)) (nonblank? (:reason outcome)))
        (fail! "The model did not say why it left out a learning outcome." {:field :unassessed-outcomes})))
    blueprint))

(defn- question-points
  "Points a question claims, or 0 when the model gave none usable."
  [question]
  (let [points (or (:points question) 20)]
    (if (and (integer? points) (pos? points)) points 0)))

(defn- invalid-reason
  "Run check. Return nil when it passes, else the ex-data of its refusal with :reason."
  [check]
  (try (check) nil
       (catch clojure.lang.ExceptionInfo e
         (if (= ::invalid-assessment (:type (ex-data e)))
           (assoc (ex-data e) :reason (ex-message e))
           (throw e)))))

(defn- salvage-outcomes
  "Keep the assessed outcomes that still have kept questions and valid quotes.
   Post: [assessed unassessed-additions]. An outcome that loses either moves to
   unassessed with a reason that says which."
  [blueprint kept-ids materials]
  (reduce
    (fn [[assessed unassessed] outcome]
      (let [question-ids (into [] (comp (map str) (filter kept-ids)) (:question_ids outcome))
            evidence (if materials
                       (filterv #(nil? (invalid-reason (fn [] (validate-citations! materials [%]))))
                         (:evidence outcome))
                       (vec (:evidence outcome)))
            statement (:statement outcome)]
        (cond
          (and (nonblank? (:id outcome)) (nonblank? statement) (seq question-ids) (seq evidence))
          [(conj assessed (assoc outcome :question_ids question-ids :evidence evidence)) unassessed]

          (not (nonblank? statement))
          [assessed unassessed]

          (empty? question-ids)
          [assessed (conj unassessed {:statement statement
                                      :reason "The questions written for this outcome did not pass the checks."})]

          :else
          [assessed (conj unassessed {:statement statement
                                      :reason "The quotes given for this outcome do not match the material."})])))
    [[] []] (:assessed_outcomes blueprint)))

(defn- salvage-omissions
  "List every format the kept questions do not use, each with a reason."
  [blueprint original-questions kept-questions]
  (let [used (set (map #(or (:format %) "long-essay") kept-questions))
        attempted (set (map #(or (:format %) "long-essay") original-questions))
        given (into {} (keep (fn [{:keys [format reason]}] (when (nonblank? reason) [format reason])))
                (:omitted_formats blueprint))]
    (vec (for [format (sort (remove used formats))]
           {:format format
            :reason (cond
                      (attempted format) "The questions in this format did not pass the checks."
                      (given format) (given format)
                      :else "The model gave no reason for leaving out this format.")}))))

(defn- salvage-form
  "Drop the generated questions that fail their own checks, and keep the rest.
   Pre: the form has a blueprint and from 1 to 40 questions.
   Post: a form the strict checks accept, with :dropped_questions in its blueprint;
   or a throw with the first question's reason when no question survives.
   Invariant: a kept question is unchanged except for a fixed :id."
  [form evidence]
  (let [facts (set (keep :id (:facts evidence)))
        materials (:materials evidence)
        checked
        (reduce
          (fn [{:keys [ids prompts] :as acc} [index question]]
            (let [problem (invalid-reason
                            #(let [q (normalize-question index question facts)]
                               (when materials
                                 (validate-citations! materials (:evidence q) {:question index}))
                               q))
                  normalized (when-not problem (normalize-question index question facts))
                  prompt-key (some-> (:prompt normalized) normalize-text str/lower-case)
                  problem (or problem
                            (when (or (contains? ids (:id normalized)) (contains? prompts prompt-key))
                              {:question index :reason "Two questions have the same ID or the same text."}))]
              (if problem
                (update acc :dropped conj {:question index :prompt (str (:prompt question)) :reason (:reason problem)})
                (-> acc
                    (update :kept conj [index (assoc question :id (:id normalized))])
                    (update :ids conj (:id normalized))
                    (update :prompts conj prompt-key)))))
          {:kept [] :dropped [] :ids #{} :prompts #{}}
          (map vector (range 1 (inc (count (:questions form)))) (:questions form)))
        blueprint (:blueprint form)
        linked? (some :format (:questions form))
        [assessed unassessed] (if linked?
                                (salvage-outcomes blueprint (set (map (comp :id second) (:kept checked))) materials)
                                [(:assessed_outcomes blueprint) []])
        covered (set (map str (mapcat :question_ids assessed)))
        unlinked (if linked? (remove #(contains? covered (:id (second %))) (:kept checked)) [])
        kept (mapv second (remove (set unlinked) (:kept checked)))
        dropped (->> (concat (:dropped checked)
                       (map (fn [[index question]]
                              {:question index :prompt (str (:prompt question))
                               :reason "A question is not linked to a learning outcome."}) unlinked))
                  (sort-by :question) vec)
        total (reduce + (map question-points (:questions form)))
        estimate (:estimated_minutes blueprint)]
    (when (empty? kept)
      (let [{:keys [question reason]} (first dropped)]
        (fail! reason {:question question})))
    {:blueprint (cond-> (assoc blueprint
                          :assessed_outcomes assessed
                          :unassessed_outcomes (into (vec (:unassessed_outcomes blueprint)) unassessed)
                          :omitted_formats (salvage-omissions blueprint (:questions form) kept)
                          :dropped_questions dropped)
                  (and (seq dropped) (integer? estimate) (pos? estimate) (pos? total))
                  (assoc :estimated_minutes
                    (max 1 (Math/round (double (* estimate (/ (reduce + (map question-points kept)) total)))))))
     :questions kept}))

(defn validate-form!
  "Validate a generated form before persistence.
   Pre: evidence contains frozen materials and approved facts.
   Post: questions have stable ids and source-bound criteria. With
   `:drop-invalid? true` in options, a question that fails its own checks is
   removed and listed in the blueprint's :dropped_questions instead of refusing
   the form; the form is refused only when no question survives.
   Invariant: unsupported formats are omitted, never invented."
  ([form] (validate-form! form nil {}))
  ([form evidence options]
   (when (:insufficient form)
     (throw (ex-info (str (or (:reason form) "The source cannot support this assessment.")
                      " Add material or choose a broader source.") {:type ::insufficient-evidence})))
   (when-not (and (map? (:blueprint form))
                 (vector? (get-in form [:blueprint :assessed_outcomes]))
                 (vector? (get-in form [:blueprint :unassessed_outcomes])))
     (fail! "The model did not list the learning outcomes." {:field :blueprint}))
   (when-not (and (vector? (:questions form)) (<= 1 (count (:questions form)) 40))
     (fail! "An assessment must contain from 1 to 40 questions." {:field :questions}))
   (let [form (if (:drop-invalid? options) (salvage-form form evidence) form)
         facts (set (keep :id (:facts evidence)))
         questions (mapv #(normalize-question %1 %2 facts)
                     (range 1 (inc (count (:questions form)))) (:questions form))
         blueprint (assoc (:blueprint form)
                     :target_minutes (or (:target-minutes options) 30)
                     :estimated_minutes (or (get-in form [:blueprint :estimated_minutes]) (:target-minutes options) 30)
                     :omitted_formats (vec (or (get-in form [:blueprint :omitted_formats]) [])))
         blueprint (if evidence (assoc blueprint :evidence_packets (:materials evidence) :facts (:facts evidence)) blueprint)]
     (when-not (= (count questions) (count (set (map :id questions)))
                  (count (set (map #(str/lower-case (normalize-text (:prompt %))) questions))))
       (fail! "Two questions have the same ID or the same text." {:field :duplicate-questions}))
     (when-not (and (integer? (:estimated_minutes blueprint)) (pos? (:estimated_minutes blueprint)))
       (fail! "The model gave no time estimate." {:field :estimated-minutes}))
     (when evidence
       (doseq [[index question] (map-indexed vector questions)]
         (validate-citations! (:materials evidence) (:evidence question) {:question (inc index)})))
     (when (some :format (:questions form)) (validate-blueprint! blueprint questions evidence))
     (doseq [omission (:omitted_formats blueprint)]
       (when-not (and (contains? formats (:format omission)) (nonblank? (:reason omission)))
         (fail! "The model did not say why it left out a question format." {:field :omitted-formats})))
     {:blueprint blueprint :questions questions})))

(defn- fact-in-excerpts? [materials fact]
  (let [normalize #(str/lower-case (normalize-text %))
        subject (:subject_label fact)
        object (or (:object_label fact) (:object_literal fact))
        sections (mapcat (fn [material]
                           (filter (fn [section]
                                     (and (or (= (:graph_topic_id fact) (:material-id material))
                                              (= (:graph_topic_id fact) (:topic-id section)))
                                          (or (nil? (:page_number fact))
                                              (= (:page_number fact) (:page section)))))
                             (:evidence material))) materials)]
    (and (integer? (:id fact)) (nonblank? subject) (nonblank? object)
         (every? #(or (nil? %) (and (string? %) (<= (count %) 1000)))
           [subject object (:predicate_label fact)])
         (some (fn [section]
                 (let [text (normalize (:text section))]
                   (and (str/includes? text (normalize subject))
                        (str/includes? text (normalize object))))) sections))))

(defn bounded-evidence
  "Freeze at most 5000 characters per section and 120000 characters overall.
   Pre: source evidence contains strings. Post: limits describe every omitted character.
   Invariant: the caller's original material packets remain unchanged."
  [evidence]
  (let [[materials remaining omitted]
        (reduce
          (fn [[materials remaining omitted] material]
            (let [[sections remaining omitted]
                  (reduce
                    (fn [[sections remaining omitted] section]
                      (let [text (str (:text section))
                            length (min 5000 remaining (count text))
                            excluded (- (count text) length)]
                        [(conj sections (cond-> (assoc section :text (subs text 0 length))
                                          (pos? excluded) (assoc :text_omitted true)))
                         (- remaining length) (+ omitted excluded)]))
                    [[] remaining omitted] (:evidence material))]
              [(conj materials (assoc material :evidence sections)) remaining omitted]))
          [[] 120000 0] (:materials evidence))
        facts (->> (:facts evidence) (filter #(fact-in-excerpts? materials %)) (take 500)
                (mapv #(select-keys % [:id :graph_topic_id :page_number :subject_label
                                      :predicate_label :object_label :object_literal])))]
    (assoc evidence :materials materials :facts facts
      :source_limits {:max_section_characters 5000 :max_total_characters 120000
                      :included_characters (- 120000 remaining) :omitted_characters omitted
                      :max_facts 500 :included_facts (count facts)
                      :omitted_facts (- (count (:facts evidence)) (count facts))})))

(defn- include-source-limits [form evidence]
  (let [limits (:source_limits evidence)
        form (assoc-in form [:blueprint :source_limits] limits)
        form (if (pos? (:omitted_characters limits))
               (update-in form [:blueprint :unassessed_outcomes] (fnil conj [])
                 {:id "source-snapshot-limit"
                  :statement "Source text outside the assessment snapshot."
                  :reason (str (:omitted_characters limits)
                            " source characters were omitted. Each section includes at most 5000 characters. "
                            "The assessment includes at most 120000 source characters in total. "
                            "This assessment does not establish coverage of the omitted text.")})
               form)]
    (if (pos? (:omitted_facts limits))
      (update-in form [:blueprint :unassessed_outcomes] (fnil conj [])
        {:id "fact-snapshot-limit"
         :statement "Approved facts outside the assessment snapshot."
         :reason (str (:omitted_facts limits)
                   " approved facts were excluded from generation. "
                   "At most 500 facts with subject and object text in the included source excerpts can be linked. "
                   "This assessment does not establish coverage of excluded facts.")})
      form)))

(defn generate!
  "Generate one mixed assessment and charge its completed model call.
   Pre: the caller owns all evidence. Optional `:on-step` receives :writing before
   the model call and :checking before validation.
   Post: returns a validated frozen form. A refused form throws ::invalid-assessment
   with the attempt's `:charged-credits` in its data.
   Invariant: validation failure cannot suppress a completed call charge."
  [user-id evidence options]
  (let [minutes (or (:target-minutes options) 30)
        evidence (bounded-evidence evidence)
        step! (or (:on-step options) (fn [_]))]
    (when-not (and (integer? minutes) (<= 5 minutes 180))
      (fail! "Choose an estimated duration from 5 to 180 minutes." {:field :target-minutes}))
    (when-not (some nonblank? (mapcat #(map :text (:evidence %)) (:materials evidence)))
      (throw (ex-info "Add source text before generating an assessment." {:type ::insufficient-evidence})))
    (let [{:keys [api-key entry model-fields]} (llm/resolve-model+gate! user-id :synthesis)
          _ (step! :writing)
          {:keys [parsed cost tokens]} (llm/chat! api-key model-fields
                                        (cards/load-prompt-template "curriculum-exam-form.md")
                                        (llm-json/write-prompt-input {:target_minutes minutes :target_outcomes (:target-outcomes options)
                                                                      :evidence evidence})
                                        :form :map {:feature :assessment-form :user-id user-id})
          charge (credits/record-cost-charge! user-id (or (:generation-feature options) :comprehension.exam-form)
                   (:id entry) cost tokens)
          _ (step! :checking)
          form (try (validate-form! (include-source-limits parsed evidence) evidence options)
                 (catch clojure.lang.ExceptionInfo e
                   (throw (ex-info (ex-message e) (assoc (ex-data e) :charged-credits charge) e))))]
      (assoc form :cost cost :charged-credits charge))))

(defn normalized-score
  "Return an integer percentage from earned and possible points."
  [criteria]
  (when-not (and (seq criteria)
                (every? (fn [{:keys [points max_points]}]
                          (and (number? points) (number? max_points)
                               (Double/isFinite (double points)) (Double/isFinite (double max_points))
                               (pos? max_points) (<= 0 points max_points))) criteria))
    (fail! "Question points must fall within their positive maximum." {}))
  (let [possible (reduce + (map :max_points criteria))]
    (long (Math/round (* 100.0 (/ (reduce + (map :points criteria)) possible))))))

(defn- unknown-answer? [answer]
  (let [text (str/lower-case (normalize-text answer))]
    (or (str/blank? text) (boolean (re-matches #"i (?:do not|don't) know[.!]?" text)))))

(defn- objective-grade [question answer]
  (let [text (normalize-objective-answer answer)
        normalize-answer (if (false? (:case_sensitive question))
                           #(str/lower-case (normalize-objective-answer %)) normalize-objective-answer)
        correct
        (and (not (unknown-answer? answer))
             (if (= "multiple-choice" (:format question))
               (let [option-id (some #(when (= (str/lower-case (normalize-objective-answer (:id %))) (str/lower-case text)) (:id %))
                                 (:options question))
                     correct-option (some #(when (= (:id %) (:correct_option_id question)) %) (:options question))]
                 (if option-id
                   (= option-id (:correct_option_id question))
                   (= (normalize-answer text) (normalize-answer (:text correct-option)))))
               (contains? (set (map normalize-answer (:accepted_answers question))) (normalize-answer text))))
        points (:points question)
        gap {:question_id (:id question) :criterion "factual_accuracy"
             :category (if (= "multiple-choice" (:format question)) "uncertain" "recall")
             :reason "The answer does not match an accepted answer."
             :evidence (:evidence question)
             :fact_ids (if (= 1 (count (:fact_ids question))) (:fact_ids question) [])}]
    {:question_id (:id question) :criteria {:factual_accuracy (if correct 4 0)}
     :points (if correct points 0) :max_points points
     :explanation (if correct "The answer matches an accepted answer." (:reason gap))
     :deductions (if correct [] [{:criterion "factual_accuracy" :reason (:reason gap)
                                 :citation (get-in question [:evidence 0 :quote])}])
     :weaknesses (if correct [] [(:reason gap)]) :gaps (if correct [] [gap])}))

(defn validate-free-grade!
  "Validate a free-response grade against that question's frozen evidence.
   Pre: question and sources belong to the frozen assessment.
   Post: every lost criterion has a cited gap. Invariant: unrelated facts receive no result."
  [question grade materials answer]
  (let [rubric-keys (keys (:rubric question))
        scores (select-keys (:criteria grade) rubric-keys)
        deductions (:deductions grade)]
    (when-not (and (= (set rubric-keys) (set (keys (:criteria grade))))
                  (every? #(and (integer? %) (<= 0 % 4)) (vals scores))
                  (vector? deductions) (nonblank? (:explanation grade)))
      (fail! "The model returned invalid rubric scores." {:question (:id question)}))
    (when (and (unknown-answer? answer) (some pos? (vals scores)))
      (fail! "An unanswered question must receive zero points." {:question (:id question)}))
    (let [gaps
          (mapv (fn [deduction]
                  (let [criterion (keyword (:criterion deduction))
                        category (or (:category deduction) "reasoning")
                        citations (or (seq (:evidence deduction))
                                      (when-let [quote (:citation deduction)]
                                        (let [citation (some #(when (str/includes? (normalize-text (:quote %)) (normalize-text quote))
                                                               (assoc % :quote quote)) (:evidence question))]
                                          (when citation [citation]))))]
                    (when-not (and (contains? scores criterion) (< (get scores criterion) 4)
                                  (contains? gap-categories category) (nonblank? (:reason deduction)) (seq citations)
                                  (every? (fn [citation]
                                            (some #(and (= (:material_id %) (:material_id citation))
                                                        (= (:topic_id %) (:topic_id citation))) (:evidence question))) citations))
                      (fail! "Every deduction needs a question criterion and frozen evidence." {:question (:id question)}))
                    (validate-citations! materials citations)
                    {:question_id (:id question) :criterion (name criterion) :category category
                     :reason (:reason deduction) :evidence (vec citations)
                     :fact_ids (vec (filter (set (:fact_ids question)) (:fact_ids deduction)))})) deductions)]
      (doseq [[criterion score] scores :when (< score 4)]
        (when-not (some #(= (name criterion) (:criterion %)) gaps)
          (fail! "Every lost point needs a frozen evidence citation." {:question (:id question) :criterion criterion})))
      {:question_id (:id question) :criteria scores :max_points (:points question)
       :points (* (double (:points question)) (/ (reduce + (vals scores)) (* 4 (count scores))))
       :explanation (:explanation grade) :weaknesses (mapv :reason gaps) :gaps gaps
       :deductions (mapv (fn [gap] {:criterion (:criterion gap) :reason (:reason gap)
                                   :citation (get-in gap [:evidence 0 :quote])}) gaps)})))

(defn grade!
  "Grade objective answers locally and essays against frozen criteria.
   Pre: answers map stable question ids to text. Post: returns weighted score and cited gaps.
   Invariant: the grade changes no review state or curriculum progression."
  [user-id assessment answers]
  (let [questions (mapv (fn [index q] (assoc q :id (str (or (:id q) (str "q" (inc index))))
                                                       :format (or (:format q) "long-essay")
                                                       :points (or (:points q) 20)))
                     (range) (:questions assessment))
        free (filterv #(not (contains? #{"multiple-choice" "fill-in-the-blank"} (:format %))) questions)
        materials (evidence-materials (get-in assessment [:blueprint :evidence_packets]))
        result (when (seq free)
                 (let [{:keys [api-key entry model-fields]} (llm/resolve-model+gate! user-id :grade)
                       {:keys [parsed cost tokens]} (llm/chat! api-key model-fields
                                                     (cards/load-prompt-template "curriculum-exam-grade.md")
                                                     (llm-json/write-prompt-input {:questions free :blueprint (:blueprint assessment)
                                                                                   :answers (select-keys answers (map :id free))})
                                                     :grade :map {:feature :assessment-grade :user-id user-id})
                       charge (credits/record-cost-charge! user-id (or (:grading-feature assessment) :comprehension.exam-grade)
                                (:id entry) cost tokens)]
                   {:parsed parsed :cost cost :charged-credits charge}))
        grades (get-in result [:parsed :question_grades])]
    (when (and (seq free) (or (not= (count free) (count grades))
                              (not (nonblank? (get-in result [:parsed :explanation])))))
      (fail! "The model must grade every essay question and explain the result." {}))
    (let [free-grades (into {} (map (fn [q g]
                                     (when (and (:question_id g) (not= (:id q) (:question_id g)))
                                       (fail! "The grade names the wrong question." {:question (:id q)}))
                                     [(:id q) (validate-free-grade! q g materials (get answers (:id q) ""))]) free grades))
          criteria (mapv (fn [q] (or (get free-grades (:id q)) (objective-grade q (get answers (:id q) "")))) questions)]
      {:raw-score (normalized-score criteria) :criteria criteria :gaps (vec (mapcat :gaps criteria))
       :explanation (or (get-in result [:parsed :explanation]) "Objective answers were checked against the frozen answer keys.")
       :cost (or (:cost result) 0) :charged-credits (or (:charged-credits result) 0)})))
