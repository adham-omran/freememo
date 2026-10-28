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
  [materials citations]
  (let [sources (into {} (mapcat (fn [{:keys [material-id evidence]}]
                                         (map (fn [{:keys [topic-id text]}]
                                                [[material-id topic-id] (normalize-text text)]) evidence)) materials))]
    (doseq [{:keys [material_id topic_id quote]} citations]
      (when-not (and (integer? material_id) (integer? topic_id) (nonblank? quote)
                    (some-> (get sources [material_id topic_id])
                            (str/includes? (normalize-text quote))))
        (fail! "The model cited text that is not in the frozen source."
          {:material-id material_id :topic-id topic_id}))))
  citations)

(defn- normalize-question [index question valid-fact-ids]
  (let [format (or (:format question) "long-essay")
        points (or (:points question) 20)
        rubric (:rubric question)
        objective? (contains? #{"fill-in-the-blank" "multiple-choice"} format)
        options (:options question)]
    (when-not (and (contains? formats format) (nonblank? (:prompt question))
                  (integer? points) (<= 1 points 1000))
      (fail! "Every question needs a supported format, text, and positive points." {:question index}))
    (when-not (and (vector? (:evidence question)) (seq (:evidence question)))
      (fail! "Every question needs source evidence." {:question index}))
    (when (and (not objective?)
               (not (and (map? rubric) (seq rubric)
                         (every? #(and (contains? (set criterion-keys) (key %)) (nonblank? (val %))) rubric))))
      (fail! "Every essay needs source-bound rubric criteria." {:question index}))
    (when (and (= format "fill-in-the-blank")
               (not (and (vector? (:accepted_answers question)) (seq (:accepted_answers question))
                         (every? nonblank? (:accepted_answers question)))))
      (fail! "A fill-in-the-blank question needs accepted answers." {:question index}))
    (when (and (= format "multiple-choice")
               (not (and (vector? options) (<= 2 (count options) 8)
                         (every? #(and (nonblank? (:id %)) (nonblank? (:text %))) options)
                         (= (count options) (count (set (map #(str/lower-case (normalize-objective-answer (:id %))) options))))
                         (= (count options) (count (set (map #(normalize-objective-answer (:text %)) options))))
                         (some #(= (:id %) (:correct_option_id question)) options))))
      (fail! "A multiple-choice question needs distinct options and one correct option." {:question index}))
    (assoc question :id (str (or (:id question) (str "q" index))) :format format :points points
      :rubric (or rubric {})
      :fact_ids (vec (filter valid-fact-ids (:fact_ids question)))
      :grading_guide (or (:grading_guide question) (str/join " " (vals rubric))))))

(defn- validate-blueprint! [blueprint questions evidence]
  (let [used (set (map :format questions))
        omitted (set (map :format (:omitted_formats blueprint)))
        ids (set (map :id questions))]
    (when-not (= (set (remove used formats)) omitted)
      (fail! "The blueprint must explain every omitted format." {:field :omitted-formats}))
    (doseq [outcome (:assessed_outcomes blueprint)]
      (when-not (and (nonblank? (:id outcome)) (nonblank? (:statement outcome))
                    (seq (:question_ids outcome)) (every? ids (:question_ids outcome))
                    (seq (:evidence outcome)))
        (fail! "Every assessed outcome needs question ids and source evidence." {:field :assessed-outcomes}))
      (when evidence (validate-citations! (:materials evidence) (:evidence outcome))))
    (when-not (and (seq (:assessed_outcomes blueprint))
                   (= ids (set (mapcat :question_ids (:assessed_outcomes blueprint)))))
      (fail! "Every question must assess a declared outcome." {:field :assessed-outcomes}))
    (doseq [outcome (:unassessed_outcomes blueprint)]
      (when-not (and (nonblank? (:statement outcome)) (nonblank? (:reason outcome)))
        (fail! "Every unassessed outcome needs an omission reason." {:field :unassessed-outcomes})))
    blueprint))

(defn validate-form!
  "Validate a generated form before persistence.
   Pre: evidence contains frozen materials and approved facts.
   Post: questions have stable ids and source-bound criteria.
   Invariant: unsupported formats are omitted, never invented."
  ([form] (validate-form! form nil {}))
  ([form evidence options]
   (when (:insufficient form)
     (throw (ex-info (str (or (:reason form) "The source cannot support this assessment.")
                      " Add material or choose a broader source.") {:type ::insufficient-evidence})))
   (when-not (and (map? (:blueprint form))
                 (vector? (get-in form [:blueprint :assessed_outcomes]))
                 (vector? (get-in form [:blueprint :unassessed_outcomes])))
     (fail! "The blueprint must account for assessed and unassessed outcomes." {:field :blueprint}))
   (when-not (and (vector? (:questions form)) (<= 1 (count (:questions form)) 40))
     (fail! "An assessment must contain from 1 to 40 questions." {:field :questions}))
   (let [facts (set (keep :id (:facts evidence)))
         questions (mapv #(normalize-question %1 %2 facts)
                     (range 1 (inc (count (:questions form)))) (:questions form))
         blueprint (assoc (:blueprint form)
                     :target_minutes (or (:target-minutes options) 30)
                     :estimated_minutes (or (get-in form [:blueprint :estimated_minutes]) (:target-minutes options) 30)
                     :omitted_formats (vec (or (get-in form [:blueprint :omitted_formats]) [])))
         blueprint (if evidence (assoc blueprint :evidence_packets (:materials evidence) :facts (:facts evidence)) blueprint)]
     (when-not (= (count questions) (count (set (map :id questions)))
                  (count (set (map #(str/lower-case (normalize-text (:prompt %))) questions))))
       (fail! "Assessment questions must have distinct ids and prompts." {:field :duplicate-questions}))
     (when-not (and (integer? (:estimated_minutes blueprint)) (pos? (:estimated_minutes blueprint)))
       (fail! "The assessment needs a positive time estimate." {:field :estimated-minutes}))
     (when evidence (validate-citations! (:materials evidence) (mapcat :evidence questions)))
     (when (some :format (:questions form)) (validate-blueprint! blueprint questions evidence))
     (doseq [omission (:omitted_formats blueprint)]
       (when-not (and (contains? formats (:format omission)) (nonblank? (:reason omission)))
         (fail! "Every omitted format needs a reason." {:field :omitted-formats})))
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
   Pre: the caller owns all evidence. Post: returns a validated frozen form.
   Invariant: validation failure cannot suppress a completed call charge."
  [user-id evidence options]
  (let [minutes (or (:target-minutes options) 30)
        evidence (bounded-evidence evidence)]
    (when-not (and (integer? minutes) (<= 5 minutes 180))
      (fail! "Choose an estimated duration from 5 to 180 minutes." {:field :target-minutes}))
    (when-not (some nonblank? (mapcat #(map :text (:evidence %)) (:materials evidence)))
      (throw (ex-info "Add source text before generating an assessment." {:type ::insufficient-evidence})))
    (let [{:keys [api-key entry model-fields]} (llm/resolve-model+gate! user-id :synthesis)
          {:keys [parsed cost tokens]} (llm/chat! api-key model-fields
                                        (cards/load-prompt-template "curriculum-exam-form.md")
                                        (llm-json/write-prompt-input {:target_minutes minutes :target_outcomes (:target-outcomes options)
                                                                      :evidence evidence})
                                        :form :map {:feature :assessment-form :user-id user-id})
          charge (credits/record-cost-charge! user-id (or (:generation-feature options) :comprehension.exam-form)
                   (:id entry) cost tokens)]
      (assoc (validate-form! (include-source-limits parsed evidence) evidence options) :cost cost :charged-credits charge))))

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
