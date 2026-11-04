(ns freememo.assessment
  "Source-bound mixed assessments and diagnostic grading."
  (:require [clojure.string :as str]
            [freememo.card-models :as card-models]
            [freememo.cards :as cards]
            [freememo.config :as config]
            [freememo.credits :as credits]
            [freememo.exam-grid :as grid]
            [freememo.kg-llm :as llm]
            [freememo.llm-json :as llm-json]
            [freememo.reply-schema :as reply-schema
             :refer [strict-object nullable array-of enum-of json-string json-integer json-boolean]]
            [freememo.settings :as settings])
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

(def ^:private option-letters "ABCDEFGH")

(defn- lettered-options
  "Give a checked multiple-choice question the option IDs A, B, C and so on.
   Pre: the option texts are unique, and :correct_option_id names one option.
   Post: the options are in a shuffled order with consecutive letter IDs, and
   :correct_option_id names the letter of the same option text.
   Invariant: the order depends only on the prompt and the option texts, so
   two checks of one question give the same letters. The model's own IDs are
   gone, so an ID cannot hint at the answer, and the correct option is not
   always in the position where the model wrote it."
  [question]
  (let [options (:options question)
        order (java.util.ArrayList. ^java.util.Collection options)
        _ (java.util.Collections/shuffle order
            (java.util.Random. (hash [(:prompt question) (mapv :text options)])))
        lettered (mapv (fn [letter option] (assoc option :id (str letter))) option-letters order)
        correct-text (:text (some #(when (= (:id %) (:correct_option_id question)) %) options))]
    (assoc question :options lettered
      :correct_option_id (:id (some #(when (= (:text %) correct-text) %) lettered)))))

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
    (cond-> (assoc question :id (str (or (:id question) (str "q" index))) :format format :points points
              :rubric (or rubric {})
              :fact_ids (vec (filter valid-fact-ids (:fact_ids question)))
              :grading_guide (or (:grading_guide question) (str/join " " (vals rubric))))
      (= format "multiple-choice") lettered-options)))

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
                (update acc :dropped conj {:question index :format (str (or (:format question) "long-essay"))
                                          :prompt (str (:prompt question)) :reason (:reason problem)})
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
                              {:question index :format (str (or (:format question) "long-essay")) :prompt (str (:prompt question))
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

;; ── Source snapshot ──────────────────────────────────────────────────────────

(def max-source-characters
  "The most source text one exam reads. At 3 characters per token (set low so
   that scripts other than Latin fit) that is about 400K tokens, which leaves
   room in a 1M-token window for the prompt and the reply. Every registry model
   has about 1M tokens. See plans/blueprint-first-exams.md §2."
  1200000)

(defn bounded-evidence
  "Freeze at most max-source-characters of source text, in section order.
   A section is cut only where the total runs out.
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
                            length (min remaining (count text))
                            excluded (- (count text) length)]
                        [(conj sections (cond-> (assoc section :text (subs text 0 length))
                                          (pos? excluded) (assoc :text_omitted true)))
                         (- remaining length) (+ omitted excluded)]))
                    [[] remaining omitted] (:evidence material))]
              [(conj materials (assoc material :evidence sections)) remaining omitted]))
          [[] max-source-characters 0] (:materials evidence))
        facts (->> (:facts evidence) (filter #(fact-in-excerpts? materials %)) (take 500)
                (mapv #(select-keys % [:id :graph_topic_id :page_number :subject_label
                                      :predicate_label :object_label :object_literal])))]
    (assoc evidence :materials materials :facts facts
      :source_limits {:max_total_characters max-source-characters
                      :included_characters (- max-source-characters remaining) :omitted_characters omitted
                      :max_facts 500 :included_facts (count facts)
                      :omitted_facts (- (count (:facts evidence)) (count facts))})))

(defn- include-source-limits [form evidence]
  (let [limits (:source_limits evidence)
        form (assoc-in form [:blueprint :source_limits] limits)
        form (if (pos? (:omitted_characters limits))
               (update-in form [:blueprint :unassessed_outcomes] (fnil conj [])
                 {:id "source-snapshot-limit"
                  :statement "Source text outside the assessment snapshot."
                  :reason (str (credits/group-digits (:omitted_characters limits))
                            " source characters were omitted. The assessment includes at most "
                            (credits/group-digits max-source-characters) " source characters in total. "
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

;; ── Model contracts ──────────────────────────────────────────────────────────
;;
;; Two calls make an exam: assessment-blueprint.md plans themes, outcomes and
;; the grid's slots, and assessment-questions.md writes one question per slot.
;; The schemas state the shapes the code below reads. The checks still run on
;; every reply: a schema cannot check a quote against the source.

(def ^:private citation-schema
  "What validate-citations! reads from one citation."
  (strict-object {:material_id json-integer :topic_id json-integer :quote json-string}))

(def ^:private exam-format-schema (enum-of (sort (keys grid/format-minutes))))
(def ^:private level-schema (enum-of grid/levels))

(def blueprint-reply
  "The reply contract of assessment-blueprint.md."
  (reply-schema/contract "assessment_blueprint" :blueprint
    (strict-object
      {:insufficient (nullable json-boolean)
       :reason (nullable json-string)
       :themes (array-of (strict-object {:id json-string :title json-string
                                         :topic_ids (array-of json-integer)
                                         :weight json-integer}))
       :outcomes (array-of (strict-object {:id json-string :statement json-string
                                           :level level-schema
                                           :theme_ids (array-of json-string)}))
       :slots (array-of (strict-object {:id json-string :format exam-format-schema
                                        ;; A string, not a nullable enum: Anthropic refuses
                                        ;; an enum whose type admits null (2026-11-03). The
                                        ;; grid owns the role, so the code never reads it.
                                        :role (nullable json-string)
                                        :level level-schema :outcome_id json-string}))
       :unassessed_outcomes (array-of (strict-object {:id (nullable json-string)
                                                      :statement json-string :reason json-string
                                                      :theme_id (nullable json-string)}))})))

(def questions-reply
  "The reply contract of assessment-questions.md."
  (reply-schema/contract "assessment_questions" :questions
    (array-of
      (strict-object
        {:slot_id json-string
         :format exam-format-schema
         :prompt json-string
         :evidence (array-of citation-schema)
         :fact_ids (nullable (array-of json-integer))
         :options (nullable (array-of (strict-object {:id json-string :text json-string})))
         :correct_option_id (nullable json-string)
         :grading_guide (nullable json-string)
         ;; A list, not a map of five optional criteria: Anthropic refused that map
         ;; as a grammar too large (2026-11-03). rubric-entries->map reads it back.
         :rubric (nullable (array-of (strict-object {:criterion (enum-of (map name criterion-keys))
                                                     :expectation json-string})))}))))

(defn- rubric-entries->map
  "Read a rubric written as [{:criterion :expectation}] as the criterion map the
   checks and grading read. A repeated criterion keeps its last expectation.
   A rubric that is already a map passes through."
  [rubric]
  (if (vector? rubric)
    (into {} (map (fn [{:keys [criterion expectation]}] [(keyword (str criterion)) expectation])) rubric)
    rubric))

(def grade-reply
  "The reply contract of curriculum-exam-grade.md."
  (reply-schema/contract "assessment_grade" :grade
    (strict-object
      {:question_grades
       (array-of
         (strict-object
           {:question_id json-string
            ;; A list for the reason the form's rubric is one: a map of five
            ;; optional criteria is the shape Anthropic refused.
            :criteria (array-of (strict-object {:criterion (enum-of (map name criterion-keys))
                                                :score json-integer}))
            :explanation json-string
            :deductions (array-of
                          (strict-object
                            {:criterion (enum-of (map name criterion-keys))
                             :category (enum-of (sort gap-categories))
                             :reason json-string
                             :evidence (array-of citation-schema)
                             :fact_ids (array-of json-integer)}))}))
       :explanation json-string})))

(defn- criterion-scores->map
  "Read criteria written as [{:criterion :score}] as the score map
   validate-free-grade! reads. A repeated criterion keeps its last score.
   Criteria that are already a map pass through."
  [criteria]
  (if (vector? criteria)
    (into {} (map (fn [{:keys [criterion score]}] [(keyword (str criterion)) score])) criteria)
    criteria))

(defn reply->grade
  "Read a curriculum-exam-grade.md reply as the grade grade! checks.
   Post: every question grade's criteria is a score map.
   Invariant: no question grade or deduction is added or removed."
  [reply]
  (cond-> reply
    (vector? (:question_grades reply))
    (update :question_grades (partial mapv #(cond-> % (contains? % :criteria) (update :criteria criterion-scores->map))))))

(defn prompt-source
  "The source as the exam prompts read it.
   Pre:  evidence is the result of bounded-evidence.
   Post: {:materials [{:material_id :title :sections [{:topic_id …}]}] :facts
         :source_limits}. The ids carry the names a citation uses, and the word
         evidence names nothing here: in the reply it means citations only.
   Invariant: the frozen evidence in the blueprint keeps the application's
         own shape. Only the prompt sees this one."
  [evidence]
  {:materials (mapv (fn [material]
                      (cond-> {:material_id (:material-id material)
                               :sections (mapv (fn [section]
                                                 (-> section (dissoc :topic-id)
                                                     (assoc :topic_id (:topic-id section))))
                                           (:evidence material))}
                        (:title material) (assoc :title (:title material))))
                (:materials evidence))
   :facts (:facts evidence)
   :source_limits (:source_limits evidence)})

;; ── The plan: call 1's reply held to the grid ───────────────────────────────
;;
;; The grid is the source of truth for slots, formats and points. A blueprint
;; that strays from it is corrected and the correction is recorded in
;; :plan_warnings, never refused: call 1 is already paid for, and a refusal
;; would leave the learner with nothing for it.

(def theme-count-range [3 15])

(defn- valid-level [level allowed fallback]
  (if (contains? allowed level) level fallback))

(defn plan-from-blueprint
  "Hold a blueprint reply to its grid.
   Pre:  grid-slots come from exam-grid/slots for kind; section-ids are the
         topic ids of the frozen source; blueprint is the parsed reply.
   Post: {:themes :outcomes :slots :unassessed_outcomes :plan_warnings}.
         :slots is grid-slots in order, each with a valid :level and an
         :outcome_id that names a kept outcome. Recall slots are multiple-choice
         and hold at most max-recall-share of the points. Every theme has a slot
         or an unassessed entry.
   Throws ::insufficient-evidence only when the model says the source cannot
   support an exam."
  [kind grid-slots section-ids blueprint]
  (when (:insufficient blueprint)
    (throw (ex-info (str (or (:reason blueprint) "The source cannot support this assessment.")
                      " Add material or choose a broader source.") {:type ::insufficient-evidence})))
  (let [warnings (atom [])
        warn! #(swap! warnings conj %)
        themes (->> (:themes blueprint)
                    (filter #(and (nonblank? (:id %)) (nonblank? (:title %))))
                    (reduce (fn [acc t] (if (some #(= (:id t) (:id %)) acc) acc (conj acc t))) [])
                    (mapv #(update % :topic_ids (fn [ids] (vec (filter section-ids ids))))))
        theme-ids (set (map :id themes))
        [lo hi] theme-count-range
        _ (when-not (<= lo (count themes) hi)
            (warn! (str "The plan outlined " (count themes) " themes, outside the range of " lo " to " hi ".")))
        outcomes (->> (:outcomes blueprint)
                      (filter #(and (nonblank? (:id %)) (nonblank? (:statement %))))
                      (reduce (fn [acc o] (if (some #(= (:id o) (:id %)) acc) acc (conj acc o))) [])
                      (mapv #(-> %
                                 (update :level valid-level (set grid/levels) "understand")
                                 (update :theme_ids (fn [ids] (vec (filter theme-ids ids)))))))
        outcomes (if (seq outcomes)
                   outcomes
                   (do (warn! "The plan stated no usable outcome, so one covers the whole source.")
                       [{:id "whole-source" :statement "Explain the main ideas of the source."
                         :level "understand" :theme_ids (mapv :id themes)}]))
        outcome-ids (mapv :id outcomes)
        by-id (into {} (map (juxt :id identity)) (:slots blueprint))
        slots (vec (map-indexed
                     (fn [i grid-slot]
                       (let [chosen (get by-id (:id grid-slot))
                             essay? (not= "multiple-choice" (:format grid-slot))
                             outcome-id (if (some #{(:outcome_id chosen)} outcome-ids)
                                          (:outcome_id chosen)
                                          (do (warn! (str "Slot " (:id grid-slot) " named no outcome of the plan, so it was given one."))
                                              (nth outcome-ids (mod i (count outcome-ids)))))
                             level (cond
                                     (:level grid-slot) (:level grid-slot)
                                     essay? (valid-level (:level chosen) grid/essay-levels "apply")
                                     :else (valid-level (:level chosen) (set grid/levels) "understand"))]
                         (when (and (nil? (:level grid-slot)) (not= level (:level chosen)))
                           (warn! (str "Slot " (:id grid-slot) " had no valid level, so it is " level ".")))
                         (assoc grid-slot :level level :outcome_id outcome-id)))
                     grid-slots))
        allowed (long (Math/floor (* grid/max-recall-share (grid/slot-points slots))))
        [slots _recall-points]
        (reduce (fn [[acc points] slot]
                  (if (= grid/recall-level (:level slot))
                    (if (<= (+ points (:points slot)) allowed)
                      [(conj acc slot) (+ points (:points slot))]
                      (do (warn! (str "Slot " (:id slot) " made recall pass a fifth of the points, so it asks for understanding."))
                          [(conj acc (assoc slot :level "understand")) points]))
                    [(conj acc slot) points]))
                [[] 0] slots)
        reached (set (mapcat (fn [slot] (:theme_ids (some #(when (= (:id %) (:outcome_id slot)) %) outcomes))) slots))
        unassessed (vec (filter #(and (nonblank? (:statement %)) (nonblank? (:reason %)))
                          (:unassessed_outcomes blueprint)))
        explained (set (keep :theme_id unassessed))
        unassessed (into unassessed
                     (for [theme themes :when (not (or (reached (:id theme)) (explained (:id theme))))]
                       {:id (str "theme-" (:id theme)) :statement (:title theme) :theme_id (:id theme)
                        :reason "The exam plan gave this theme no question."}))]
    {:themes themes :outcomes outcomes :slots slots :unassessed_outcomes unassessed
     :grid_kind (name kind) :plan_warnings @warnings}))

;; ── Quotes ───────────────────────────────────────────────────────────────────
;;
;; Every saved quote is an exact substring of its section. A quote the model
;; did not copy exactly is replaced by the most similar passage of the source.
;; At similarity theta or more that is a repair. Below it the question is kept
;; and flagged, so the learner still gets the question that was paid for and
;; is told to check it. See plans/blueprint-first-exams.md §7.

(def quote-repair-threshold
  "The word-level similarity at which a replaced quote counts as the same
   passage. Measured in plans/blueprint-first-exams.md §11."
  0.85)

(defn- word-key [word] (str/lower-case (str/replace word #"[^\p{L}\p{N}]" "")))

(defn- word-spans
  "Post: [[start end key] …] for every run of non-space characters in text."
  [^String text]
  (let [m (re-matcher #"\S+" text)]
    (loop [acc (transient [])]
      (if (.find m)
        (recur (conj! acc [(.start m) (.end m) (word-key (.group m))]))
        (persistent! acc)))))

(defn- lcs-length [xs ys]
  (let [ys (vec ys)
        n (count ys)]
    (loop [xs (seq xs) prev (vec (repeat (inc n) 0))]
      (if-not xs
        (peek prev)
        (let [x (first xs)
              row (reduce (fn [row j]
                            (conj row (if (= x (ys (dec j)))
                                        (inc (prev (dec j)))
                                        (max (prev j) (peek row)))))
                    [0] (range 1 (inc n)))]
          (recur (next xs) row))))))

(defn word-similarity
  "2 × common words in order / total words, from 0 to 1."
  [xs ys]
  (let [total (+ (count xs) (count ys))]
    (if (zero? total) 0.0 (/ (* 2.0 (lcs-length xs ys)) total))))

(defn- best-window
  "The passage of normalized section text most similar to quote-keys.
   Post: {:quote :similarity} with :quote a substring of text, or nil when the
   section shares no word with the quote."
  [text spans quote-keys]
  (let [n (count quote-keys)
        size (min n (count spans))
        wanted (set quote-keys)]
    (when (and (pos? n) (pos? size))
      (let [hits (mapv #(if (wanted (nth % 2)) 1 0) spans)
            first-sum (reduce + (subvec hits 0 size))
            sums (reductions (fn [acc i] (+ acc (- (hits (+ i size -1)) (hits (dec i)))))
                   first-sum (range 1 (inc (- (count spans) size))))
            starts (->> (map-indexed vector sums) (filter #(pos? (second %)))
                        (sort-by (comp - second)) (take 5) (map first))]
        (->> (for [start starts
                   from (range (max 0 (- start 2)) (min (count spans) (+ start 3)))
                   len (range (max 1 (- size 2)) (+ size 3))
                   :let [to (min (count spans) (+ from len))]
                   :when (< from to)]
               {:quote (subs text (first (spans from)) (second (spans (dec to))))
                :similarity (word-similarity quote-keys (map #(nth % 2) (subvec spans from to)))})
             (reduce (fn [best c] (if (or (nil? best) (> (:similarity c) (:similarity best))) c best)) nil))))))

(defn- section-index
  "Post: {[material-id topic-id] {:text normalized-text :spans word-spans}}."
  [materials]
  (into {} (for [{:keys [material-id evidence]} materials
                 {:keys [topic-id text]} evidence
                 :let [norm (normalize-text text)]]
             [[material-id topic-id] {:text norm :spans (word-spans norm)}])))

(defn repair-citation
  "Make a citation's quote an exact substring of the source.
   Pre:  index comes from section-index over the frozen materials.
   Post: {:status :exact|:repaired|:replaced|:none :citation c}. A :repaired
         or :replaced citation quotes the most similar passage, of its own
         section first and of any section when that one scores below the
         threshold, and keeps the model's text as :original_quote. :none means
         no section shares a word with the quote.
   Invariant: a returned quote is always a substring of its section."
  [index {:keys [material_id topic_id quote] :as citation}]
  (let [declared (get index [material_id topic_id])
        normalized (normalize-text quote)]
    (if (and declared (nonblank? normalized) (str/includes? (:text declared) normalized))
      {:status :exact :citation citation}
      (let [quote-keys (mapv #(nth % 2) (word-spans normalized))
            score (fn [[[mid tid] {:keys [text spans]}]]
                    (some-> (best-window text spans quote-keys) (assoc :material_id mid :topic_id tid)))
            own (when declared (score [[material_id topic_id] declared]))
            best (if (and own (>= (:similarity own) quote-repair-threshold))
                   own
                   (reduce (fn [best c] (if (or (nil? best) (> (:similarity c) (:similarity best))) c best))
                     own (keep score index)))]
        (if (nil? best)
          {:status :none :citation citation}
          {:status (if (>= (:similarity best) quote-repair-threshold) :repaired :replaced)
           :citation {:material_id (:material_id best) :topic_id (:topic_id best) :quote (:quote best)
                      :original_quote (subs (str quote) 0 (min 500 (count (str quote))))
                      :similarity (/ (Math/round (* 100.0 (:similarity best))) 100.0)}})))))

(def ^:private replaced-quote-flag
  {:kind "quote-replaced"
   :message (str "The model's quote does not match the source, so the closest passage replaced it. "
                 "Check this question against the source before you trust it.")})

(defn- repair-question-evidence
  "Post: [evidence flags]. Evidence holds only quotes found in the source."
  [index evidence]
  (let [results (mapv #(repair-citation index %) evidence)
        kept (into [] (comp (remove #(= :none (:status %))) (map :citation)) results)]
    [kept (if (some #(= :replaced (:status %)) results) [replaced-quote-flag] [])]))

;; ── The form: call 2's reply over the plan ──────────────────────────────────

(defn- grid-omissions
  "One reason for each format that the grid does not use."
  [kind slots]
  (let [used (set (map :format slots))]
    (vec (for [f (sort (remove used formats))]
           {:format f
            :reason (cond
                      (= f "fill-in-the-blank") "New exams do not use fill-in-the-blank questions."
                      (and (= kind :essay) (= f "multiple-choice")) "Curriculum exams ask essay questions only."
                      :else "The target minutes leave no time for this format.")}))))

(defn- slot-number [slot-id] (or (some-> (re-find #"\d+" (str slot-id)) parse-long) 0))

(defn questions-for-plan
  "One candidate question per planned slot, from call 2's reply.
   Pre:  plan comes from plan-from-blueprint; reply is the parsed questions vector.
   Post: [questions dropped]. Each question carries its slot's id, format,
         points, level and role, its outcome, and quotes found in the source.
         A slot with no usable answer is in dropped, with its slot number."
  [plan reply materials]
  (let [index (section-index materials)
        slot-ids (set (map :id (:slots plan)))
        written (reduce (fn [acc q]
                          (let [id (str (:slot_id q))]
                            (if (and (slot-ids id) (not (contains? acc id))) (assoc acc id q) acc)))
                  {} (if (vector? reply) reply []))]
    (reduce
      (fn [[questions dropped] slot]
        (let [q (get written (:id slot))
              [evidence flags] (when q (repair-question-evidence index (vec (:evidence q))))
              drop-entry (fn [reason] {:question (slot-number (:id slot)) :slot_id (:id slot)
                                       :format (:format slot) :prompt (str (:prompt q)) :reason reason})]
          (cond
            (nil? q) [questions (conj dropped (drop-entry "The model wrote no question for this slot."))]
            (empty? evidence) [questions (conj dropped (drop-entry "None of its quotes shares a word with the source."))]
            :else
            [(conj questions
               (cond-> (-> (select-keys q [:prompt :fact_ids :options :correct_option_id :grading_guide])
                           (assoc :id (:id slot) :format (:format slot) :points (:points slot)
                                  :level (:level slot) :role (:role slot)
                                  :outcomes [(:outcome_id slot)] :evidence evidence :flags flags))
                 (some? (:rubric q)) (assoc :rubric (rubric-entries->map (:rubric q)))))
             dropped])))
      [[] []] (:slots plan))))

(defn- outcome-blueprint
  "The saved blueprint's outcome lists, from the plan and the written questions.
   Post: [assessed unassessed]. An assessed outcome lists its questions and
   their quotes. An outcome with no question moves to unassessed."
  [plan questions]
  (reduce
    (fn [[assessed unassessed] outcome]
      (let [mine (filter #(= [(:id outcome)] (:outcomes %)) questions)]
        (if (seq mine)
          [(conj assessed (assoc (select-keys outcome [:id :statement :level :theme_ids])
                            :question_ids (mapv :id mine)
                            :evidence (vec (distinct (map #(select-keys % [:material_id :topic_id :quote])
                                                       (mapcat :evidence mine))))))
           unassessed]
          [assessed (conj unassessed {:id (:id outcome) :statement (:statement outcome)
                                      :reason "No question was written for this outcome."})])))
    [[] (:unassessed_outcomes plan)] (:outcomes plan)))

(defn assemble-form
  "The form to save from a plan and call 2's reply.
   Pre:  plan from plan-from-blueprint; evidence from bounded-evidence.
   Post: a form validate-form! accepted, holding every question that can be
         served. A question whose structure fails its checks is listed in
         :dropped_questions; a replaced quote only flags its question.
         :estimated_minutes is the minute cost of the kept questions.
   Throws ::invalid-assessment only when no question survives."
  [kind minutes plan reply evidence target-outcomes]
  (let [[questions pre-dropped] (questions-for-plan plan reply (:materials evidence))
        _ (when (empty? questions)
            (let [{:keys [slot_id reason]} (first pre-dropped)]
              (fail! (str "No question passed the checks."
                       (when reason (str " Slot " slot_id ": " reason)))
                {:question (slot-number slot_id)})))
        [assessed unassessed] (outcome-blueprint plan questions)
        form (include-source-limits
               {:blueprint (merge (select-keys plan [:themes :slots :grid_kind :plan_warnings])
                             {:assessed_outcomes assessed :unassessed_outcomes unassessed
                              :omitted_formats (grid-omissions kind (:slots plan))})
                :questions questions}
               evidence)
        validated (validate-form! form evidence {:drop-invalid? true :target-minutes minutes})
        position->slot (into {} (map-indexed (fn [i q] [(inc i) (:id q)]) questions))
        dropped (->> (get-in validated [:blueprint :dropped_questions])
                     (map #(let [slot-id (position->slot (:question %))]
                             (assoc % :slot_id slot-id :question (slot-number slot-id))))
                     (concat pre-dropped)
                     (sort-by :question) vec)]
    (cond-> (-> validated
                (assoc-in [:blueprint :dropped_questions] dropped)
                (assoc-in [:blueprint :estimated_minutes]
                  (long (Math/ceil (grid/slot-minutes (:questions validated))))))
      (seq target-outcomes) (assoc-in [:blueprint :target_outcomes] (vec target-outcomes)))))

;; ── Generation ──────────────────────────────────────────────────────────────

(defn- prompt-input [m] (llm-json/write-prompt-input m))

(defn- charge-total [charges] (when (seq charges) (reduce + charges)))

(defn generate!
  "Generate one exam in two charged calls: a blueprint over the whole source,
   then one question per slot of the grid.
   Pre:  the caller owns all evidence. options: :target-minutes (5 to 90,
         default 30), :grid (:mixed or :essay, default :mixed),
         :target-outcomes, :generation-feature, and :on-step, which receives
         :planning, :writing and :checking.
   Post: a validated frozen form with :cost and :charged-credits. A paid call
         never ends in nothing while a question can be served: a stray plan is
         corrected and a bad quote is replaced and flagged.
   Throws: ::insufficient-evidence when the model says the source cannot
         support an exam, ::invalid-assessment when no question survives. Each
         throw after a paid call carries :charged-credits.
   Invariant: validation failure cannot suppress a completed call charge."
  [user-id evidence options]
  (let [minutes (or (:target-minutes options) 30)
        kind (or (:grid options) :mixed)
        step! (or (:on-step options) (fn [_]))]
    (when-not (grid/minutes-in-range? minutes)
      (fail! (str "Choose an estimated duration from " grid/min-minutes " to " grid/max-minutes " minutes.")
        {:field :target-minutes}))
    (when-not (grid/grid-kinds kind)
      (throw (ex-info "Unknown exam grid." {:grid kind})))
    (let [evidence (bounded-evidence evidence)]
      (when-not (some nonblank? (mapcat #(map :text (:evidence %)) (:materials evidence)))
        (throw (ex-info "Add source text before generating an assessment." {:type ::insufficient-evidence})))
      (let [{:keys [entry] :as resolved} (llm/resolve-model+gate! user-id :synthesis)
            feature (or (:generation-feature options) :comprehension.exam-form)
            grid-slots (grid/slots kind minutes)
            section-ids (set (mapcat #(map :topic-id (:evidence %)) (:materials evidence)))
            source (prompt-source evidence)
            charges (atom []) costs (atom 0.0)
            call! (fn [template input reply llm-feature]
                    (let [{:keys [parsed cost tokens]} (llm/chat! resolved (cards/load-prompt-template template)
                                                         (prompt-input input) reply
                                                         {:feature llm-feature :user-id user-id})]
                      (when-let [c (credits/record-cost-charge! user-id feature (:id entry) cost tokens)]
                        (swap! charges conj c))
                      (swap! costs + (double (or cost 0)))
                      parsed))]
        (try
          (step! :planning)
          (let [plan (plan-from-blueprint kind grid-slots section-ids
                       (call! "assessment-blueprint.md"
                         {:target_minutes minutes :target_outcomes (:target-outcomes options)
                          :grid {:kind (name kind) :slots (mapv #(select-keys % [:id :format :role :level]) grid-slots)}
                          :source source}
                         blueprint-reply :assessment-blueprint))
                outcome-of (into {} (map (juxt :id :statement)) (:outcomes plan))
                _ (step! :writing)
                reply (call! "assessment-questions.md"
                        {:target_minutes minutes :source source
                         :themes (:themes plan) :outcomes (:outcomes plan)
                         :slots (mapv #(assoc (select-keys % [:id :format :level :role :outcome_id])
                                         :outcome (outcome-of (:outcome_id %)))
                                  (:slots plan))}
                        questions-reply :assessment-questions)
                _ (step! :checking)
                form (assemble-form kind minutes plan reply evidence (:target-outcomes options))]
            (assoc form :cost @costs :charged-credits (charge-total @charges)))
          (catch Exception e
            (if (seq @charges)
              (throw (ex-info (ex-message e) (assoc (ex-data e) :charged-credits (charge-total @charges)) e))
              (throw e))))))))

(defn source-overflow
  "How much of evidence's source text an exam would leave out, before any call.
   Counts what bounded-evidence counts, so the figure shown before generation
   is the figure the form records after it.
   Pre:  evidence holds :materials.
   Post: {:source-characters :omitted-characters}; omitted is 0 when the
         source fits in max-source-characters."
  [evidence]
  (let [total (reduce + (map #(count (str (:text %))) (mapcat :evidence (:materials evidence))))]
    {:source-characters total
     :omitted-characters (max 0 (- total max-source-characters))}))

(defn estimated-cost
  "The price sheet for an exam over evidence, before any call: what the
   input costs, what one output token costs, and how much source text the
   exam would leave out. exam-grid/estimate turns it into a cost for a
   length, so the slider can move without a server call.
   Pre:  evidence holds :materials.
   Post: {:model :input-usd :usd-per-output-token :credits-per-usd
          :source-characters :omitted-characters}, or {:none true
          :source-characters :omitted-characters} when the model has no
          price, so the omission warning shows either way. :credits-per-usd
          is nil when credits are off or unconfigured. Both calls read the
          whole source (plans/blueprint-first-exams.md §9)."
  [user-id evidence]
  (let [overflow (source-overflow evidence)
        model-id (settings/get-kg-model user-id :synthesis)
        {:keys [input output]} (:usd-per-mtok (card-models/resolve-model model-id))]
    (merge overflow
      (if (and input output)
        (let [source-chars (min max-source-characters (:source-characters overflow))
              prompt-chars (+ (count (cards/load-prompt-template "assessment-blueprint.md"))
                              (count (cards/load-prompt-template "assessment-questions.md")))
              input-tokens (+ (* 2 (/ source-chars 3.0)) (/ prompt-chars 3.0))
              credits-per-usd (when (config/credits-enabled?)
                                (try (credits/charge-iqd-from-usd user-id 1000.0)
                                     (catch Exception _ nil)))]
          {:model model-id
           :input-usd (* input-tokens (/ input 1e6))
           :usd-per-output-token (/ output 1e6)
           :credits-per-usd (some-> credits-per-usd (/ 1000.0))})
        {:none true}))))

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
               (let [id-of (fn [candidate]
                             (some #(when (= (str/lower-case (normalize-objective-answer (:id %))) (str/lower-case candidate)) (:id %))
                               (:options question)))
                     ;; A handwritten letter often comes back as "B." or "(B)".
                     option-id (or (id-of text)
                                   (some-> (re-matches #"\(?\s*(\p{L})\s*[.)]?" text) second id-of))
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
                 (let [{:keys [entry] :as resolved} (llm/resolve-model+gate! user-id :grade)
                       {:keys [parsed cost tokens]} (llm/chat! resolved
                                                     (cards/load-prompt-template "curriculum-exam-grade.md")
                                                     (llm-json/write-prompt-input {:questions free :blueprint (:blueprint assessment)
                                                                                   :answers (select-keys answers (map :id free))})
                                                     grade-reply {:feature :assessment-grade :user-id user-id})
                       charge (credits/record-cost-charge! user-id (or (:grading-feature assessment) :comprehension.exam-grade)
                                (:id entry) cost tokens)]
                   {:parsed (reply->grade parsed) :cost cost :charged-credits charge}))
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
