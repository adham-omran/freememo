(ns freememo.kg-grade
  "LLM grading of free-form answers against a question's linked facts
   (plan M4, spec 6.4).

   Contract with the model output (all gated here, never trusted):
   - verdict must normalize to correct|partial|incorrect, else the grade
     fails visibly (no silent clamping);
   - missed-fact-ids are filtered to the question's linked facts;
   - matched-keywords are filtered case-insensitively to the linked entities'
     labels/aliases — a hallucinated keyword can never render as a highlight.

   The answer text is persisted BEFORE the LLM call, so a grading failure
   never loses what the learner typed. Billed per call (:kg.grade)."
  (:require
   [freememo.db :as db]
   [freememo.credits :as credits]
   [freememo.kg-llm :as llm]
   [freememo.cards :as cards]
   [freememo.toasts :as toasts]
   [freememo.user-state :as us]
   [taoensso.telemere :as tel]
   [clojure.string :as str]))

(defn- normalize-verdict [v]
  (let [s (str/lower-case (name (or v "")))]
    (#{"correct" "partial" "incorrect"} s)))

(defn grade-answer!
  "Record + grade one answer. Synchronous (one LLM call ~seconds) — UI runs
   it via e/Offload.
   Pre:  session belongs to user-id and question-id is in its draw (the UI
         iterates the session's own question_ids — violation = caller bug).
   Post: {:success true :verdict str :explanation str
          :missed-facts [fact-maps] :matched-keywords [str]
          :reference-answer str :keywords [str]} persisted to kg_answers, or
          {:success false :error msg :error-type kw?} with the raw answer
          still persisted."
  [user-id session-id question-id position user-answer]
  (let [answer-id (db/record-kg-answer! user-id session-id question-id position user-answer)]
    (try
      (when-not answer-id
        (throw (ex-info "Session not found" {})))
      (let [{:keys [api-key entry model-slug]} (llm/resolve-model+gate! user-id :grade)
            {:keys [facts keywords question reference-answer]}
            (or (db/get-kg-question-for-session user-id question-id)
                (throw (ex-info "Question not found" {})))
            fact->row (fn [{:keys [id subject_label predicate_label
                                   object_label object_literal]}]
                        {:id id :s subject_label :p predicate_label
                         :o (or object_label object_literal)})
            ;; Sibling facts sharing predicate+object or subject+predicate:
            ;; an answer the graph confirms must never be marked wrong just
            ;; because the question was generated from a different fact.
            also-true (db/kg-fact-alternates user-id (mapv :id facts))
            payload (pr-str (cond-> {:question question
                                     :reference-answer reference-answer
                                     :facts (mapv fact->row facts)
                                     :keywords keywords
                                     :answer user-answer}
                              (seq also-true)
                              (assoc :also-true (mapv fact->row also-true))))
            prompt (cards/load-prompt-template "kg-grade.md")
            ;; one retry on unparseable output, mirroring extraction
            {:keys [parsed cost]}
            (try (llm/chat! api-key model-slug prompt payload)
              (catch clojure.lang.ExceptionInfo e
                (if (:raw (ex-data e))
                  (llm/chat! api-key model-slug prompt payload)
                  (throw e))))
            verdict (or (normalize-verdict (:verdict parsed))
                        (throw (ex-info "Model returned no usable verdict" {})))
            linked-ids (into #{} (map :id) facts)
            missed (filterv linked-ids (:missed-fact-ids parsed))
            lexicon (into {} (map (juxt str/lower-case identity)) keywords)
            matched (into []
                      (comp (map str) (keep #(lexicon (str/lower-case %))) (distinct))
                      (:matched-keywords parsed))]
        (db/grade-kg-answer! answer-id verdict (str (:explanation parsed)) missed matched)
        (credits/record-cost-charge! user-id :kg.grade (:id entry) cost)
        {:success true
         :verdict verdict
         :explanation (str (:explanation parsed))
         :missed-facts (filterv #(contains? (set missed) (:id %)) facts)
         :matched-keywords matched
         :reference-answer reference-answer
         :keywords keywords})
      (catch Exception e
        (if (llm/insufficient-credits? e)
          (tel/log! {:level :info :id ::spend-refused :data {:user-id user-id}}
            "Grading refused: out of credits")
          (tel/error! {:id ::grade-answer} e))
        ;; The answer text is already persisted — tell the learner and let
        ;; them hit Submit again.
        (toasts/push! user-id
          {:level :error
           :message (str "Grading failed: " (ex-message (llm/root-cause e))
                      " — your answer was kept, try Submit again.")
           :actions (if (llm/insufficient-credits? e)
                      [{:label "Top up credits" :nav :settings}]
                      [])})
        {:success false
         :error (ex-message (llm/root-cause e))
         :error-type (when (llm/insufficient-credits? e) :insufficient-credits)}))))

(defn grade-exam-session!
  "Grade every saved-but-ungraded answer of an exam sitting, then close the
   session. Sequential LLM calls (one per answer, each billed by
   grade-answer!); progress published in the :exam-grading atom as
   {session-id [graded total]} for the UI's progress line.
   A failed grade is skipped — its verdict stays NULL and shows as ungraded —
   except insufficient credits, which aborts the remainder (every further
   call would fail the same way). The session closes regardless: an exam,
   once submitted, is over.
   Pre:  session belongs to user-id (callers pass their own session id).
   Post: the session is finished; returns its verdict counts."
  [user-id session-id]
  (let [rows (db/kg-ungraded-answers user-id session-id)
        total (count rows)
        progress! (fn [n] (swap! (us/get-atom user-id :exam-grading)
                            assoc session-id [n total]))]
    (progress! 0)
    (try
      (reduce (fn [n {:keys [question_id position user_answer]}]
                (let [r (grade-answer! user-id session-id question_id position user_answer)]
                  (progress! (inc n))
                  (if (= :insufficient-credits (:error-type r))
                    (reduced n)
                    (inc n))))
        0 rows)
      (finally
        (swap! (us/get-atom user-id :exam-grading) dissoc session-id)
        (db/finish-kg-session! user-id session-id)))
    (db/kg-session-verdict-counts user-id session-id)))
