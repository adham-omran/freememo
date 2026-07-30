(ns freememo.kg-grade
  "LLM grading of free-form answers, for both Quiz item types.

   A QUESTION is graded against its linked facts (plan M4, spec 6.4). A CARD item
   has no facts, so it is graded against its own reference answer by a separate
   prompt — a weaker rubric, which is why the learner can turn it off and self-rate
   instead (plans/cards-in-quiz-queue.md D2, D3).

   Contract with the model output (all gated here, never trusted):
   - verdict must normalize to correct|partial|incorrect, else the grade
     fails visibly (no silent clamping);
   - missed-fact-ids are filtered to the question's linked facts;
   - matched-keywords are filtered case-insensitively to the linked entities'
     labels/aliases — a hallucinated keyword can never render as a highlight.
   A card grade returns neither missed facts nor keywords: there is nothing to
   filter them against, and inventing either would fake provenance the card
   does not have.

   The answer text is persisted BEFORE the LLM call in the session flows, so a
   grading failure never loses what the learner typed. Billed per call
   (:kg.grade for a question, :cards.grade for a card item)."
  (:require
   [freememo.db :as db]
   [freememo.credits :as credits]
   [freememo.kg-llm :as llm]
   [freememo.cards :as cards]
   [freememo.settings :as settings]
   [freememo.text :as text]
   [freememo.toasts :as toasts]
   [freememo.user-state :as us]
   [taoensso.telemere :as tel]
   [clojure.string :as str]))

(defn- normalize-verdict [v]
  (let [s (str/lower-case (name (or v "")))]
    (#{"correct" "partial" "incorrect"} s)))

(defn verdict->rating
  "Map an LLM verdict to an FSRS rating (binary, per product decision):
   correct → Good (3); partial | incorrect → Again (1). Hard/Easy unused.

   Both graders share this mapping, so an LLM-graded card and an LLM-graded question
   feed the scheduler the same two ratings. Only the self-rating arm ever produces
   Hard or Easy (plans/cards-in-quiz-queue.md D9, limitation 3)."
  [verdict]
  (if (= verdict "correct") 3 1))

(defn- grade-failure!
  "Shared failure path for both graders: log it, tell the learner, return the failure
   value. The session flows persist the typed answer before grading, so Submit works
   again; the Review flow persists nothing, and this toast is its only recovery path.
   Post: {:success false :error msg :error-type kw?}."
  [user-id log-id e]
  (if (llm/insufficient-credits? e)
    (tel/log! {:level :info :id ::spend-refused :data {:user-id user-id}}
      "Grading refused: out of credits")
    (tel/error! {:id log-id} e))
  (toasts/push! user-id
    {:level :error
     :message (str "Grading failed: " (ex-message (llm/root-cause e))
                " — your answer was kept, try Submit again.")
     :actions (if (llm/insufficient-credits? e)
                [{:label "Top up credits" :nav :settings}]
                [])})
  {:success false
   :error (ex-message (llm/root-cause e))
   :error-type (when (llm/insufficient-credits? e) :insufficient-credits)})

(defn grade-question
  "Grade one answer against its question's facts WITHOUT persisting anything —
   the session-less core shared by the session quiz/exam (grade-answer!) and
   the FSRS Review flow (review-question!). Charges the grading call and toasts
   on failure; the caller owns all DB writes.
   Post: {:success true :verdict str :explanation str :missed-fact-ids [int]
          :missed-facts [fact-map] :matched-keywords [str]
          :reference-answer str :keywords [str]} or
         {:success false :error msg :error-type kw?}."
  [user-id question-id user-answer]
  (try
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
          gctx {:feature :kg-grade :user-id user-id}
          ;; unusable output is retried inside llm/chat!; :cost covers both attempts
          {:keys [parsed cost]} (llm/chat! api-key model-slug prompt payload gctx)
          verdict (or (normalize-verdict (:verdict parsed))
                      (throw (ex-info "Model returned no usable verdict" {})))
          linked-ids (into #{} (map :id) facts)
          missed (filterv linked-ids (:missed-fact-ids parsed))
          lexicon (into {} (map (juxt str/lower-case identity)) keywords)
          matched (into []
                    (comp (map str) (keep #(lexicon (str/lower-case %))) (distinct))
                    (:matched-keywords parsed))]
      (credits/record-cost-charge! user-id :kg.grade (:id entry) cost)
      {:success true
       :verdict verdict
       :explanation (str (:explanation parsed))
       :missed-fact-ids missed
       :missed-facts (filterv #(contains? (set missed) (:id %)) facts)
       :matched-keywords matched
       :reference-answer reference-answer
       :keywords keywords})
    (catch Exception e (grade-failure! user-id ::grade-question e))))

(defn grade-card
  "Grade one answer to a card item against that card's own reference answer.

   The rubric is a single phrasing with no fact graph behind it, so this is weaker
   than fact grading by construction — D2 accepts that, and D3 gives the learner a
   self-rating arm instead. HTML is stripped before the call: the model gets the text
   the learner sees, and the markup would only cost tokens. Charges the grading call
   and toasts on failure; the caller owns all DB writes.
   Pre:  ord is live for this card (db/get-card-item enforces it).
   Post: {:success true :verdict str :explanation str :reference-answer html
          :missed-fact-ids [] :missed-facts [] :matched-keywords []} or
         {:success false :error msg :error-type kw?}. The empty fact and keyword keys
         are present so one feedback shape serves both item types; :reference-answer
         keeps its HTML so the caller can render formatting and math."
  [user-id card-id ord user-answer]
  (try
    (let [{:keys [api-key entry model-slug]} (llm/resolve-model+gate! user-id :grade)
          {:keys [prompt answer]} (or (db/get-card-item user-id card-id ord)
                                    (throw (ex-info "Card item not found" {})))
          payload (pr-str {:prompt (text/strip-html prompt)
                           :reference-answer (text/strip-html answer)
                           :answer user-answer})
          tmpl (or (cards/load-prompt-template "card-grade.md")
                 (throw (ex-info "Failed to load card-grade prompt" {})))
          {:keys [parsed cost]} (llm/chat! api-key model-slug tmpl payload
                                  {:feature :card-grade :user-id user-id})
          verdict (or (normalize-verdict (:verdict parsed))
                    (throw (ex-info "Model returned no usable verdict" {})))]
      (credits/record-cost-charge! user-id :cards.grade (:id entry) cost)
      {:success true
       :verdict verdict
       :explanation (str (:explanation parsed))
       :reference-answer answer
       :missed-fact-ids [] :missed-facts [] :matched-keywords []})
    (catch Exception e (grade-failure! user-id ::grade-card e))))

(defn grade-answer!
  "Record + grade one answer inside a session (quiz/exam). Persists the answer
   text first (an LLM failure must not lose what was typed), grades via
   grade-question, then writes the verdict to kg_answers.
   Pre:  session belongs to user-id and question-id is in its draw (the UI
         iterates the session's own question_ids — violation = caller bug).
   Post: grade-question's result, with the verdict persisted to kg_answers; the
         raw answer is persisted even on grade failure."
  [user-id session-id question-id position user-answer]
  (let [answer-id (db/record-kg-answer! user-id session-id question-id position user-answer)]
    (if-not answer-id
      {:success false :error "Session not found"}
      (let [res (grade-question user-id question-id user-answer)]
        (when (:success res)
          (db/grade-kg-answer! answer-id (:verdict res) (:explanation res)
            (:missed-fact-ids res) (:matched-keywords res)))
        res))))

(defn- schedule-review!
  "Advance one item's FSRS schedule and append its kg_reviews row.

   Caught here, not thrown. This runs AFTER the billed LLM call on the graded arms,
   so letting it throw would propagate out of the caller's e/Offload and tear down
   the Electric session — the UI latches on \"Grading…\" with no way back and the
   user has already paid. Catching it degrades to graded-but-unscheduled: the verdict
   is shown, the item keeps its previous due date and so returns later.
   Post: the apply-fsrs-review! map, or nil when the write failed or the item is not
         this user's. Never throws for a DB fault."
  [user-id item-ref rating record]
  (let [{:keys [scheduler enable-fuzzing]} (settings/fsrs-config user-id)]
    (try
      (db/apply-fsrs-review! user-id item-ref rating record scheduler enable-fuzzing)
      (catch Exception e
        (tel/error! {:id ::review-schedule-failed
                     :data {:user-id user-id :item item-ref}} e)
        (toasts/push! user-id
          {:level :error
           :message "Your answer was graded, but its schedule could not be saved — this review will come up again."})
        nil))))

(defn review-question!
  "FSRS Review path (H2): grade session-lessly, then advance the question's FSRS
   schedule and append a kg_reviews row carrying the answer. Writes NO
   kg_answers/kg_sessions row — that row IS this flow's answer history.

   A failed grade writes nothing at all, so the typed answer is not retained; unlike
   the session flows there is no pre-grade persist to fall back on, and the toast
   pushed by grade-question is the only recovery path.
   Post: {:result <grade-question map> :schedule <apply-fsrs-review! map|nil>};
         :schedule is nil when grading failed (nothing written) OR when scheduling
         failed (grade shown, no review row, schedule untouched)."
  [user-id question-id user-answer]
  (let [res (grade-question user-id question-id user-answer)]
    (if-not (:success res)
      {:result res :schedule nil}
      {:result res
       :schedule (schedule-review! user-id [:question question-id]
                   (verdict->rating (:verdict res))
                   {:verdict (:verdict res)
                    :explanation (:explanation res)
                    :user-answer user-answer
                    :missed-fact-ids (:missed-fact-ids res)
                    :grade-source "ai"})})))

(defn review-card!
  "The LLM-graded card arm: grade the answer against the card's reference answer,
   then advance that item's schedule. Same shape as review-question!, so the Review
   flow treats the two item types identically once it has the result.
   Pre:  the card-quiz LLM setting is on (settings/card-quiz-llm-grading?) — the
         caller owns that check, since it also decides which UI arm to render.
   Post: {:result <grade-card map> :schedule <apply-fsrs-review! map|nil>}."
  [user-id card-id ord user-answer]
  (let [res (grade-card user-id card-id ord user-answer)]
    (if-not (:success res)
      {:result res :schedule nil}
      {:result res
       :schedule (schedule-review! user-id [:card card-id ord]
                   (verdict->rating (:verdict res))
                   {:verdict (:verdict res)
                    :explanation (:explanation res)
                    :user-answer user-answer
                    :grade-source "ai"})})))

(defn rate-card!
  "The self-rated card arm: no LLM call, no verdict — the learner's own rating IS the
   grade. Records grade_source 'self', which is what tells this row apart from an
   LLM-graded one and from a row written before the column existed.
   Pre:  rating ∈ #{1 2 3 4} (the caller renders exactly four buttons — any other
         value is a caller bug and is refused here rather than scheduled).
   Post: {:result {:success true :rating n} :schedule <map|nil>}, or
         {:result {:success false …} :schedule nil} for a rating outside 1..4."
  [user-id card-id ord rating]
  (if-not (#{1 2 3 4} rating)
    (do (tel/error! {:id ::rate-card-bad-rating
                     :data {:user-id user-id :card-id card-id :rating rating}}
          (ex-info "Rating outside 1..4" {:rating rating}))
      {:result {:success false :error "Invalid rating"} :schedule nil})
    {:result {:success true :rating rating}
     :schedule (schedule-review! user-id [:card card-id ord] rating
                 {:grade-source "self"})}))

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
