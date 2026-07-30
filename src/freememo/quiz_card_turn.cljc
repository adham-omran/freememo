(ns freememo.quiz-card-turn
  "One Quiz Review turn over a CARD item, in either of two arms
   (plans/cards-in-quiz-queue.md D3).

     LLM arm   — type an answer, the server grades it against the card's own
                 reference answer, then the verdict is shown. Same loop as a
                 question turn; only the rubric is weaker.
     Self arm  — reveal the answer and rate it 1-4. No LLM call, no verdict.

   `settings/card-quiz-llm-grading?` picks the arm, and the master LLM switch
   outranks it, so a user with LLM features off always self-rates. Card text is
   stored HTML with `\\(TeX\\)` math, so every display site writes through
   `math/set-html!` rather than `dom/text` — a card carries formatting a question
   never does.

   Lives in its own namespace because ReviewFlow's method is already large and
   `:prod` bytecode limits bite per method (CLAUDE.md)."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.electric-forms5 :as forms]
   [clojure.string :as str]
   [freememo.math :as math]
   [freememo.quiz-feedback :as fb]
   #?(:clj [freememo.db :as db])
   #?(:clj [freememo.kg-grade :as grade])
   #?(:clj [freememo.settings :as settings])
   #?(:clj [freememo.user-state :as us])))

;; ---------------------------------------------------------------------------
;; Server queries/effects — whole defns under #?(:clj …) so CLJS misuse warns
;; loudly instead of silently nil'ing; called only inside e/server.
;; ---------------------------------------------------------------------------

#?(:clj
   (defn card-turn*
     "Everything one card turn needs, in one round trip: which arm to render, the
      prompt, and — on the self arm only — the answer.

      The LLM arm withholds :answer so the learner cannot read the reference out of
      the DOM before submitting; it arrives with the grade instead. The self arm IS
      revealing the answer, so it ships up front.
      Takes the settings bump so flipping the toggle switches arms without a reload.
      Post: {:llm-grading? bool :card-id :ord :kind :prompt :answer?} or nil when the
            card is gone, not owned, or its ord was edited away."
     [_settings-bump user-id card-id ord]
     (when-let [item (db/get-card-item user-id card-id ord)]
       (let [llm? (settings/card-quiz-llm-grading? user-id)]
         (cond-> (assoc item :llm-grading? llm?)
           llm? (dissoc :answer))))))

#?(:clj
   (defn grade-card-answer!*
     "LLM arm: grade the typed answer and advance the item's schedule.
      Post: {:result <grade map> :schedule {:due-today? …}|nil}."
     [user-id card-id ord answer]
     (grade/review-card! user-id card-id ord answer)))

#?(:clj
   (defn rate-card!*
     "Self arm: record the learner's own 1-4 rating and advance the schedule.
      Post: {:result {:success bool} :schedule {:due-today? …}|nil}."
     [user-id card-id ord rating]
     (grade/rate-card! user-id card-id ord rating)))

;; ---------------------------------------------------------------------------
;; Views
;; ---------------------------------------------------------------------------

(def ^:private label-style
  {:font-size "12px" :color "var(--color-text-secondary)" :margin-top "10px"})

(def ^:private ratings
  "The four FSRS ratings, in the order Anki shows them. `Again` is the lapse."
  [[1 "Again"] [2 "Hard"] [3 "Good"] [4 "Easy"]])

(e/defn CardHtml
  "One card field's HTML. `dir=\"auto\"` flips to RTL for Arabic content; math is
   typeset by math/set-html!, never by a bare innerHTML write."
  [html size]
  (e/client
    (dom/div
      (dom/props {:dir "auto" :style {:font-size size :line-height "1.5"
                                      :text-align "start"}})
      (math/set-html! dom/node html))))

(e/defn CardFeedback
  "The LLM arm's graded result: verdict badge, explanation, reference answer, and
   what the learner typed.

   Deliberately NOT freememo.quiz-feedback/QuizFeedback: that component renders its
   answers as plain text through the entity-highlighting path, which would show a
   card's HTML literally. A card has no entities or missed facts to show, so what is
   left is the badge — shared here via fb/verdict-badge — plus two rendered fields."
  [result user-answer]
  (e/client
    (let [{:keys [verdict explanation reference-answer]} result
          [label color] (get fb/verdict-badge verdict ["?" "inherit"])]
      (dom/div
        (dom/props {:style {:border "1px solid var(--color-border)"
                            :border-radius "8px" :padding "12px 16px" :margin-top "12px"}})
        (dom/div
          (dom/props {:style {:font-weight "700" :color color :font-size "15px"}})
          (dom/text label))
        (dom/p (dom/props {:style {:margin "8px 0" :font-size "14px"}})
          (dom/text explanation))
        (dom/div (dom/props {:style label-style}) (dom/text "Reference answer"))
        (CardHtml reference-answer "14px")
        (dom/div (dom/props {:style label-style}) (dom/text "Your answer"))
        (dom/p (dom/props {:style {:font-size "14px" :margin "4px 0"}})
          (dom/text user-answer))))))

(e/defn AnswerForm
  "LLM arm, before the grade: the answer box and Submit.

   Forms5 Input! + Button! via e/amb — never `do` here, it would swallow whichever
   token is not last and hang that control. Field edits mirror into `!draft` and ack
   at once (no server round-trip per keystroke); Submit grades on the server."
  [user-id card-id ord !draft draft !grading? grading? !feedback]
  (e/client
    (dom/div
      (e/for [[t edit] (e/amb
                         (forms/Input! :card/answer draft :as :textarea
                           :class "form-input" :rows 4
                           :placeholder "Answer in your own words…"
                           :aria-label "Your answer"
                           :style {:width "100%" :resize "vertical"})
                         (forms/Button! [::submit]
                           :class "btn btn-primary" :style {:margin-top "8px"}
                           :label (if grading? "Grading…" "Submit")
                           :disabled (or grading? (str/blank? (str draft)))))]
        (if (map? edit)
          (do (reset! !draft (:card/answer edit)) (t))
          (let [answer (str/trim (str draft))]
            (if (str/blank? answer)
              (t) ; defence in depth — the button is disabled for this
              (do (reset! !grading? true)
                (let [res (e/server
                            (e/Offload #(grade-card-answer!* user-id card-id ord answer)))]
                  (case res ; wait on the value — `do` would race the token
                    (do (reset! !grading? false)
                      (when (:success (:result res))
                        (reset! !feedback {:result (:result res)
                                           :answer answer
                                           :schedule (:schedule res)}))
                      (t))))))))))))

(e/defn RatingBar
  "Self arm: the four ratings. The rating IS the grade, so pressing one records the
   review and leaves the card — there is no separate Next.

   `advance!` takes requeue?, read from the schedule the server just wrote: a card
   still due today (an FSRS learning step) comes back in this sitting.

   `advance!` mutates the queue, which unmounts this very component, so it runs from
   `e/on-unmount` registered INSIDE the resolved branch — calling it as a sibling of
   `(t)` would tear the frame down mid-flight and the token would never be spent
   (CLAUDE.md, Token Lifecycle). A failed rating advances nothing: the item keeps its
   place and its due date."
  [user-id card-id ord !rating-busy? rating-busy? advance!]
  (e/client
    (dom/div
      (dom/props {:style {:display "flex" :gap "8px" :margin-top "12px"}})
      (e/for [[t cmd] (e/amb
                        (forms/Button! [::rate 1] :class "btn" :label "Again"
                          :disabled rating-busy?)
                        (forms/Button! [::rate 2] :class "btn" :label "Hard"
                          :disabled rating-busy?)
                        (forms/Button! [::rate 3] :class "btn btn-primary" :label "Good"
                          :disabled rating-busy?)
                        (forms/Button! [::rate 4] :class "btn" :label "Easy"
                          :disabled rating-busy?))]
        ;; Latch before the call so a second rating cannot race the first. The keyed
        ;; frame in ReviewFlow remounts this component per item, which clears it.
        (reset! !rating-busy? true)
        (let [rating (second cmd)
              res (e/server (e/Offload #(rate-card!* user-id card-id ord rating)))]
          (case res
            (if (:success (:result res))
              (do (e/on-unmount
                    #(advance! (boolean (get-in res [:schedule :due-today?]))))
                (t))
              (do (reset! !rating-busy? false)
                (t "That rating could not be recorded.")))))))))

(e/defn SelfArm
  "Self arm: prompt, then Show answer, then the reference answer plus the ratings.

   The reveal is browser-local state, so it needs no token — nothing leaves the
   client until a rating is pressed."
  [user-id card-id ord answer advance!]
  (e/client
    (let [!revealed? (atom false) revealed? (e/watch !revealed?)
          !rating-busy? (atom false) rating-busy? (e/watch !rating-busy?)]
      (if revealed?
        (dom/div
          (dom/div (dom/props {:style label-style}) (dom/text "Answer"))
          (CardHtml answer "15px")
          (RatingBar user-id card-id ord !rating-busy? rating-busy? advance!))
        (dom/button
          (dom/props {:class "btn btn-primary" :style {:margin-top "12px"}})
          (dom/text "Show answer")
          (dom/On "click" (fn [_] (reset! !revealed? true)) nil))))))

(e/defn LlmArm
  "LLM arm: the answer box until a grade arrives, then the verdict and Next."
  [user-id card-id ord !feedback feedback advance!]
  (e/client
    (let [!draft (atom "") draft (e/watch !draft)
          !grading? (atom false) grading? (e/watch !grading?)]
      (if (nil? feedback)
        (AnswerForm user-id card-id ord !draft draft !grading? grading? !feedback)
        (dom/div
          (CardFeedback (:result feedback) (:answer feedback))
          (dom/button
            (dom/props {:class "btn btn-primary" :style {:margin-top "12px"}})
            (dom/text "Next")
            (dom/On "click"
              (fn [_] (advance! (boolean (get-in feedback [:schedule :due-today?]))))
              nil)))))))

(e/defn CardReviewBody
  "History detail for one CARD review: the prompt as asked, the reference answer, and
   — when an LLM graded it — the explanation and the typed answer.

   No curation controls: Flag, Suspend and Edit act on kg_questions, and a card row
   has no question id to give them. Fields render as HTML for the same reason the
   turn does; a plain-text detail view would show a card's markup literally.
   Pre:  `review` is a kg-review-detail :review map with :flashcard_id set.
   Post: renders the reference answer only when it is still resolvable — a deletion
         edited away since the review leaves nothing to show."
  [review]
  (e/client
    (let [{:keys [prompt live_question reference_answer explanation user_answer
                  grade_source]} review
          self? (= "self" grade_source)]
      (dom/div
        (CardHtml (or live_question prompt) "16px")
        (when (and prompt live_question (not= prompt live_question))
          (dom/div
            (fb/EditedSinceNote true)
            (dom/div (dom/props {:style label-style}) (dom/text "You answered"))
            (CardHtml prompt "14px")))
        (dom/div (dom/props {:style label-style})
          (dom/text (if self? "Self rated" "AI graded")))
        (when (not-empty (str reference_answer))
          (dom/div
            (dom/div (dom/props {:style label-style}) (dom/text "Answer"))
            (CardHtml reference_answer "14px")))
        (when (not-empty (str explanation))
          (dom/div
            (dom/div (dom/props {:style label-style}) (dom/text "Grader"))
            (dom/p (dom/props {:style {:font-size "14px" :margin "4px 0"}})
              (dom/text (str explanation)))))
        (when (not-empty (str user_answer))
          (dom/div
            (dom/div (dom/props {:style label-style}) (dom/text "Your answer"))
            (dom/p (dom/props {:style {:font-size "14px" :margin "4px 0"}})
              (dom/text (str user_answer)))))))))

(e/defn CardTurn
  "One card item's turn. Renders whichever arm the setting selects.

   `advance!` takes requeue? and counts the item as reviewed; `skip!` leaves without
   counting, and is reachable only when the item has vanished. A card has no curation
   controls, so those are the only two exits (D8: the curation bar is question-only).
   Pre:  `item-ref` is [:card flashcard-id ord] and its ord was live at draw time.
   Post: an item that has since vanished — the card deleted, or the deletion edited
         away in another tab — renders a Skip instead. A card has no curation bar to
         escape with, so without this the sitting would dead-end on it."
  [user-id item-ref !feedback feedback advance! skip!]
  (e/client
    (let [[_ card-id ord] item-ref
          settings-bump (e/server (e/watch (us/get-atom user-id :settings-refresh)))
          item (e/server (card-turn* settings-bump user-id card-id ord))]
      (if (nil? item)
        (dom/div
          (dom/p (dom/props {:style {:font-size "14px" :margin "14px 0"}})
            (dom/text "This card is no longer available."))
          (dom/button
            (dom/props {:class "btn"})
            (dom/text "Skip")
            (dom/On "click" (fn [_] (skip!)) nil)))
        (dom/div
          (dom/div
            (dom/props {:style {:display "flex" :align-items "center" :gap "8px"
                                :margin "14px 0 6px"}})
            (dom/span
              (dom/props {:class (str "type-badge type-badge-" (:kind item))})
              (dom/text (:kind item)))
            (dom/span
              (dom/props {:style {:font-size "12px" :color "var(--color-text-secondary)"}})
              (dom/text (if (:llm-grading? item) "AI graded" "Self rated"))))
          (CardHtml (:prompt item) "16px")
          (if (:llm-grading? item)
            (LlmArm user-id card-id ord !feedback feedback advance!)
            (SelfArm user-id card-id ord (:answer item) advance!)))))))
