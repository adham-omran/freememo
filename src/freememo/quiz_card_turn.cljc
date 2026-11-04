(ns freememo.quiz-card-turn
  "One Quiz Review turn over a CARD item, in either of two arms
   (plans/cards-in-quiz-queue.md D3).

     LLM arm   — type an answer, the server grades it against the card's own
                 reference answer, then the verdict is shown. Same loop as a
                 question turn; only the rubric is weaker.
     Self arm  — reveal the answer and rate it 1-4. No LLM call, no verdict.

   `settings/card-quiz-llm-grading?` picks the arm, and the master LLM switch
   outranks it, so a user with LLM features off always self-rates. An empty credit
   balance outranks it too (kg-grade/card-grading-arm): a card self-rates rather
   than ask for an answer the gate will refuse. If credits run out mid-turn, the
   refused grade falls back to the same 1-4 bar (SelfRateFallback), which the
   question turn in freememo.quiz-page uses as well. Card text is
   stored HTML with `\\(TeX\\)` math, so every display site writes through
   `math/set-html!` rather than `dom/text` — a card carries formatting a question
   never does.

   Lives in its own namespace because ReviewFlow's method is already large and
   `:prod` bytecode limits bite per method (docs/electric-patterns.md)."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.electric-forms5 :as forms]
   [clojure.string :as str]
   [freememo.inline-error :as inline-error]
   [freememo.math :as math]
   [freememo.modal-shell :as modal-shell]
   [freememo.motion :as motion]
   [freememo.quiz-feedback :as fb]
   [freememo.turn-timing :as timing]
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
      Takes the settings bump so flipping the toggle switches arms without a reload,
      and the credits bump so a top-up does the same (grade/card-grading-arm).
      Post: {:llm-grading? bool :credits-out? bool :card-id :ord :kind :prompt
             :answer?} or nil when the card is gone, not owned, or its ord was
            edited away."
     [_settings-bump _credits-bump user-id card-id ord]
     (when-let [item (db/get-card-item user-id card-id ord)]
       (let [{:keys [llm-grading?] :as arm} (grade/card-grading-arm user-id)]
         (cond-> (merge item arm)
           llm-grading? (dissoc :answer))))))

#?(:clj
   (defn grade-card-answer!*
     "LLM arm: grade the typed answer and advance the item's schedule.
      Pre:  `duration-ms` is the turn's client measurement, or nil (untimed).
      Post: {:result <grade map> :schedule {:due-today? …}|nil}."
     [user-id card-id ord answer duration-ms]
     (grade/review-card! user-id card-id ord answer duration-ms)))

#?(:clj
   (defn rate-item!*
     "Record the learner's own 1-4 rating for one item and advance its schedule: the
      card self arm, and the fallback for either item type when grading is refused for
      credits.
      Pre:  `item-ref` is [:card flashcard-id ord] or [:question id]. `duration-ms`
            spans mount to rating, so it includes the reveal. `user-answer` is the
            typed answer on the fallback, nil on the self arm.
      Post: {:result {:success bool} :schedule {:due-today? …}|nil}."
     [user-id item-ref rating duration-ms user-answer]
     (let [[kind a b] item-ref]
       (if (= :card kind)
         (grade/rate-card! user-id a b rating duration-ms user-answer)
         (grade/rate-question! user-id a rating duration-ms user-answer)))))

;; ---------------------------------------------------------------------------
;; Views
;; ---------------------------------------------------------------------------

(def ^:private label-style
  {:font-size "12px" :color "var(--color-text-secondary)" :margin-top "10px"})

(def ^:private ratings
  "The four FSRS ratings, in the order Anki shows them. `Again` is the lapse."
  [[1 "Again"] [2 "Hard"] [3 "Good"] [4 "Easy"]])

;; Bare 1-4 press the matching rating, as Anki does. They bind ONLY on the revealed
;; self arm, and that is what makes an unmodified key safe: that subtree holds no
;; text input, so nothing there can be typing a digit. A modifier is refused so the
;; chord above and browser shortcuts keep their meaning, and the click goes through
;; the rendered button rather than around it — the button owns the token, the busy
;; latch and the advance, and a second path to the same effect would drift.
;;
;; Top-level platform-split defn, never an inline #?(:cljs) in an e/defn body: a
;; handler must capture the same bindings on both peers or the frame slot counts
;; diverge and the server crashes on mount (docs/electric-patterns.md, modal-shell precedent).
#?(:cljs
   (defn rating-key-press! [e]
     (when-not (or (.-metaKey e) (.-ctrlKey e) (.-altKey e) (.-shiftKey e))
       (when (#{"1" "2" "3" "4"} (.-key e))
         (when-let [btn (.querySelector (.-currentTarget e)
                          (str "button[data-rating='" (.-key e) "']:not([disabled])"))]
           (.preventDefault e)
           (.click btn))))))
#?(:clj (defn rating-key-press! [_e] nil))

(e/defn CardHtml
  "One card field's HTML. Math is typeset by math/set-html!, never by a bare
   innerHTML write.

   Direction: `dir=\"auto\"` picks the container's direction from its content, and
   `.bidi-per-block` (`unicode-bidi: plaintext`) makes each block child resolve its
   own — a card whose first block is Latin and whose body is Arabic needs both.
   Sets NO `text-align`: an explicit `start` is not resolved against a
   dir=\"auto\"-computed rtl direction in Chromium (index.css, .ql-editor), while
   the initial value aligns per paragraph correctly
   (plans/quiz-rtl-support.md §3.1, §4.1)."
  [html size]
  (e/client
    (dom/div
      (dom/props {:dir "auto" :class "bidi-per-block fm-rich"
                  :style {:font-size size :line-height "1.5"}})
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
        (dom/p (dom/props {:dir "auto" :style {:margin "8px 0" :font-size "14px"}})
          (dom/text explanation))
        (dom/div (dom/props {:style label-style}) (dom/text "Reference answer"))
        (CardHtml reference-answer "14px")
        (dom/div (dom/props {:style label-style}) (dom/text "Your answer"))
        (dom/p (dom/props {:dir "auto" :style {:font-size "14px" :margin "4px 0"}})
          (dom/text user-answer))))))

(defn graded-feedback
  "What a turn shows once its grade call returns, shared by the card and question
   turns.
   Post: the graded map {:result :answer :schedule} on success, plus
         :schedule-error when the grade was shown but its schedule was not saved;
         {:self-rate result :answer} on a credit refusal, which the turn answers
         with SelfRateFallback; nil on any other failure, which leaves the answer
         box up so Submit can be tried again (`grade-error` says why)."
  [res answer]
  (let [result (:result res)]
    (cond
      (:success result) (cond-> {:result result :answer answer :schedule (:schedule res)}
                          (:schedule-error res) (assoc :schedule-error (:schedule-error res)))
      (= :insufficient-credits (:error-type result)) {:self-rate result :answer answer}
      :else nil)))

(defn grade-error
  "The error a turn shows beside Submit after a failed grade, or nil.
   A credit refusal is not one: the turn answers it with the self-rating bar.
   Pre: `res` is the {:result …} map review-card! or review-question! returned."
  [res]
  (let [result (:result res)]
    (when (and (not (:success result))
            (not= :insufficient-credits (:error-type result)))
      (or (:error result) "Grading failed."))))

(e/defn AnswerForm
  "LLM arm, before the grade: the answer box and Submit.

   Forms5 Input! + Button! via e/amb — never `do` here, it would swallow whichever
   token is not last and hang that control. Field edits mirror into `!draft` and ack
   at once (no server round-trip per keystroke); Submit grades on the server."
  [user-id card-id ord !draft draft !grading? grading? !feedback started-at]
  (e/client
    (let [!grade-error (atom nil) grade-error (e/watch !grade-error)]
    (dom/div
      (e/for [[t edit] (e/amb
                         (forms/Input! :card/answer draft :as :textarea
                           :class "input" :rows 4 :dir "auto"
                           :placeholder "Answer in your own words…"
                           :aria-label "Your answer"
                           :autofocus true
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
                (reset! !grade-error nil)
                ;; Client scope: pass the number, never the mark inside the
                ;; server-sited closure (docs/electric-patterns.md, unserializable capture).
                (let [ms (timing/elapsed-ms started-at)
                      res (e/server
                            (e/Offload #(grade-card-answer!* user-id card-id ord answer ms)))]
                  (case res ; wait on the value — `do` would race the token
                    (do (reset! !grading? false)
                      (reset! !grade-error (grade-error res))
                      (reset! !feedback (graded-feedback res answer))
                      (t)))))))))
      (inline-error/InlineError grade-error)))))

(e/defn RatingBar
  "Self arm: the four ratings. The rating IS the grade, so pressing one records the
   review and leaves the card — there is no separate Next.

   `advance!` takes requeue?, read from the schedule the server just wrote: a card
   still due today (an FSRS learning step) comes back in this sitting.

   `advance!` mutates the queue, which unmounts this very component, so it runs from
   `e/on-unmount` registered INSIDE the resolved branch — calling it as a sibling of
   `(t)` would tear the frame down mid-flight and the token would never be spent
   (docs/electric-patterns.md, Token Lifecycle). A failed rating advances nothing: the item keeps its
   place and its due date.

   `item-ref` is [:card flashcard-id ord] or [:question id]; `user-answer` is the typed
   answer when this bar stands in for a refused grade, else nil."
  [user-id item-ref user-answer !rating-busy? rating-busy? advance! started-at]
  (e/client
    (let [!rate-error (atom nil) rate-error (e/watch !rate-error)]
    (dom/div
      (dom/props {:style {:display "flex" :gap "8px" :margin-top "12px"}})
      ;; data-rating is what rating-key-press! resolves a keystroke through, so the
      ;; key finds the right button by its meaning rather than by its position.
      (e/for [[t cmd] (e/amb
                        (forms/Button! [::rate 1] :class "btn" :label "Again (1)"
                          :data-rating "1" :disabled rating-busy?)
                        (forms/Button! [::rate 2] :class "btn" :label "Hard (2)"
                          :data-rating "2" :disabled rating-busy?)
                        (forms/Button! [::rate 3] :class "btn btn-primary" :label "Good (3)"
                          :data-rating "3" :disabled rating-busy?)
                        (forms/Button! [::rate 4] :class "btn" :label "Easy (4)"
                          :data-rating "4" :disabled rating-busy?))]
        ;; Latch before the call so a second rating cannot race the first. The keyed
        ;; frame in ReviewFlow remounts this component per item, which clears it.
        (reset! !rating-busy? true)
        (reset! !rate-error nil)
        (let [rating (second cmd)
              ms (timing/elapsed-ms started-at)
              res (e/server
                    (e/Offload #(rate-item!* user-id item-ref rating ms user-answer)))]
          (case res
            (if (and (:success (:result res)) (not (:schedule-error res)))
              (do (e/on-unmount
                    #(advance! (boolean (get-in res [:schedule :due-today?]))))
                (t))
              (do (reset! !rating-busy? false)
                (reset! !rate-error (or (:schedule-error res)
                                      "That rating could not be recorded."))
                (t))))))
      (inline-error/InlineError rate-error)))))

(e/defn SelfArm
  "Self arm: prompt, then Show answer, then the reference answer plus the ratings.

   The reveal is browser-local state, so it needs no token — nothing leaves the
   client until a rating is pressed. `started-at` is the TURN's mark, not the
   reveal's: the recorded duration spans the reveal, because revealing is part of
   answering."
  [user-id item-ref answer advance! started-at]
  (e/client
    (let [!revealed? (atom false) revealed? (e/watch !revealed?)
          !rating-busy? (atom false) rating-busy? (e/watch !rating-busy?)]
      (if revealed?
        (dom/div
          ;; Bare 1-4 rate. The key needs focus INSIDE this div, and the reveal
          ;; button that had it has just unmounted — so the region takes focus
          ;; itself, the tabindex="-1" + rAF pattern ModalEscape uses. Cmd/Ctrl+Enter
          ;; is handled by CardTurn's container above and takes Good, the primary.
          (dom/props {:tabindex "-1"})
          (e/snapshot (modal-shell/focus-on-mount! dom/node))
          ;; The answer opens down from the prompt. The rating keys stay
          ;; instant: the motion does not delay focus or the buttons.
          (e/snapshot (motion/open-down! dom/node))
          (dom/On "keydown" rating-key-press! nil)
          (dom/div (dom/props {:style label-style}) (dom/text "Answer"))
          (CardHtml answer "15px")
          (RatingBar user-id item-ref nil !rating-busy? rating-busy? advance! started-at))
        (dom/button
          (dom/props {:class "btn btn-primary" :style {:margin-top "12px"}})
          (dom/text "Show answer")
          ;; Focused on mount so Enter or the chord reveals without a click first.
          (e/snapshot (modal-shell/focus-on-mount! dom/node))
          (dom/On "click" (fn [_] (reset! !revealed? true)) nil))))))

(e/defn SelfRateFallback
  "Stands in for a grade the credit gate refused, on either item type: the learner
   compares what they typed with the reference answer and rates it 1-4, as on the
   self arm. Nothing was charged and nothing was written by the refused call, so this
   rating is the turn's only review row.

   `html?` says how to render the reference: a card's answer is stored HTML, a
   question's is plain text. `advance!` takes requeue?, as RatingBar's does."
  [user-id item-ref user-answer reference-answer html? advance! started-at]
  (e/client
    (let [!rating-busy? (atom false) rating-busy? (e/watch !rating-busy?)]
      (dom/div
        ;; Bare 1-4 rate, as on the self arm. The answer box that had focus has just
        ;; unmounted, so the region takes focus itself for the keys to reach it.
        (dom/props {:tabindex "-1"})
        (e/snapshot (modal-shell/focus-on-mount! dom/node))
        (e/snapshot (motion/open-down! dom/node))
        (dom/On "keydown" rating-key-press! nil)
        (dom/p
          (dom/props {:style {:font-size "13px" :margin "12px 0 0"
                              :color "var(--color-text-secondary)"}})
          (dom/text (str "AI grading needs credits, and your balance is empty. "
                      "Compare your answer with the reference and rate it yourself. "
                      "Top up in Settings to have AI grade again.")))
        (dom/div (dom/props {:style label-style}) (dom/text "Reference answer"))
        (if html?
          (CardHtml reference-answer "15px")
          (dom/p (dom/props {:dir "auto" :style {:font-size "15px" :margin "4px 0"}})
            (dom/text (str reference-answer))))
        (dom/div (dom/props {:style label-style}) (dom/text "Your answer"))
        (dom/p (dom/props {:dir "auto" :style {:font-size "14px" :margin "4px 0"}})
          (dom/text (str user-answer)))
        (RatingBar user-id item-ref user-answer !rating-busy? rating-busy?
          advance! started-at)))))

(e/defn LlmArm
  "LLM arm: the answer box until a grade arrives, then the verdict and Next — or,
   when the grade was refused for credits, the self-rating fallback."
  [user-id item-ref !feedback feedback advance! started-at]
  (e/client
    (let [[_ card-id ord] item-ref
          !draft (atom "") draft (e/watch !draft)
          !grading? (atom false) grading? (e/watch !grading?)]
      (cond
        (nil? feedback)
        (AnswerForm user-id card-id ord !draft draft !grading? grading? !feedback started-at)

        (some? (:self-rate feedback))
        (SelfRateFallback user-id item-ref (:answer feedback)
          (get-in feedback [:self-rate :reference-answer]) true advance! started-at)

        :else
        (dom/div
          ;; The verdict rises in as the result of the answer.
          (e/snapshot (motion/rise-in! dom/node))
          (CardFeedback (:result feedback) (:answer feedback))
          (dom/button
            (dom/props {:class "btn btn-primary" :style {:margin-top "12px"}})
            (dom/text "Next")
            ;; Focus on mount — the answer box that had focus is gone by now, so
            ;; without this neither Enter nor the chord reaches anything.
            (e/snapshot (modal-shell/focus-on-mount! dom/node))
            (dom/On "click"
              (fn [_] (advance! (boolean (get-in feedback [:schedule :due-today?]))))
              nil))
          (inline-error/InlineError (:schedule-error feedback)))))))

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
            (dom/p (dom/props {:dir "auto" :style {:font-size "14px" :margin "4px 0"}})
              (dom/text (str explanation)))))
        (when (not-empty (str user_answer))
          (dom/div
            (dom/div (dom/props {:style label-style}) (dom/text "Your answer"))
            (dom/p (dom/props {:dir "auto" :style {:font-size "14px" :margin "4px 0"}})
              (dom/text (str user_answer)))))))))

(defn arm-label
  "The card turn's grading label. Says why when the setting asked for AI grading and
   the credits refused it, so the switch to self-rating does not look like a setting
   change."
  [item]
  (cond
    (:llm-grading? item) "AI graded"
    (:credits-out? item) "Self rated (no credits for AI grading)"
    :else "Self rated"))

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
          credits-bump (e/server (e/watch (us/get-atom user-id :credits-refresh)))
          ;; e/snapshot, so flipping the grading setting mid-turn re-renders the arm
          ;; without restarting the clock. ReviewFlow's keyed frame remounts this
          ;; component per item, which is what makes the mark per-turn.
          started-at (e/snapshot (timing/start-ms))
          item (e/server
                 (e/Offload #(card-turn* settings-bump credits-bump user-id card-id ord)))]
      (if (nil? item)
        (dom/div
          (dom/p (dom/props {:style {:font-size "14px" :margin "14px 0"}})
            (dom/text "This card is no longer available."))
          (dom/button
            (dom/props {:class "btn"})
            (dom/text "Skip")
            (dom/On "click" (fn [_] (skip!)) nil)))
        (dom/div
          ;; One chord handler for both arms: Cmd/Ctrl+Enter clicks whichever button
          ;; is primary right now — Submit, then Next on the LLM arm; Show answer,
          ;; then Good on the self arm. See modal-shell/mod-enter-click-primary!.
          (dom/On "keydown" modal-shell/mod-enter-click-primary! nil)
          (dom/div
            (dom/props {:style {:display "flex" :align-items "center" :gap "8px"
                                :margin "14px 0 6px"}})
            (dom/span
              (dom/props {:class (str "type-badge type-badge-" (:kind item))})
              (dom/text (:kind item)))
            (dom/span
              (dom/props {:style {:font-size "12px" :color "var(--color-text-secondary)"}})
              (dom/text (arm-label item))))
          (CardHtml (:prompt item) "16px")
          (if (:llm-grading? item)
            (LlmArm user-id item-ref !feedback feedback advance! started-at)
            (SelfArm user-id item-ref (:answer item) advance! started-at)))))))
