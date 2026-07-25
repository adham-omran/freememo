(ns freememo.question-curation
  "Per-question curation, shared by the Questions bank and the in-quiz control bar
   (plans/quiz-iteration.md §4, §6).

   Three axes, deliberately independent:
     edit      — rewrite the question text / reference answer
     flagged   — \"revisit this later\"; never filters a draw
     suspended — withheld from every draw until cleared

   Lives in its own namespace because both the bank (freememo.knowledge-page) and
   all three quiz flows (freememo.quiz-page) need the same controls, and because
   reaching into an 872-line page namespace for one modal would push the requiring
   page's e/defn bytecode toward the 64KB ceiling."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [clojure.string :as str]
   [freememo.icons :as icons]
   [freememo.modal-shell :as modal]
   [freememo.tooltip :as tooltip]
   #?(:clj [freememo.commands :as commands])
   #?(:clj [freememo.db :as db])
   #?(:clj [freememo.logging :as log])))

;; ---------------------------------------------------------------------------
;; Pure
;; ---------------------------------------------------------------------------

(defn question-edited?
  "True when a recorded question wording differs from the question's current
   wording — the condition that raises the edited-since marker in history detail.

   A nil or blank snapshot means the wording was never recorded (rows written
   before the column existed) and returns false: absence of a record is not
   evidence of a change. Comparison is whitespace-collapsed so reflowing or a
   trailing space does not read as an edit.
   Post: false whenever `snapshot` is nil/blank, regardless of `live`."
  [snapshot live]
  (let [norm #(some-> % str str/trim (str/replace #"\s+" " "))
        s (norm snapshot)]
    (boolean (and (not (str/blank? (str s)))
               (not= s (norm live))))))

;; ---------------------------------------------------------------------------
;; Server effects — whole defns under #?(:clj …) so CLJS misuse warns loudly
;; instead of silently nil'ing; called only inside e/server.
;; ---------------------------------------------------------------------------

#?(:clj
   (defn update-question!*
     "Edit a KG question's Q/A text. Audits + bumps on success.
      Post: {:success bool :error msg-or-nil}."
     [user-id question-id q a]
     (let [ok? (db/update-kg-question! user-id question-id q a)]
       (when ok?
         (commands/bump! user-id :generate-questions)
         (log/audit! {:id ::update-question :user-id user-id :action :update
                      :entity :kg-question :entity-id question-id}))
       {:success ok? :error (when-not ok? "Question not found")})))

#?(:clj
   (defn set-flags!*
     "Toggle a question's curation flags; nil field = leave unchanged. Writes no FSRS
      column, so a suspension neither freezes nor shifts the schedule.
      Pre:  at least one of :flagged / :suspended is non-nil (else this is a no-op
            write — caller bug).
      Post: :done, with :kg-mutations bumped so the bank and the scope picker
            re-query. Returns :done even when the row was missing: a flag toggle on
            a vanished question is silent by design, same as reject."
     [user-id question-id {:keys [suspended] :as flags}]
     (when (db/set-kg-question-flags! user-id question-id flags)
       ;; Both ids bump :kg-mutations; the distinction is what the audit trail and
       ;; the palette registry call this mutation. audit! whitelists its data keys,
       ;; so the flag values themselves are not loggable through it.
       (commands/bump! user-id (if (some? suspended) :suspend-question :flag-question))
       (log/audit! {:id ::set-question-flags :user-id user-id :action :update
                    :entity :kg-question :entity-id question-id}))
     :done))

;; ---------------------------------------------------------------------------
;; Controls
;; ---------------------------------------------------------------------------

(e/defn FlagToggle
  "Flag / unflag one question. Flagging never withholds the question — it only puts
   it on the bank's flagged worklist, which is where editing happens later."
  [user-id question-id flagged?]
  (e/client
    (dom/button
      (dom/props {:class (str "btn btn-sm" (when flagged? " btn-secondary"))
                  :aria-pressed (if flagged? "true" "false")
                  :aria-label (if flagged? "Remove flag" "Flag for later")})
      (tooltip/Tooltip! (if flagged?
                          "Flagged — find it under Flagged in the Questions bank"
                          "Flag for later; the question keeps appearing"))
      (icons/Icon :flag :size 14)
      (let [click (dom/On "click" (fn [e] (.stopPropagation e)
                                    {:id (str (random-uuid)) :to (not flagged?)}) nil)
            [t _] (e/Token click)]
        (dom/props {:disabled (some? t)})
        (when t
          (case (e/server (e/Offload #(set-flags!* user-id question-id
                                        {:flagged (:to click)})))
            (t)))))))

(e/defn SuspendToggle
  "Suspend / unsuspend one question — the bank's half of the pair. Suspending
   withholds it from every draw; unsuspending returns it, overdue if its due date
   passed while it was held."
  [user-id question-id suspended?]
  (e/client
    (dom/button
      (dom/props {:class (str "btn btn-sm" (when suspended? " btn-secondary"))
                  :aria-pressed (if suspended? "true" "false")
                  :aria-label (if suspended? "Unsuspend question" "Suspend question")})
      (tooltip/Tooltip! (if suspended?
                          "Suspended — returns to the queue when you unsuspend it"
                          "Suspend — withhold from every quiz and review"))
      (icons/Icon (if suspended? :eye-off :eye) :size 14)
      (let [click (dom/On "click" (fn [e] (.stopPropagation e)
                                    {:id (str (random-uuid)) :to (not suspended?)}) nil)
            [t _] (e/Token click)]
        (dom/props {:disabled (some? t)})
        (when t
          (case (e/server (e/Offload #(set-flags!* user-id question-id
                                        {:suspended (:to click)})))
            (t)))))))

(e/defn SuspendSkipButton
  "Suspend the current question and leave it — the in-quiz escape hatch.

   `advance!` is a plain client fn that moves the flow past this question (drop the
   queue head, bump the index, latch the exam submit). It runs in e/on-unmount
   registered INSIDE (when t …), so it fires only once the server write has
   returned and spent the token: registering it at the surrounding let scope would
   fire on any parent unmount, and calling it as a sibling of the e/server form
   would race it and cancel the write mid-flight.

   Pass advance! = nil after grading, where Next already advances: the button then
   suspends in place and the label drops \"& Skip\".
   Post: the question is suspended; no answer row, no review row, no FSRS advance —
         a skip is not an attempt."
  [user-id question-id advance!]
  (e/client
    (dom/button
      (dom/props {:class "btn btn-sm"
                  :aria-label (if advance! "Suspend and skip this question"
                                "Suspend this question")})
      (tooltip/Tooltip! (if advance!
                          "Withhold from future review and move on — this does not count as an answer"
                          "Withhold this question from future review"))
      (icons/Icon :eye-off :size 14)
      (dom/text (if advance! " Suspend & Skip" " Suspend"))
      (let [click (dom/On "click" (fn [_] {:id (str (random-uuid))}) nil)
            [t _] (e/Token click)]
        (dom/props {:disabled (some? t)})
        (when t
          (when advance! (e/on-unmount advance!))
          (case (e/server (e/Offload #(set-flags!* user-id question-id
                                        {:suspended true})))
            (t)))))))

(e/defn EditQuestionButton
  "Open the question editor on `question` (the map QuestionEditorModal expects)."
  [!editing question]
  (e/client
    (dom/button
      (dom/props {:class "btn btn-sm" :aria-label "Edit this question"})
      (tooltip/Tooltip! "Edit the question text or its reference answer")
      (icons/Icon :pen-line :size 14)
      (dom/On "click" (fn [e] (.stopPropagation e) (reset! !editing question)) nil))))

(e/defn CurationBar
  "The three in-quiz controls: Flag, Suspend (& Skip), Edit.

   All three quiz flows render exactly this row, so it lives here rather than being
   repeated per flow.
   Pre:  `qdata` is a get-kg-question-for-session map (carries :id and :flagged).
   Post: renders nothing when qdata is nil (no question on screen to curate)."
  [user-id qdata advance! !editing]
  (e/client
    (when-some [qid (:id qdata)]
      (dom/div
        (dom/props {:style {:display "flex" :gap "6px" :align-items "center"
                            :margin "8px 0"}})
        (FlagToggle user-id qid (:flagged qdata))
        (SuspendSkipButton user-id qid advance!)
        (EditQuestionButton !editing {:id qid
                                      :question (:question qdata)
                                      :reference_answer (:reference-answer qdata)})))))

(e/defn QuestionEditorModal
  "Edit a question's text and reference answer in a modal — plain textareas,
   since questions carry no rich formatting. `!editing` holds the question map
   being edited, or nil. Save writes both fields; Cancel/Escape/backdrop close.

   Editing does not touch any session's frozen draw: a quiz reads the question by id
   on every turn, so the new wording shows immediately, while history lists keep the
   wording snapshot taken when the answer was recorded."
  [user-id !editing]
  (e/client
    (when-some [q (e/watch !editing)]
      (let [qid (:id q)
            !question (atom (or (:question q) ""))
            !answer (atom (or (:reference_answer q) ""))
            close! (fn [] (reset! !editing nil))]
        (dom/div
          (dom/props {:class "modal-backdrop"})
          (dom/On "click" (fn [_] (close!)) nil)
          (modal/ModalEscape close! "Edit question")
          (dom/div
            (dom/props {:class "modal-content"
                        :style {:width "min(640px, 95vw)" :max-height "85vh"
                                :display "flex" :flex-direction "column" :gap "10px"}})
            (dom/On "click" (fn [e] (.stopPropagation e)) nil)
            (dom/h3 (dom/props {:style {:margin "0" :font-size "16px"}}) (dom/text "Edit question"))
            (dom/label (dom/props {:style {:font-size "13px" :color "var(--color-text-secondary)"}})
              (dom/text "Question"))
            (dom/textarea
              (dom/props {:class "input" :rows 3 :aria-label "Question"
                          :value (or (:question q) "")
                          :style {:width "100%" :resize "vertical"}})
              (dom/On "input" (fn [ev] (reset! !question (.. ev -target -value))) nil))
            (dom/label (dom/props {:style {:font-size "13px" :color "var(--color-text-secondary)"}})
              (dom/text "Reference answer"))
            (dom/textarea
              (dom/props {:class "input" :rows 5 :aria-label "Reference answer"
                          :value (or (:reference_answer q) "")
                          :style {:width "100%" :resize "vertical"}})
              (dom/On "input" (fn [ev] (reset! !answer (.. ev -target -value))) nil))
            (dom/div
              (dom/props {:style {:display "flex" :gap "8px" :justify-content "flex-end"}})
              (dom/button (dom/props {:class "btn btn-secondary"})
                (dom/text "Cancel")
                (dom/On "click" (fn [_] (close!)) nil))
              (dom/button
                (dom/props {:class "btn btn-primary"})
                (dom/text "Save")
                ;; Capture the edited values client-side in the click event, then
                ;; save on the server (an e/server thunk can't deref client atoms).
                (let [ev (dom/On "click"
                           (fn [_] {:id (str (random-uuid)) :q @!question :a @!answer}) nil)
                      [t _] (e/Token ev)]
                  (dom/props {:disabled (some? t)})
                  (when t
                    (let [result (e/server
                                   (e/Offload
                                     #(update-question!* user-id qid (:q ev) (:a ev))))]
                      (case result
                        (if (:success result)
                          (do (reset! !editing nil) (t))
                          (t (:error result)))))))))))))))
