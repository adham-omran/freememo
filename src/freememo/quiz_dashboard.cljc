(ns freememo.quiz-dashboard
  "The Quiz tab's landing surface (plans/quiz-iteration.md §8).

   Tiles over two virtual-scrolled history tabs:
     Reviews  — one row per graded review answer (the FSRS flow keeps no session,
                so kg_reviews IS its answer history)
     Sessions — one row per finished quiz/exam sitting

   Both lists render the question wording SNAPSHOT taken when the answer was
   recorded, so a later edit cannot rewrite the record; both detail views read the
   question LIVE by id and mark the divergence. Replaces the old ReviewHistory
   (two unwindowed tables) and QuizHistoryModal (a separate overlay)."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.electric-scroll0 :refer [Tape]]
   [freememo.scroll :refer [Scroll-window]]
   [freememo.icons :as icons]
   [freememo.question-curation :as curate]
   [freememo.quiz-feedback :as fb]
   [freememo.tooltip :as tooltip]
   #?(:clj [freememo.db :as db])
   #?(:clj [freememo.settings :as settings])
   #?(:clj [freememo.user-state :as us])))

(def ^:private panel-style
  {:max-width "760px" :margin "0 auto" :padding "16px"})

(def ^:private row-height 44)

;; The Reviews log is windowed, so the cap exists only to bound one wire payload.
;; Deliberately far above any plausible sitting count — a user with 500 reviews
;; scrolls all 500, which the old 30-day/50-row caps could not do.
(def ^:private review-log-limit 2000)

;; ---------------------------------------------------------------------------
;; Server queries — whole defns under #?(:clj …) so CLJS misuse warns loudly
;; instead of silently nil'ing; called only inside e/server.
;; ---------------------------------------------------------------------------

#?(:clj
   (defn dashboard-stats*
     "The tile numbers. Due count runs the same draw the Start button opens, under
      the same daily caps, so the tile can never disagree with the queue.
      Post: {:due n :reviewed-today n :new-today n :live n :flagged n :suspended n}."
     [_kg-bump user-id]
     (let [{:keys [new-per-day review-per-day]} (settings/fsrs-config user-id)
           {:keys [new-today reviews-today]} (db/fsrs-daily-counts user-id)]
       (merge (db/kg-question-bank-counts user-id)
         {:due (db/fsrs-due-count user-id new-per-day review-per-day)
          :reviewed-today (+ new-today reviews-today)
          :new-today new-today}))))

#?(:clj
   (defn review-log* [_kg-bump user-id]
     (db/fsrs-review-log user-id review-log-limit)))

#?(:clj
   (defn review-detail* [_kg-bump user-id review-id]
     (db/kg-review-detail user-id review-id)))

#?(:clj
   (defn history-sessions* [user-id]
     (db/list-kg-sessions user-id)))

#?(:clj
   (defn session-detail* [_kg-bump user-id session-id]
     (db/kg-session-detail user-id session-id)))

;; ---------------------------------------------------------------------------
;; Pure
;; ---------------------------------------------------------------------------

(defn session-score
  "Σ(1 / 0.5 / 0) over `total` questions, as a rounded percentage."
  [correct partial total]
  (when (pos? (or total 0))
    (int (+ 0.5 (* 100.0 (/ (+ correct (* 0.5 partial)) total))))))

(e/defn AnsweredAs
  "The wording as answered, shown beneath a detail view's live question when the two
   differ. Detail views lead with the LIVE question — the row is traversed by
   question id — so the recorded wording is the secondary fact here, the inverse of
   the list, which leads with the snapshot and never moves.
   Post: renders nothing unless the question was reworded since it was answered."
  [snapshot live]
  (e/client
    (when (curate/question-edited? snapshot live)
      (dom/div
        (fb/EditedSinceNote true)
        (dom/p
          (dom/props {:style {:font-size "13px" :margin "2px 0 0"
                              :color "var(--color-text-secondary)"}})
          (dom/text (str "You answered: " snapshot)))))))

;; ---------------------------------------------------------------------------
;; Tiles
;; ---------------------------------------------------------------------------

(e/defn StatTile [label value hint accent]
  (e/client
    (dom/div
      (dom/props {:style {:flex "1" :min-width "110px" :padding "10px 12px"
                          :border "1px solid var(--color-border)"
                          :border-radius "var(--radius-md)"
                          :background "var(--color-bg-card)"}})
      (tooltip/Tooltip! hint)
      (dom/div
        (dom/props {:style {:font-size "22px" :font-weight "700" :line-height "1.1"
                            :color (or accent "var(--color-text-primary)")}})
        (dom/text (str value)))
      (dom/div
        (dom/props {:style {:font-size "12px" :color "var(--color-text-secondary)"
                            :margin-top "2px"}})
        (dom/text label)))))

(e/defn DashboardTiles [stats]
  (e/client
    (dom/div
      (dom/props {:style {:display "flex" :gap "8px" :flex-wrap "wrap"
                          :margin "12px 0"}})
      (StatTile "Due now" (:due stats 0)
        "Questions the next Review sitting will draw, under your daily caps"
        (when (pos? (:due stats 0)) "var(--color-primary-text)"))
      (StatTile "Reviewed today" (:reviewed-today stats 0)
        (str (:new-today stats 0) " of them first-time introductions") nil)
      (StatTile "Flagged" (:flagged stats 0)
        "Questions you marked to revisit — still drawn as normal" nil)
      (StatTile "Suspended" (:suspended stats 0)
        "Questions withheld from every draw until you unsuspend them" nil))))

;; ---------------------------------------------------------------------------
;; Session report — the exam's post-grading report and the Sessions drill-down
;; ---------------------------------------------------------------------------

(e/defn SessionResult
  "Per-question record of a finished session. Verdict nil renders as ungraded —
   which covers both a grading failure and a question skipped via Suspend & Skip,
   since a skip writes no answer row yet still counts toward the frozen total.

   Each entry shows the question as ASKED and marks it when the live wording has
   since changed."
  [user-id result-sid !result-sid]
  (e/client
    (let [kg-bump (e/server (e/watch (us/get-atom user-id :kg-mutations)))
          {:keys [session answers]} (e/server (session-detail* kg-bump user-id result-sid))
          {:keys [kind total started]} session
          tally (frequencies (keep :verdict answers))
          score (session-score (get tally "correct" 0) (get tally "partial" 0) total)]
      (dom/div
        (dom/props {:style panel-style})
        (dom/div
          (dom/props {:style {:display "flex" :gap "8px" :align-items "center"}})
          (dom/button
            (dom/props {:class "btn btn-sm"})
            (dom/text "← Back")
            (dom/On "click" (fn [_] (reset! !result-sid nil)) nil))
          (dom/h2 (dom/props {:style {:font-size "18px" :margin "0"}})
            (dom/text (str (if (= kind "exam") "Exam" "Quiz") " · " started)))
          (dom/span (dom/props {:style {:font-size "16px" :font-weight "700" :margin-left "auto"}})
            (dom/text (str (or score "—") "%"))))
        (dom/p (dom/props {:style {:font-size "13px" :color "var(--color-text-secondary)"}})
          (dom/text (str (get tally "correct" 0) " correct, "
                      (get tally "partial" 0) " partial, "
                      (get tally "incorrect" 0) " incorrect"
                      (let [ungraded (- total (count (keep :verdict answers)))]
                        (when (pos? ungraded) (str ", " ungraded " unanswered/ungraded")))
                      " — of " total)))
        (e/for [{:keys [position question question_text reference_answer user_answer
                        verdict explanation]}
                (e/diff-by :position answers)]
          (let [[label color] (get fb/verdict-badge verdict ["— Ungraded" "var(--color-text-secondary)"])]
            (dom/div
              (dom/props {:style {:border "1px solid var(--color-border)" :border-radius "8px"
                                  :padding "10px 14px" :margin "10px 0"}})
              (dom/div
                (dom/props {:style {:display "flex" :gap "8px" :align-items "baseline"}})
                (dom/span (dom/props {:style {:font-weight "700" :color color :font-size "13px"
                                              :flex-shrink "0"}})
                  (dom/text label))
                (dom/span (dom/props {:style {:font-size "14px" :font-weight "600"}})
                  ;; Live wording — this is a detail view, reached by clicking the
                  ;; session row, so it shows the question as it is now.
                  (dom/text (str (inc position) ". " question))))
              (AnsweredAs question_text question)
              (dom/p (dom/props {:style {:font-size "13px" :margin "6px 0 0"}})
                (dom/text (str "Your answer: " user_answer)))
              (when explanation
                (dom/p (dom/props {:style {:font-size "13px" :margin "4px 0 0"
                                           :color "var(--color-text-secondary)"}})
                  (dom/text explanation)))
              (dom/p (dom/props {:style {:font-size "13px" :margin "4px 0 0"
                                         :color "var(--color-text-secondary)"}})
                (dom/text (str "Reference: " reference_answer))))))))))

;; ---------------------------------------------------------------------------
;; Review detail — the answer you gave for one graded review
;; ---------------------------------------------------------------------------

(e/defn ReviewDetail
  "One review answer: what you wrote, how it was graded, which facts you missed.

   This is a view of YOUR ANSWER, not of the question's source material — the
   question is shown as asked, with the live wording surfaced only when it differs,
   because the row is traversed by question id.
   Pre:  review-id is owned by user-id (else renders a not-found line)."
  [user-id review-id !review-id navigate-source!]
  (e/client
    (let [kg-bump (e/server (e/watch (us/get-atom user-id :kg-mutations)))
          detail (e/server (review-detail* kg-bump user-id review-id))
          {:keys [review missed-facts]} detail
          {:keys [verdict user_answer explanation question_text live_question
                  reference_answer reviewed_at flagged suspended question_id]} review
          !entity-card (atom nil) entity-card (e/watch !entity-card)
          !editing (atom nil)
          [label color] (get fb/verdict-badge verdict ["— Ungraded" "var(--color-text-secondary)"])]
      (dom/div
        (dom/props {:style panel-style})
        (dom/div
          (dom/props {:style {:display "flex" :gap "8px" :align-items "center"}})
          (dom/button
            (dom/props {:class "btn btn-sm"})
            (dom/text "← Back")
            (dom/On "click" (fn [_] (reset! !review-id nil)) nil))
          (dom/h2 (dom/props {:style {:font-size "18px" :margin "0"}})
            (dom/text (str "Review · " reviewed_at)))
          (dom/span (dom/props {:style {:font-weight "700" :color color :font-size "14px"
                                        :margin-left "auto"}})
            (dom/text label)))
        (if (nil? review)
          (dom/p (dom/props {:style {:color "var(--color-text-secondary)"}})
            (dom/text "That review is no longer available."))
          (dom/div
            ;; Live wording: the row is traversed by question id, so clicking through
            ;; shows what the question says now. The recorded wording follows below
            ;; only when the two diverge.
            (dom/h3 (dom/props {:style {:font-size "16px" :margin "14px 0 0"}})
              (dom/text (str live_question)))
            (AnsweredAs question_text live_question)
            ;; Curation from history: this is where a bad question found in review
            ;; gets flagged, suspended or fixed. No advance! — nothing to skip here.
            (dom/div
              (dom/props {:style {:display "flex" :gap "6px" :margin "10px 0"}})
              (curate/FlagToggle user-id question_id flagged)
              (curate/SuspendToggle user-id question_id suspended)
              (curate/EditQuestionButton !editing
                {:id question_id :question live_question
                 :reference_answer reference_answer}))
            (fb/QuizFeedback {:verdict verdict
                              :explanation explanation
                              :reference-answer reference_answer
                              :matched-keywords []
                              :missed-facts missed-facts}
              (str user_answer) [] !entity-card navigate-source!)
            (curate/QuestionEditorModal user-id !editing)
            (fb/EntityCardPopover user-id !entity-card entity-card)))))))

;; ---------------------------------------------------------------------------
;; History lists
;; ---------------------------------------------------------------------------

(e/defn ReviewLogList
  "Every graded review, newest first. The question column is the wording as asked."
  [user-id !review-id]
  (e/client
    (let [kg-bump (e/server (e/watch (us/get-atom user-id :kg-mutations)))
          rows (e/server (review-log* kg-bump user-id))
          row-count (e/server (count rows))]
      (if (zero? row-count)
        (dom/p (dom/props {:style {:padding "24px 16px" :text-align "center"
                                   :color "var(--color-text-secondary)"}})
          (dom/text "No reviews yet. Answers you grade in Review appear here."))
        ;; Constant reset-key on purpose: the tab `if` unmounts this list when you
        ;; switch away, so switching already lands at top, while a constant key
        ;; suppresses the record-count fallback that would yank a mid-scroll reader
        ;; to the top the moment a new review lands.
        (let [[offset limit] (Scroll-window row-height row-count dom/node
                               {:overquery-factor 2 :reset-key [:review-log]})]
          (dom/props {:class "tape-scroll"
                      :style {:--count row-count
                              :--grid-cols "1fr 90px 130px"
                              :--row-height (str row-height "px")}})
          (dom/table
            (dom/props {:style {:width "100%"}})
            (e/for [i (Tape offset limit)]
              (when-let [{:keys [id question_text verdict reviewed_at]}
                         (e/server (nth rows i nil))]
                (let [[label color] (get fb/verdict-badge verdict
                                      ["—" "var(--color-text-secondary)"])]
                  (dom/tr
                    (dom/props {:class (when (even? i) "row-alt")
                                :style {:--order i :height (str row-height "px")
                                        :cursor "pointer"}})
                    (dom/On "click" (fn [_] (reset! !review-id id)) nil)
                    (dom/td
                      (dom/props {:style {:display "flex" :align-items "center"
                                          :padding "0 12px" :overflow "hidden"
                                          :border-bottom "1px solid var(--color-bg-subtle)"}})
                      (dom/span
                        (dom/props {:style {:font-size "13px" :white-space "nowrap"
                                            :overflow "hidden" :text-overflow "ellipsis"}})
                        (tooltip/Tooltip! (str question_text))
                        (dom/text (or (not-empty (str question_text)) "(question not recorded)"))))
                    (dom/td
                      (dom/props {:style {:display "flex" :align-items "center"
                                          :font-size "13px" :font-weight "600" :color color
                                          :border-bottom "1px solid var(--color-bg-subtle)"}})
                      (dom/text label))
                    (dom/td
                      (dom/props {:style {:display "flex" :align-items "center"
                                          :font-size "13px"
                                          :color "var(--color-text-secondary)"
                                          :border-bottom "1px solid var(--color-bg-subtle)"}})
                      (dom/text (str reviewed_at)))))))))))))

(e/defn SessionLogList
  "Finished quiz/exam sittings, newest first. Capped at the query's 100 rows."
  [user-id !result-sid]
  (e/client
    (let [sessions (e/server (history-sessions* user-id))
          row-count (e/server (count sessions))]
      (if (zero? row-count)
        (dom/p (dom/props {:style {:padding "24px 16px" :text-align "center"
                                   :color "var(--color-text-secondary)"}})
          (dom/text "No finished sessions yet."))
        ;; Constant reset-key — see ReviewLogList.
        (let [[offset limit] (Scroll-window row-height row-count dom/node
                               {:overquery-factor 2 :reset-key [:session-log]})]
          (dom/props {:class "tape-scroll"
                      :style {:--count row-count
                              :--grid-cols "90px 1fr 140px 70px"
                              :--row-height (str row-height "px")}})
          (dom/table
            (dom/props {:style {:width "100%"}})
            (e/for [i (Tape offset limit)]
              (when-let [{:keys [id kind total started correct partial incorrect]}
                         (e/server (nth sessions i nil))]
                (dom/tr
                  (dom/props {:class (when (even? i) "row-alt")
                              :style {:--order i :height (str row-height "px")
                                      :cursor "pointer"}})
                  (dom/On "click" (fn [_] (reset! !result-sid id)) nil)
                  (dom/td
                    (dom/props {:style {:display "flex" :align-items "center" :padding "0 12px"
                                        :border-bottom "1px solid var(--color-bg-subtle)"}})
                    (dom/span
                      (dom/props {:class "type-badge"
                                  :style {:background (if (= kind "exam")
                                                        "var(--color-badge-epub)"
                                                        "var(--color-badge-pdf)")}})
                      (dom/text kind)))
                  (dom/td
                    (dom/props {:style {:display "flex" :align-items "center" :font-size "13px"
                                        :border-bottom "1px solid var(--color-bg-subtle)"}})
                    (dom/text (str started)))
                  (dom/td
                    (dom/props {:style {:display "flex" :align-items "center" :font-size "13px"
                                        :color "var(--color-text-secondary)"
                                        :border-bottom "1px solid var(--color-bg-subtle)"}})
                    (dom/text (str (+ correct partial incorrect) " / " total " answered")))
                  (dom/td
                    (dom/props {:style {:display "flex" :align-items "center"
                                        :justify-content "flex-end" :padding-right "12px"
                                        :font-size "14px" :font-weight "600"
                                        :border-bottom "1px solid var(--color-bg-subtle)"}})
                    (dom/text (str (or (session-score correct partial total) "—") "%"))))))))))))

;; ---------------------------------------------------------------------------
;; The landing surface
;; ---------------------------------------------------------------------------

(e/defn QuizDashboard
  "Quiz tab landing: tiles, Start Review, and the two history tabs.

   `start-review!` / `open-custom!` are plain client fns supplied by QuizPage — the
   dashboard owns no flow state, so it can be the default view without knowing how
   sessions are run.
   Post: exactly one of {tiles+list, review detail, session report} is on screen."
  [user-id start-review! open-custom! navigate-source!]
  (e/client
    (let [!tab (atom :reviews) tab (e/watch !tab)
          !review-id (atom nil) review-id (e/watch !review-id)
          !result-sid (atom nil) result-sid (e/watch !result-sid)]
      (cond
        (some? review-id) (ReviewDetail user-id review-id !review-id navigate-source!)
        (some? result-sid) (SessionResult user-id result-sid !result-sid)
        :else
        (let [kg-bump (e/server (e/watch (us/get-atom user-id :kg-mutations)))
              stats (e/server (dashboard-stats* kg-bump user-id))
              due (e/server (:due stats 0))]
          (dom/div
            (dom/props {:style (assoc panel-style :display "flex"
                                 :flex-direction "column"
                                 :height "calc(100vh - 130px)")})
            (dom/div
              (dom/props {:style {:display "flex" :justify-content "space-between"
                                  :align-items "center" :flex-shrink "0"}})
              (dom/h2 (dom/props {:style {:font-size "18px" :margin "0"}}) (dom/text "Quiz"))
              (dom/div
                (dom/props {:style {:display "flex" :gap "6px"}})
                (dom/button (dom/props {:class "btn btn-sm"})
                  (dom/text "Custom quiz")
                  (dom/On "click" (fn [_] (open-custom!)) nil))
                (dom/button
                  (dom/props {:class "btn btn-primary btn-sm"})
                  (dom/text (if (pos? due) (str "Start Review · " due) "Start Review"))
                  (dom/On "click" (fn [_] (start-review!)) nil))))
            (dom/div (dom/props {:style {:flex-shrink "0"}})
              (DashboardTiles stats))
            (dom/div
              (dom/props {:style {:display "flex" :gap "4px" :flex-shrink "0"
                                  :border-bottom "1px solid var(--color-border)"}})
              (e/for [[k label] (e/diff-by first [[:reviews "Reviews"] [:sessions "Sessions"]])]
                (dom/button
                  (dom/props {:class (str "btn btn-sm" (when (= tab k) " btn-secondary"))
                              :role "tab" :aria-selected (if (= tab k) "true" "false")
                              :style {:border-radius "var(--radius-sm) var(--radius-sm) 0 0"}})
                  (dom/text label)
                  (dom/On "click" (fn [_] (reset! !tab k)) nil))))
            (dom/div
              (dom/props {:style {:flex "1" :min-height "0" :overflow-y "auto"}})
              (if (= tab :reviews)
                (ReviewLogList user-id !review-id)
                (SessionLogList user-id !result-sid)))))))))

(e/defn ReviewHistoryButton
  "Opens the dashboard from inside a Review sitting."
  [!view]
  (e/client
    (dom/button (dom/props {:class "btn btn-sm" :aria-label "Quiz dashboard"})
      (icons/Icon :history :size 14) (dom/text " History")
      (dom/On "click" (fn [_] (reset! !view :dashboard)) nil))))
