(ns freememo.quiz-dashboard
  "The Quiz tab's landing surface (plans/quiz-iteration.md §8).

   Tiles over two virtual-scrolled history tabs:
     Reviews — one row per graded review answer (the FSRS flow keeps no session,
               so kg_reviews IS its answer history)
     History — one row per finished quiz/exam sitting AND one per Review day

   A Review sitting is persisted nowhere, so the History tab's unit for it is the
   calendar day of kg_reviews — a real boundary rather than a guessed one, and the
   unit Anki reports in too. The two row shapes share one list and dispatch on
   :kind; see db/list-quiz-history.

   Both lists render the question wording SNAPSHOT taken when the answer was
   recorded, so a later edit cannot rewrite the record; both detail views read the
   question LIVE by id and mark the divergence. Replaces the old ReviewHistory
   (two unwindowed tables) and QuizHistoryModal (a separate overlay).

   Both lists render through freememo.tape-table, so the Quiz history is uniform
   with the Knowledge and Library tables: sibling header, ten-row container cap,
   fifteen mounted rows whatever the viewport, and an empty state that overlays the
   table rather than unmounting it."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.icons :as icons]
   [freememo.question-curation :as curate]
   [freememo.quiz-heatmap :as heatmap]
   [freememo.quiz-card-turn :as card-turn]
   [freememo.quiz-feedback :as fb]
   [freememo.quiz-item-stats :as stats]
   [freememo.tape-table :as tape]
   [freememo.tooltip :as tooltip]
   #?(:clj [freememo.db :as db])
   #?(:clj [freememo.settings :as settings])
   #?(:clj [freememo.user-state :as us])))

(def ^:private panel-style
  {:max-width "760px" :margin "0 auto" :padding "16px"})

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

      No today's-count here: the footer's first line reports it from the SAME daily
      query that draws the heatmap, so a second read would be one more round trip
      for a number already on the page — see quiz-heatmap/footer-stats*.
      Post: {:due n :live n :flagged n :suspended n}."
     [_kg-bump user-id]
     (let [{:keys [new-per-day review-per-day]} (settings/fsrs-config user-id)]
       (merge (db/kg-question-bank-counts user-id)
         {:due (db/review-due-count user-id new-per-day review-per-day)}))))

#?(:clj
   (defn review-log*
     "The Reviews list. `day` nil is the whole log; an ISO date scopes it to one
      Review day, which is what the History list's review rows open."
     [_kg-bump user-id day]
     (db/fsrs-review-log user-id review-log-limit day)))

#?(:clj
   (defn review-detail* [_kg-bump user-id review-id]
     (db/kg-review-detail user-id review-id)))

#?(:clj
   (defn quiz-history*
     "Sittings and Review days, interleaved. Takes the KG bump, which the
      sessions-only query it replaces did not need: a finished sitting cannot
      change, but a review landing during this sitting moves a day row's counts."
     [_kg-bump user-id]
     (db/list-quiz-history user-id)))

#?(:clj
   (defn review-day-summary* [_kg-bump user-id day]
     (db/kg-review-day-summary user-id day)))

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

(defn retention
  "Share of a Review day's reviews NOT rated Again, as a rounded percentage.

   `rating = 1` is the lapse, so this is FSRS's own definition of a successful
   review rather than a re-derived one — the dashboard and the scheduler cannot
   disagree about the same day. It shares the History list's Result column with
   session-score, which measures something else on the same 0-100 scale; the row's
   :kind is what says which.
   Post: nil for a day with no reviews — the same 'no score' signal session-score
         gives for an empty paper."
  [reviewed again]
  (when (pos? (or reviewed 0))
    (int (+ 0.5 (* 100.0 (/ (- reviewed (or again 0)) reviewed))))))

(defn fmt-duration
  "Milliseconds as a reading duration.
   Post: \"—\" for nil (NOT RECORDED, which is not zero); seconds under a minute,
         else minutes with one decimal."
  [ms]
  (if (nil? ms)
    "—"
    (let [secs (/ (double ms) 1000.0)]
      (if (< secs 60)
        (str (int (+ 0.5 secs)) "s")
        (str (/ (double (int (+ 0.5 (* 10 (/ secs 60.0))))) 10.0) " min")))))

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
        ;; "You answered: " shares this paragraph's bidi run, so dir="auto" resolves
        ;; from the label and an RTL snapshot stays LTR — recorded limitation
        ;; (plans/quiz-rtl-support.md §5).
        (dom/p
          (dom/props {:dir "auto"
                      :style {:font-size "13px" :margin "2px 0 0"
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
      ;; No "Reviewed today" tile: the footer's first line says the same number plus
      ;; the minutes and the per-item average, so the tile was the same fact twice.
      ;; dashboard-stats* no longer computes it either — nothing else read it, and
      ;; the db/fsrs-daily-counts call that produced it went with the tile.
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
          {:keys [session answers]} (e/server (e/Offload #(session-detail* kg-bump user-id result-sid)))
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
            (dom/text (str (if (= kind "exam") "Timed Quiz" "Quiz") " · " started)))
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
                ;; dir="auto" resolves from the question here, not from the "N. "
                ;; prefix: digits are directionally weak.
                (dom/span (dom/props {:dir "auto"
                                      :style {:font-size "14px" :font-weight "600"}})
                  ;; Live wording — this is a detail view, reached by clicking the
                  ;; session row, so it shows the question as it is now.
                  (dom/text (str (inc position) ". " question))))
              (AnsweredAs question_text question)
              ;; This line and the Reference line below carry an English label in
              ;; the same bidi run, so RTL content stays LTR — see AnsweredAs.
              (dom/p (dom/props {:dir "auto" :style {:font-size "13px" :margin "6px 0 0"}})
                (dom/text (str "Your answer: " user_answer)))
              (when explanation
                (dom/p (dom/props {:dir "auto"
                                   :style {:font-size "13px" :margin "4px 0 0"
                                           :color "var(--color-text-secondary)"}})
                  (dom/text explanation)))
              (dom/p (dom/props {:dir "auto"
                                 :style {:font-size "13px" :margin "4px 0 0"
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
          detail (e/server (e/Offload #(review-detail* kg-bump user-id review-id)))
          {:keys [review missed-facts]} detail
          {:keys [verdict rating user_answer explanation question_text live_question
                  reference_answer reviewed_at flagged suspended question_id
                  flashcard_id ord]} review
          !entity-card (atom nil) entity-card (e/watch !entity-card)
          !editing (atom nil)
          !stats (atom nil)
          ;; The stats opener sits in the HEADER, not in the curation bar: a card
          ;; review takes the CardReviewBody branch and has no bar, and both arms
          ;; have the same lifetime record to show.
          stats-target (if flashcard_id
                         {:card flashcard_id :ord ord}
                         {:question question_id})
          [label color] (fb/grade-badge verdict rating)]
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
            (dom/text label))
          (when review (stats/StatsOpenerButton !stats stats-target)))
        (cond
          (nil? review)
          (dom/p (dom/props {:style {:color "var(--color-text-secondary)"}})
            (dom/text "That review is no longer available."))

          ;; A card review has no question id, so none of the question-shaped
          ;; presentation below applies — see quiz-card-turn/CardReviewBody.
          (some? flashcard_id) (card-turn/CardReviewBody review)

          :else
          (dom/div
            ;; Live wording: the row is traversed by question id, so clicking through
            ;; shows what the question says now. The recorded wording follows below
            ;; only when the two diverge.
            (dom/h3 (dom/props {:dir "auto" :style {:font-size "16px" :margin "14px 0 0"}})
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
            (fb/EntityCardPopover user-id !entity-card entity-card))))
      ;; Sibling of the panel div and the LAST form of the body: a modal nested
      ;; inside another overlay inherits pointer-events:none and shares its Escape
      ;; (CLAUDE.md). Load-bearing TODAY, not prospectively — QuestionEditorModal
      ;; and EntityCardPopover are both overlays and both mount inside the cond
      ;; branch above, so nesting this there would make it mouse-dead, and being
      ;; last is what paints it over them.
      (stats/QuizItemStatsModal user-id !stats))))

;; ---------------------------------------------------------------------------
;; History lists
;; ---------------------------------------------------------------------------

(e/defn ReviewRow
  "One graded review. The question column is the wording AS ASKED, so this row never
   reads the live item — a later edit must not rewrite the record."
  [row i open!]
  (e/client
    (let [{:keys [id prompt verdict rating reviewed_at]} row
          ;; A self-rated card row has no verdict, so it shows its rating — never an
          ;; empty badge.
          [label color] (fb/grade-badge verdict rating)]
      (dom/tr
        (dom/props {:class (when (even? i) "row-alt") :style {:--order i}})
        (dom/On "click" (fn [_] (open! id)) nil)
        (dom/td
          (dom/props {:class "tape-cell-click"})
          ;; dir="auto" on the text span only, NOT on the table: the .tape-scroll
          ;; grid is shared by a dozen sites, and one RTL row must not flip every
          ;; other row's columns (plans/quiz-rtl-support.md D5).
          (dom/span
            (dom/props {:dir "auto" :class "tape-cell-text"})
            (tooltip/Tooltip! (str prompt))
            (dom/text (or (not-empty (str prompt)) "(wording not recorded)"))))
        ;; The colour is data-dependent and stays inline; the weight is not and
        ;; lives in .tape-cell-strong.
        (dom/td (dom/props {:class "tape-cell-click tape-cell-strong"
                            :style {:color color}})
          (dom/text label))
        (dom/td (dom/props {:class "tape-cell-click tape-cell-muted"})
          (dom/text (str reviewed_at)))))))

(e/defn ReviewLogList
  "Graded reviews, newest first — the Reviews tab, and the body of a Review day.

   `day` nil is the whole log; an ISO date scopes it to that day. One component for
   both, because a day's rows ARE review rows and each still clicks through to the
   same detail.
   Post: the table is mounted at every count, including 0."
  [user-id !review-id day]
  (e/client
    (let [kg-bump (e/server (e/watch (us/get-atom user-id :kg-mutations)))
          rows (e/server (e/Offload #(review-log* kg-bump user-id day)))
          row-count (e/server (count rows))
          open! (fn [id] (reset! !review-id id))]
      ;; reset-key is the day, not the row count: switching tabs or opening another
      ;; day is a genuinely different list and belongs at the top, while a new review
      ;; landing mid-scroll must not yank the reader there.
      (tape/TapeTable ["Question" "Grade" "Reviewed"] "1fr 90px 130px" row-count
        [:review-log day]
        (if day
          "No reviews recorded on this day."
          "No reviews yet. Answers you grade in Review appear here.")
        (e/fn [i]
          (when-let [row (e/server (nth rows i nil))]
            (ReviewRow row i open!)))))))

(e/defn HistoryRow
  "One history row: a finished quiz/exam sitting, or a Review day.

   The two shapes share four columns and differ in what fills the last two, so the
   row dispatches on :kind rather than the list holding two row components. Result
   is a percentage either way, but a sitting's is a score against its frozen paper
   and a day's is retention — see session-score and retention."
  [row i open-session! open-day!]
  (e/client
    (let [{:keys [kind key at_label total correct partial incorrect reviewed again]} row
          review-day? (= "review" kind)
          result (if review-day?
                   (retention reviewed again)
                   (session-score correct partial total))]
      (dom/tr
        (dom/props {:class (when (even? i) "row-alt") :style {:--order i}})
        (dom/On "click"
          (fn [_] (if review-day? (open-day! key) (open-session! (parse-long key))))
          nil)
        (dom/td (dom/props {:class "tape-cell-click"})
          (dom/span
            (dom/props {:class "type-badge"
                        :style {:background (case kind
                                              "exam" "var(--color-badge-epub)"
                                              "review" "var(--color-badge-web)"
                                              "var(--color-badge-pdf)")}})
            (dom/text kind)))
        (dom/td (dom/props {:class "tape-cell-click"}) (dom/text (str at_label)))
        (dom/td (dom/props {:class "tape-cell-click tape-cell-muted"})
          (dom/span (dom/props {:class "tape-cell-text"})
            (dom/text (if review-day?
                        (str reviewed " reviewed")
                        (str (+ correct partial incorrect) " / " total " answered")))))
        (dom/td (dom/props {:class "tape-cell-click tape-cell-num tape-cell-strong"})
          (dom/text (if result (str result "%") "—")))))))

(e/defn HistoryList
  "Finished sittings and Review days, interleaved newest-first.

   A Review sitting is persisted nowhere, so its unit here is the calendar day —
   see db/list-quiz-history. Clicking a sitting opens its per-question report;
   clicking a day opens that day's reviews.
   Post: the table is mounted at every count, including 0."
  [user-id !result-sid !day]
  (e/client
    (let [kg-bump (e/server (e/watch (us/get-atom user-id :kg-mutations)))
          rows (e/server (e/Offload #(quiz-history* kg-bump user-id)))
          row-count (e/server (count rows))
          open-session! (fn [id] (reset! !result-sid id))
          open-day! (fn [d] (reset! !day d))]
      (tape/TapeTable ["Type" "Date" "Items" "Result"] "90px 140px 1fr 80px" row-count
        [:quiz-history]
        "No history yet. Reviews and finished quizzes appear here."
        (e/fn [i]
          (when-let [row (e/server (nth rows i nil))]
            (HistoryRow row i open-session! open-day!)))))))

(e/defn ReviewDayDetail
  "One Review day: its totals, then its reviews. Rows click through to the same
   ReviewDetail the Reviews tab opens — the day scopes the list, it does not change
   what a review is.

   Time reads '—' when no row of the day carries a duration, and states its own
   denominator when only some do: duration_ms is NULL for every review taken before
   the column existed, and NULL is not zero."
  [user-id day !day !review-id]
  (e/client
    (let [kg-bump (e/server (e/watch (us/get-atom user-id :kg-mutations)))
          {:keys [reviewed new again hard good easy timed duration_ms]}
          (e/server (e/Offload #(review-day-summary* kg-bump user-id day)))]
      (dom/div
        (dom/props {:style {:display "flex" :gap "8px" :align-items "center"
                            :margin-bottom "8px"}})
        (dom/button
          (dom/props {:class "btn btn-sm"})
          (dom/text "← Back")
          (dom/On "click" (fn [_] (reset! !day nil)) nil))
        (dom/h2 (dom/props {:style {:font-size "18px" :margin "0"}})
          (dom/text (str "Review · " day)))
        (dom/span (dom/props {:style {:font-size "16px" :font-weight "700"
                                      :margin-left "auto"}})
          (dom/text (str (or (retention reviewed again) "—") "%"))))
      (dom/p
        (dom/props {:style {:font-size "13px" :margin "0 0 10px"
                            :color "var(--color-text-secondary)"}})
        (dom/text (str reviewed " reviewed"
                    (when (pos? (or new 0)) (str ", " new " new"))
                    " · " again " again, " hard " hard, " good " good, " easy " easy"
                    (cond
                      (zero? (or timed 0)) " · time not recorded"
                      (= timed reviewed) (str " · " (fmt-duration duration_ms))
                      :else (str " · " (fmt-duration duration_ms)
                              " over " timed " of " reviewed)))))
      (ReviewLogList user-id !review-id day))))

;; ---------------------------------------------------------------------------
;; The landing surface
;; ---------------------------------------------------------------------------

(e/defn QuizDashboard
  "Quiz tab landing: tiles, Start Review, and the two history tabs.

   `start-review!` / `open-custom!` are plain client fns supplied by QuizPage — the
   dashboard owns no flow state, so it can be the default view without knowing how
   sessions are run.
   Post: exactly one of {tiles+lists, review detail, session report, review day} is
         on screen.

         The review branch is FIRST and the day branch third, so a review opened
         from inside a day wins while !day stays set — and Back, which clears only
         !review-id, falls through to the day rather than to the list. Reversing
         them would keep re-rendering the day and no review could ever open."
  [user-id start-review! open-custom! navigate-source!]
  (e/client
    (let [!tab (atom :reviews) tab (e/watch !tab)
          !review-id (atom nil) review-id (e/watch !review-id)
          !result-sid (atom nil) result-sid (e/watch !result-sid)
          !day (atom nil) day (e/watch !day)]
      (cond
        (some? review-id) (ReviewDetail user-id review-id !review-id navigate-source!)
        (some? result-sid) (SessionResult user-id result-sid !result-sid)
        (some? day)
        (dom/div (dom/props {:style panel-style})
          (ReviewDayDetail user-id day !day !review-id))
        :else
        (let [kg-bump (e/server (e/watch (us/get-atom user-id :kg-mutations)))
              stats (e/server (e/Offload #(dashboard-stats* kg-bump user-id)))
              due (e/server (:due stats 0))]
          (dom/div
            ;; A normal block column, NOT a viewport-height flex box. TapeTable caps
            ;; its own scroll box at ten rows, so the page has a bounded height of
            ;; its own and the outer scroll and the table's inner scroll never
            ;; compete for one gesture. The footer below the table depends on this.
            (dom/props {:style panel-style})
            (dom/div
              (dom/props {:style {:display "flex" :justify-content "space-between"
                                  :align-items "center"}})
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
            (DashboardTiles stats)
            (dom/div
              (dom/props {:style {:display "flex" :gap "4px" :margin-bottom "8px"
                                  :border-bottom "1px solid var(--color-border)"}})
              (e/for [[k label] (e/diff-by first [[:reviews "Reviews"] [:history "History"]])]
                (dom/button
                  (dom/props {:class (str "btn btn-sm" (when (= tab k) " btn-secondary"))
                              :role "tab" :aria-selected (if (= tab k) "true" "false")
                              :style {:border-radius "var(--radius-sm) var(--radius-sm) 0 0"}})
                  (dom/text label)
                  (dom/On "click" (fn [_] (reset! !tab k)) nil))))
            ;; No scroll container of its own — TapeTable owns one and caps it.
            (if (= tab :reviews)
              (ReviewLogList user-id !review-id nil)
              (HistoryList user-id !result-sid !day))
            ;; Last, and the page scrolls to reach it: the table above is capped at
            ;; ten rows, so the outer scroll and the table's inner scroll never
            ;; compete for one gesture.
            (heatmap/QuizFooter user-id (fn [d] (reset! !day d)))))))))

(e/defn ReviewHistoryButton
  "Opens the dashboard from inside a Review sitting."
  [!view]
  (e/client
    (dom/button (dom/props {:class "btn btn-sm" :aria-label "Quiz dashboard"})
      (icons/Icon :history :size 14) (dom/text " History")
      (dom/On "click" (fn [_] (reset! !view :dashboard)) nil))))
