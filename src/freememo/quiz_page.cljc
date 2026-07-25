(ns freememo.quiz-page
  "Quiz tab: three answering flows over the same question bank.

     ReviewFlow  — the FSRS due queue; grades instantly, advances the schedule,
                   keeps no session row (kg_reviews carries the answer)
     QuizActive  — a custom quiz session; frozen draw, instant feedback
     ExamActive  — a timed sitting; answers saved ungraded, graded at submit

   Landing is freememo.quiz-dashboard, not a flow: tiles plus the review and
   session histories. Session state lives in kg_sessions/kg_answers, so a reload
   lands back in the active session at the first unanswered question; client atoms
   only steer the in-page flow.

   Every flow renders the same three curation controls (Flag, Suspend & Skip, Edit)
   from freememo.question-curation. A skip suspends the question and writes nothing:
   it is not an attempt, so it never scores and never advances a schedule."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.electric-forms5 :as forms]
   [hyperfiddle.electric-scroll0 :refer [Scroll-window Tape]]
   [clojure.string :as str]
   [taoensso.telemere :as tel]
   [freememo.command-bus :as bus]
   [freememo.navigation :as nav]
   [freememo.question-curation :as curate]
   [freememo.quiz-dashboard :as qd]
   [freememo.quiz-feedback :as fb]
   #?(:clj [freememo.db :as db])
   #?(:clj [freememo.kg-grade :as grade])
   #?(:clj [freememo.settings :as settings])
   #?(:clj [freememo.toasts :as toasts])
   #?(:clj [freememo.user-state :as us])))

;; ---------------------------------------------------------------------------
;; Server queries/effects — whole defns under #?(:clj …) so CLJS misuse warns
;; loudly instead of silently nil'ing; called only inside e/server.
;; ---------------------------------------------------------------------------

#?(:clj
   (defn quiz-docs*
     "Documents that have drawable questions. Post: [{:id :title :questions}]."
     [_kg-bump user-id]
     (let [per-doc (db/kg-questions-per-doc user-id)]
       (into []
         (keep (fn [t]
                 (let [id (:topics/id t)
                       n (get per-doc id 0)]
                   (when (pos? n)
                     {:id id :title (:topics/title t) :questions n}))))
         (db/get-root-topics user-id)))))

#?(:clj
   (defn active-session*
     "The session to resume, exam first — a running exam clock outranks an
      idle quiz."
     [user-id]
     (or (db/get-active-kg-session user-id "exam")
         (db/get-active-kg-session user-id "quiz"))))

#?(:clj
   (defn quiz-question*
     "The current turn's question. Takes the KG bump so an in-quiz edit or flag
      toggle re-renders the question and its control states immediately — without
      it the displayed wording would stay stale until the frame remounted."
     [_kg-bump user-id question-id]
     (db/get-kg-question-for-session user-id question-id)))

#?(:clj
   (defn start-session!*
     "Draw + freeze a session (quiz: nil time limit; exam: seconds). Post:
      session map (get-active shape) or nil when the scope yields no
      questions (toasted here, UI stays dumb)."
     [user-id kind scope-ids n time-limit-seconds]
     (let [qids (db/draw-kg-questions user-id scope-ids n)]
       (if (empty? qids)
         (do (toasts/push! user-id
               {:level :error
                :message "No questions cover the selected documents — generate some first."})
           nil)
         (let [sid (db/create-kg-session! user-id kind scope-ids qids time-limit-seconds)]
           {:id sid :kind kind :question-ids qids :answered 0
            :time-limit-seconds time-limit-seconds :elapsed-seconds 0})))))

#?(:clj
   (defn save-exam-answer!*
     "Persist one exam answer without grading (exam grading runs at submit)."
     [user-id session-id question-id position answer]
     (db/record-kg-answer! user-id session-id question-id position answer)
     :saved))

#?(:clj
   (defn finish-quiz!*
     "Close the session; return its verdict tally for the summary."
     [user-id session-id]
     (db/finish-kg-session! user-id session-id)
     (db/kg-session-verdict-counts user-id session-id)))

;; --- FSRS Review (default quiz) --------------------------------------------

#?(:clj
   (defn review-queue*
     "Ordered question ids due today, capped per the user's FSRS settings.
      Post: vector of ids (learning → review → new)."
     [_kg-bump user-id]
     (let [{:keys [new-per-day review-per-day]} (settings/fsrs-config user-id)]
       (db/draw-fsrs-due-queue user-id new-per-day review-per-day))))

#?(:clj
   (defn review-answer!*
     "Grade one Review answer and advance its FSRS schedule (no session row).
      Post: {:result <grade map> :schedule {:due-today? …}|nil}."
     [user-id question-id answer]
     (grade/review-question! user-id question-id answer)))

;; ---------------------------------------------------------------------------
;; Views
;; ---------------------------------------------------------------------------

(def ^:private panel-style
  {:max-width "720px" :margin "0 auto" :padding "16px"})

;; Fixed row height for the virtualized material (document) list — Scroll-window
;; needs it up front and the .tape-scroll CSS consumes it as --row-height.
(def ^:private quiz-material-row-height 36)

(e/defn QuizSetup [user-id !session initial-mode !view]
  (e/client
    (let [kg-bump (e/server (e/watch (us/get-atom user-id :kg-mutations)))
          docs (e/server (quiz-docs* kg-bump user-id))
          !scope (atom #{}) scope (e/watch !scope)
          !mode (atom (or initial-mode "quiz")) mode (e/watch !mode)
          !count-str (atom (if (= initial-mode "exam") "20" "10"))
          count-str (e/watch !count-str)
          !limit-str (atom "30") limit-str (e/watch !limit-str)]
      (dom/div
        (dom/props {:style panel-style})
        (dom/div
          (dom/props {:style {:display "flex" :justify-content "space-between"
                              :align-items "center"}})
          (dom/div
            (dom/props {:style {:display "flex" :gap "8px" :align-items "center"}})
            (dom/button
              (dom/props {:class "btn btn-sm" :aria-label "Back to the quiz dashboard"})
              (dom/text "← Dashboard")
              (dom/On "click" (fn [_] (reset! !view :dashboard)) nil))
            (dom/h2 (dom/props {:style {:font-size "18px" :margin "0"}})
              (dom/text (if (= mode "exam") "Start an exam" "Custom quiz")))))
        ;; Mode toggle — quiz: untimed, instant feedback; exam: timed, graded at end.
        (dom/div
          (dom/props {:style {:display "flex" :gap "4px" :margin "10px 0"}})
          (e/for [[k label dflt] (e/diff-by first [["quiz" "Quiz" "10"] ["exam" "Exam" "20"]])]
            (dom/button
              (dom/props {:class (str "btn btn-sm" (when (= mode k) " btn-secondary"))
                          :style {:font-weight (if (= mode k) "600" "400")}
                          :aria-pressed (if (= mode k) "true" "false")})
              (dom/text label)
              (dom/On "click" (fn [_] (reset! !mode k) (reset! !count-str dflt)) nil))))
        (if (e/server (empty? docs))
          (dom/p (dom/props {:style {:color "var(--color-text-secondary)"}})
            (dom/text "No questions yet — distill a document and generate questions in Knowledge."))
          (dom/div
            ;; Virtualized 1-col material list (Scroll-window/Tape; shared by quiz+exam
            ;; modes). Bounded height gives the virtual viewport its bounds; reuses the
            ;; .tape-scroll table CSS (per-row transform via --order; --count table height).
            (dom/div
              (dom/props {:style {:max-height "40vh" :overflow-y "auto" :min-height "0"
                                  :border "1px solid var(--color-border)"
                                  :border-radius "var(--radius-sm)"}})
              (let [row-count (e/server (count docs))
                    [offset limit] (Scroll-window quiz-material-row-height row-count dom/node
                                     {:overquery-factor 2})]
                (dom/props {:class "tape-scroll"
                            :style {:--count row-count
                                    :--grid-cols "1fr"
                                    :--row-height (str quiz-material-row-height "px")}})
                (dom/table
                  (dom/props {:style {:width "100%"}})
                  (e/for [i (Tape offset limit)]
                    (when-let [{:keys [id title questions]} (e/server (nth docs i nil))]
                      (dom/tr
                        (dom/props {:class (when (even? i) "row-alt")
                                    :style {:--order i
                                            :height (str quiz-material-row-height "px")}})
                        (dom/td
                          (dom/props {:style {:display "flex" :align-items "center" :gap "8px"
                                              :padding "0 4px"}})
                          (dom/label
                            (dom/props {:style {:display "flex" :align-items "center" :gap "8px"
                                                :cursor "pointer" :width "100%"}})
                            ;; Local-only (never hits the server — !scope only feeds the
                            ;; "Start quiz" command below) — still Forms5 Checkbox! for
                            ;; consistency with anki_sync_form.cljc's local-checkbox idiom.
                            ;; e/amb (not do) — Checkbox! is a tracked control sharing this
                            ;; <label> with plain text siblings.
                            (e/amb
                              (e/for [[t edit] (forms/Checkbox! id (contains? scope id))]
                                (swap! !scope (fn [s] (if (get edit id) (conj s id) (disj s id))))
                                (t))
                              (do (dom/span (dom/text title))
                                (dom/span (dom/props {:style {:color "var(--color-text-secondary)"
                                                              :font-size "12px" :margin-left "auto"}})
                                  (dom/text (str questions " questions")))
                                (e/amb)))))))))))
            (dom/div
              (dom/props {:style {:display "flex" :gap "8px" :align-items "center" :margin-top "12px"}})
              (dom/label (dom/props {:style {:font-size "13px"}}) (dom/text "Questions:"))
              ;; Local-only (!count-str only feeds the "Start quiz" command below)
              ;; — Input! + inline mirror-ack, no server round-trip per keystroke.
              (e/for [[t {v :count}] (forms/Input! :count count-str :type "number" :min "1" :max "50"
                                        :class "form-input" :style {:width "70px"}
                                        :aria-label "Question count")]
                (reset! !count-str v)
                (t))
              (when (= mode "exam")
                (dom/label (dom/props {:style {:font-size "13px"}}) (dom/text "Minutes:"))
                (e/for [[t {v :limit}] (forms/Input! :limit limit-str :type "number" :min "1" :max "180"
                                          :class "form-input" :style {:width "70px"}
                                          :aria-label "Time limit in minutes")]
                  (reset! !limit-str v)
                  (t)))
              (dom/button
                (dom/props {:class "btn btn-primary" :disabled (empty? scope)})
                (dom/text (if (= mode "exam") "Start exam" "Start quiz"))
                (let [click (dom/On "click"
                              (fn [_]
                                (let [kind @!mode
                                      n (or (parse-long (str @!count-str))
                                            (if (= kind "exam") 20 10))]
                                  {:id (str (random-uuid))
                                   :scope (vec @!scope)
                                   :kind kind
                                   :n (max 1 (min 50 n))
                                   :limit-s (when (= kind "exam")
                                              (* 60 (max 1 (min 180 (or (parse-long (str @!limit-str)) 30)))))}))
                              nil)
                      [t _] (e/Token click)]
                  (dom/props {:disabled (or (empty? scope) (some? t))})
                  (when t
                    ;; case must wait on the RESULT — a `do` is concurrent and
                    ;; would yield its constant immediately, spending the token
                    ;; and unmounting (cancelling) the Offload mid-flight.
                    (let [result (e/server
                                   (e/Offload
                                     #(start-session!* user-id (:kind click) (:scope click)
                                        (:n click) (:limit-s click))))]
                      (case result
                        nil (t) ; empty draw — server already toasted
                        (do (reset! !session result)
                          (t))))))))))))))

(e/defn QuizActive [user-id !session session !summary !editing navigate-source!]
  (e/client
    (let [{sid :id qids :question-ids answered :answered} session
          total (count qids)
          !idx (atom answered) idx (e/watch !idx)
          !draft (atom "") draft (e/watch !draft) ; textarea mirror
          !grading? (atom false) grading? (e/watch !grading?)
          !feedback (atom nil) feedback (e/watch !feedback)
          !entity-card (atom nil) entity-card (e/watch !entity-card)
          ;; One latch for every path that ends the sitting: End quiz, Finish on the
          ;; last question, and Suspend & Skip on the last question. Without it each
          ;; path would carry its own copy of the finish call.
          !finish-req (atom nil) finish-req (e/watch !finish-req)
          finish! (fn [] (reset! !finish-req {:id (str (random-uuid))}))
          kg-bump (e/server (e/watch (us/get-atom user-id :kg-mutations)))
          qid (nth qids (min idx (dec total)) nil)
          qdata (e/server (quiz-question* kg-bump user-id qid))
          ;; Skipping the final question ends the sitting — there is nothing to
          ;; advance to, and the skip is not an answer, so the tally is unaffected.
          advance! (fn []
                     (if (>= (inc @!idx) total)
                       (finish!)
                       (do (reset! !feedback nil) (reset! !draft "")
                         (swap! !idx inc))))]
      (dom/div
        (dom/props {:style panel-style})
        (if (some? finish-req)
          (do
            (dom/h3 (dom/props {:style {:font-size "16px"}}) (dom/text "Finishing…"))
            (let [counts (e/server (e/Offload #(finish-quiz!* user-id sid)))]
              (case counts ; wait on the value — do would race the transition
                (reset! !summary {:counts counts :total total}))))
          (dom/div
            (dom/div
              (dom/props {:style {:display "flex" :justify-content "space-between"
                                  :align-items "center"}})
              (dom/span (dom/props {:style {:font-size "13px" :color "var(--color-text-secondary)"}})
                (dom/text (str "Question " (inc idx) " / " total)))
              (dom/button
                (dom/props {:class "btn btn-sm" :aria-label "End quiz"})
                (dom/text "End quiz")
                (dom/On "click" (fn [_] (finish!)) nil)))
            (dom/h3 (dom/props {:style {:font-size "16px" :margin "14px 0"}})
              (dom/text (str (:question qdata))))
            ;; One advance for both states: it clears the feedback pane and steps the
            ;; index, or finishes on the last question, so suspending after grading
            ;; moves on exactly as Next would.
            (curate/CurationBar user-id qdata advance! !editing)
            (if (nil? feedback)
              (dom/div
                ;; Forms5: Input! (tracked textarea) + Button! (tracked submit),
                ;; combined via e/amb — a `do` here would swallow whichever token
                ;; isn't last and hang that control. One inline service below:
                ;; field edits mirror into !draft and ack immediately (no server
                ;; round-trip per keystroke); the submit command grades on the server.
                (e/for [[t edit] (e/amb
                                   (forms/Input! :quiz/answer draft :as :textarea
                                     :class "form-input" :rows 4
                                     :placeholder "Answer in your own words…"
                                     :aria-label "Your answer"
                                     :style {:width "100%" :resize "vertical"})
                                   (forms/Button! [::submit]
                                     :class "btn btn-primary" :style {:margin-top "8px"}
                                     :label (if grading? "Grading…" "Submit")
                                     ;; Blank is unsubmittable: Suspend & Skip is the
                                     ;; way past a question you cannot answer.
                                     :disabled (or grading? (str/blank? (str draft)))))]
                  (if (map? edit)
                    (do (reset! !draft (:quiz/answer edit)) (t))
                    (let [answer (str/trim (str draft))]
                      (if (str/blank? answer)
                        (t) ; defence in depth — the button is disabled for this
                        (do (reset! !grading? true)
                          (let [position idx
                                result (e/server
                                         (e/Offload
                                           #(grade/grade-answer! user-id sid qid position answer)))]
                            (case result ; wait on the value — do would race the token
                              (do (reset! !grading? false)
                                (when (:success result)
                                  (reset! !feedback {:result result :answer answer}))
                                (t))))))))))
              (dom/div
                (fb/QuizFeedback (:result feedback) (:answer feedback)
                  (:entities qdata) !entity-card navigate-source!)
                (dom/button
                  (dom/props {:class "btn btn-primary" :style {:margin-top "12px"}})
                  (dom/text (if (< (inc idx) total) "Next question" "Finish"))
                  (if (< (inc idx) total)
                    (dom/On "click" (fn [_]
                                      (reset! !feedback nil)
                                      (reset! !draft "")
                                      (swap! !idx inc))
                      nil)
                    (dom/On "click" (fn [_] (finish!)) nil)))))))
        ;; Sibling of the answer/feedback `if`, inside the panel div — NOT
        ;; nested in any branch: it must mount for the whole session.
        (fb/EntityCardPopover user-id !entity-card entity-card)))))

(e/defn QuizSummary [!session !summary summary]
  (e/client
    (let [{:keys [counts total]} summary
          correct (get counts "correct" 0)
          partial (get counts "partial" 0)
          incorrect (get counts "incorrect" 0)
          graded (+ correct partial incorrect)
          score (when (pos? graded) (* 100.0 (/ (+ correct (* 0.5 partial)) graded)))]
      (dom/div
        (dom/props {:style panel-style})
        (dom/h2 (dom/props {:style {:font-size "18px"}}) (dom/text "Quiz finished"))
        (dom/p (dom/props {:style {:font-size "14px"}})
          (dom/text (str "Answered " graded " of " total " — "
                      correct " correct, " partial " partial, " incorrect " incorrect"
                      (when score (str " · " (int (+ 0.5 score)) "%")))))
        (dom/button
          (dom/props {:class "btn btn-primary"})
          (dom/text "New quiz")
          (dom/On "click" (fn [_] (reset! !summary nil) (reset! !session nil)) nil))))))

;; ---------------------------------------------------------------------------
;; Exam sitting — timed, no feedback until submitted
;; ---------------------------------------------------------------------------

(defn- now-ms [] #?(:cljs (js/Date.now) :clj 0))

(defn fmt-mmss
  "Seconds → m:ss."
  [s]
  (let [m (quot s 60) ss (mod s 60)]
    (str m ":" (when (< ss 10) "0") ss)))

(e/defn ExamActive
  "One exam sitting: forward-only questions, answers saved ungraded, visible
   countdown from the server's started_at (client clock only ticks display).
   Expiry auto-submits ONCE (the !submit-req atom is the latch): the current
   draft is saved if non-blank, then every saved answer grades sequentially
   and the session closes. No verdict is visible before submission.

   A blank answer can no longer be saved-and-advanced; Suspend & Skip is the escape,
   which withholds that question from future review. The frozen paper still counts
   it, so a skipped question shows as unanswered in the report."
  [user-id !session session !result-sid !editing]
  (e/client
    (let [{sid :id qids :question-ids answered :answered
           limit :time-limit-seconds elapsed :elapsed-seconds} session
          total (count qids)
          !idx (atom (min answered (dec total))) idx (e/watch !idx)
          !draft (atom "") draft (e/watch !draft)
          !now (atom (now-ms)) now (e/watch !now)
          deadline (+ (now-ms) (* 1000 (max 0 (- (or limit 0) (or elapsed 0)))))
          ;; e/on-unmount + let must live on BOTH peers (identical frame slots);
          ;; guarding only the JS interop keeps CLJ/CLJS frame counts in lockstep.
          ;; Wrapping e/on-unmount itself in #?(:cljs …) diverged the frames and
          ;; crashed the exam view with a frame_signal AIOOBE (modal-shell/20 rule).
          _ticker (let [iv #?(:cljs (js/setInterval (fn [] (reset! !now (js/Date.now))) 1000)
                              :clj nil)]
                    (e/on-unmount (fn [] #?(:cljs (js/clearInterval iv) :clj nil))))
          remaining (max 0 (quot (- deadline now) 1000))
          !submit-req (atom nil) submit-req (e/watch !submit-req)
          grading (e/server (e/watch (us/get-atom user-id :exam-grading)))
          kg-bump (e/server (e/watch (us/get-atom user-id :kg-mutations)))
          qid (nth qids (min idx (dec total)) nil)
          qdata (e/server (quiz-question* kg-bump user-id qid))
          ;; Skipping the last question submits the paper — the sitting is over
          ;; either way, and the skip contributes no answer.
          advance! (fn []
                     (if (>= (inc @!idx) total)
                       (when (nil? @!submit-req) (reset! !submit-req {:draft nil}))
                       (do (reset! !draft "") (swap! !idx inc))))]
      ;; Expiry latch — fires once; submit-req non-nil blocks re-entry.
      (when (and (zero? remaining) (nil? submit-req))
        (reset! !submit-req {:draft @!draft}))
      (dom/div
        (dom/props {:style panel-style})
        (if (some? submit-req)
          ;; Submitted (button or expiry) — save last draft, grade everything.
          (let [[done gtotal] (get grading sid [0 nil])
                draft (:draft submit-req)
                position idx
                ;; grade-exam-session! has no :success contract of its own (a
                ;; per-answer grade failure is skipped internally), but it CAN
                ;; throw on an unexpected DB error — catch that here so a throw
                ;; can't latch "Grading…" forever with no way back.
                result (e/server
                         (e/Offload
                           #(try
                              (when-not (str/blank? draft)
                                (db/record-kg-answer! user-id sid qid position draft))
                              {:success true :counts (grade/grade-exam-session! user-id sid)}
                              (catch Exception e
                                (tel/error! {:id ::exam-grading-failed} e)
                                (toasts/push! user-id
                                  {:level :error
                                   :message "Grading failed — try submitting again."})
                                {:success false}))))]
            (dom/h3 (dom/props {:style {:font-size "16px"}}) (dom/text "Grading your exam…"))
            (dom/p (dom/props {:style {:font-size "13px" :color "var(--color-text-secondary)"}})
              (dom/text (if gtotal (str done " / " gtotal " answers graded") "Preparing…")))
            (case result ; wait on the value — do would race the transition
              (if (:success result)
                (do (reset! !result-sid sid)
                  (reset! !session nil)
                  (reset! !submit-req nil))
                ;; Unlatch so the (now-expired-or-clicked) submit can retry
                ;; instead of leaving the user stuck on "Grading…" forever.
                (reset! !submit-req nil))))
          (dom/div
            (dom/div
              (dom/props {:style {:display "flex" :justify-content "space-between"
                                  :align-items "center"}})
              (dom/span (dom/props {:style {:font-size "13px" :color "var(--color-text-secondary)"}})
                (dom/text (str "Question " (inc idx) " / " total)))
              (dom/span
                (dom/props {:aria-label "Time remaining"
                            :style {:font-size "15px" :font-weight "700"
                                    :font-variant-numeric "tabular-nums"
                                    :color (if (< remaining 60)
                                             "var(--color-danger, #c62828)"
                                             "var(--color-text-primary)")}})
                (dom/text (fmt-mmss remaining)))
              (dom/button
                (dom/props {:class "btn btn-sm" :aria-label "Submit exam now"})
                (dom/text "Submit exam")
                (dom/On "click" (fn [_] (when (nil? @!submit-req)
                                          (reset! !submit-req {:draft @!draft})))
                  nil)))
            (dom/h3 (dom/props {:style {:font-size "16px" :margin "14px 0"}})
              (dom/text (str (:question qdata))))
            (curate/CurationBar user-id qdata advance! !editing)
            ;; Forms5: Input! (tracked textarea) + Button! (tracked save), via
            ;; e/amb — never `do` here, it would swallow whichever token isn't
            ;; last and hang that control. Field edits mirror into !draft and
            ;; ack immediately (no server round-trip per keystroke); the save
            ;; command persists the answer.
            (e/for [[t edit] (e/amb
                               (forms/Input! :exam/answer draft :as :textarea
                                 :class "form-input" :rows 4
                                 :placeholder "Answer in your own words — graded when you submit the exam"
                                 :aria-label "Your answer"
                                 :style {:width "100%" :resize "vertical"})
                               (forms/Button! [::save]
                                 :class "btn btn-primary" :style {:margin-top "8px"}
                                 :label (if (< (inc idx) total) "Save & next" "Save & submit exam")
                                 :disabled (str/blank? (str draft))))]
              (if (map? edit)
                (do (reset! !draft (:exam/answer edit)) (t))
                (let [answer (str/trim (str draft))
                      position idx
                      last? (>= (inc idx) total)
                      r (e/server
                          (e/Offload
                            #(save-exam-answer!* user-id sid qid position answer)))]
                  (case r ; wait on the value — do would race the token
                    (do (if last?
                          (when (nil? @!submit-req)
                            (reset! !submit-req {:draft nil}))
                          (do (reset! !draft "")
                            (swap! !idx inc)))
                      (t))))))))))))

;; ---------------------------------------------------------------------------
;; FSRS Review — a live queue of everything due today. Answers grade instantly
;; and advance the schedule (freememo.kg-grade/review-question!); a card that stays
;; due today (learning steps) is re-enqueued for this sitting. No kg_sessions row —
;; the queue is recomputed from FSRS due on every mount.
;; ---------------------------------------------------------------------------

(e/defn ReviewDone [reviewed !view]
  (e/client
    (dom/div
      (dom/props {:style {:padding "24px 8px" :text-align "center"}})
      (dom/p (dom/props {:style {:font-size "15px" :margin "0 0 6px"}})
        (dom/text (if (pos? reviewed)
                    (str "Done for today — " reviewed " reviewed.")
                    "Nothing due right now.")))
      (dom/p (dom/props {:style {:font-size "13px" :color "var(--color-text-secondary)"}})
        (dom/text "New questions surface here as you generate them and as cards come due."))
      (dom/div
        (dom/props {:style {:display "flex" :gap "8px" :justify-content "center" :margin-top "12px"}})
        (dom/button (dom/props {:class "btn"})
          (dom/text "Custom quiz")
          (dom/On "click" (fn [_] (reset! !view :custom)) nil))
        (dom/button (dom/props {:class "btn"})
          (dom/text "Dashboard")
          (dom/On "click" (fn [_] (reset! !view :dashboard)) nil))))))

(e/defn ReviewFlow
  "Live FSRS due-today queue. `!queue` holds the remaining question ids; a
   graded card that is still due today is appended back (learning steps).

   A skipped card is dropped from the queue and not counted as reviewed — the
   `::unset` seed guard means the KG bump a suspension triggers recomputes
   `initial` without reshuffling the sitting in progress."
  [user-id !view !editing navigate-source!]
  (e/client
    (let [kg-bump (e/server (e/watch (us/get-atom user-id :kg-mutations)))
          initial (e/server (review-queue* kg-bump user-id))
          !queue (atom ::unset) queue (e/watch !queue)
          !feedback (atom nil) feedback (e/watch !feedback)
          !draft (atom "") draft (e/watch !draft)
          !grading? (atom false) grading? (e/watch !grading?)
          !reviewed (atom 0) reviewed (e/watch !reviewed)
          !entity-card (atom nil) entity-card (e/watch !entity-card)]
      (dom/div
        (dom/props {:style panel-style})
        (dom/div
          (dom/props {:style {:display "flex" :justify-content "space-between"
                              :align-items "center"}})
          (dom/h2 (dom/props {:style {:font-size "18px" :margin "0"}}) (dom/text "Review"))
          (dom/div
            (dom/props {:style {:display "flex" :gap "6px"}})
            (dom/button (dom/props {:class "btn btn-sm"})
              (dom/text "Custom quiz")
              (dom/On "click" (fn [_] (reset! !view :custom)) nil))
            (qd/ReviewHistoryButton !view)))
        (cond
          (= ::unset queue) (do (reset! !queue (vec initial)) nil)
          (empty? queue) (ReviewDone reviewed !view)
          :else
          (let [qid (first queue)
                remaining (count queue)
                qdata (e/server (quiz-question* kg-bump user-id qid))
                ;; Every way out of the current card goes through here, so the drop
                ;; and the answer-box reset cannot drift between them.
                ;; `counted?` — the learner answered it, so it belongs in the
                ;; sitting's tally; a skip did not and must not inflate it.
                ;; `requeue?` — bring it back later today (FSRS learning steps).
                leave-card! (fn [counted? requeue?]
                              (let [head (first @!queue)]
                                (when counted? (swap! !reviewed inc))
                                (reset! !feedback nil)
                                (reset! !draft "")
                                (swap! !queue
                                  (fn [q]
                                    (let [rest-q (into [] (rest q))]
                                      (if requeue? (conj rest-q head) rest-q))))))
                ;; Unanswered: no tally, no return.
                skip! (fn [] (leave-card! false false))
                ;; Answered, then suspended: the grade counted, but a suspended card
                ;; must never be re-enqueued or it would reappear immediately.
                leave-suspended! (fn [] (leave-card! true false))]
            (dom/div
              (dom/props {:style {:font-size "13px" :color "var(--color-text-secondary)"
                                  :margin "10px 0"}})
              (dom/text (str remaining " due · " reviewed " reviewed")))
            (dom/h3 (dom/props {:style {:font-size "16px" :margin "14px 0"}})
              (dom/text (str (:question qdata))))
            ;; Pre-answer a suspend is a skip; post-answer the grade already counted,
            ;; so it leaves as a completed review that will not come back.
            (curate/CurationBar user-id qdata
              (if (nil? feedback) skip! leave-suspended!) !editing)
            (if (nil? feedback)
              (dom/div
                ;; Forms5: Input! (tracked textarea) + Button! (tracked submit),
                ;; via e/amb — never `do` here, it would swallow whichever token
                ;; isn't last and hang that control. Field edits mirror into
                ;; !draft and ack immediately (no server round-trip per
                ;; keystroke); the submit command grades on the server.
                (e/for [[t edit] (e/amb
                                   (forms/Input! :review/answer draft :as :textarea
                                     :class "form-input" :rows 4
                                     :placeholder "Answer in your own words…"
                                     :aria-label "Your answer"
                                     :style {:width "100%" :resize "vertical"})
                                   (forms/Button! [::submit]
                                     :class "btn btn-primary" :style {:margin-top "8px"}
                                     :label (if grading? "Grading…" "Submit")
                                     :disabled (or grading? (str/blank? (str draft)))))]
                  (if (map? edit)
                    (do (reset! !draft (:review/answer edit)) (t))
                    (let [answer (str/trim (str draft))]
                      (if (str/blank? answer)
                        (t) ; defence in depth — the button is disabled for this
                        (do (reset! !grading? true)
                          (let [res (e/server
                                      (e/Offload #(review-answer!* user-id qid answer)))]
                            (case res ; wait on the value — do would race the token
                              (do (reset! !grading? false)
                                (when (:success (:result res))
                                  (reset! !feedback {:result (:result res)
                                                     :answer answer
                                                     :schedule (:schedule res)}))
                                (t))))))))))
              (dom/div
                (fb/QuizFeedback (:result feedback) (:answer feedback)
                  (:entities qdata) !entity-card navigate-source!)
                (dom/button
                  (dom/props {:class "btn btn-primary" :style {:margin-top "12px"}})
                  (dom/text "Next")
                  (dom/On "click"
                    (fn [_]
                      (leave-card! true
                        (boolean (get-in feedback [:schedule :due-today?]))))
                    nil))))
            ;; Sibling of the answer/feedback `if` — must mount for the session.
            (fb/EntityCardPopover user-id !entity-card entity-card)))))))

(defonce !pending-preset
  ;; Palette → QuizPage handoff ({:mode "quiz"|"exam"} or {:view :history}),
  ;; set by GlobalQuizInvokers, consumed ONCE on QuizPage mount. A resumable
  ;; session still outranks a mode preset (cond order below).
  (atom nil))

;; Plain fn (NOT inside an e/defn reactive body) so the cljs-only nested calls
;; don't compile to client-only reactive nodes the server lacks. An inline
;; #?(:cljs (first (swap-vals! …)) :clj nil) in QuizPage's let diverged the
;; CLJ/CLJS frame slot counts (client > server → frame_signal AIOOBE on /quiz).
;; Same platform-split-behind-a-defn shape as now-ms.
#?(:cljs
   (defn consume-pending-preset! []
     (first (swap-vals! !pending-preset (constantly nil)))))

(e/defn GlobalQuizInvokers
  "Headless; mounted once in Main. Publishes the invokers that make the
   global quiz :nav commands available from every tab (command-bus requires
   an invoker for :exec :ui-button availability)."
  [navigate!]
  (e/client
    (bus/publish-invoker! :start-quiz
      (fn [] (reset! !pending-preset {:mode "quiz"}) (navigate! :quiz)))
    (bus/publish-invoker! :start-exam
      (fn [] (reset! !pending-preset {:mode "exam"}) (navigate! :quiz)))
    (bus/publish-invoker! :quiz-history
      (fn [] (reset! !pending-preset {:view :history}) (navigate! :quiz)))
    (e/on-unmount (fn []
                    (bus/retract-invoker! :start-quiz)
                    (bus/retract-invoker! :start-exam)
                    (bus/retract-invoker! :quiz-history)))))

(e/defn QuizPage [user-id navigate!]
  (e/client
    (let [preset (consume-pending-preset!)
          resume (e/server (active-session* user-id))
          !session (atom ::unset) session (e/watch !session)
          !summary (atom nil) summary (e/watch !summary)
          !result-sid (atom nil) result-sid (e/watch !result-sid)
          ;; The question being edited, hoisted here so one modal serves all three
          ;; flows — the flows differ in how they advance, not in how they edit.
          !editing (atom nil)
          ;; Landing is the dashboard; an explicit start-quiz/start-exam invoker
          ;; (preset :mode) jumps to the custom setup, and the history preset lands
          ;; on the dashboard it was folded into.
          !view (atom (if (:mode preset) :custom :dashboard)) view (e/watch !view)
          ;; Fact provenance in feedback navigates to the source document page —
          ;; the shortest route from "this question is wrong" to the material.
          navigate-source! (fn [topic-id page]
                             (navigate! :viewer (nav/nav-topic topic-id :quiz page)))]
      (curate/QuestionEditorModal user-id !editing)
      (cond
        (= ::unset session) (do (reset! !session resume) nil)
        (some? result-sid) (qd/SessionResult user-id result-sid !result-sid)
        (some? summary) (QuizSummary !session !summary summary)
        (some? session) (if (= "exam" (:kind session))
                          (ExamActive user-id !session session !result-sid !editing)
                          (QuizActive user-id !session session !summary !editing navigate-source!))
        (= :custom view) (QuizSetup user-id !session (:mode preset) !view)
        (= :review view) (ReviewFlow user-id !view !editing navigate-source!)
        :else (qd/QuizDashboard user-id
                (fn [] (reset! !view :review))
                (fn [] (reset! !view :custom))
                navigate-source!)))))
