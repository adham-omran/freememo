(ns freememo.quiz-page
  "Quiz tab (plan M4): scope → frozen draw → answer → instant LLM grade with
   keyword highlighting and (Doc, p.N) provenance.

   Session state lives in kg_sessions/kg_answers — reload lands back in the
   active session at the first unanswered question (spec 6.1 done-condition).
   Client atoms only steer the in-page flow."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.electric-scroll0 :refer [Scroll-window Tape]]
   [clojure.string :as str]
   [freememo.command-bus :as bus]
   [freememo.icons :as icons]
   [freememo.modal-shell :as modal]
   #?(:clj [freememo.db :as db])
   #?(:clj [freememo.kg-grade :as grade])
   #?(:clj [freememo.toasts :as toasts])
   #?(:clj [freememo.user-state :as us])))

;; ---------------------------------------------------------------------------
;; Keyword highlighting (spec 6.5) — pure, shared by reference + user answer
;; ---------------------------------------------------------------------------

(defn highlight-segments
  "Split text into [[segment highlighted?] ...] on case-insensitive keyword
   occurrences. Earliest match wins; ties go to the longest keyword.
   Post: (apply str (map first result)) = text — nothing added or lost, so a
   keyword absent from the text can never produce a phantom highlight."
  [text keywords]
  (let [t (str text)
        lower (str/lower-case t)
        kws (->> keywords (map str/lower-case) (remove str/blank?) distinct
              (sort-by count >) vec)]
    (if (or (empty? kws) (= t ""))
      [[t false]]
      (loop [from 0 acc []]
        (let [hit (reduce (fn [best kw]
                            (let [i (str/index-of lower kw from)]
                              (if (and i (or (nil? best)
                                             (< i (first best))
                                             (and (= i (first best))
                                               (> (count kw) (second best)))))
                                [i (count kw)]
                                best)))
                    nil kws)]
          (if (nil? hit)
            (into [] (remove #(= "" (first %)))
              (conj acc [(subs t from) false]))
            (let [[i len] hit
                  acc (cond-> acc (> i from) (conj [(subs t from i) false]))]
              (recur (+ i len) (conj acc [(subs t i (+ i len)) true])))))))))

(defn- entity-lexicon
  "[{:id :label :aliases}] → {lower-mention {:id :label}} for segment lookup."
  [entities]
  (into {}
    (comp (mapcat (fn [{:keys [id label aliases]}]
                    (map #(vector (str/lower-case (str %)) {:id id :label label})
                      (cons label aliases))))
      (remove (comp str/blank? first)))
    entities))

(e/defn EntityLinkedText
  "Text with entity mentions as concept links (click → popover via
   !entity-card) and matched keywords highlighted. Highlight ⊆ links: every
   mention comes from the graph's labels/aliases, so a click always resolves."
  [text entities matched-keywords !entity-card]
  (e/client
    (let [lex (entity-lexicon entities)
          matched (into #{} (map str/lower-case) matched-keywords)]
      (e/for [[idx seg hit] (e/diff-by first
                              (map-indexed (fn [i [s h]] [i s h])
                                (highlight-segments text (keys lex))))]
        (let [_ idx
              ent (when hit (lex (str/lower-case seg)))]
          (if ent
            (dom/span
              (dom/props {:role "button"
                          :style (cond-> {:cursor "pointer"
                                          :text-decoration "underline dotted"
                                          :text-underline-offset "3px"
                                          :border-radius "2px" :padding "0 2px"}
                                   (matched (str/lower-case seg))
                                   (assoc :background "var(--color-warning-bg, #fff3cd)"
                                     :font-weight "600"))})
              (dom/text seg)
              (dom/On "click" (fn [_] (reset! !entity-card ent)) nil))
            (dom/text seg)))))))

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
   (defn quiz-question* [user-id question-id]
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
   (defn history-sessions* [user-id]
     (db/list-kg-sessions user-id)))

#?(:clj
   (defn session-detail* [user-id session-id]
     (db/kg-session-detail user-id session-id)))

#?(:clj
   (defn finish-quiz!*
     "Close the session; return its verdict tally for the summary."
     [user-id session-id]
     (db/finish-kg-session! user-id session-id)
     (db/kg-session-verdict-counts user-id session-id)))

#?(:clj
   (defn entity-card* [user-id entity-id]
     (db/kg-entity-card user-id entity-id)))

;; ---------------------------------------------------------------------------
;; Views
;; ---------------------------------------------------------------------------

(def ^:private panel-style
  {:max-width "720px" :margin "0 auto" :padding "16px"})

;; Fixed row height for the virtualized material (document) list — Scroll-window
;; needs it up front and the .tape-scroll CSS consumes it as --row-height.
(def ^:private quiz-material-row-height 36)

;; Fixed row height for the virtualized session-history list in QuizHistoryModal.
(def ^:private quiz-history-row-height 52)

(e/defn QuizSetup [user-id !session !history-open? initial-mode]
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
          (dom/h2 (dom/props {:style {:font-size "18px" :margin "0"}})
            (dom/text (if (= mode "exam") "Start an exam" "Start a quiz")))
          (dom/button
            (dom/props {:class "btn btn-sm" :aria-label "Session history"})
            (icons/Icon :history :size 14)
            (dom/text " History")
            (dom/On "click" (fn [_] (reset! !history-open? true)) nil)))
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
        (if (empty? docs)
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
              (let [row-count (count docs)
                    [offset limit] (Scroll-window quiz-material-row-height row-count dom/node
                                     {:overquery-factor 2})]
                (dom/props {:class "tape-scroll"
                            :style {:--count row-count
                                    :--grid-cols "1fr"
                                    :--row-height (str quiz-material-row-height "px")}})
                (dom/table
                  (dom/props {:style {:width "100%"}})
                  (e/for [i (Tape offset limit)]
                    (when-let [{:keys [id title questions]} (nth docs i nil)]
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
                            (dom/input
                              (dom/props {:type "checkbox" :checked (contains? scope id)})
                              (dom/On "change"
                                (fn [_] (swap! !scope #(if (% id) (disj % id) (conj % id)))) nil))
                            (dom/span (dom/text title))
                            (dom/span (dom/props {:style {:color "var(--color-text-secondary)"
                                                          :font-size "12px" :margin-left "auto"}})
                              (dom/text (str questions " questions")))))))))))
            (dom/div
              (dom/props {:style {:display "flex" :gap "8px" :align-items "center" :margin-top "12px"}})
              (dom/label (dom/props {:style {:font-size "13px"}}) (dom/text "Questions:"))
              (dom/input
                (dom/props {:type "number" :min "1" :max "50" :value count-str
                            :class "form-input" :style {:width "70px"}
                            :aria-label "Question count"})
                (dom/On "input" (fn [ev] (reset! !count-str (.. ev -target -value))) nil))
              (when (= mode "exam")
                (dom/label (dom/props {:style {:font-size "13px"}}) (dom/text "Minutes:"))
                (dom/input
                  (dom/props {:type "number" :min "1" :max "180" :value limit-str
                              :class "form-input" :style {:width "70px"}
                              :aria-label "Time limit in minutes"})
                  (dom/On "input" (fn [ev] (reset! !limit-str (.. ev -target -value))) nil)))
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

(e/defn EntityMention
  "A clickable entity label inside a fact line (nil id → plain text)."
  [label entity-id !entity-card]
  (e/client
    (if entity-id
      (dom/span
        (dom/props {:role "button"
                    :style {:cursor "pointer" :text-decoration "underline dotted"
                            :text-underline-offset "3px"}})
        (dom/text label)
        (dom/On "click" (fn [_] (reset! !entity-card {:id entity-id :label label})) nil))
      (dom/text label))))

(e/defn FactLine
  "One fact as `s — p → o (Doc, p.N)` with linkable entities."
  [{:keys [subject_label predicate_label object_label object_literal
           doc_title page_number subject_entity_id object_entity_id]}
   !entity-card]
  (e/client
    (dom/li
      (dom/props {:style {:font-size "13px" :padding "2px 0"}})
      (EntityMention subject_label subject_entity_id !entity-card)
      (dom/text (str " — " predicate_label " → "))
      (if object_label
        (EntityMention object_label object_entity_id !entity-card)
        (dom/text object_literal))
      (dom/span
        (dom/props {:style {:color "var(--color-text-secondary)" :font-size "12px"
                            :margin-left "6px"}})
        (dom/text (str "(" doc_title (when page_number (str ", p." page_number)) ")"))))))

(e/defn EntityCardPopover
  "Concept card: the clicked entity's label, aliases, and fact neighborhood.
   Fact rows link onward — the popover walks the graph."
  [user-id !entity-card entity-card]
  (e/client
    (when entity-card
      (let [card (e/server (entity-card* user-id (:id entity-card)))]
        (dom/div ; backdrop — click closes
          (dom/props {:style {:position "fixed" :inset "0" :z-index "1200"
                              :background "rgba(0,0,0,0.35)"
                              :display "flex" :align-items "center"
                              :justify-content "center"}})
          (dom/On "click" (fn [_] (reset! !entity-card nil)) nil)
          (dom/div
            (dom/props {:role "dialog" :aria-label (str "About " (:label card))
                        :style {:background "var(--color-bg, #fff)"
                                :border-radius "8px" :padding "16px 20px"
                                :max-width "600px" :width "90%"
                                :max-height "70vh" :overflow-y "auto"
                                :box-shadow "0 8px 32px rgba(0,0,0,0.25)"}})
            (dom/On "click" (fn [e] #?(:cljs (.stopPropagation e)) nil) nil)
            (dom/h3 (dom/props {:style {:font-size "16px" :margin "0 0 4px"}})
              (dom/text (:label card)))
            (when (seq (:aliases card))
              (dom/p (dom/props {:style {:font-size "12px" :margin "0 0 8px"
                                         :color "var(--color-text-secondary)"}})
                (dom/text (str "Also: " (str/join ", " (:aliases card))))))
            (if (empty? (:facts card))
              (dom/p (dom/props {:style {:font-size "13px"}})
                (dom/text "No facts reference this concept."))
              (dom/ul (dom/props {:style {:margin "4px 0" :padding-left "20px"}})
                (e/for [f (e/diff-by :id (:facts card))]
                  (FactLine f !entity-card))))))))))

(def ^:private verdict-badge
  {"correct"   ["✓ Correct" "var(--color-success-dark, #2e7d32)"]
   "partial"   ["◐ Partial" "#b26a00"]
   "incorrect" ["✗ Incorrect" "var(--color-danger, #c62828)"]})

(e/defn QuizFeedback
  "Graded result: verdict, explanation, reference + user answer with entity
   mentions linked (matched keywords highlighted), missed facts with
   provenance — every entity click opens the concept popover."
  [result user-answer entities !entity-card]
  (e/client
    (let [{:keys [verdict explanation reference-answer matched-keywords missed-facts]} result
          [label color] (get verdict-badge verdict ["?" "inherit"])]
      (dom/div
        (dom/props {:style {:border "1px solid var(--color-border)" :border-radius "8px"
                            :padding "12px 16px" :margin-top "12px"}})
        (dom/div
          (dom/props {:style {:font-weight "700" :color color :font-size "15px"}})
          (dom/text label))
        (dom/p (dom/props {:style {:margin "8px 0" :font-size "14px"}})
          (dom/text explanation))
        (dom/div (dom/props {:style {:font-size "12px" :color "var(--color-text-secondary)"
                                     :margin-top "10px"}})
          (dom/text "Reference answer"))
        (dom/p (dom/props {:style {:font-size "14px" :margin "4px 0"}})
          (EntityLinkedText reference-answer entities matched-keywords !entity-card))
        (dom/div (dom/props {:style {:font-size "12px" :color "var(--color-text-secondary)"
                                     :margin-top "10px"}})
          (dom/text "Your answer"))
        (dom/p (dom/props {:style {:font-size "14px" :margin "4px 0"}})
          (EntityLinkedText user-answer entities matched-keywords !entity-card))
        (when (seq missed-facts)
          (dom/div
            (dom/div (dom/props {:style {:font-size "12px" :color "var(--color-text-secondary)"
                                         :margin-top "10px"}})
              (dom/text "You missed"))
            (dom/ul (dom/props {:style {:margin "4px 0" :padding-left "20px"}})
              (e/for [f (e/diff-by :id missed-facts)]
                (FactLine f !entity-card)))))))))

(e/defn QuizActive [user-id !session session !summary]
  (e/client
    (let [{sid :id qids :question-ids answered :answered} session
          total (count qids)
          !idx (atom answered) idx (e/watch !idx)
          !draft (atom "") ; textarea mirror
          !feedback (atom nil) feedback (e/watch !feedback)
          !entity-card (atom nil) entity-card (e/watch !entity-card)
          qid (nth qids (min idx (dec total)) nil)
          qdata (e/server (quiz-question* user-id qid))]
      (dom/div
        (dom/props {:style panel-style})
        (dom/div
          (dom/props {:style {:display "flex" :justify-content "space-between"
                              :align-items "center"}})
          (dom/span (dom/props {:style {:font-size "13px" :color "var(--color-text-secondary)"}})
            (dom/text (str "Question " (inc idx) " / " total)))
          (dom/button
            (dom/props {:class "btn btn-sm" :aria-label "End quiz"})
            (dom/text "End quiz")
            (let [click (dom/On "click" (fn [_] {:id (str (random-uuid))}) nil)
                  [t _] (e/Token click)]
              (when t
                (let [counts (e/server (e/Offload #(finish-quiz!* user-id sid)))]
                  (case counts ; wait on the value — do would race the token
                    (do (reset! !summary {:counts counts :total total})
                      (t))))))))
        (dom/h3 (dom/props {:style {:font-size "16px" :margin "14px 0"}})
          (dom/text (str (:question qdata))))
        (if (nil? feedback)
          (dom/div
            (dom/textarea
              (dom/props {:class "form-input" :rows 4 :placeholder "Answer in your own words…"
                          :aria-label "Your answer"
                          :style {:width "100%" :resize "vertical"}})
              (dom/On "input" (fn [ev] (reset! !draft (.. ev -target -value))) nil))
            (dom/button
              (dom/props {:class "btn btn-primary" :style {:margin-top "8px"}})
              (let [click (dom/On "click"
                            (fn [_]
                              (let [a (str/trim (str @!draft))]
                                (when-not (str/blank? a)
                                  {:id (str (random-uuid)) :answer a})))
                            nil)
                    [t _] (e/Token click)]
                (dom/props {:disabled (some? t)})
                (dom/text (if (some? t) "Grading…" "Submit"))
                (when t
                  (let [answer (:answer click)
                        position idx
                        result (e/server
                                 (e/Offload
                                   #(grade/grade-answer! user-id sid qid position answer)))]
                    (case result ; wait on the value — do would race the token
                      (do (when (:success result)
                            (reset! !feedback {:result result :answer answer}))
                        (t))))))))
          (dom/div
            (QuizFeedback (:result feedback) (:answer feedback)
              (:entities qdata) !entity-card)
            (dom/button
              (dom/props {:class "btn btn-primary" :style {:margin-top "12px"}})
              (dom/text (if (< (inc idx) total) "Next question" "Finish"))
              (if (< (inc idx) total)
                (dom/On "click" (fn [_]
                                  (reset! !feedback nil)
                                  (reset! !draft "")
                                  (swap! !idx inc))
                  nil)
                (let [click (dom/On "click" (fn [_] {:id (str (random-uuid))}) nil)
                      [t _] (e/Token click)]
                  (when t
                    (let [counts (e/server (e/Offload #(finish-quiz!* user-id sid)))]
                      (case counts ; wait on the value — do would race the token
                        (do (reset! !summary {:counts counts :total total})
                          (t))))))))))
        ;; Sibling of the answer/feedback `if`, inside the panel div — NOT
        ;; nested in any branch: it must mount for the whole session.
        (EntityCardPopover user-id !entity-card entity-card)))))

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
;; Exam sitting — timed, no feedback until submitted (spec 6.3)
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
   and the session closes. No verdict is visible before submission."
  [user-id !session session !result-sid]
  (e/client
    (let [{sid :id qids :question-ids answered :answered
           limit :time-limit-seconds elapsed :elapsed-seconds} session
          total (count qids)
          !idx (atom (min answered (dec total))) idx (e/watch !idx)
          !draft (atom "")
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
          qid (nth qids (min idx (dec total)) nil)
          qdata (e/server (quiz-question* user-id qid))]
      ;; Expiry latch — fires once; submit-req non-nil blocks re-entry.
      (when (and (zero? remaining) (nil? submit-req))
        (reset! !submit-req {:draft #?(:cljs @!draft :clj "")}))
      (dom/div
        (dom/props {:style panel-style})
        (if (some? submit-req)
          ;; Submitted (button or expiry) — save last draft, grade everything.
          (let [[done gtotal] (get grading sid [0 nil])
                draft (:draft submit-req)
                position idx
                counts (e/server
                         (e/Offload
                           #(do (when-not (str/blank? draft)
                                  (db/record-kg-answer! user-id sid qid position draft))
                              (grade/grade-exam-session! user-id sid))))]
            (dom/h3 (dom/props {:style {:font-size "16px"}}) (dom/text "Grading your exam…"))
            (dom/p (dom/props {:style {:font-size "13px" :color "var(--color-text-secondary)"}})
              (dom/text (if gtotal (str done " / " gtotal " answers graded") "Preparing…")))
            (case counts ; wait on the value — do would race the transition
              (do (reset! !result-sid sid)
                (reset! !session nil)
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
            (dom/textarea
              (dom/props {:class "form-input" :rows 4
                          :placeholder "Answer in your own words — graded when you submit the exam"
                          :aria-label "Your answer"
                          :style {:width "100%" :resize "vertical"}})
              (dom/On "input" (fn [ev] (reset! !draft (.. ev -target -value))) nil))
            (dom/button
              (dom/props {:class "btn btn-primary" :style {:margin-top "8px"}})
              (dom/text (if (< (inc idx) total) "Save & next" "Save & submit exam"))
              (let [click (dom/On "click"
                            (fn [_] {:id (str (random-uuid))
                                     :answer (str/trim (str @!draft))})
                            nil)
                    [t _] (e/Token click)]
                (dom/props {:disabled (some? t)})
                (when t
                  (let [answer (:answer click)
                        position idx
                        last? (>= (inc idx) total)
                        r (e/server
                            (e/Offload
                              #(do (when-not (str/blank? answer)
                                     (save-exam-answer!* user-id sid qid position answer))
                                 :saved)))]
                    (case r ; wait on the value — do would race the token
                      (do (if last?
                            (when (nil? @!submit-req)
                              (reset! !submit-req {:draft nil}))
                            (do (reset! !draft "")
                              (swap! !idx inc)))
                        (t)))))))))))))

;; ---------------------------------------------------------------------------
;; History + session result (spec 6.6). SessionResult doubles as the exam's
;; post-grading report — one component for both.
;; ---------------------------------------------------------------------------

(defn- session-score
  "Σ(1 / 0.5 / 0) over `total` questions, as a rounded percentage."
  [correct partial total]
  (when (pos? (or total 0))
    (int (+ 0.5 (* 100.0 (/ (+ correct (* 0.5 partial)) total))))))

;; History is now a virtualized modal overlay — see QuizHistoryModal (below
;; SessionResult, which it reuses for the in-modal drill-down).

(e/defn SessionResult
  "Per-question record of a finished session — the exam report and the
   history drill-down. Verdict nil renders as ungraded ('—')."
  [user-id result-sid !result-sid]
  (e/client
    (let [{:keys [session answers]} (e/server (session-detail* user-id result-sid))
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
        (e/for [{:keys [position question reference_answer user_answer verdict explanation]}
                (e/diff-by :position answers)]
          (let [[label color] (get verdict-badge verdict ["— Ungraded" "var(--color-text-secondary)"])]
            (dom/div
              (dom/props {:style {:border "1px solid var(--color-border)" :border-radius "8px"
                                  :padding "10px 14px" :margin "10px 0"}})
              (dom/div
                (dom/props {:style {:display "flex" :gap "8px" :align-items "baseline"}})
                (dom/span (dom/props {:style {:font-weight "700" :color color :font-size "13px"
                                              :flex-shrink "0"}})
                  (dom/text label))
                (dom/span (dom/props {:style {:font-size "14px" :font-weight "600"}})
                  (dom/text (str (inc position) ". " question))))
              (dom/p (dom/props {:style {:font-size "13px" :margin "6px 0 0"}})
                (dom/text (str "Your answer: " user_answer)))
              (when explanation
                (dom/p (dom/props {:style {:font-size "13px" :margin "4px 0 0"
                                           :color "var(--color-text-secondary)"}})
                  (dom/text explanation)))
              (dom/p (dom/props {:style {:font-size "13px" :margin "4px 0 0"
                                         :color "var(--color-text-secondary)"}})
                (dom/text (str "Reference: " reference_answer))))))))))

(e/defn QuizHistoryModal
  "Finished-session history as a modal overlay with a virtualized list. Selecting
   a session swaps the body to its SessionResult report (modal-local !result-sid);
   Back returns to the list. Closing resets both. Chrome mirrors HistoryModal /
   ZoteroPickerModal (fixed height so the virtual viewport is bounded)."
  [user-id !open? !result-sid]
  (e/client
    (when (e/watch !open?)
      (let [result-sid (e/watch !result-sid)
            close! (fn [] (reset! !open? false) (reset! !result-sid nil))]
        (dom/div
          (dom/props {:class "modal-backdrop"})
          (dom/On "click" (fn [_] (close!)) nil)
          (modal/ModalEscape close! "Quiz history")
          (dom/div
            (dom/props {:class "modal-content"
                        :style {:width "min(680px, 95vw)" :height "75vh"
                                :display "flex" :flex-direction "column" :padding "0"}})
            (dom/On "click" (fn [e] (.stopPropagation e)) nil)
            (dom/div
              (dom/props {:style {:display "flex" :align-items "center" :gap "8px"
                                  :padding "12px 16px" :flex-shrink "0"
                                  :border-bottom "1px solid var(--color-border)"}})
              (dom/h3 (dom/props {:style {:margin "0" :font-size "16px" :flex "1"}})
                (dom/text "Quiz history"))
              (dom/button
                (dom/props {:class "btn btn-sm" :aria-label "Close"})
                (icons/Icon :x :size 16)
                (dom/On "click" (fn [_] (close!)) nil)))
            (if (some? result-sid)
              (dom/div
                (dom/props {:style {:flex "1" :min-height "0" :overflow-y "auto"}})
                (SessionResult user-id result-sid !result-sid))
              (dom/div
                (dom/props {:style {:flex "1" :min-height "0" :overflow-y "auto"}})
                (let [sessions (e/server (history-sessions* user-id))
                      row-count (count sessions)]
                  (if (zero? row-count)
                    (dom/p (dom/props {:style {:padding "24px 16px" :text-align "center"
                                               :color "var(--color-text-secondary)"}})
                      (dom/text "No finished sessions yet."))
                    (let [[offset limit] (Scroll-window quiz-history-row-height row-count dom/node
                                           {:overquery-factor 2})]
                      (dom/props {:class "tape-scroll"
                                  :style {:--count row-count
                                          :--grid-cols "1fr"
                                          :--row-height (str quiz-history-row-height "px")}})
                      (dom/table
                        (dom/props {:style {:width "100%"}})
                        (e/for [i (Tape offset limit)]
                          (when-let [{:keys [id kind total started correct partial incorrect]}
                                     (nth sessions i nil)]
                            (dom/tr
                              (dom/props {:class (when (even? i) "row-alt")
                                          :style {:--order i
                                                  :height (str quiz-history-row-height "px")}})
                              (dom/td
                                (dom/props {:role "button" :tabindex "0"
                                            :style {:display "flex" :gap "12px" :align-items "center"
                                                    :padding "0 14px" :cursor "pointer"
                                                    :border-bottom "1px solid var(--color-bg-subtle)"}})
                                (dom/On "click" (fn [_] (reset! !result-sid id)) nil)
                                (dom/span
                                  (dom/props {:class "type-badge"
                                              :style {:background (if (= kind "exam")
                                                                    "var(--color-badge-epub)"
                                                                    "var(--color-badge-pdf)")}})
                                  (dom/text kind))
                                (dom/span (dom/props {:style {:font-size "13px"}})
                                  (dom/text started))
                                (dom/span (dom/props {:style {:font-size "13px"
                                                              :color "var(--color-text-secondary)"}})
                                  (dom/text (str (+ correct partial incorrect) " / " total " answered")))
                                (dom/span (dom/props {:style {:font-size "14px" :font-weight "600"
                                                              :margin-left "auto"}})
                                  (dom/text (str (or (session-score correct partial total) "—") "%")))))))))))))))))))

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

(e/defn QuizPage [user-id]
  (e/client
    (let [preset (consume-pending-preset!)
          resume (e/server (active-session* user-id))
          !session (atom ::unset) session (e/watch !session)
          !summary (atom nil) summary (e/watch !summary)
          !result-sid (atom nil) result-sid (e/watch !result-sid)
          ;; History is a modal overlay, not a view branch: seeded open from the
          ;; :quiz-history preset handoff, and QuizSetup's History button opens it.
          !history-open? (atom (= :history (:view preset)))
          !history-result-sid (atom nil)]  ; modal-local drill-down, independent of the page result
      (QuizHistoryModal user-id !history-open? !history-result-sid)
      (cond
        (= ::unset session) (do (reset! !session resume) nil)
        (some? result-sid) (SessionResult user-id result-sid !result-sid)
        (some? summary) (QuizSummary !session !summary summary)
        (nil? session) (QuizSetup user-id !session !history-open? (:mode preset))
        (= "exam" (:kind session)) (ExamActive user-id !session session !result-sid)
        :else (QuizActive user-id !session session !summary)))))
