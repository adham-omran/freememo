(ns freememo.quiz-item-stats
  "One quiz item's lifetime record, as a modal — the panel Anki calls Card Info.

   The item is [:question id] or [:card flashcard-id ord], the ref the review queue
   and the review log already speak, so the same panel serves both arms. It opens
   from three places: a review's detail, a Knowledge question row, and a Library
   card row.

   A Library row is ONE flashcard but can be SEVERAL quiz items — a cloze card is
   one item per deletion, each with its own schedule — so `!stats` may carry a card
   with no ord and this namespace resolves it: one live ord opens directly, several
   show a picker first. Which ords are live is db/card-quiz-ords' answer, never a
   caller's: the kind restriction and the live-ord rule both live down there.

   Four of Anki's fields have no analog here and are deliberately absent rather
   than mapped to something that only looks like them: Position (there is no
   new-card queue position — the draw orders by id), Preset (there is one FSRS
   config per user, not several), Note Type, and Deck. Source document, item kind
   and item id say what this app actually knows instead.

   Time is the one field with a gap. `kg_reviews.duration_ms` is new and was not
   backfilled, so an item reviewed five times may carry one measurement; every time
   field therefore states its own denominator rather than averaging NULL as zero."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.electric-svg3 :as svg]
   [clojure.string :as str]
   [freememo.fsrs :as fsrs]
   [freememo.icons :as icons]
   [freememo.math :as math]
   [freememo.modal-shell :as modal]
   [freememo.tooltip :as tooltip]
   #?(:clj [freememo.db :as db])
   #?(:clj [freememo.settings :as settings])
   #?(:clj [freememo.user-state :as us])))

;; ---------------------------------------------------------------------------
;; Server queries — whole defns under #?(:clj …) so CLJS misuse warns loudly
;; instead of silently nil'ing; called only inside e/server.
;; ---------------------------------------------------------------------------

#?(:clj
   (defn item-stats*
     "The panel's whole payload in one round trip: the item's record plus the two
      scheduler constants the forgetting curve needs.

      The curve is drawn on the client because freememo.fsrs is .cljc — shipping
      {:decay :factor :desired-retention} costs three numbers, while a point series
      computed here would cost one per sample and would have to be recomputed
      server-side whenever the chart's width changed.
      Post: db/quiz-item-stats' map plus :curve, or nil when not owned/found."
     [_kg-bump user-id ref]
     (when-let [s (db/quiz-item-stats user-id ref)]
       (assoc s :curve (-> (settings/fsrs-config user-id)
                         :scheduler
                         (select-keys [:decay :factor :desired-retention]))))))

#?(:clj
   (defn card-ords* [_kg-bump user-id card-id]
     (db/card-quiz-ords user-id card-id)))

;; ---------------------------------------------------------------------------
;; Pure
;; ---------------------------------------------------------------------------

(defn state-label
  "FSRS state int → the word Anki uses in a review's Type column.
   Pre:  `n` is 1, 2 or 3 (db.clj's fsrs_state vocabulary).
   Post: a string; \"—\" for anything else, including nil."
  [n]
  (case (long (or n 0)) 1 "Learn" 2 "Review" 3 "Relearn" "—"))

(defn difficulty-pct
  "FSRS difficulty (1..10) as the percentage Anki displays.
   Post: nil when never reviewed — difficulty is NULL until the first review."
  [d]
  (when d (int (+ 0.5 (* 100.0 (/ (- (double d) 1.0) 9.0))))))

(defn retrievability-pct
  "Probability of recall right now, as a percentage.
   Pre:  `curve` carries :factor and :decay from the user's scheduler.
   Post: nil when the item was never reviewed (no stability, no elapsed time) —
         'not applicable', which is not 0%."
  [curve stability days-since]
  (when (and stability (pos? (double stability)) days-since)
    (int (+ 0.5 (* 100.0 (fsrs/retrievability curve (double stability)
                           (max 0.0 (double days-since))))))))

(defn fmt-days
  "A day count as an interval, in the units Anki switches between.
   Post: \"—\" for nil; days under 30, months under 365, else years."
  [d]
  (cond
    (nil? d) "—"
    (< d 30) (str d (if (= 1 d) " day" " days"))
    (< d 365) (str (/ (double (int (+ 0.5 (* 10 (/ d 30.44))))) 10.0) " months")
    :else (str (/ (double (int (+ 0.5 (* 10 (/ d 365.25))))) 10.0) " years")))

(defn fmt-secs
  "Milliseconds as seconds, one decimal.
   Post: \"—\" for nil — NOT RECORDED, which is not zero."
  [ms]
  (if (nil? ms) "—" (str (/ (double (int (+ 0.5 (/ (double ms) 100.0)))) 10.0) "s")))

(defn time-totals
  "Total and average turn time over the reviews that carry one.

   Post: {:total ms|nil :average ms|nil :timed n :of n}. :timed < :of means the
         denominator is partial and the caller MUST say so — duration_ms is NULL
         for every review taken before the column existed, and averaging NULL as
         zero would report a fast card that is merely an old one."
  [reviews]
  (let [ds (into [] (keep :duration_ms) reviews)
        n (count ds)]
    {:total (when (pos? n) (reduce + 0 ds))
     :average (when (pos? n) (long (/ (reduce + 0 ds) n)))
     :timed n
     :of (count reviews)}))

;; ---------------------------------------------------------------------------
;; Forgetting curve
;; ---------------------------------------------------------------------------

(def ^:private samples-per-segment
  "Points per inter-review segment. The decay is smooth and the chart is ~600px
   wide, so more than this buys nothing visible."
  24)

(defn curve-points
  "Retrievability over time, as [{:x epoch-seconds :y 0..1 :projected? bool}].

   One segment per review: from that review's instant to the next one, decaying
   from its stability_after. The LAST segment carries past `end-epoch` and is
   marked :projected? — it is what has not happened yet, and Anki draws it dashed.
   A rating resets retrievability to 1, which is why the series saw-tooths.

   Pre:  `reviews` is OLDEST FIRST, each with :at_epoch and :stability_after
         (db/quiz-item-stats' order).
   Post: [] when there is no review, or none with a stability — nothing to draw."
  [curve reviews end-epoch]
  (let [usable (filterv #(and (:at_epoch %) (:stability_after %)) reviews)
        n (count usable)]
    (if (zero? n)
      []
      (into []
        (comp
          (map-indexed
            (fn [i {:keys [at_epoch stability_after]}]
              (let [t0 (double at_epoch)
                    last? (= i (dec n))
                    t1 (double (if last?
                                 (max (double end-epoch) (+ t0 86400.0))
                                 (:at_epoch (nth usable (inc i)))))
                    s (double stability_after)
                    step (/ (- t1 t0) samples-per-segment)]
                (mapv (fn [k]
                        (let [t (+ t0 (* k step))]
                          {:x t
                           :y (fsrs/retrievability curve s (/ (- t t0) 86400.0))
                           :projected? last?}))
                  (range (inc samples-per-segment))))))
          cat)
        usable))))

(defn- scale-fn
  "Linear map from [lo hi] onto [0 span]. Post: a fn; identity-ish when lo = hi,
   so a single-instant series does not divide by zero."
  [lo hi span]
  (let [d (- hi lo)]
    (if (zero? d) (fn [_] 0.0) (fn [v] (* span (/ (- v lo) d))))))

(e/defn ForgettingCurve
  "Retrievability decay across the item's history, with the future dashed.

   Hand-rolled SVG, as learn-page's bar chart is — no charting dependency, and the
   vendored tree is untouched. The horizontal line is the user's desired retention,
   so the dashed crossing IS the due date's justification.
   Pre:  `points` from curve-points, non-empty."
  [points retention]
  (e/client
    (let [w 620 h 200 pad-l 34 pad-b 22 pad-t 10
          plot-w (- w pad-l 8) plot-h (- h pad-b pad-t)
          xs (mapv :x points) ys (mapv :y points)
          x-lo (reduce min xs) x-hi (reduce max xs)
          ;; Floor the y window at the retention line so it is always on screen,
          ;; and never above 1 — the axis reads as a percentage.
          y-lo (min (reduce min ys) (double retention))
          sx (scale-fn x-lo x-hi plot-w)
          sy (scale-fn y-lo 1.0 plot-h)
          ->px (fn [{:keys [x y]}]
                 (str (+ pad-l (sx x)) "," (+ pad-t (- plot-h (sy y)))))
          past (filterv (complement :projected?) points)
          ;; The projected run starts at the last past point, or the series would
          ;; show a gap where the two polylines meet.
          future (into (if (seq past) [(peek past)] []) (filter :projected?) points)
          retention-y (+ pad-t (- plot-h (sy (double retention))))]
      (svg/svg
        (dom/props {:viewBox (str "0 0 " w " " h) :width "100%"
                    :role "img"
                    :aria-label "Forgetting curve: probability of recall over time"
                    :style {:display "block" :max-width (str w "px")}})
        (svg/line
          (dom/props {:x1 pad-l :y1 retention-y :x2 (- w 8) :y2 retention-y
                      :stroke "var(--color-text-secondary)" :stroke-width 1
                      :stroke-dasharray "4 4" :opacity "0.7"}))
        (svg/text
          (dom/props {:x 2 :y (+ retention-y 4) :font-size 10
                      :fill "var(--color-text-secondary)"})
          (dom/text (str (int (+ 0.5 (* 100 retention))) "%")))
        (when (seq past)
          (svg/polyline
            (dom/props {:points (str/join " " (mapv ->px past))
                        :fill "none" :stroke "var(--color-primary-text)"
                        :stroke-width 2})))
        (when (seq future)
          (svg/polyline
            (dom/props {:points (str/join " " (mapv ->px future))
                        :fill "none" :stroke "var(--color-primary-text)"
                        :stroke-width 2 :stroke-dasharray "5 4" :opacity "0.8"})))))))

;; ---------------------------------------------------------------------------
;; Panel sections — separate e/defns so no one method approaches the :prod
;; 64KB bytecode cap (CLAUDE.md).
;; ---------------------------------------------------------------------------

(e/defn StatRow [label value]
  (e/client
    (dom/div
      (dom/props {:class "item-stats-row"})
      (dom/span (dom/props {:class "item-stats-label"}) (dom/text label))
      (dom/span (dom/props {:class "item-stats-value" :dir "auto"}) (dom/text value)))))

(e/defn StatFields
  "The scheduling facts, in Anki's order where an analog exists."
  [stats]
  (e/client
    (let [{:keys [added sources card-kind item-label reviews curve schedule]} stats
          {:keys [state stability difficulty reps lapses due days_since]} schedule
          {:keys [total average timed of]} (time-totals reviews)
          qualify (fn [s] (if (or (zero? timed) (= timed of))
                            s
                            (str s " (over " timed " of " of " reviews)")))
          latest (peek (vec reviews))]
      (dom/div
        (dom/props {:class "item-stats-fields"})
        (StatRow "Added" (str added))
        (StatRow "First Review" (str (:reviewed_at (first reviews) "—")))
        (StatRow "Latest Review" (str (:reviewed_at latest "—")))
        (StatRow "Due" (str (or due "—")))
        (StatRow "Interval" (fmt-days (:scheduled_days latest)))
        ;; Both units, as Anki shows them: the scaled form reads at a glance and the
        ;; raw day count is what the scheduler actually holds.
        (StatRow "Stability"
          (if stability
            (let [d (int (+ 0.5 (double stability)))]
              (str (fmt-days d) (when (>= d 30) (str " (" d " days)"))))
            "—"))
        (StatRow "Difficulty" (if-let [d (difficulty-pct difficulty)] (str d "%") "—"))
        (StatRow "Retrievability"
          (if-let [r (retrievability-pct curve stability days_since)] (str r "%") "—"))
        (StatRow "Reviews" (str (or reps (count reviews))))
        (StatRow "Lapses" (str (or lapses 0)))
        (StatRow "Average Time" (qualify (fmt-secs average)))
        (StatRow "Total Time" (qualify (fmt-secs total)))
        (StatRow "Source" (if (seq sources) (str/join ", " sources) "—"))
        (StatRow "Item kind" (str card-kind))
        (StatRow "Item" (str item-label))))))

(e/defn ReviewHistoryTable
  "Every review of this item, newest first — the inverse of curve-points' order,
   which is oldest-first because it is a time series."
  [reviews]
  (e/client
    (dom/table
      (dom/props {:class "item-stats-history"})
      (dom/thead
        (dom/tr
          (e/for [h (e/diff-by identity ["Date" "Type" "Rating" "Interval" "Ease" "Time"])]
            (dom/th (dom/text h)))))
      (dom/tbody
        ;; Keyed by row id, not by :at_epoch — that is truncated to the second and
        ;; three learning-step reviews inside one second is ordinary.
        (e/for [r (e/diff-by :id (reverse (vec reviews)))]
          (dom/tr
            (dom/td (dom/text (str (:reviewed_at r))))
            (dom/td (dom/text (state-label (:state_before r))))
            (dom/td (dom/text (str (:rating r))))
            (dom/td (dom/text (fmt-days (:scheduled_days r))))
            (dom/td (dom/text (if-let [d (difficulty-pct (:difficulty_after r))]
                                (str "D:" d "%") "—")))
            (dom/td (dom/text (fmt-secs (:duration_ms r))))))))))

(e/defn StatsBody
  "The resolved panel for one item ref.

   `stats` is read in CLIENT scope, so the whole payload crosses at once rather
   than field-by-field through (e/server (:k stats)) — the LibraryCardsView shape
   (CLAUDE.md). That is deliberate here, and it saves nothing to change: :reviews
   is the payload, and curve-points reads every row of it on the client because
   freememo.fsrs is .cljc and the curve is drawn from the epochs. What crosses is
   one item's history, opened by an explicit click, not a windowed table's rows."
  [user-id ref]
  (e/client
    (let [kg-bump (e/server (e/watch (us/get-atom user-id :kg-mutations)))
          stats (e/server (e/Offload #(item-stats* kg-bump user-id ref)))]
      (if (nil? stats)
        (dom/p (dom/props {:class "item-stats-empty"})
          (dom/text "That item is no longer available."))
        (dom/div
          ;; A card's prompt is stored HTML carrying \(TeX\), so it goes through
          ;; math/set-html!; a question's is plain text (CLAUDE.md).
          (if (= :card (:item-kind stats))
            (dom/div (dom/props {:dir "auto" :class "bidi-per-block item-stats-prompt"})
              (math/set-html! dom/node (str (:prompt stats))))
            (dom/p (dom/props {:dir "auto" :class "item-stats-prompt"})
              (dom/text (str (:prompt stats)))))
          (StatFields stats)
          (if (seq (:reviews stats))
            (dom/div
              (ReviewHistoryTable (:reviews stats))
              (let [pts (curve-points (:curve stats) (:reviews stats)
                          ;; Project a quarter past the due date so the crossing of
                          ;; the retention line is inside the frame rather than at
                          ;; its right edge.
                          (let [due (:due_epoch (:schedule stats))
                                last-at (:at_epoch (peek (vec (:reviews stats))))]
                            (if (and due last-at (> due last-at))
                              (+ due (long (* 0.25 (- due last-at))))
                              (+ (or last-at 0) 86400))))]
                (when (seq pts)
                  (ForgettingCurve pts (:desired-retention (:curve stats) 0.9)))))
            (dom/p (dom/props {:class "item-stats-empty"})
              (dom/text "No reviews yet."))))))))

(e/defn OrdPicker
  "Which deletion of a cloze card to show. Only reached when the card has more than
   one — a single live ord resolves without asking."
  [ords !stats card-id]
  (e/client
    (dom/div
      (dom/p (dom/props {:class "item-stats-empty"})
        (dom/text "This card has one schedule per deletion. Which one?"))
      (dom/div
        (dom/props {:style {:display "flex" :gap "6px" :flex-wrap "wrap"}})
        (e/for [ord (e/diff-by identity ords)]
          (dom/button
            (dom/props {:class "btn btn-sm"})
            (dom/text (str "c" ord))
            (dom/On "click" (fn [_] (reset! !stats {:card card-id :ord ord})) nil)))))))

(e/defn QuizItemStatsModal
  "Lifetime stats for whatever `!stats` points at; renders nothing when it is nil.

   `!stats` holds nil | {:question id} | {:card id} | {:card id :ord n}. A card with
   no ord is resolved here against db/card-quiz-ords, so no opener has to know
   whether a card is one quiz item or five.
   Pre:  mounted as a SIBLING of any surrounding modal's overlay, never inside one —
         pointer-events: none inherits, and a nested backdrop is mouse-dead while
         Escape closes both (CLAUDE.md)."
  [user-id !stats]
  (e/client
    (let [stats (e/watch !stats)
          close! (fn [] (reset! !stats nil))]
      (when (some? stats)
        (dom/div
          (dom/props {:class "modal-backdrop" :style {:pointer-events "auto"}})
          (dom/div
            (dom/props {:class "modal-content item-stats-modal"})
            (modal/ModalEscape "Item statistics" close!)
            (dom/div
              (dom/props {:class "item-stats-head"})
              (dom/h3 (dom/props {:style {:margin "0" :font-size "16px"}})
                (dom/text "Statistics"))
              (dom/button
                (dom/props {:class "btn btn-sm" :aria-label "Close statistics"})
                (dom/text "Close")
                (dom/On "click" (fn [_] (close!)) nil)))
            (if-let [qid (:question stats)]
              (StatsBody user-id [:question qid])
              (let [card-id (:card stats)
                    ord (:ord stats)]
                (if (some? ord)
                  (StatsBody user-id [:card card-id ord])
                  ;; Resolve the card to its quiz items. Which ords are live is the
                  ;; database's answer — see db/card-quiz-ords.
                  (let [kg-bump (e/server (e/watch (us/get-atom user-id :kg-mutations)))
                        ords (e/server (e/Offload #(card-ords* kg-bump user-id card-id)))
                        n (e/server (count ords))]
                    (cond
                      (zero? n) (dom/p (dom/props {:class "item-stats-empty"})
                                  (dom/text "This card has no quiz items."))
                      (= 1 n) (StatsBody user-id [:card card-id (e/server (first ords))])
                      :else (OrdPicker (e/server (vec ords)) !stats card-id))))))))))))

(e/defn StatsOpenerButton
  "The per-row affordance that opens the panel.

   Its own cell with stopPropagation, the RejectQuestionButton / DeleteCardButton
   idiom: every other cell of those rows opens an editor, so a shared cell would
   open both.
   Pre:  `target` is the map QuizItemStatsModal reads — {:question id} or {:card id}."
  [!stats target]
  (e/client
    (dom/button
      (dom/props {:class "btn btn-sm" :aria-label "Lifetime statistics"})
      (tooltip/Tooltip! "Statistics — schedule, review history, forgetting curve")
      (icons/Icon :chart-column :size 14)
      (dom/On "click" (fn [e] (.stopPropagation e) (reset! !stats target)) nil))))
