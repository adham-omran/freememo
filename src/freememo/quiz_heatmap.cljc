(ns freememo.quiz-heatmap
  "The Quiz dashboard's footer: today's line, a year of review activity, and the
   four numbers Anki puts under its graph.

   Every number here comes from kg_reviews and nothing else. The Learn dashboard
   already reports a streak over topic_repetitions — incremental-reading advances —
   which is a different activity that disagrees with this one on most days; the two
   are deliberately not merged, and each names its own source.

   Two windows, on purpose. The grid, the daily average and the days-learned share
   cover a rolling 365 days. The streaks are ALL-TIME, because a 400-day streak
   reported as 365 just because the grid stops there would be visibly wrong to the
   one person who has it — which is also what Anki does.

   The whole grid is built on the SERVER. Calendar arithmetic needs a notion of
   today, and 'today' in this app is server-local everywhere else (CURRENT_DATE in
   every due query); computing it on the client would introduce a second, silently
   different one for a user whose machine is in another timezone."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.tooltip :as tooltip]
   #?(:clj [freememo.db :as db])
   #?(:clj [freememo.user-state :as us])))

(def window-days
  "Days the grid and the two averages cover. 365 rather than a calendar year so the
   rightmost column is always today — a January visit to a year-grid is mostly
   empty."
  365)

(def ^:private week-rows 7)

;; ---------------------------------------------------------------------------
;; Pure — the grid shape. Server-called, but platform-free and testable.
;; ---------------------------------------------------------------------------

(defn bucket-thresholds
  "Quartile cut-points over the NON-ZERO daily counts.

   Quantiles, not fixed counts: a user reviewing 20 a day and one reviewing 400 both
   want the busiest quarter of their own days to read as the darkest. A day with no
   reviews is its own bucket (0) and never enters the quantile.
   Post: an ascending vector of three ints; [1 1 1] when every active day has the
         same count, which collapses to one shade rather than dividing by zero."
  [counts]
  (let [nz (vec (sort (remove zero? counts)))
        n (count nz)]
    (if (zero? n)
      [1 1 1]
      (mapv #(nth nz (min (dec n) (int (* % n)))) [0.25 0.5 0.75]))))

(defn bucket-of
  "Which shade a day's count belongs to.
   Post: 0 for no reviews, else 1..4 ascending with the count."
  [thresholds n]
  (cond
    (nil? n) 0
    (zero? n) 0
    (<= n (nth thresholds 0)) 1
    (<= n (nth thresholds 1)) 2
    (<= n (nth thresholds 2)) 3
    :else 4))

;; ---------------------------------------------------------------------------
;; Server query
;; ---------------------------------------------------------------------------

#?(:clj
   (defn footer-stats*
     "Everything the footer renders, in one round trip.

      One daily query feeds the grid, both averages AND today's line — today is
      simply the row whose day is today, so the tile that used to carry that number
      is redundant and has been removed.

      :cells is the grid in COLUMN-MAJOR order, so the caller renders it straight
      into a 7-row grid with grid-auto-flow: column and needs no calendar arithmetic
      of its own. A leading {:pad i} is the hole before the window's first day, never
      a day with no reviews — a zero day is a cell with :reviews 0. The pads carry an
      index because they are diffed like any other cell and need a stable key.
      :streaks is passed through verbatim, INCLUDING its failure shape — see
      db/kg-review-streaks. Defaulting it to zeros here would report a long streak
      as 0 and hide a database failure behind a plausible number.
      Post: {:cells [{:date :reviews :bucket :today?} | {:pad i}]
             :today {:reviews :new :timed :duration_ms}
             :daily-average n :days-learned n :days-learned-pct n
             :streaks {:current n :longest n} | {:error msg}}."
     [_kg-bump user-id]
     (let [rows (db/fsrs-review-history-daily user-id window-days)
           by-date (into {} (map (juxt #(str (:day %)) identity)) rows)
           today (java.time.LocalDate/now)
           start (.minusDays today (dec window-days))
           ;; Monday-first columns: pad the leading partial week so every ROW is one
           ;; weekday for the whole grid.
           pad (dec (.getValue (.getDayOfWeek start)))
           thresholds (bucket-thresholds (mapv :reviews rows))
           active (count rows)]
       {:cells (into (mapv (fn [i] {:pad i}) (range pad))
                 (map (fn [i]
                        (let [d (.plusDays start i)
                              iso (.toString d)
                              n (long (:reviews (get by-date iso) 0))]
                          {:date iso :reviews n
                           :bucket (bucket-of thresholds n)
                           :today? (= d today)})))
                 (range window-days))
        :today (let [t (get by-date (.toString today))]
                 {:reviews (long (:reviews t 0))
                  :new (long (:new t 0))
                  :timed (long (:timed t 0))
                  :duration_ms (:duration_ms t)})
        ;; Anki's denominator is days in the PERIOD, not days studied — the number
        ;; answers "how much do I do per day", which a days-studied denominator
        ;; would turn into "how much do I do on the days I show up".
        :daily-average (int (+ 0.5 (/ (double (reduce + 0 (map :reviews rows)))
                                     window-days)))
        :days-learned active
        :days-learned-pct (int (+ 0.5 (* 100.0 (/ (double active) window-days))))
        :streaks (db/kg-review-streaks user-id)})))

;; ---------------------------------------------------------------------------
;; Views
;; ---------------------------------------------------------------------------

(defn today-line
  "The footer's first line.
   Post: the count alone when nothing today was timed — duration_ms is NULL for
         every review taken before the column existed, and a 0-minute claim would
         be a lie rather than a gap."
  [{:keys [reviews new timed duration_ms]}]
  (let [intro (when (pos? (or new 0))
                (str ", " new " of them new"))]
    (cond
      (zero? (or reviews 0)) "Nothing reviewed today."
      (or (zero? (or timed 0)) (nil? duration_ms))
      (str "Studied " reviews (if (= 1 reviews) " item" " items") " today" intro)
      :else
      (let [mins (/ (double (int (+ 0.5 (* 100.0 (/ (double duration_ms) 60000.0))))) 100.0)
            per (/ (double (int (+ 0.5 (* 10.0 (/ (double duration_ms) 1000.0 timed))))) 10.0)]
        (str "Studied " reviews (if (= 1 reviews) " item" " items")
          " in " mins " minutes today (" per "s/item"
          (when (not= timed reviews) (str ", over " timed " of " reviews))
          ")" intro)))))

(e/defn HeatmapGrid
  "A year of review activity, one cell per day.

   Divs in a CSS grid rather than SVG rects: the project's instant `data-tooltip`
   is a ::after pseudo-element, which does not render on an SVG child, and the
   hover text is the point of the grid.

   A cell with reviews opens that day's detail — the grid is navigation, not
   decoration. A day with none is inert and says so by carrying no pointer cursor."
  [cells open-day!]
  (e/client
    (dom/div
      (dom/props {:class "quiz-heatmap"})
      ;; Dates are strings and pads are ints, so one key function covers both and
      ;; the two spaces cannot collide.
      (e/for [c (e/diff-by (fn [c] (or (:date c) (:pad c))) cells)]
        (if (nil? (:date c))
          ;; The hole before the window's first day — not a day with no reviews.
          (dom/div (dom/props {:class "quiz-heatmap-pad"}))
          (dom/div
            (dom/props {:class (str "quiz-heatmap-cell quiz-heatmap-b" (:bucket c)
                                 (when (:today? c) " quiz-heatmap-today")
                                 (when (pos? (:reviews c)) " quiz-heatmap-open"))})
            (tooltip/Tooltip! (str (:reviews c)
                                (if (= 1 (:reviews c)) " review · " " reviews · ")
                                (:date c)))
            (when (pos? (:reviews c))
              (dom/On "click" (fn [_] (open-day! (:date c))) nil))))))))

(e/defn FooterStat [label value]
  (e/client
    (dom/div
      (dom/props {:class "quiz-footer-stat"})
      (dom/span (dom/props {:class "quiz-footer-stat-label"}) (dom/text label))
      (dom/span (dom/props {:class "quiz-footer-stat-value"}) (dom/text value)))))

(defn streak-value
  "One streak number for display.
   Pre:  `streaks` is db/kg-review-streaks' return — either both counts or :error.
   Post: \"N days\" on success; \"—\" when the read failed, NEVER \"0 days\", which
         a reader cannot tell from a real broken streak."
  [streaks k]
  (if (:error streaks) "—" (str (get streaks k 0) " days")))

(defn footer-note
  "The line under the numbers, saying what they cover.

   The Review scope is stated because it is not guessable: every figure above reads
   kg_reviews, and a quiz or exam sitting writes kg_answers, which carries no
   duration at all — so an hour spent on an exam appears nowhere here.
   Post: ends with the streak failure text when the streak read failed, so the two
         em-dashes above are explained rather than left looking like zero."
  [streaks]
  (str "Counts Review items only — quiz and exam sittings are not included. "
    "Average and days-learned cover the last " window-days " days; "
    (or (:error streaks) "streaks cover all history.")))

(e/defn QuizFooter
  "Today's line, the year grid, and the four numbers.

   Pre:  `open-day!` takes an ISO date and opens that Review day's detail — the same
         surface the History list's review rows open.
   Post: renders nothing at all when the account has never reviewed. An empty grid
         plus four zeros is noise on a dashboard whose job at that moment is to get
         the user into their first sitting."
  [user-id open-day!]
  (e/client
    (let [kg-bump (e/server (e/watch (us/get-atom user-id :kg-mutations)))
          stats (e/server (e/Offload #(footer-stats* kg-bump user-id)))
          ever? (e/server (pos? (:days-learned stats 0)))
          ;; Two keys, so the whole map crosses; both the values and the failure
          ;; branch are read on the client.
          streaks (e/server (:streaks stats))]
      (when ever?
        (dom/div
          (dom/props {:class "quiz-footer"})
          ;; The tile that used to carry today's count was removed with this line —
          ;; it said strictly less and would have shown the same number twice.
          (dom/p (dom/props {:class "quiz-footer-today"})
            (dom/text (today-line (e/server (:today stats)))))
          (HeatmapGrid (e/server (:cells stats)) open-day!)
          (dom/div
            (dom/props {:class "quiz-footer-stats"})
            (FooterStat "Daily average"
              (str (e/server (:daily-average stats 0)) " / day"))
            (FooterStat "Days learned"
              (str (e/server (:days-learned-pct stats 0)) "%"))
            (FooterStat "Longest streak" (streak-value streaks :longest))
            (FooterStat "Current streak" (streak-value streaks :current)))
          (dom/p (dom/props {:class "quiz-footer-note"})
            (dom/text (footer-note streaks))))))))
