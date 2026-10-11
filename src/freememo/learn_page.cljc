(ns freememo.learn-page
  "Learn tab — incremental reading with spaced review queue."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.electric-svg3 :as svg]
   [hyperfiddle.electric-scroll0 :refer [Scroll-window Tape]]
   [contrib.data :refer [clamp-left]]
   [freememo.a11y :as a11y]
   [freememo.navigation :as nav]
   [freememo.bibliography-form :as bibform]
   [freememo.loading :as loading]
   [freememo.read-region :as read-region]
   [freememo.route-timing :as route-timing]
   #?(:clj [freememo.user-state :as us])
   [freememo.util :as util]
   #?(:clj [freememo.db :as db])))

;; Queue data helpers

#?(:clj
   (defn- ts->local-date [ts]
     (when ts
       (.toLocalDate (java.time.LocalDateTime/ofInstant (.toInstant ts) (java.time.ZoneId/systemDefault))))))

#?(:clj
   (defn format-due-date
     "Absolute review date for the Learn-tab Date column, e.g. \"Jun 21, 2026\".
      nil when there is no scheduled date (new/done) — caller renders an em dash."
     [next-review-at]
     (when next-review-at
       (.format (ts->local-date next-review-at)
         (java.time.format.DateTimeFormatter/ofPattern "MMM d, yyyy")))))

#?(:clj
   (defn format-due [next-review-at status]
     (cond
       (= status "done") "done"
       (nil? next-review-at) "new"
       :else (let [now (java.time.LocalDate/now)
                   due (ts->local-date next-review-at)
                   days (.between java.time.temporal.ChronoUnit/DAYS now due)]
               (cond
                 (neg? days) (str (- days) "d ago")
                 (zero? days) "today"
                 (= days 1) "tomorrow"
                 (<= days 30) (str "in " days "d")
                 :else (.format due (java.time.format.DateTimeFormatter/ofPattern "MMM d")))))))

(defn prepare-queue-rows
  "Queue rows shaped for the overview table.

   Pre : `user-id` identifies an existing user.
   Post: `{:rows [row …] :error nil}`, or `{:rows [] :error message}`, carrying
         db/get-learning-queue's failure through untouched. The caller MUST
         render `:error` — an empty `:rows` alone does not mean an empty queue."
  [_refresh user-id]
  #?(:clj
     (let [{:keys [rows error]} (db/get-learning-queue user-id)]
       {:error error
        :rows (vec (map (fn [row]
                          (let [kind (:topics/kind row)
                                parent-id (:topics/parent_id row)
                                title (or (:topics/title row) "")
                                ;; 400 characters of ALREADY-STRIPPED text from
                                ;; the content_preview generated column, so
                                ;; extract-preview only truncates here.
                                ;; Qualified key: it is a real column now, not
                                ;; the expression it replaced.
                                content (or (:topics/content_preview row) "")
                                is-root (nil? parent-id)
                                display-title (if is-root
                                                title
                                                (let [preview (util/extract-preview content 80)]
                                                  (if (seq preview) preview title)))]
                            {:id (:topics/id row)
                             :kind kind
                             :display-title display-title
                             :due-label (format-due (:topics/next_review_at row) (or (:topics/status row) "active"))
                             :due-date (format-due-date (:topics/next_review_at row))
                             :status (or (:topics/status row) "active")
                             :source-container (:sources/container_title row)
                             :source-title (:sources/title row)}))
                     rows))})
     :cljs {:rows [] :error nil}))

(defn get-queue-summary* [_refresh user-id]
  #?(:clj (db/get-queue-summary user-id)
     :cljs {:total 0 :inactive 0 :due-today 0 :due-week 0}))

;; Dashboard data — zero-filled 14-day series + scalar stats, server-sited.

#?(:clj
   (defn- weekday-letter [^java.time.LocalDate d]
     (subs (.getDisplayName (.getDayOfWeek d)
             java.time.format.TextStyle/SHORT java.util.Locale/ENGLISH)
       0 1)))

#?(:clj
   (defn- fill-series
     "n maps (oldest→newest) {:label :count :today :date}, gaps zero-filled.
      Day i = today + start-offset + i."
     [start-offset n counts-map]
     (let [today (java.time.LocalDate/now)]
       (mapv (fn [i]
               (let [d (.plusDays today (+ start-offset i))
                     iso (.toString d)]
                 {:label (weekday-letter d)
                  :count (long (get counts-map iso 0))
                  :today (= d today)
                  :date iso}))
         (range n)))))

(defn queue-result* [_refresh user-id]
  #?(:clj
     (read-region/measured-result :learn-queue
       (fn []
       (let [{:keys [rows error] :as queue} (prepare-queue-rows _refresh user-id)]
         (if error
           (throw (ex-info error {:region :learn-queue}))
           (assoc queue :rows (vec rows))))))
     :cljs (loading/region-success {:rows [] :error nil})))

(defn queue-summary-result* [_refresh user-id]
  #?(:clj
     (read-region/measured-result :learn-summary
       #(get-queue-summary* _refresh user-id))
     :cljs (loading/region-success {:total 0 :inactive 0 :due-today 0 :due-week 0})))

(defn studied-series-result* [_refresh user-id]
  #?(:clj
     (read-region/measured-result :learn-studied-series
       (fn []
       (let [studied-map (into {} (map (juxt :d :c) (db/get-study-calendar user-id)))]
         ;; The chart ends yesterday. Today has its own KPI.
         (fill-series -14 14 studied-map))))
     :cljs (loading/region-success [])))

(defn future-due-result* [_refresh user-id]
  #?(:clj
     (read-region/measured-result :learn-future-due
       (fn []
       (let [due-map (into {} (map (fn [row] [(str (:review_date row)) (:count row)])
                                (db/get-review-calendar user-id 30)))]
         {:backlog (db/get-overdue-count user-id)
          :days (fill-series 0 31 due-map)})))
     :cljs (loading/region-success {:backlog 0 :days []})))

(defn learn-kpis-result* [_refresh user-id]
  #?(:clj
     (read-region/measured-result :learn-kpis
       (fn []
       (let [summary (db/get-queue-summary user-id)
             today (.toString (java.time.LocalDate/now))
             studied-today (->> (db/get-study-calendar user-id)
                             (some #(when (= today (:d %)) (:c %)))
                             (or 0))]
         {:due-today (:due-today summary)
          :studied-today studied-today
          :due-week (:due-week summary)
          :streak (db/get-study-streak user-id)
          :reviews (db/get-review-counts user-id)})))
     :cljs (loading/region-success
             {:due-today 0 :studied-today 0 :due-week 0 :streak 0
              :reviews {:all-time 0 :this-week 0}})))

(defn status-breakdown-result* [_refresh user-id]
  #?(:clj
     (read-region/measured-result :learn-status
       #(db/get-status-breakdown user-id))
     :cljs (loading/region-success {:active 0 :done 0 :dismissed 0})))

;; Dashboard UI

(e/defn Bar-chart [data]
  (e/client
    (let [w 300 top 16 plot-h 100 base (+ top plot-h)
          n (max 1 (count data))
          slot (/ w n)
          bw (* slot 0.62)
          maxc (max 1 (reduce max 0 (map :count data)))
          bars (vec (map-indexed
                      (fn [i d]
                        (let [c (:count d)
                              bh (if (zero? c) 2 (* plot-h (/ c maxc)))
                              x (+ (* i slot) (/ (- slot bw) 2))]
                          (assoc d :i i :x x :bh bh :y (- base bh))))
                      data))]
      (svg/svg
        (dom/props {:viewBox (str "0 0 " w " " (+ base 16)) :width "100%"
                    :style {:display "block"}})
        (e/for [bar (e/diff-by :date bars)]
          (svg/g
            (svg/title (dom/text (str (:count bar)
                                   (if (= 1 (:count bar)) " topic on " " topics on ")
                                   (:date bar))))
            (svg/rect
              (dom/props {:x (:x bar) :y (:y bar) :width bw :height (:bh bar) :rx 2
                          :style {:fill (if (:today bar) "var(--color-success)" "var(--color-primary-text)")
                                  :opacity (if (zero? (:count bar)) "0.4" "1")}}))
            ;; Count above each non-zero bar (zeros omitted to avoid clutter).
            (when (pos? (:count bar))
              (svg/text
                (dom/props {:x (+ (:x bar) (/ bw 2)) :y (- (:y bar) 3) :text-anchor "middle"
                            :style {:fill (if (:today bar)
                                            "var(--color-success-dark)"
                                            "var(--color-text-secondary)")
                                    :font-size "8px"
                                    :font-weight (if (:today bar) "700" "500")}})
                (dom/text (str (:count bar)))))
            (svg/text
              (dom/props {:x (+ (:x bar) (/ bw 2)) :y (+ base 12) :text-anchor "middle"
                          :style {:fill (if (:today bar)
                                          "var(--color-success-dark)"
                                          "var(--color-text-secondary)")
                                  :font-size "9px"
                                  :font-weight (if (:today bar) "700" "400")}})
              (dom/text (:label bar)))))))))

(e/defn Chart-card [title data]
  (e/client
    (dom/div (dom/props {:class "dash-card"})
      (dom/div (dom/props {:class "dash-cap"}) (dom/text title))
      (Bar-chart data))))

;; Anki-style Future Due: one backlog bar (overdue), today at day 0, then
;; forward days; a cumulative line (running total) overlays on the right.
(e/defn Future-due-chart [future-due]
  (e/client
    (let [backlog (:backlog future-due)
          days (:days future-due)
          ;; Backlog bar (offset -1) prepended to the day series.
          raw (into [{:label "" :count backlog :backlog true :date "overdue"}] days)
          n (max 1 (count raw))
          w 340 top 16 plot-h 100 base (+ top plot-h)
          slot (/ w n) bw (* slot 0.62)
          maxc (max 1 (reduce max 0 (map :count raw)))
          total (max 1 (reduce + 0 (map :count raw)))
          cums (vec (reductions + (map :count raw)))
          bars (vec (map-indexed
                      (fn [i d]
                        (let [c (:count d)
                              bh (if (zero? c) 2 (* plot-h (/ c maxc)))
                              x (+ (* i slot) (/ (- slot bw) 2))
                              cum (nth cums i)]
                          (assoc d :i i :x x :bh bh :y (- base bh)
                                 :cx (+ x (/ bw 2))
                                 :cy (- base (* plot-h (/ cum total)))
                                 :offset (- i 1))))
                      raw))
          line-pts (apply str (interpose " " (map (fn [b] (str (:cx b) "," (:cy b))) bars)))]
      (svg/svg
        (dom/props {:viewBox (str "0 0 " w " " (+ base 16)) :width "100%"
                    :style {:display "block"}})
        (e/for [bar (e/diff-by :date bars)]
          (svg/g
            (svg/title (dom/text (if (:backlog bar)
                                   (str (:count bar) " overdue (backlog)")
                                   (str (:count bar)
                                        (if (= 1 (:count bar)) " topic on " " topics on ")
                                        (:date bar)))))
            (svg/rect
              (dom/props {:x (:x bar) :y (:y bar) :width bw :height (:bh bar) :rx 2
                          :style {:fill (cond (:backlog bar) "var(--color-warning, #d97706)"
                                              (:today bar) "var(--color-success)"
                                              :else "var(--color-primary-text)")
                                  :opacity (if (zero? (:count bar)) "0.35" "1")}}))
            (when (or (:backlog bar) (zero? (mod (:offset bar) 5)))
              (svg/text
                (dom/props {:x (+ (:x bar) (/ bw 2)) :y (+ base 12) :text-anchor "middle"
                            :style {:fill (if (:today bar)
                                            "var(--color-success-dark)"
                                            "var(--color-text-secondary)")
                                    :font-size "9px"
                                    :font-weight (if (:today bar) "700" "400")}})
                (dom/text (if (:backlog bar) "‹" (str (:offset bar))))))))
        ;; Cumulative running-total line.
        (svg/polyline
          (dom/props {:points line-pts :fill "none"
                      :style {:stroke "var(--color-text-secondary)"
                              :stroke-width "1.5" :opacity "0.7"}}))
        ;; Cumulative total, top-right.
        (svg/text
          (dom/props {:x w :y (+ top 2) :text-anchor "end"
                      :style {:fill "var(--color-text-secondary)" :font-size "9px"}})
          (dom/text (str "Σ " total)))))))

(e/defn Kpi-tile [icon value label]
  (e/client
    (dom/div (dom/props {:class "kpi-tile"})
      (dom/div (dom/props {:class "kpi-value"})
        (when icon (dom/span (dom/props {:class "kpi-icon"}) (dom/text icon)))
        (dom/text value))
      (dom/div (dom/props {:class "kpi-label"}) (dom/text label)))))

(e/defn Legend-item [cls label n]
  (e/client
    (dom/span (dom/props {:class "legend-item"})
      (dom/span (dom/props {:class (str "legend-dot " cls)}))
      (dom/text (str label " " n)))))

(e/defn Status-bar [status]
  (e/client
    (let [{:keys [active done]} status
          total (max 1 (+ active done))
          pct (fn [x] (str (* 100.0 (/ x total)) "%"))]
      (dom/div (dom/props {:class "dash-card"})
        (dom/div (dom/props {:class "dash-cap"}) (dom/text "Topics"))
        (dom/div (dom/props {:class "stat-bar"})
          (dom/div (dom/props {:class "stat-seg seg-active" :style {:width (pct active)}}))
          (dom/div (dom/props {:class "stat-seg seg-done" :style {:width (pct done)}})))
        (dom/div (dom/props {:class "stat-legend"})
          (Legend-item "seg-active" "Active" active)
          (Legend-item "seg-done" "Done" done))))))

(e/defn Teaser-card [title promise]
  (e/client
    (dom/div (dom/props {:class "dash-card teaser-card"})
      (dom/div (dom/props {:class "teaser-head"})
        (dom/span (dom/props {:class "teaser-title"}) (dom/text title))
        (dom/span (dom/props {:class "soon-badge"}) (dom/text "Soon")))
      (dom/div (dom/props {:class "teaser-promise"}) (dom/text promise)))))

(e/defn KpiStrip [kpis]
  (e/client
    (dom/div (dom/props {:class "kpi-strip"})
      (Kpi-tile nil (str (:due-today kpis)) "due today")
      (Kpi-tile nil (str (:studied-today kpis)) "studied today")
      (Kpi-tile nil (str (:due-week kpis)) "due over 7 days")
      (Kpi-tile nil (str (:streak kpis)) "day streak")
      (Kpi-tile nil (str (:this-week (:reviews kpis))) "reviews this week")
      (Kpi-tile nil (str (:all-time (:reviews kpis))) "reviews all-time"))))

(e/defn LearnKpiSkeleton []
  (e/client
    (dom/div (dom/props {:class "kpi-strip" :aria-hidden "true"})
      (e/for [i (e/diff-by identity (range 6))]
        (dom/div (dom/props {:class "kpi-tile learn-kpi-skeleton" :key i})
          (dom/div (dom/props {:class "skeleton learn-kpi-skeleton-value"}))
          (dom/div (dom/props {:class "skeleton learn-kpi-skeleton-label"})))))))

(e/defn LearnChartSkeleton []
  (e/client
    (dom/div
      (dom/props {:class "dash-card learn-chart-skeleton" :aria-hidden "true"})
      (dom/div (dom/props {:class "skeleton learn-chart-skeleton-title"}))
      (dom/div (dom/props {:class "learn-chart-skeleton-plot"})
        (dom/div (dom/props {:class "learn-chart-skeleton-bars"})
          (e/for [[i height]
                  (e/diff-by first
                    (map-indexed vector ["18%" "55%" "30%" "72%" "42%" "64%" "24%" "48%"]))]
            (dom/span
              (dom/props {:class "skeleton learn-chart-skeleton-bar"
                          :style {:height height}
                          :key i}))))))))

(e/defn LearnStatusSkeleton []
  (e/client
    (dom/div
      (dom/props {:class "dash-card learn-status-skeleton" :aria-hidden "true"})
      (dom/div (dom/props {:class "skeleton learn-status-skeleton-title"}))
      (dom/div (dom/props {:class "skeleton learn-status-skeleton-bar"}))
      (dom/div (dom/props {:class "learn-status-skeleton-legend"})
        (e/for [i (e/diff-by identity (range 2))]
          (dom/div (dom/props {:class "learn-status-skeleton-item" :key i})
            (dom/span (dom/props {:class "skeleton learn-status-skeleton-dot"}))
            (dom/span (dom/props {:class "skeleton learn-status-skeleton-label"}))))))))

(e/defn LearnQueueSkeleton []
  (e/client
    (dom/div
      (dom/props {:class "learn-queue-loading" :aria-hidden "true"})
      (e/for [i (e/diff-by identity (range 6))]
        (dom/div
          (dom/props {:class "learn-queue-skeleton-row"
                      :style {:--skeleton-title-width
                              (nth ["58%" "72%" "46%" "64%" "52%" "69%"] i)}
                      :key i})
          (dom/div (dom/props {:class "learn-queue-skeleton-document"})
            (dom/span (dom/props {:class "skeleton learn-queue-skeleton-badge"}))
            (dom/span (dom/props {:class "skeleton learn-queue-skeleton-title"})))
          (dom/span (dom/props {:class "skeleton learn-queue-skeleton-due"}))
          (dom/span (dom/props {:class "skeleton learn-queue-skeleton-date"})))))))

(e/defn DashboardRegionFailure [error height]
  (e/client
    (dom/div
      (dom/props {:class "dash-card progressive-region-failed"
                  :style {:min-height height}
                  :role "status"})
      (dom/text (str "Could not load this section. " error)))))

(e/defn LearnDashboard [revision user-id]
  (e/client
    (dom/div (dom/props {:class "learn-dashboard"})
      (loading/WithLatestRegion [revision user-id :kpis]
        (e/fn []
          (e/server
            (let [refresh (+ (e/watch (us/get-atom user-id :refresh))
                            (e/watch (us/get-atom user-id :queue-mutations)))]
              (e/Offload #(assoc (learn-kpis-result* refresh user-id)
                            :request-key [refresh user-id :kpis])))))
        (e/fn [kpis phase error]
          (dom/div
            (dom/props {:class (when (= phase :refreshing) "progressive-region-refreshing")})
            (KpiStrip kpis)
            (loading/RegionNotice phase error)))
        (e/fn [_phase] (LearnKpiSkeleton))
        (e/fn [error] (DashboardRegionFailure error "77px")))

      (dom/div (dom/props {:class "dash-charts"})
        (loading/WithLatestRegion [revision user-id :studied]
          (e/fn []
            (e/server
              (let [refresh (+ (e/watch (us/get-atom user-id :refresh))
                              (e/watch (us/get-atom user-id :queue-mutations)))]
                (e/Offload #(assoc (studied-series-result* refresh user-id)
                              :request-key [refresh user-id :studied])))))
          (e/fn [studied phase error]
            (dom/div
              (dom/props {:class (str "learn-chart-region"
                                  (when (= phase :refreshing)
                                    " progressive-region-refreshing"))})
              (Chart-card "Studied: last 14 days" studied)
              (loading/RegionNotice phase error)))
          (e/fn [_phase] (LearnChartSkeleton))
          (e/fn [error] (DashboardRegionFailure error "184px")))
        (loading/WithLatestRegion [revision user-id :future-due]
          (e/fn []
            (e/server
              (let [refresh (+ (e/watch (us/get-atom user-id :refresh))
                              (e/watch (us/get-atom user-id :queue-mutations)))]
                (e/Offload #(assoc (future-due-result* refresh user-id)
                              :request-key [refresh user-id :future-due])))))
          (e/fn [future-due phase error]
            (dom/div
              (dom/props {:class (str "learn-chart-region"
                                  (when (= phase :refreshing)
                                    " progressive-region-refreshing"))})
              (dom/div (dom/props {:class "dash-card"})
                (dom/div (dom/props {:class "dash-cap"})
                  (dom/text "Future due: backlog + 30 days"))
                (Future-due-chart future-due))
              (loading/RegionNotice phase error)))
          (e/fn [_phase] (LearnChartSkeleton))
          (e/fn [error] (DashboardRegionFailure error "184px"))))

      (loading/WithLatestRegion [revision user-id :status]
        (e/fn []
          (e/server
            (let [refresh (+ (e/watch (us/get-atom user-id :refresh))
                            (e/watch (us/get-atom user-id :queue-mutations)))]
              (e/Offload #(assoc (status-breakdown-result* refresh user-id)
                            :request-key [refresh user-id :status])))))
        (e/fn [status phase error]
          (dom/div
            (dom/props {:class (when (= phase :refreshing) "progressive-region-refreshing")})
            (Status-bar status)
            (loading/RegionNotice phase error)))
        (e/fn [_phase] (LearnStatusSkeleton))
        (e/fn [error] (DashboardRegionFailure error "94px")))

      (dom/div (dom/props {:class "dash-teasers"})
        (Teaser-card "Total study time" "See how long you actually study.")
        (Teaser-card "Expected study time" "Know how long your queue will take.")))))

(e/defn StartLearningRegion [revision user-id navigate! viewer-nav]
  (e/client
    (dom/div (dom/props {:class "learn-start-region"})
      (loading/WithLatestRegion [revision user-id :queue-summary]
        (e/fn []
          (e/server
            (let [refresh (+ (e/watch (us/get-atom user-id :refresh))
                            (e/watch (us/get-atom user-id :queue-mutations)))]
              (e/Offload #(assoc (queue-summary-result* refresh user-id)
                            :request-key [refresh user-id :queue-summary])))))
        (e/fn [summary phase error]
          (let [due-today (:due-today summary)
                session-active? (= :learn-session (:type viewer-nav))]
            (when (pos? due-today)
              (dom/button
                (dom/props {:class "btn btn-primary learn-start-button"})
                (dom/props {:data-tour "learn-start"})
                (dom/text (if session-active? "Resume Learning" "Start Learning"))
                (dom/On "click"
                  (fn [_]
                    (if session-active?
                      (navigate! :viewer)
                      (navigate! :viewer (nav/nav-learn-session))))
                  nil)))
            (loading/RegionNotice phase error)))
        (e/fn [_phase] (loading/SkeletonBlock "44px"))
        (e/fn [error]
          (dom/div
            (dom/props {:class "progressive-region-failed" :role "status"})
            (dom/text (str "Could not load the learning summary. " error))))))))

(e/defn QueueRow [item i navigate!]
  (e/client
    (let [item-status (or (:status item) "active")
          inactive? (not= item-status "active")
          kind (:kind item)
          due-label (:due-label item)
          due-date (:due-date item)
          display-title (or (:display-title item) "")
          id (:id item)
          source-container (:source-container item)
          [badge-text badge-color] (bibform/topic-badge kind source-container)
          open! (fn [_] (navigate! :viewer (nav/nav-topic id :learn)))]
      (dom/tr
        (dom/props {:class (when (even? i) "row-alt")
                    :role "row"
                    :style {:opacity (if inactive? "0.6" "1")
                            :--order i}})
        (dom/On "click" open! nil)
        (dom/td
          (dom/props {:role "cell" :class "learn-queue-document"})
          (a11y/KeyActivate {:label (str "Open " display-title)} open!)
          (dom/span
            (dom/props {:class "type-badge" :style {:background badge-color :flex-shrink "0"}})
            (dom/text badge-text))
          (dom/span (dom/props {:class "learn-queue-title"}) (dom/text display-title)))
        (dom/td
          (dom/props {:role "cell"
                      :class "learn-queue-due"
                      :style {:color (case due-label
                                       "done" "var(--color-success-dark)"
                                       "var(--color-text-secondary)")}})
          (dom/text due-label))
        (dom/td
          (dom/props {:role "cell" :class "learn-queue-date"})
          (dom/text (or due-date "—")))))))

(e/defn QueueTable [revision user-id navigate!]
  (e/client
    (let [grid-cols "1fr 90px 120px"
          row-height 36
          visible-rows 12]
      (dom/div
        (dom/props {:class "table-frame"
                    :role "table" :aria-label "Due topics"})
        (dom/table
          (dom/props {:role "presentation" :class "learn-queue-header"
                      :style {:--grid-cols grid-cols}})
          (dom/thead
            (dom/props {:role "rowgroup"})
            (dom/tr (dom/props {:role "row"})
              (dom/th (dom/props {:role "columnheader"}) (dom/text "Document"))
              (dom/th (dom/props {:role "columnheader"}) (dom/text "Due"))
              (dom/th (dom/props {:role "columnheader"}) (dom/text "Date")))))
        (loading/WithLatestServerRegion [revision user-id :queue]
          (e/fn []
            (e/server
              (let [refresh (+ (e/watch (us/get-atom user-id :refresh))
                              (e/watch (us/get-atom user-id :queue-mutations)))]
                (e/Offload #(assoc (queue-result* refresh user-id)
                              :request-key [refresh user-id :queue])))))
          (e/fn [queue phase error]
            (let [items-vec (e/server (:rows queue))
                  item-count (e/server (count items-vec))]
              (case (e/snapshot (route-timing/mark-first-useful! :learn :queue))
                (dom/div
                  (dom/props {:role "rowgroup"
                              :class (str "learn-queue-scroll"
                                       (when (= phase :refreshing)
                                         " progressive-region-refreshing"))
                              :style {:max-height (str (* row-height visible-rows) "px")
                                      :overflow-y "auto"
                                      :min-height (if (zero? item-count) "72px" "0")
                                      :position "relative" :scrollbar-gutter "stable"}})
                  (let [[offset limit] (Scroll-window row-height item-count dom/node
                                         {:overquery-factor 2})]
                    (dom/props {:class "tape-scroll"
                                :style {:--count item-count
                                        :--grid-cols grid-cols
                                        :--row-height (str row-height "px")}})
                    (dom/table
                      (dom/props {:role "presentation"})
                      (e/for [i (Tape offset limit)]
                        (when-let [item (e/server (nth items-vec i nil))]
                          (QueueRow item i navigate!)))))
                  (when (zero? item-count)
                    (dom/div
                      (dom/props {:class "learn-queue-empty" :role "status"})
                      (dom/text "Nothing is due.")))
                  (loading/RegionNotice phase error)))))
          (e/fn [_phase] (LearnQueueSkeleton))
          (e/fn [error]
            (dom/div
              (dom/props {:class "learn-queue-failed" :role "status"})
              (dom/text (str "The queue could not load. This does not mean that you are caught up. "
                          error)))))))))

(e/defn LearnOverview [user-id navigate! viewer-nav]
  (e/client
    (let [!revision (atom ::pending)
          revision (e/watch !revision)]
      ;; The client sentinel mounts the shell. The server revision arrives as a
      ;; sibling update and changes only each data region's request key.
      (let [server-revision
            (e/server (+ (e/watch (us/get-atom user-id :refresh))
                       (e/watch (us/get-atom user-id :queue-mutations))))]
        (when (not= server-revision revision)
          (case server-revision (reset! !revision server-revision))))
      ;; The full-width outer div scrolls, so the scrollbar sits at the pane
      ;; edge and not at the edge of the 960px column.
      (dom/div
        (dom/props {:class "learn-page"})
        (dom/div
          (dom/props {:class "page-container"})
          (StartLearningRegion revision user-id navigate! viewer-nav)
          (QueueTable revision user-id navigate!)
          (LearnDashboard revision user-id))))))

(e/defn LearnPage [user-id navigate! viewer-nav]
  (LearnOverview user-id navigate! viewer-nav))
