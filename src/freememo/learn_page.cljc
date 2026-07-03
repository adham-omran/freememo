(ns freememo.learn-page
  "Learn tab — incremental reading with spaced review queue."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.electric-svg3 :as svg]
   [hyperfiddle.electric-scroll0 :refer [Scroll-window Tape]]
   [contrib.data :refer [clamp-left]]
   [freememo.navigation :as nav]
   [freememo.a11y :as a11y]
   [freememo.bibliography-form :as bibform]
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

(defn prepare-queue-rows [_refresh user-id]
  #?(:clj
     (let [raw (db/get-learning-queue user-id)]
       (vec (map (fn [row]
                   (let [kind (:topics/kind row)
                         parent-id (:topics/parent_id row)
                         title (or (:topics/title row) "")
                         content (or (:topics/content row) "")
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
              raw)))
     :cljs nil))

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

(defn dashboard-data* [_refresh user-id]
  #?(:clj
     (let [studied-map (into {} (map (juxt :d :c) (db/get-study-calendar user-id)))
           due-map (into {} (map (fn [r] [(str (:review_date r)) (:count r)])
                              (db/get-review-calendar user-id 30)))
           today-iso (.toString (java.time.LocalDate/now))]
       ;; Studied = 14 days ending yesterday (today excluded → "studied today" KPI).
       ;; Future Due (Anki-style) = backlog (overdue) + today (day 0) + next 30 days.
       {:studied (fill-series -14 14 studied-map)
        :future-due {:backlog (db/get-overdue-count user-id)
                     :days (fill-series 0 31 due-map)}
        :studied-today (long (get studied-map today-iso 0))
        :streak (db/get-study-streak user-id)
        :reviews (db/get-review-counts user-id)
        :status (db/get-status-breakdown user-id)})
     :cljs {:studied [] :future-due {:backlog 0 :days []} :studied-today 0 :streak 0
            :reviews {:all-time 0 :this-week 0}
            :status {:active 0 :done 0}}))

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
                                   (if (= 1 (:count bar)) " topic · " " topics · ")
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
                                        (if (= 1 (:count bar)) " topic · " " topics · ")
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

(e/defn Dashboard [dash due-today due-week]
  (e/client
    (dom/div (dom/props {:class "learn-dashboard"})
      (dom/div (dom/props {:class "kpi-strip"})
        (Kpi-tile nil (str due-today) "due today")
        (Kpi-tile nil (str (:studied-today dash)) "studied today")
        (Kpi-tile nil (str due-week) "due over 7 days")
        (Kpi-tile nil (str (:streak dash)) "day streak")
        (Kpi-tile nil (str (:this-week (:reviews dash))) "reviews this week")
        (Kpi-tile nil (str (:all-time (:reviews dash))) "reviews all-time"))
      (dom/div (dom/props {:class "dash-charts"})
        (Chart-card "Studied · last 14 days" (:studied dash))
        (dom/div (dom/props {:class "dash-card"})
          (dom/div (dom/props {:class "dash-cap"}) (dom/text "Future due · backlog + 30 days"))
          (Future-due-chart (:future-due dash))))
      (Status-bar (:status dash))
      (dom/div (dom/props {:class "dash-teasers"})
        (Teaser-card "Total study time" "See how long you actually study.")
        (Teaser-card "Expected study time" "Know how long your queue will take.")))))

(e/defn LearnOverview [user-id navigate! viewer-nav]
  (e/client
    (dom/div
      (dom/props {:class "page-container"
                  :style {:height "100%" :display "flex" :flex-direction "column"}})

      (let [refresh (e/server (+ (e/watch (us/get-atom user-id :refresh))
                                (e/watch (us/get-atom user-id :queue-mutations))))
            summary (e/server (get-queue-summary* refresh user-id))
            items-vec (e/server (prepare-queue-rows refresh user-id))
            item-count (e/server (count items-vec))
            dash (e/server (dashboard-data* refresh user-id))
            due-today (:due-today summary)]

        ;; Header: full-width Start Learning (only when something is due;
        ;; stats now live in the dashboard's KPI tiles below).
        (when (pos? due-today)
          (let [session-active? (= :learn-session (:type viewer-nav))]
            (dom/button
              (dom/props {:class "btn btn-primary"
                          :style {:width "100%" :padding "12px 24px" :font-size "16px"
                                  :font-weight "600" :margin-bottom "16px" :flex-shrink "0"}})
              (dom/text (if session-active? "Resume Learning" "Start Learning"))
              (dom/On "click"
                (fn [_]
                  (if session-active?
                    (navigate! :viewer)
                    (navigate! :viewer (nav/nav-learn-session))))
                nil))))

        (let [grid-cols "1fr 90px 120px"
              row-height 36
              visible-rows 12]
          ;; ONE ARIA table spans the two native tables (header + virtual-
          ;; scrolled body). A physical merge would put the thead inside the
          ;; tape-scroll table, whose --offset translation breaks sticky
          ;; headers — so the natives become role=presentation and explicit
          ;; row/columnheader/cell roles restore the header-cell association
          ;; (WCAG 1.3.1) under this wrapper's role=table.
          (dom/div
            (dom/props {:class "table-frame"
                        :role "table" :aria-label "Due topics"
                        :style {:display "flex" :flex-direction "column" :min-height "0"}})

            ;; Fixed header
            (dom/table
              (dom/props {:role "presentation"
                          :style {:width "100%" :display "grid" :grid-template-columns grid-cols :font-size "13px" :flex-shrink "0"}})
              (dom/thead
                (dom/props {:role "rowgroup" :style {:display "contents"}})
                (let [th-base {:padding "8px 6px" :border-bottom "2px solid var(--color-border)" :font-weight "600" :color "var(--color-text-primary)"}]
                  (dom/tr
                    (dom/props {:role "row" :style {:display "contents"}})
                    (dom/th (dom/props {:role "columnheader" :style (merge th-base {:text-align "left" :padding "8px 10px"})}) (dom/text "Document"))
                    (dom/th (dom/props {:role "columnheader" :style (merge th-base {:text-align "center"})}) (dom/text "Due"))
                    (dom/th (dom/props {:role "columnheader" :style (merge th-base {:text-align "right" :padding "8px 10px"})}) (dom/text "Date"))))))

            ;; Scrollable body
            (dom/div
              (dom/props {:role "rowgroup"
                          :style {:max-height (str (* row-height visible-rows) "px") :overflow-y "auto" :min-height "0" :scrollbar-gutter "stable"}})
              (let [[offset limit] (Scroll-window row-height item-count dom/node {:overquery-factor 1})
                    occluded-height (clamp-left (* row-height (- item-count limit)) 0)]
                (dom/props {:class "tape-scroll"
                            :style {:--offset offset :--row-height (str row-height "px")}})
                (dom/table
                  (dom/props {:role "presentation"
                              :style {:width "100%" :display "grid" :grid-template-columns grid-cols :font-size "13px"}})
                  (if (pos? item-count)
                    (e/for [i (Tape offset limit)]
                      (let [item (e/server (nth items-vec i nil))]
                        (when item
                          (let [item-status (or (:status item) "active")
                                inactive? (not= item-status "active")
                                kind (:kind item)
                                due-label (:due-label item)
                                due-date (:due-date item)
                                display-title (or (:display-title item) "")
                                id (:id item)
                                source-container (:source-container item)
                                [badge-text badge-color] (bibform/topic-badge kind source-container)]
                            (let [open-topic! (fn [_] (navigate! :viewer (nav/nav-topic id :learn)))]
                              (dom/tr
                                (dom/props {:class (when (even? i) "row-alt")
                                            :role "row"
                                            :style {:border-bottom "1px solid var(--color-bg-subtle)" :height (str row-height "px")
                                                    :opacity (if inactive? "0.6" "1")
                                                    :cursor "pointer" :--order (inc i)}})
                                (dom/On "click" open-topic! nil)
                              ;; Document (badge + title). KeyActivate on the
                              ;; td (has a box; the display:contents tr does
                              ;; not) — Enter/Space share open-topic! with the
                              ;; tr's click handler.
                              (dom/td
                                (dom/props {:role "cell"
                                            :style {:padding "4px 10px" :overflow "hidden" :text-overflow "ellipsis"
                                                    :white-space "nowrap" :display "flex" :align-items "center" :gap "8px"}})
                                (a11y/KeyActivate {} open-topic!)
                                (dom/span
                                  (dom/props {:class "type-badge" :style {:background badge-color :flex-shrink "0"}})
                                  (dom/text badge-text))
                                (dom/span
                                  (dom/props {:style {:overflow "hidden" :text-overflow "ellipsis" :white-space "nowrap"}})
                                  (dom/text display-title)))
                              ;; Due (relative label)
                              (dom/td
                                (dom/props {:role "cell"
                                            :style {:padding "4px 6px" :text-align "center" :font-size "12px"
                                                    :color (case due-label "done" "var(--color-success-dark)" "var(--color-text-secondary)")}})
                                (dom/text due-label))
                              ;; Date (absolute next-review date; em dash when unscheduled)
                              (dom/td
                                (dom/props {:role "cell"
                                            :style {:padding "4px 10px" :text-align "right" :font-size "12px"
                                                    :color "var(--color-text-secondary)"}})
                                (dom/text (or due-date "—")))))))))
                    (dom/tr
                      (dom/props {:role "row"})
                      (dom/td
                        (dom/props {:role "cell"
                                    :style {:grid-column "1 / -1" :text-align "center" :padding "24px 12px"
                                            :color "var(--color-text-secondary)" :font-size "13px"}})
                        (dom/text (if (pos? (:total summary))
                                    "Nothing due — you're all caught up."
                                    "No topics yet. Import a document from the Import tab to start learning."))))))
                (dom/div (dom/props {:aria-hidden "true" :style {:height (str occluded-height "px")}}))))))

        (Dashboard dash due-today (:due-week summary))))))

(e/defn LearnPage [user-id navigate! viewer-nav]
  (LearnOverview user-id navigate! viewer-nav))
