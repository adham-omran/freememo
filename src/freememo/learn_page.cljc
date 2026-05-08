(ns freememo.learn-page
  "Learn tab — incremental reading with spaced review queue."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.electric-scroll0 :refer [Scroll-window Tape]]
   [contrib.data :refer [clamp-left]]
   [freememo.navigation :as nav]
   #?(:clj [freememo.user-state :as us])
   [freememo.util :as util]
   #?(:clj [freememo.db :as db])))

;; Badge display for topic kinds
(defn kind-badge [kind]
  (case kind
    "pdf" ["PDF" "var(--color-badge-pdf)"]
    "epub" ["EPUB" "var(--color-badge-epub)"]
    ("web" "wikipedia") ["Web" "var(--color-badge-web)"]
    "markdown" ["MD" "var(--color-badge-web)"]
    ["Topic" "var(--color-badge-epub)"]))

;; Queue data helpers

(defn- ts->local-date [ts]
  #?(:clj (when ts
            (.toLocalDate (java.time.LocalDateTime/ofInstant (.toInstant ts) (java.time.ZoneId/systemDefault))))
     :cljs nil))

(defn format-due [next-review-at status]
  #?(:clj
     (cond
       (= status "done") "done"
       (nil? next-review-at) "new"
       :else (let [now (java.time.LocalDate/now)
                   due (ts->local-date next-review-at)
                   days (.between java.time.temporal.ChronoUnit/DAYS now due)]
               (cond
                 (<= days 0) "today"
                 (= days 1) "tomorrow"
                 (<= days 30) (str "in " days "d")
                 :else (.format due (java.time.format.DateTimeFormatter/ofPattern "MMM d")))))
     :cljs nil))

(defn prepare-queue-rows [user-id]
  #?(:clj
     (let [raw (db/get-full-queue user-id)]
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
                      :status (or (:topics/status row) "active")}))
              raw)))
     :cljs nil))

(defn get-queue-summary* [_refresh user-id]
  #?(:clj (db/get-queue-summary user-id)
     :cljs {:total 0 :inactive 0 :due-today 0 :due-week 0}))

(e/defn LearnOverview [user-id navigate! viewer-nav]
  (e/client
    (dom/div
      (dom/props {:class "page-container"
                  :style {:height "100%" :display "flex" :flex-direction "column"}})

      (let [refresh (e/server (e/watch (us/get-atom user-id :refresh)))
            summary (e/server (get-queue-summary* refresh user-id))
            items-vec (e/server (prepare-queue-rows user-id))
            item-count (e/server (count items-vec))
            due-today (:due-today summary)]

        ;; Header: Start Learning + stats
        (dom/div
          (dom/props {:style {:display "flex" :align-items "center" :gap "16px" :margin-bottom "16px" :flex-shrink "0"}})
          (when (pos? due-today)
            (let [session-active? (= :learn-session (:type viewer-nav))]
              (dom/button
                (dom/props {:class "btn btn-primary" :style {:padding "8px 24px" :font-size "15px" :font-weight "600"}})
                (dom/text (if session-active? "Resume Learning" "Start Learning"))
                (dom/On "click"
                  (fn [_]
                    (if session-active?
                      (navigate! :viewer)
                      (navigate! :viewer (nav/nav-learn-session))))
                  nil))))
          (dom/span
            (dom/props {:style {:color "var(--color-text-secondary)" :font-size "14px"}})
            (dom/text (str (:due-today summary) " due today, "
                        (:due-week summary) " this week, "
                        (:total summary) " total, "
                        (:inactive summary) " inactive"))))

        (let [grid-cols "1fr 80px"
              row-height 36
              visible-rows 10]
          (dom/div
            (dom/props {:style {:display "flex" :flex-direction "column" :min-height "0"}})

            ;; Fixed header
            (dom/table
              (dom/props {:style {:width "100%" :display "grid" :grid-template-columns grid-cols :font-size "13px" :flex-shrink "0"}})
              (dom/thead
                (dom/props {:style {:display "contents"}})
                (let [th-base {:padding "8px 6px" :border-bottom "2px solid var(--color-border)" :font-weight "600" :color "var(--color-text-primary)"}]
                  (dom/tr
                    (dom/props {:style {:display "contents"}})
                    (dom/th (dom/props {:style (merge th-base {:text-align "left" :padding "8px 10px"})}) (dom/text "Document"))
                    (dom/th (dom/props {:style (merge th-base {:text-align "center"})}) (dom/text "Due"))))))

            ;; Scrollable body
            (dom/div
              (dom/props {:style {:max-height (str (* row-height visible-rows) "px") :overflow-y "auto" :min-height "0" :scrollbar-gutter "stable"}})
              (let [[offset limit] (Scroll-window row-height item-count dom/node {:overquery-factor 1})
                    occluded-height (clamp-left (* row-height (- item-count limit)) 0)]
                (dom/props {:class "tape-scroll"
                            :style {:--offset offset :--row-height (str row-height "px")}})
                (dom/table
                  (dom/props {:style {:width "100%" :display "grid" :grid-template-columns grid-cols :font-size "13px"}})
                  (if (pos? item-count)
                    (e/for [i (Tape offset limit)]
                      (let [item (e/server (nth items-vec i nil))]
                        (when item
                          (let [item-status (or (:status item) "active")
                                inactive? (not= item-status "active")
                                kind (:kind item)
                                due-label (:due-label item)
                                display-title (or (:display-title item) "")
                                id (:id item)
                                [badge-text badge-color] (kind-badge kind)]
                            (dom/tr
                              (dom/props {:style {:border-bottom "1px solid var(--color-bg-subtle)" :height (str row-height "px")
                                                  :opacity (if inactive? "0.6" "1")
                                                  :cursor "pointer" :--order (inc i)}})
                              (dom/On "click"
                                (fn [_]
                                  (if (= kind "pdf")
                                    (navigate! :viewer (nav/nav-browse-pdf id nil :learn))
                                    (navigate! :viewer (nav/nav-browse-topic id :learn))))
                                nil)
                              ;; Document (badge + title)
                              (dom/td
                                (dom/props {:style {:padding "4px 10px" :overflow "hidden" :text-overflow "ellipsis"
                                                    :white-space "nowrap" :display "flex" :align-items "center" :gap "8px"}})
                                (dom/span
                                  (dom/props {:class "type-badge" :style {:background badge-color :flex-shrink "0"}})
                                  (dom/text badge-text))
                                (dom/span
                                  (dom/props {:style {:overflow "hidden" :text-overflow "ellipsis" :white-space "nowrap"}
                                              :data-tooltip display-title})
                                  (dom/text display-title)))
                              ;; Due
                              (dom/td
                                (dom/props {:style {:padding "4px 6px" :text-align "center" :font-size "12px"
                                                    :color (case due-label "done" "var(--color-success-dark)" "var(--color-text-secondary)")}})
                                (dom/text due-label)))))))
                    (dom/tr
                      (dom/td
                        (dom/props {:style {:grid-column "1 / -1" :text-align "center" :padding "24px 12px"
                                            :color "var(--color-text-secondary)" :font-size "13px"}})
                        (dom/text "No topics yet. Import a document from the Import tab to start learning.")))))
                (dom/div (dom/props {:style {:height (str occluded-height "px")}}))))))))))

(e/defn LearnPage [user-id navigate! viewer-nav]
  (LearnOverview user-id navigate! viewer-nav))
