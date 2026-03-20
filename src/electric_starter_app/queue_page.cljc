(ns electric-starter-app.queue-page
  "Queue page — shows all topics (documents + extracts) with summary, calendar heatmap, and virtual-scrolled table."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.electric-scroll0 :refer [Scroll-window Tape]]
   [contrib.data :refer [clamp-left]]
   [electric-starter-app.util :as util]
   #?(:clj [electric-starter-app.db :as db])))

;; Server wrappers
(defn get-full-queue* [user-id]
  #?(:clj (vec (db/get-full-queue user-id)) :cljs nil))

;; Format due date for display
(defn format-due [next-review-at status]
  #?(:clj
     (cond
       (= status "done") "done"
       (= status "done") "done"
       (nil? next-review-at) "new"
       :else (let [now (java.time.LocalDate/now)
                   due (.toLocalDate (.toLocalDateTime next-review-at))
                   days (.between java.time.temporal.ChronoUnit/DAYS now due)]
               (cond
                 (<= days 0) "today"
                 (= days 1) "tomorrow"
                 (<= days 30) (str "in " days "d")
                 :else (.format due (java.time.format.DateTimeFormatter/ofPattern "MMM d")))))
     :cljs nil))

;; Prepare queue rows on server (add formatted fields)
(defn prepare-queue-rows [user-id]
  #?(:clj
     (let [raw (db/get-full-queue user-id)]
       (vec (map (fn [row]
                   (let [topic-type (:topic_type row)
                         title (or (:title row) "")
                         content (or (:content row) "")
                         display-title (if (= topic-type "extract")
                                         (util/extract-preview content 80)
                                         (util/display-name title))]
                     {:topic-type topic-type
                      :id (:id row)
                      :title title
                      :priority (:priority row)
                      :next-review (:next_review_at row)
                      :status (or (:status row) "active")
                      :source-type (:source_type row)
                      :display-title display-title
                      :due-label (format-due (:next_review_at row) (or (:status row) "active"))}))
              raw)))
     :cljs nil))

;; Compute summary counts on server
;; NULL next_review_at means "new/unreviewed" = due now (same logic as db/get-learning-queue-count)
(defn compute-summary [rows]
  #?(:clj
     (let [now (java.time.LocalDate/now)
           week-end (.plusDays now 7)
           active? #(not (#{"done"} (:status %)))]
       {:total (count rows)
        :inactive (count (filter #(#{"done"} (:status %)) rows))
        :due-today (count (filter (fn [r]
                                    (and (active? r)
                                      (or (nil? (:next-review r))
                                          (let [due (.toLocalDate (.toLocalDateTime (:next-review r)))]
                                            (not (.isAfter due now))))))
                            rows))
        :due-week (count (filter (fn [r]
                                   (and (active? r)
                                     (or (nil? (:next-review r))
                                         (let [due (.toLocalDate (.toLocalDateTime (:next-review r)))]
                                           (not (.isAfter due week-end))))))
                           rows))})
     :cljs nil))

;; --- UI Components ---

(e/defn QueueSummary [summary]
  (e/client
    (dom/div
      (dom/props {:style {:display "flex" :gap "16px" :margin-bottom "16px" :flex-shrink "0"}})
      (e/for-by first [item [["Due today" (:due-today summary)]
                             ["Due this week" (:due-week summary)]
                             ["Total topics" (:total summary)]
                             ["Inactive" (:inactive summary)]]]
        (dom/div
          (dom/props {:style {:flex "1" :padding "12px" :background "var(--color-bg-subtle)"
                              :border-radius "var(--radius-md)" :text-align "center"}})
          (dom/div
            (dom/props {:style {:font-size "24px" :font-weight "700" :color "#222"}})
            (dom/text (second item)))
          (dom/div
            (dom/props {:style {:font-size "12px" :color "var(--color-text-secondary)" :margin-top "4px"}})
            (dom/text (first item))))))))

(e/defn QueueTable [items-vec item-count !nav-target navigate!]
  (e/client
    (if (pos? item-count)
      (dom/div
        (dom/props {:style {:flex "1" :display "flex" :flex-direction "column" :min-height "0"}})

        ;; Fixed header
        (let [grid-cols "50px 60px 80px 1fr"]
          (dom/table
            (dom/props {:style {:width "100%" :display "grid" :grid-template-columns grid-cols :font-size "14px" :flex-shrink "0"}})
            (dom/thead
              (dom/props {:style {:display "contents"}})
              (let [th-base {:padding "8px 6px" :border-bottom "2px solid var(--color-border)" :font-weight "600" :color "var(--color-text-primary)"}]
                (dom/tr
                  (dom/props {:style {:display "contents"}})
                  (dom/th (dom/props {:style (merge th-base {:text-align "center"})}) (dom/text "Pri"))
                  (dom/th (dom/props {:style (merge th-base {:text-align "center"})}) (dom/text "Type"))
                  (dom/th (dom/props {:style (merge th-base {:text-align "center"})}) (dom/text "Due"))
                  (dom/th (dom/props {:style (merge th-base {:text-align "left" :padding "8px 10px"})}) (dom/text "Title"))))))

        ;; Scrollable body with virtual scroll
          (let [row-height 36]
            (dom/div
              (dom/props {:style {:flex "1" :overflow-y "auto" :min-height "0"}})
              (let [[offset limit] (Scroll-window row-height item-count dom/node {:overquery-factor 1})
                    occluded-height (clamp-left (* row-height (- item-count limit)) 0)]
                (dom/props {:class "tape-scroll"
                            :style {:--offset offset :--row-height (str row-height "px")}})
                (dom/table
                  (dom/props {:style {:width "100%" :grid-template-columns grid-cols :font-size "14px"}})
                  (e/for [i (Tape offset limit)]
                    (let [item (e/server (nth items-vec i nil))]
                      (when item
                        (let [item-status (or (:status item) "active")
                              inactive? (not= item-status "active")
                              topic-type (:topic-type item)
                              source-type (:source-type item)
                              priority (:priority item)
                              due-label (:due-label item)
                              display-title (or (:display-title item) "")
                              id (:id item)
                            ;; Type badge
                              [badge-text badge-color]
                              (if (= topic-type "extract")
                                ["Ext" "#44C2FF"]
                                (case source-type
                                  "wikipedia" ["Wiki" "#fef3c7"]
                                  "web" ["Web" "#e0f2fe"]
                                  ["PDF" "#dcfce7"]))]
                          (dom/tr
                            (dom/props {:style {:border-bottom "1px solid #f0f0f0" :height (str row-height "px")
                                                :opacity (case item-status "done" "0.6" "1")
                                                :--order (inc i)}})
                          ;; Priority
                            (dom/td
                              (dom/props {:style {:padding "4px 6px" :text-align "center" :color "#555" :font-size "13px"}})
                              (dom/text (if inactive? "\u2014" (str (or priority "")))))
                          ;; Type badge
                            (dom/td
                              (dom/props {:style {:padding "4px 6px" :text-align "center"}})
                              (dom/span
                                (dom/props {:class "type-badge" :style {:background badge-color}})
                                (dom/text badge-text)))
                          ;; Due / status
                            (dom/td
                              (dom/props {:style {:padding "4px 6px" :text-align "center" :font-size "12px"
                                                  :color (case due-label "done" "#16a34a" "#555")}})
                              (dom/text due-label))
                          ;; Title (clickable)
                            (dom/td
                              (dom/props {:style {:padding "4px 10px" :overflow "hidden" :text-overflow "ellipsis"
                                                  :white-space "nowrap" :cursor "pointer"}
                                          :title display-title})
                              (dom/On "mouseenter" (fn [e] (set! (.-textDecoration (.-style (.-target e))) "underline")) nil)
                              (dom/On "mouseleave" (fn [e] (set! (.-textDecoration (.-style (.-target e))) "none")) nil)
                              (dom/text display-title)
                              (dom/On "click"
                                (fn [_]
                                  (if (= topic-type "extract")
                                    (do (reset! !nav-target {:content-item-id id})
                                      (navigate! :extract))
                                    (do (reset! !nav-target {:doc-id id})
                                      (navigate! :learn))))
                                nil))))))))
                (dom/div (dom/props {:style {:height (str occluded-height "px")}})))))))

      (dom/p
        (dom/props {:style {:color "var(--color-text-secondary)" :font-size "14px" :margin-top "24px"}})
        (dom/text "No topics in your queue yet. Import a document from the Import tab \u2014 it will appear here automatically.")))))

(e/defn QueuePage [user-id !nav-target navigate!]
  (e/client
    (dom/div
      (dom/props {:class "page-container"
                  :style {:height "100%" :display "flex" :flex-direction "column"}})

      (let [items-vec (e/server (prepare-queue-rows user-id))
            item-count (e/server (count items-vec))
            summary (e/server (compute-summary items-vec))]
        (QueueSummary summary)
        (QueueTable items-vec item-count !nav-target navigate!)))))
