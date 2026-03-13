(ns electric-starter-app.queue-page
  "Queue page — shows all content items (extracts) with virtual scrolling."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.electric-scroll0 :refer [Scroll-window]]
   [contrib.data :refer [clamp-left]]
   #?(:clj [electric-starter-app.db :as db])))

(e/defn QueuePage [user-id !nav-target navigate-to-extract!]
  (e/client
    (dom/div
      (dom/props {:style {:padding "16px" :max-width "900px" :height "100%" :display "flex" :flex-direction "column"}})
      (dom/h2
        (dom/props {:style {:margin "0 0 16px 0" :font-size "20px" :flex-shrink "0"}})
        (dom/text "Extracts"))

      (let [items-vec (e/server
                        (let [raw (db/get-all-content-items user-id)]
                          (vec (map (fn [item]
                                      (assoc item :formatted-date
                                        (when-let [ts (:content_items/created_at item)]
                                          (.format (java.time.format.DateTimeFormatter/ofPattern "MMM d")
                                                   (.toLocalDate (.toLocalDateTime ts))))))
                                    raw))))
            item-count (e/server (count items-vec))]

        (if (pos? item-count)
          (dom/div
            (dom/props {:style {:flex "1" :display "flex" :flex-direction "column" :min-height "0"}})

            ;; Fixed header
            (dom/table
              (dom/props {:style {:width "100%" :border-collapse "collapse" :font-size "14px" :table-layout "fixed" :flex-shrink "0"}})
              (dom/thead
                (dom/tr
                  (dom/th (dom/props {:style {:text-align "left" :padding "8px 10px" :border-bottom "2px solid #e0e0e0" :font-weight "600" :color "#444" :width "20%"}}) (dom/text "Document"))
                  (dom/th (dom/props {:style {:text-align "center" :padding "8px 10px" :border-bottom "2px solid #e0e0e0" :font-weight "600" :color "#444" :width "60px"}}) (dom/text "Page"))
                  (dom/th (dom/props {:style {:text-align "left" :padding "8px 10px" :border-bottom "2px solid #e0e0e0" :font-weight "600" :color "#444"}}) (dom/text "Content"))
                  (dom/th (dom/props {:style {:text-align "center" :padding "8px 10px" :border-bottom "2px solid #e0e0e0" :font-weight "600" :color "#444" :width "100px"}}) (dom/text "Date"))
                  (dom/th (dom/props {:style {:padding "8px 10px" :border-bottom "2px solid #e0e0e0" :width "60px"}}) (dom/text "")))))

            ;; Scrollable body with virtual scroll
            (let [row-height 40]
              (dom/div
                (dom/props {:style {:flex "1" :overflow-y "auto" :min-height "0"}})
                (let [[offset limit] (Scroll-window row-height item-count dom/node {:overquery-factor 1})
                      occluded-height (clamp-left (* row-height (- item-count limit)) 0)]
                  (dom/table
                    (dom/props {:style {:width "100%" :border-collapse "collapse" :font-size "14px" :table-layout "fixed"}})
                    (dom/tbody
                      (dom/props {:style {:position "relative" :top (str (* offset row-height) "px")}})
                      (e/for [i (e/diff-by {} (range offset (+ offset limit)))]
                        (let [item (e/server (nth items-vec i nil))]
                          (when item
                            (let [doc-id   (:content_items/document_id item)
                                  page-num (:content_items/page_number item)
                                  content  (or (:content_items/content item) "")
                                  filename (or (:documents/filename item) "—")
                                  date-str (:formatted-date item)
                                  truncated (if (> (count content) 80)
                                              (str (subs content 0 80) "…")
                                              content)]
                              (dom/tr
                                (dom/props {:style {:border-bottom "1px solid #f0f0f0" :height (str row-height "px")}})
                                (dom/td
                                  (dom/props {:style {:padding "8px 10px" :overflow "hidden" :text-overflow "ellipsis" :white-space "nowrap" :width "20%"}})
                                  (dom/text filename))
                                (dom/td
                                  (dom/props {:style {:padding "8px 10px" :text-align "center" :color "#555" :width "60px"}})
                                  (dom/text page-num))
                                (dom/td
                                  (dom/props {:style {:padding "8px 10px" :overflow "hidden" :text-overflow "ellipsis" :white-space "nowrap"}})
                                  (dom/text truncated))
                                (dom/td
                                  (dom/props {:style {:padding "8px 10px" :text-align "center" :color "#888" :font-size "12px" :width "100px"}})
                                  (dom/text (or date-str "")))
                                (dom/td
                                  (dom/props {:style {:padding "8px 6px" :text-align "center" :width "60px"}})
                                  (dom/button
                                    (dom/props {:style {:padding "4px 12px" :background "#007bff" :color "white"
                                                        :border "none" :border-radius "4px" :cursor "pointer"
                                                        :font-size "13px"}})
                                    (dom/text "Go")
                                    (dom/On "click"
                                      (fn [_]
                                        (reset! !nav-target {:content-item-id (:content_items/id item)})
                                        (navigate-to-extract!))
                                      nil))))))))))
                  (dom/div (dom/props {:style {:height (str occluded-height "px")}}))))))

          (dom/p
            (dom/props {:style {:color "#888" :font-size "14px" :margin-top "24px"}})
            (dom/text "No extracts yet.")))))))
