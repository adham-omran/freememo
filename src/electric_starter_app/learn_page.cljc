(ns electric-starter-app.learn-page
  "Learn tab — incremental reading with spaced review queue."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.electric-scroll0 :refer [Scroll-window]]
   [contrib.data :refer [clamp-left]]
   [electric-starter-app.learn-session :refer [LearnSession]]
   [electric-starter-app.ocr-page :refer [OcrPage]]
   #?(:clj [electric-starter-app.db :as db])))

#?(:clj (defonce !refresh (atom 0)))

(defn get-learning-queue* [_refresh user-id]
  #?(:clj (vec (db/get-learning-queue user-id))
     :cljs nil))

(defn get-learning-queue-count* [_refresh user-id]
  #?(:clj (db/get-learning-queue-count user-id)
     :cljs 0))

(e/defn LearnBrowse [user-id enc-key doc-id !mode]
  (e/client
    (dom/div
      (dom/props {:style {:height "100%" :display "flex" :flex-direction "column" :overflow "hidden"}})

      ;; Header bar
      (dom/div
        (dom/props {:style {:display "flex" :align-items "center" :gap "12px"
                            :padding "8px 16px" :flex-shrink "0"
                            :border-bottom "1px solid #e0e0e0"}})
        (dom/button
          (dom/props {:style {:padding "4px 12px" :background "#f0f0f0" :border "1px solid #ccc"
                              :border-radius "4px" :cursor "pointer" :font-size "13px"}})
          (dom/text "Back to Overview")
          (dom/On "click" (fn [_] (reset! !mode :overview)) nil))
        (dom/span
          (dom/props {:style {:color "#555" :font-size "14px"}})
          (dom/text "Browsing document")))

      ;; OcrPage workspace
      (dom/div
        (dom/props {:style {:flex "1" :min-height "0" :overflow "hidden"}})
        (let [!nav (atom {:doc-id doc-id})]
          (OcrPage user-id enc-key !nav))))))

(e/defn LearnOverview [user-id !mode]
  (e/client
    (dom/div
      (dom/props {:style {:padding "16px" :max-width "900px" :height "100%" :display "flex" :flex-direction "column"}})

      (let [refresh (e/server (e/watch !refresh))
            due-count (e/server (get-learning-queue-count* refresh user-id))]

        ;; Header with count and Learn button
        (dom/div
          (dom/props {:style {:display "flex" :align-items "center" :gap "16px" :margin-bottom "16px" :flex-shrink "0"}})
          (dom/h2
            (dom/props {:style {:margin "0" :font-size "20px"}})
            (dom/text "Learn"))
          (dom/span
            (dom/props {:style {:color "#888" :font-size "14px"}})
            (dom/text (str due-count " topics due")))
          (when (pos? due-count)
            (dom/button
              (dom/props {:style {:padding "8px 24px" :background "#2563eb" :color "white"
                                  :border "none" :border-radius "6px" :cursor "pointer"
                                  :font-size "15px" :font-weight "600"}})
              (dom/text "Start Learning")
              (dom/On "click" (fn [_] (reset! !mode :session)) nil))))

        (if (pos? due-count)
          ;; Virtual-scrolled list of due topics
          (let [items-vec (e/server (get-learning-queue* refresh user-id))
                item-count (e/server (count items-vec))
                row-height 40]
            (dom/div
              (dom/props {:style {:flex "1" :display "flex" :flex-direction "column" :min-height "0"}})

              ;; Table header
              (dom/table
                (dom/props {:style {:width "100%" :border-collapse "collapse" :font-size "14px" :table-layout "fixed" :flex-shrink "0"}})
                (dom/thead
                  (dom/tr
                    (dom/th (dom/props {:style {:text-align "center" :padding "8px 10px" :border-bottom "2px solid #e0e0e0" :font-weight "600" :color "#444" :width "70px"}}) (dom/text "Type"))
                    (dom/th (dom/props {:style {:text-align "left" :padding "8px 10px" :border-bottom "2px solid #e0e0e0" :font-weight "600" :color "#444" :width "20%"}}) (dom/text "Document"))
                    (dom/th (dom/props {:style {:text-align "center" :padding "8px 10px" :border-bottom "2px solid #e0e0e0" :font-weight "600" :color "#444" :width "60px"}}) (dom/text "Page"))
                    (dom/th (dom/props {:style {:text-align "center" :padding "8px 10px" :border-bottom "2px solid #e0e0e0" :font-weight "600" :color "#444" :width "60px"}}) (dom/text "Pri"))
                    (dom/th (dom/props {:style {:text-align "center" :padding "8px 10px" :border-bottom "2px solid #e0e0e0" :font-weight "600" :color "#444" :width "80px"}}) (dom/text "Interval"))
                    (dom/th (dom/props {:style {:text-align "left" :padding "8px 10px" :border-bottom "2px solid #e0e0e0" :font-weight "600" :color "#444"}}) (dom/text "Content")))))

              ;; Scrollable body
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
                            (let [topic-type (:topic_type item)
                                  filename (or (:filename item) "-")
                                  page-num (:page_number item)
                                  priority (or (:priority item) 50)
                                  interval (or (:interval_days item) 1.0)
                                  content (or (:content item) "")
                                  is-doc (= topic-type "document")
                                  truncated (if is-doc
                                              filename
                                              (if (> (count content) 60)
                                                (str (subs content 0 60) "...")
                                                content))
                                  type-label (if is-doc "Doc" "Extract")
                                  type-color (if is-doc "#dcfce7" "#fef3c7")
                                  interval-str (if (< interval 1.0)
                                                 (str (int (* interval 24)) "h")
                                                 (str (int interval) "d"))]
                              (dom/tr
                                (dom/props {:style {:border-bottom "1px solid #f0f0f0" :height (str row-height "px")}})
                                (dom/td
                                  (dom/props {:style {:padding "8px 10px" :text-align "center" :width "70px"}})
                                  (dom/span
                                    (dom/props {:style {:padding "2px 8px" :border-radius "4px" :font-size "11px"
                                                        :font-weight "600" :background type-color}})
                                    (dom/text type-label)))
                                (dom/td
                                  (dom/props {:style {:padding "8px 10px" :overflow "hidden" :text-overflow "ellipsis" :white-space "nowrap" :width "20%"}})
                                  (dom/text filename))
                                (dom/td
                                  (dom/props {:style {:padding "8px 10px" :text-align "center" :color "#555" :width "60px"}})
                                  (dom/text (if is-doc "-" (str page-num))))
                                (dom/td
                                  (dom/props {:style {:padding "8px 10px" :text-align "center" :color "#555" :width "60px"}})
                                  (dom/text (str priority)))
                                (dom/td
                                  (dom/props {:style {:padding "8px 10px" :text-align "center" :color "#888" :font-size "12px" :width "80px"}})
                                  (dom/text interval-str))
                                (dom/td
                                  (dom/props {:style {:padding "8px 10px" :overflow "hidden" :text-overflow "ellipsis" :white-space "nowrap"}})
                                  (dom/text truncated)))))))))
                  (dom/div (dom/props {:style {:height (str occluded-height "px")}}))))))

          ;; Empty state
          (dom/p
            (dom/props {:style {:color "#888" :font-size "14px" :margin-top "24px"}})
            (dom/text "All caught up! No topics due for review.")))))))

(e/defn LearnPage [user-id enc-key !nav-target navigate-to-extract!]
  (e/client
    (let [!mode (atom :overview)
          mode (e/watch !mode)
          !browse-doc-id (atom nil)
          browse-doc-id (e/watch !browse-doc-id)
          !queue-idx (atom 0)
          ;; Reactive watch — fires when "Open" from PDF Documents sets nav-target
          nav-val (e/watch !nav-target)]

      ;; Consume nav-target reactively: switch to browse mode when set
      (when (and (map? nav-val) (:doc-id nav-val))
        (reset! !browse-doc-id (:doc-id nav-val))
        (reset! !mode :browse)
        (reset! !nav-target nil))

      (case mode
        :overview
        (LearnOverview user-id !mode)

        :browse
        (when browse-doc-id
          (LearnBrowse user-id enc-key browse-doc-id !mode))

        :session
        (let [refresh (e/server (e/watch !refresh))
              queue-vec (e/server (get-learning-queue* refresh user-id))]
          (LearnSession user-id enc-key queue-vec !queue-idx !mode !refresh !nav-target navigate-to-extract!))))))
