(ns electric-starter-app.learn-session
  "Active learning session — displays topics one at a time with Next advancement."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [electric-starter-app.ocr-page :refer [OcrPage]]
   [electric-starter-app.extract-page :refer [ExtractPage]]
   #?(:clj [electric-starter-app.db :as db])))

(defn advance-topic* [topic-type id]
  #?(:clj (db/advance-topic topic-type id)
     :cljs nil))

(defn update-topic-priority* [topic-type id priority]
  #?(:clj (db/update-topic-priority topic-type id priority)
     :cljs nil))

(defn postpone-topic* [topic-type id days]
  #?(:clj (db/postpone-topic topic-type id days)
     :cljs nil))

(defn dismiss-topic* [topic-type id]
  #?(:clj (db/dismiss-topic topic-type id)
     :cljs nil))

;; Priority modal — own e/defn for frame isolation
;; !local-priority is the caller's atom — updated on save for immediate UI feedback
(e/defn PriorityModal [!show !local-priority topic-type topic-id]
  (e/client
    (let [!val (atom (str @!local-priority))]
      (dom/div
        (dom/props {:style {:position "fixed" :top "0" :left "0" :width "100%" :height "100%"
                            :background "rgba(0,0,0,0.3)" :display "flex" :align-items "center"
                            :justify-content "center" :z-index "1000"}})
        (dom/On "click" (fn [e]
                          (when (= (.-target e) (.-currentTarget e))
                            (reset! !show false)))
          nil)
        (dom/div
          (dom/props {:style {:background "white" :border-radius "8px" :padding "24px"
                              :width "280px" :box-shadow "0 4px 20px rgba(0,0,0,0.25)"}})
          (dom/h3
            (dom/props {:style {:margin "0 0 16px 0" :font-size "16px"}})
            (dom/text "Set Priority"))
          (dom/p
            (dom/props {:style {:margin "0 0 12px 0" :font-size "13px" :color "#666"}})
            (dom/text "0 = highest, 100 = lowest"))
          (dom/input
            (dom/props {:type "number" :min "0" :max "100"
                        :value (e/watch !val)
                        :style {:width "100%" :padding "8px 12px" :font-size "16px"
                                :border "1px solid #ccc" :border-radius "4px"
                                :text-align "center" :box-sizing "border-box"}})
            (dom/On "input" (fn [e] (reset! !val (-> e .-target .-value))) nil))
          (dom/div
            (dom/props {:style {:display "flex" :gap "8px" :margin-top "16px"}})
            (dom/button
              (dom/props {:style {:flex "1" :padding "8px" :background "#f0f0f0" :border "1px solid #ccc"
                                  :border-radius "4px" :cursor "pointer" :font-size "14px"}})
              (dom/text "Cancel")
              (dom/On "click" (fn [_] (reset! !show false)) nil))
            (dom/button
              (dom/props {:style {:flex "1" :padding "8px" :background "#2563eb" :color "white"
                                  :border "none" :border-radius "4px" :cursor "pointer"
                                  :font-size "14px" :font-weight "600"}})
              (dom/text "Save")
              (let [event (dom/On "click"
                            (fn [_]
                              (let [v @!val]
                                #?(:cljs (js/parseInt v) :clj nil)))
                            nil)
                    [?token _error] (e/Token event)]
                (when-some [token ?token]
                  (when (and (some? event) (>= event 0) (<= event 100))
                    (e/server (update-topic-priority* topic-type topic-id event))
                    (reset! !local-priority event))
                  (reset! !show false)
                  (token))))))))))

;; Dismiss button — isolated e/defn
(e/defn DismissButton [topic-type topic-id !queue-idx]
  (e/client
    (dom/button
      (dom/props {:style {:padding "4px 10px" :background "transparent" :border "1px solid #ccc"
                          :border-radius "4px" :cursor "pointer" :font-size "12px" :color "#888"}
                  :title "Remove from review queue (keep content)"})
      (dom/text "Dismiss")
      (let [event (dom/On "click" (fn [_] :dismiss) nil)
            [?token _error] (e/Token event)]
        (when-some [token ?token]
          (e/server (dismiss-topic* topic-type topic-id))
          (swap! !queue-idx inc)
          (token))))))

;; Shared bottom bar with Postpone + Next
(e/defn BottomBar [topic-type topic-id !queue-idx]
  (e/client
    (let [!show-postpone (atom false)
          show-postpone (e/watch !show-postpone)
          !postpone-days (atom "7")]
      (dom/div
        (dom/props {:style {:display "flex" :align-items "center" :justify-content "center"
                            :gap "12px" :padding "12px 16px" :flex-shrink "0"
                            :border-top "1px solid #e0e0e0"}})

        ;; Postpone toggle + input
        (if show-postpone
          (dom/div
            (dom/props {:style {:display "flex" :align-items "center" :gap "8px"}})
            (dom/input
              (dom/props {:type "number" :min "1" :max "365"
                          :value (e/watch !postpone-days)
                          :style {:width "60px" :padding "4px 8px" :font-size "14px"
                                  :border "1px solid #ccc" :border-radius "4px"}})
              (dom/On "input" (fn [e] (reset! !postpone-days (-> e .-target .-value))) nil))
            (dom/span
              (dom/props {:style {:font-size "13px" :color "#666"}})
              (dom/text "days"))
            (dom/button
              (dom/props {:style {:padding "6px 16px" :background "#f59e0b" :color "white"
                                  :border "none" :border-radius "6px" :cursor "pointer"
                                  :font-size "14px" :font-weight "600"}})
              (dom/text "Go")
              (let [event (dom/On "click"
                            (fn [_]
                              (let [v @!postpone-days]
                                #?(:cljs (js/parseInt v) :clj nil)))
                            nil)
                    [?token _error] (e/Token event)]
                (when-some [token ?token]
                  (when (and (some? event) (pos? event))
                    (e/server (postpone-topic* topic-type topic-id event)))
                  (reset! !show-postpone false)
                  (swap! !queue-idx inc)
                  (token))))
            (dom/button
              (dom/props {:style {:padding "6px 12px" :background "#f0f0f0" :border "1px solid #ccc"
                                  :border-radius "4px" :cursor "pointer" :font-size "13px"}})
              (dom/text "Cancel")
              (dom/On "click" (fn [_] (reset! !show-postpone false)) nil)))

          ;; Collapsed postpone button
          (dom/button
            (dom/props {:style {:padding "8px 20px" :background "#f59e0b" :color "white"
                                :border "none" :border-radius "6px" :cursor "pointer"
                                :font-size "14px"}})
            (dom/text "Postpone")
            (dom/On "click" (fn [_] (reset! !show-postpone true)) nil)))

        ;; Next button
        (dom/button
          (dom/props {:style {:padding "8px 28px" :background "#2563eb" :color "white"
                              :border "none" :border-radius "6px" :cursor "pointer"
                              :font-size "15px" :font-weight "600"}})
          (dom/text "Next")
          (let [event (dom/On "click" (fn [_] :next) nil)
                [?token _error] (e/Token event)]
            (when-some [token ?token]
              (e/server (advance-topic* topic-type topic-id))
              (swap! !queue-idx inc)
              (token))))))))

;; Session header bar
(e/defn SessionHeader [item !queue-idx !mode idx total view-source!]
  (e/client
    (let [topic-type (:topic_type item)
          topic-id (:id item)
          document-id (:document_id item)
          filename (or (:filename item) "-")
          page-num (:page_number item)
          !local-priority (atom (or (:priority item) 50))
          local-priority (e/watch !local-priority)
          is-doc (= topic-type "document")
          type-label (cond is-doc "Doc" (= topic-type "extract") "Extract" :else "Page")
          type-color (cond is-doc "#dcfce7" (= topic-type "extract") "#fef3c7" :else "#e0f2fe")
          !show-priority (atom false)
          show-priority (e/watch !show-priority)]

      ;; Priority modal (rendered outside header flow)
      (when show-priority
        (PriorityModal !show-priority !local-priority topic-type topic-id))

      (dom/div
        (dom/props {:style {:display "flex" :align-items "center" :gap "12px"
                            :padding "8px 16px" :flex-shrink "0"
                            :border-bottom "1px solid #e0e0e0"}})
        ;; Back to Learn
        (dom/button
          (dom/props {:style {:padding "4px 12px" :background "#f0f0f0" :border "1px solid #ccc"
                              :border-radius "4px" :cursor "pointer" :font-size "13px"}})
          (dom/text "\u2190 Back to Learn")
          (dom/On "click" (fn [_] (reset! !queue-idx 0) (reset! !mode :overview)) nil))

        ;; Dismiss
        (DismissButton topic-type topic-id !queue-idx)

        ;; Filename / source link
        (if is-doc
          (dom/span
            (dom/props {:style {:color "#555" :font-size "13px"}})
            (dom/text filename))
          (dom/span
            (dom/props {:style {:color "#2563eb" :font-size "13px" :cursor "pointer"
                                :text-decoration "underline"}
                        :title "View source PDF page"})
            (dom/text (str filename "  p." page-num))
            (dom/On "click"
              (fn [_] (view-source! document-id page-num))
              nil)))

        ;; Type badge
        (dom/span
          (dom/props {:style {:padding "2px 8px" :border-radius "4px" :font-size "11px"
                              :font-weight "600" :background type-color}})
          (dom/text type-label))

        ;; Priority button — opens modal
        (dom/button
          (dom/props {:style {:padding "2px 10px" :background "#f8f8f8" :border "1px solid #ddd"
                              :border-radius "4px" :cursor "pointer" :font-size "12px" :color "#555"}
                      :title "Click to change priority"})
          (dom/text (str "Priority " local-priority))
          (dom/On "click" (fn [_] (reset! !show-priority true)) nil))

        ;; Counter
        (dom/span
          (dom/props {:style {:margin-left "auto" :color "#888" :font-size "13px"}})
          (dom/text (str (inc idx) " / " total)))))))

(e/defn LearnSession [user-id enc-key queue-vec !queue-idx !mode !refresh !nav-target navigate-to-extract! view-source!]
  (e/client
    (let [idx (e/watch !queue-idx)
          total (count queue-vec)]
      (dom/div
        (dom/props {:style {:height "100%" :display "flex" :flex-direction "column" :overflow "hidden"}})

        (if (>= idx total)
          ;; All done
          (dom/div
            (dom/props {:style {:flex "1" :display "flex" :flex-direction "column"
                                :align-items "center" :justify-content "center" :gap "16px"}})
            (dom/div
              (dom/props {:style {:font-size "24px" :color "#444"}})
              (dom/text "All caught up!"))
            (dom/div
              (dom/props {:style {:font-size "14px" :color "#888"}})
              (dom/text (str "Reviewed " total " topic" (when (not= total 1) "s") ".")))
            (dom/button
              (dom/props {:style {:padding "10px 28px" :background "#2563eb" :color "white"
                                  :border "none" :border-radius "6px" :cursor "pointer"
                                  :font-size "15px" :font-weight "600"}})
              (dom/text "Back to Overview")
              (let [event (dom/On "click" (fn [_] :back) nil)
                    [?token _error] (e/Token event)]
                (when-some [token ?token]
                  (e/server (swap! !refresh inc))
                  (reset! !queue-idx 0)
                  (reset! !mode :overview)
                  (token)))))

          ;; Active topic
          (let [item (nth queue-vec idx nil)
                topic-type (:topic_type item)
                topic-id (:id item)
                is-doc (= topic-type "document")]

            (when item
              ;; Header
              (SessionHeader item !queue-idx !mode idx total view-source!)

              ;; Content
              (if is-doc
                (dom/div
                  (dom/props {:style {:flex "1" :min-height "0" :display "flex" :flex-direction "column" :overflow "hidden"}})
                  (dom/div
                    (dom/props {:style {:flex "1" :min-height "0" :overflow "hidden"}})
                    (let [!nav (atom {:doc-id (:document_id item)})]
                      (OcrPage user-id enc-key !nav)))
                  (BottomBar topic-type topic-id !queue-idx))

                (dom/div
                  (dom/props {:style {:flex "1" :min-height "0" :display "flex" :flex-direction "column" :overflow "hidden"}})
                  (dom/div
                    (dom/props {:style {:flex "1" :min-height "0" :overflow "hidden"}})
                    (ExtractPage user-id enc-key topic-id nil nil))
                  (BottomBar topic-type topic-id !queue-idx))))))))))
