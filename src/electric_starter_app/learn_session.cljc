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
              (token)
              (swap! !queue-idx inc))))))))

;; Session header bar
(e/defn SessionHeader [item !queue-idx !mode idx total view-source!]
  (e/client
    (let [topic-type (:topic_type item)
          topic-id (:id item)
          document-id (:document_id item)
          filename (or (:filename item) "-")
          page-num (:page_number item)
          priority (or (:priority item) 50)
          is-doc (= topic-type "document")
          type-label (cond is-doc "Doc" (= topic-type "extract") "Extract" :else "Page")
          type-color (cond is-doc "#dcfce7" (= topic-type "extract") "#fef3c7" :else "#e0f2fe")]

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

        ;; Priority — inline input (e/for-by isolates the frame)
        (dom/label
          (dom/props {:style {:display "flex" :align-items "center" :gap "4px" :font-size "12px" :color "#888"}
                      :title "Priority (0=highest, 100=lowest)"})
          (dom/text "Priority")
          (e/for-by identity [_k [topic-id]]
            (dom/input
              (dom/props {:type "number" :min "0" :max "100"
                          :style {:width "48px" :font-size "12px" :padding "2px 4px"
                                  :border "1px solid #ddd" :border-radius "3px" :text-align "center"}})
              (set! (.-value dom/node) (str priority))
              (let [change-event (dom/On "change" #(-> % .-target .-value js/parseInt) nil)
                    [?token _] (e/Token change-event)]
                (when-some [token ?token]
                  (e/server (update-topic-priority* topic-type topic-id change-event))
                  (token))))))

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
