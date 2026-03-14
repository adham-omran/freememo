(ns electric-starter-app.learn-session
  "Active learning session — displays topics one at a time with Next advancement."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [electric-starter-app.ocr-page :refer [OcrPage]]
   #?(:clj [electric-starter-app.db :as db])))

(defn advance-topic* [topic-type id]
  #?(:clj (db/advance-topic topic-type id)
     :cljs nil))

(e/defn LearnSession [user-id enc-key queue-vec !queue-idx !mode !refresh !nav-target navigate-to-extract!]
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
          (let [item (nth queue-vec idx)
                topic-type (:topic_type item)
                topic-id   (:id item)
                filename   (or (:filename item) "-")
                page-num   (:page_number item)
                priority   (or (:priority item) 50)
                is-doc     (= topic-type "document")
                type-label (cond is-doc "Doc" (= topic-type "extract") "Extract" :else "Page")
                type-color (cond is-doc "#dcfce7" (= topic-type "extract") "#fef3c7" :else "#e0f2fe")]

            ;; Header bar
            (dom/div
              (dom/props {:style {:display "flex" :align-items "center" :gap "12px"
                                  :padding "8px 16px" :flex-shrink "0"
                                  :border-bottom "1px solid #e0e0e0"}})
              (dom/button
                (dom/props {:style {:padding "4px 12px" :background "#f0f0f0" :border "1px solid #ccc"
                                    :border-radius "4px" :cursor "pointer" :font-size "13px"}})
                (dom/text "Overview")
                (dom/On "click" (fn [_] (reset! !queue-idx 0) (reset! !mode :overview)) nil))
              (dom/span
                (dom/props {:style {:color "#555" :font-size "13px"}})
                (dom/text (if is-doc filename (str filename "  p." page-num))))
              (dom/span
                (dom/props {:style {:padding "2px 8px" :border-radius "4px" :font-size "11px"
                                    :font-weight "600" :background type-color}})
                (dom/text type-label))
              (dom/span
                (dom/props {:style {:color "#888" :font-size "12px"}})
                (dom/text (str "P:" priority)))
              (dom/span
                (dom/props {:style {:margin-left "auto" :color "#888" :font-size "13px"}})
                (dom/text (str (inc idx) " / " total))))

            (if is-doc
              ;; Document topic — embed full OcrPage workspace
              (dom/div
                (dom/props {:style {:flex "1" :min-height "0" :display "flex" :flex-direction "column" :overflow "hidden"}})
                (dom/div
                  (dom/props {:style {:flex "1" :min-height "0" :overflow "hidden"}})
                  (let [!nav (atom {:doc-id (:document_id item)})]
                    (OcrPage user-id enc-key !nav)))

                ;; Next button at bottom
                (dom/div
                  (dom/props {:style {:display "flex" :align-items "center" :justify-content "center"
                                      :padding "12px 16px" :flex-shrink "0"
                                      :border-top "1px solid #e0e0e0"}})
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
                        (token))))))

              ;; Extract topic — show content read-only with Open Extract + Next
              (let [content (or (:content item) "")]
                (dom/div
                  (dom/props {:style {:flex "1" :min-height "0" :overflow "auto"
                                      :display "flex" :justify-content "center"
                                      :padding "24px 16px"}})
                  (dom/div
                    (dom/props {:style {:width "100%" :max-width "800px" :line-height "1.6"}})
                    (dom/div
                      (dom/props {:style {:font-size "15px"}})
                      (set! (.-innerHTML dom/node) content))))

                ;; Bottom bar with Open Extract + Next
                (dom/div
                  (dom/props {:style {:display "flex" :align-items "center" :justify-content "center"
                                      :gap "12px" :padding "12px 16px" :flex-shrink "0"
                                      :border-top "1px solid #e0e0e0"}})

                  (when (= topic-type "extract")
                    (dom/button
                      (dom/props {:style {:padding "8px 20px" :background "#f0f0f0" :color "#333"
                                          :border "1px solid #ccc" :border-radius "6px" :cursor "pointer"
                                          :font-size "14px"}})
                      (dom/text "Open Extract")
                      (dom/On "click"
                        (fn [_]
                          (reset! !nav-target {:content-item-id topic-id})
                          (navigate-to-extract!))
                        nil)))

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
                        (token)))))))))))))
