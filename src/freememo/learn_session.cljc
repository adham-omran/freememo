(ns freememo.learn-session
  "Active learning session — displays topics one at a time with Next advancement."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.logging :as log]
   [freememo.topic-page :refer [TopicPage]]
   [freememo.bibliography-form :as bibform]
   [freememo.keyboard :as keyboard]
   #?(:clj [freememo.db :as db])))

(defn advance-topic* [id]
  #?(:clj (db/advance-topic! id)
     :cljs nil))

(defn update-topic-priority* [id priority]
  #?(:clj (db/update-topic-priority! id priority)
     :cljs nil))

(defn postpone-topic* [id days]
  #?(:clj (db/postpone-topic! id days)
     :cljs nil))

(defn done-topic* [id]
  #?(:clj (db/done-topic! id)
     :cljs nil))

;; Done button — marks topic as fully processed
(e/defn DoneButton [topic-id !queue-idx]
  (e/client
    (dom/button
      (dom/props {:class "btn btn-sm btn-secondary"
                  :style {:padding "4px 10px" :background "transparent"
                          :color "var(--color-success-dark)" :border "1px solid var(--color-success-dark)"}
                  :title "Mark as fully processed (extracted/carded everything useful)"})
      (dom/text "Done")
      (reset! keyboard/!done-btn-ref dom/node)
      (e/on-unmount (fn [] (reset! keyboard/!done-btn-ref nil)))
      (let [event (dom/On "click" (fn [_] (str (random-uuid))) nil)
            [t _error] (e/Token event)]
        (when t
          (e/on-unmount #(swap! !queue-idx inc))
          (case (e/server (e/Offload #(do (done-topic* topic-id) :ok)))
            (t)))))))

;; Shared bottom bar with Postpone + Next
(e/defn BottomBar [topic-id !queue-idx]
  (e/client
    (let [!show-postpone (atom false)
          show-postpone (e/watch !show-postpone)
          !postpone-days (atom "7")]
      (dom/div
        (dom/props {:style {:display "flex" :align-items "center" :justify-content "center"
                            :gap "12px" :padding "6px 16px" :flex-shrink "0"
                            :border-top "1px solid var(--color-border)"}})

        ;; Postpone toggle + input
        (if show-postpone
          (dom/div
            (dom/props {:style {:display "flex" :align-items "center" :gap "8px"}})
            (dom/input
              (dom/props {:type "number" :min "1" :max "365"
                          :value (e/watch !postpone-days)
                          :style {:width "60px" :padding "4px 8px" :font-size "14px"
                                  :border "1px solid var(--color-border)" :border-radius "var(--radius-sm)"}})
              (dom/On "input" (fn [e] (reset! !postpone-days (-> e .-target .-value))) nil))
            (dom/span
              (dom/props {:style {:font-size "13px" :color "var(--color-text-secondary)"}})
              (dom/text "days"))
            (dom/button
              (dom/props {:class "btn btn-primary" :style {:padding "6px 16px"}})
              (dom/text "Go")
              (let [event (dom/On "click"
                            (fn [_]
                              (let [v @!postpone-days]
                                {:id (str (random-uuid))
                                 :days #?(:cljs (js/parseInt v) :clj nil)}))
                            nil)
                    [t _error] (e/Token event)]
                (when t
                  (e/on-unmount #(swap! !queue-idx inc))
                  (let [days (:days event)]
                    (case (e/server (when (and days (pos? days))
                                      (postpone-topic* topic-id days)))
                      (case (e/client (reset! !show-postpone false))
                        (t)))))))
            (dom/button
              (dom/props {:class "btn btn-sm btn-secondary" :style {:padding "6px 12px"}})
              (dom/text "Cancel")
              (dom/On "click" (fn [_] (reset! !show-postpone false)) nil)))

          ;; Collapsed postpone button
          (dom/button
            (dom/props {:class "btn btn-secondary" :style {:padding "8px 20px"}})
            (dom/text "Postpone")
            (dom/On "click" (fn [_] (reset! !show-postpone true)) nil)))

        ;; Next button — disabled briefly after click to prevent skipping during content load
        (let [!busy (atom false)
              busy (e/watch !busy)]
          (dom/button
            (dom/props {:class "btn btn-primary"
                        :disabled busy
                        :style {:padding "8px 28px" :font-size "15px" :font-weight "600"
                                :opacity (if busy "0.5" "1")
                                :cursor (if busy "not-allowed" "pointer")}})
            (dom/text "Next")
            (let [event (dom/On "click" (fn [_] (when-not @!busy (str (random-uuid)))) nil)
                  [t _error] (e/Token event)]
              (when t
                (e/on-unmount #(do (swap! !queue-idx inc)
                                 (js/setTimeout (fn [] (reset! !busy false)) 500)))
                (reset! !busy true)
                (case (e/server (advance-topic* topic-id))
                  (case (e/client (log/log-debug (str "Session advancing idx=" (inc @!queue-idx))))
                    (t)))))))))))

;; Session header bar
(e/defn SessionHeader [item !queue-idx navigate! idx total]
  (e/client
    (let [topic-id (:topics/id item)
          kind (:topics/kind item)
          source-container (:sources/container_title item)
          parent-id (:topics/parent_id item)
          title (or (:topics/title item) "-")
          priority (or (:topics/priority item) 50)
          is-root (nil? parent-id)
          ;; For child items, fetch root topic's title and kind
          root-topic (when parent-id
                       (e/server
                         (let [root-id (db/get-root-topic-id topic-id)]
                           (db/get-topic root-id))))
          root-title (when root-topic (:topics/title root-topic))
          [type-label type-color] (bibform/topic-badge kind source-container)]

      (dom/div
        (dom/props {:class "header-bar" :style {:gap "6px" :padding "2px var(--sp-3)"}})

        ;; Title (no source-click — the side panel handles tree navigation)
        (dom/span
          (dom/props {:style {:color "var(--color-text-secondary)" :font-size "13px"}})
          (dom/text (or root-title title)))

        ;; Type badge
        (dom/span
          (dom/props {:class "type-badge" :style {:padding "2px 8px" :background type-color}})
          (dom/text type-label))

        ;; Priority — inline input (e/for-by isolates the frame)
        (dom/label
          (dom/props {:style {:display "flex" :align-items "center" :gap "4px" :font-size "12px" :color "var(--color-text-secondary)"}
                      :title "Priority (0=highest, 100=lowest)"})
          (dom/text "Priority")
          (e/for-by identity [_k [topic-id]]
            (dom/input
              (dom/props {:type "number" :min "0" :max "100"
                          :style {:width "48px" :font-size "12px" :padding "2px 4px"
                                  :border "1px solid var(--color-border)" :border-radius "var(--radius-sm)" :text-align "center"}})
              (set! (.-value dom/node) (str priority))
              (let [change-event (dom/On "change" #(-> % .-target .-value js/parseInt) nil)
                    [t _] (e/Token change-event)]
                (when t
                  (case (e/server (e/Offload #(do (update-topic-priority* topic-id change-event) :ok)))
                    (t)))))))

        ;; Counter
        (dom/span
          (dom/props {:style {:margin-left "auto" :color "var(--color-text-secondary)" :font-size "13px"}})
          (dom/text (str (inc idx) " / " total)))))))

(e/defn LearnSession [user-id enc-key queue-vec !queue-idx navigate! llm-enabled?]
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
              (dom/props {:style {:font-size "24px" :color "var(--color-text-primary)"}})
              (dom/text "All caught up!"))
            (dom/div
              (dom/props {:style {:font-size "14px" :color "var(--color-text-secondary)"}})
              (dom/text (str "Reviewed " total " topic" (when (not= total 1) "s") ".")))
            (dom/div
              (dom/props {:style {:font-size "13px" :color "var(--color-text-hint)" :margin-top "4px"}})
              (dom/text "Return to Overview to browse your topics."))
            (dom/button
              (dom/props {:class "btn btn-primary" :style {:padding "10px 28px" :font-size "15px" :font-weight "600"}})
              (dom/text "Back to Overview")
              (let [event (dom/On "click" (fn [_] :back) nil)
                    [t _error] (e/Token event)]
                (when t
                  (case (t) (navigate! :learn))))))

          ;; Active topic
          (let [item (nth queue-vec idx nil)
                kind (:topics/kind item)
                topic-id (:topics/id item)
                is-pdf? (= kind "pdf")
                queue-pos (str (inc idx) " / " total)
                item-priority (or (:topics/priority item) 50)
                ;; Queue context only for non-PDFs (per design decision §3.2 = Y).
                ;; For PDFs, the title bar's hamburger + page-stats already crowd the row.
                queue-ctx (when-not is-pdf?
                            {:queue-position queue-pos
                             :priority item-priority
                             :on-priority-change! (fn [new-val]
                                                    (update-topic-priority* topic-id new-val))
                             :origin :learn})]

            (when item
              (dom/div
                (dom/props {:style {:flex "1" :min-height "0" :display "flex" :flex-direction "column" :overflow "hidden"}})
                (dom/div
                  (dom/props {:style {:flex "1" :min-height "0" :overflow "hidden"}})
                  (TopicPage user-id enc-key topic-id
                    (fn [& _] (swap! !queue-idx inc))
                    llm-enabled?
                    queue-ctx))
                (BottomBar topic-id !queue-idx)))))))))
