(ns electric-starter-app.learn-session
  "Active learning session — displays topics one at a time with Next advancement."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [electric-starter-app.logging :as log]
   [electric-starter-app.ocr-page :refer [OcrPage]]
   [electric-starter-app.extract-page :refer [ExtractPage]]
   [electric-starter-app.keyboard :as keyboard]
   #?(:clj [electric-starter-app.db :as db])))

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
                          :color "#16a34a" :border "1px solid #16a34a"}
                  :title "Mark as fully processed (extracted/carded everything useful)"})
      (dom/text "Done")
      (reset! keyboard/!done-btn-ref dom/node)
      (e/on-unmount (fn [] (reset! keyboard/!done-btn-ref nil)))
      (let [event (dom/On "click" (fn [_] (str (random-uuid))) nil)
            [?token _error] (e/Token event)]
        (when-some [token ?token]
          (e/server (done-topic* topic-id))
          (token)
          (swap! !queue-idx inc))))))

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
                    [?token _error] (e/Token event)]
                (when-some [token ?token]
                  (when-some [days (:days event)]
                    (when (pos? days)
                      (e/server (postpone-topic* topic-id days))))
                  (reset! !show-postpone false)
                  (token)
                  (swap! !queue-idx inc))))
            (dom/button
              (dom/props {:class "btn btn-sm btn-secondary" :style {:padding "6px 12px"}})
              (dom/text "Cancel")
              (dom/On "click" (fn [_] (reset! !show-postpone false)) nil)))

          ;; Collapsed postpone button
          (dom/button
            (dom/props {:class "btn btn-secondary" :style {:padding "8px 20px"}})
            (dom/text "Postpone")
            (dom/On "click" (fn [_] (reset! !show-postpone true)) nil)))

        ;; Next button
        (dom/button
          (dom/props {:class "btn btn-primary" :style {:padding "8px 28px" :font-size "15px" :font-weight "600"}})
          (dom/text "Next")
          (let [event (dom/On "click" (fn [_] (str (random-uuid))) nil)
                [?token _error] (e/Token event)]
            (when-some [token ?token]
              (e/server (advance-topic* topic-id))
              (token)
              (log/log-debug (str "Session advancing idx=" (inc @!queue-idx)))
              (swap! !queue-idx inc))))))))

;; Badge display for topic kinds
(defn kind-badge [kind]
  (case kind
    "pdf" ["PDF" "#dcfce7"]
    "epub" ["EPUB" "#f3e8ff"]
    ("web" "wikipedia") ["Web" "#e0f2fe"]
    ["Topic" "#f3e8ff"]))

;; Session header bar
(e/defn SessionHeader [item !queue-idx !mode idx total view-source!]
  (e/client
    (let [topic-id (:topics/id item)
          kind (:topics/kind item)
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
          root-kind (when root-topic (:topics/kind root-topic))
          [type-label type-color] (kind-badge kind)]

      (dom/div
        (dom/props {:class "header-bar" :style {:gap "8px"}})
        ;; Back to Learn
        (dom/button
          (dom/props {:class "btn btn-sm btn-secondary"})
          (dom/text "\u2190 Back to Learn")
          (dom/On "click" (fn [_] (reset! !queue-idx 0) (reset! !mode :overview)) nil))

        ;; Done
        (DoneButton topic-id !queue-idx)

        ;; Title / source link
        (if is-root
          (dom/span
            (dom/props {:style {:color "#555" :font-size "13px"}})
            (dom/text title))
          (dom/span
            (dom/props {:style {:color "var(--color-primary)" :font-size "13px" :cursor "pointer"
                                :text-decoration "underline"}
                        :title "View source"})
            (dom/text (or root-title title))
            (when (and view-source! parent-id)
              (dom/On "click"
                (fn [_] (view-source! parent-id nil root-kind))
                nil))))

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
                    [?token _] (e/Token change-event)]
                (when-some [token ?token]
                  (e/server (update-topic-priority* topic-id change-event))
                  (token))))))

        ;; Counter
        (dom/span
          (dom/props {:style {:margin-left "auto" :color "var(--color-text-secondary)" :font-size "13px"}})
          (dom/text (str (inc idx) " / " total)))))))

(e/defn LearnSession [user-id enc-key queue-vec !queue-idx !mode !refresh !nav-target navigate-to-extract! view-source! llm-enabled?]
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
                    [?token _error] (e/Token event)]
                (when-some [token ?token]
                  (e/server (swap! !refresh inc))
                  (reset! !queue-idx 0)
                  (reset! !mode :overview)
                  (token)))))

          ;; Active topic
          (let [item (nth queue-vec idx nil)
                kind (:topics/kind item)
                topic-id (:topics/id item)
                show-pdf? (= kind "pdf")]

            (when item
              ;; Header
              (SessionHeader item !queue-idx !mode idx total view-source!)

              ;; Content
              (if show-pdf?
                (dom/div
                  (dom/props {:style {:flex "1" :min-height "0" :display "flex" :flex-direction "column" :overflow "hidden"}})
                  (dom/div
                    (dom/props {:style {:flex "1" :min-height "0" :overflow "hidden"}})
                    (let [!nav (atom {:topic-id topic-id})]
                      (OcrPage user-id enc-key !nav llm-enabled?)))
                  (BottomBar topic-id !queue-idx))

                (dom/div
                  (dom/props {:style {:flex "1" :min-height "0" :display "flex" :flex-direction "column" :overflow "hidden"}})
                  (dom/div
                    (dom/props {:style {:flex "1" :min-height "0" :overflow "hidden"}})
                    (ExtractPage user-id enc-key topic-id nil nil llm-enabled?))
                  (BottomBar topic-id !queue-idx))))))))))
