(ns freememo.learn-session
  "Active learning session — displays topics one at a time with Next advancement."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.logging :as log]
   [freememo.topic-page :refer [TopicPage]]
   [freememo.bibliography-form :as bibform]
   #?(:clj [freememo.user-state :as us])
   #?(:clj [freememo.db :as db])))

#?(:clj
   (defn advance-topic* [user-id id]
     (db/advance-topic! id)
     (swap! (us/get-atom user-id :queue-mutations) inc)))

#?(:clj
   (defn postpone-topic* [user-id id days]
     (db/postpone-topic! id days)
     (swap! (us/get-atom user-id :queue-mutations) inc)))

;; Shared bottom bar with Postpone + Next
(e/defn BottomBar [user-id topic-id !queue-idx]
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
                                      (postpone-topic* user-id topic-id days)))
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
                (case (e/server (advance-topic* user-id topic-id))
                  (case (e/client (log/log-debug (str "Session advancing idx=" (inc @!queue-idx))))
                    (t)))))))))))

(e/defn LearnSession [user-id enc-key queue-vec !queue-idx navigate! llm-enabled?]
  (e/client
    (let [idx (e/watch !queue-idx)
          ;; queue-vec is a server-sited value (form binding in ViewerContent)
          ;; — consume it only inside (e/server ...) so the full queue never
          ;; crosses the wire; one row crosses per review step.
          total (e/server (count queue-vec))
          branch-done? (>= idx total)]
      (dom/div
        (dom/props {:style {:height "100%" :display "flex" :flex-direction "column" :overflow "hidden"}})

        (if branch-done?
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
          (let [item (e/server (nth queue-vec idx nil))
                item-present? (some? item)
                kind (:topics/kind item)
                topic-id (:topics/id item)
                is-pdf? (= kind "pdf")
                ;; :origin :learn marks every learn topic so mobile reading-mode
                ;; (TopicPage) can strip chrome on a phone — PDFs included, since
                ;; a book PDF is the main thing read in a session. on-done!
                ;; (auto-advance on extract-done) stays non-PDF only: PDFs have no
                ;; extract Done button and advance manually via Next, as before.
                queue-ctx (if is-pdf?
                            {:origin :learn}
                            {:origin :learn
                             :on-done! #(swap! !queue-idx inc)})]

            (when item-present?
              (dom/div
                (dom/props {:style {:flex "1" :min-height "0" :display "flex" :flex-direction "column" :overflow "hidden"}})
                (dom/div
                  (dom/props {:style {:flex "1" :min-height "0" :overflow "hidden"}})
                  (TopicPage user-id enc-key topic-id
                    navigate!
                    llm-enabled?
                    queue-ctx))
                (BottomBar user-id topic-id !queue-idx)))))))))
