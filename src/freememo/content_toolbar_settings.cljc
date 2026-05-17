(ns freememo.content-toolbar-settings
  "Settings controls for ContentToolbar: context toggle, card type radios, card count stepper.
   Extracted to stay under JVM 64KB method limit."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   #?(:clj [freememo.settings :as settings])))

(e/defn ToolbarSettings [cfg !use-context !context-window !card-type !card-count]
  (e/client
    (let [{:keys [user-id context-tooltip radio-name llm-enabled?
                  use-context context-window card-type card-count-val]} cfg]
      (when llm-enabled?
        ;; Context group
        (dom/div
          (dom/props {:class "toolbar-settings-row"})
          (dom/label
            (dom/props {:style {:display "flex" :align-items "center" :gap "4px"}
                        :title (or context-tooltip "Include context for better cards")})
            (dom/input
              (dom/props {:type "checkbox" :checked use-context})
              (let [change-event (dom/On "change" (fn [e] (-> e .-target .-checked)) nil)
                    [t ?error] (e/Token change-event)]
                (when (some? change-event)
                  (reset! !use-context change-event))
                (when t
                  (let [r (e/server (e/Offload #(settings/save-context-enabled user-id change-event)))]
                    (case r
                      (if (:success r) (t) (t (:error r))))))))
            (dom/text "Context"))
          (dom/input
            (dom/props {:type "number" :min "1" :max "10" :value (str context-window)
                        :disabled (not use-context)
                        :title "Number of previous pages to include (1-10)"
                        :style {:padding "2px 4px" :font-size "13px" :width "40px"
                                :opacity (if use-context "1" "0.5")}})
            (let [input-event (dom/On "change"
                                (fn [e] (let [v (-> e .-target .-value)]
                                          (if (seq v) (js/parseInt v) nil)))
                                nil)
                  [t ?error] (e/Token input-event)]
              (when (some? input-event)
                (reset! !context-window input-event))
              (when t
                (let [r (e/server (e/Offload #(settings/save-context-pages user-id input-event)))]
                  (case r
                    (if (:success r) (t) (t (:error r))))))))
          (dom/span (dom/props {:style {:font-size "11px" :color "var(--color-text-hint)"}}) (dom/text "pages")))

        ;; Separator
        (dom/span (dom/props {:class "toolbar-sep"}) (dom/text "|"))

        ;; Card type group — uses non-focusable spans to avoid stealing
        ;; focus from Quill editor (preserves text selection)
        (dom/div
          (dom/props {:class "toolbar-settings-row"})
          (dom/span
            (dom/props {:style {:display "flex" :align-items "center" :gap "4px" :cursor "pointer"}})
            (dom/On "mousedown" (fn [e] (.preventDefault e)) nil)
            (dom/span
              (dom/props {:class (str "radio-dot" (when (= card-type "basic") " radio-dot--checked"))})
              (dom/text (if (= card-type "basic") "\u25C9" "\u25CB")))
            (e/for [[t e] (dom/On-all "click")]
              (when e
                (reset! !card-type "basic")
                (let [r (e/server (e/Offload #(settings/save-card-type user-id "basic")))]
                  (when (some? r)
                    (if (:success r) (t) (t (:error r)))))))
            (dom/text "Basic"))
          (dom/span
            (dom/props {:style {:display "flex" :align-items "center" :gap "4px" :cursor "pointer"}})
            (dom/On "mousedown" (fn [e] (.preventDefault e)) nil)
            (dom/span
              (dom/props {:class (str "radio-dot" (when (= card-type "cloze") " radio-dot--checked"))})
              (dom/text (if (= card-type "cloze") "\u25C9" "\u25CB")))
            (e/for [[t e] (dom/On-all "click")]
              (when e
                (reset! !card-type "cloze")
                (let [r (e/server (e/Offload #(settings/save-card-type user-id "cloze")))]
                  (when (some? r)
                    (if (:success r) (t) (t (:error r)))))))
            (dom/text "Cloze")))

        ;; Separator
        (dom/span (dom/props {:class "toolbar-sep"}) (dom/text "|"))

        ;; Card count with +/- stepper for touch devices
        (dom/span
          (dom/props {:class "card-count-control"
                      :style {:display "flex" :align-items "center" :gap "4px" :font-size "13px"}})
          (dom/text "#")

          ;; Decrement button (visible on touch only via CSS)
          (dom/button
            (dom/props {:class "card-count-btn"
                        :title "Decrease card count"
                        :style {:width "28px" :height "28px" :border "1px solid var(--color-border)"
                                :border-radius "4px" :background "var(--color-bg-subtle)"
                                :font-size "16px" :cursor "pointer" :display "flex"
                                :align-items "center" :justify-content "center"
                                :padding "0"}})
            (dom/text "\u2212")
            (e/for [[t e] (dom/On-all "click")]
              (when e
                (let [new-val (swap! !card-count (fn [v] (max 1 (dec v))))
                      r (e/server (e/Offload #(settings/save-card-count user-id new-val)))]
                  (when (some? r)
                    (if (:success r) (t) (t (:error r))))))))

          ;; Number input (keyboard suppressed on touch via inputmode)
          (dom/input
            (dom/props {:type "number" :min "1" :max "50" :value (str card-count-val)
                        :inputmode "none"
                        :title "Number of flashcards to generate (1-50)"
                        :class "card-count-input"
                        :style {:padding "2px 4px" :font-size "13px" :width "50px"}})
            (let [input-event (dom/On "change"
                                (fn [e] (let [v (-> e .-target .-value)]
                                          (if (seq v) (js/parseInt v) nil)))
                                nil)
                  [t ?error] (e/Token input-event)]
              (when (some? input-event)
                (reset! !card-count input-event))
              (when t
                (let [r (e/server (e/Offload #(settings/save-card-count user-id input-event)))]
                  (case r
                    (if (:success r) (t) (t (:error r))))))))

          ;; Increment button (visible on touch only via CSS)
          (dom/button
            (dom/props {:class "card-count-btn"
                        :title "Increase card count"
                        :style {:width "28px" :height "28px" :border "1px solid var(--color-border)"
                                :border-radius "4px" :background "var(--color-bg-subtle)"
                                :font-size "16px" :cursor "pointer" :display "flex"
                                :align-items "center" :justify-content "center"
                                :padding "0"}})
            (dom/text "+")
            (e/for [[t e] (dom/On-all "click")]
              (when e
                (let [new-val (swap! !card-count (fn [v] (min 50 (inc v))))
                      r (e/server (e/Offload #(settings/save-card-count user-id new-val)))]
                  (when (some? r)
                    (if (:success r) (t) (t (:error r)))))))))))))
