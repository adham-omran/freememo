(ns freememo.content-toolbar-settings
  "Settings controls for ContentToolbar: context toggle, card type radios.
   CardCountStepper is a sibling component, rendered next to Generate so
   the count parameter clusters with the action that consumes it.
   Extracted to stay under JVM 64KB method limit."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   #?(:clj [freememo.settings :as settings])))

;; Pre:  llm-enabled? gates rendering. use-context / context-window are
;;       current values; !use-context / !context-window are the controlling atoms.
;; Post: Renders Context checkbox + numeric page-window input. Mounted inside
;;       the overflow panel so it hides on desktop / shows in the mobile
;;       dropdown via the `.toolbar-overflow-panel-action` wrapper at the call site.
(e/defn ContextSettings [cfg !use-context !context-window]
  (e/client
    (let [{:keys [user-id context-tooltip llm-enabled?
                  use-context context-window]} cfg]
      (when llm-enabled?
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
          (dom/span (dom/props {:style {:font-size "11px" :color "var(--color-text-hint)"}}) (dom/text "pages")))))))

;; Pre:  llm-enabled? gates rendering. card-type is current value; !card-type
;;       is the controlling atom.
;; Post: Renders Basic / Cloze radio dots inline in the toolbar.
;; Note:  uses non-focusable spans to avoid stealing focus from Quill editor
;;        (preserves text selection).
(e/defn ToolbarSettings [cfg _!use-context _!context-window !card-type]
  (e/client
    (let [{:keys [llm-enabled? user-id card-type]} cfg]
      (when llm-enabled?
        (dom/div
          (dom/props {:class "toolbar-settings-row"})
          (dom/span
            (dom/props {:style {:display "flex" :align-items "center" :gap "4px" :cursor "pointer"}})
            (dom/On "mousedown" (fn [e] (.preventDefault e)) nil)
            (dom/span
              (dom/props {:class (str "radio-dot" (when (= card-type "basic") " radio-dot--checked"))})
              (dom/text (if (= card-type "basic") "◉" "○")))
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
              (dom/text (if (= card-type "cloze") "◉" "○")))
            (e/for [[t e] (dom/On-all "click")]
              (when e
                (reset! !card-type "cloze")
                (let [r (e/server (e/Offload #(settings/save-card-type user-id "cloze")))]
                  (when (some? r)
                    (if (:success r) (t) (t (:error r)))))))
            (dom/text "Cloze")))))))

(def ^:private card-count-min 1)
(def ^:private card-count-max 20)

;; Pre:  card-count-val is current persisted count (1..20). !card-count is
;;       the controlling atom (writes here flow to settings/save-card-count).
;; Post: Renders `# −[n]+` stepper. Rendered beside Generate so the count
;;       parameter visually clusters with the action that consumes it.
;; Invariant: both client and server clamp to [1,20]; typed values outside
;;            the range collapse on change (no save fired for an out-of-range
;;            attempt before clamping).
(e/defn CardCountStepper [user-id card-count-val !card-count]
  (e/client
    (dom/span
      (dom/props {:class "card-count-control"
                  :style {:display "flex" :align-items "center" :gap "4px" :font-size "13px"}})
      (dom/text "#")

      ;; Decrement button
      (dom/button
        (dom/props {:class "card-count-btn"
                    :title "Decrease card count"
                    :style {:width "28px" :height "28px" :border "1px solid var(--color-border)"
                            :border-radius "4px" :background "var(--color-bg-subtle)"
                            :font-size "16px" :cursor "pointer" :display "flex"
                            :align-items "center" :justify-content "center"
                            :padding "0"}})
        (dom/text "−")
        (e/for [[t e] (dom/On-all "click")]
          (when e
            (let [new-val (swap! !card-count (fn [v] (max card-count-min (dec v))))
                  r (e/server (e/Offload #(settings/save-card-count user-id new-val)))]
              (when (some? r)
                (if (:success r) (t) (t (:error r))))))))

      ;; Number input — width fits two digits; client clamps to [min,max]
      ;; before saving. HTML5 :max attribute only blocks form submission, not
      ;; free typing — must force-set the DOM `.value` property to the clamped
      ;; value so the displayed digits cannot exceed the range.
      (dom/input
        (dom/props {:type "number" :min (str card-count-min) :max (str card-count-max)
                    :value (str card-count-val)
                    :inputmode "none"
                    :title (str "Number of flashcards to generate ("
                                card-count-min "-" card-count-max ")")
                    :class "card-count-input"
                    :style {:padding "2px 4px" :font-size "13px" :width "36px"}})
        (let [input-node dom/node
              raw-event (dom/On "change"
                          (fn [e] (let [v (-> e .-target .-value)]
                                    (if (seq v) (js/parseInt v) nil)))
                          nil)
              clamped (when (some? raw-event)
                        (max card-count-min (min card-count-max raw-event)))
              [t ?error] (e/Token clamped)]
          (when (some? clamped)
            (set! (.-value input-node) (str clamped))
            (reset! !card-count clamped))
          (when t
            (let [r (e/server (e/Offload #(settings/save-card-count user-id clamped)))]
              (case r
                (if (:success r) (t) (t (:error r))))))))

      ;; Increment button
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
            (let [new-val (swap! !card-count (fn [v] (min card-count-max (inc v))))
                  r (e/server (e/Offload #(settings/save-card-count user-id new-val)))]
              (when (some? r)
                (if (:success r) (t) (t (:error r)))))))))))
