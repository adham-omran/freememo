(ns freememo.content-toolbar-settings
  "Settings controls for ContentToolbar: context toggle, card type radios.
   CardCountStepper is a sibling component, rendered next to Generate so
   the count parameter clusters with the action that consumes it.
   Extracted to stay under JVM 64KB method limit."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.doc-context :as dctx]
   [freememo.number-stepper :refer [NumberStepper]]
   #?(:clj [freememo.settings :as settings])))

(def ^:private context-pages-min 1)
(def ^:private context-pages-max 10)

;; Pre:  llm-enabled? gates rendering. use-context / context-window are
;;       current values; !use-context / !context-window are the controlling atoms.
;; Post: Renders Context checkbox + numeric page-window input. Mounted inside
;;       the overflow panel so it hides on desktop / shows in the mobile
;;       dropdown via the `.toolbar-overflow-panel-action` wrapper at the call site.
(e/defn ContextSettings []
  (e/client
    (let [user-id dctx/user-id context-tooltip dctx/context-tooltip llm-enabled? dctx/llm-enabled?
          use-context dctx/use-context context-window dctx/context-window
          !use-context dctx/!use-context !context-window dctx/!context-window]
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
          (NumberStepper context-window context-pages-min context-pages-max
            :context-pages nil "pages" (not use-context)
            !context-window
            (e/fn [nv] (e/server (e/Offload #(settings/save-context-pages user-id nv))))))))))

;; Pre:  llm-enabled? gates rendering. card-type is current value; !card-type
;;       is the controlling atom.
;; Post: Renders Basic / Cloze radio dots inline in the toolbar.
;; Note:  uses non-focusable spans to avoid stealing focus from Quill editor
;;        (preserves text selection).
(e/defn ToolbarSettings []
  (e/client
    (let [llm-enabled? dctx/llm-enabled? user-id dctx/user-id card-type dctx/card-type
          !card-type dctx/!card-type]
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
;;       the controlling atom (writes flow to settings/save-card-count; the
;;       overflow-panel proxy mount reads the same atom and stays in sync).
;; Post: Renders the shared `# [−] N [+]` borderless stepper beside Generate.
;; Invariant: clamp to [1,20] on both the button and typed-input paths.
(e/defn CardCountStepper [user-id card-count-val !card-count]
  (e/client
    (NumberStepper card-count-val card-count-min card-count-max
      :card-count "#" nil nil
      !card-count
      (e/fn [nv] (e/server (e/Offload #(settings/save-card-count user-id nv)))))))
