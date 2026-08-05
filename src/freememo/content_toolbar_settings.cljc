(ns freememo.content-toolbar-settings
  "Settings controls for ContentToolbar: context toggle, card type radios.
   CardCountStepper is a sibling component, rendered next to Generate so
   the count parameter clusters with the action that consumes it.
   Extracted to stay under JVM 64KB method limit."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.electric-forms5 :as forms]
   [freememo.doc-context :as dctx]
   [freememo.segmented :as seg]
   [freememo.number-stepper :refer [NumberStepper]]
   [freememo.card-count :as cc]
   #?(:clj [freememo.settings :as settings])))

(def ^:private context-pages-min 1)
(def ^:private context-pages-max 10)

;; Pre:  llm-enabled? gates rendering. use-context / context-window are
;;       current values; !use-context / !context-window are the controlling atoms.
;; Post: Renders Context checkbox + numeric page-window input. Mounted inside
;;       the overflow panel so it hides on desktop / shows in the mobile
;;       dropdown via the `.toolbar-overflow-panel-action` wrapper at the call site.
;; Note: Checkbox! is bound to use-context (authoritative) rather than
;;       optimistically written on change — !use-context only advances after
;;       the server confirms, so a failed save leaves the DOM on the prior
;;       (correct) value with no manual rollback.
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
            (e/amb
              (e/for [[t {new-use-context :use-context}] (forms/Checkbox! :use-context use-context)]
                (let [r (e/server (e/Offload #(settings/save-context-enabled user-id new-use-context)))]
                  (case r
                    (if (:success r)
                      (do (reset! !use-context new-use-context) (t))
                      (t (:error r))))))
              (do (dom/text "Include context") (e/amb))))
          (NumberStepper context-window context-pages-min context-pages-max 0
            :context-pages nil "Context pages"
            (if (= 1 context-window) "page" "pages") (not use-context)
            !context-window
            (e/fn [nv] (e/server (e/Offload #(settings/save-context-pages user-id nv))))))))))

;; Pre:  llm-enabled? gates rendering. card-type is current value; !card-type
;;       is the controlling atom.
;; Post: Renders the card-type segmented control as a keyboard-operable
;;       radiogroup (Tab to each option, Enter/Space selects) plus a one-line
;;       description of the selected kind.
;; Note: SaveType is one save path for both input modalities (click and
;;       keyboard) — the token spends on server completion, mirroring the
;;       number-stepper policy.
(e/defn ToolbarSettings []
  (e/client
    (let [llm-enabled? dctx/llm-enabled? user-id dctx/user-id card-type dctx/card-type
          !card-type dctx/!card-type
          SaveType (e/fn [t kind]
                     (e/client
                       (reset! !card-type kind)
                       (let [r (e/server (e/Offload #(settings/save-card-type user-id kind)))]
                         (when (some? r)
                           (if (:success r) (t) (t (:error r)))))))]
      (when llm-enabled?
        (dom/div
          (dom/props {:class "toolbar-settings-row toolbar-type-row"})
          (dom/div
            (dom/props {:class "segmented" :role "radiogroup" :aria-label "Card type"})
            (seg/Segment card-type "basic" "Basic" SaveType)
            (seg/Segment card-type "cloze" "Cloze" SaveType)
            (seg/Segment card-type "overlapping" "Overlapping" SaveType))
          (dom/div
            (dom/props {:class "segmented-description"})
            (dom/text (case card-type
                        "basic" "Question → answer."
                        "cloze" "Fill-in-the-blank deletions."
                        "overlapping" "Chained cloze for lists & sequences."
                        ""))))))))

;; Single source of the 1–20 bounds is freememo.card-count (shared with the
;; format-menu popup); alias locally so the stepper call site stays readable.
(def ^:private card-count-min cc/card-count-min)
(def ^:private card-count-max cc/card-count-max)

;; Pre:  card-count-val is current persisted count (1..20). !card-count is
;;       the controlling atom (writes flow to settings/save-card-count; the
;;       overflow-panel proxy mount reads the same atom and stays in sync).
;; Post: Renders the shared `Cards [−] N [+]` borderless stepper in the
;;       Generate menu. Visible label "Cards"; aria "Cards to generate".
;; Invariant: clamp to [1,20] on both the button and typed-input paths.
(e/defn CardCountStepper [user-id card-count-val !card-count]
  (e/client
    (NumberStepper card-count-val card-count-min card-count-max 0
      :card-count "Cards" "Cards to generate" nil nil
      !card-count
      (e/fn [nv] (e/server (e/Offload #(settings/save-card-count user-id nv)))))))
