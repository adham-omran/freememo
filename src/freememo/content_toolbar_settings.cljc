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
   [freememo.a11y :as a11y]
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
          (NumberStepper context-window context-pages-min context-pages-max
            :context-pages nil "Context pages"
            (if (= 1 context-window) "page" "pages") (not use-context)
            !context-window
            (e/fn [nv] (e/server (e/Offload #(settings/save-context-pages user-id nv))))))))))

;; Enter/Space on the focused option, as an On-all transform: returns the
;; event (truthy → enters the fork) for an activation keypress on the element
;; itself, nil otherwise (listen-some drops it). preventDefault stops Space
;; from scrolling the page.
(defn- radio-activation-key [e]
  #?(:cljs (when (a11y/focused-enter-or-space? e)
             (.preventDefault e)
             e)
     :clj nil))

;; One save path for both input modalities (click and keyboard) — the token
;; spends on server completion, mirroring the number-stepper policy.
(e/defn SaveCardType [t user-id !card-type kind]
  (e/client
    (reset! !card-type kind)
    (let [r (e/server (e/Offload #(settings/save-card-type user-id kind)))]
      (when (some? r)
        (if (:success r) (t) (t (:error r)))))))

;; Pre:  current-type is the live card-type; !card-type the controlling atom.
;; Post: one segment of the card-type segmented control — a `role=radio`
;;       button that persists `kind` on click / Enter / Space via SaveCardType.
;; Note: mousedown preventDefault keeps a CLICK from stealing focus off the
;;       Quill editor (preserves text selection); Tab-focus is deliberate and
;;       allowed. Selection is shown by the --selected CSS class, not a glyph.
(e/defn TypeSegment [user-id !card-type current-type kind label]
  (e/client
    (let [selected? (= current-type kind)]
      (dom/button
        (dom/props {:class (str "segmented-option"
                             (when selected? " segmented-option--selected"))
                    :type "button" :role "radio" :tabindex "0"
                    :aria-checked (str selected?)})
        (dom/On "mousedown" (fn [e] (.preventDefault e)) nil)
        (e/for [[t e] (dom/On-all "click")]
          (when e (SaveCardType t user-id !card-type kind)))
        (e/for [[t e] (dom/On-all "keydown" radio-activation-key)]
          (when e (SaveCardType t user-id !card-type kind)))
        (dom/text label)))))

;; Pre:  llm-enabled? gates rendering. card-type is current value; !card-type
;;       is the controlling atom.
;; Post: Renders the card-type segmented control as a keyboard-operable
;;       radiogroup (Tab to each option, Enter/Space selects) plus a one-line
;;       description of the selected kind.
(e/defn ToolbarSettings []
  (e/client
    (let [llm-enabled? dctx/llm-enabled? user-id dctx/user-id card-type dctx/card-type
          !card-type dctx/!card-type]
      (when llm-enabled?
        (dom/div
          (dom/props {:class "toolbar-settings-row toolbar-type-row"})
          (dom/div
            (dom/props {:class "segmented" :role "radiogroup" :aria-label "Card type"})
            (TypeSegment user-id !card-type card-type "basic" "Basic")
            (TypeSegment user-id !card-type card-type "cloze" "Cloze")
            (TypeSegment user-id !card-type card-type "overlapping" "Overlapping"))
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
    (NumberStepper card-count-val card-count-min card-count-max
      :card-count "Cards" "Cards to generate" nil nil
      !card-count
      (e/fn [nv] (e/server (e/Offload #(settings/save-card-count user-id nv)))))))
