(ns freememo.toolbar-generate-dropdown
  "Generate dropdown — replaces the inline Generate + Generate-with-Prompt
   buttons with a single dropdown trigger. Action wiring is reused from
   ToolbarGenerate (which is rendered inside a hidden source wrapper so its
   invoker handles / modal / e/Token paths stay live). Menu items dispatch
   their commands through `freememo.command-bus`, identical to the
   overflow-panel pattern.

   New e/defn lives in its own namespace to stay under the JVM 64KB bytecode
   limit (pattern from `extract_topic_button.cljc`).

   Click-outside-to-close and Escape-to-close are wired via a plain-Clojure
   helper that installs document listeners on mount and returns a cleanup fn —
   the installation must not live in the reactive e/defn body (per CLAUDE.md
   'JS Library Init Side Effects in e/defn')."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.doc-context :as dctx]
   [freememo.content-toolbar-generate :refer [ToolbarGenerate]]
   [freememo.content-toolbar-helpers :as helpers]
   [freememo.card-compare :refer [CardCompareButton]]
   [freememo.content-toolbar-settings :as settings]
   [freememo.icons :as icons]
   [freememo.command-bus :as bus]
   [freememo.toolbar-overflow :as overflow]
   [freememo.tooltip :as tooltip]))

;; Pre:  `!open` is a CLJS atom holding the menu open state.
;;       `trigger-class` / `menu-class` are bare CSS class names (no leading dot).
;; Post: Installs a document keydown listener (Escape → close) and a document
;;       mousedown listener (click outside trigger AND menu → close). Returns a
;;       0-arg cleanup fn that removes both listeners.
;; Invariant: kept as a plain (defn) so the addEventListener calls run exactly
;;            once per (when open …) mount/unmount cycle — Electric's reactive
;;            graph re-evaluates `let` bodies unpredictably (CLAUDE.md
;;            'JS Library Init Side Effects in e/defn').
(defn install-dropdown-listeners! [!open trigger-class menu-class]
  #?(:cljs
     (let [trigger-sel (str "." trigger-class)
           menu-sel (str "." menu-class)
           on-key (fn [e]
                    (when (= (.-key e) "Escape")
                      (reset! !open false)))
           on-mouse (fn [e]
                      (let [target (.-target e)]
                        (when-not (or (.closest target menu-sel)
                                    (.closest target trigger-sel))
                          (reset! !open false))))
           ;; Pin the menu fixed under the trigger so .toolbar's overflow-x:clip
           ;; can't cut it off horizontally (see install-dropdown-menu-position!).
           unposition (overflow/install-dropdown-menu-position! trigger-class menu-class)]
       (.addEventListener js/document "keydown" on-key)
       (.addEventListener js/document "mousedown" on-mouse)
       (fn []
         (.removeEventListener js/document "keydown" on-key)
         (.removeEventListener js/document "mousedown" on-mouse)
         (unposition)))
     :clj (fn [] nil)))

;; !use-context / !context-window / !card-type are passed positionally (atoms
;; cannot ride inside the serialized cfg map). They feed the settings widgets'
;; own dom-event tokens — NOT cfg-derived values — so no reactive loop arises
;; when cfg changes (e.g. gen-active? flips); see CLAUDE.md token-input rule.
(e/defn GenerateDropdown []
  (e/client
    (let [user-id dctx/user-id llm-enabled? dctx/llm-enabled? gen-active? dctx/gen-active?
          gen-pending dctx/gen-pending gen-error dctx/gen-error
          card-type dctx/card-type card-count-val dctx/card-count-val use-context dctx/use-context
          context-window dctx/context-window context-tooltip dctx/context-tooltip
          content-text dctx/content-text mod-key dctx/mod-key
          !use-context dctx/!use-context !context-window dctx/!context-window !card-type dctx/!card-type
          !card-count dctx/!card-count
          !open (atom false)
          open (e/watch !open)
          no-content? (empty? content-text)
          ;; idle-label = visible primary text ("Generate N Type"); action-summary
          ;; = full text incl. context, used for tooltip/aria only.
          idle-label (helpers/gen-label card-count-val card-type)
          action-summary (helpers/gen-summary card-count-val card-type use-context context-window)]

      ;; Hidden source — ToolbarGenerate renders the real Generate +
      ;; Generate-with-Prompt buttons (which set keyboard refs), the e/Token
      ;; processors, and the PromptDialog modal. The wrapper has
      ;; `display: contents` so it doesn't take layout space; a direct-child
      ;; rule hides the inline buttons but leaves the modal (a div) visible.
      (dom/div
        (dom/props {:class "toolbar-dropdown-sources"})
        (ToolbarGenerate)
        (CardCompareButton))

      ;; Visible dropdown — gated on llm-enabled so the trigger disappears
      ;; when LLM is unavailable (same gating as the original buttons).
      (when llm-enabled?
        (dom/div
          (dom/props {:class "toolbar-dropdown toolbar-generate-dropdown"})

          ;; Trigger — split button: the primary zone generates now (dispatches
          ;; :generate, reusing the hidden ToolbarGenerate button so selection
          ;; capture + enqueue stay single-owner); the caret zone opens the
          ;; options menu. The primary mirrors generation state (spinner +
          ;; pending count in-flight, error text on failure).
          (dom/div
            (dom/props {:class "toolbar-generate-split"})

            ;; Primary zone — generate now.
            (dom/button
              (dom/props {:class "btn btn-sm btn-primary toolbar-generate-trigger toolbar-generate-primary"
                          :style {:background (cond no-content? "var(--color-disabled-bg)"
                                                gen-active? "var(--color-primary-light)"
                                                :else "var(--color-primary)")
                                  :cursor (if no-content? "not-allowed" "pointer")
                                  :font-weight "bold"}
                          :disabled no-content?
                          :aria-label (str action-summary " (" mod-key "+Shift+G)")})
              (tooltip/Tooltip! (if no-content?
                                  "Extract text first to generate flashcards"
                                  (str action-summary " (" mod-key "+Shift+G)")))
              (if (and gen-active? (nil? gen-error))
                (icons/Icon :loader-2 :size 16 :class "spin")
                (icons/Icon :sparkles :size 16))
              (dom/span (dom/props {:class "icon-label"})
                (dom/text (cond gen-error   gen-error
                            gen-active? (str "Generating… (" gen-pending ")")
                            :else       idle-label)))
              (dom/On "click"
                (fn [e]
                  (.stopPropagation e)
                  (when-not no-content?
                    (bus/dispatch! :generate)))
                nil))

            ;; Caret zone — open/close the options menu.
            (dom/button
              (dom/props {:class "btn btn-sm btn-primary toolbar-generate-trigger toolbar-generate-caret"
                          :style {:background (cond no-content? "var(--color-disabled-bg)"
                                                gen-active? "var(--color-primary-light)"
                                                :else "var(--color-primary)")
                                  :cursor (if no-content? "not-allowed" "pointer")}
                          :disabled no-content?
                          :aria-haspopup "menu"
                          :aria-expanded (if open "true" "false")
                          :aria-label "Generation options"})
              (tooltip/Tooltip! "Generation options")
              (icons/Icon :chevron-down :size 14)
              (dom/On "click"
                (fn [e]
                  (.stopPropagation e)
                  (when-not no-content?
                    (swap! !open not)))
                nil)))

          ;; Menu — mounts only when open. Listeners are installed inside a
          ;; `let` whose mount lifecycle matches the menu's; e/on-unmount
          ;; removes them when the menu closes.
          (when open
            (let [cleanup (install-dropdown-listeners!
                            !open
                            "toolbar-generate-split"
                            "toolbar-generate-menu")]
              (e/on-unmount cleanup)
              (dom/div
                (dom/props {:class "toolbar-dropdown-menu toolbar-generate-menu"
                            :role "menu"})

                ;; Generation settings (C3): card-count + card-type + Context live
                ;; here as live widgets above the actions. Clicking them stays
                ;; inside the menu (install-dropdown-listeners! keeps it open).
                ;; Card-count is ALSO shown inline in the toolbar until it collapses
                ;; (tier 4); both share !card-count, so they stay synced.
                (dom/div
                  (dom/props {:class "toolbar-generate-menu-settings"})
                  (settings/CardCountStepper user-id card-count-val !card-count)
                  (settings/ToolbarSettings)
                  (settings/ContextSettings))
                (dom/div (dom/props {:class "toolbar-dropdown-separator"}))

                ;; Generate with Prompt
                (dom/button
                  (dom/props {:class "toolbar-dropdown-item"
                              :role "menuitem"
                              :disabled no-content?
                              :aria-label "Generate with Prompt"})
                  (icons/Icon :pen-sparkles :size 16)
                  (dom/span (dom/text "Generate with prompt..."))
                  (dom/On "click"
                    (fn [_]
                      (bus/dispatch! :gen-prompt)
                      (reset! !open false))
                    nil))

                ;; Compare models — generate the same content with several
                ;; models and pick the best set (bills per model).
                (dom/button
                  (dom/props {:class "toolbar-dropdown-item"
                              :role "menuitem"
                              :disabled no-content?
                              :aria-label "Compare card models"})
                  (icons/Icon :git-compare :size 16)
                  (dom/span (dom/text "Compare models..."))
                  (dom/On "click"
                    (fn [_]
                      (bus/dispatch! :compare-card-gen)
                      (reset! !open false))
                    nil))))))))))
