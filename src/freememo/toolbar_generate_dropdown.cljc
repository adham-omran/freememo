(ns freememo.toolbar-generate-dropdown
  "Generate dropdown — replaces the inline Generate + Generate-with-Prompt
   buttons with a single dropdown trigger. Action wiring is reused from
   ToolbarGenerate (which is rendered inside a hidden source wrapper so its
   button refs / modal / e/Token paths stay live). Menu items dispatch via
   `.click()` on the existing button refs in `freememo.keyboard`, identical to
   the established overflow-panel proxy pattern.

   New e/defn lives in its own namespace to stay under the JVM 64KB bytecode
   limit (pattern from `extract_topic_button.cljc`).

   Click-outside-to-close and Escape-to-close are wired via a plain-Clojure
   helper that installs document listeners on mount and returns a cleanup fn —
   the installation must not live in the reactive e/defn body (per CLAUDE.md
   'JS Library Init Side Effects in e/defn')."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.content-toolbar-generate :refer [ToolbarGenerate]]
   [freememo.content-toolbar-settings :as settings]
   [freememo.icons :as icons]
   [freememo.keyboard :as keyboard]))

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
                          (reset! !open false))))]
       (.addEventListener js/document "keydown" on-key)
       (.addEventListener js/document "mousedown" on-mouse)
       (fn []
         (.removeEventListener js/document "keydown" on-key)
         (.removeEventListener js/document "mousedown" on-mouse)))
     :clj (fn [] nil)))

;; !use-context / !context-window / !card-type are passed positionally (atoms
;; cannot ride inside the serialized cfg map). They feed the settings widgets'
;; own dom-event tokens — NOT cfg-derived values — so no reactive loop arises
;; when cfg changes (e.g. gen-active? flips); see CLAUDE.md token-input rule.
(e/defn GenerateDropdown [cfg !use-context !context-window !card-type]
  (e/client
    (let [{:keys [user-id llm-enabled? gen-active? gen-pending gen-error
                  card-type card-count-val use-context context-window
                  context-tooltip content-text mod-key]} cfg
          !open (atom false)
          open (e/watch !open)
          no-content? (empty? content-text)
          type-label (case card-type
                       "basic" "Basic"
                       "cloze" "Cloze"
                       (str card-type))
          action-summary (str "Generate " card-count-val " " type-label
                           (when use-context
                             (str " · " context-window
                               " page" (when (not= 1 context-window) "s")
                               " context")))]

      ;; Hidden source — ToolbarGenerate renders the real Generate +
      ;; Generate-with-Prompt buttons (which set keyboard refs), the e/Token
      ;; processors, and the PromptDialog modal. The wrapper has
      ;; `display: contents` so it doesn't take layout space; a direct-child
      ;; rule hides the inline buttons but leaves the modal (a div) visible.
      (dom/div
        (dom/props {:class "toolbar-dropdown-sources"})
        (ToolbarGenerate cfg))

      ;; Visible dropdown — gated on llm-enabled so the trigger disappears
      ;; when LLM is unavailable (same gating as the original buttons).
      (when llm-enabled?
        (dom/div
          (dom/props {:class "toolbar-dropdown toolbar-generate-dropdown"})

          ;; Trigger — mirrors the original Generate button's state display
          ;; (spinner + count when in-flight, error text on failure) so the
          ;; user retains immediate feedback even though the action moved into
          ;; a menu.
          (dom/button
            (dom/props {:class "btn btn-sm btn-primary toolbar-dropdown-trigger toolbar-generate-trigger"
                        :style {:background (cond no-content? "var(--color-disabled-bg)"
                                              gen-active? "var(--color-primary-light)"
                                              :else "var(--color-primary)")
                                :cursor (if no-content? "not-allowed" "pointer")
                                :font-weight "bold"}
                        :disabled no-content?
                        :aria-haspopup "menu"
                        :aria-expanded (if open "true" "false")
                        :aria-label (str action-summary " menu")
                        :data-tooltip (if no-content?
                                        "Extract text first to generate flashcards"
                                        action-summary)})
            (if (and gen-active? (nil? gen-error))
              (icons/Icon :loader-2 :size 16 :class "spin")
              (icons/Icon :sparkles :size 16))
            (dom/span (dom/props {:class "icon-label"})
              (dom/text (cond gen-error gen-error
                          gen-active? "Generating..."
                          :else "Generate")))
            (when (and gen-active? (nil? gen-error))
              (dom/span (dom/props {:class "icon-label"})
                (dom/text (str " (" gen-pending ")"))))
            (icons/Icon :chevron-down :size 14)
            (dom/On "click"
              (fn [e]
                #?(:cljs (.stopPropagation e))
                (when-not no-content?
                  (swap! !open not)))
              nil))

          ;; Menu — mounts only when open. Listeners are installed inside a
          ;; `let` whose mount lifecycle matches the menu's; e/on-unmount
          ;; removes them when the menu closes.
          (when open
            (let [cleanup (install-dropdown-listeners!
                            !open
                            "toolbar-generate-trigger"
                            "toolbar-generate-menu")]
              (e/on-unmount cleanup)
              (dom/div
                (dom/props {:class "toolbar-dropdown-menu toolbar-generate-menu"
                            :role "menu"})

                ;; Generation settings (C3): card-type + Context live here as
                ;; live widgets above the actions. Clicking them stays inside the
                ;; menu (install-dropdown-listeners! keeps it open). Card-count
                ;; deliberately stays inline in the toolbar — commonly changed.
                (dom/div
                  (dom/props {:class "toolbar-generate-menu-settings"})
                  (settings/ToolbarSettings
                    {:user-id user-id :llm-enabled? llm-enabled? :card-type card-type}
                    !use-context !context-window !card-type)
                  (settings/ContextSettings
                    {:user-id user-id :context-tooltip context-tooltip
                     :llm-enabled? llm-enabled?
                     :use-context use-context :context-window context-window}
                    !use-context !context-window))
                (dom/div (dom/props {:class "toolbar-dropdown-separator"}))

                ;; Generate
                (dom/button
                  (dom/props {:class "toolbar-dropdown-item"
                              :role "menuitem"
                              :disabled no-content?
                              :aria-label action-summary})
                  (icons/Icon :sparkles :size 16)
                  (dom/span (dom/text "Generate"))
                  (dom/span (dom/props {:class "dropdown-shortcut"})
                    (dom/text (str mod-key "+Shift+G")))
                  (dom/On "click"
                    (fn [_]
                      #?(:cljs
                         (when-let [btn @keyboard/!generate-btn-ref]
                           (when-not (.-disabled btn)
                             (.click btn))))
                      (reset! !open false))
                    nil))

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
                      #?(:cljs
                         (when-let [btn @keyboard/!gen-prompt-btn-ref]
                           (when-not (.-disabled btn)
                             (.click btn))))
                      (reset! !open false))
                    nil))))))))))
