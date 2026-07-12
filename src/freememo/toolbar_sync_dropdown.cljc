(ns freememo.toolbar-sync-dropdown
  "Sync dropdown — replaces the inline Export + Pull from Anki + Anki Sync
   buttons with a single dropdown trigger. The three source buttons are
   rendered (hidden) inside this component so their invoker handles / modals /
   e/Token paths stay live; menu items dispatch their commands through
   `freememo.command-bus`.

   New e/defn lives in its own namespace to stay under the JVM 64KB bytecode
   limit (pattern from `extract_topic_button.cljc`).

   See `toolbar_generate_dropdown.cljc` for the listener-install rationale —
   the same plain-defn helper would be appropriate but is duplicated here to
   keep each dropdown component self-contained (one file per e/defn, two
   independent dropdowns)."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.doc-context :as dctx]
   [freememo.anki-sync :refer [AnkiSyncButton]]
   [freememo.quick-sync :refer [QuickSyncButton]]
   [freememo.anki-pull-button :refer [PullFromAnkiButton]]
   [freememo.export-button :refer [ExportButton]]
   [freememo.icons :as icons]
   [freememo.command-bus :as bus]
   [freememo.tooltip :as tooltip]))

;; See `toolbar_generate_dropdown.cljc:install-dropdown-listeners!` for
;; rationale on keeping this as a plain (defn) outside e/defn bodies. We
;; duplicate rather than share to keep each dropdown a self-contained unit.
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

(e/defn SyncDropdown []
  (e/client
    (let [user-id dctx/user-id topic-id dctx/topic-id root-topic-id dctx/root-topic-id
          card-type dctx/card-type
          unsynced-count dctx/unsynced-count mod-key dctx/mod-key
          !open (atom false)
          open (e/watch !open)
          has-unsynced? (and unsynced-count (pos? unsynced-count))
          trigger-label (if has-unsynced?
                          (str "Sync (" unsynced-count ")")
                          "Sync")]

      ;; Hidden source buttons — mount them so their refs/modals/tokens stay
      ;; live. CSS hides the buttons themselves (direct-child `button` rule);
      ;; the wrapper uses `display: contents` so it takes no layout space, and
      ;; the modals (rendered as div siblings inside each *Button e/defn) are
      ;; not affected by the button-only display:none.
      (dom/div
        (dom/props {:class "toolbar-dropdown-sources"})
        (ExportButton user-id topic-id root-topic-id unsynced-count)
        (PullFromAnkiButton user-id root-topic-id)
        (AnkiSyncButton user-id root-topic-id card-type unsynced-count)
        ;; Hidden proxy for background Quick Sync (Cmd-Shift-Opt-X). No menu
        ;; item — keyboard-only; the executor runs without opening the modal.
        (QuickSyncButton user-id root-topic-id card-type unsynced-count))

      ;; Visible dropdown. `toolbar-collapse-sync` participates in the tier
      ;; ladder — hidden at viewport-tier 7 so only the trigger collapses;
      ;; source buttons + their modals (sibling div above) stay mounted.
      (dom/div
        (dom/props {:class "toolbar-dropdown toolbar-sync-dropdown toolbar-collapse-sync"})

        (dom/button
          (dom/props {:class "btn btn-sm btn-secondary toolbar-dropdown-trigger toolbar-sync-trigger"
                      :style {:background "var(--color-bg-subtle)"
                              :color "var(--color-text-primary)"
                              :font-weight "500"}
                      :aria-haspopup "menu"
                      :aria-expanded (if open "true" "false")
                      :aria-label "Sync menu"})
          (tooltip/Tooltip! "Sync with Anki, or export cards")
          (icons/Icon :refresh-cw :size 16)
          (dom/span (dom/props {:class "icon-label"}) (dom/text trigger-label))
          (icons/Icon :chevron-down :size 14)
          (dom/On "click"
            (fn [e]
              #?(:cljs (.stopPropagation e))
              (swap! !open not))
            nil))

        (when open
          (let [cleanup (install-dropdown-listeners!
                          !open
                          "toolbar-sync-trigger"
                          "toolbar-sync-menu")]
            (e/on-unmount cleanup)
            (dom/div
              (dom/props {:class "toolbar-dropdown-menu toolbar-sync-menu"
                          :role "menu"})

              ;; Push to Anki
              (dom/button
                (dom/props {:class "toolbar-dropdown-item"
                            :role "menuitem"
                            :aria-label "Push to Anki"})
                (icons/Icon :refresh-cw :size 16)
                (dom/span (dom/text (if has-unsynced?
                                      (str "Push to Anki (" unsynced-count ")...")
                                      "Push to Anki...")))
                (dom/span (dom/props {:class "dropdown-shortcut"})
                  (dom/text (str mod-key "+Shift+X")))
                (dom/On "click"
                  (fn [_]
                    (bus/dispatch! :anki-sync)
                    (reset! !open false))
                  nil))

              ;; Pull from Anki
              (dom/button
                (dom/props {:class "toolbar-dropdown-item"
                            :role "menuitem"
                            :aria-label "Pull from Anki"})
                (icons/Icon :cloud-download :size 16)
                (dom/span (dom/text "Pull from Anki"))
                (dom/On "click"
                  (fn [_]
                    (bus/dispatch! :pull-anki)
                    (reset! !open false))
                  nil))

              ;; Export as CSV
              (dom/button
                (dom/props {:class "toolbar-dropdown-item"
                            :role "menuitem"
                            :aria-label "Export as CSV"})
                (icons/Icon :download :size 16)
                (dom/span (dom/text (if has-unsynced?
                                      (str "Export as CSV (" unsynced-count ")...")
                                      "Export as CSV...")))
                (dom/On "click"
                  (fn [_]
                    (bus/dispatch! :export)
                    (reset! !open false))
                  nil)))))))))
