(ns freememo.anki-sync
  "Full-stack Anki sync component — entry point (AnkiSyncButton).
   Reactive sub-components split across namespaces to stay below the
   JVM 64KB method limit imposed by Electric v3's e/defn macro."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.anki-sync-helpers :as helpers]
   [freememo.anki-sync-panels :as panels]
   [freememo.brand-icons :refer [AnkiMark]]
   [freememo.command-bus :as bus]
   [freememo.commands :as commands]
   [freememo.tooltip :as tooltip]))

(e/defn AnkiSyncInnerBody
  "Form-config state (scope, allow-dupes) + delegate to AnkiSyncSyncBody.
   conn = {:!status :!error :!decks :!models :!selected-deck :!all-tags}"
  [user-id selected-doc !show-modal conn]
  (e/client
    ;; Custom header is per-document, owned by HeaderSettings. Tags are
    ;; per-document too, owned by TagSettings — they were form state persisted to
    ;; an account-wide row, which applied one document's tag to the whole
    ;; collection on the next reconcile. See plans/anki-tags-per-document.md.
    (let [form {:!scope (atom "self")
                :!allow-dupes (atom false)}]
      (panels/AnkiSyncSyncBody
        user-id selected-doc !show-modal conn form))))

(e/defn AnkiSyncModalBody
  "Delegates to AnkiSyncInnerBody — split point to stay below JVM 64KB method limit.
   conn = {:!status :!error :!decks :!models :!selected-deck :!all-tags}"
  [user-id selected-doc !show-modal conn]
  (AnkiSyncInnerBody user-id selected-doc !show-modal conn))

(e/defn AnkiSyncModal
  "Connection/selection atoms + config-fetch token; delegates to AnkiSyncModalBody."
  [user-id selected-doc card-type !show-modal]
  (e/client
    (let [conn {:!status (atom :connecting)
                :!error (atom nil)
                :!decks (atom [])
                :!models (atom [])
                :!selected-deck (atom nil)
                :!all-tags (atom [])}]

      ;; On mount: fetch decks, models, and tags
      (let [[t _] (e/Token :anki-sync-fetch-config)]
        (when t
          (case (e/client (helpers/run-fetch-config! conn)) (t))))

      (AnkiSyncModalBody user-id selected-doc !show-modal conn))))

(e/defn AnkiSyncButton [user-id selected-doc card-type unsynced-count]
  (e/client
    (let [!show-modal (atom false)
          show-modal (e/watch !show-modal)]

      ;; Toolbar button. The meta+shift+x shortcut and the palette invoke it
      ;; through the :anki-sync command. `toolbar-collapse-sync` hides it at
      ;; viewport-tier 7; the modal below is a sibling, so it stays mounted.
      (dom/button
        (dom/props {:class "btn btn-sm btn-secondary toolbar-sync-trigger toolbar-collapse-sync"
                    :style {:background "var(--color-bg-subtle)" :color "var(--color-text-primary)" :font-weight "500"}
                    :data-tour "anki-sync"})
        (tooltip/Tooltip! (str "Sync with Anki (" (commands/display-chord :anki-sync) ")") :aria? true)
        (AnkiMark :size 16)
        (dom/span (dom/props {:class "icon-label"})
          (dom/text (if (and unsynced-count (pos? unsynced-count))
                      (str "Anki Sync (" unsynced-count ")")
                      "Anki Sync")))
        (let [node dom/node]
          (bus/publish-invoker! :anki-sync (fn [] (.click node)))
          (e/on-unmount (fn [] (bus/retract-invoker! :anki-sync))))
        (dom/On "click" (fn [_] (reset! !show-modal true)) nil))

      ;; Modal
      (when show-modal
        (AnkiSyncModal user-id selected-doc card-type !show-modal)))))
