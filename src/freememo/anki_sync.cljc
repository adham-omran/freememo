(ns freememo.anki-sync
  "Full-stack Anki sync component — entry point (AnkiSyncButton).
   Reactive sub-components split across namespaces to stay below the
   JVM 64KB method limit imposed by Electric v3's e/defn macro."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.anki-sync-helpers :as helpers]
   [freememo.anki-sync-panels :as panels]
   [freememo.icons :as icons]
   [freememo.keyboard :as keyboard]))

(e/defn AnkiSyncInnerBody
  "Form-config state (scope, fields, options, tags) + delegate to AnkiSyncSyncBody.
   conn = {:!status :!error :!decks :!models :!selected-deck :!basic-model :!cloze-model :!all-tags}"
  [user-id selected-doc !show-modal conn]
  (e/client
    (let [form {:!scope (atom "self")
                :!allow-dupes (atom false)
                ;; Custom header is per-PDF, owned by HeaderSettings (not here).
                :!use-tags (atom false)
                :!tags (atom [])
                :!basic-fields (atom [])
                :!cloze-fields (atom [])}]
      (panels/AnkiSyncSyncBody
        user-id selected-doc !show-modal conn form))))

(e/defn AnkiSyncModalBody
  "Delegates to AnkiSyncInnerBody — split point to stay below JVM 64KB method limit.
   conn = {:!status :!error :!decks :!models :!selected-deck :!basic-model :!cloze-model :!all-tags}"
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
                :!basic-model (atom nil)
                :!cloze-model (atom nil)
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

      ;; Toolbar button (hidden inside Sync dropdown; its ref is .click()'d
      ;; by the dropdown menu item to surface the modal).
      (dom/button
        (dom/props {:class "btn btn-sm btn-secondary"
                    :style {:background "var(--color-bg-subtle)" :color "var(--color-text-primary)" :font-weight "500"}
                    :aria-label "Push to Anki"
                    :data-tooltip "Push to Anki"})
        (icons/Icon :refresh-cw :size 16)
        (dom/span (dom/props {:class "icon-label"})
          (dom/text (if (and unsynced-count (pos? unsynced-count))
                      (str "Push to Anki (" unsynced-count ")...")
                      "Push to Anki...")))
        (reset! keyboard/!anki-sync-btn-ref dom/node)
        (e/on-unmount (fn [] (reset! keyboard/!anki-sync-btn-ref nil)))
        (dom/On "click" (fn [_] (reset! !show-modal true)) nil))

      ;; Modal
      (when show-modal
        (AnkiSyncModal user-id selected-doc card-type !show-modal)))))
