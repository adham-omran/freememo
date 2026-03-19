(ns electric-starter-app.anki-sync
  "Full-stack Anki sync component — entry point (AnkiSyncButton).
   Reactive sub-components split across namespaces to stay below the
   JVM 64KB method limit imposed by Electric v3's e/defn macro."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [electric-starter-app.anki-sync-helpers :as helpers]
   [electric-starter-app.anki-sync-panels :as panels]
   [electric-starter-app.keyboard :as keyboard]))

(e/defn AnkiSyncInnerBody
  "Form-config state (scope, fields, options, tags) + delegate to AnkiSyncSyncBody.
   conn = {:!status :!error :!decks :!models :!selected-deck :!basic-model :!cloze-model :!all-tags}"
  [user-id selected-doc current-pdf-page !refresh !show-modal conn]
  (e/client
    (let [form {:!scope        (atom "Current Page")
                :!allow-dupes  (atom false)
                :!use-header   (atom false)
                :!header-text  (atom "")
                :!use-tags     (atom false)
                :!tags         (atom [])
                :!basic-fields (atom [])
                :!cloze-fields (atom [])
                :!source-field (atom "Source")}]
      (panels/AnkiSyncSyncBody
        user-id selected-doc current-pdf-page !refresh !show-modal conn form))))

(e/defn AnkiSyncModalBody
  "Delegates to AnkiSyncInnerBody — split point to stay below JVM 64KB method limit.
   conn = {:!status :!error :!decks :!models :!selected-deck :!basic-model :!cloze-model :!all-tags}"
  [user-id selected-doc current-pdf-page !refresh !show-modal conn]
  (AnkiSyncInnerBody user-id selected-doc current-pdf-page !refresh !show-modal conn))

(e/defn AnkiSyncModal
  "Connection/selection atoms + config-fetch token; delegates to AnkiSyncModalBody."
  [user-id selected-doc current-pdf-page card-type !refresh !show-modal]
  (e/client
    (let [conn {:!status        (atom :connecting)
                :!error         (atom nil)
                :!decks         (atom [])
                :!models        (atom [])
                :!selected-deck (atom nil)
                :!basic-model   (atom nil)
                :!cloze-model   (atom nil)
                :!all-tags      (atom [])}]

      ;; On mount: fetch decks, models, and tags
      (let [[?token _] (e/Token :anki-sync-fetch-config)]
        (when-some [token ?token]
          (helpers/run-fetch-config! conn)
          (token)))

      (AnkiSyncModalBody user-id selected-doc current-pdf-page !refresh !show-modal conn))))

(e/defn AnkiSyncButton [user-id selected-doc current-pdf-page card-type !refresh unsynced-count]
  (e/client
    (let [!show-modal (atom false)
          show-modal (e/watch !show-modal)]

      ;; Toolbar button
      (dom/button
        (dom/props {:class "btn btn-sm btn-secondary" :style {:background "#f0f0f0" :color "#333" :font-weight "500"}})
        (dom/text (if (and unsynced-count (pos? unsynced-count))
                    (str "Anki Sync (" unsynced-count ")...")
                    "Anki Sync..."))
        (reset! keyboard/!anki-sync-btn-ref dom/node)
        (e/on-unmount (fn [] (reset! keyboard/!anki-sync-btn-ref nil)))
        (dom/On "click" (fn [_] (reset! !show-modal true)) nil))

      ;; Modal
      (when show-modal
        (AnkiSyncModal user-id selected-doc current-pdf-page card-type !refresh !show-modal)))))
