(ns electric-starter-app.anki-sync
  "Full-stack Anki sync component — entry point (AnkiSyncButton).
   Reactive sub-components split across namespaces to stay below the
   JVM 64KB method limit imposed by Electric v3's e/defn macro."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [electric-starter-app.anki-sync-helpers :as helpers]
   [electric-starter-app.anki-sync-panels :as panels]))

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

(e/defn AnkiSyncButton [user-id selected-doc current-pdf-page card-type !refresh]
  (e/client
    (let [!show-modal (atom false)
          show-modal (e/watch !show-modal)]

      ;; Toolbar button
      (dom/button
        (dom/props {:style {:padding "4px 12px" :background "#f0f0f0" :color "#333" :border "1px solid #ccc"
                            :border-radius "4px" :cursor "pointer" :font-size "13px" :font-weight "500"}})
        (dom/text "Anki Sync...")
        (dom/On "click" (fn [_] (reset! !show-modal true)) nil))

      ;; Modal
      (when show-modal
        (AnkiSyncModal user-id selected-doc current-pdf-page card-type !refresh !show-modal)))))
