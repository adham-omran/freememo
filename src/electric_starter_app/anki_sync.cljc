(ns electric-starter-app.anki-sync
  "Full-stack Anki sync component — entry point (AnkiSyncButton).
   Reactive sub-components split across namespaces to stay below the
   JVM 64KB method limit imposed by Electric v3's e/defn macro."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [electric-starter-app.anki-sync-helpers :as helpers]
   [electric-starter-app.anki-sync-panels :as panels]))

(e/defn AnkiSyncModalBody
  "Form/sync state, field-fetch tokens, executor, and modal overlay DOM.
   Receives connection atoms (unwatched) and watches them locally."
  [user-id selected-doc current-pdf-page !refresh !show-modal
   !conn-status !conn-error !decks !models !selected-deck !basic-model !cloze-model !all-tags]
  (e/client
    (let [conn-status (e/watch !conn-status)
          conn-error (e/watch !conn-error)
          decks (e/watch !decks)
          models (e/watch !models)
          selected-deck (e/watch !selected-deck)
          basic-model (e/watch !basic-model)
          cloze-model (e/watch !cloze-model)
          !basic-fields (atom [])
          basic-fields (e/watch !basic-fields)
          !cloze-fields (atom [])
          cloze-fields (e/watch !cloze-fields)
          !scope (atom "Current Page")
          scope (e/watch !scope)
          !allow-dupes (atom false)
          allow-dupes (e/watch !allow-dupes)
          !use-header (atom false)
          use-header (e/watch !use-header)
          !header-text (atom "")
          header-text (e/watch !header-text)
          all-tags (e/watch !all-tags)
          !use-tags (atom false)
          use-tags (e/watch !use-tags)
          !tags (atom [])
          tags (e/watch !tags)
          !sync-phase (atom nil)
          sync-phase (e/watch !sync-phase)
          !sync-result (atom nil)
          sync-result (e/watch !sync-result)
          !sync-error (atom nil)
          sync-error (e/watch !sync-error)
          !push-pairs (atom nil)
          !pull-updates (atom nil)]

      ;; Auto-fetch fields when model changes
      (let [[?token _] (e/Token [:anki-sync-basic-fields conn-status basic-model])]
        (when (and basic-model (= conn-status :connected))
          (when-some [token ?token]
            (helpers/run-fetch-fields! basic-model !basic-fields)
            (token))))
      (let [[?token _] (e/Token [:anki-sync-cloze-fields conn-status cloze-model])]
        (when (and cloze-model (= conn-status :connected))
          (when-some [token ?token]
            (helpers/run-fetch-fields! cloze-model !cloze-fields)
            (token))))

      ;; Executor (server recording + push/pull logic)
      (panels/AnkiSyncExecutor user-id sync-phase scope selected-doc current-pdf-page selected-deck
        basic-model cloze-model basic-fields cloze-fields allow-dupes use-header header-text
        use-tags tags
        !sync-phase !sync-result !sync-error !push-pairs !pull-updates !refresh)

      ;; Modal overlay DOM
      (panels/AnkiSyncModalDom user-id sync-phase !show-modal conn-status conn-error
        decks models !scope !selected-deck !basic-model !cloze-model
        basic-fields cloze-fields !allow-dupes !use-header !header-text
        all-tags !use-tags !tags sync-result sync-error
        !sync-phase !sync-result !sync-error !push-pairs !pull-updates
        !conn-status !conn-error !decks !models !all-tags))))

(e/defn AnkiSyncModal
  "Connection/selection atoms + config-fetch token; delegates to AnkiSyncModalBody."
  [user-id selected-doc current-pdf-page card-type !refresh !show-modal]
  (e/client
    (let [!conn-status (atom :connecting)
          !conn-error (atom nil)
          !decks (atom [])
          !models (atom [])
          !selected-deck (atom nil)
          !basic-model (atom nil)
          !cloze-model (atom nil)
          !all-tags (atom [])]

      ;; On mount: fetch decks, models, and tags
      (let [[?token _] (e/Token :anki-sync-fetch-config)]
        (when-some [token ?token]
          (helpers/run-fetch-config! !decks !models !selected-deck !basic-model !cloze-model
            !all-tags !conn-status !conn-error)
          (token)))

      (AnkiSyncModalBody user-id selected-doc current-pdf-page !refresh !show-modal
        !conn-status !conn-error !decks !models !selected-deck !basic-model !cloze-model !all-tags))))

(e/defn AnkiSyncButton [user-id selected-doc current-pdf-page card-type !refresh]
  (e/client
    (let [!show-modal (atom false)
          show-modal (e/watch !show-modal)]

      ;; Toolbar button
      (dom/button
        (dom/props {:style {:padding "4px 12px" :background "#7952b3" :color "white" :border "none"
                            :border-radius "4px" :cursor "pointer" :font-size "13px" :font-weight "500"}})
        (dom/text "Anki Sync...")
        (dom/On "click" (fn [_] (reset! !show-modal true)) nil))

      ;; Modal
      (when show-modal
        (AnkiSyncModal user-id selected-doc current-pdf-page card-type !refresh !show-modal)))))
