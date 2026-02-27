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
  "Form/sync atoms, field-fetch tokens, executor, and modal overlay DOM."
  [user-id selected-doc current-pdf-page !refresh !show-modal
   !conn-status conn-status !conn-error conn-error
   !decks decks !models models
   !selected-deck selected-deck !basic-model basic-model !cloze-model cloze-model]
  (e/client
    (let [!basic-fields (atom [])
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
        !sync-phase !sync-result !sync-error !push-pairs !pull-updates !refresh)

      ;; Modal overlay
      (dom/div
        (dom/props {:style {:position "fixed" :top "0" :left "0" :width "100%" :height "100%"
                            :background "rgba(0,0,0,0.5)" :display "flex" :align-items "center"
                            :justify-content "center" :z-index "1000"}
                    :tabindex "-1"})
        (dom/On "click" (fn [_] (when-not sync-phase (reset! !show-modal false))) nil)
        (dom/On "keydown"
          (fn [e]
            (when (and (helpers/escape-key? e) (not sync-phase))
              (reset! !show-modal false)))
          nil)

        (dom/div
          (dom/props {:style {:background "white" :border-radius "8px" :padding "24px"
                              :width "520px" :max-width "90%" :box-shadow "0 4px 6px rgba(0,0,0,0.1)"
                              :max-height "80vh" :overflow-y "auto"}})
          (dom/On "click" (fn [e] (.stopPropagation e)) nil)

          (dom/h3 (dom/props {:style {:margin-top "0" :margin-bottom "20px"}})
            (dom/text "Anki Sync"))

          (cond
            (= conn-status :connecting)
            (dom/div
              (dom/props {:style {:text-align "center" :padding "20px" :color "#666"}})
              (dom/text "Connecting to Anki..."))

            (= conn-status :error)
            (panels/AnkiSyncErrorPanel conn-error !conn-status !conn-error !show-modal !decks !models
              !selected-deck !basic-model !cloze-model)

            (= conn-status :connected)
            (panels/AnkiSyncConnectedPanel user-id decks models !scope !selected-deck !basic-model !cloze-model
              basic-fields cloze-fields !allow-dupes !use-header !header-text
              sync-phase sync-result sync-error !show-modal !sync-phase !sync-result !sync-error
              !push-pairs !pull-updates)))))))

(e/defn AnkiSyncModal
  "Connection/selection atoms + config-fetch token; delegates to AnkiSyncModalBody."
  [user-id selected-doc current-pdf-page card-type !refresh !show-modal]
  (e/client
    (let [!conn-status (atom :connecting)
          conn-status (e/watch !conn-status)
          !conn-error (atom nil)
          conn-error (e/watch !conn-error)
          !decks (atom [])
          decks (e/watch !decks)
          !models (atom [])
          models (e/watch !models)
          !selected-deck (atom nil)
          selected-deck (e/watch !selected-deck)
          !basic-model (atom nil)
          basic-model (e/watch !basic-model)
          !cloze-model (atom nil)
          cloze-model (e/watch !cloze-model)]

      ;; On mount: fetch decks and models
      (let [[?token _] (e/Token :anki-sync-fetch-config)]
        (when-some [token ?token]
          (helpers/run-fetch-config! !decks !models !selected-deck !basic-model !cloze-model
            !conn-status !conn-error)
          (token)))

      (AnkiSyncModalBody user-id selected-doc current-pdf-page !refresh !show-modal
        !conn-status conn-status !conn-error conn-error
        !decks decks !models models
        !selected-deck selected-deck !basic-model basic-model !cloze-model cloze-model))))

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
