(ns electric-starter-app.anki-sync-panels
  "Executor + error/connected panels for Anki sync — separate namespace to stay
   below the JVM 64KB method limit."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [electric-starter-app.anki-sync-helpers :as helpers]
   [electric-starter-app.anki-sync-form :as form]
   #?(:clj [electric-starter-app.anki-sync-server :as sync])
   #?(:clj [electric-starter-app.settings :as settings])))

(e/defn AnkiSyncExecutor
  "Handles push/pull execution and server recording."
  [user-id sync-phase scope selected-doc current-pdf-page selected-deck
   basic-model cloze-model basic-fields cloze-fields allow-dupes use-header header-text
   use-tags tags
   !sync-phase !sync-result !sync-error !push-pairs !pull-updates !refresh]
  ;; Record push pairs on server
  (when (and (= sync-phase :recording) (some? (e/watch !push-pairs)))
    (let [pairs (e/watch !push-pairs)
          [?token _] (e/Token :record-push)]
      (when-some [token ?token]
        (let [result (e/server (sync/record-pushed-notes pairs))]
          (if (:success result)
            (do (e/server (swap! !refresh inc))
                (e/server (settings/save-anki-sync-settings user-id
                  {:scope scope :deck selected-deck
                   :basic-model basic-model :cloze-model cloze-model
                   :allow-dupes allow-dupes
                   :use-header use-header :header-text header-text
                   :use-tags use-tags :tags tags}))
                (reset! !sync-phase :done)
                (token))
            (do (reset! !sync-error (:error result))
                (reset! !sync-phase :error)
                (token)))))))

  ;; Record pull updates on server
  (when (and (= sync-phase :recording) (some? (e/watch !pull-updates)))
    (let [updates (e/watch !pull-updates)
          [?token _] (e/Token :record-pull)]
      (when-some [token ?token]
        (let [result (e/server (sync/apply-pull-updates updates))]
          (if (:success result)
            (do (e/server (swap! !refresh inc))
                (e/server (settings/save-anki-sync-settings user-id
                  {:scope scope :deck selected-deck
                   :basic-model basic-model :cloze-model cloze-model
                   :allow-dupes allow-dupes
                   :use-header use-header :header-text header-text
                   :use-tags use-tags :tags tags}))
                (reset! !sync-phase :done)
                (token))
            (do (reset! !sync-error (:error result))
                (reset! !sync-phase :error)
                (token)))))))

  ;; Pull execution
  (e/client
    (when (= sync-phase :pulling)
      (let [page-num (when (= scope "Current Page") current-pdf-page)
            cards-result (e/server (sync/get-cards-for-sync
                                     {:document-id selected-doc
                                      :page-number page-num}))]
        (if-not (:success cards-result)
          (do (reset! !sync-error (:error cards-result))
              (reset! !sync-phase :error))
          (let [cards (:cards cards-result)]
            (helpers/run-pull! cards basic-fields cloze-fields
              !sync-result !pull-updates !sync-error !sync-phase))))))

  ;; Push execution
  (e/client
    (when (= sync-phase :pushing)
      (let [page-num (when (= scope "Current Page") current-pdf-page)
            cards-result (e/server (sync/get-cards-for-sync
                                     {:document-id selected-doc
                                      :page-number page-num}))
            effective-tags (if use-tags tags [])]
        (if-not (:success cards-result)
          (do (reset! !sync-error (:error cards-result))
              (reset! !sync-phase :error))
          (let [cards (:cards cards-result)]
            (helpers/run-push! cards selected-deck basic-model cloze-model
              basic-fields cloze-fields allow-dupes use-header header-text effective-tags
              !sync-result !push-pairs !sync-error !sync-phase)))))))

(e/defn AnkiSyncErrorPanel
  "Error state with retry and cancel buttons."
  [conn-error !conn-status !conn-error !show-modal !decks !models
   !selected-deck !basic-model !cloze-model !all-tags]
  (e/client
    (dom/div
      (dom/props {:style {:text-align "center" :padding "20px"}})
      (dom/div
        (dom/props {:style {:color "#dc3545" :margin-bottom "12px"}})
        (dom/text (or conn-error "Connection failed")))
      (dom/div
        (dom/props {:style {:font-size "13px" :color "#666" :margin-bottom "16px"}})
        (dom/text "Make sure Anki is running with the AnkiConnect plugin installed."))
      (dom/button
        (dom/props {:style {:padding "8px 16px" :background "#007bff" :color "white" :border "none"
                            :border-radius "4px" :cursor "pointer"}})
        (dom/text "Retry")
        (dom/On "click"
          (fn [_]
            (reset! !conn-status :connecting)
            (reset! !conn-error nil)
            (helpers/run-fetch-config! !decks !models !selected-deck !basic-model !cloze-model
              !all-tags !conn-status !conn-error))
          nil))
      (dom/button
        (dom/props {:style {:padding "8px 16px" :background "#f8f9fa" :color "#333"
                            :border "1px solid #ccc" :border-radius "4px" :cursor "pointer"
                            :margin-left "8px"}})
        (dom/text "Cancel")
        (dom/On "click" (fn [_] (reset! !show-modal false)) nil)))))

(e/defn AnkiSyncConnectedPanel
  "Connected state: last-settings button, form, and status."
  [user-id decks models !scope !selected-deck !basic-model !cloze-model
   basic-fields cloze-fields !allow-dupes !use-header !header-text
   all-tags !use-tags !tags
   sync-phase sync-result sync-error !show-modal !sync-phase !sync-result !sync-error
   !push-pairs !pull-updates]
  (e/client
    (dom/div
      ;; "Use Last Settings" button
      (dom/button
        (dom/props {:style {:padding "6px 14px" :background "#f0f0f0" :color "#333"
                            :border "1px solid #ccc" :border-radius "4px" :cursor "pointer"
                            :font-size "13px" :margin-bottom "16px"}})
        (dom/text "Use Last Settings")
        (let [click (dom/On "click" identity nil)
              [?token _] (e/Token click)]
          (when-some [token ?token]
            (let [result (e/server (sync/load-anki-preferences user-id))]
              (if (:success result)
                (let [prefs (:prefs result)]
                  (when (:scope prefs) (reset! !scope (:scope prefs)))
                  (when (:deck prefs)
                    (when (some #{(:deck prefs)} decks)
                      (reset! !selected-deck (:deck prefs))))
                  (when (:basic-model prefs)
                    (when (some #{(:basic-model prefs)} models)
                      (reset! !basic-model (:basic-model prefs))))
                  (when (:cloze-model prefs)
                    (when (some #{(:cloze-model prefs)} models)
                      (reset! !cloze-model (:cloze-model prefs))))
                  (when (some? (:allow-dupes prefs))
                    (reset! !allow-dupes (:allow-dupes prefs)))
                  (when (some? (:use-header prefs))
                    (reset! !use-header (:use-header prefs)))
                  (when (:header-text prefs)
                    (reset! !header-text (:header-text prefs)))
                  (when (some? (:use-tags prefs))
                    (reset! !use-tags (:use-tags prefs)))
                  (when (:tags prefs)
                    (reset! !tags (:tags prefs)))
                  (token))
                (token))))))
      (form/AnkiSyncForm !scope decks !selected-deck models
        !basic-model basic-fields !cloze-model cloze-fields !allow-dupes !use-header !header-text
        all-tags !use-tags !tags)
      (form/AnkiSyncStatus sync-phase sync-result sync-error
        !show-modal !sync-phase !sync-result !sync-error !push-pairs !pull-updates))))
