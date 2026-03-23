(ns freememo.anki-sync-panels
  "Executor + error/connected panels for Anki sync — separate namespace to stay
   below the JVM 64KB method limit."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.anki-sync-helpers :as helpers]
   [freememo.anki-sync-form :as form]
   #?(:clj [freememo.anki-sync-server :as sync])
   #?(:clj [freememo.settings :as settings])
   #?(:clj [freememo.db :as db])))

(e/defn AnkiSyncExecutor
  "Handles push/pull execution and server recording.
   conn = {:!status :!decks :!models :!selected-deck :!basic-model :!cloze-model ...}
   form = {:!scope :!allow-dupes :!use-header :!header-text :!use-tags :!tags :!basic-fields :!cloze-fields}
   sync = {:!phase :!result :!error :!push-pairs :!pull-updates}"
  [user-id selected-doc current-pdf-page !refresh conn form sync]
  (let [{:keys [!phase !error !push-pairs !pull-updates]} sync
        sync-phase (e/watch (:!phase sync))
        scope (e/watch (:!scope form))
        selected-deck (e/watch (:!selected-deck conn))
        basic-model (e/watch (:!basic-model conn))
        cloze-model (e/watch (:!cloze-model conn))
        basic-fields (e/watch (:!basic-fields form))
        cloze-fields (e/watch (:!cloze-fields form))
        allow-dupes (e/watch (:!allow-dupes form))
        use-header (e/watch (:!use-header form))
        header-text (e/watch (:!header-text form))
        use-tags (e/watch (:!use-tags form))
        tags (e/watch (:!tags form))
        source-display-mode (e/server (settings/get-source-display-mode user-id))
        source-field (e/watch (:!source-field form))
        settings {:deck selected-deck
                  :basic-model basic-model
                  :cloze-model cloze-model
                  :basic-fields basic-fields
                  :cloze-fields cloze-fields
                  :allow-dupes allow-dupes
                  :use-header use-header
                  :header-text header-text
                  :tags tags
                  :source-display-mode source-display-mode
                  :source-field source-field}]

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
                (reset! !phase :done)
                (token))
              (do (reset! !error (:error result))
                (reset! !phase :error)
                (token)))))))

    ;; Record pull updates + deletions on server
    (when (and (= sync-phase :recording) (some? (e/watch !pull-updates)))
      (let [pull-data (e/watch !pull-updates)
            updates (:updates pull-data)
            deleted (:deleted pull-data)
            [?token _] (e/Token :record-pull)]
        (when-some [token ?token]
          (let [result (e/server (sync/apply-pull-updates updates deleted))]
            (if (:success result)
              (do (e/server (swap! !refresh inc))
                (e/server (settings/save-anki-sync-settings user-id
                            {:scope scope :deck selected-deck
                             :basic-model basic-model :cloze-model cloze-model
                             :allow-dupes allow-dupes
                             :use-header use-header :header-text header-text
                             :use-tags use-tags :tags tags}))
                (reset! !phase :done)
                (token))
              (do (reset! !error (:error result))
                (reset! !phase :error)
                (token)))))))

    ;; Pull execution
    (e/client
      (when (= sync-phase :pulling)
        (let [page-num (when (= scope "Current Page") current-pdf-page)
              page-topic-id (when page-num
                              (e/server
                                (:topics/id
                                  (first (filter #(= (:topics/page_number %) page-num)
                                           (db/list-pages selected-doc))))))
              cards-result (e/server (sync/get-cards-for-sync
                                       {:topic-id page-topic-id
                                        :root-topic-id selected-doc}))]
          (if-not (:success cards-result)
            (do (reset! !error (:error cards-result))
              (reset! !phase :error))
            (let [cards (:cards cards-result)]
              (helpers/run-pull! cards settings sync))))))

    ;; Push execution
    (e/client
      (when (= sync-phase :pushing)
        (let [page-num (when (= scope "Current Page") current-pdf-page)
              page-topic-id (when page-num
                              (e/server
                                (:topics/id
                                  (first (filter #(= (:topics/page_number %) page-num)
                                           (db/list-pages selected-doc))))))
              cards-result (e/server (sync/get-cards-for-sync
                                       {:topic-id page-topic-id
                                        :root-topic-id selected-doc}))]
          (if-not (:success cards-result)
            (do (reset! !error (:error cards-result))
              (reset! !phase :error))
            (let [cards (:cards cards-result)]
              (helpers/run-push! cards (assoc settings :tags (if use-tags tags [])) sync))))))))

(e/defn AnkiSyncErrorPanel
  "Error state with retry and cancel buttons.
   conn = {:!status :!error :!decks :!models :!selected-deck :!basic-model :!cloze-model :!all-tags}"
  [conn !show-modal]
  (e/client
    (let [conn-error (e/watch (:!error conn))]
      (dom/div
        (dom/props {:style {:text-align "center" :padding "20px"}})
        (dom/div
          (dom/props {:style {:color "var(--color-danger)" :margin-bottom "var(--sp-3)"}})
          (dom/text (or conn-error "Connection failed")))
        (dom/div
          (dom/props {:style {:font-size "14px" :color "var(--color-text-secondary)" :margin-bottom "var(--sp-4)"}})
          (dom/text "Make sure Anki is running with the AnkiConnect plugin installed."))
        (dom/button
          (dom/props {:class "btn btn-primary" :style {:font-size "14px"}})
          (dom/text "Retry")
          (dom/On "click"
            (fn [_]
              (reset! (:!status conn) :connecting)
              (reset! (:!error conn) nil)
              (helpers/run-fetch-config! conn))
            nil))
        (dom/button
          (dom/props {:class "btn btn-secondary" :style {:font-size "14px" :margin-left "var(--sp-2)"}})
          (dom/text "Cancel")
          (dom/On "click" (fn [_] (reset! !show-modal false)) nil))))))

(e/defn AnkiSyncConnectedPanel
  "Connected state: last-settings button, form, and status.
   conn = {:!decks :!models :!selected-deck :!basic-model :!cloze-model ...}
   form = {:!scope :!allow-dupes :!use-header :!header-text :!use-tags :!tags ...}
   sync = {:!phase :!result :!error :!push-pairs :!pull-updates}"
  [user-id conn form sync !show-modal]
  (e/client
    (let [decks (e/watch (:!decks conn))
          models (e/watch (:!models conn))]
      (dom/div
        ;; "Use Last Settings" button
        (dom/button
          (dom/props {:class "btn btn-secondary" :style {:padding "6px 14px" :font-size "14px" :margin-bottom "var(--sp-4)"}})
          (dom/text "Use Last Settings")
          (let [click (dom/On "click" identity nil)
                [?token _] (e/Token click)]
            (when-some [token ?token]
              (let [result (e/server (sync/load-anki-preferences user-id))]
                (if (:success result)
                  (let [prefs (:prefs result)]
                    (when (:scope prefs) (reset! (:!scope form) (:scope prefs)))
                    (when (:deck prefs)
                      (when (some #{(:deck prefs)} decks)
                        (reset! (:!selected-deck conn) (:deck prefs))))
                    (when (:basic-model prefs)
                      (when (some #{(:basic-model prefs)} models)
                        (reset! (:!basic-model conn) (:basic-model prefs))))
                    (when (:cloze-model prefs)
                      (when (some #{(:cloze-model prefs)} models)
                        (reset! (:!cloze-model conn) (:cloze-model prefs))))
                    (when (some? (:allow-dupes prefs))
                      (reset! (:!allow-dupes form) (:allow-dupes prefs)))
                    (when (some? (:use-header prefs))
                      (reset! (:!use-header form) (:use-header prefs)))
                    (when (:header-text prefs)
                      (reset! (:!header-text form) (:header-text prefs)))
                    (when (some? (:use-tags prefs))
                      (reset! (:!use-tags form) (:use-tags prefs)))
                    (when (:tags prefs)
                      (reset! (:!tags form) (:tags prefs)))
                    (token))
                  (token))))))
        (form/AnkiSyncForm conn form)
        (form/AnkiSyncStatus sync !show-modal)))))

(e/defn AnkiSyncModalDom
  "Modal overlay + inner dialog; delegates to error/connected/connecting panels.
   conn = {:!status :!error ...}  sync = {:!phase ...}"
  [user-id !show-modal conn form sync]
  (e/client
    (let [conn-status (e/watch (:!status conn))
          sync-phase (e/watch (:!phase sync))]
      (dom/div
        (dom/props {:class "modal-backdrop" :style {:background "rgba(0,0,0,0.5)"}
                    :tabindex "-1"})
        (dom/On "click" (fn [_] (when-not sync-phase (reset! !show-modal false))) nil)
        (dom/On "keydown"
          (fn [e]
            (when (and (helpers/escape-key? e) (not sync-phase))
              (reset! !show-modal false)))
          nil)
        (dom/div
          (dom/props {:class "modal-content modal-lg" :style {:width "620px" :max-height "80vh" :overflow-y "auto"}})
          (dom/On "click" (fn [e] (.stopPropagation e)) nil)
          (dom/h3 (dom/props {:style {:margin-top "0" :margin-bottom "20px"}})
            (dom/text "Anki Sync"))
          (cond
            (= conn-status :connecting)
            (dom/div
              (dom/props {:style {:text-align "center" :padding "var(--sp-5)" :color "var(--color-text-secondary)"}})
              (dom/text "Connecting to Anki..."))
            (= conn-status :error)
            (AnkiSyncErrorPanel conn !show-modal)
            (= conn-status :connected)
            (AnkiSyncConnectedPanel user-id conn form sync !show-modal)))))))

(e/defn AnkiSyncSyncBody
  "Sync state, field-fetch tokens, executor, and modal DOM.
   conn = {:!status :!basic-model :!cloze-model ...}
   form = {:!basic-fields :!cloze-fields ...}"
  [user-id selected-doc current-pdf-page !refresh !show-modal conn form]
  (e/client
    (let [!sync-phase (atom nil)
          !sync-result (atom nil)
          !sync-error (atom nil)
          !push-pairs (atom nil)
          !pull-updates (atom nil)
          sync {:!phase !sync-phase
                :!result !sync-result
                :!error !sync-error
                :!push-pairs !push-pairs
                :!pull-updates !pull-updates}
          conn-status (e/watch (:!status conn))
          basic-model (e/watch (:!basic-model conn))
          cloze-model (e/watch (:!cloze-model conn))]
      (let [[?token _] (e/Token [:anki-sync-basic-fields conn-status basic-model])]
        (when (and basic-model (= conn-status :connected))
          (when-some [token ?token]
            (helpers/run-fetch-fields! basic-model (:!basic-fields form))
            (token))))
      (let [[?token _] (e/Token [:anki-sync-cloze-fields conn-status cloze-model])]
        (when (and cloze-model (= conn-status :connected))
          (when-some [token ?token]
            (helpers/run-fetch-fields! cloze-model (:!cloze-fields form))
            (token))))
      (AnkiSyncExecutor user-id selected-doc current-pdf-page !refresh conn form sync)
      (AnkiSyncModalDom user-id !show-modal conn form sync))))
