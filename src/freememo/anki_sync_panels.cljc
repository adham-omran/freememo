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
   #?(:clj [freememo.user-state :as us])
   #?(:clj [freememo.db :as db])))

(e/defn AnkiSyncExecutor
  "Handles push/pull execution and server recording.
   conn = {:!status :!decks :!models :!selected-deck :!basic-model :!cloze-model ...}
   form = {:!scope :!allow-dupes :!use-header :!header-text :!use-tags :!tags :!basic-fields :!cloze-fields}
   sync = {:!phase :!result :!error :!push-pairs :!pull-updates}"
  [user-id selected-doc current-pdf-page conn form sync]
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
        topic-source (e/server (db/get-topic-source selected-doc))
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
                  :source-field source-field
                  :topic-source topic-source}]

    ;; Record push pairs on server
    (when (and (= sync-phase :recording) (some? (e/watch !push-pairs)))
      (let [pairs (e/watch !push-pairs)
            [?token _] (e/Token :record-push)]
        (when-some [token ?token]
          (let [result (e/server (sync/record-pushed-notes pairs))]
            (if (:success result)
              (do (e/server (swap! (us/get-atom user-id :sync-mutations) inc))
                (e/server (settings/save-anki-sync-settings user-id
                            {:scope scope :deck selected-deck
                             :basic-model basic-model :cloze-model cloze-model
                             :allow-dupes allow-dupes
                             :use-header use-header :header-text header-text
                             :use-tags use-tags :tags tags
                             :source-field source-field}))
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
              (do (e/server (swap! (us/get-atom user-id :sync-mutations) inc))
                (e/server (settings/save-anki-sync-settings user-id
                            {:scope scope :deck selected-deck
                             :basic-model basic-model :cloze-model cloze-model
                             :allow-dupes allow-dupes
                             :use-header use-header :header-text header-text
                             :use-tags use-tags :tags tags
                             :source-field source-field}))
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

(defn schedule-reset! [!atom delay-ms]
  #?(:cljs (js/setTimeout #(reset! !atom :idle) delay-ms)
     :clj nil))

(defn apply-prefs!
  "Apply a preferences map to form/conn atoms, validating deck/model against available lists."
  [prefs conn form decks models]
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
  (when (:source-field prefs)
    (reset! (:!source-field form) (:source-field prefs))))

(defn collect-current-settings
  "Collect current form/conn state into a preset map."
  [conn form]
  {:scope (deref (:!scope form))
   :deck (deref (:!selected-deck conn))
   :basic-model (deref (:!basic-model conn))
   :cloze-model (deref (:!cloze-model conn))
   :allow-dupes (deref (:!allow-dupes form))
   :use-header (deref (:!use-header form))
   :header-text (deref (:!header-text form))
   :use-tags (deref (:!use-tags form))
   :tags (deref (:!tags form))
   :source-field (deref (:!source-field form))})

(e/defn AnkiSyncConnectedPanel
  "Connected state: preset auto-load, last-settings button, save-preset button, form, and status.
   conn = {:!decks :!models :!selected-deck :!basic-model :!cloze-model ...}
   form = {:!scope :!allow-dupes :!use-header :!header-text :!use-tags :!tags ...}
   sync = {:!phase :!result :!error :!push-pairs :!pull-updates}"
  [user-id selected-doc conn form sync !show-modal]
  (e/client
    (let [decks (e/watch (:!decks conn))
          models (e/watch (:!models conn))
          root-id (e/server (db/get-root-topic-id selected-doc))
          item-preset (e/server (sync/load-item-preset user-id root-id))
          !preset-loaded (atom false)
          preset-loaded (e/watch !preset-loaded)]

      ;; Auto-load per-item preset on first render
      (when (and item-preset (not preset-loaded) (seq decks) (seq models))
        (apply-prefs! item-preset conn form decks models)
        (reset! !preset-loaded true))

      (dom/div
        ;; Indicator when preset was auto-loaded
        (when preset-loaded
          (dom/div
            (dom/props {:style {:font-size "12px" :color "var(--color-text-secondary)"
                                :margin-bottom "var(--sp-2)" :font-style "italic"
                                :animation "fade-in 0.3s ease-in"}})
            (dom/text "Using saved settings for this document")))

        ;; Button row
        (let [!save-state (atom :idle)
              save-state (e/watch !save-state)]
          (dom/div
            (dom/props {:style {:display "flex" :gap "8px" :margin-bottom "var(--sp-4)"}})

            ;; "Use Last Settings" button (global)
            (dom/button
              (dom/props {:class "btn btn-secondary" :style {:padding "6px 14px" :font-size "14px"}})
              (dom/text "Use Last Settings")
              (let [click (dom/On "click" identity nil)
                    [?token _] (e/Token click)]
                (when-some [token ?token]
                  (let [result (e/server (sync/load-anki-preferences user-id))]
                    (if (:success result)
                      (do (apply-prefs! (:prefs result) conn form decks models)
                        (token))
                      (token))))))

            ;; "Save Current Settings for Item" button
            (dom/button
              (dom/props {:class "btn btn-secondary"
                          :style {:padding "6px 14px" :font-size "14px"}
                          :disabled (not= save-state :idle)})
              (dom/text (case save-state
                          :saving "Saving..."
                          :saved "Saved!"
                          "Save Settings for Item"))
              (let [click (dom/On "click" identity nil)
                    [?token _] (e/Token click)]
                (when-some [token ?token]
                  (reset! !save-state :saving)
                  (let [preset (collect-current-settings conn form)
                        result (e/server (sync/save-item-preset user-id root-id preset))]
                    (if (:success result)
                      (do (reset! !preset-loaded true)
                        (reset! !save-state :saved)
                        (schedule-reset! !save-state 1500)
                        (token))
                      (do (reset! !save-state :idle)
                        (token)))))))))

        (form/AnkiSyncForm conn form)
        (form/AnkiSyncStatus sync !show-modal)))))

(e/defn AnkiSyncModalDom
  "Modal overlay + inner dialog; delegates to error/connected/connecting panels.
   conn = {:!status :!error ...}  sync = {:!phase ...}"
  [user-id selected-doc !show-modal conn form sync]
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
            (AnkiSyncConnectedPanel user-id selected-doc conn form sync !show-modal)))))))

(e/defn AnkiSyncSyncBody
  "Sync state, field-fetch tokens, executor, and modal DOM.
   conn = {:!status :!basic-model :!cloze-model ...}
   form = {:!basic-fields :!cloze-fields ...}"
  [user-id selected-doc current-pdf-page !show-modal conn form]
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
      (AnkiSyncExecutor user-id selected-doc current-pdf-page conn form sync)
      (AnkiSyncModalDom user-id selected-doc !show-modal conn form sync))))
