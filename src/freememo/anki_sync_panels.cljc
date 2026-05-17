(ns freememo.anki-sync-panels
  "Executor + error/connected panels for Anki sync — separate namespace to stay
   below the JVM 64KB method limit."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.anki-sync-helpers :as helpers]
   [freememo.anki-sync-form :as form]
   [freememo.logging :as log]
   #?(:clj [freememo.anki-sync-server :as sync])
   #?(:clj [freememo.settings :as settings])
   #?(:clj [freememo.db :as db])))

(defn get-source-display-mode* [user-id]
  #?(:clj (settings/get-source-display-mode user-id)
     :cljs nil))

(defn get-anki-source-field* [user-id]
  #?(:clj (settings/get-anki-source-field user-id)
     :cljs nil))

(defn get-bibliography-display-mode* [user-id]
  #?(:clj (settings/get-bibliography-display-mode user-id)
     :cljs nil))

(defn get-bibliography-field-name* [user-id]
  #?(:clj (settings/get-bibliography-field-name user-id)
     :cljs nil))

(defn get-anki-auto-load-mode* [user-id]
  #?(:clj (settings/get-anki-auto-load-mode user-id)
     :cljs nil))

(defn get-anki-images-front-field* [user-id]
  #?(:clj (when-let [f (requiring-resolve 'freememo.settings/get-anki-images-front-field)]
            (f user-id))
     :cljs nil))

(defn get-anki-images-back-field* [user-id]
  #?(:clj (when-let [f (requiring-resolve 'freememo.settings/get-anki-images-back-field)]
            (f user-id))
     :cljs nil))

(defn get-image-display-mode* [user-id]
  #?(:clj (when-let [f (requiring-resolve 'freememo.settings/get-image-display-mode)]
            (f user-id))
     :cljs nil))

(defn get-root-topic-id* [selected-doc]
  #?(:clj (db/get-root-topic-id selected-doc)
     :cljs nil))

(defn get-app-base-url* []
  #?(:clj settings/app-base-url
     :cljs nil))

(defn resolve-preferred-fields* [user-id root-topic-id kind]
  #?(:clj (sync/resolve-preferred-fields user-id root-topic-id kind)
     :cljs nil))

(e/defn AnkiSyncExecutor
  "Handles push execution and server recording. Pull is handled by the
   toolbar Pull button (content_toolbar_actions), not the modal.
   conn = {:!status :!decks :!models :!selected-deck :!basic-model :!cloze-model ...}
   form = {:!scope :!allow-dupes :!use-header :!header-text :!use-tags :!tags :!basic-fields :!cloze-fields}
   sync = {:!phase :!result :!error :!push-pairs}"
  [user-id selected-doc current-pdf-page conn form sync]
  (let [{:keys [!phase !error !push-pairs]} sync
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
        source-display-mode (e/server (get-source-display-mode* user-id))
        source-field (e/server (get-anki-source-field* user-id))
        bibliography-display-mode (e/server (get-bibliography-display-mode* user-id))
        bibliography-field-name (e/server (get-bibliography-field-name* user-id))
        auto-load-mode (e/server (get-anki-auto-load-mode* user-id))
        images-front-field (e/server (get-anki-images-front-field* user-id))
        images-back-field (e/server (get-anki-images-back-field* user-id))
        image-display-mode (e/server (get-image-display-mode* user-id))
        root-id (e/server (get-root-topic-id* selected-doc))
        topic-info (e/server (when selected-doc
                               (let [t (db/get-topic-for-user user-id selected-doc)]
                                 {:kind (:topics/kind t)
                                  :title (:topics/title t)})))
        app-base-url (e/server (get-app-base-url*))
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
                  :bibliography-display-mode bibliography-display-mode
                  :bibliography-field-name bibliography-field-name
                  :images-front-field images-front-field
                  :images-back-field images-back-field
                  :image-display-mode image-display-mode
                  :topic-kind (:kind topic-info)
                  :topic-title (:title topic-info)
                  :root-topic-id selected-doc
                  :app-base-url app-base-url}
        prefs-map {:scope scope :deck selected-deck
                   :basic-model basic-model :cloze-model cloze-model
                   :allow-dupes allow-dupes
                   :use-header use-header :header-text header-text
                   :use-tags use-tags :tags tags
                   :basic-fields (if (vector? basic-fields) basic-fields [])
                   :cloze-fields (if (vector? cloze-fields) cloze-fields [])}]

    ;; All post-push server work in a single e/server call whose result is
    ;; observed below — Electric drops unused intermediate side effects when
    ;; multiple sibling e/server calls sit in a do-body.
    (when (and (= sync-phase :recording) (some? (e/watch !push-pairs)))
      (let [pairs (e/watch !push-pairs)
            [?token _] (e/Token :record-push)]
        (when-some [token ?token]
          (log/log-info (str "[anki-sync] record branch firing pairs=" (count pairs)
                          " auto-load-mode=" auto-load-mode
                          " root-id=" root-id
                          " deck=" selected-deck
                          " basic-model=" basic-model
                          " cloze-model=" cloze-model))
          (let [result (e/server (sync/finalize-push! user-id root-id pairs prefs-map auto-load-mode))]
            (log/log-info (str "[anki-sync] finalize-push! returned success=" (:success result)
                            " error=" (:error result)))
            (if (:success result)
              (do (reset! !phase :done)
                (token))
              (do (reset! !error (:error result))
                (reset! !phase :error)
                (token)))))))

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
                                       {:user-id user-id
                                        :topic-id page-topic-id
                                        :root-topic-id selected-doc}))]
          (if-not (:success cards-result)
            (do (reset! !error (:error cards-result))
              (reset! !phase :error))
            (let [cards (:cards cards-result)
                  ;; Override mount-time topic info with fresh values from
                  ;; this push's get-cards-for-sync call so PDF rename
                  ;; propagates to the Source anchor.
                  push-settings (assoc settings
                                  :tags (if use-tags tags [])
                                  :topic-title (:topic-title cards-result)
                                  :topic-kind (:topic-kind cards-result)
                                  :bibliography-text (:bibliography-text cards-result)
                                  :bibliography-html (:bibliography-html cards-result))]
              (helpers/run-push! cards push-settings sync))))))))

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

(defn apply-prefs!
  "Apply a preferences map to form/conn atoms, validating deck/model against available lists.
   Source-field is no longer part of prefs — it lives on the settings page now;
   any :source-field key in legacy presets is ignored.

   :basic-fields/:cloze-fields are applied directly here only as a fallback for
   the rare case where the model picker hasn't triggered run-fetch-fields! yet.
   Authoritative loading happens through run-fetch-fields!'s preferred-fields
   argument so the ordering is validated against the model's actual fields."
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
  (when (some? (:basic-fields prefs))
    (reset! (:!basic-fields form) (vec (:basic-fields prefs))))
  (when (some? (:cloze-fields prefs))
    (reset! (:!cloze-fields form) (vec (:cloze-fields prefs)))))

(e/defn AnkiSyncConnectedPanel
  "Connected state: preset auto-load (per the user's auto-load mode), form, and status.
   conn = {:!decks :!models :!selected-deck :!basic-model :!cloze-model ...}
   form = {:!scope :!allow-dupes :!use-header :!header-text :!use-tags :!tags ...}
   sync = {:!phase :!result :!error :!push-pairs}"
  [user-id selected-doc conn form sync !show-modal]
  (e/client
    (let [decks (e/watch (:!decks conn))
          models (e/watch (:!models conn))
          root-id (e/server (get-root-topic-id* selected-doc))
          auto-load-mode (e/server (get-anki-auto-load-mode* user-id))
          item-preset (e/server (sync/load-item-preset user-id root-id))
          global-prefs (e/server (:prefs (sync/load-anki-preferences user-id)))
          !applied-mode (atom nil)
          applied-mode (e/watch !applied-mode)]

      ;; First-render auto-load: branch on user's auto-load mode.
      ;; Spec lookup order (per-doc → user-level → empty):
      ;;   "per-item": apply per-doc preset; on miss, fall through to global.
      ;;   "global":   apply global last-used (user-level layer).
      ;;   "none":     skip.
      (when (and (nil? applied-mode) (seq decks) (seq models))
        (log/log-info (str "[anki-sync] auto-load fire mode=" auto-load-mode
                        " root-id=" root-id
                        " item-preset?=" (some? item-preset)
                        " item-preset-keys=" (when item-preset (vec (keys item-preset)))
                        " global-prefs-keys=" (when global-prefs (vec (keys global-prefs)))))
        (case auto-load-mode
          "per-item" (if item-preset
                       (do (log/log-info (str "[anki-sync] applying per-item preset deck=" (:deck item-preset)
                                           " basic-model=" (:basic-model item-preset)))
                         (apply-prefs! item-preset conn form decks models)
                         (reset! !applied-mode "per-item"))
                       ;; No per-doc preset — FALL THROUGH to user-level
                       ;; (global). Without this, user-level :basic-fields /
                       ;; :cloze-fields (and other defaults) never load in
                       ;; per-item mode. Applied-mode is "global" here, not
                       ;; "per-item", so the doc-specific indicator stays off.
                       (do (log/log-info (str "[anki-sync] per-item mode but no preset — falling through to global"
                                           " deck=" (:deck global-prefs)
                                           " basic-fields=" (:basic-fields global-prefs)
                                           " cloze-fields=" (:cloze-fields global-prefs)))
                         (apply-prefs! global-prefs conn form decks models)
                         (reset! !applied-mode "global")))
          "global" (do (log/log-info (str "[anki-sync] applying global prefs deck=" (:deck global-prefs)))
                     (apply-prefs! global-prefs conn form decks models)
                     (reset! !applied-mode "global"))
          "none" (log/log-info "[anki-sync] none mode — skipping auto-load")
          (log/log-info (str "[anki-sync] unknown auto-load-mode=" auto-load-mode))))

      (dom/div
        ;; Indicator only when per-item preset was auto-loaded
        (when (= applied-mode "per-item")
          (dom/div
            (dom/props {:style {:font-size "12px" :color "var(--color-text-secondary)"
                                :margin-bottom "var(--sp-2)" :font-style "italic"
                                :animation "fade-in 0.3s ease-in"}})
            (dom/text "Using saved settings for this document")))

        (form/AnkiSyncForm user-id conn form)
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
              (dom/span (dom/props {:class "spinner"}))
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
          sync {:!phase !sync-phase
                :!result !sync-result
                :!error !sync-error
                :!push-pairs !push-pairs}
          conn-status (e/watch (:!status conn))
          basic-model (e/watch (:!basic-model conn))
          cloze-model (e/watch (:!cloze-model conn))
          ;; Resolve preferred field ordering ONCE at modal open.
          ;; Lookup order: per-doc preset → user-level setting → empty.
          ;; root-id derives from a stable selected-doc, so this e/server is
          ;; effectively resolved-once per modal session.
          root-id (e/server (get-root-topic-id* selected-doc))
          preferred-basic-fields (e/server (resolve-preferred-fields* user-id root-id :basic))
          preferred-cloze-fields (e/server (resolve-preferred-fields* user-id root-id :cloze))]
      (let [[t _] (e/Token [:anki-sync-basic-fields conn-status basic-model])]
        (when (and basic-model (= conn-status :connected))
          (when t
            (case (helpers/run-fetch-fields! basic-model (:!basic-fields form)
                    preferred-basic-fields) (t)))))
      (let [[t _] (e/Token [:anki-sync-cloze-fields conn-status cloze-model])]
        (when (and cloze-model (= conn-status :connected))
          (when t
            (case (helpers/run-fetch-fields! cloze-model (:!cloze-fields form)
                    preferred-cloze-fields) (t)))))
      (AnkiSyncExecutor user-id selected-doc current-pdf-page conn form sync)
      (AnkiSyncModalDom user-id selected-doc !show-modal conn form sync))))
