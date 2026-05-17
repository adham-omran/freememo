(ns freememo.settings-page
  "Settings page UI component."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.util :refer [mac-platform?]]
   [freememo.storage-section :refer [StorageSection]]
   [freememo.ai-features-section :refer [AIFeaturesSection]]
   [freememo.typeahead :refer [Typeahead]]
   [freememo.anki-sync-helpers :as anki-helpers]
   [freememo.zotero-client :as zc]
   #?(:clj [freememo.settings :as settings])
   #?(:clj [freememo.anki-sync-server :as sync])
   #?(:clj [freememo.user-state :as us])))


;; Section anchor ids + sidebar labels, in display order.
;; Storage is folded into the Account tab.
(def section-defs
  [{:id "settings-account" :label "Account"}
   {:id "settings-extraction" :label "Page Extraction"}
   {:id "settings-ai" :label "AI Features"}
   {:id "settings-appearance" :label "Appearance"}
   {:id "settings-anki" :label "Anki Sync"}
   {:id "settings-zotero" :label "Zotero"}
   {:id "settings-keyboard" :label "Keyboard Shortcuts"}])

;; Stable atom shared across reactive cycles. Atoms created inside e/defn let
;; bodies are recreated on every cycle (CLAUDE.md anti-pattern) — the scroll
;; handler would write to a stale atom while SettingsNav watches a fresh one.
#?(:cljs (defonce !settings-active-section
           (atom (-> section-defs first :id))))

(defn valid-section-id? [id]
  (boolean (some #(= id (:id %)) section-defs)))

(defn init-active-from-hash!
  "On page mount, if `location.hash` names a section, set the active tab to it.
   Idempotent — safe to call from a reactive body."
  []
  #?(:cljs
     (let [hash (.-hash js/window.location)
           id (when (and hash (> (.-length hash) 1)) (subs hash 1))]
       (when (valid-section-id? id)
         (reset! !settings-active-section id)))
     :clj nil))

(defn set-active-tab!
  "Switch active tab and reflect the choice in the URL hash without scrolling."
  [id]
  #?(:cljs
     (do (reset! !settings-active-section id)
       (.replaceState js/history #js {} "" (str "#" id)))
     :clj nil))

(defn tab-style
  "display:block when this tab is active, display:none otherwise.
   Hides inactive sections without unmounting them — server fetches in each
   section's let bindings still happen once on mount."
  [active section-id]
  {:display (if (= active section-id) "block" "none")})

(defn save-anki-model-only!*
  "Save only :basic-model or :cloze-model into the global Anki sync settings,
   preserving every other key. Uses load-anki-preferences to read current
   values, then save-anki-sync-settings which is the only non-granular setter
   the partition for this task allows us to use.

   kind is :basic or :cloze. Server-side only."
  [user-id kind new-model]
  #?(:clj
     (try
       (let [loaded (sync/load-anki-preferences user-id)
             prefs (:prefs loaded)
             ;; save-anki-sync-settings ignores :basic-fields/:cloze-fields
             ;; keys — they're not in its destructuring — so leaving them in
             ;; the map is fine (they go through save-anki-basic-fields /
             ;; save-anki-cloze-fields separately on this page).
             key-to-set (case kind :basic :basic-model :cloze :cloze-model)]
         (settings/save-anki-sync-settings user-id (assoc prefs key-to-set new-model)))
       (catch Exception _
         {:success false :error "Failed to save model"}))
     :cljs nil))

(e/defn FieldSlotSelect
  "Field-slot dropdown with auto-save on change. Pattern mirrors the rest of
   settings_page.cljc (dom/On change → e/Token → e/Offload save → resolve).

   `compute-vec` is a client-side fn from new-value to the full field-vector
   that should be persisted. The same dropdown can therefore save either of
   the basic slots (question / answer) by composing with the sibling slot's
   current value; the cloze slot trivially returns a single-element vector.
   `save-kind` is :basic or :cloze, dispatched server-side to the matching
   settings/save-anki-*-fields fn."
  [user-id label value available !atom save-kind compute-vec]
  (e/client
    (dom/div
      (dom/props {:style {:display "flex" :align-items "center" :gap "8px"
                          :margin-bottom "var(--sp-2)"}})
      (dom/label (dom/props {:style {:font-size "13px" :min-width "120px"
                                     :color "var(--color-text-secondary)"}})
        (dom/text label))
      (dom/select
        (dom/props {:class "input" :style {:flex "1" :max-width "300px"
                                           :font-size "14px"}
                    :value (or value "")})
        (dom/option (dom/props {:value ""}) (dom/text "— select —"))
        (e/for [f (e/diff-by identity (vec available))]
          (dom/option (dom/props {:value f :selected (= f value)})
            (dom/text f)))
        (let [change-event (dom/On "change" (fn [e] (-> e .-target .-value)) nil)
              normalized (when (some? change-event)
                           (if (= "" change-event) nil change-event))
              [t _] (e/Token change-event)]
          (when (some? change-event)
            (reset! !atom normalized))
          (when t
            (let [new-vec (vec (compute-vec normalized))
                  r (e/server
                      (e/Offload
                        #(case save-kind
                           :basic (settings/save-anki-basic-fields user-id new-vec)
                           :cloze (settings/save-anki-cloze-fields user-id new-vec))))]
              (case r
                (if (:success r) (t) (t (:error r)))))))))))

(e/defn AnkiConnectStatusBanner
  "Connecting / error / connected banner above the field-defaults body."
  [status ac-error]
  (e/client
    (cond
      (#{:idle :connecting} status)
      (dom/div
        (dom/props {:style {:padding "8px 12px"
                            :background "var(--color-bg-card)"
                            :border-radius "var(--radius-sm)"
                            :font-size "13px"
                            :color "var(--color-text-secondary)"
                            :margin-bottom "var(--sp-3)"}})
        (dom/span (dom/props {:class "spinner"
                              :style {:margin-right "6px"}}))
        (dom/text "Connecting to Anki…"))
      (= status :error)
      (dom/div
        (dom/props {:style {:padding "8px 12px"
                            :background "var(--color-warning-bg)"
                            :border "1px solid var(--color-warning)"
                            :border-radius "var(--radius-sm)"
                            :font-size "13px"
                            :margin-bottom "var(--sp-3)"}})
        (dom/text (str "Cannot reach AnkiConnect"
                    (when ac-error (str ": " ac-error))
                    ". Field-default controls are disabled. Make sure Anki is running with AnkiConnect installed, then reload.")))
      :else
      (dom/div
        (dom/props {:style {:padding "6px 10px"
                            :background "var(--color-bg-card)"
                            :border-radius "var(--radius-sm)"
                            :font-size "12px"
                            :color "var(--color-text-secondary)"
                            :margin-bottom "var(--sp-3)"}})
        (dom/text "Connected to Anki")))))

(e/defn AnkiBasicDefaults
  "Basic Note Type picker + Question/Answer slots. Owns its own model /
   committed / fields / q-field / a-field atoms; fetches saved values on
   mount; auto-saves on model commit (Typeahead) and on field-slot change
   (FieldSlotSelect)."
  [user-id connected? models]
  (e/client
    (let [server-saved (e/server (settings/get-anki-basic-fields user-id))
          server-saved-model (e/server (settings/get-anki-basic-model user-id))
          initial (e/snapshot (or server-saved []))
          initial-model (e/snapshot (or server-saved-model ""))
          !basic-model (atom initial-model)
          basic-model (e/watch !basic-model)
          !basic-model-committed (atom nil)
          basic-model-committed (e/watch !basic-model-committed)
          !basic-model-fields (atom [])
          basic-model-fields (e/watch !basic-model-fields)
          !q-field (atom (first initial))
          q-field (e/watch !q-field)
          !a-field (atom (second initial))
          a-field (e/watch !a-field)]
      (let [[t _] (e/Token [:settings-basic-fields-fetch connected? basic-model])]
        (when (and connected? (seq basic-model))
          (when t
            (case (anki-helpers/run-fetch-fields! basic-model !basic-model-fields) (t)))))
      (let [[t _] (e/Token basic-model-committed)]
        (when (and t (some? basic-model-committed) connected?)
          (let [committed-val basic-model-committed
                r (e/server (e/Offload
                              #(save-anki-model-only!* user-id :basic committed-val)))]
            (case r
              (if (:success r) (t) (t (:error r)))))))
      (dom/div
        (dom/props {:style {:margin-bottom "var(--sp-4)"}})
        (dom/label (dom/props {:style {:font-weight "600" :font-size "14px"
                                       :display "block"
                                       :margin-bottom "4px"}})
          (dom/text "Basic Note Type"))
        (Typeahead !basic-model (vec models) "Start typing model name..."
          !basic-model-committed nil)
        (cond
          (= basic-model-fields :loading)
          (dom/div
            (dom/props {:style {:font-size "13px" :margin-top "var(--sp-2)"
                                :color "var(--color-text-secondary)"}})
            (dom/span (dom/props {:class "spinner"
                                  :style {:margin-right "6px"}}))
            (dom/text "Loading fields..."))
          (and (vector? basic-model-fields) (seq basic-model-fields))
          (dom/div
            (dom/props {:style {:margin-top "var(--sp-2)"}})
            (FieldSlotSelect user-id "Question field" q-field
              basic-model-fields !q-field :basic
              (fn [new-q] (remove nil? [new-q a-field])))
            (FieldSlotSelect user-id "Answer field" a-field
              basic-model-fields !a-field :basic
              (fn [new-a] (remove nil? [q-field new-a]))))
          (and (vector? basic-model-fields) (empty? basic-model-fields) (seq basic-model))
          (dom/div (dom/props {:class "hint" :style {:margin-top "var(--sp-2)"}})
            (dom/text "No fields returned for this model.")))))))

(e/defn AnkiClozeDefaults
  "Cloze Note Type picker + cloze field slot. Mirrors AnkiBasicDefaults with
   a single field instead of q+a."
  [user-id connected? models]
  (e/client
    (let [server-saved (e/server (settings/get-anki-cloze-fields user-id))
          server-saved-model (e/server (settings/get-anki-cloze-model user-id))
          initial (e/snapshot (or server-saved []))
          initial-model (e/snapshot (or server-saved-model ""))
          !cloze-model (atom initial-model)
          cloze-model (e/watch !cloze-model)
          !cloze-model-committed (atom nil)
          cloze-model-committed (e/watch !cloze-model-committed)
          !cloze-model-fields (atom [])
          cloze-model-fields (e/watch !cloze-model-fields)
          !c-field (atom (first initial))
          c-field (e/watch !c-field)]
      (let [[t _] (e/Token [:settings-cloze-fields-fetch connected? cloze-model])]
        (when (and connected? (seq cloze-model))
          (when t
            (case (anki-helpers/run-fetch-fields! cloze-model !cloze-model-fields) (t)))))
      (let [[t _] (e/Token cloze-model-committed)]
        (when (and t (some? cloze-model-committed) connected?)
          (let [committed-val cloze-model-committed
                r (e/server (e/Offload
                              #(save-anki-model-only!* user-id :cloze committed-val)))]
            (case r
              (if (:success r) (t) (t (:error r)))))))
      (dom/div
        (dom/props {:style {:margin-bottom "var(--sp-2)"}})
        (dom/label (dom/props {:style {:font-weight "600" :font-size "14px"
                                       :display "block"
                                       :margin-bottom "4px"}})
          (dom/text "Cloze Note Type"))
        (Typeahead !cloze-model (vec models) "Start typing model name..."
          !cloze-model-committed nil)
        (cond
          (= cloze-model-fields :loading)
          (dom/div
            (dom/props {:style {:font-size "13px" :margin-top "var(--sp-2)"
                                :color "var(--color-text-secondary)"}})
            (dom/span (dom/props {:class "spinner"
                                  :style {:margin-right "6px"}}))
            (dom/text "Loading fields..."))
          (and (vector? cloze-model-fields) (seq cloze-model-fields))
          (dom/div
            (dom/props {:style {:margin-top "var(--sp-2)"}})
            (FieldSlotSelect user-id "Cloze field" c-field
              cloze-model-fields !c-field :cloze
              (fn [new-c] (remove nil? [new-c]))))
          (and (vector? cloze-model-fields) (empty? cloze-model-fields) (seq cloze-model))
          (dom/div (dom/props {:class "hint" :style {:margin-top "var(--sp-2)"}})
            (dom/text "No fields returned for this model.")))))))

(e/defn AnkiFieldDefaultsSection
  "User-level field-defaults UI on the Settings → Anki tab. Lookup order in
   the modal: per-doc preset → user-level (this section) → empty picker.

   Shell: owns AnkiConnect status (status/models/error) + fetch-models on
   mount; delegates picker bodies to AnkiBasicDefaults / AnkiClozeDefaults,
   which each own their own state."
  [user-id]
  (e/client
    (let [!status (atom :idle)
          status (e/watch !status)
          !models (atom [])
          models (e/watch !models)
          !ac-error (atom nil)
          ac-error (e/watch !ac-error)
          connected? (= status :connected)]
      (let [[t _] (e/Token :settings-fields-fetch-models)]
        (when t
          (case (anki-helpers/run-fetch-models! !status !models !ac-error) (t))))
      (dom/div
        (dom/props {:class "anki-settings-section"})
        (dom/h4 (dom/props {:class "anki-settings-section-title"})
          (dom/text "Field Defaults"))
        (dom/div (dom/props {:class "hint" :style {:margin-bottom "var(--sp-3)"}})
          (dom/text "Pick the model first, then choose which Anki fields receive question / answer (basic) and cloze text. Changes save automatically. Used as the user-level default when no per-doc preset is set."))
        (AnkiConnectStatusBanner status ac-error)
        (dom/div
          (dom/props {:style {:opacity (if connected? "1" "0.4")
                              :pointer-events (if connected? "auto" "none")}})
          (AnkiBasicDefaults user-id connected? models)
          (AnkiClozeDefaults user-id connected? models))))))

;; Plugin .xpi download URL — points at the latest GitHub Release.
;; Update this if the plugin's repo/owner changes.
(def ^:private plugin-download-url
  "https://github.com/adham-omran/electric-card-maker/releases/latest")

(e/defn ZoteroSection
  "Settings → Zotero. Per-user enable toggle + plugin-install link + a
   Test Connection button that talks to the FreeMemo Zotero plugin
   directly from the browser. No server round-trip for the probe — the
   plugin is the boundary that lets the browser reach the user's local
   Zotero across origins.

   Pre:  user-id non-nil.
   Post: enable toggle auto-saves; probe result renders inline as
         :ok / :error / :idle and surfaces plugin + Zotero versions on success."
  [user-id]
  (e/client
    (let [server-enabled (e/server (settings/zotero-enabled? user-id))
          !enabled       (atom server-enabled)
          enabled        (e/watch !enabled)
          !probe-state   (atom :idle)   ; :idle | :running | :ok | :error
          probe-state    (e/watch !probe-state)
          !probe-data    (atom nil)     ; map on :ok, string on :error
          probe-data     (e/watch !probe-data)]
      (dom/div
        (dom/props {:class "card"})
        (dom/h3 (dom/props {:class "section-title"}) (dom/text "Zotero"))

        ;; Intro
        (dom/div
          (dom/props {:style {:padding "10px 12px" :background "var(--color-info-bg)"
                              :border-radius "var(--radius-md)" :margin-bottom "var(--sp-3)"
                              :font-size "13px" :line-height "1.5"
                              :color "var(--color-text-secondary)"}})
          (dom/text "Import PDFs from your local Zotero library. Requires the ")
          (dom/strong (dom/text "FreeMemo for Zotero"))
          (dom/text " plugin installed in Zotero, and ")
          (dom/strong (dom/text "Allow other applications on this computer to communicate with Zotero"))
          (dom/text " enabled in Zotero → Settings → Advanced."))

        ;; Enable toggle
        (dom/div
          (dom/props {:class "field"})
          (dom/label
            (dom/props {:style {:display "flex" :align-items "center" :gap "10px" :cursor "pointer"}})
            (dom/input
              (dom/props {:type "checkbox" :checked enabled
                          :style {:width "18px" :height "18px"
                                  :accent-color "var(--color-primary)"}})
              (let [change-event (dom/On "change" (fn [e] (-> e .-target .-checked)) nil)
                    [t _]        (e/Token change-event)]
                (when (some? change-event)
                  (reset! !enabled change-event))
                (when t
                  (let [r (e/server (e/Offload #(settings/save-zotero-enabled user-id change-event)))]
                    (case r
                      (if (:success r) (t) (t (:error r))))))))
            (dom/span
              (dom/props {:style {:font-size "14px" :font-weight "500"
                                  :color "var(--color-text-primary)"}})
              (dom/text "Enable Zotero import"))))

        ;; Plugin install link
        (dom/div
          (dom/props {:class "field"})
          (dom/label (dom/props {:class "label"}) (dom/text "Plugin"))
          (dom/div
            (dom/props {:style {:display "flex" :align-items "center" :gap "10px"}})
            (dom/a
              (dom/props {:href plugin-download-url
                          :target "_blank"
                          :rel "noopener noreferrer"
                          :class "btn btn-secondary"
                          :style {:font-size "13px"}})
              (dom/text "Download FreeMemo for Zotero (.xpi)")))
          (dom/div (dom/props {:class "hint"})
            (dom/text "Then in Zotero: Tools → Plugins → gear icon → \"Install Plugin From File\". Restart Zotero if prompted.")))

        ;; Test Connection
        (dom/div
          (dom/props {:class "field"})
          (dom/button
            (dom/props {:class "btn btn-secondary"
                        :disabled (= probe-state :running)})
            (dom/text (case probe-state
                        :running "Testing…"
                        "Test Connection"))
            (let [click-event (dom/On "click" (fn [_] :run) nil)
                  [t _]       (e/Token click-event)]
              (when t
                (reset! !probe-state :running)
                (reset! !probe-data nil)
                (-> (zc/probe!)
                    (.then (fn [result]
                             (if (and (:ok? result)
                                      (get-in result [:data :ok]))
                               (do (reset! !probe-state :ok)
                                   (reset! !probe-data (:data result)))
                               (do (reset! !probe-state :error)
                                   (reset! !probe-data
                                     (or (get-in result [:data :error])
                                         (:error result)
                                         "Could not reach the FreeMemo Zotero plugin.")))))))
                (t))))
          (when (= probe-state :ok)
            (dom/span
              (dom/props {:style {:margin-left "10px" :font-size "13px"
                                  :color "var(--color-success)"}})
              (dom/text (str "Connected — plugin "
                          (:plugin_version probe-data)
                          ", Zotero "
                          (:zotero_version probe-data)))))
          (when (= probe-state :error)
            (dom/div
              (dom/props {:style {:margin-top "8px" :padding "8px 12px"
                                  :background "var(--color-warning-bg)"
                                  :border "1px solid var(--color-warning)"
                                  :border-radius "var(--radius-sm)"
                                  :font-size "13px"}})
              (dom/text (or probe-data "Connection failed."))
              (dom/div
                (dom/props {:style {:margin-top "6px" :font-size "12px"
                                    :color "var(--color-text-secondary)"}})
                (dom/text "Troubleshoot: install the plugin (above), confirm Zotero is running, and that \"Allow other applications…\" is enabled in Zotero → Settings → Advanced.")))))))))

(e/defn SettingsNav []
  (e/client
    (let [active (e/watch !settings-active-section)]
      (dom/nav
        (dom/props {:class "settings-nav"})
        (e/for-by :id [{:keys [id label]} section-defs]
          (dom/a
            (dom/props {:href (str "#" id)
                        :class (if (= id active)
                                 "settings-nav__item settings-nav__item--active"
                                 "settings-nav__item")})
            (dom/text label)
            (dom/On "click"
              (fn [e] (.preventDefault e) (set-active-tab! id))
              nil)))))))

(e/defn SettingsPage [user-id username enc-key]
  (e/client
    (init-active-from-hash!)
    (let [active (e/watch !settings-active-section)]
      (dom/div
        (dom/props {:class "page-container settings-tabs-page"})

        ;; TOP: horizontal tab bar (Anki-style)
        (SettingsNav)

        ;; BELOW: content panel — only the active tab is visible (display:none others)
        (dom/div
          (dom/props {:class "settings-content"})

      ;; ── Account tab (includes Storage usage) ──
          (dom/div
            (dom/props {:class "card" :id "settings-account"
                        :style (tab-style active "settings-account")})
            (dom/h3 (dom/props {:class "section-title"}) (dom/text "Account"))
            (dom/div
              (dom/props {:style {:display "flex" :align-items "center" :justify-content "space-between"}})
              (dom/span
                (dom/props {:style {:font-size "14px" :color "var(--color-text-label)"}})
                (dom/text "Logged in as ")
                (dom/strong (dom/text username)))
              (dom/form
                (dom/props {:action "/api/logout" :method "post" :style {:margin "0"}})
                (dom/button
                  (dom/props {:type "submit"
                              :class "btn btn-danger"
                              :style {:padding "6px 14px" :font-size "13px"}})
                  (dom/text "Logout"))))

        ;; Email updates toggle
            (let [server-email-updates (e/server (settings/get-email-updates user-id))
                  !email-updates (atom server-email-updates)
                  email-updates (e/watch !email-updates)]
              (dom/div
                (dom/props {:class "field" :style {:margin-top "12px"}})
                (dom/label
                  (dom/props {:style {:display "flex" :align-items "center" :gap "10px" :cursor "pointer"}})
                  (dom/input
                    (dom/props {:type "checkbox" :checked email-updates
                                :style {:width "18px" :height "18px" :accent-color "var(--color-primary)"}})
                    (let [change-event (dom/On "change" (fn [e] (-> e .-target .-checked)) nil)
                          [t ?error] (e/Token change-event)]
                      (when (some? change-event)
                        (reset! !email-updates change-event))
                      (when t
                        (let [r (e/server (e/Offload #(settings/save-email-updates user-id change-event)))]
                          (case r
                            (if (:success r) (t) (t (:error r))))))))
                  (dom/span
                    (dom/props {:style {:font-size "14px" :font-weight "500" :color "var(--color-text-primary)"}})
                    (dom/text "Subscribe to email updates")))))

        ;; Storage usage (formerly its own tab; now part of Account)
            (StorageSection user-id))

      ;; ── Page Extraction Buttons section ──
          (let [server-ai-btn (e/server (settings/get-enable-ai-scan-button user-id))
                !ai-btn (atom server-ai-btn)
                ai-btn (e/watch !ai-btn)
                server-pdfbox-btn (e/server (settings/get-enable-pdfbox-button user-id))
                !pdfbox-btn (atom server-pdfbox-btn)
                pdfbox-btn (e/watch !pdfbox-btn)
                server-pdfjs-btn (e/server (settings/get-enable-pdfjs-button user-id))
                !pdfjs-btn (atom server-pdfjs-btn)
                pdfjs-btn (e/watch !pdfjs-btn)]
            (dom/div
              (dom/props {:class "card" :id "settings-extraction"
                          :style (tab-style active "settings-extraction")})
              (dom/h3 (dom/props {:class "section-title"}) (dom/text "Page Extraction Buttons"))
              (dom/div
                (dom/props {:style {:padding "10px 12px" :background "var(--color-info-bg)" :border-radius "var(--radius-md)"
                                    :margin-bottom "var(--sp-3)" :font-size "13px" :line-height "1.5"
                                    :color "var(--color-text-secondary)"}})
                (dom/text "Choose which extraction buttons appear on the page toolbar. Native extractors (PDFBox, PDF.js) read text directly from the PDF — fast and free, but produce no result on scanned pages."))

          ;; AI Scan Page toggle
              (dom/div
                (dom/props {:class "field"})
                (dom/label
                  (dom/props {:style {:display "flex" :align-items "center" :gap "10px" :cursor "pointer"}})
                  (dom/input
                    (dom/props {:type "checkbox" :checked ai-btn
                                :style {:width "18px" :height "18px" :accent-color "var(--color-primary)"}})
                    (let [change-event (dom/On "change" (fn [e] (-> e .-target .-checked)) nil)
                          [t _] (e/Token change-event)]
                      (when (some? change-event)
                        (reset! !ai-btn change-event))
                      (when t
                        (let [r (e/server (e/Offload #(settings/save-enable-ai-scan-button user-id change-event)))]
                          (case r
                            (if (:success r) (t) (t (:error r))))))))
                  (dom/div
                    (dom/span
                      (dom/props {:style {:font-size "14px" :font-weight "500" :color "var(--color-text-primary)"}})
                      (dom/text "Show \"Scan Page\" button (AI / OpenAI Vision)"))
                    (dom/div
                      (dom/props {:class "hint"})
                      (dom/text "Best for scanned PDFs and complex layouts. Requires LLM features and an API key.")))))

          ;; PDFBox toggle
              (dom/div
                (dom/props {:class "field"})
                (dom/label
                  (dom/props {:style {:display "flex" :align-items "center" :gap "10px" :cursor "pointer"}})
                  (dom/input
                    (dom/props {:type "checkbox" :checked pdfbox-btn
                                :style {:width "18px" :height "18px" :accent-color "var(--color-primary)"}})
                    (let [change-event (dom/On "change" (fn [e] (-> e .-target .-checked)) nil)
                          [t _] (e/Token change-event)]
                      (when (some? change-event)
                        (reset! !pdfbox-btn change-event))
                      (when t
                        (let [r (e/server (e/Offload #(settings/save-enable-pdfbox-button user-id change-event)))]
                          (case r
                            (if (:success r) (t) (t (:error r))))))))
                  (dom/div
                    (dom/span
                      (dom/props {:style {:font-size "14px" :font-weight "500" :color "var(--color-text-primary)"}})
                      (dom/text "Show \"Extract (PDFBox)\" button"))
                    (dom/div
                      (dom/props {:class "hint"})
                      (dom/text "Server-side native text extraction. No AI, no API key.")))))

          ;; PDF.js toggle
              (dom/div
                (dom/props {:class "field"})
                (dom/label
                  (dom/props {:style {:display "flex" :align-items "center" :gap "10px" :cursor "pointer"}})
                  (dom/input
                    (dom/props {:type "checkbox" :checked pdfjs-btn
                                :style {:width "18px" :height "18px" :accent-color "var(--color-primary)"}})
                    (let [change-event (dom/On "change" (fn [e] (-> e .-target .-checked)) nil)
                          [t _] (e/Token change-event)]
                      (when (some? change-event)
                        (reset! !pdfjs-btn change-event))
                      (when t
                        (let [r (e/server (e/Offload #(settings/save-enable-pdfjs-button user-id change-event)))]
                          (case r
                            (if (:success r) (t) (t (:error r))))))))
                  (dom/div
                    (dom/span
                      (dom/props {:style {:font-size "14px" :font-weight "500" :color "var(--color-text-primary)"}})
                      (dom/text "Show \"Extract (PDF.js)\" button"))
                    (dom/div
                      (dom/props {:class "hint"})
                      (dom/text "Client-side native text extraction using the loaded PDF.js viewer. No AI, no API key.")))))))

          (dom/div
            (dom/props {:id "settings-ai"
                        :style (tab-style active "settings-ai")})
            (AIFeaturesSection user-id enc-key))

      ;; ── Appearance section ──
          (let [server-font-size (e/server (settings/get-card-font-size user-id))
                !font-size (atom server-font-size)
                font-size (e/watch !font-size)
                server-theme (e/server (settings/get-theme user-id))
                !theme (atom server-theme)
                theme (e/watch !theme)]
            (dom/div
              (dom/props {:class "card" :id "settings-appearance"
                          :style (tab-style active "settings-appearance")})
              (dom/h3 (dom/props {:class "section-title"}) (dom/text "Appearance"))

              (dom/div
                (dom/props {:class "field"})
                (dom/label (dom/props {:class "label"}) (dom/text "Theme"))
                (dom/select
                  (dom/props {:value theme :class "select"})
                  (dom/option (dom/props {:value "auto"}) (dom/text "Auto"))
                  (dom/option (dom/props {:value "light"}) (dom/text "Light"))
                  (dom/option (dom/props {:value "dark"}) (dom/text "Dark"))
                  (let [change-event (dom/On "change" #(-> % .-target .-value) nil)
                        [t ?error] (e/Token change-event)]
                    (when (some? change-event)
                      (reset! !theme change-event))
                    (when t
                      (let [r (e/server (e/Offload #(settings/save-theme user-id change-event)))]
                        (case r
                          (if (:success r)
                            (case (e/server (swap! (us/get-atom user-id :settings-refresh) inc))
                              (t))
                            (t (:error r))))))))
                (dom/div (dom/props {:class "hint"})
                  (dom/text "Auto follows your system preference")))

              (dom/div
                (dom/props {:class "field"})
                (dom/label
                  (dom/props {:style {:display "flex" :align-items "center" :gap "10px"}})
                  (dom/span (dom/props {:class "label" :style {:margin-bottom "0"}})
                    (dom/text "Card Text Size"))
                  (e/for-by identity [_k [:font-size-input]]
                    (dom/input
                      (dom/props {:type "number" :min "10" :max "20"
                                  :style {:width "56px" :font-size "13px" :padding "4px 6px"
                                          :border "1px solid var(--color-border)" :border-radius "var(--radius-sm)"}})
                      (set! (.-value dom/node) (str font-size))
                      (let [change-event (dom/On "change" #(-> % .-target .-value js/parseInt) nil)
                            [t _] (e/Token change-event)]
                        (when (some? change-event)
                          (reset! !font-size change-event))
                        (when t
                          (let [r (e/server (e/Offload #(settings/save-card-font-size user-id change-event)))]
                            (case r
                              (if (:success r) (t) (t (:error r)))))))))
                  (dom/span (dom/props {:style {:font-size "13px" :color "var(--color-text-secondary)"}})
                    (dom/text "px")))
                (dom/div (dom/props {:class "hint"})
                  (dom/text "Adjusts text size for cards (10-20px)")))))

      ;; ── Anki Sync section ──
          (let [server-source-mode (e/server (settings/get-source-display-mode user-id))
                !source-mode (atom server-source-mode)
                source-mode (e/watch !source-mode)
                server-source-field (e/server (settings/get-anki-source-field user-id))
                !source-field (atom server-source-field)
                source-field (e/watch !source-field)
                server-bib-mode (e/server (settings/get-bibliography-display-mode user-id))
                !bib-mode (atom server-bib-mode)
                bib-mode (e/watch !bib-mode)
                server-bib-field (e/server (settings/get-bibliography-field-name user-id))
                !bib-field (atom server-bib-field)
                bib-field (e/watch !bib-field)
                server-auto-mode (e/server (settings/get-anki-auto-load-mode user-id))
                !auto-mode (atom server-auto-mode)
                auto-mode (e/watch !auto-mode)
                server-image-mode (e/server (settings/get-image-display-mode user-id))
                !image-mode (atom server-image-mode)
                image-mode (e/watch !image-mode)
                server-images-front (e/server (settings/get-anki-images-front-field user-id))
                !images-front (atom server-images-front)
                images-front (e/watch !images-front)
                server-images-back (e/server (settings/get-anki-images-back-field user-id))
                !images-back (atom server-images-back)
                images-back (e/watch !images-back)]
            (dom/div
              (dom/props {:class "card" :id "settings-anki"
                          :style (tab-style active "settings-anki")})
              (dom/h3 (dom/props {:class "section-title"}) (dom/text "Anki Sync"))

            ;; ── Source section ─────────────────────────────────────────
              (dom/div
                (dom/props {:class "anki-settings-section"})
                (dom/h4 (dom/props {:class "anki-settings-section-title"})
                  (dom/text "Source Reference"))

            ;; Source Display Mode
                (dom/div
                  (dom/props {:class "field"})
                  (dom/label (dom/props {:class "label"}) (dom/text "Source Display Mode"))
                  (dom/div
                    (dom/props {:style {:display "flex" :flex-direction "column" :gap "10px" :margin-top "4px"}})
                    (dom/label
                      (dom/props {:style {:display "flex" :align-items "flex-start" :gap "8px" :cursor "pointer"}})
                      (dom/input
                        (dom/props {:type "radio" :name "source-display-mode" :value "append"
                                    :checked (= source-mode "append")
                                    :style {:margin-top "3px"}})
                        (let [change-event (dom/On "change" #(-> % .-target .-value) nil)
                              [t ?error] (e/Token change-event)]
                          (when (some? change-event)
                            (reset! !source-mode change-event))
                          (when t
                            (let [r (e/server (e/Offload #(settings/save-source-display-mode user-id change-event)))]
                              (case r
                                (if (:success r)
                                  (case (e/server (swap! (us/get-atom user-id :settings-refresh) inc))
                                    (t))
                                  (t (:error r))))))))
                      (dom/div
                        (dom/span (dom/props {:style {:font-size "14px" :color "var(--color-text-primary)"}})
                          (dom/text "Append to card"))
                        (dom/div (dom/props {:class "hint"})
                          (dom/text "Source text appended to card content during Anki sync"))))
                    (dom/label
                      (dom/props {:style {:display "flex" :align-items "flex-start" :gap "8px" :cursor "pointer"}})
                      (dom/input
                        (dom/props {:type "radio" :name "source-display-mode" :value "field"
                                    :checked (= source-mode "field")
                                    :style {:margin-top "3px"}})
                        (let [change-event (dom/On "change" #(-> % .-target .-value) nil)
                              [t ?error] (e/Token change-event)]
                          (when (some? change-event)
                            (reset! !source-mode change-event))
                          (when t
                            (let [r (e/server (e/Offload #(settings/save-source-display-mode user-id change-event)))]
                              (case r
                                (if (:success r)
                                  (case (e/server (swap! (us/get-atom user-id :settings-refresh) inc))
                                    (t))
                                  (t (:error r))))))))
                      (dom/div
                        (dom/span (dom/props {:style {:font-size "14px" :color "var(--color-text-primary)"}})
                          (dom/text "Separate field"))
                        (dom/div (dom/props {:class "hint"})
                          (dom/text "Source sent as a separate Anki field"))))))

            ;; Source Field Name — only meaningful in "field" mode
                (when (= source-mode "field")
                  (dom/div
                    (dom/props {:class "field"})
                    (dom/label (dom/props {:class "label"}) (dom/text "Source Field Name"))
                    (dom/input
                      (dom/props {:type "text"
                                  :value source-field
                                  :placeholder "Source"
                                  :class "input"
                                  :style {:width "240px"}})
                      (let [change-event (dom/On "change" #(-> % .-target .-value) nil)
                            [t _] (e/Token change-event)]
                        (when (some? change-event)
                          (reset! !source-field change-event))
                        (when t
                          (let [r (e/server (e/Offload #(settings/save-anki-source-field user-id change-event)))]
                            (case r
                              (if (:success r) (t) (t (:error r))))))))
                    (dom/div (dom/props {:class "hint"})
                      (dom/text "Anki field name that receives the source reference. Defaults to \"Source\".")))))

            ;; ── Bibliography section ───────────────────────────────────
              (dom/div
                (dom/props {:class "anki-settings-section"})
                (dom/h4 (dom/props {:class "anki-settings-section-title"})
                  (dom/text "Bibliography"))

            ;; Bibliography Display Mode
                (dom/div
                  (dom/props {:class "field"})
                  (dom/label (dom/props {:class "label"}) (dom/text "Bibliography Display Mode"))
                  (dom/div
                    (dom/props {:style {:display "flex" :flex-direction "column" :gap "10px" :margin-top "4px"}})
                    (dom/label
                      (dom/props {:style {:display "flex" :align-items "flex-start" :gap "8px" :cursor "pointer"}})
                      (dom/input
                        (dom/props {:type "radio" :name "bibliography-display-mode" :value "off"
                                    :checked (= bib-mode "off")
                                    :style {:margin-top "3px"}})
                        (let [change-event (dom/On "change" #(-> % .-target .-value) nil)
                              [t _] (e/Token change-event)]
                          (when (some? change-event)
                            (reset! !bib-mode change-event))
                          (when t
                            (let [r (e/server (e/Offload #(settings/save-bibliography-display-mode user-id change-event)))]
                              (case r
                                (if (:success r)
                                  (case (e/server (swap! (us/get-atom user-id :settings-refresh) inc))
                                    (t))
                                  (t (:error r))))))))
                      (dom/div
                        (dom/span (dom/props {:style {:font-size "14px" :color "var(--color-text-primary)"}})
                          (dom/text "Off"))
                        (dom/div (dom/props {:class "hint"})
                          (dom/text "No bibliography in Anki output"))))
                    (dom/label
                      (dom/props {:style {:display "flex" :align-items "flex-start" :gap "8px" :cursor "pointer"}})
                      (dom/input
                        (dom/props {:type "radio" :name "bibliography-display-mode" :value "append"
                                    :checked (= bib-mode "append")
                                    :style {:margin-top "3px"}})
                        (let [change-event (dom/On "change" #(-> % .-target .-value) nil)
                              [t _] (e/Token change-event)]
                          (when (some? change-event)
                            (reset! !bib-mode change-event))
                          (when t
                            (let [r (e/server (e/Offload #(settings/save-bibliography-display-mode user-id change-event)))]
                              (case r
                                (if (:success r)
                                  (case (e/server (swap! (us/get-atom user-id :settings-refresh) inc))
                                    (t))
                                  (t (:error r))))))))
                      (dom/div
                        (dom/span (dom/props {:style {:font-size "14px" :color "var(--color-text-primary)"}})
                          (dom/text "Append to card"))
                        (dom/div (dom/props {:class "hint"})
                          (dom/text "Citation appended below the card content during Anki sync"))))
                    (dom/label
                      (dom/props {:style {:display "flex" :align-items "flex-start" :gap "8px" :cursor "pointer"}})
                      (dom/input
                        (dom/props {:type "radio" :name "bibliography-display-mode" :value "field"
                                    :checked (= bib-mode "field")
                                    :style {:margin-top "3px"}})
                        (let [change-event (dom/On "change" #(-> % .-target .-value) nil)
                              [t _] (e/Token change-event)]
                          (when (some? change-event)
                            (reset! !bib-mode change-event))
                          (when t
                            (let [r (e/server (e/Offload #(settings/save-bibliography-display-mode user-id change-event)))]
                              (case r
                                (if (:success r)
                                  (case (e/server (swap! (us/get-atom user-id :settings-refresh) inc))
                                    (t))
                                  (t (:error r))))))))
                      (dom/div
                        (dom/span (dom/props {:style {:font-size "14px" :color "var(--color-text-primary)"}})
                          (dom/text "Separate field"))
                        (dom/div (dom/props {:class "hint"})
                          (dom/text "Citation sent as a separate Anki field"))))))

            ;; Bibliography Field Name — only meaningful in "field" mode
                (when (= bib-mode "field")
                  (dom/div
                    (dom/props {:class "field"})
                    (dom/label (dom/props {:class "label"}) (dom/text "Bibliography Field Name"))
                    (dom/input
                      (dom/props {:type "text"
                                  :value bib-field
                                  :placeholder "Bibliography"
                                  :class "input"
                                  :style {:width "240px"}})
                      (let [change-event (dom/On "change" #(-> % .-target .-value) nil)
                            [t _] (e/Token change-event)]
                        (when (some? change-event)
                          (reset! !bib-field change-event))
                        (when t
                          (let [r (e/server (e/Offload #(settings/save-bibliography-field-name user-id change-event)))]
                            (case r
                              (if (:success r) (t) (t (:error r))))))))
                    (dom/div (dom/props {:class "hint"})
                      (dom/text "Anki field name that receives the citation. Defaults to \"Bibliography\". When set to the same field as Source, the two values are combined in append style.")))))

            ;; ── Images section ─────────────────────────────────────────
              (dom/div
                (dom/props {:class "anki-settings-section"})
                (dom/h4 (dom/props {:class "anki-settings-section-title"})
                  (dom/text "Images"))

            ;; Image Display Mode — how pinned images are routed to Anki on push
                (dom/div
                  (dom/props {:class "field"})
                  (dom/label (dom/props {:class "label"}) (dom/text "Image Display Mode"))
                  (dom/div
                    (dom/props {:style {:display "flex" :flex-direction "column" :gap "10px" :margin-top "4px"}})
                    (dom/label
                      (dom/props {:style {:display "flex" :align-items "flex-start" :gap "8px" :cursor "pointer"}})
                      (dom/input
                        (dom/props {:type "radio" :name "image-display-mode" :value "inline"
                                    :checked (= image-mode "inline")
                                    :style {:margin-top "3px"}})
                        (let [change-event (dom/On "change" #(-> % .-target .-value) nil)
                              [t _] (e/Token change-event)]
                          (when (some? change-event)
                            (reset! !image-mode change-event))
                          (when t
                            (let [r (e/server (e/Offload #(settings/save-image-display-mode user-id change-event)))]
                              (case r
                                (if (:success r)
                                  (case (e/server (swap! (us/get-atom user-id :settings-refresh) inc))
                                    (t))
                                  (t (:error r))))))))
                      (dom/div
                        (dom/span (dom/props {:style {:font-size "14px" :color "var(--color-text-primary)"}})
                          (dom/text "Inline"))
                        (dom/div (dom/props {:class "hint"})
                          (dom/text "Pinned images stay inside the card's front/back HTML"))))
                    (dom/label
                      (dom/props {:style {:display "flex" :align-items "flex-start" :gap "8px" :cursor "pointer"}})
                      (dom/input
                        (dom/props {:type "radio" :name "image-display-mode" :value "field"
                                    :checked (= image-mode "field")
                                    :style {:margin-top "3px"}})
                        (let [change-event (dom/On "change" #(-> % .-target .-value) nil)
                              [t _] (e/Token change-event)]
                          (when (some? change-event)
                            (reset! !image-mode change-event))
                          (when t
                            (let [r (e/server (e/Offload #(settings/save-image-display-mode user-id change-event)))]
                              (case r
                                (if (:success r)
                                  (case (e/server (swap! (us/get-atom user-id :settings-refresh) inc))
                                    (t))
                                  (t (:error r))))))))
                      (dom/div
                        (dom/span (dom/props {:style {:font-size "14px" :color "var(--color-text-primary)"}})
                          (dom/text "Separate fields"))
                        (dom/div (dom/props {:class "hint"})
                          (dom/text "Pinned images routed to dedicated Anki fields (named below)"))))))

            ;; Image Front/Back Field Names — only meaningful in field mode
                (when (= image-mode "field")
                  (dom/div
                    (dom/props {:class "field"})
                    (dom/label (dom/props {:class "label"}) (dom/text "Images Front Field"))
                    (dom/input
                      (dom/props {:type "text"
                                  :value images-front
                                  :placeholder "Images Front"
                                  :class "input"
                                  :style {:width "240px"}})
                      (let [change-event (dom/On "change" #(-> % .-target .-value) nil)
                            [t _] (e/Token change-event)]
                        (when (some? change-event)
                          (reset! !images-front change-event))
                        (when t
                          (let [r (e/server (e/Offload #(settings/save-anki-images-front-field user-id change-event)))]
                            (case r
                              (if (:success r) (t) (t (:error r))))))))
                    (dom/div (dom/props {:class "hint"})
                      (dom/text "Anki field receiving front-pin images. Defaults to \"Images Front\"."))))

                (when (= image-mode "field")
                  (dom/div
                    (dom/props {:class "field"})
                    (dom/label (dom/props {:class "label"}) (dom/text "Images Back Field"))
                    (dom/input
                      (dom/props {:type "text"
                                  :value images-back
                                  :placeholder "Images Back"
                                  :class "input"
                                  :style {:width "240px"}})
                      (let [change-event (dom/On "change" #(-> % .-target .-value) nil)
                            [t _] (e/Token change-event)]
                        (when (some? change-event)
                          (reset! !images-back change-event))
                        (when t
                          (let [r (e/server (e/Offload #(settings/save-anki-images-back-field user-id change-event)))]
                            (case r
                              (if (:success r) (t) (t (:error r))))))))
                    (dom/div (dom/props {:class "hint"})
                      (dom/text "Anki field receiving back-pin images. Defaults to \"Images Back\".")))))

            ;; ── Auto-load section ──────────────────────────────────────
              (dom/div
                (dom/props {:class "anki-settings-section"})
                (dom/h4 (dom/props {:class "anki-settings-section-title"})
                  (dom/text "Auto-load Settings"))

            ;; Auto-load Settings — controls what the Anki Sync modal restores on open
                (dom/div
                  (dom/props {:class "field"})
                  (dom/label (dom/props {:class "label"}) (dom/text "Auto-load Settings"))
                  (dom/div
                    (dom/props {:style {:display "flex" :flex-direction "column" :gap "10px" :margin-top "4px"}})
                    (dom/label
                      (dom/props {:style {:display "flex" :align-items "flex-start" :gap "8px" :cursor "pointer"}})
                      (dom/input
                        (dom/props {:type "radio" :name "anki-auto-load-mode" :value "per-item"
                                    :checked (= auto-mode "per-item")
                                    :style {:margin-top "3px"}})
                        (let [change-event (dom/On "change" #(-> % .-target .-value) nil)
                              [t _] (e/Token change-event)]
                          (when (some? change-event)
                            (reset! !auto-mode change-event))
                          (when t
                            (let [r (e/server (e/Offload #(settings/save-anki-auto-load-mode user-id change-event)))]
                              (case r
                                (if (:success r) (t) (t (:error r))))))))
                      (dom/div
                        (dom/span (dom/props {:style {:font-size "14px" :color "var(--color-text-primary)"}})
                          (dom/text "Use Last Settings Per Item"))
                        (dom/div (dom/props {:class "hint"})
                          (dom/text "Restore the saved settings for this specific document; auto-saves on every successful push."))))
                    (dom/label
                      (dom/props {:style {:display "flex" :align-items "flex-start" :gap "8px" :cursor "pointer"}})
                      (dom/input
                        (dom/props {:type "radio" :name "anki-auto-load-mode" :value "global"
                                    :checked (= auto-mode "global")
                                    :style {:margin-top "3px"}})
                        (let [change-event (dom/On "change" #(-> % .-target .-value) nil)
                              [t _] (e/Token change-event)]
                          (when (some? change-event)
                            (reset! !auto-mode change-event))
                          (when t
                            (let [r (e/server (e/Offload #(settings/save-anki-auto-load-mode user-id change-event)))]
                              (case r
                                (if (:success r) (t) (t (:error r))))))))
                      (dom/div
                        (dom/span (dom/props {:style {:font-size "14px" :color "var(--color-text-primary)"}})
                          (dom/text "Use Last Settings Globally"))
                        (dom/div (dom/props {:class "hint"})
                          (dom/text "Restore the most-recent settings used anywhere; same defaults across documents."))))
                    (dom/label
                      (dom/props {:style {:display "flex" :align-items "flex-start" :gap "8px" :cursor "pointer"}})
                      (dom/input
                        (dom/props {:type "radio" :name "anki-auto-load-mode" :value "none"
                                    :checked (= auto-mode "none")
                                    :style {:margin-top "3px"}})
                        (let [change-event (dom/On "change" #(-> % .-target .-value) nil)
                              [t _] (e/Token change-event)]
                          (when (some? change-event)
                            (reset! !auto-mode change-event))
                          (when t
                            (let [r (e/server (e/Offload #(settings/save-anki-auto-load-mode user-id change-event)))]
                              (case r
                                (if (:success r) (t) (t (:error r))))))))
                      (dom/div
                        (dom/span (dom/props {:style {:font-size "14px" :color "var(--color-text-primary)"}})
                          (dom/text "None (Manual)"))
                        (dom/div (dom/props {:class "hint"})
                          (dom/text "Open the modal with default values; you set everything fresh each time.")))))))

            ;; ── Field Defaults sub-section ──
            ;; User-level fallback for basic-fields/cloze-fields. The modal
            ;; lookup order is per-doc preset → user-level (here) → empty.
              (AnkiFieldDefaultsSection user-id)))

      ;; ── Zotero section ─────────────────────────────────────────
          (dom/div
            (dom/props {:id "settings-zotero"
                        :style (tab-style active "settings-zotero")})
            (ZoteroSection user-id))

      ;; ── Keyboard Shortcuts section (read-only) ──
          (let [mac? (e/client (mac-platform?))
                mod-key (if mac? "Cmd" "Ctrl")]
            (dom/div
              (dom/props {:class "card" :id "settings-keyboard"
                          :style (tab-style active "settings-keyboard")})
              (dom/h3 (dom/props {:class "section-title"}) (dom/text "Keyboard Shortcuts"))
              (let [shortcut-row (fn [key desc]
                                   {:key (str mod-key "+Shift+" key) :desc desc})
                    shortcuts [(shortcut-row "E" "Extract topic from selection")
                               (shortcut-row "G" "Generate cards")
                               (shortcut-row "S" "Scan Page (OCR)")
                               (shortcut-row "X" "Anki Sync")
                               (shortcut-row "D" "Mark Done")]]
                
                (dom/table
                  (dom/props {:style {:width "100%" :border-collapse "collapse" :font-size "14px"}})
                  (e/for-by :key [s shortcuts]
                    (dom/tr
                      (dom/td (dom/props {:style {:padding "6px 0" :color "var(--color-text-secondary)" :width "180px"}})
                        (dom/text (:key s)))
                      (dom/td (dom/props {:style {:padding "6px 0" :color "var(--color-text-primary)"}})
                        (dom/text (:desc s))))))))))))))
