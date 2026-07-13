(ns freememo.settings-page
  "Settings page UI component."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.util :refer [mac-platform?]]
   [freememo.storage-section :refer [StorageSection]]
   [freememo.cost-history :refer [CostHistorySection]]
   [freememo.ai-features-section :refer [AIFeaturesSection]]
   [freememo.zotero-client :as zc]
   [freememo.commands :as commands]
   #?(:clj [freememo.settings :as settings])))


;; Section anchor ids + sidebar labels, in display order.
;; Storage is folded into the Account tab.
(def section-defs
  [{:id "settings-account" :label "Account"}
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

;; Plugin .xpi download URL — points at the committed artifact in the repo.
;; /raw/ (not /blob/) so the browser downloads the binary instead of rendering
;; GitHub's HTML "binary file" preview page. Bump the version segment in the
;; filename when freememo-zotero-plugin/manifest.json version changes (the
;; build.sh output filename embeds it).
(def ^:private plugin-download-url
  "https://github.com/adham-omran/freememo/raw/main/freememo-zotero-plugin/dist/freememo-zotero-plugin-0.1.0.xpi")

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
                          ;; download attribute hints "save as" instead of
                          ;; navigating; GitHub's raw redirect already serves
                          ;; binary so this is belt-and-braces.
                          :download "freememo-zotero-plugin-0.1.0.xpi"
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

(e/defn SettingsPage [user-id username enc-key base-url client-country]
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
            (StorageSection user-id)

        ;; AI cost history — the user's credit ledger (searchable/filterable)
            (CostHistorySection user-id))


          (dom/div
            (dom/props {:id "settings-ai"
                        :style (tab-style active "settings-ai")})
            (AIFeaturesSection user-id enc-key base-url client-country))

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
                            (case (e/server (commands/bump! user-id :set-setting))
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
          (let [server-auto-mode (e/server (settings/get-anki-auto-load-mode user-id))
                !auto-mode (atom server-auto-mode)
                auto-mode (e/watch !auto-mode)]
            (dom/div
              (dom/props {:class "card" :id "settings-anki"
                          :style (tab-style active "settings-anki")})
              (dom/h3 (dom/props {:class "section-title"}) (dom/text "Anki Sync"))

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
                          (dom/text "Open the modal with default values; you set everything fresh each time.")))))))))

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
