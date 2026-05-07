(ns freememo.settings-page
  "Settings page UI component."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.util :refer [mac-platform?]]
   [freememo.storage-section :refer [StorageSection]]
   [freememo.ai-features-section :refer [AIFeaturesSection]]
   #?(:clj [freememo.settings :as settings])
   #?(:clj [freememo.user-state :as us])))

(e/defn SettingsPage [user-id username enc-key]
  (e/client
    (dom/div
      (dom/props {:class "page-container"})

      (StorageSection user-id)

      ;; ── Account section ──
      (dom/div
        (dom/props {:class "card"})
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
                      [?token ?error] (e/Token change-event)]
                  (when (some? change-event)
                    (reset! !email-updates change-event))
                  (when-some [token ?token]
                    (e/server (settings/save-email-updates user-id change-event))
                    (token))))
              (dom/span
                (dom/props {:style {:font-size "14px" :font-weight "500" :color "var(--color-text-primary)"}})
                (dom/text "Subscribe to email updates"))))))

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
          (dom/props {:class "card"})
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
                      [?token _] (e/Token change-event)]
                  (when (some? change-event)
                    (reset! !ai-btn change-event))
                  (when-some [token ?token]
                    (e/server (settings/save-enable-ai-scan-button user-id change-event))
                    (token))))
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
                      [?token _] (e/Token change-event)]
                  (when (some? change-event)
                    (reset! !pdfbox-btn change-event))
                  (when-some [token ?token]
                    (e/server (settings/save-enable-pdfbox-button user-id change-event))
                    (token))))
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
                      [?token _] (e/Token change-event)]
                  (when (some? change-event)
                    (reset! !pdfjs-btn change-event))
                  (when-some [token ?token]
                    (e/server (settings/save-enable-pdfjs-button user-id change-event))
                    (token))))
              (dom/div
                (dom/span
                  (dom/props {:style {:font-size "14px" :font-weight "500" :color "var(--color-text-primary)"}})
                  (dom/text "Show \"Extract (PDF.js)\" button"))
                (dom/div
                  (dom/props {:class "hint"})
                  (dom/text "Client-side native text extraction using the loaded PDF.js viewer. No AI, no API key.")))))))

      (AIFeaturesSection user-id enc-key)

      ;; ── Appearance section ──
        (let [server-font-size (e/server (settings/get-card-font-size user-id))
              !font-size (atom server-font-size)
              font-size (e/watch !font-size)
              server-theme (e/server (settings/get-theme user-id))
              !theme (atom server-theme)
              theme (e/watch !theme)]
          (dom/div
            (dom/props {:class "card"})
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
                      [?token ?error] (e/Token change-event)]
                  (when (some? change-event)
                    (reset! !theme change-event))
                  (when-some [token ?token]
                    (e/server (settings/save-theme user-id change-event))
                    (e/server (swap! (us/get-atom user-id :settings-refresh) inc))
                    (token))))
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
                          [?token _] (e/Token change-event)]
                      (when (some? change-event)
                        (reset! !font-size change-event))
                      (when-some [token ?token]
                        (e/server (settings/save-card-font-size user-id change-event))
                        (token)))))
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
              server-auto-mode (e/server (settings/get-anki-auto-load-mode user-id))
              !auto-mode (atom server-auto-mode)
              auto-mode (e/watch !auto-mode)]
          (dom/div
            (dom/props {:class "card"})
            (dom/h3 (dom/props {:class "section-title"}) (dom/text "Anki Sync"))

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
                          [?token ?error] (e/Token change-event)]
                      (when (some? change-event)
                        (reset! !source-mode change-event))
                      (when-some [token ?token]
                        (e/server (settings/save-source-display-mode user-id change-event))
                        (e/server (swap! (us/get-atom user-id :settings-refresh) inc))
                        (token))))
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
                          [?token ?error] (e/Token change-event)]
                      (when (some? change-event)
                        (reset! !source-mode change-event))
                      (when-some [token ?token]
                        (e/server (settings/save-source-display-mode user-id change-event))
                        (e/server (swap! (us/get-atom user-id :settings-refresh) inc))
                        (token))))
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
                        [?token _] (e/Token change-event)]
                    (when (some? change-event)
                      (reset! !source-field change-event))
                    (when-some [token ?token]
                      (e/server (settings/save-anki-source-field user-id change-event))
                      (token))))
                (dom/div (dom/props {:class "hint"})
                  (dom/text "Anki field name that receives the source reference. Defaults to \"Source\"."))))

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
                          [?token _] (e/Token change-event)]
                      (when (some? change-event)
                        (reset! !auto-mode change-event))
                      (when-some [token ?token]
                        (e/server (settings/save-anki-auto-load-mode user-id change-event))
                        (token))))
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
                          [?token _] (e/Token change-event)]
                      (when (some? change-event)
                        (reset! !auto-mode change-event))
                      (when-some [token ?token]
                        (e/server (settings/save-anki-auto-load-mode user-id change-event))
                        (token))))
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
                          [?token _] (e/Token change-event)]
                      (when (some? change-event)
                        (reset! !auto-mode change-event))
                      (when-some [token ?token]
                        (e/server (settings/save-anki-auto-load-mode user-id change-event))
                        (token))))
                  (dom/div
                    (dom/span (dom/props {:style {:font-size "14px" :color "var(--color-text-primary)"}})
                      (dom/text "None (Manual)"))
                    (dom/div (dom/props {:class "hint"})
                      (dom/text "Open the modal with default values; you set everything fresh each time."))))))))

      ;; ── Keyboard Shortcuts section (read-only) ──
      (let [mac? (e/client (mac-platform?))
            mod-key (if mac? "Cmd" "Ctrl")]
        (dom/div
          (dom/props {:class "card"})
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
                    (dom/text (:desc s))))))))))))
