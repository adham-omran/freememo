(ns freememo.settings-page
  "Settings page UI component."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.util :refer [mac-platform?]]
   #?(:clj [freememo.settings :as settings])
   #?(:clj [freememo.db :as db])
   #?(:clj [freememo.quota :as quota])
   #?(:clj [freememo.user-state :as us])))

;; Server-only wrappers — visible to both compilers, plain CLJ body.
(defn get-usage* [_refresh user-id]
  #?(:clj (quota/get-user-usage db/ds user-id)
     :cljs 0))

(defn get-quota* [_refresh user-id]
  #?(:clj (quota/get-user-quota db/ds user-id)
     :cljs 0))

;; Server-only formatters — `format` is CLJ-only; calling it from inside an
;; e/defn body captures the var, which Electric then tries to serialize.
(defn format-mb* [_refresh bytes]
  #?(:clj (let [mb (/ (double (or bytes 0)) 1048576.0)]
            (if (< mb 10) (format "%.2f MB" mb) (format "%.1f MB" mb)))
     :cljs ""))

(defn format-pct* [_refresh pct]
  #?(:clj (format "%.0f" (double (or pct 0)))
     :cljs ""))

(e/defn SettingsPage [user-id username enc-key]
  (e/client
    (dom/div
      (dom/props {:class "page-container"})

      ;; ── Storage section ──
      (let [refresh (e/server (e/watch (us/get-atom user-id :refresh)))
            usage (e/server (get-usage* refresh user-id))
            cap (e/server (get-quota* refresh user-id))
            pct (if (and cap (pos? cap)) (* 100.0 (/ (double usage) cap)) 0.0)
            used-mb (e/server (format-mb* refresh usage))
            cap-mb (e/server (format-mb* refresh cap))
            pct-str (e/server (format-pct* refresh pct))
            bar-color (cond (>= pct 100) "var(--color-danger)"
                            (>= pct 80)  "var(--color-warning, #c98a00)"
                            :else        "var(--color-primary)")
            bar-width-pct (if (>= pct 100) 100 pct)]
        (dom/div
          (dom/props {:class "card"})
          (dom/h3 (dom/props {:class "section-title"}) (dom/text "Storage"))
          (dom/div
            (dom/props {:style {:font-size "14px" :color "var(--color-text-primary)" :margin-bottom "8px"}})
            (dom/text (str used-mb " / " cap-mb " used"))
            (dom/span
              (dom/props {:style {:margin-left "8px" :color "var(--color-text-secondary)" :font-size "13px"}})
              (dom/text (str "(" pct-str "%)"))))
          (dom/div
            (dom/props {:style {:height "8px" :background "var(--color-bg-subtle)"
                                :border-radius "4px" :overflow "hidden"
                                :margin-bottom "8px"}})
            (dom/div
              (dom/props {:style {:height "100%"
                                  :width (str bar-width-pct "%")
                                  :background bar-color
                                  :transition "width 0.2s"}})))
          (dom/div
            (dom/props {:class "hint"})
            (dom/text "Storage is consumed by uploaded PDFs and EPUBs. Delete documents from the Library to free space."))))

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

      ;; ── AI Features section ──
      (let [server-llm-enabled (e/server (settings/get-llm-enabled user-id))
            !llm-enabled (atom server-llm-enabled)
            llm-enabled (e/watch !llm-enabled)
            !key-status-refresh (atom 0)
            key-status-refresh (e/watch !key-status-refresh)
            api-key-status (e/server
                             (do key-status-refresh
                               (settings/get-openai-api-key-status user-id enc-key)))
            api-key-source (:source api-key-status)
            !show-key-modal (atom false)
            show-key-modal (e/watch !show-key-modal)
            !draft-key (atom "")
            draft-key (e/watch !draft-key)
            !key-save-error (atom nil)
            key-save-error (e/watch !key-save-error)
            server-reasoning (e/server (settings/get-reasoning user-id))
            !reasoning (atom server-reasoning)
            reasoning (e/watch !reasoning)
            server-verbosity (e/server (settings/get-verbosity user-id))
            !verbosity (atom server-verbosity)
            verbosity (e/watch !verbosity)]

        (dom/div
          (dom/props {:class "card"})
          (dom/h3 (dom/props {:class "section-title"}) (dom/text "AI Features"))

          ;; BYOK explainer
          (dom/div
            (dom/props {:style {:padding "12px 14px" :background "var(--color-info-bg)" :border-radius "var(--radius-md)"
                                :margin-bottom "var(--sp-4)" :font-size "13px" :line-height "1.5"
                                :color "var(--color-text-secondary)"}})
            (dom/text "Incremental reading and Anki sync are always free. AI features (OCR and flashcard generation) use OpenAI and require your own API key -- bring your own key, pay only for what you use."))

          ;; LLM toggle
          (dom/div
            (dom/props {:class "field"})
            (dom/label
              (dom/props {:style {:display "flex" :align-items "center" :gap "10px" :cursor "pointer"}})
              (dom/input
                (dom/props {:type "checkbox" :checked llm-enabled
                            :style {:width "18px" :height "18px" :accent-color "var(--color-primary)"}})
                (let [change-event (dom/On "change" (fn [e] (-> e .-target .-checked)) nil)
                      [?token ?error] (e/Token change-event)]
                  (when (some? change-event)
                    (reset! !llm-enabled change-event))
                  (when-some [token ?token]
                    (e/server (settings/save-llm-enabled user-id change-event))
                    (e/server (swap! (us/get-atom user-id :settings-refresh) inc))
                    (token))))
              (dom/div
                (dom/span
                  (dom/props {:style {:font-size "14px" :font-weight "500" :color "var(--color-text-primary)"}})
                  (dom/text "Enable LLM features"))
                (dom/div
                  (dom/props {:class "hint"})
                  (dom/text "OCR text extraction and flashcard generation. Requires your own OpenAI API key.")))))

          (when llm-enabled
            ;; API Key
            (dom/div
              (dom/props {:class "field"
                          :style {:padding "14px" :background "var(--color-bg-subtle)"
                                  :border-radius "var(--radius-md)" :border "1px solid var(--color-bg-hover)"}})
              (dom/div
                (dom/props {:style {:display "flex" :align-items "center" :justify-content "space-between"
                                    :margin-bottom "4px"}})
                (dom/span
                  (dom/props {:style {:font-size "13px" :font-weight "500" :color "var(--color-text-label)"}})
                  (dom/text "OpenAI API Key"))
                (dom/span
                  (dom/props {:class (case api-key-source
                                       :user "badge badge-success"
                                       :shared "badge badge-warning"
                                       "badge badge-error")})
                  (dom/text (case api-key-source
                              :user "Configured"
                              :shared "Demo key"
                              "Not set"))))
              (dom/button
                (dom/props {:type "button"
                            :class "btn btn-secondary"
                            :style {:margin-top "8px" :padding "6px 14px"
                                    :font-size "13px" :color "var(--color-primary)"
                                    :border "1px solid var(--color-primary)"}})
                (dom/text (if (= api-key-source :user) "Update Key" "Set API Key"))
                (dom/On "click"
                  (fn [_]
                    (reset! !draft-key "")
                    (reset! !key-save-error nil)
                    (reset! !show-key-modal true))
                  nil)))

            ;; API Key Modal
            (when show-key-modal
              (dom/div
                (dom/props {:class "modal-backdrop" :tabindex "-1"})
                (dom/On "click" (fn [_] (reset! !show-key-modal false)) nil)
                (dom/On "keydown"
                  (fn [e]
                    #?(:cljs
                       (cond
                         (= (.-key e) "Escape")
                         (reset! !show-key-modal false)
                         (and (= (.-key e) "Enter") (or (.-metaKey e) (.-ctrlKey e)))
                         (when-let [btn (.querySelector (.-currentTarget e) ".btn-primary")]
                           (.preventDefault e)
                           (.click btn)))))
                  nil)
                (dom/div
                  (dom/props {:class "modal-content modal-md"})
                  (dom/On "click" (fn [e] (.stopPropagation e)) nil)
                  (dom/h3
                    (dom/props {:style {:margin-top "0" :margin-bottom "4px" :font-size "17px"}})
                    (dom/text "OpenAI API Key"))
                  (dom/p
                    (dom/props {:style {:margin-top "0" :margin-bottom "8px" :font-size "13px" :color "var(--color-text-hint)"}})
                    (dom/text "FreeMemo uses OpenAI to scan documents and generate flashcards. "))
                  (dom/p
                    (dom/props {:style {:margin-top "0" :margin-bottom "8px" :font-size "13px" :color "var(--color-text-hint)"}})
                    (dom/a
                      (dom/props {:href "https://platform.openai.com/api-keys" :target "_blank" :rel "noopener"
                                  :style {:color "var(--color-primary)" :text-decoration "underline"}})
                      (dom/text "Get your API key from OpenAI"))
                    (dom/text "."))
                  (dom/p
                    (dom/props {:style {:margin-top "0" :margin-bottom "16px" :font-size "13px" :color "var(--color-text-hint)"}})
                    (dom/text "Your key is encrypted and stored securely. Saving an empty value clears it."))
                  (dom/input
                    (dom/props {:type "password"
                                :value draft-key
                                :placeholder "sk-..."
                                :class "input input-full"
                                :style {:padding "10px 12px"}})
                    (let [input-event (dom/On "input" #(-> % .-target .-value) nil)]
                      (when (some? input-event)
                        (reset! !draft-key input-event))))
                  (when key-save-error
                    (dom/div
                      (dom/props {:style {:margin-top "10px" :font-size "13px" :color "var(--color-danger)"
                                          :padding "8px 10px" :background "var(--color-danger-bg)" :border-radius "var(--radius-sm)"}})
                      (dom/text key-save-error)))
                  (dom/div
                    (dom/props {:style {:display "flex" :justify-content "flex-end" :gap "10px"
                                        :margin-top "20px"}})
                    (dom/button
                      (let [click-event (dom/On "click" identity nil)
                            [?token _] (e/Token click-event)]
                        (dom/props {:type "button"
                                    :disabled (some? ?token)
                                    :class "btn btn-primary"
                                    :style {:order "1"}})
                        (dom/text (if (some? ?token) "Saving..." "Save"))
                        (when-some [token ?token]
                          (let [result (e/server (settings/save-openai-api-key user-id draft-key enc-key))]
                            (if (:success result)
                              (do
                                (reset! !draft-key "")
                                (reset! !key-save-error nil)
                                (swap! !key-status-refresh inc)
                                (reset! !show-key-modal false)
                                (token))
                              (let [err-msg (or (:error result) "Failed to save API key")]
                                (reset! !key-save-error err-msg)
                                (token err-msg)))))))
                    (dom/button
                      (dom/props {:type "button"
                                  :class "btn btn-secondary"})
                      (dom/text "Cancel")
                      (dom/On "click" (fn [_] (reset! !show-key-modal false)) nil))))))

            ;; Reasoning
            (dom/div
              (dom/props {:class "field"})
              (dom/label (dom/props {:class "label"}) (dom/text "Reasoning Effort"))
              (dom/select
                (dom/props {:value reasoning :class "select"})
                (dom/option (dom/props {:value "none"}) (dom/text "None"))
                (dom/option (dom/props {:value "minimal"}) (dom/text "Minimal"))
                (dom/option (dom/props {:value "low"}) (dom/text "Low"))
                (dom/option (dom/props {:value "medium"}) (dom/text "Medium"))
                (dom/option (dom/props {:value "high"}) (dom/text "High"))
                (let [change-event (dom/On "change" #(-> % .-target .-value) nil)
                      [?token ?error] (e/Token change-event)]
                  (when (some? change-event)
                    (reset! !reasoning change-event))
                  (when-some [token ?token]
                    (e/server (settings/save-reasoning user-id change-event))
                    (token))))
              (dom/div (dom/props {:class "hint"})
                (dom/text "Higher = better quality but slower and more expensive")))

            ;; Verbosity
            (dom/div
              (dom/props {:class "field"})
              (dom/label (dom/props {:class "label"}) (dom/text "Verbosity"))
              (dom/select
                (dom/props {:value verbosity :class "select"})
                (dom/option (dom/props {:value "low"}) (dom/text "Low"))
                (dom/option (dom/props {:value "medium"}) (dom/text "Medium"))
                (dom/option (dom/props {:value "high"}) (dom/text "High"))
                (let [change-event (dom/On "change" #(-> % .-target .-value) nil)
                      [?token ?error] (e/Token change-event)]
                  (when (some? change-event)
                    (reset! !verbosity change-event))
                  (when-some [token ?token]
                    (e/server (settings/save-verbosity user-id change-event))
                    (token))))
              (dom/div (dom/props {:class "hint"})
                (dom/text "Controls detail level of generated flashcards")))

            ;; Scan Quality (DPI)
            (let [server-dpi (e/server (settings/get-scan-dpi user-id))
                  !dpi (atom (str server-dpi))
                  dpi (e/watch !dpi)]
              (dom/div
                (dom/props {:class "field"})
                (dom/label (dom/props {:class "label"}) (dom/text "Scan Quality (DPI)"))
                (dom/select
                  (dom/props {:value dpi :class "select"})
                  (dom/option (dom/props {:value "72"}) (dom/text "Low (72 DPI)"))
                  (dom/option (dom/props {:value "150"}) (dom/text "Standard (150 DPI)"))
                  (dom/option (dom/props {:value "300"}) (dom/text "High (300 DPI)"))
                  (let [change-event (dom/On "change" #(-> % .-target .-value) nil)
                        [?token ?error] (e/Token change-event)]
                    (when (some? change-event)
                      (reset! !dpi change-event))
                    (when-some [token ?token]
                      (e/server (settings/save-scan-dpi user-id change-event))
                      (token))))
                (dom/div (dom/props {:class "hint"})
                  (dom/text "Higher quality improves text recognition but increases processing time and API cost"))))

            ;; ── Prompts (inside when llm-enabled) ──
            (let [default-sys (e/server (settings/get-default-system-prompt))
                  server-sys (e/server (settings/get-system-prompt user-id))
                  !sys-prompt (atom server-sys)
                  sys-prompt (e/watch !sys-prompt)
                  default-ocr (e/server (settings/get-default-ocr-prompt))
                  server-ocr (e/server (settings/get-ocr-prompt user-id))
                  !ocr-prompt (atom server-ocr)
                  ocr-prompt (e/watch !ocr-prompt)]

              ;; Card Generation System Prompt
              (dom/div
                (dom/props {:class "field"})
                (dom/label (dom/props {:class "label"}) (dom/text "Card Generation System Prompt"))
                (dom/div (dom/props {:class "hint" :style {:margin-bottom "8px"}})
                  (dom/text "Controls the persona, rules, and style for flashcard generation. Format-specific instructions (basic/cloze/context) are appended automatically."))
                (dom/textarea
                  (dom/props {:rows "20"
                              :style {:width "100%" :font-family "monospace" :font-size "12px" :line-height "1.5"
                                      :padding "10px" :border "1px solid var(--color-border)" :border-radius "var(--radius-sm)"
                                      :background "var(--color-bg-subtle)" :resize "vertical"
                                      :color "var(--color-text-primary)"}})
                  (set! (.-value dom/node) sys-prompt)
                  (let [change-event (dom/On "change" #(-> % .-target .-value) nil)
                        [?token _] (e/Token change-event)]
                    (when (some? change-event)
                      (reset! !sys-prompt change-event))
                    (when-some [token ?token]
                      (e/server (settings/save-system-prompt user-id change-event))
                      (token))))
                (dom/button
                  (dom/props {:type "button" :class "btn btn-secondary"
                              :disabled (= sys-prompt default-sys)
                              :style {:margin-top "8px" :padding "4px 12px" :font-size "12px"}})
                  (dom/text "Reset to Default")
                  (let [click-event (dom/On "click" identity nil)
                        [?token _] (e/Token click-event)]
                    (when-some [token ?token]
                      (e/server (settings/reset-system-prompt user-id))
                      (reset! !sys-prompt default-sys)
                      (token)))))

              ;; OCR Extraction Prompt
              (dom/div
                (dom/props {:class "field" :style {:margin-top "20px"}})
                (dom/label (dom/props {:class "label"}) (dom/text "OCR Extraction Prompt"))
                (dom/div (dom/props {:class "hint" :style {:margin-bottom "8px"}})
                  (dom/text "Instructions for extracting text from PDF page images. Controls how tables, headings, and structure are handled."))
                (dom/textarea
                  (dom/props {:rows "10"
                              :style {:width "100%" :font-family "monospace" :font-size "12px" :line-height "1.5"
                                      :padding "10px" :border "1px solid var(--color-border)" :border-radius "var(--radius-sm)"
                                      :background "var(--color-bg-subtle)" :resize "vertical"
                                      :color "var(--color-text-primary)"}})
                  (set! (.-value dom/node) ocr-prompt)
                  (let [change-event (dom/On "change" #(-> % .-target .-value) nil)
                        [?token _] (e/Token change-event)]
                    (when (some? change-event)
                      (reset! !ocr-prompt change-event))
                    (when-some [token ?token]
                      (e/server (settings/save-ocr-prompt user-id change-event))
                      (token))))
                (dom/button
                  (dom/props {:type "button" :class "btn btn-secondary"
                              :disabled (= ocr-prompt default-ocr)
                              :style {:margin-top "8px" :padding "4px 12px" :font-size "12px"}})
                  (dom/text "Reset to Default")
                  (let [click-event (dom/On "click" identity nil)
                        [?token _] (e/Token click-event)]
                    (when-some [token ?token]
                      (e/server (settings/reset-ocr-prompt user-id))
                      (reset! !ocr-prompt default-ocr)
                      (token))))))))

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
                      (dom/text "Open the modal with default values; you set everything fresh each time.")))))))))

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
