(ns freememo.ai-features-section
  "AI Features card on the Settings page: LLM toggle, API key modal, reasoning,
   verbosity, scan DPI, system + OCR prompts. Extracted from settings_page so
   each e/defn stays under the JVM 64KB bytecode limit."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   #?(:clj [freememo.settings :as settings])
   #?(:clj [freememo.user-state :as us])))

(e/defn AIFeaturesSection [user-id enc-key]
  (e/client
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
                    (token)))))))))))
