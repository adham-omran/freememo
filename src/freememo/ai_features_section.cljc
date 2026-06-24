(ns freememo.ai-features-section
  "AI Features card on the Settings page: LLM toggle, API key modal, reasoning,
   verbosity, scan DPI, system + OCR prompts. Extracted from settings_page so
   each e/defn stays under the JVM 64KB bytecode limit."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [clojure.string :as str]
   [freememo.home-page :refer [get-api-key-status*]]
   [freememo.ocr-models :as ocr-models]
   #?(:clj [freememo.ocr :as ocr])
   #?(:clj [freememo.settings :as settings])
   #?(:clj [freememo.config :as config])
   #?(:clj [freememo.credits :as credits])
   #?(:clj [freememo.db :as db])
   #?(:clj [freememo.user-state :as us])))

;; Defined on both platforms (per CLAUDE.md) so referencing them in e/defn
;; bodies never causes a CLJ/CLJS frame-signal mismatch.
#?(:cljs (defn navigate-external! [url] (when url (set! (.. js/window -location -href) url)))
   :clj (defn navigate-external! [_url] nil))

#?(:clj (defn credit-balance*
          "Reactive wrapper — _refresh forces a re-query on :credits-refresh bump."
          [_refresh user-id]
          (db/get-credit-balance user-id)))

(e/defn CreditsSection
  "Official-deployment credits panel: balance, top-up presets, cost estimates.
   Rendered in place of the BYO-key block when CREDITS_ENABLED is set (§5.8).
   `base-url` is the public origin (derived from ring-request at Main) — used
   for the Wayl webhook + redirection URLs so dev (localhost) and prod work
   without a config knob. `client-country` is the ISO-3166 alpha-2 code resolved
   from the client IP at session boot (nil = unknown → USD)."
  [user-id model base-url client-country]
  (e/client
    (let [credits-refresh (e/server (e/watch (us/get-atom user-id :credits-refresh)))
          balance (e/server (credit-balance* credits-refresh user-id))
          presets (e/server (mapv (fn [amt] {:iqd amt :usd-str (credits/iqd->usd-str amt)})
                              (config/presets)))
          estimates (e/server (credits/cost-estimates user-id model))
          !checkout-error (atom nil)
          checkout-error (e/watch !checkout-error)]
      (dom/div
        (dom/props {:class "field"
                    :style {:padding "14px" :background "var(--color-bg-subtle)"
                            :border-radius "var(--radius-md)" :border "1px solid var(--color-bg-hover)"}})
        (dom/div
          (dom/props {:style {:display "flex" :align-items "center" :justify-content "space-between"
                              :margin-bottom "10px"}})
          (dom/span
            (dom/props {:style {:font-size "13px" :font-weight "500" :color "var(--color-text-label)"}})
            (dom/text "Credit Balance"))
          (dom/span
            (dom/props {:style {:font-size "16px" :font-weight "600"
                                :color (if (and balance (> balance 0))
                                         "var(--color-text-primary)" "var(--color-danger)")}})
            (dom/text (str (or balance 0) " credits"))))

        (dom/div (dom/props {:class "hint" :style {:margin-bottom "6px"}}) (dom/text "Top up:"))
        (dom/div
          (dom/props {:style {:display "flex" :gap "8px" :flex-wrap "wrap"}})
          (e/for [{:keys [iqd usd-str]} (e/diff-by :iqd presets)]
            (dom/button
              (dom/props {:type "button" :class "btn btn-secondary"
                          :style {:padding "6px 14px" :font-size "13px" :cursor "pointer"}})
              (dom/text (str iqd " credits" (when usd-str (str " (" usd-str ")"))))
              (let [ev (dom/On "click" identity nil)
                    [t _] (e/Token ev)]
                (when t
                  (let [r (e/server (e/Offload #(credits/start-checkout! user-id iqd base-url client-country)))]
                    (case r
                      (if (:ok r)
                        (do (navigate-external! (:url r)) (t))
                        (do (reset! !checkout-error (:error r)) (t (:error r)))))))))))

        (dom/div
          (dom/props {:class "hint" :style {:margin-top "8px" :font-size "12px"
                                            :color "var(--color-text-secondary)"}})
          (dom/text "USD prices shown here are approximate and may be adjusted if we change AI models. The amount you pay is the credits amount shown."))

        (when checkout-error
          (dom/div
            (dom/props {:style {:margin-top "8px" :font-size "13px" :color "var(--color-danger)"}})
            (dom/text checkout-error)))

        (when estimates
          (dom/div
            (dom/props {:style {:margin-top "12px"}})
            (dom/div (dom/props {:class "hint" :style {:margin-bottom "4px"}})
              (dom/text "Typical cost:"))
            (dom/table
              (dom/props {:style {:width "100%" :font-size "12px" :border-collapse "collapse"}})
              (e/for [{:keys [label unit-cost]} (e/diff-by :label estimates)]
                (dom/tr
                  (dom/td (dom/props {:style {:padding "2px 0" :color "var(--color-text-secondary)"}})
                    (dom/text label))
                  (dom/td (dom/props {:style {:padding "2px 0" :text-align "right"
                                              :color "var(--color-text-primary)"}})
                    (dom/text (str "~" unit-cost " credits"))))))
            (when (pos? (or balance 0))
              (dom/div
                (dom/props {:class "hint" :style {:margin-top "8px"}})
                (dom/text
                  (str "Your " balance " credits ≈ "
                    (str/join " or "
                      (mapv (fn [{:keys [iqd units-per-action unit]}]
                              (str "~" (* (quot balance iqd) units-per-action) " " unit))
                        estimates))))))))))))

(e/defn AIFeaturesSection [user-id enc-key base-url client-country]
  (e/client
    (let [server-llm-enabled (e/server (settings/get-llm-enabled user-id))
          !llm-enabled (atom server-llm-enabled)
          llm-enabled (e/watch !llm-enabled)
          settings-refresh (e/server (e/watch (us/get-atom user-id :settings-refresh)))
          api-key-status (e/server (get-api-key-status* settings-refresh user-id enc-key))
          api-key-source (:source api-key-status)
          credits-enabled? (e/server (config/credits-enabled?))
          !show-key-modal (atom false)
          show-key-modal (e/watch !show-key-modal)
          !draft-key (atom "")
          draft-key (e/watch !draft-key)
          !key-save-error (atom nil)
          key-save-error (e/watch !key-save-error)
          server-model (e/server (settings/get-model user-id))
          !model (atom server-model)
          model (e/watch !model)
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
          (dom/text (if credits-enabled?
                      "Incremental reading and Anki sync are always free. OCR and flashcard generation spend credits — top up below."
                      "Incremental reading and Anki sync are always free. AI features (OCR and flashcard generation) use OpenAI and require your own API key -- bring your own key, pay only for what you use.")))

        ;; LLM toggle
        (dom/div
          (dom/props {:class "field"})
          (dom/label
            (dom/props {:style {:display "flex" :align-items "center" :gap "10px" :cursor "pointer"}})
            (dom/input
              (dom/props {:type "checkbox" :checked llm-enabled
                          :style {:width "18px" :height "18px" :accent-color "var(--color-primary)"}})
              (let [change-event (dom/On "change" (fn [e] (-> e .-target .-checked)) nil)
                    [t ?error] (e/Token change-event)]
                (when (some? change-event)
                  (reset! !llm-enabled change-event))
                (when t
                  (let [r (e/server (e/Offload #(settings/save-llm-enabled user-id change-event)))]
                    (case r
                      (if (:success r)
                        (case (e/server (swap! (us/get-atom user-id :settings-refresh) inc))
                          (t))
                        (t (:error r))))))))
            (dom/div
              (dom/span
                (dom/props {:style {:font-size "14px" :font-weight "500" :color "var(--color-text-primary)"}})
                (dom/text "Enable LLM features"))
              (dom/div
                (dom/props {:class "hint"})
                (dom/text (if credits-enabled?
                            "OCR text extraction and flashcard generation. Uses platform credits — top up below."
                            "OCR text extraction and flashcard generation. Requires your own OpenAI API key."))))))

        (when llm-enabled
          ;; Credits panel (official, CREDITS_ENABLED) or BYO-key block (self-host)
          (if credits-enabled?
            (CreditsSection user-id model base-url client-country)
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
                  nil))))

          ;; API Key Modal (self-host only)
          (when (and (not credits-enabled?) show-key-modal)
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
                          [t _] (e/Token click-event)]
                      (dom/props {:type "button"
                                  :disabled (some? t)
                                  :class "btn btn-primary"
                                  :style {:order "1"}})
                      (dom/text (if (some? t) "Saving..." "Save"))
                      (when t
                        (let [result (e/server (e/Offload #(settings/save-openai-api-key user-id draft-key enc-key)))]
                          (case result
                            (if (:success result)
                              (do (e/on-unmount #(do (reset! !draft-key "")
                                                   (reset! !key-save-error nil)
                                                   (reset! !show-key-modal false)))
                                (case (e/server (swap! (us/get-atom user-id :settings-refresh) inc))
                                  (t)))
                              (let [err-msg (or (:error result) "Failed to save API key")]
                                (reset! !key-save-error err-msg)
                                (t err-msg))))))))
                  (dom/button
                    (dom/props {:type "button"
                                :class "btn btn-secondary"})
                    (dom/text "Cancel")
                    (dom/On "click" (fn [_] (reset! !show-key-modal false)) nil))))))

          ;; Model — drives both OCR and card generation. Hidden in prod
          ;; (credits-enabled?) where the model is pinned via
          ;; freememo.config/!prod-model in src-prod/prod.cljc.
          (when-not credits-enabled?
            (dom/div
              (dom/props {:class "field"})
              (dom/label (dom/props {:class "label"}) (dom/text "Model"))
              (dom/select
                (dom/props {:value model :class "select"})
                (dom/option (dom/props {:value "gpt-5.1"}) (dom/text "gpt-5.1"))
                (let [change-event (dom/On "change" #(-> % .-target .-value) nil)
                      [t _] (e/Token change-event)]
                  (when (some? change-event)
                    (reset! !model change-event))
                  (when t
                    (let [r (e/server (e/Offload #(settings/save-model user-id change-event)))]
                      (case r
                        (if (:success r) (t) (t (:error r))))))))
              (dom/div (dom/props {:class "hint"})
                (dom/text "OpenAI model used for OCR and flashcard generation."))))

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
                    [t ?error] (e/Token change-event)]
                (when (some? change-event)
                  (reset! !reasoning change-event))
                (when t
                  (let [r (e/server (e/Offload #(settings/save-reasoning user-id change-event)))]
                    (case r
                      (if (:success r) (t) (t (:error r))))))))
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
                    [t ?error] (e/Token change-event)]
                (when (some? change-event)
                  (reset! !verbosity change-event))
                (when t
                  (let [r (e/server (e/Offload #(settings/save-verbosity user-id change-event)))]
                    (case r
                      (if (:success r) (t) (t (:error r))))))))
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
                      [t ?error] (e/Token change-event)]
                  (when (some? change-event)
                    (reset! !dpi change-event))
                  (when t
                    (let [r (e/server (e/Offload #(settings/save-scan-dpi user-id change-event)))]
                      (case r
                        (if (:success r) (t) (t (:error r))))))))
              (dom/div (dom/props {:class "hint"})
                (dom/text "Higher quality improves text recognition but increases processing time and API cost"))))

          ;; Default OCR Model (Scan Page) — a document may override it in Document options
          (let [server-ocr-model (e/server (settings/get-ocr-model-default user-id))
                allowed-ids (e/server (ocr/allowed-ocr-model-ids))
                label-of (into {} (map (juxt :id :label)) ocr-models/registry)
                !ocr-model (atom (or server-ocr-model ocr-models/default-id))
                ocr-model (e/watch !ocr-model)]
            (dom/div
              (dom/props {:class "field"})
              (dom/label (dom/props {:class "label"}) (dom/text "Default OCR Model"))
              (dom/select
                (dom/props {:value ocr-model :class "select"})
                (e/for [id (e/diff-by identity allowed-ids)]
                  (dom/option (dom/props {:value id}) (dom/text (get label-of id id))))
                (let [change-event (dom/On "change" #(-> % .-target .-value) nil)
                      [t ?error] (e/Token change-event)]
                  (when (some? change-event)
                    (reset! !ocr-model change-event))
                  (when t
                    (let [r (e/server (e/Offload #(settings/save-ocr-model-default user-id change-event)))]
                      (case r
                        (if (:success r) (t) (t (:error r))))))))
              (dom/div (dom/props {:class "hint"})
                (dom/text "Used for Scan Page unless a document overrides it in Document options"))))

          ;; Card Generation Retries — all attempts are billed (§5.4.5)
          (let [server-retries (e/server (settings/get-card-gen-max-retries user-id))
                !retries (atom (str server-retries))
                retries (e/watch !retries)]
            (dom/div
              (dom/props {:class "field"})
              (dom/label (dom/props {:class "label"}) (dom/text "Card Generation Retries"))
              (dom/select
                (dom/props {:value retries :class "select"})
                (dom/option (dom/props {:value "1"}) (dom/text "1 (no retry)"))
                (dom/option (dom/props {:value "2"}) (dom/text "2"))
                (dom/option (dom/props {:value "3"}) (dom/text "3"))
                (let [change-event (dom/On "change" #(-> % .-target .-value) nil)
                      [t ?error] (e/Token change-event)]
                  (when (some? change-event)
                    (reset! !retries change-event))
                  (when t
                    (let [r (e/server (e/Offload #(settings/save-card-gen-max-retries user-id change-event)))]
                      (case r
                        (if (:success r) (t) (t (:error r))))))))
              (dom/div (dom/props {:class "hint"})
                (dom/text "If the model returns the wrong number of cards, retry up to N times. Each attempt uses tokens and is billed."))))

          ;; ── Prompts (inside when llm-enabled) ──
          (let [default-sys (e/server (settings/get-default-system-prompt))
                server-sys (e/server (settings/get-system-prompt user-id))
                !sys-prompt (atom server-sys)
                sys-prompt (e/watch !sys-prompt)
                default-ocr (e/server (settings/get-default-ocr-prompt))
                server-ocr (e/server (settings/get-ocr-prompt user-id))
                !ocr-prompt (atom server-ocr)
                ocr-prompt (e/watch !ocr-prompt)]

            ;; Card Generation System Prompt — collapsed by default
            (dom/details
              (dom/props {:class "settings-accordion"})
              (dom/summary
                (dom/props {:class "settings-accordion__summary"})
                (dom/text "Card Generation System Prompt"))
              (dom/div
                (dom/props {:class "settings-accordion__body"})
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
                        [t _] (e/Token change-event)]
                    (when (some? change-event)
                      (reset! !sys-prompt change-event))
                    (when t
                      (let [r (e/server (e/Offload #(settings/save-system-prompt user-id change-event)))]
                        (case r
                          (if (:success r) (t) (t (:error r))))))))
                (dom/button
                  (dom/props {:type "button" :class "btn btn-secondary"
                              :disabled (= sys-prompt default-sys)
                              :style {:margin-top "8px" :padding "4px 12px" :font-size "12px"}})
                  (dom/text "Reset to Default")
                  (let [click-event (dom/On "click" identity nil)
                        [t _] (e/Token click-event)]
                    (when t
                      (let [r (e/server (e/Offload #(settings/reset-system-prompt user-id)))]
                        (case r
                          (if (:success r)
                            (do (reset! !sys-prompt default-sys) (t))
                            (t (:error r))))))))))

            ;; OCR Extraction Prompt — collapsed by default
            (dom/details
              (dom/props {:class "settings-accordion" :style {:margin-top "12px"}})
              (dom/summary
                (dom/props {:class "settings-accordion__summary"})
                (dom/text "OCR Extraction Prompt"))
              (dom/div
                (dom/props {:class "settings-accordion__body"})
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
                        [t _] (e/Token change-event)]
                    (when (some? change-event)
                      (reset! !ocr-prompt change-event))
                    (when t
                      (let [r (e/server (e/Offload #(settings/save-ocr-prompt user-id change-event)))]
                        (case r
                          (if (:success r) (t) (t (:error r)))))))))
              (dom/button
                (dom/props {:type "button" :class "btn btn-secondary"
                            :disabled (= ocr-prompt default-ocr)
                            :style {:margin-top "8px" :padding "4px 12px" :font-size "12px"}})
                (dom/text "Reset to Default")
                (let [click-event (dom/On "click" identity nil)
                      [t _] (e/Token click-event)]
                  (when t
                    (let [r (e/server (e/Offload #(settings/reset-ocr-prompt user-id)))]
                      (case r
                        (if (:success r)
                          (do (reset! !ocr-prompt default-ocr) (t))
                          (t (:error r)))))))))))))))
