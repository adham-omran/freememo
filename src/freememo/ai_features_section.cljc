(ns freememo.ai-features-section
  "AI Features card on the Settings page: LLM toggle, provider-key status, card
   model, reasoning, verbosity, scan DPI, system + OCR prompts. Extracted from
   settings_page so each e/defn stays under the JVM 64KB bytecode limit."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.electric-forms5 :as forms]
   [clojure.string :as str]
   [freememo.home-page :refer [get-api-key-status*]]
   [freememo.ocr-models :as ocr-models]
   [freememo.card-models :as card-models]
   [freememo.commands :as commands]
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
  "Official-deployment credits panel: balance + top-up presets.
   Rendered in place of the key-status block when CREDITS_ENABLED is set (§5.8).
   `base-url` is the public origin (derived from ring-request at Main) — used
   for the Wayl webhook + redirection URLs so dev (localhost) and prod work
   without a config knob. `client-country` is the ISO-3166 alpha-2 code resolved
   from the client IP at session boot (nil = unknown → USD)."
  [user-id base-url client-country]
  (e/client
    (let [credits-refresh (e/server (e/watch (us/get-atom user-id :credits-refresh)))
          balance (e/server (credit-balance* credits-refresh user-id))
          presets (e/server (mapv (fn [amt] {:iqd amt :usd-str (credits/iqd->usd-str amt)})
                              (config/presets)))
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
            (dom/props {:style {:margin-top "8px" :font-size "13px" :color "var(--color-danger-text)"}})
            (dom/text checkout-error)))))))

(e/defn KgModelField
  "One per-step KG model selector. Owns its value atom + server read/save so the
   six steps render from data (kg-model-step-choices) without six copied blocks.
   card-model-ids/card-label-of are passed in from the parent (built once)."
  [user-id step label card-model-ids card-label-of]
  (e/client
    (let [server-val (e/server (settings/get-kg-model user-id step))
          !val (atom server-val)
          val (e/watch !val)]
      (dom/div
        (dom/props {:class "field" :style {:margin-left "12px"}})
        (dom/label (dom/props {:class "label"}) (dom/text label))
        (dom/select
          (dom/props {:value val :class "select"})
          (e/for [id (e/diff-by identity card-model-ids)]
            (dom/option (dom/props {:value id :selected (= id val)}) (dom/text (get card-label-of id id))))
          ;; A1-fallback: Forms5 has no tracked select
          (let [change-event (dom/On "change" #(-> % .-target .-value) nil)
                [t _] (e/Token change-event)]
            (dom/props {:disabled (some? t) :aria-busy (some? t)})
            (when (some? change-event)
              (reset! !val change-event))
            (when t
              (let [r (e/server (e/Offload #(settings/save-kg-model user-id step change-event)))]
                (case r
                  (if (:success r) (t) (t (:error r))))))))))))

;; ---------------------------------------------------------------------------
;; Field sub-components (split per the JVM 64KB method-limit convention) —
;; each field owns its own atom + server round trip, mirroring KgModelField
;; above, so AIFeaturesSection's body stays a flat list of calls. Behavior is
;; unchanged: these are the same reactively-independent siblings, just each
;; in its own e/defn.
;; ---------------------------------------------------------------------------

(e/defn LlmToggleField
  "The Enable-LLM-features checkbox. `!llm-enabled`/`llm-enabled` stay owned by
   the parent — the rest of the section's fields are gated on this value via
   `when`, so the atom can't move into this field alone (mirrors how
   ContentToolbar passes `!show-bib` into DocumentOptionsButton)."
  [user-id credits-enabled? !llm-enabled llm-enabled]
  (e/client
    (dom/div
      (dom/props {:class "field"})
      (dom/label
        (dom/props {:style {:display "flex" :align-items "center" :gap "10px" :cursor "pointer"}})
        (e/for [[t {:keys [llm-enabled]}] (forms/Checkbox! :llm-enabled llm-enabled
                                             :style {:width "18px" :height "18px" :accent-color "var(--color-primary)"})]
          (reset! !llm-enabled llm-enabled)
          (let [r (e/server (e/Offload #(settings/save-llm-enabled user-id llm-enabled)))]
            (case r
              (if (:success r)
                (case (e/server (commands/bump! user-id :set-setting))
                  (t))
                (t (:error r))))))
        (dom/div
          (dom/span
            (dom/props {:style {:font-size "14px" :font-weight "500" :color "var(--color-text-primary)"}})
            (dom/text "Enable LLM features"))
          (dom/div
            (dom/props {:class "hint"})
            (dom/text (if credits-enabled?
                        "OCR text extraction and flashcard generation. Uses platform credits — top up below."
                        "OCR text extraction and flashcard generation. Requires an OpenRouter API key."))))))))

(e/defn ProviderKeyStatusField
  "Self-host mode's OpenRouter API-key status card (credits mode shows
   CreditsSection instead — the parent picks between the two)."
  [api-key-configured?]
  (e/client
    (dom/div
      (dom/props {:class "field"
                  :style {:padding "14px" :background "var(--color-bg-subtle)"
                          :border-radius "var(--radius-md)" :border "1px solid var(--color-bg-hover)"}})
      (dom/div
        (dom/props {:style {:display "flex" :align-items "center" :justify-content "space-between"}})
        (dom/span
          (dom/props {:style {:font-size "13px" :font-weight "500" :color "var(--color-text-label)"}})
          (dom/text "OpenRouter API Key"))
        (dom/span
          (dom/props {:class (if api-key-configured? "badge badge-success" "badge badge-error")})
          (dom/text (if api-key-configured? "Configured" "Not set"))))
      (when-not api-key-configured?
        (dom/div (dom/props {:class "hint" :style {:margin-top "8px"}})
          (dom/text "Set PLATFORM_OPENROUTER_API_KEY in your environment or config.edn."))))))

(e/defn CardModelField
  "Card-generation model. Shown in all modes; in credits mode the options are
   the configured allow-list (config/card-model-allowlist), defaulting to
   !prod-model. card-model-ids/card-label-of are built once by the parent and
   shared with AssistantModelField/KgModelsField."
  [user-id card-model-ids card-label-of]
  (e/client
    (let [server-model (e/server (settings/get-model user-id))
          !model (atom server-model)
          model (e/watch !model)]
      (dom/div
        (dom/props {:class "field"})
        (dom/label (dom/props {:class "label"}) (dom/text "Card Model"))
        (dom/select
          (dom/props {:value model :class "select"})
          (e/for [id (e/diff-by identity card-model-ids)]
            (dom/option (dom/props {:value id :selected (= id model)}) (dom/text (get card-label-of id id))))
          ;; A1-fallback: Forms5 has no tracked select
          (let [change-event (dom/On "change" #(-> % .-target .-value) nil)
                [t _] (e/Token change-event)]
            (dom/props {:disabled (some? t) :aria-busy (some? t)})
            (when (some? change-event)
              (reset! !model change-event))
            (when t
              (let [r (e/server (e/Offload #(settings/save-model user-id change-event)))]
                (case r
                  (if (:success r) (t) (t (:error r))))))))
        (dom/div (dom/props {:class "hint"})
          (dom/text "Model used for flashcard generation."))))))

(e/defn AssistantModelField
  "Assistant model — Socratic reading-view chatbot. Same registry as card
   generation; defaults to Gemini 3 Flash."
  [user-id card-model-ids card-label-of]
  (e/client
    (let [server-assistant-model (e/server (settings/get-assistant-model user-id))
          !assistant-model (atom server-assistant-model)
          assistant-model (e/watch !assistant-model)]
      (dom/div
        (dom/props {:class "field"})
        (dom/label (dom/props {:class "label"}) (dom/text "Assistant Model"))
        (dom/select
          (dom/props {:value assistant-model :class "select"})
          (e/for [id (e/diff-by identity card-model-ids)]
            (dom/option (dom/props {:value id :selected (= id assistant-model)}) (dom/text (get card-label-of id id))))
          ;; A1-fallback: Forms5 has no tracked select
          (let [change-event (dom/On "change" #(-> % .-target .-value) nil)
                [t _] (e/Token change-event)]
            (dom/props {:disabled (some? t) :aria-busy (some? t)})
            (when (some? change-event)
              (reset! !assistant-model change-event))
            (when t
              (let [r (e/server (e/Offload #(settings/save-assistant-model user-id change-event)))]
                (case r
                  (if (:success r) (t) (t (:error r))))))))
        (dom/div (dom/props {:class "hint"})
          (dom/text "Model used by the Socratic AI assistant in the reading view."))))))

(e/defn KgModelsField
  "Knowledge-graph models — one KgModelField per KG pipeline step, rendered
   from the step registry (settings/kg-model-step-choices). Same registry as
   card generation; each defaults to Gemini 3 Flash."
  [user-id card-model-ids card-label-of]
  (e/client
    (let [kg-steps (e/server (settings/kg-model-step-choices))]
      (dom/div
        (dom/props {:class "field"})
        (dom/label (dom/props {:class "label"}) (dom/text "Knowledge-graph models"))
        (dom/div (dom/props {:class "hint" :style {:margin-bottom "8px"}})
          (dom/text "Model used for each knowledge-graph step. Defaults to Gemini 3 Flash."))
        (e/for [pair (e/diff-by first kg-steps)]
          (let [[step label] pair]
            (KgModelField user-id step label card-model-ids card-label-of)))))))

(e/defn ReasoningField
  "Reasoning-effort select for flashcard generation."
  [user-id]
  (e/client
    (let [server-reasoning (e/server (settings/get-reasoning user-id))
          !reasoning (atom server-reasoning)
          reasoning (e/watch !reasoning)]
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
          ;; A1-fallback: Forms5 has no tracked select
          (let [change-event (dom/On "change" #(-> % .-target .-value) nil)
                [t ?error] (e/Token change-event)]
            (dom/props {:disabled (some? t) :aria-busy (some? t)})
            (when (some? change-event)
              (reset! !reasoning change-event))
            (when t
              (let [r (e/server (e/Offload #(settings/save-reasoning user-id change-event)))]
                (case r
                  (if (:success r) (t) (t (:error r))))))))
        (dom/div (dom/props {:class "hint"})
          (dom/text "Higher = better quality but slower and more expensive"))))))

(e/defn VerbosityField
  "Verbosity select for flashcard generation."
  [user-id]
  (e/client
    (let [server-verbosity (e/server (settings/get-verbosity user-id))
          !verbosity (atom server-verbosity)
          verbosity (e/watch !verbosity)]
      (dom/div
        (dom/props {:class "field"})
        (dom/label (dom/props {:class "label"}) (dom/text "Verbosity"))
        (dom/select
          (dom/props {:value verbosity :class "select"})
          (dom/option (dom/props {:value "low"}) (dom/text "Low"))
          (dom/option (dom/props {:value "medium"}) (dom/text "Medium"))
          (dom/option (dom/props {:value "high"}) (dom/text "High"))
          ;; A1-fallback: Forms5 has no tracked select
          (let [change-event (dom/On "change" #(-> % .-target .-value) nil)
                [t ?error] (e/Token change-event)]
            (dom/props {:disabled (some? t) :aria-busy (some? t)})
            (when (some? change-event)
              (reset! !verbosity change-event))
            (when t
              (let [r (e/server (e/Offload #(settings/save-verbosity user-id change-event)))]
                (case r
                  (if (:success r) (t) (t (:error r))))))))
        (dom/div (dom/props {:class "hint"})
          (dom/text "Controls detail level of generated flashcards"))))))

(e/defn ScanDpiField
  "Scan-quality (DPI) used for Scan Page OCR. Free-numeric Input! (72-300);
   settings/save-scan-dpi clamps server-side regardless of the client value."
  [user-id]
  (e/client
    (let [server-dpi (e/server (settings/get-scan-dpi user-id))
          !dpi (atom (str server-dpi))
          dpi (e/watch !dpi)]
      (dom/div
        (dom/props {:class "field"})
        (dom/label (dom/props {:class "label"}) (dom/text "Scan Quality (DPI)"))
        (e/for [[t {:keys [dpi]}] (forms/Input! :dpi dpi :type "number" :min "72" :max "300" :class "input")]
          (let [r (e/server (e/Offload #(settings/save-scan-dpi user-id dpi)))]
            (case r
              (if (:success r) (do (reset! !dpi dpi) (t)) (t (:error r))))))
        (dom/div (dom/props {:class "hint"})
          (dom/text "Higher quality improves text recognition but increases processing time and API cost"))))))

(e/defn AssistantPdfContextField
  "Pages before AND after the current page the Socratic assistant reads
   (0 = current page only)."
  [user-id]
  (e/client
    (let [server-window (e/server (settings/get-assistant-pdf-window user-id))
          !window (atom server-window)
          window (e/watch !window)]
      (dom/div
        (dom/props {:class "field"})
        (dom/label
          (dom/props {:style {:display "flex" :align-items "center" :gap "10px"}})
          (dom/span (dom/props {:class "label" :style {:margin-bottom "0"}})
            (dom/text "Assistant PDF context"))
          (e/for [[t {:keys [window]}] (forms/Input! :window window :type "number" :min "0" :max "50"
                                          :style {:width "56px" :font-size "13px" :padding "4px 6px"
                                                  :border "1px solid var(--color-border)" :border-radius "var(--radius-sm)"})]
            (let [r (e/server (e/Offload #(settings/save-assistant-pdf-window user-id window)))]
              (case r
                (if (:success r) (do (reset! !window window) (t)) (t (:error r))))))
          (dom/span (dom/props {:style {:font-size "13px" :color "var(--color-text-secondary)"}})
            (dom/text "pages")))
        (dom/div (dom/props {:class "hint"})
          (dom/text "Pages before and after the current page the assistant reads (0-50)"))))))

(e/defn DefaultOcrModelField
  "Default OCR model (Scan Page) — a document may override it in Document
   options."
  [user-id]
  (e/client
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
            (dom/option (dom/props {:value id :selected (= id ocr-model)}) (dom/text (get label-of id id))))
          ;; A1-fallback: Forms5 has no tracked select
          (let [change-event (dom/On "change" #(-> % .-target .-value) nil)
                [t ?error] (e/Token change-event)]
            (dom/props {:disabled (some? t) :aria-busy (some? t)})
            (when (some? change-event)
              (reset! !ocr-model change-event))
            (when t
              (let [r (e/server (e/Offload #(settings/save-ocr-model-default user-id change-event)))]
                (case r
                  (if (:success r) (t) (t (:error r))))))))
        (dom/div (dom/props {:class "hint"})
          (dom/text "Used for Scan Page unless a document overrides it in Document options"))))))

(e/defn CardGenRetriesField
  "Card generation retries — all attempts are billed (§5.4.5). Free-numeric
   Input! (1-3); settings/save-card-gen-max-retries clamps server-side."
  [user-id]
  (e/client
    (let [server-retries (e/server (settings/get-card-gen-max-retries user-id))
          !retries (atom (str server-retries))
          retries (e/watch !retries)]
      (dom/div
        (dom/props {:class "field"})
        (dom/label (dom/props {:class "label"}) (dom/text "Card Generation Retries"))
        (e/for [[t {:keys [retries]}] (forms/Input! :retries retries :type "number" :min "1" :max "3" :class "input")]
          (let [r (e/server (e/Offload #(settings/save-card-gen-max-retries user-id retries)))]
            (case r
              (if (:success r) (do (reset! !retries retries) (t)) (t (:error r))))))
        (dom/div (dom/props {:class "hint"})
          (dom/text "If the model returns the wrong number of cards, retry up to N times. Each attempt uses tokens and is billed."))))))

(e/defn SystemPromptField
  "Card Generation System Prompt — collapsed accordion. Controls the persona,
   rules, and style for flashcard generation; format-specific instructions
   (basic/cloze/context) are appended automatically."
  [user-id]
  (e/client
    (let [default-sys (e/server (settings/get-default-system-prompt))
          server-sys (e/server (settings/get-system-prompt user-id))
          !sys-prompt (atom server-sys)
          sys-prompt (e/watch !sys-prompt)]
      (dom/details
        (dom/props {:class "settings-accordion"})
        (dom/summary
          (dom/props {:class "settings-accordion__summary"})
          (dom/text "Card Generation System Prompt"))
        (dom/div
          (dom/props {:class "settings-accordion__body"})
          (dom/div (dom/props {:class "hint" :style {:margin-bottom "8px"}})
            (dom/text "Controls the persona, rules, and style for flashcard generation. Format-specific instructions (basic/cloze/context) are appended automatically."))
          (e/for [[t {:keys [sys-prompt]}] (forms/Input! :sys-prompt sys-prompt :as :textarea
                                              :rows "20"
                                              :style {:width "100%" :font-family "monospace" :font-size "12px" :line-height "1.5"
                                                      :padding "10px" :border "1px solid var(--color-border)" :border-radius "var(--radius-sm)"
                                                      :background "var(--color-bg-subtle)" :resize "vertical"
                                                      :color "var(--color-text-primary)"})]
            (let [r (e/server (e/Offload #(settings/save-system-prompt user-id sys-prompt)))]
              (case r
                (if (:success r) (do (reset! !sys-prompt sys-prompt) (t)) (t (:error r))))))
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
                      (t (:error r)))))))))))))

(e/defn OcrPromptField
  "OCR Extraction Prompt — collapsed accordion. Instructions for extracting
   text from PDF page images; controls how tables, headings, and structure
   are handled."
  [user-id]
  (e/client
    (let [default-ocr (e/server (settings/get-default-ocr-prompt))
          server-ocr (e/server (settings/get-ocr-prompt user-id))
          !ocr-prompt (atom server-ocr)
          ocr-prompt (e/watch !ocr-prompt)]
      (dom/details
        (dom/props {:class "settings-accordion" :style {:margin-top "12px"}})
        (dom/summary
          (dom/props {:class "settings-accordion__summary"})
          (dom/text "OCR Extraction Prompt"))
        (dom/div
          (dom/props {:class "settings-accordion__body"})
          (dom/div (dom/props {:class "hint" :style {:margin-bottom "8px"}})
            (dom/text "Instructions for extracting text from PDF page images. Controls how tables, headings, and structure are handled."))
          (e/for [[t {:keys [ocr-prompt]}] (forms/Input! :ocr-prompt ocr-prompt :as :textarea
                                              :rows "10"
                                              :style {:width "100%" :font-family "monospace" :font-size "12px" :line-height "1.5"
                                                      :padding "10px" :border "1px solid var(--color-border)" :border-radius "var(--radius-sm)"
                                                      :background "var(--color-bg-subtle)" :resize "vertical"
                                                      :color "var(--color-text-primary)"})]
            (let [r (e/server (e/Offload #(settings/save-ocr-prompt user-id ocr-prompt)))]
              (case r
                (if (:success r) (do (reset! !ocr-prompt ocr-prompt) (t)) (t (:error r)))))))
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
                    (t (:error r))))))))))))

(e/defn AIFeaturesSection [user-id enc-key base-url client-country]
  (e/client
    (let [server-llm-enabled (e/server (settings/get-llm-enabled user-id))
          !llm-enabled (atom server-llm-enabled)
          llm-enabled (e/watch !llm-enabled)
          settings-refresh (e/server (e/watch (us/get-atom user-id :settings-refresh)))
          api-key-status (e/server (get-api-key-status* settings-refresh user-id enc-key))
          api-key-configured? (:configured? api-key-status)
          credits-enabled? (e/server (config/credits-enabled?))
          card-model-ids (e/server (settings/card-model-ids))
          card-label-of (into {} (map (juxt :id :label)) card-models/registry)]

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
                      "Incremental reading and Anki sync are always free. AI features (OCR, flashcard generation, transcription) use OpenRouter and require an OpenRouter API key set by the operator.")))

        (LlmToggleField user-id credits-enabled? !llm-enabled llm-enabled)

        (when llm-enabled
          ;; Credits panel (official) or provider-key status (self-host).
          (if credits-enabled?
            (CreditsSection user-id base-url client-country)
            (ProviderKeyStatusField api-key-configured?))

          (CardModelField user-id card-model-ids card-label-of)

          (AssistantModelField user-id card-model-ids card-label-of)

          (KgModelsField user-id card-model-ids card-label-of)

          (ReasoningField user-id)

          (VerbosityField user-id)

          (ScanDpiField user-id)

          (AssistantPdfContextField user-id)

          (DefaultOcrModelField user-id)

          (CardGenRetriesField user-id)

          ;; ── Prompts (inside when llm-enabled) ──
          (SystemPromptField user-id)
          (OcrPromptField user-id))))))
