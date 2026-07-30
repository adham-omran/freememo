(ns freememo.ai-features-section
  "AI Features card on the Settings page: LLM toggle, provider-key status, card
   model, reasoning, verbosity, scan DPI, transcription language, system + OCR
   prompts. Extracted from settings_page so each e/defn stays under the JVM 64KB
   bytecode limit.

   The credit balance and top-up presets are NOT here — they moved to
   freememo.credits-section, rendered from the Account tab, because storage rent
   spends credits whether or not AI features are enabled."
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
   [freememo.transcribe-language :as tlang]
   #?(:clj [freememo.settings :as settings])
   #?(:clj [freememo.config :as config])
   #?(:clj [freememo.db :as db])
   #?(:clj [freememo.user-state :as us])))

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
                        "OCR text extraction and flashcard generation. Uses platform credits — see the Account tab."
                        "OCR text extraction and flashcard generation. Requires an OpenRouter API key."))))))))

(e/defn ProviderKeyStatusField
  "Self-host mode's OpenRouter API-key status card. Credits mode has no
   provider key of its own — the platform holds it — and shows the balance in
   the Account tab instead (freememo.credits-section)."
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

(e/defn CardQuizGradingField
  "Whether the Quiz grades CARD items with the LLM, or the learner rates them 1-4.

   Card items carry no facts, so the LLM can only grade against the card's own
   answer — a weaker rubric than a question's fact set, and the reason this switch
   exists (plans/cards-in-quiz-queue.md D3). Questions are always LLM-graded and
   this never affects them.

   Lives inside the parent's `when llm-enabled` block, so with LLM features off it
   is not shown AND settings/card-quiz-llm-grading? already reads false — the Quiz
   falls back to self-rating rather than offering an arm it cannot run."
  [user-id credits-enabled?]
  (e/client
    (let [server-on? (e/server (settings/get-card-quiz-llm-grading user-id))
          !on? (atom server-on?)
          on? (e/watch !on?)]
      (dom/div
        (dom/props {:class "field"})
        (dom/label
          (dom/props {:style {:display "flex" :align-items "center" :gap "10px"
                              :cursor "pointer"}})
          (e/for [[t {:keys [card-quiz-llm]}] (forms/Checkbox! :card-quiz-llm on?
                                                :style {:width "18px" :height "18px"
                                                        :accent-color "var(--color-primary)"})]
            (reset! !on? card-quiz-llm)
            (let [r (e/server (e/Offload #(settings/save-card-quiz-llm-grading
                                            user-id card-quiz-llm)))]
              (case r
                (if (:success r)
                  (case (e/server (commands/bump! user-id :set-setting))
                    (t))
                  (t (:error r))))))
          (dom/div
            (dom/span
              (dom/props {:style {:font-size "14px" :font-weight "500"
                                  :color "var(--color-text-primary)"}})
              (dom/text "Use LLM for Generated Cards in Quiz"))
            (dom/div
              (dom/props {:class "hint"})
              (dom/text (if credits-enabled?
                          "Cards are graded against their own answer, which spends credits per review. Off: reveal the answer and rate it yourself, Again to Easy."
                          "Cards are graded against their own answer, one model call per review. Off: reveal the answer and rate it yourself, Again to Easy.")))))))))

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
  "Scan-quality (DPI) used for Scan Page OCR — preset select (Low/Standard/High);
   settings/save-scan-dpi clamps server-side regardless of the client value."
  [user-id]
  (e/client
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
          ;; A1-fallback: Forms5 has no tracked select
          (let [change-event (dom/On "change" #(-> % .-target .-value) nil)
                [t ?error] (e/Token change-event)]
            (dom/props {:disabled (some? t) :aria-busy (some? t)})
            (when (some? change-event)
              (reset! !dpi change-event))
            (when t
              (let [r (e/server (e/Offload #(settings/save-scan-dpi user-id change-event)))]
                (case r
                  (if (:success r) (t) (t (:error r))))))))
        (dom/div (dom/props {:class "hint"})
          (dom/text "Higher quality improves text recognition but increases processing time and API cost"))))))

(e/defn TranscribeLanguageField
  "Spoken language forced on audio and video transcription, or auto-detect.

   Auto-detect is Whisper's default and is right on most material, so it stays
   the default here. It is offered as a setting because when detection is wrong
   it does not degrade gracefully: the model commits to the language it guessed
   and writes fluent, plausible text in it. Measured on a 10 s English screen
   recording — three Arabic segments, two of them identical. The same bytes with
   English selected transcribed correctly."
  [user-id]
  (e/client
    (let [server-lang (e/server (or (settings/get-transcribe-language user-id) ""))
          !lang (atom server-lang)
          lang (e/watch !lang)]
      (dom/div
        (dom/props {:class "field"})
        (dom/label (dom/props {:class "label"}) (dom/text "Transcription Language"))
        (dom/select
          (dom/props {:value lang :class "select"})
          ;; tlang/options is .cljc, so these are CLIENT literals — the same
          ;; shape as ScanDpiField above. Reading the list from the CLJ-only
          ;; `settings` ns instead crashed the page: the peers emitted different
          ;; frame shapes and the mismatch surfaced in runtime3/socket-transfer.
          (e/for [[code label] (e/diff-by second tlang/options)]
            (dom/option (dom/props {:value (or code "")}) (dom/text label)))
          ;; A1-fallback: Forms5 has no tracked select
          (let [change-event (dom/On "change" #(-> % .-target .-value) nil)
                [t ?error] (e/Token change-event)]
            (dom/props {:disabled (some? t) :aria-busy (some? t)})
            (when (some? change-event)
              (reset! !lang change-event))
            (when t
              (let [r (e/server (e/Offload #(settings/save-transcribe-language user-id change-event)))]
                (case r
                  (if (:success r) (t) (t (:error r))))))))
        (dom/div (dom/props {:class "hint"})
          (dom/text (str "Auto-detect can pick the wrong language on short or quiet audio "
                      "and then transcribe in it. Set the language explicitly if that happens.")))))))

(e/defn AssistantPdfContextField
  "Pages before AND after the current page the Socratic assistant reads
   (0 = current page only). Autosave-on-blur (A1-fallback — Forms5 has no
   commit-on-blur text primitive)."
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
          ;; A1-fallback: Forms5 has no commit-on-blur text primitive (Input!/Input
          ;; fire on "input" = per-keystroke). Managed via a Focused? guard (A5 —
          ;; don't clobber the user's draft mid-edit), committed once on "change"
          ;; (blur); clamped to [0,50] client-side before saving so the field
          ;; snaps to the clamped value immediately, not just after a refresh.
          (dom/input
            (dom/props {:type "number" :min "0" :max "50"
                        :style {:width "56px" :font-size "13px" :padding "4px 6px"
                                :border "1px solid var(--color-border)" :border-radius "var(--radius-sm)"}})
            (when-not (dom/Focused?) (set! (.-value dom/node) (str window)))
            (let [committed (dom/On "change" #(-> % .-target .-value) nil)
                  [t _] (e/Token committed)]
              (dom/props {:disabled (some? t) :aria-busy (some? t)})
              (when t
                (let [parsed (js/parseInt committed)]
                  (if (js/isNaN parsed)
                    (t) ; blank/unparseable on blur — release the token, nothing to save
                    (let [clamped (max 0 (min 50 parsed))
                          r (e/server (e/Offload #(settings/save-assistant-pdf-window user-id clamped)))]
                      (case r
                        (if (:success r) (do (reset! !window clamped) (t)) (t (:error r))))))))))
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
  "Card generation retries — all attempts are billed (§5.4.5). Preset select
   (1-3); settings/save-card-gen-max-retries clamps server-side."
  [user-id]
  (e/client
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
          ;; A1-fallback: Forms5 has no tracked select
          (let [change-event (dom/On "change" #(-> % .-target .-value) nil)
                [t ?error] (e/Token change-event)]
            (dom/props {:disabled (some? t) :aria-busy (some? t)})
            (when (some? change-event)
              (reset! !retries change-event))
            (when t
              (let [r (e/server (e/Offload #(settings/save-card-gen-max-retries user-id change-event)))]
                (case r
                  (if (:success r) (t) (t (:error r))))))))
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
          ;; A1-fallback: Forms5 has no commit-on-blur text primitive (Input!/Input
          ;; fire on "input" = per-keystroke). Managed via a Focused? guard (A5 —
          ;; don't clobber the user's draft mid-edit), committed once on "change"
          ;; (blur).
          (dom/textarea
            (dom/props {:rows "20"
                        :style {:width "100%" :font-family "monospace" :font-size "12px" :line-height "1.5"
                                :padding "10px" :border "1px solid var(--color-border)" :border-radius "var(--radius-sm)"
                                :background "var(--color-bg-subtle)" :resize "vertical"
                                :color "var(--color-text-primary)"}})
            (when-not (dom/Focused?) (set! (.-value dom/node) (str sys-prompt)))
            (let [committed (dom/On "change" #(-> % .-target .-value) nil)
                  [t _] (e/Token committed)]
              (dom/props {:disabled (some? t) :aria-busy (some? t)})
              (when t
                (let [r (e/server (e/Offload #(settings/save-system-prompt user-id committed)))]
                  (case r
                    (if (:success r) (do (reset! !sys-prompt committed) (t)) (t (:error r))))))))
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
          ;; A1-fallback: Forms5 has no commit-on-blur text primitive (Input!/Input
          ;; fire on "input" = per-keystroke). Managed via a Focused? guard (A5 —
          ;; don't clobber the user's draft mid-edit), committed once on "change"
          ;; (blur).
          (dom/textarea
            (dom/props {:rows "10"
                        :style {:width "100%" :font-family "monospace" :font-size "12px" :line-height "1.5"
                                :padding "10px" :border "1px solid var(--color-border)" :border-radius "var(--radius-sm)"
                                :background "var(--color-bg-subtle)" :resize "vertical"
                                :color "var(--color-text-primary)"}})
            (when-not (dom/Focused?) (set! (.-value dom/node) (str ocr-prompt)))
            (let [committed (dom/On "change" #(-> % .-target .-value) nil)
                  [t _] (e/Token committed)]
              (dom/props {:disabled (some? t) :aria-busy (some? t)})
              (when t
                (let [r (e/server (e/Offload #(settings/save-ocr-prompt user-id committed)))]
                  (case r
                    (if (:success r) (do (reset! !ocr-prompt committed) (t)) (t (:error r)))))))))
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

(e/defn AIFeaturesSection [user-id enc-key]
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
                      "Incremental reading and Anki sync are always free. OCR and flashcard generation spend credits — your balance and top-up are in the Account tab."
                      "Incremental reading and Anki sync are always free. AI features (OCR, flashcard generation, transcription) use OpenRouter and require an OpenRouter API key set by the operator.")))

        (LlmToggleField user-id credits-enabled? !llm-enabled llm-enabled)

        (when llm-enabled
          ;; Self-host only. The credits panel moved to the Account tab: storage
          ;; rent debits credits with LLM features off, so gating the balance on
          ;; this toggle would hide it from a user who is still being charged.
          (when-not credits-enabled?
            (ProviderKeyStatusField api-key-configured?))

          (CardModelField user-id card-model-ids card-label-of)

          (CardQuizGradingField user-id credits-enabled?)

          (AssistantModelField user-id card-model-ids card-label-of)

          (KgModelsField user-id card-model-ids card-label-of)

          (ReasoningField user-id)

          (VerbosityField user-id)

          (ScanDpiField user-id)

          (TranscribeLanguageField user-id)

          (AssistantPdfContextField user-id)

          (DefaultOcrModelField user-id)

          (CardGenRetriesField user-id)

          ;; ── Prompts (inside when llm-enabled) ──
          (SystemPromptField user-id)
          (OcrPromptField user-id))))))
