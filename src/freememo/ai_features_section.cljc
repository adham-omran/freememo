(ns freememo.ai-features-section
  "AI Features tab on the Settings page: LLM toggle, provider-key status, card
   model, reasoning, verbosity, scan DPI, transcription language, system + OCR
   prompts. Extracted from settings_page so each e/defn stays under the JVM 64KB
   bytecode limit.

   The credit balance and top-up presets are NOT here — they moved to
   freememo.credits-section, rendered from the Usage tab, because storage rent
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
   [freememo.ai-tier-views :as views]
   [freememo.dedup-section :refer [DedupSection]]
   [freememo.assistant-panel-setting :refer [AssistantPanelLayoutField]]
   [freememo.segmented :as seg]
   [freememo.pre-prompt-mode :as ppm]
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
        (dom/props {:class "field setting-row"})
        (dom/div
          (dom/props {:class "setting-row__text"})
          (dom/label (dom/props {:class "label" :for (str "ai-kg-model-" (name step))})
            (dom/text label)))
        (dom/div
          (dom/props {:class "setting-row__control"})
          (dom/select
            (dom/props {:value val :class "select" :id (str "ai-kg-model-" (name step))})
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
                    (if (:success r)
                      ;; :settings-refresh re-reads the levels, so the group's
                      ;; label turns Custom.
                      (case (e/server (commands/bump! user-id :set-setting))
                        (t))
                      (t (:error r)))))))))))))

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
   DocumentToolbars passes `!show-bib` into DocumentOptionsModal)."
  [user-id credits-enabled? !llm-enabled llm-enabled]
  (e/client
    (dom/div
      (dom/props {:class "field setting-row setting-row--switch"})
      (dom/props {:data-tour "settings-ai"})
      (dom/div
        (dom/props {:class "setting-row__text"})
        (dom/label (dom/props {:class "label" :for "ai-enabled"}) (dom/text "Use AI features"))
        (dom/div
          (dom/props {:class "hint"})
          (dom/text (if credits-enabled?
                      "Scanning, card generation, the assistant, quiz grading and the knowledge graph spend credits. Reading and Anki sync are always free. Your balance is in the Usage tab."
                      "Scanning, card generation, the assistant, quiz grading and the knowledge graph call OpenRouter, which needs an API key. Reading and Anki sync work without one."))))
      (dom/div
        (dom/props {:class "setting-row__control"})
        (e/for [[t {:keys [llm-enabled]}] (forms/Checkbox! :llm-enabled llm-enabled
                  :id "ai-enabled" :class "switch" :role "switch")]
          (reset! !llm-enabled llm-enabled)
          (let [r (e/server (e/Offload #(settings/save-llm-enabled user-id llm-enabled)))]
            (case r
              (if (:success r)
                (case (e/server (commands/bump! user-id :set-setting))
                  (t))
                (t (:error r))))))))))

(e/defn ProviderKeyStatusField
  "Self-host mode's OpenRouter API-key status card. Credits mode has no
   provider key of its own — the platform holds it — and shows the balance in
   the Usage tab instead (freememo.credits-section)."
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
        (dom/props {:class "field setting-row"})
        (dom/div
          (dom/props {:class "setting-row__text"})
          (dom/label (dom/props {:class "label" :for "ai-card-model"}) (dom/text "Card model"))
          (dom/div (dom/props {:class "hint"})
            (dom/text "The model that writes flashcards.")))
        (dom/div
          (dom/props {:class "setting-row__control"})
          (dom/select
            (dom/props {:value model :class "select" :id "ai-card-model"})
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
                    (if (:success r)
                      ;; :settings-refresh re-reads the levels, so the group's
                      ;; label turns Custom.
                      (case (e/server (commands/bump! user-id :set-setting))
                        (t))
                      (t (:error r)))))))))))))

(e/defn UseFactsForCardsField
  "Account default for using approved document facts during card generation."
  [user-id]
  (e/client
    (let [server-on? (e/server (settings/get-use-facts-for-card-generation user-id))
          !on? (atom server-on?)
          on? (e/watch !on?)]
      (dom/div
        (dom/props {:class "field setting-row setting-row--switch"})
        (dom/div
          (dom/props {:class "setting-row__text"})
          (dom/label (dom/props {:class "label" :for "ai-use-facts"}) (dom/text "Use approved facts"))
          (dom/div
            (dom/props {:class "hint"})
            (dom/text "When a document has approved facts, write cards from them. A document can override this.")))
        (dom/div
          (dom/props {:class "setting-row__control"})
          (e/for [[t {:keys [use-facts]}]
                  (forms/Checkbox! :use-facts on?
                    :id "ai-use-facts" :class "switch" :role "switch")]
            (reset! !on? use-facts)
            (let [r (e/server (e/Offload
                                #(settings/save-use-facts-for-card-generation user-id use-facts)))]
              (case r
                (if (:success r)
                  (case (e/server (commands/bump! user-id :set-setting))
                    (t))
                  (t (:error r)))))))))))

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
        (dom/props {:class "field setting-row setting-row--switch"})
        (dom/div
          (dom/props {:class "setting-row__text"})
          (dom/label (dom/props {:class "label" :for "ai-quiz-grading"}) (dom/text "Grade generated cards with AI"))
          (dom/div
            (dom/props {:class "hint"})
            (dom/text (if credits-enabled?
                          "Grades your answer against the card, which spends credits on each review. When off, you see the answer and rate it yourself."
                          "Grades your answer against the card, one model call per review. When off, you see the answer and rate it yourself."))))
        (dom/div
          (dom/props {:class "setting-row__control"})
          (e/for [[t {:keys [card-quiz-llm]}] (forms/Checkbox! :card-quiz-llm on?
                    :id "ai-quiz-grading" :class "switch" :role "switch")]
            (reset! !on? card-quiz-llm)
            (let [r (e/server (e/Offload #(settings/save-card-quiz-llm-grading
                                            user-id card-quiz-llm)))]
              (case r
                (if (:success r)
                  (case (e/server (commands/bump! user-id :set-setting))
                    (t))
                  (t (:error r)))))))))))

(e/defn AssistantModelField
  "Assistant model — Socratic reading-view chatbot. Same registry as card
   generation; defaults to Gemini 3 Flash."
  [user-id card-model-ids card-label-of]
  (e/client
    (let [server-assistant-model (e/server (settings/get-assistant-model user-id))
          !assistant-model (atom server-assistant-model)
          assistant-model (e/watch !assistant-model)]
      (dom/div
        (dom/props {:class "field setting-row"})
        (dom/div
          (dom/props {:class "setting-row__text"})
          (dom/label (dom/props {:class "label" :for "ai-assistant-model"}) (dom/text "Assistant model"))
          (dom/div (dom/props {:class "hint"})
            (dom/text "The model that answers in the reading assistant.")))
        (dom/div
          (dom/props {:class "setting-row__control"})
          (dom/select
            (dom/props {:value assistant-model :class "select" :id "ai-assistant-model"})
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
                    (if (:success r)
                      ;; :settings-refresh re-reads the levels, so the group's
                      ;; label turns Custom.
                      (case (e/server (commands/bump! user-id :set-setting))
                        (t))
                      (t (:error r)))))))))))))

(e/defn KgModelsField
  "Knowledge-graph models — one KgModelField per KG pipeline step, rendered
   from the step registry (settings/kg-model-step-choices), except :grade,
   which Quiz grading owns. Same registry as card generation."
  [user-id card-model-ids card-label-of]
  (e/client
    (let [kg-steps (e/server (filterv (fn [[step _]] (not= step :grade))
                               (settings/kg-model-step-choices)))]
      (dom/div
        (dom/props {:class "setting-rows"})
        (e/for [pair (e/diff-by first kg-steps)]
          (let [[step label] pair]
            (KgModelField user-id step label card-model-ids card-label-of)))))))

(e/defn ReasoningField
  "Reasoning-effort select for one lane: `lane` is :cards or :assistant. Each
   lane has its own setting, because each task has its own level."
  [user-id lane]
  (e/client
    (let [server-reasoning (e/server (if (= lane :assistant)
                                       (settings/get-assistant-reasoning user-id)
                                       (settings/get-reasoning user-id)))
          !reasoning (atom server-reasoning)
          reasoning (e/watch !reasoning)]
      (dom/div
        (dom/props {:class "field setting-row"})
        (dom/div
          (dom/props {:class "setting-row__text"})
          (dom/label (dom/props {:class "label" :for (str "ai-reasoning-" (name lane))}) (dom/text "Reasoning effort"))
          (dom/div (dom/props {:class "hint"})
            (dom/text "More effort costs more and takes longer. GLM 5.3 Flash gives the same result for Low and High.")))
        (dom/div
          (dom/props {:class "setting-row__control"})
          (dom/select
            (dom/props {:value reasoning :class "select" :id (str "ai-reasoning-" (name lane))})
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
                (let [r (e/server (e/Offload #(if (= lane :assistant)
                                                (settings/save-assistant-reasoning user-id change-event)
                                                (settings/save-reasoning user-id change-event))))]
                  (case r
                    (if (:success r)
                      (case (e/server (commands/bump! user-id :set-setting))
                        (t))
                      (t (:error r)))))))))))))

(e/defn VerbosityField
  "Verbosity select for flashcard generation."
  [user-id]
  (e/client
    (let [server-verbosity (e/server (settings/get-verbosity user-id))
          !verbosity (atom server-verbosity)
          verbosity (e/watch !verbosity)]
      (dom/div
        (dom/props {:class "field setting-row"})
        (dom/div
          (dom/props {:class "setting-row__text"})
          (dom/label (dom/props {:class "label" :for "ai-verbosity"}) (dom/text "Detail level"))
          (dom/div (dom/props {:class "hint"})
            (dom/text "How much detail generated cards carry. Only Claude Sonnet 5 uses this setting.")))
        (dom/div
          (dom/props {:class "setting-row__control"})
          (dom/select
            (dom/props {:value verbosity :class "select" :id "ai-verbosity"})
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
                    (if (:success r) (t) (t (:error r)))))))))))))

(e/defn ScanDpiField
  "Scan-quality (DPI) used for Scan Page OCR — preset select (Low/Standard/High);
   settings/save-scan-dpi clamps server-side regardless of the client value."
  [user-id]
  (e/client
    (let [server-dpi (e/server (settings/get-scan-dpi user-id))
          !dpi (atom (str server-dpi))
          dpi (e/watch !dpi)]
      (dom/div
        (dom/props {:class "field setting-row"})
        (dom/div
          (dom/props {:class "setting-row__text"})
          (dom/label (dom/props {:class "label" :for "ai-scan-dpi"}) (dom/text "Scan quality"))
          (dom/div (dom/props {:class "hint"})
            (dom/text "Higher quality reads small print better. It takes longer and costs more.")))
        (dom/div
          (dom/props {:class "setting-row__control"})
          (dom/select
            (dom/props {:value dpi :class "select" :id "ai-scan-dpi"})
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
                    (if (:success r)
                      ;; :settings-refresh re-reads the levels, so the group's
                      ;; label turns Custom.
                      (case (e/server (commands/bump! user-id :set-setting))
                        (t))
                      (t (:error r)))))))))))))

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
        (dom/props {:class "field setting-row"})
        (dom/div
          (dom/props {:class "setting-row__text"})
          (dom/label (dom/props {:class "label" :for "ai-transcribe-language"}) (dom/text "Transcription language"))
          (dom/div (dom/props {:class "hint"})
            (dom/text (str "Auto-detect can pick the wrong language on short or quiet audio "
                        "and then transcribe in it. Set the language explicitly if that happens."))))
        (dom/div
          (dom/props {:class "setting-row__control"})
          (dom/select
            (dom/props {:value lang :class "select" :id "ai-transcribe-language"})
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
                    (if (:success r) (t) (t (:error r)))))))))))))

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
        (dom/props {:class "field setting-row"})
        (dom/div
          (dom/props {:class "setting-row__text"})
          (dom/label (dom/props {:class "label" :for "ai-pdf-context"}) (dom/text "PDF context"))
          (dom/div (dom/props {:class "hint"})
            (dom/text "Pages before and after the current page that the assistant reads, from 0 to 50.")))
        (dom/div
          (dom/props {:class "setting-row__control"})
          ;; A1-fallback: Forms5 has no commit-on-blur text primitive (Input!/Input
          ;; fire on "input" = per-keystroke). Managed via a Focused? guard (A5 —
          ;; don't clobber the user's draft mid-edit), committed once on "change"
          ;; (blur); clamped to [0,50] client-side before saving so the field
          ;; snaps to the clamped value immediately, not just after a refresh.
          (dom/input
            (dom/props {:type "number" :min "0" :max "50" :id "ai-pdf-context"
                        :class "setting-row__number"})
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
          (dom/span (dom/props {:class "setting-row__unit"})
            (dom/text "pages each side")))))))

(e/defn DefaultOcrModelField
  "Default OCR model (Scan Page). Every document uses it; per-document overrides
   were removed on 2026-09-23."
  [user-id]
  (e/client
    (let [server-ocr-model (e/server (settings/get-ocr-model-default user-id))
          allowed-ids (e/server (ocr/allowed-ocr-model-ids))
          label-of (into {} (map (juxt :id :label)) ocr-models/registry)
          !ocr-model (atom (or server-ocr-model ocr-models/default-id))
          ocr-model (e/watch !ocr-model)]
      (dom/div
        (dom/props {:class "field setting-row"})
        (dom/div
          (dom/props {:class "setting-row__text"})
          (dom/label (dom/props {:class "label" :for "ai-ocr-model"}) (dom/text "Scanning model"))
          (dom/div (dom/props {:class "hint"})
            (dom/text "Reads scanned pages, in every document.")))
        (dom/div
          (dom/props {:class "setting-row__control"})
          (dom/select
            (dom/props {:value ocr-model :class "select" :id "ai-ocr-model"})
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
                    (if (:success r)
                      ;; :settings-refresh re-reads the levels, so the group's
                      ;; label turns Custom.
                      (case (e/server (commands/bump! user-id :set-setting))
                        (t))
                      (t (:error r)))))))))))))

(e/defn CardGenRetriesField
  "Card generation retries — all attempts are billed (§5.4.5). Preset select
   (1-3); settings/save-card-gen-max-retries clamps server-side."
  [user-id]
  (e/client
    (let [server-retries (e/server (settings/get-card-gen-max-retries user-id))
          !retries (atom (str server-retries))
          retries (e/watch !retries)]
      (dom/div
        (dom/props {:class "field setting-row"})
        (dom/div
          (dom/props {:class "setting-row__text"})
          (dom/label (dom/props {:class "label" :for "ai-card-retries"}) (dom/text "Retries"))
          (dom/div (dom/props {:class "hint"})
            (dom/text "Try again when the model returns the wrong number of cards. Each attempt is billed.")))
        (dom/div
          (dom/props {:class "setting-row__control"})
          (dom/select
            (dom/props {:value retries :class "select" :id "ai-card-retries"})
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
                    (if (:success r) (t) (t (:error r)))))))))))))

(e/defn PrePromptModeField
  "Pre-prompt strictness — how literally card generation reads the text typed in
   Generate with Prompt. This is the default for every generation; the dialog
   itself can override it for one run without changing this value.

   A segmented control rather than a select (the idiom of the fields above)
   because all three positions must be readable at once: the choice is a
   guarantee about the output, not a preference among interchangeable presets."
  [user-id]
  (e/client
    (let [server-mode (e/server (settings/get-pre-prompt-mode user-id))
          !mode (atom server-mode)
          mode (e/watch !mode)
          SaveMode (e/fn [t value]
                     (e/client
                       (reset! !mode value)
                       (let [r (e/server (e/Offload #(settings/save-pre-prompt-mode user-id value)))]
                         (when (some? r)
                           (if (:success r) (t) (t (:error r)))))))]
      (dom/div
        (dom/props {:class "field setting-row"})
        (dom/div
          (dom/props {:class "setting-row__text"})
          ;; A div, not a label: a radiogroup is not a labelable element. The
          ;; group carries its own aria-label.
          (dom/div (dom/props {:class "label"}) (dom/text "Pre-prompt strictness"))
          (dom/div (dom/props {:class "hint"})
            (dom/text (ppm/description mode)))
          (dom/div (dom/props {:class "hint"})
            (dom/text "Clarify and Verbatim generate a single card. Cloze has no question field, so it is bounded to the prompt's topic instead.")))
        (dom/div
          (dom/props {:class "setting-row__control"})
          (dom/div
            (dom/props {:class "segmented" :role "radiogroup" :aria-label "Pre-prompt strictness"})
            (seg/Segment mode ppm/guide (ppm/label ppm/guide) SaveMode)
            (seg/Segment mode ppm/clarify (ppm/label ppm/clarify) SaveMode)
            (seg/Segment mode ppm/verbatim (ppm/label ppm/verbatim) SaveMode)))))))

(def ^:private prompt-chip-text
  "The Advanced view's mark for a saved prompt, by
   `settings/prompt-override-state`. :default has none. Never \"Custom\": the
   task level chip uses that word, and a saved prompt does not change the level."
  {:edited "Edited"
   :stale "Edited, default changed"
   :unknown-base "Edited, default may have changed"})

(def ^:private stale-notice-text
  {:stale "The default prompt changed after you saved yours. Your version is still in use."
   :unknown-base "The default prompt may have changed after you saved yours. Your version is still in use."})

(defn- state-after-save
  "The saved prompt's state once `text` is saved. The server deletes a save equal
   to the default and stamps any other with the current default."
  [text default]
  (if (= text default) :default :edited))

(e/defn PromptChip [state]
  (e/client
    (when-some [label (prompt-chip-text state)]
      (dom/span
        (dom/props {:class (str "prompt-chip" (when (stale-notice-text state) " prompt-chip--stale"))})
        (dom/text label)))))

(e/defn PromptStaleNotice
  "Says a saved prompt is not following the default, with the default readable."
  [state default-text]
  (e/client
    (when-some [message (stale-notice-text state)]
      (let [!show? (atom false)
            show? (e/watch !show?)]
        (dom/div
          (dom/props {:class "prompt-stale-notice" :role "status"})
          (dom/p (dom/text message))
          (dom/button
            (dom/props {:type "button" :class "btn btn-secondary" :aria-expanded (str show?)})
            (dom/text (if show? "Hide default" "View default"))
            (dom/On "click" (fn [_] (swap! !show? not)) nil))
          (when show?
            (dom/pre (dom/props {:class "prompt-default-text"}) (dom/text default-text))))))))

(e/defn SystemPromptField
  "Card Generation System Prompt — collapsed accordion. Controls the persona,
   rules, and style for flashcard generation; format-specific instructions
   (basic/cloze/context) are appended automatically."
  [user-id]
  (e/client
    (let [default-sys (e/server (settings/get-default-system-prompt))
          server-sys (e/server (settings/get-system-prompt user-id))
          !sys-prompt (atom server-sys)
          sys-prompt (e/watch !sys-prompt)
          !state (atom (e/server (settings/prompt-override-state user-id :system)))
          state (e/watch !state)]
      (dom/details
        (dom/props {:class "settings-accordion"})
        (dom/summary
          (dom/props {:class "settings-accordion__summary"})
          (dom/text "Card Generation System Prompt")
          (PromptChip state))
        (dom/div
          (dom/props {:class "settings-accordion__body"})
          (dom/div (dom/props {:class "hint" :style {:margin-bottom "8px"}})
            (dom/text "Controls the persona, rules, and style for flashcard generation. Format-specific instructions (basic/cloze/context) are appended automatically."))
          (PromptStaleNotice state default-sys)
          ;; A1-fallback: Forms5 has no commit-on-blur text primitive (Input!/Input
          ;; fire on "input" = per-keystroke). Managed via a Focused? guard (A5 —
          ;; don't clobber the user's draft mid-edit), committed once on "change"
          ;; (blur).
          (dom/textarea
            (dom/props {:rows "20" :class "input"
                        :style {:width "100%" :font-family "monospace" :font-size "12px" :line-height "1.5"
                                :padding "10px" :border-radius "var(--radius-sm)"
                                :background "var(--color-bg-subtle)" :resize "vertical"}})
            (when-not (dom/Focused?) (set! (.-value dom/node) (str sys-prompt)))
            (let [committed (dom/On "change" #(-> % .-target .-value) nil)
                  [t _] (e/Token committed)]
              (dom/props {:disabled (some? t) :aria-busy (some? t)})
              (when t
                (let [r (e/server (e/Offload #(settings/save-system-prompt user-id committed)))]
                  (case r
                    (if (:success r)
                      (do (reset! !sys-prompt committed)
                          (reset! !state (state-after-save committed default-sys))
                          (t))
                      (t (:error r))))))))
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
                      (do (reset! !sys-prompt default-sys) (reset! !state :default) (t))
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
          ocr-prompt (e/watch !ocr-prompt)
          !state (atom (e/server (settings/prompt-override-state user-id :ocr)))
          state (e/watch !state)]
      (dom/details
        (dom/props {:class "settings-accordion" :style {:margin-top "12px"}})
        (dom/summary
          (dom/props {:class "settings-accordion__summary"})
          (dom/text "OCR Extraction Prompt")
          (PromptChip state))
        (dom/div
          (dom/props {:class "settings-accordion__body"})
          (dom/div (dom/props {:class "hint" :style {:margin-bottom "8px"}})
            (dom/text "Instructions for extracting text from PDF page images. Controls how tables, headings, and structure are handled."))
          (PromptStaleNotice state default-ocr)
          ;; A1-fallback: Forms5 has no commit-on-blur text primitive (Input!/Input
          ;; fire on "input" = per-keystroke). Managed via a Focused? guard (A5 —
          ;; don't clobber the user's draft mid-edit), committed once on "change"
          ;; (blur).
          (dom/textarea
            (dom/props {:rows "10" :class "input"
                        :style {:width "100%" :font-family "monospace" :font-size "12px" :line-height "1.5"
                                :padding "10px" :border-radius "var(--radius-sm)"
                                :background "var(--color-bg-subtle)" :resize "vertical"}})
            (when-not (dom/Focused?) (set! (.-value dom/node) (str ocr-prompt)))
            (let [committed (dom/On "change" #(-> % .-target .-value) nil)
                  [t _] (e/Token committed)]
              (dom/props {:disabled (some? t) :aria-busy (some? t)})
              (when t
                (let [r (e/server (e/Offload #(settings/save-ocr-prompt user-id committed)))]
                  (case r
                    (if (:success r)
                      (do (reset! !ocr-prompt committed)
                          (reset! !state (state-after-save committed default-ocr))
                          (t))
                      (t (:error r)))))))))
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
                    (do (reset! !ocr-prompt default-ocr) (reset! !state :default) (t))
                    (t (:error r))))))))))))

(e/defn AdvancedCards [user-id level card-model-ids card-label-of]
  (e/client
    (dom/section
      (dom/props {:class "settings-group"})
      (views/GroupHeading "Flashcards" level)
      (CardModelField user-id card-model-ids card-label-of)
      (ReasoningField user-id :cards)
      (VerbosityField user-id)
      (UseFactsForCardsField user-id)
      (PrePromptModeField user-id)
      (CardGenRetriesField user-id)
      (SystemPromptField user-id))))

(e/defn AdvancedScan [user-id level]
  (e/client
    (dom/section
      (dom/props {:class "settings-group"})
      (views/GroupHeading "Page scanning" level)
      (DefaultOcrModelField user-id)
      (ScanDpiField user-id)
      (OcrPromptField user-id))))

(e/defn AdvancedAssistant [user-id level card-model-ids card-label-of]
  (e/client
    (dom/section
      (dom/props {:class "settings-group"})
      (views/GroupHeading "Reading assistant" level)
      (AssistantModelField user-id card-model-ids card-label-of)
      (ReasoningField user-id :assistant)
      (AssistantPdfContextField user-id)
      (AssistantPanelLayoutField user-id))))

(e/defn AdvancedQuiz [user-id level credits-enabled? card-model-ids card-label-of]
  (e/client
    (dom/section
      (dom/props {:class "settings-group"})
      (views/GroupHeading "Quiz grading" level)
      (KgModelField user-id :grade "Answer grading model" card-model-ids card-label-of)
      (CardQuizGradingField user-id credits-enabled?))))

(e/defn AdvancedKg [user-id level card-model-ids card-label-of]
  (e/client
    (dom/section
      (dom/props {:class "settings-group"})
      (views/GroupHeading "Knowledge graph" level)
      (KgModelsField user-id card-model-ids card-label-of)
      (DedupSection user-id))))

(e/defn AdvancedView
  "Every setting, grouped by task. A group's label shows its task's level; an
   edit here sets that task to Custom (settings/write-setting!)."
  [user-id tiers credits-enabled? card-model-ids card-label-of]
  (e/client
    (dom/div
      (dom/props {:class "ai-advanced"})
      (dom/p (dom/props {:class "ai-advanced__note"})
        (dom/text "Each task starts on the level you chose in Simple or By task. Changing a setting here sets that task to Custom."))
      (AdvancedCards user-id (get tiers :cards) card-model-ids card-label-of)
      (AdvancedScan user-id (get tiers :scan))
      (AdvancedAssistant user-id (get tiers :assistant) card-model-ids card-label-of)
      (AdvancedQuiz user-id (get tiers :quiz) credits-enabled? card-model-ids card-label-of)
      (AdvancedKg user-id (get tiers :kg) card-model-ids card-label-of)
      (dom/section
        (dom/props {:class "settings-group"})
        (views/GroupHeading "Transcription" nil)
        (TranscribeLanguageField user-id)))))

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
          card-label-of (into {} (map (juxt :id :label)) card-models/registry)
          tiers (e/server (views/get-ai-tiers* settings-refresh user-id))
          !view (atom (views/load-view))
          view (e/watch !view)]

      (dom/div
        (dom/props {:class "ai-features-section"})
        (LlmToggleField user-id credits-enabled? !llm-enabled llm-enabled)

        (when llm-enabled
          ;; Self-host only. The credits panel moved to the Usage tab: storage
          ;; rent debits credits with LLM features off, so gating the balance on
          ;; this toggle would hide it from a user who is still being charged.
          (when-not credits-enabled?
            (ProviderKeyStatusField api-key-configured?))

          (views/ViewPicker !view view)

          ;; The case gates each view's DOM: switching views is meant to
          ;; remount it.
          (case view
            "simple" (views/SimpleView user-id tiers !view)
            "by-task" (views/ByTaskView user-id tiers)
            (AdvancedView user-id tiers credits-enabled? card-model-ids card-label-of)))))))
