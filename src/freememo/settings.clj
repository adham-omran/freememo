(ns freememo.settings
  "Business logic for application settings."
  (:require
   [freememo.db :as db]
   [freememo.card-models :as card-models]
   [freememo.config :as config]
   [freememo.fsrs :as fsrs]
   [freememo.input-check :as input]
   [freememo.toasts :as toasts]
   [taoensso.telemere :as tel]
   [clojure.java.io :as io]
   [clojure.string :as str]))

(def app-base-url
  "Public base URL for self-references (e.g. Anki card source links).
   Override with APP_BASE_URL env."
  (or (System/getenv "APP_BASE_URL") "https://freememo.net"))

;; Setting keys (constants)
(def MODEL "model")
(def CARD_COUNT "card_count")
(def REASONING "reasoning")
(def VERBOSITY "verbosity")
(def CONTEXT_ENABLED "context_enabled")
(def CONTEXT_PAGES "context_pages")
(def ASSISTANT_PDF_WINDOW "assistant_pdf_window")
(def CARD_TYPE "card_type")
(def ACTIVE_TAB "active_tab")
(def ANKI_SCOPE "anki_scope")
(def ANKI_DECK "anki_deck")
(def ANKI_ALLOW_DUPES "anki_allow_dupes")
(def ANKI_USE_TAGS "anki_use_tags")
(def ANKI_TAGS "anki_tags")
(def ANKI_AUTO_LOAD_MODE "anki_auto_load_mode")
(def LLM_ENABLED "llm_enabled")
(def LAST_DOCUMENT "last_document")
(def PRE_PROMPT_HISTORY "pre_prompt_history")
(def CARD_FONT_SIZE "card_font_size")
(def SCAN_DPI "scan_dpi")
(def CARD_GEN_MAX_RETRIES "card_gen_max_retries")
(def PROMPT_SYSTEM "prompt_system")
(def PROMPT_OCR "prompt_ocr")
(def EMAIL_UPDATES "email_updates")
(def THEME "theme")
(def ZOTERO_ENABLED "zotero_enabled")
(def CARD_SPLIT "card_split")
(def ASSISTANT_MODEL "assistant_model")
(def FSRS_RETENTION "fsrs_retention")
(def FSRS_NEW_PER_DAY "fsrs_new_per_day")
(def FSRS_REVIEW_PER_DAY "fsrs_review_per_day")
(def FSRS_FUZZ "fsrs_fuzz")
; Per-document page keys are dynamic: (str "last_page_" doc-id)

(def kg-default-model-id
  "Default model for every knowledge-graph step when the user has no saved
   selection (product decision — a cheap, fast model for high-volume KG work)."
  "gemini-3-flash")

(def kg-model-steps
  "Per-step KG model knobs — the single source for both resolution
   (get/save-kg-model) and the Settings selectors; order = display order.
   :step is the id threaded through kg-llm/resolve-model+gate!; :setting is the
   db/get-setting key."
  [{:step :extract     :setting "kg_extract_model"            :label "Fact extraction (distill)"}
   {:step :link        :setting "kg_link_model"               :label "Entity linking"}
   {:step :atomic      :setting "kg_atomic_question_model"    :label "Atomic questions"}
   {:step :synthesis   :setting "kg_synthesis_question_model" :label "Synthesis questions"}
   {:step :grade       :setting "kg_grade_model"              :label "Answer grading"}
   {:step :fact-select :setting "kg_fact_select_model"        :label "Fact selection (cards)"}])

(def ^:private kg-step->setting
  (into {} (map (juxt :step :setting)) kg-model-steps))

(defn get-email-updates [user-id]
  (= "true" (db/get-setting user-id EMAIL_UPDATES)))

(defn save-email-updates [user-id value]
  (try
    (db/set-setting user-id EMAIL_UPDATES (str (boolean value)))
    {:success true}
    (catch Exception e
      (tel/error! {:id ::save-email-updates} e)
      {:success false :error "Failed to save email updates preference"})))

(defn get-openrouter-api-key
  "Resolve the OpenRouter key — the single provider key for OCR, card generation,
   and transcription (topology A1). The platform key serves both official and
   self-host; self-host operators set :secrets :platform-openrouter-api-key (or
   env PLATFORM_OPENROUTER_API_KEY). `user-id` is accepted for a future per-user
   BYOK path.
   Post: the key string, or nil when unconfigured (caller refuses the action)."
  [_user-id]
  (config/platform-openrouter-api-key))

(defn get-openrouter-key-status
  "Whether the OpenRouter key is configured, for the settings + home UI. There is
   no per-user BYOK — a single operator-set key backs both modes.
   Post: {:source :platform :configured? bool}."
  [user-id]
  {:source :platform
   :configured? (some? (get-openrouter-api-key user-id))})

(defn card-model-ids
  "Card-generation model :ids a user may pick (single source of truth for the
   resolver + picker). Credits-mode: config/card-model-allowlist when non-empty,
   else all registry ids (an unset allow-list means \"no restriction\"). Self-host:
   all registry ids.
   Post: a non-empty vector of registry ids (registry is never empty)."
  []
  (let [all (mapv :id card-models/registry)]
    (if (config/credits-enabled?)
      (let [allow (config/card-model-allowlist)]
        (if (seq allow) (vec allow) all))
      all)))

(defn get-model
  "Effective card-generation model :id for a user.
   Precedence among allowed ids: per-user saved id → prod default (!prod-model in
   credits mode) or registry default → first allowed. Mirrors
   `ocr/effective-ocr-model`; !prod-model is now the default, not a hard pin.
   Post: an :id present in (card-model-ids)."
  [user-id]
  (let [allowed (card-model-ids)
        saved (db/get-setting user-id MODEL)
        default (if (config/credits-enabled?)
                  (or @config/!prod-model card-models/default-id)
                  card-models/default-id)]
    (cond
      (some #{saved} allowed) saved
      (some #{default} allowed) default
      :else (first allowed))))

(def assistant-default-model-id
  "Default assistant model when the user has no saved selection. Distinct from
   card-models/default-id (the card-generation default) by product decision."
  "gemini-3-flash")

(defn get-assistant-model
  "Effective assistant-chat model :id for a user: saved id if allowed, else
   gemini-3-flash if allowed, else the first allowed id.
   Post: an :id present in (card-model-ids)."
  [user-id]
  (let [allowed (card-model-ids)
        saved (db/get-setting user-id ASSISTANT_MODEL)]
    (cond
      (some #{saved} allowed) saved
      (some #{assistant-default-model-id} allowed) assistant-default-model-id
      :else (first allowed))))

(defn card-model-choices
  "[[id label] …] for the allowed card-generation models, feeding the A1-select
   pickers (card-models is server-only, so options are built here and shipped as
   data). Post: non-empty vec; every id ∈ (card-model-ids)."
  []
  (let [labels (into {} (map (juxt :id :label)) card-models/registry)]
    (mapv (fn [id] [id (labels id id)]) (card-model-ids))))

(defn get-kg-model
  "Effective model :id for KG pipeline `step`. Saved id if ∈ (card-model-ids),
   else kg-default-model-id if allowed, else first allowed.
   Pre:  step ∈ (map :step kg-model-steps) — else caller bug.
   Post: an :id ∈ (card-model-ids); never nil."
  [user-id step]
  (let [setting (or (kg-step->setting step)
                    (throw (ex-info (str "Unknown KG model step: " step) {:step step})))
        allowed (card-model-ids)
        saved (db/get-setting user-id setting)]
    (cond
      (some #{saved} allowed) saved
      (some #{kg-default-model-id} allowed) kg-default-model-id
      :else (first allowed))))

(defn kg-model-step-choices
  "[[step-keyword label] …] in display order — ships the KG step list + labels
   to the Settings UI (kg-model-steps is server-only)."
  []
  (mapv (juxt :step :label) kg-model-steps))

(defn get-card-count [user-id]
  (try
    (Integer/parseInt (or (db/get-setting user-id CARD_COUNT) "2"))
    (catch Exception e
      (tel/error! {:id ::get-card-count} e)
      2)))

(defn get-reasoning [user-id]
  (or (db/get-setting user-id REASONING) "low"))

(defn get-verbosity [user-id]
  (or (db/get-setting user-id VERBOSITY) "low"))

(defn get-context-enabled [user-id]
  (try
    (= "true" (or (db/get-setting user-id CONTEXT_ENABLED) "true"))
    (catch Exception e
      (tel/error! {:id ::get-context-enabled} e)
      true)))

(defn get-context-pages [user-id]
  (try
    (Integer/parseInt (or (db/get-setting user-id CONTEXT_PAGES) "1"))
    (catch Exception e
      (tel/error! {:id ::get-context-pages} e)
      1)))

(defn get-assistant-pdf-window
  "Pages before AND after the current PDF page the assistant reads as context.
   Default 20. Post: integer in [0,50] (clamped defensively on read)."
  [user-id]
  (try
    (let [v (Integer/parseInt (or (db/get-setting user-id ASSISTANT_PDF_WINDOW) "20"))]
      (max 0 (min 50 v)))
    (catch Exception e
      (tel/error! {:id ::get-assistant-pdf-window} e)
      20)))

(defn get-active-tab [user-id]
  (try
    (let [raw (db/get-setting user-id ACTIVE_TAB)
          valid #{"home" "settings" "library" "import" "learn" "queue" "status" "help"}]
      (if (valid raw)
        (keyword raw)
        (case raw
          "pdf" :library
          "contents" :library
          "workspace" :learn
          :home)))
    (catch Exception e
      (tel/error! {:id ::get-active-tab} e)
      :home)))

(defn get-card-type [user-id]
  (try
    (or (db/get-setting user-id CARD_TYPE) "basic")
    (catch Exception e
      (tel/error! {:id ::get-card-type} e)
      "basic")))

(defn get-llm-enabled [user-id]
  (let [v (db/get-setting user-id LLM_ENABLED)]
    (or (nil? v) (= "true" v))))

(defn save-llm-enabled [user-id value]
  (try
    (db/set-setting user-id LLM_ENABLED (str (boolean value)))
    {:success true}
    (catch Exception e
      (tel/error! {:id ::save-llm-enabled} e)
      {:success false :error "Failed to save LLM enabled"})))

(defn normalize-scope
  "Map any stored/legacy Anki-sync scope value to a current key:
   'self' | 'subtree' | 'document'. Legacy 'Current Page' → 'self',
   'Entire Doc' → 'document'; nil/unknown → 'self' (narrowest default)."
  [v]
  (case v
    ("self" "subtree" "document") v
    "Current Page" "self"
    "Entire Doc"   "document"
    "self"))

(defn get-anki-scope [user-id]
  (normalize-scope (db/get-setting user-id ANKI_SCOPE)))

(defn get-anki-deck [user-id]
  (db/get-setting user-id ANKI_DECK))

(defn get-anki-allow-dupes [user-id]
  (= "true" (or (db/get-setting user-id ANKI_ALLOW_DUPES) "false")))

(defn get-anki-use-tags [user-id]
  (= "true" (or (db/get-setting user-id ANKI_USE_TAGS) "false")))

(defn get-anki-tags [user-id]
  (try
    (let [raw (db/get-setting user-id ANKI_TAGS)]
      (if (seq raw) (clojure.edn/read-string raw) []))
    (catch Exception _ [])))

(defn get-anki-auto-load-mode
  "Returns one of \"per-item\", \"global\", or \"none\". Defaults to \"per-item\"."
  [user-id]
  (let [v (db/get-setting user-id ANKI_AUTO_LOAD_MODE)]
    (if (#{"per-item" "global" "none"} v) v "per-item")))

(defn save-anki-auto-load-mode [user-id value]
  (try
    (when-not (#{"per-item" "global" "none"} value)
      (throw (Exception. "Invalid auto-load mode")))
    (db/set-setting user-id ANKI_AUTO_LOAD_MODE value)
    {:success true}
    (catch Exception e
      (tel/error! {:id ::save-anki-auto-load-mode} e)
      {:success false :error "Failed to save auto-load mode"})))

;; Save functions with validation
(defn save-model
  "Persist a card-generation model selection. Pre: `value` is a card-models :id
   (else rejected — caller bug). Post: MODEL setting stored, or {:success false}."
  [user-id value]
  (if-not (card-models/resolve-model value)
    {:success false :error (str "Unknown card model: " value)}
    (try
      (db/set-setting user-id MODEL value)
      {:success true}
      (catch Exception e
        (tel/error! {:id ::save-model} e)
        {:success false :error "Failed to save model"}))))

(defn save-assistant-model
  "Persist an assistant model selection. Pre: `value` is a card-models :id
   (else rejected — caller bug). Post: ASSISTANT_MODEL stored, or {:success false}."
  [user-id value]
  (if-not (card-models/resolve-model value)
    {:success false :error (str "Unknown assistant model: " value)}
    (try
      (db/set-setting user-id ASSISTANT_MODEL value)
      {:success true}
      (catch Exception e
        (tel/error! {:id ::save-assistant-model} e)
        {:success false :error "Failed to save assistant model"}))))

(defn save-kg-model
  "Persist a KG step model selection. Pre: `step` known and `value` a
   card-models :id (else rejected — caller bug). Post: the step's setting
   stored, or {:success false}."
  [user-id step value]
  (let [setting (kg-step->setting step)]
    (cond
      (nil? setting)
      {:success false :error (str "Unknown KG model step: " step)}
      (not (card-models/resolve-model value))
      {:success false :error (str "Unknown model: " value)}
      :else
      (try
        (db/set-setting user-id setting value)
        {:success true}
        (catch Exception e
          (tel/error! {:id ::save-kg-model} e)
          {:success false :error "Failed to save KG model"})))))

(defn save-reasoning [user-id value]
  (try
    (db/set-setting user-id REASONING value)
    {:success true}
    (catch Exception e
      (tel/error! {:id ::save-reasoning} e)
      {:success false :error "Failed to save reasoning"})))

(defn save-verbosity [user-id value]
  (try
    (db/set-setting user-id VERBOSITY value)
    {:success true}
    (catch Exception e
      (tel/error! {:id ::save-verbosity} e)
      {:success false :error "Failed to save verbosity"})))

(defn save-context-enabled [user-id value]
  (try
    (db/set-setting user-id CONTEXT_ENABLED (str value))
    {:success true}
    (catch Exception e
      (tel/error! {:id ::save-context-enabled} e)
      {:success false :error "Failed to save context enabled"})))

(defn save-context-pages [user-id value]
  (try
    (let [parsed (Integer/parseInt (str value))
          clamped (max 1 (min 10 parsed))] ; Enforce 1-10 range
      (db/set-setting user-id CONTEXT_PAGES (str clamped))
      {:success true})
    (catch Exception e
      (tel/error! {:id ::save-context-pages} e)
      {:success false :error "Failed to save context pages"})))

(defn save-assistant-pdf-window [user-id value]
  (try
    (let [parsed (Integer/parseInt (str value))
          clamped (max 0 (min 50 parsed))] ; Enforce 0-50 range
      (db/set-setting user-id ASSISTANT_PDF_WINDOW (str clamped))
      {:success true})
    (catch Exception e
      (tel/error! {:id ::save-assistant-pdf-window} e)
      {:success false :error "Failed to save assistant PDF context"})))

(defn save-card-type [user-id value]
  (try
    (when-not (#{"basic" "cloze" "overlapping"} value)
      (throw (Exception. "Invalid card type")))
    (db/set-setting user-id CARD_TYPE value)
    {:success true}
    (catch Exception e
      (tel/error! {:id ::save-card-type} e)
      {:success false :error "Failed to save card type"})))

(defn save-active-tab [user-id tab]
  (try
    (db/set-setting user-id ACTIVE_TAB (name tab))
    {:success true}
    (catch Exception e
      (tel/error! {:id ::save-active-tab} e)
      {:success false :error "Failed to save active tab"})))

(defn save-card-count [user-id value]
  (try
    (let [parsed (Integer/parseInt (str value))
          clamped (max 1 (min 20 parsed))] ; Enforce 1-20 range
      (db/set-setting user-id CARD_COUNT (str clamped))
      {:success true})
    (catch Exception e
      (tel/error! {:id ::save-card-count} e)
      {:success false :error "Failed to save card count"})))

(defn get-last-document [user-id]
  (try
    (when-let [v (db/get-setting user-id LAST_DOCUMENT)]
      (let [n (Integer/parseInt v)]
        (when (pos? n) n)))
    (catch Exception _ nil)))

(defn save-last-document [user-id doc-id]
  (try
    (db/set-setting user-id LAST_DOCUMENT (str doc-id))
    {:success true}
    (catch Exception e
      (tel/error! {:id ::save-last-document} e)
      {:success false})))

(defn get-last-page [user-id doc-id]
  (try
    (let [v (db/get-setting user-id (str "last_page_" doc-id))]
      (if v (max 1 (Integer/parseInt v)) 1))
    (catch Exception _ 1)))

(defn save-last-page [user-id doc-id page]
  (try
    (db/set-setting user-id (str "last_page_" doc-id) (str page))
    {:success true}
    (catch Exception e
      (tel/error! {:id ::save-last-page} e)
      {:success false})))

(defn get-pdf-layout [user-id doc-id]
  (try
    (or (db/get-setting user-id (str "pdf_layout_" doc-id)) "left-right")
    (catch Exception _ "left-right")))

(defn save-pdf-layout [user-id doc-id layout]
  (try
    (db/set-setting user-id (str "pdf_layout_" doc-id) layout)
    {:success true}
    (catch Exception e
      (tel/error! {:id ::save-pdf-layout} e)
      {:success false})))

;; Per-PDF quick-text-extraction engine: "client" (PDF.js) | "remote" (PDFBox).
;; nil = unset → Copy-text runs both and shows the compare modal.
(defn get-extract-style [user-id doc-id]
  (db/get-setting user-id (str "extract_style_" doc-id)))

(defn save-extract-style [user-id doc-id style]
  (try
    (db/set-setting user-id (str "extract_style_" doc-id) style)
    {:success true}
    (catch Exception e
      (tel/error! {:id ::save-extract-style} e)
      {:success false :error "Failed to save extraction style"})))

;; Per-document OCR model for "Scan Page". Value is an :id from
;; freememo.ocr-models/registry. nil = unset → use the global default (get-model).
(defn get-ocr-model [user-id doc-id]
  (db/get-setting user-id (str "ocr_model_" doc-id)))

(defn save-ocr-model [user-id doc-id model-id]
  (try
    (db/set-setting user-id (str "ocr_model_" doc-id) model-id)
    {:success true}
    (catch Exception e
      (tel/error! {:id ::save-ocr-model} e)
      {:success false :error "Failed to save OCR model"})))

;; Per-document card-generation model. Value is an :id from
;; freememo.card-models/registry. nil/"" = unset → global default (get-model).
(defn get-card-model
  "Raw per-document card-model selection; nil/\"\" when unset."
  [user-id root-topic-id]
  (db/get-setting user-id (str "card_model_" root-topic-id)))

(defn save-card-model
  "Persist a per-document card-model selection.
   Pre:  `value` is \"\" (clear the override) or a card-models :id (else rejected —
         caller bug: pickers only emit allowed ids).
   Post: setting stored, or {:success false :error}."
  [user-id root-topic-id value]
  (if (and (seq value) (not (card-models/resolve-model value)))
    {:success false :error (str "Unknown card model: " value)}
    (try
      (db/set-setting user-id (str "card_model_" root-topic-id) value)
      {:success true}
      (catch Exception e
        (tel/error! {:id ::save-card-model} e)
        {:success false :error "Failed to save card model"}))))

(defn effective-card-model
  "Card-model :id used when generating cards under `root-topic-id`: the
   per-document selection when set and still allowed, else the user's global
   default. Post: an :id present in (card-model-ids)."
  [user-id root-topic-id]
  (let [per-doc (get-card-model user-id root-topic-id)]
    (if (some #{per-doc} (card-model-ids))
      per-doc
      (get-model user-id))))

;; Per-document assistant-chat model. Value is an :id from card-models/registry.
;; nil/"" = unset → global default (get-assistant-model).
(defn get-assistant-model-for
  "Raw per-document assistant-model selection; nil/\"\" when unset."
  [user-id root-topic-id]
  (db/get-setting user-id (str "assistant_model_" root-topic-id)))

(defn save-assistant-model-for
  "Persist a per-document assistant-model selection.
   Pre:  `value` is \"\" (clear) or a card-models :id (else rejected — caller bug).
   Post: setting stored, or {:success false :error}."
  [user-id root-topic-id value]
  (if (and (seq value) (not (card-models/resolve-model value)))
    {:success false :error (str "Unknown assistant model: " value)}
    (try
      (db/set-setting user-id (str "assistant_model_" root-topic-id) value)
      {:success true}
      (catch Exception e
        (tel/error! {:id ::save-assistant-model-for} e)
        {:success false :error "Failed to save assistant model"}))))

(defn effective-assistant-model
  "Assistant-model :id for chatting about `root-topic-id`: the per-document
   selection when set and still allowed, else the user's global assistant model.
   Post: an :id present in (card-model-ids)."
  [user-id root-topic-id]
  (let [per-doc (get-assistant-model-for user-id root-topic-id)]
    (if (some #{per-doc} (card-model-ids))
      per-doc
      (get-assistant-model user-id))))

;; User's global default OCR model (an :id from freememo.ocr-models/registry),
;; used when a document has no per-document selection. nil = unset → registry default.
(defn get-ocr-model-default [user-id]
  (db/get-setting user-id "ocr_model_default"))

(defn save-ocr-model-default [user-id model-id]
  (try
    (db/set-setting user-id "ocr_model_default" model-id)
    {:success true}
    (catch Exception e
      (tel/error! {:id ::save-ocr-model-default} e)
      {:success false :error "Failed to save default OCR model"})))

;; Per-document pane open/closed state. Keys: "hierarchy_open_<root-id>",
;; "pins_open_<root-id>". State is shared across every topic in the same
;; document tree (PDF root + pages + extracts) so collapsing the panel on
;; one page persists when navigating to a sibling page. Missing key →
;; default open (true).
(defn- pane-scope-id
  "Resolves the topic-id under which pane state is keyed: the document root
   (topmost ancestor via parent_id), or `topic-id` itself when the ancestor
   walk yields nil (orphan / unknown row)."
  [topic-id]
  (or (db/get-root-topic-id topic-id) topic-id))

(defn get-hierarchy-open [user-id topic-id]
  (try
    (let [v (db/get-setting user-id (str "hierarchy_open_" (pane-scope-id topic-id)))]
      (if (nil? v) true (= v "true")))
    (catch Exception _ true)))

(defn save-hierarchy-open [user-id topic-id open?]
  (try
    (db/set-setting user-id (str "hierarchy_open_" (pane-scope-id topic-id)) (str (boolean open?)))
    {:success true}
    (catch Exception e
      (tel/error! {:id ::save-hierarchy-open} e)
      {:success false})))

(defn get-pins-open [user-id topic-id]
  (try
    (let [v (db/get-setting user-id (str "pins_open_" (pane-scope-id topic-id)))]
      (if (nil? v) true (= v "true")))
    (catch Exception _ true)))

(defn save-pins-open [user-id topic-id open?]
  (try
    (db/set-setting user-id (str "pins_open_" (pane-scope-id topic-id)) (str (boolean open?)))
    {:success true}
    (catch Exception e
      (tel/error! {:id ::save-pins-open} e)
      {:success false})))

;; Per-document pane widths in pixels. Keys: "hierarchy_width_<root-id>",
;; "pins_width_<root-id>", scoped like the open/closed state above. Missing or
;; unparseable value → panel default.
(defn- get-pane-width [user-id topic-id key-prefix default-px]
  (try
    (let [v (db/get-setting user-id (str key-prefix (pane-scope-id topic-id)))]
      (if v (Long/parseLong v) default-px))
    (catch Exception _ default-px)))

(defn- save-pane-width [user-id topic-id key-prefix px error-id]
  (try
    (db/set-setting user-id (str key-prefix (pane-scope-id topic-id)) (str (long px)))
    {:success true}
    (catch Exception e
      (tel/error! {:id error-id} e)
      {:success false})))

(defn get-hierarchy-width [user-id topic-id]
  (get-pane-width user-id topic-id "hierarchy_width_" 280))

(defn save-hierarchy-width [user-id topic-id px]
  (save-pane-width user-id topic-id "hierarchy_width_" px ::save-hierarchy-width))

(defn get-pins-width [user-id topic-id]
  (get-pane-width user-id topic-id "pins_width_" 180))

(defn save-pins-width [user-id topic-id px]
  (save-pane-width user-id topic-id "pins_width_" px ::save-pins-width))

;; Right-panel active tab ("pins" | "assistant"), scoped per document like the
;; pane open/width state above. Missing key → "pins".
(defn get-assistant-tab [user-id topic-id]
  (try
    (let [v (db/get-setting user-id (str "assistant_tab_" (pane-scope-id topic-id)))]
      (if (#{"pins" "assistant"} v) v "pins"))
    (catch Exception _ "pins")))

(defn save-assistant-tab [user-id topic-id tab]
  (try
    (db/set-setting user-id (str "assistant_tab_" (pane-scope-id topic-id))
      (if (#{"pins" "assistant"} tab) tab "pins"))
    {:success true}
    (catch Exception e
      (tel/error! {:id ::save-assistant-tab} e)
      {:success false})))

(defn add-to-history [history new-prompt]
  (->> (cons new-prompt history)
    (distinct)
    (take 50)
    (vec)))

(defn get-pre-prompt-history [user-id]
  (try
    (let [raw (db/get-setting user-id PRE_PROMPT_HISTORY)]
      (if (seq raw)
        (vec (remove str/blank? (str/split-lines raw)))
        []))
    (catch Exception e
      (tel/error! {:id ::get-pre-prompt-history} e)
      [])))

(defn save-pre-prompt-history [user-id history-vec]
  (try
    (db/set-setting user-id PRE_PROMPT_HISTORY (str/join "\n" history-vec))
    {:success true}
    (catch Exception e
      (tel/error! {:id ::save-pre-prompt-history} e)
      {:success false :error "Failed to save prompt history"})))

(defn get-card-font-size [user-id]
  (try
    (Integer/parseInt (or (db/get-setting user-id CARD_FONT_SIZE) "13"))
    (catch Exception _ 13)))

(defn save-card-font-size [user-id value]
  (try
    (let [parsed (Integer/parseInt (str value))
          clamped (max 10 (min 20 parsed))]
      (db/set-setting user-id CARD_FONT_SIZE (str clamped))
      {:success true})
    (catch Exception e
      (tel/error! {:id ::save-card-font-size} e)
      {:success false :error "Failed to save card font size"})))

(defn get-card-split
  "Global per-user content↕card-table split, as a percentage in [15,85].
   Returns nil when unset — caller falls back to the client default
   (default-split-pct, which depends on window height)."
  [user-id]
  (try
    (when-let [v (db/get-setting user-id CARD_SPLIT)]
      (-> (Double/parseDouble v) (max 15.0) (min 85.0)))
    (catch Exception _ nil)))

(defn save-card-split [user-id value]
  (try
    (let [clamped (-> (Double/parseDouble (str value)) (max 15.0) (min 85.0))]
      (db/set-setting user-id CARD_SPLIT (str clamped))
      {:success true})
    (catch Exception e
      (tel/error! {:id ::save-card-split} e)
      {:success false :error "Failed to save card split"})))

;; --- FSRS scheduling config (quiz Review flow) ---------------------------
;; Only retention / daily caps / fuzz are user-tunable; learning steps and
;; maximum interval stay at the FSRS-6 defaults baked into fsrs/make-scheduler.

(defn get-fsrs-retention
  "Desired retention in [0.7, 0.99]; default 0.9."
  [user-id]
  (try (-> (or (db/get-setting user-id FSRS_RETENTION) "0.9")
         Double/parseDouble (max 0.7) (min 0.99))
    (catch Exception _ 0.9)))

(defn get-fsrs-new-per-day
  "New cards introduced per day, in [0, 9999]; default 20."
  [user-id]
  (try (-> (or (db/get-setting user-id FSRS_NEW_PER_DAY) "20")
         Integer/parseInt (max 0) (min 9999))
    (catch Exception _ 20)))

(defn get-fsrs-review-per-day
  "Review cards per day, in [0, 99999]; default 9999 (effectively uncapped)."
  [user-id]
  (try (-> (or (db/get-setting user-id FSRS_REVIEW_PER_DAY) "9999")
         Integer/parseInt (max 0) (min 99999))
    (catch Exception _ 9999)))

(defn get-fsrs-fuzz
  "Whether interval fuzzing is on; default true."
  [user-id]
  (not= "false" (db/get-setting user-id FSRS_FUZZ)))

(defn fsrs-config
  "A user's FSRS scheduler + daily caps, resolved from settings (defaults when
   unset). Single owner of config assembly for the Review flow.
   Post: {:scheduler <fsrs scheduler map> :enable-fuzzing bool
          :new-per-day int :review-per-day int}."
  [user-id]
  (let [fuzz (get-fsrs-fuzz user-id)]
    {:scheduler (fsrs/make-scheduler {:desired-retention (get-fsrs-retention user-id)
                                      :enable-fuzzing fuzz})
     :enable-fuzzing fuzz
     :new-per-day (get-fsrs-new-per-day user-id)
     :review-per-day (get-fsrs-review-per-day user-id)}))

(defn get-scan-dpi [user-id]
  (try
    (Integer/parseInt (or (db/get-setting user-id SCAN_DPI) "150"))
    (catch Exception _ 150)))

(defn save-scan-dpi [user-id value]
  (try
    (let [parsed (Integer/parseInt (str value))
          clamped (max 72 (min 300 parsed))]
      (db/set-setting user-id SCAN_DPI (str clamped))
      {:success true})
    (catch Exception e
      (tel/error! {:id ::save-scan-dpi} e)
      {:success false :error "Failed to save scan DPI"})))

(defn get-card-gen-max-retries
  "Max card-generation attempts on count mismatch (1–3, default 2). All attempts
   are billed (§5.4.5), so a lower value caps billed retries."
  [user-id]
  (try
    (max 1 (min 3 (Integer/parseInt (or (db/get-setting user-id CARD_GEN_MAX_RETRIES) "2"))))
    (catch Exception _ 2)))

(defn save-card-gen-max-retries [user-id value]
  (try
    (let [n (max 1 (min 3 (Integer/parseInt (str value))))]
      (db/set-setting user-id CARD_GEN_MAX_RETRIES (str n))
      {:success true})
    (catch Exception e
      (tel/error! {:id ::save-card-gen-max-retries} e)
      {:success false :error "Failed to save retry setting"})))

(defn get-theme [user-id]
  (or (db/get-setting user-id THEME) "auto"))

(defn save-theme [user-id value]
  (try
    (when-not (#{"auto" "light" "dark"} value)
      (throw (Exception. "Invalid theme")))
    (db/set-setting user-id THEME value)
    {:success true}
    (catch Exception e
      (tel/error! {:id ::save-theme} e)
      {:success false :error "Failed to save theme"})))

;; Extraction button visibility toggles.
(defn save-anki-sync-settings [user-id {:keys [scope deck allow-dupes use-tags tags]}]
  (try
    (when scope (db/set-setting user-id ANKI_SCOPE scope))
    (when deck (db/set-setting user-id ANKI_DECK deck))
    (db/set-setting user-id ANKI_ALLOW_DUPES (str (boolean allow-dupes)))
    ;; Header is a per-PDF setting (see save-anki-header-for-topic!), persisted
    ;; on edit — NOT written here; it has no account-wide/global row.
    (db/set-setting user-id ANKI_USE_TAGS (str (boolean use-tags)))
    (db/set-setting user-id ANKI_TAGS (pr-str (or tags [])))
    {:success true}
    (catch Exception e
      (tel/error! {:id ::save-anki-sync-settings} e)
      {:success false :error "Failed to save Anki sync settings"})))

;; ── Per-item Anki sync presets ──

(defn anki-preset-key [root-topic-id]
  (str "anki_preset_" root-topic-id))

(defn get-anki-preset [user-id root-topic-id]
  (try
    (when-let [raw (db/get-setting user-id (anki-preset-key root-topic-id))]
      (clojure.edn/read-string raw))
    (catch Exception e
      (tel/error! {:id ::get-anki-preset} e)
      nil)))

(defn save-anki-preset [user-id root-topic-id preset-map]
  (try
    (db/set-setting user-id (anki-preset-key root-topic-id) (pr-str preset-map))
    {:success true}
    (catch Exception e
      (tel/error! {:id ::save-anki-preset} e)
      {:success false :error "Failed to save item preset"})))

;; ── Per-PDF Anki header override ──
;;
;; Header (use-header + header-text) persists per root topic in its own
;; settings rows, independent of the per-item preset blob. An absent row means
;; "no per-PDF override" (nil), which resolve-anki-header treats as header off —
;; there is no account-wide/global header. Saved on edit (blur/toggle), on
;; modal close, and on push.

(defn anki-use-header-key [root-topic-id]
  (str "anki_use_header_" root-topic-id))

(defn anki-header-text-key [root-topic-id]
  (str "anki_header_text_" root-topic-id))

(defn get-anki-header-for-topic
  "Per-PDF header override for root-topic-id.
   {:use-header bool-or-nil, :header-text string-or-nil}; nil for a field means
   no override (resolve-anki-header treats this as header off)."
  [user-id root-topic-id]
  (let [raw-use (db/get-setting user-id (anki-use-header-key root-topic-id))
        raw-text (db/get-setting user-id (anki-header-text-key root-topic-id))]
    {:use-header (when (some? raw-use) (= "true" raw-use))
     :header-text raw-text}))

(defn save-anki-header-for-topic!
  "Persist the per-PDF header override. header-text is saved verbatim — an
   explicit empty string is a valid override, NOT a fallback-to-global signal."
  [user-id root-topic-id use-header header-text]
  (try
    (db/set-setting user-id (anki-use-header-key root-topic-id) (str (boolean use-header)))
    (db/set-setting user-id (anki-header-text-key root-topic-id) (or header-text ""))
    {:success true}
    (catch Exception e
      (tel/error! {:id ::save-anki-header-for-topic!} e)
      {:success false :error "Failed to save per-PDF header"})))

(defn resolve-anki-header
  "Effective header for a topic: the per-PDF override, else no header.
   A missing override (nil field) means header off — there is no account-wide
   default. Single source of truth for the modal's display and every push path."
  [user-id root-topic-id]
  (let [pdf (get-anki-header-for-topic user-id root-topic-id)]
    {:use-header  (boolean (:use-header pdf))
     :header-text (or (:header-text pdf) "")}))

;; ── Prompt overrides ──

(def default-system-prompt
  (delay
    (try (slurp (io/resource "prompts/system.md"))
      (catch Exception _ nil))))

(def default-ocr-prompt
  "Extract all text from this image and return it as clean, semantic HTML. Use:
- <h1>, <h2>, <h3> for headings
- <p> for paragraphs
- <ul><li> and <ol><li> for lists
- <strong> for bold/important text
- <em> for italic/emphasized text
- <br> for line breaks within paragraphs
Preserve the reading order and document structure. Return only the HTML body content (no <html> or <body> tags).
IMPORTANT: Do NOT wrap the HTML in markdown code fences (```html or ```). Return raw HTML only.")

(defn get-default-system-prompt [] @default-system-prompt)
(defn get-default-ocr-prompt [] default-ocr-prompt)

(defn get-system-prompt [user-id]
  (or (db/get-setting user-id PROMPT_SYSTEM)
    @default-system-prompt))

(defn get-ocr-prompt [user-id]
  (or (db/get-setting user-id PROMPT_OCR)
    default-ocr-prompt))

(defn save-system-prompt [user-id value]
  (try
    (input/check-length! :system-prompt value input/prompt-max)
    (db/set-setting user-id PROMPT_SYSTEM value)
    {:success true}
    (catch clojure.lang.ExceptionInfo e
      (if (input/length-error? (ex-data e))
        {:success false :error (.getMessage e)}
        (do (tel/error! {:id ::save-system-prompt} e)
            {:success false :error "Failed to save system prompt"})))
    (catch Exception e
      (tel/error! {:id ::save-system-prompt} e)
      {:success false :error "Failed to save system prompt"})))

(defn save-ocr-prompt [user-id value]
  (try
    (input/check-length! :ocr-prompt value input/prompt-max)
    (db/set-setting user-id PROMPT_OCR value)
    {:success true}
    (catch clojure.lang.ExceptionInfo e
      (if (input/length-error? (ex-data e))
        {:success false :error (.getMessage e)}
        (do (tel/error! {:id ::save-ocr-prompt} e)
            {:success false :error "Failed to save OCR prompt"})))
    (catch Exception e
      (tel/error! {:id ::save-ocr-prompt} e)
      {:success false :error "Failed to save OCR prompt"})))

(defn- reset-prompt!
  "Delete a prompt setting, snapshotting the prior custom value for undo with
   an Undo toast. No undo entry when the prompt was already at default (nil)."
  [user-id setting-key label]
  (let [old (db/get-setting user-id setting-key)]
    (db/delete-setting user-id setting-key)
    (when (some? old)
      (let [undo-id (db/insert-undo-entry! user-id "reset-prompt" "setting"
                      [setting-key] [{:key setting-key :value old}])]
        (toasts/push! user-id {:level :success
                               :message (str label " reset")
                               :dedup? false
                               :actions [{:label "Undo" :undo-id undo-id}]})))))

(defn reset-system-prompt [user-id]
  (try
    (reset-prompt! user-id PROMPT_SYSTEM "System prompt")
    {:success true}
    (catch Exception e
      (tel/error! {:id ::reset-system-prompt} e)
      {:success false :error "Failed to reset system prompt"})))

(defn reset-ocr-prompt [user-id]
  (try
    (reset-prompt! user-id PROMPT_OCR "OCR prompt")
    {:success true}
    (catch Exception e
      (tel/error! {:id ::reset-ocr-prompt} e)
      {:success false :error "Failed to reset OCR prompt"})))

;; ── Per-item custom prompt ──
;; Stored per-topic in the settings KV table, keyed "custom_prompt_<topic-id>".
;; Blank = inherit. Mirrors get/save-ocr-model: no refresh bump — the editor
;; re-reads on modal mount and generation reads fresh server-side.
(defn get-custom-prompt
  "This topic's OWN custom card-generation prompt (no ancestor walk). nil = unset."
  [user-id topic-id]
  (db/get-setting user-id (str "custom_prompt_" topic-id)))

(defn save-custom-prompt [user-id topic-id value]
  (try
    (input/check-length! :custom-prompt value input/prompt-max)
    (db/set-setting user-id (str "custom_prompt_" topic-id) value)
    {:success true}
    (catch clojure.lang.ExceptionInfo e
      (if (input/length-error? (ex-data e))
        {:success false :error (.getMessage e)}
        (do (tel/error! {:id ::save-custom-prompt} e)
            {:success false :error "Failed to save custom prompt"})))
    (catch Exception e
      (tel/error! {:id ::save-custom-prompt} e)
      {:success false :error "Failed to save custom prompt"})))

(defn get-effective-system-prompt
  "Global system prompt with this topic's nearest per-item custom prompt appended.
   Walks topic-id's ancestor chain (self→root) and appends the first non-blank
   custom prompt found; topic-id nil or no override → global prompt unchanged."
  [user-id topic-id]
  (let [base (get-system-prompt user-id)
        ancestor-ids (db/get-ancestor-ids topic-id)
        prompts (db/get-settings user-id (map #(str "custom_prompt_" %) ancestor-ids))
        override (some (fn [tid]
                         (let [v (get prompts (str "custom_prompt_" tid))]
                           (when-not (str/blank? v) v)))
                   ancestor-ids)]
    (if (str/blank? override)
      base
      (str base "\n\n## Document-specific instructions\n\n" override))))

;; ── Zotero (per-user; values are stored as strings) ─────────────────

(defn zotero-enabled?
  "Single read site for the per-user Zotero feature flag.
   Pre:  user-id non-nil.
   Post: boolean — true when the user has explicitly enabled Zotero import."
  [user-id]
  (= "true" (db/get-setting user-id ZOTERO_ENABLED)))

(defn save-zotero-enabled [user-id value]
  (try
    (db/set-setting user-id ZOTERO_ENABLED (str (boolean value)))
    {:success true}
    (catch Exception e
      (tel/error! {:id ::save-zotero-enabled} e)
      {:success false :error "Failed to save Zotero enabled"})))
