(ns freememo.settings
  "Business logic for application settings."
  (:require
   [freememo.db :as db]
   [freememo.card-models :as card-models]
   [freememo.config :as config]
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
(def CARD_TYPE "card_type")
(def ACTIVE_TAB "active_tab")
(def ANKI_SCOPE "anki_scope")
(def ANKI_DECK "anki_deck")
(def ANKI_BASIC_MODEL "anki_basic_model")
(def ANKI_CLOZE_MODEL "anki_cloze_model")
(def ANKI_ALLOW_DUPES "anki_allow_dupes")
(def ANKI_USE_HEADER "anki_use_header")
(def ANKI_HEADER_TEXT "anki_header_text")
(def ANKI_USE_TAGS "anki_use_tags")
(def ANKI_TAGS "anki_tags")
(def ANKI_BASIC_FIELDS "anki_basic_fields")
(def ANKI_CLOZE_FIELDS "anki_cloze_fields")
(def ANKI_SOURCE_FIELD "anki_source_field")
(def ANKI_IMAGES_FRONT_FIELD "anki_images_front_field")
(def ANKI_IMAGES_BACK_FIELD "anki_images_back_field")
(def IMAGE_DISPLAY_MODE "image_display_mode")
(def ANKI_AUTO_LOAD_MODE "anki_auto_load_mode")
(def SOURCE_DISPLAY_MODE "source_display_mode")
(def BIBLIOGRAPHY_DISPLAY_MODE "bibliography_display_mode")
(def ANKI_BIBLIOGRAPHY_FIELD "anki_bibliography_field")
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
; Per-document page keys are dynamic: (str "last_page_" doc-id)

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

(defn get-active-tab [user-id]
  (try
    (let [raw (db/get-setting user-id ACTIVE_TAB)
          valid #{"home" "settings" "library" "import" "learn" "queue" "status"}]
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

(defn get-source-display-mode [user-id]
  (or (db/get-setting user-id SOURCE_DISPLAY_MODE) "append"))

(defn save-source-display-mode [user-id mode]
  (try
    (when-not (#{"append" "field"} mode)
      (throw (Exception. "Invalid source display mode")))
    (db/set-setting user-id SOURCE_DISPLAY_MODE mode)
    {:success true}
    (catch Exception e
      (tel/error! {:id ::save-source-display-mode} e)
      {:success false :error "Failed to save source display mode"})))

(defn get-bibliography-display-mode [user-id]
  (or (db/get-setting user-id BIBLIOGRAPHY_DISPLAY_MODE) "append"))

(defn save-bibliography-display-mode [user-id mode]
  (try
    (when-not (#{"off" "append" "field"} mode)
      (throw (Exception. "Invalid bibliography display mode")))
    (db/set-setting user-id BIBLIOGRAPHY_DISPLAY_MODE mode)
    {:success true}
    (catch Exception e
      (tel/error! {:id ::save-bibliography-display-mode} e)
      {:success false :error "Failed to save bibliography display mode"})))

(defn get-bibliography-field-name [user-id]
  (or (db/get-setting user-id ANKI_BIBLIOGRAPHY_FIELD) "Bibliography"))

(defn save-bibliography-field-name [user-id value]
  (try
    (db/set-setting user-id ANKI_BIBLIOGRAPHY_FIELD (or value "Bibliography"))
    {:success true}
    (catch Exception e
      (tel/error! {:id ::save-bibliography-field-name} e)
      {:success false :error "Failed to save Anki bibliography field name"})))

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

(defn get-anki-scope [user-id]
  (or (db/get-setting user-id ANKI_SCOPE) "Current Page"))

(defn get-anki-deck [user-id]
  (db/get-setting user-id ANKI_DECK))

(defn get-anki-basic-model [user-id]
  (db/get-setting user-id ANKI_BASIC_MODEL))

(defn get-anki-cloze-model [user-id]
  (db/get-setting user-id ANKI_CLOZE_MODEL))

(defn get-anki-allow-dupes [user-id]
  (= "true" (or (db/get-setting user-id ANKI_ALLOW_DUPES) "false")))

(defn get-anki-use-header [user-id]
  (= "true" (or (db/get-setting user-id ANKI_USE_HEADER) "false")))

(defn get-anki-header-text [user-id]
  (or (db/get-setting user-id ANKI_HEADER_TEXT) ""))

(defn get-anki-use-tags [user-id]
  (= "true" (or (db/get-setting user-id ANKI_USE_TAGS) "false")))

(defn get-anki-tags [user-id]
  (try
    (let [raw (db/get-setting user-id ANKI_TAGS)]
      (if (seq raw) (clojure.edn/read-string raw) []))
    (catch Exception _ [])))

(defn get-anki-basic-fields
  "User-level default field ordering for basic cards. Vector of field names
   (e.g. [\"Front\" \"Back\"]). Empty vector when unset."
  [user-id]
  (try
    (let [raw (db/get-setting user-id ANKI_BASIC_FIELDS)]
      (if (seq raw) (vec (clojure.edn/read-string raw)) []))
    (catch Exception _ [])))

(defn save-anki-basic-fields [user-id fields]
  (try
    (db/set-setting user-id ANKI_BASIC_FIELDS (pr-str (vec (or fields []))))
    {:success true}
    (catch Exception e
      (tel/error! {:id ::save-anki-basic-fields} e)
      {:success false :error "Failed to save Anki basic fields"})))

(defn get-anki-cloze-fields
  "User-level default field ordering for cloze cards. Vector of field names
   (e.g. [\"Text\"]). Empty vector when unset."
  [user-id]
  (try
    (let [raw (db/get-setting user-id ANKI_CLOZE_FIELDS)]
      (if (seq raw) (vec (clojure.edn/read-string raw)) []))
    (catch Exception _ [])))

(defn save-anki-cloze-fields [user-id fields]
  (try
    (db/set-setting user-id ANKI_CLOZE_FIELDS (pr-str (vec (or fields []))))
    {:success true}
    (catch Exception e
      (tel/error! {:id ::save-anki-cloze-fields} e)
      {:success false :error "Failed to save Anki cloze fields"})))

(defn get-anki-source-field [user-id]
  (or (db/get-setting user-id ANKI_SOURCE_FIELD) "Source"))

(defn save-anki-source-field [user-id value]
  (try
    (db/set-setting user-id ANKI_SOURCE_FIELD (or value "Source"))
    {:success true}
    (catch Exception e
      (tel/error! {:id ::save-anki-source-field} e)
      {:success false :error "Failed to save Anki source field name"})))

(defn get-anki-images-front-field [user-id]
  (or (db/get-setting user-id ANKI_IMAGES_FRONT_FIELD) "Images Front"))

(defn save-anki-images-front-field [user-id value]
  (try
    (db/set-setting user-id ANKI_IMAGES_FRONT_FIELD (or value "Images Front"))
    {:success true}
    (catch Exception e
      (tel/error! {:id ::save-anki-images-front-field} e)
      {:success false :error "Failed to save Anki front images field name"})))

(defn get-anki-images-back-field [user-id]
  (or (db/get-setting user-id ANKI_IMAGES_BACK_FIELD) "Images Back"))

(defn save-anki-images-back-field [user-id value]
  (try
    (db/set-setting user-id ANKI_IMAGES_BACK_FIELD (or value "Images Back"))
    {:success true}
    (catch Exception e
      (tel/error! {:id ::save-anki-images-back-field} e)
      {:success false :error "Failed to save Anki back images field name"})))

(defn get-image-display-mode
  "Returns \"inline\" (default; images stay in front/back/cloze HTML) or
   \"field\" (images extracted to dedicated Anki fields on push)."
  [user-id]
  (or (db/get-setting user-id IMAGE_DISPLAY_MODE) "inline"))

(defn save-image-display-mode [user-id mode]
  (try
    (when-not (#{"inline" "field"} mode)
      (throw (Exception. "Invalid image display mode")))
    (db/set-setting user-id IMAGE_DISPLAY_MODE mode)
    {:success true}
    (catch Exception e
      (tel/error! {:id ::save-image-display-mode} e)
      {:success false :error "Failed to save image display mode"})))

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

(defn save-card-type [user-id value]
  (try
    (when-not (#{"basic" "cloze"} value)
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
(defn save-anki-sync-settings [user-id {:keys [scope deck basic-model cloze-model allow-dupes use-tags tags]}]
  (try
    (when scope (db/set-setting user-id ANKI_SCOPE scope))
    (when deck (db/set-setting user-id ANKI_DECK deck))
    (when basic-model (db/set-setting user-id ANKI_BASIC_MODEL basic-model))
    (when cloze-model (db/set-setting user-id ANKI_CLOZE_MODEL cloze-model))
    (db/set-setting user-id ANKI_ALLOW_DUPES (str (boolean allow-dupes)))
    ;; Header is a per-PDF setting (see save-anki-header-for-topic!), persisted
    ;; on edit — NOT written here, so a push can't clobber the global fallback.
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
;; "no per-PDF override" (nil), so callers fall back to the global header.
;; Saved on edit (blur/toggle), on modal close, and on push.

(defn anki-use-header-key [root-topic-id]
  (str "anki_use_header_" root-topic-id))

(defn anki-header-text-key [root-topic-id]
  (str "anki_header_text_" root-topic-id))

(defn get-anki-header-for-topic
  "Per-PDF header override for root-topic-id.
   {:use-header bool-or-nil, :header-text string-or-nil}; nil for a field means
   no override (caller falls back to the global header)."
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
  "Effective header for a topic: per-PDF override when set, else the global
   header. Single source of truth for both the modal's display and push."
  [user-id root-topic-id]
  (let [pdf (get-anki-header-for-topic user-id root-topic-id)]
    {:use-header  (if (some? (:use-header pdf))  (:use-header pdf)  (get-anki-use-header user-id))
     :header-text (if (some? (:header-text pdf)) (:header-text pdf) (get-anki-header-text user-id))}))

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
