(ns freememo.settings
  "Business logic for application settings."
  (:require
   [freememo.db :as db]
   [freememo.crypto :as crypto]
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
(def OPENAI_API_KEY "openai_api_key")
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
(def ENABLE_AI_SCAN_BUTTON "enable_ai_scan_button")
(def ENABLE_PDFBOX_BUTTON "enable_pdfbox_button")
(def ENABLE_PDFJS_BUTTON "enable_pdfjs_button")
(def ZOTERO_ENABLED "zotero_enabled")
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

;; OpenAI key helpers — per-user (BYOK), with optional server-wide demo
;; fallback via OPENAI_DEMO_KEY env var.
(defn- get-user-openai-api-key [user-id enc-key]
  (let [k (crypto/decrypt (or (db/get-setting user-id OPENAI_API_KEY) "") enc-key)]
    (when (seq k) k)))

(defn- get-shared-openai-api-key []
  (let [k (some-> (System/getenv "OPENAI_DEMO_KEY") str/trim)]
    (when (seq k) k)))

(defn get-openai-api-key
  "Resolve the OpenAI key for a request.
   Official (CREDITS_ENABLED): the platform key only — no BYO, no demo (Choice B).
   Self-host: per-user key → demo fallback (unchanged)."
  [user-id enc-key]
  (if (config/credits-enabled?)
    (config/platform-openai-api-key)
    (or (get-user-openai-api-key user-id enc-key)
        (get-shared-openai-api-key))))

(defn get-openai-api-key-status
  "Status for the settings UI. :platform in official mode (key block hidden);
   :user / :shared / :none in self-host."
  [user-id enc-key]
  (cond
    (config/credits-enabled?)
    {:source :platform :configured? (some? (config/platform-openai-api-key))}
    (some? (get-user-openai-api-key user-id enc-key))
    {:source :user :configured? true}
    (some? (get-shared-openai-api-key))
    {:source :shared :configured? true}
    :else
    {:source :none :configured? false}))

(defn get-model
  "Effective OpenAI model for OCR + card generation.
   In credits-enabled deployments the model is pinned via
   `freememo.config/!prod-model` (set by `src-prod/prod.cljc`) and any per-user
   DB setting is ignored. Throws `{:type ::prod-model-missing}` when
   `credits-enabled?` is on but the atom is nil — fail-closed, matching
   `credits/require-rates!`."
  [user-id]
  (if (config/credits-enabled?)
    (or @config/!prod-model
        (throw (ex-info "Prod model not set — src-prod/prod.cljc must reset! freememo.config/!prod-model"
                 {:type ::prod-model-missing})))
    (or (db/get-setting user-id MODEL) "gpt-5.1")))

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
(defn save-openai-api-key [user-id api-key enc-key]
  (try
    (let [trimmed (str/trim (or api-key ""))]
      (db/set-setting user-id OPENAI_API_KEY (crypto/encrypt trimmed enc-key))
      {:success true})
    (catch Exception e
      (tel/error! {:id ::save-openai-api-key} e)
      {:success false :error "Failed to save API key"})))

(defn save-model [user-id value]
  (try
    (db/set-setting user-id MODEL value)
    {:success true}
    (catch Exception e
      (tel/error! {:id ::save-model} e)
      {:success false :error "Failed to save model"})))

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
;; AI defaults on (preserves existing behaviour); native extractors default off (additive).
(defn get-enable-ai-scan-button [user-id]
  (let [v (db/get-setting user-id ENABLE_AI_SCAN_BUTTON)]
    (or (nil? v) (= "true" v))))

(defn save-enable-ai-scan-button [user-id value]
  (try
    (db/set-setting user-id ENABLE_AI_SCAN_BUTTON (str (boolean value)))
    {:success true}
    (catch Exception e
      (tel/error! {:id ::save-enable-ai-scan-button} e)
      {:success false :error "Failed to save AI scan button setting"})))

(defn get-enable-pdfbox-button [user-id]
  (= "true" (db/get-setting user-id ENABLE_PDFBOX_BUTTON)))

(defn save-enable-pdfbox-button [user-id value]
  (try
    (db/set-setting user-id ENABLE_PDFBOX_BUTTON (str (boolean value)))
    {:success true}
    (catch Exception e
      (tel/error! {:id ::save-enable-pdfbox-button} e)
      {:success false :error "Failed to save PDFBox button setting"})))

(defn get-enable-pdfjs-button [user-id]
  (= "true" (db/get-setting user-id ENABLE_PDFJS_BUTTON)))

(defn save-enable-pdfjs-button [user-id value]
  (try
    (db/set-setting user-id ENABLE_PDFJS_BUTTON (str (boolean value)))
    {:success true}
    (catch Exception e
      (tel/error! {:id ::save-enable-pdfjs-button} e)
      {:success false :error "Failed to save PDF.js button setting"})))

(defn save-anki-sync-settings [user-id {:keys [scope deck basic-model cloze-model allow-dupes use-header header-text use-tags tags]}]
  (try
    (when scope (db/set-setting user-id ANKI_SCOPE scope))
    (when deck (db/set-setting user-id ANKI_DECK deck))
    (when basic-model (db/set-setting user-id ANKI_BASIC_MODEL basic-model))
    (when cloze-model (db/set-setting user-id ANKI_CLOZE_MODEL cloze-model))
    (db/set-setting user-id ANKI_ALLOW_DUPES (str (boolean allow-dupes)))
    (db/set-setting user-id ANKI_USE_HEADER (str (boolean use-header)))
    (when (some? header-text) (db/set-setting user-id ANKI_HEADER_TEXT header-text))
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
