(ns electric-starter-app.settings
  "Business logic for application settings."
  (:require
   [electric-starter-app.db :as db]
   [electric-starter-app.crypto :as crypto]
   [clojure.string :as str]))

;; Toggle: set to false from REPL to disable shared key fallback
(defonce !use-shared-key (atom true))

;; Load the shared key once from resources/openai.txt (nil if file absent)
(defonce shared-api-key
  (delay
    (when-let [r (clojure.java.io/resource "openai.txt")]
      (let [k (clojure.string/trim (slurp r))]
        (when (seq k) k)))))

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
(def SOURCE_DISPLAY_MODE "source_display_mode")
(def LLM_ENABLED "llm_enabled")
(def LAST_DOCUMENT "last_document")
(def PRE_PROMPT_HISTORY "pre_prompt_history")
(def CARD_FONT_SIZE "card_font_size")
(def SCAN_DPI "scan_dpi")
; Per-document page keys are dynamic: (str "last_page_" doc-id)

;; OpenAI key helpers
(defn- get-user-openai-api-key [user-id enc-key]
  (crypto/decrypt (or (db/get-setting user-id OPENAI_API_KEY) "") enc-key))

(defn- get-shared-openai-api-key []
  (when @!use-shared-key @shared-api-key))

;; Get functions
(defn get-openai-api-key [user-id enc-key]
  (let [user-key (get-user-openai-api-key user-id enc-key)]
    (if (seq user-key)
      user-key
      (get-shared-openai-api-key))))

(defn get-openai-api-key-status [user-id enc-key]
  (let [user-key (get-user-openai-api-key user-id enc-key)
        shared-key (get-shared-openai-api-key)]
    (cond
      (seq user-key) {:source :user :configured? true}
      (seq shared-key) {:source :shared :configured? true}
      :else {:source :none :configured? false})))

(defn get-model [user-id]
  (or (db/get-setting user-id MODEL) "gpt-5.1"))

(defn get-card-count [user-id]
  (try
    (Integer/parseInt (or (db/get-setting user-id CARD_COUNT) "5"))
    (catch Exception e
      (println "ERROR [get-card-count]:" (.getMessage e))
      5)))

(defn get-reasoning [user-id]
  (or (db/get-setting user-id REASONING) "low"))

(defn get-verbosity [user-id]
  (or (db/get-setting user-id VERBOSITY) "low"))

(defn get-context-enabled [user-id]
  (try
    (= "true" (db/get-setting user-id CONTEXT_ENABLED))
    (catch Exception e
      (println "ERROR [get-context-enabled]:" (.getMessage e))
      false)))

(defn get-context-pages [user-id]
  (try
    (Integer/parseInt (or (db/get-setting user-id CONTEXT_PAGES) "3"))
    (catch Exception e
      (println "ERROR [get-context-pages]:" (.getMessage e))
      3)))

(defn get-active-tab [user-id]
  (try
    (let [raw (db/get-setting user-id ACTIVE_TAB)
          valid #{"home" "settings" "library" "import" "learn" "queue"}]
      (if (valid raw)
        (keyword raw)
        (case raw
          "pdf"       :library
          "contents"  :library
          "workspace" :learn
          :home)))
    (catch Exception e
      (println "ERROR [get-active-tab]:" (.getMessage e))
      :home)))

(defn get-card-type [user-id]
  (try
    (or (db/get-setting user-id CARD_TYPE) "basic")
    (catch Exception e
      (println "ERROR [get-card-type]:" (.getMessage e))
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
      (println "ERROR [save-source-display-mode]:" (.getMessage e))
      {:success false :error "Failed to save source display mode"})))

(defn get-llm-enabled [user-id]
  (let [v (db/get-setting user-id LLM_ENABLED)]
    (or (nil? v) (= "true" v))))

(defn save-llm-enabled [user-id value]
  (try
    (db/set-setting user-id LLM_ENABLED (str (boolean value)))
    {:success true}
    (catch Exception e
      (println "ERROR [save-llm-enabled]:" (.getMessage e))
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

;; Save functions with validation
(defn save-openai-api-key [user-id api-key enc-key]
  (try
    (let [trimmed (str/trim (or api-key ""))]
      (db/set-setting user-id OPENAI_API_KEY (crypto/encrypt trimmed enc-key))
      {:success true})
    (catch Exception e
      (println "ERROR [save-openai-api-key]:" (.getMessage e))
      {:success false :error "Failed to save API key"})))

(defn save-model [user-id value]
  (try
    (db/set-setting user-id MODEL value)
    {:success true}
    (catch Exception e
      (println "ERROR [save-model]:" (.getMessage e))
      {:success false :error "Failed to save model"})))

(defn save-reasoning [user-id value]
  (try
    (db/set-setting user-id REASONING value)
    {:success true}
    (catch Exception e
      (println "ERROR [save-reasoning]:" (.getMessage e))
      {:success false :error "Failed to save reasoning"})))

(defn save-verbosity [user-id value]
  (try
    (db/set-setting user-id VERBOSITY value)
    {:success true}
    (catch Exception e
      (println "ERROR [save-verbosity]:" (.getMessage e))
      {:success false :error "Failed to save verbosity"})))

(defn save-context-enabled [user-id value]
  (try
    (db/set-setting user-id CONTEXT_ENABLED (str value))
    {:success true}
    (catch Exception e
      (println "ERROR [save-context-enabled]:" (.getMessage e))
      {:success false :error "Failed to save context enabled"})))

(defn save-context-pages [user-id value]
  (try
    (let [parsed (Integer/parseInt (str value))
          clamped (max 1 (min 10 parsed))] ; Enforce 1-10 range
      (db/set-setting user-id CONTEXT_PAGES (str clamped))
      {:success true})
    (catch Exception e
      (println "ERROR [save-context-pages]:" (.getMessage e))
      {:success false :error "Failed to save context pages"})))

(defn save-card-type [user-id value]
  (try
    (when-not (#{"basic" "cloze"} value)
      (throw (Exception. "Invalid card type")))
    (db/set-setting user-id CARD_TYPE value)
    {:success true}
    (catch Exception e
      (println "ERROR [save-card-type]:" (.getMessage e))
      {:success false :error "Failed to save card type"})))

(defn save-active-tab [user-id tab]
  (try
    (db/set-setting user-id ACTIVE_TAB (name tab))
    {:success true}
    (catch Exception e
      (println "ERROR [save-active-tab]:" (.getMessage e))
      {:success false :error "Failed to save active tab"})))

(defn save-card-count [user-id value]
  (try
    (let [parsed (Integer/parseInt (str value))
          clamped (max 1 (min 50 parsed))] ; Enforce 1-50 range
      (db/set-setting user-id CARD_COUNT (str clamped))
      {:success true})
    (catch Exception e
      (println "ERROR [save-card-count]:" (.getMessage e))
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
      (println "ERROR [save-last-document]:" (.getMessage e))
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
      (println "ERROR [save-last-page]:" (.getMessage e))
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
      (println "ERROR [get-pre-prompt-history]:" (.getMessage e))
      [])))

(defn save-pre-prompt-history [user-id history-vec]
  (try
    (db/set-setting user-id PRE_PROMPT_HISTORY (str/join "\n" history-vec))
    {:success true}
    (catch Exception e
      (println "ERROR [save-pre-prompt-history]:" (.getMessage e))
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
      (println "ERROR [save-card-font-size]:" (.getMessage e))
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
      (println "ERROR [save-scan-dpi]:" (.getMessage e))
      {:success false :error "Failed to save scan DPI"})))

(defn save-anki-sync-settings [user-id {:keys [scope deck basic-model cloze-model allow-dupes use-header header-text]}]
  (try
    (when scope (db/set-setting user-id ANKI_SCOPE scope))
    (when deck (db/set-setting user-id ANKI_DECK deck))
    (when basic-model (db/set-setting user-id ANKI_BASIC_MODEL basic-model))
    (when cloze-model (db/set-setting user-id ANKI_CLOZE_MODEL cloze-model))
    (db/set-setting user-id ANKI_ALLOW_DUPES (str (boolean allow-dupes)))
    (db/set-setting user-id ANKI_USE_HEADER (str (boolean use-header)))
    (when (some? header-text) (db/set-setting user-id ANKI_HEADER_TEXT header-text))
    {:success true}
    (catch Exception e
      (println "ERROR [save-anki-sync-settings]:" (.getMessage e))
      {:success false :error "Failed to save Anki sync settings"})))
