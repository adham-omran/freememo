(ns electric-starter-app.settings
  "Business logic for application settings."
  (:require
    [electric-starter-app.db :as db]
    [clojure.string :as str]))

;; Setting keys (constants)
(def OPENAI_API_KEY "openai_api_key")
(def MODEL "model")
(def CARD_COUNT "card_count")
(def REASONING "reasoning")
(def VERBOSITY "verbosity")
(def CONTEXT_ENABLED "context_enabled")
(def CONTEXT_PAGES "context_pages")
(def CARD_TYPE "card_type")

;; Get functions
(defn get-openai-api-key []
  (or (db/get-setting OPENAI_API_KEY) ""))

(defn get-model []
  (or (db/get-setting MODEL) "gpt-5.1"))

(defn get-card-count []
  (try
    (Integer/parseInt (or (db/get-setting CARD_COUNT) "5"))
    (catch Exception e
      (println "ERROR [get-card-count]:" (.getMessage e))
      5)))

(defn get-reasoning []
  (or (db/get-setting REASONING) "low"))

(defn get-verbosity []
  (or (db/get-setting VERBOSITY) "low"))

(defn get-context-enabled []
  (try
    (= "true" (db/get-setting CONTEXT_ENABLED))
    (catch Exception e
      (println "ERROR [get-context-enabled]:" (.getMessage e))
      false)))

(defn get-context-pages []
  (try
    (Integer/parseInt (or (db/get-setting CONTEXT_PAGES) "3"))
    (catch Exception e
      (println "ERROR [get-context-pages]:" (.getMessage e))
      3)))

(defn get-card-type []
  (try
    (or (db/get-setting CARD_TYPE) "basic")
    (catch Exception e
      (println "ERROR [get-card-type]:" (.getMessage e))
      "basic")))

;; Save functions with validation
(defn save-openai-api-key [api-key]
  (try
    (let [trimmed (str/trim (or api-key ""))]
      (db/set-setting OPENAI_API_KEY trimmed)
      {:success true})
    (catch Exception e
      (println "ERROR [save-openai-api-key]:" (.getMessage e))
      {:success false :error "Failed to save API key"})))

(defn save-model [value]
  (try
    (db/set-setting MODEL value)
    {:success true}
    (catch Exception e
      (println "ERROR [save-model]:" (.getMessage e))
      {:success false :error "Failed to save model"})))

(defn save-reasoning [value]
  (try
    (db/set-setting REASONING value)
    {:success true}
    (catch Exception e
      (println "ERROR [save-reasoning]:" (.getMessage e))
      {:success false :error "Failed to save reasoning"})))

(defn save-verbosity [value]
  (try
    (db/set-setting VERBOSITY value)
    {:success true}
    (catch Exception e
      (println "ERROR [save-verbosity]:" (.getMessage e))
      {:success false :error "Failed to save verbosity"})))

(defn save-context-enabled [value]
  (try
    (db/set-setting CONTEXT_ENABLED (str value))
    {:success true}
    (catch Exception e
      (println "ERROR [save-context-enabled]:" (.getMessage e))
      {:success false :error "Failed to save context enabled"})))

(defn save-context-pages [value]
  (try
    (let [parsed (Integer/parseInt (str value))
          clamped (max 1 (min 10 parsed))]  ; Enforce 1-10 range
      (db/set-setting CONTEXT_PAGES (str clamped))
      {:success true})
    (catch Exception e
      (println "ERROR [save-context-pages]:" (.getMessage e))
      {:success false :error "Failed to save context pages"})))

(defn save-card-type [value]
  (try
    (when-not (#{"basic" "cloze"} value)
      (throw (Exception. "Invalid card type")))
    (db/set-setting CARD_TYPE value)
    {:success true}
    (catch Exception e
      (println "ERROR [save-card-type]:" (.getMessage e))
      {:success false :error "Failed to save card type"})))

(defn save-card-count [value]
  (try
    (let [parsed (Integer/parseInt (str value))
          clamped (max 1 (min 50 parsed))]  ; Enforce 1-50 range
      (db/set-setting CARD_COUNT (str clamped))
      {:success true})
    (catch Exception e
      (println "ERROR [save-card-count]:" (.getMessage e))
      {:success false :error "Failed to save card count"})))
