(ns electric-starter-app.settings
  "Business logic for application settings."
  (:require
    [electric-starter-app.db :as db]
    [electric-starter-app.crypto :as crypto]
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
(defn get-openai-api-key [user-id enc-key]
  (crypto/decrypt (or (db/get-setting user-id OPENAI_API_KEY) "") enc-key))

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

(defn get-card-type [user-id]
  (try
    (or (db/get-setting user-id CARD_TYPE) "basic")
    (catch Exception e
      (println "ERROR [get-card-type]:" (.getMessage e))
      "basic")))

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
          clamped (max 1 (min 10 parsed))]  ; Enforce 1-10 range
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

(defn save-card-count [user-id value]
  (try
    (let [parsed (Integer/parseInt (str value))
          clamped (max 1 (min 50 parsed))]  ; Enforce 1-50 range
      (db/set-setting user-id CARD_COUNT (str clamped))
      {:success true})
    (catch Exception e
      (println "ERROR [save-card-count]:" (.getMessage e))
      {:success false :error "Failed to save card count"})))
