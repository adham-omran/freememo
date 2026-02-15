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

;; Get functions
(defn get-openai-api-key []
  (or (db/get-setting OPENAI_API_KEY) ""))

(defn get-model []
  (or (db/get-setting MODEL) "gpt-5.1"))

(defn get-card-count []
  (Integer/parseInt (or (db/get-setting CARD_COUNT) "20")))

(defn get-reasoning []
  (or (db/get-setting REASONING) "low"))

(defn get-verbosity []
  (or (db/get-setting VERBOSITY) "low"))

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
