(ns electric-starter-app.settings
  "Business logic for application settings."
  (:require
    [electric-starter-app.db :as db]
    [clojure.string :as str]))

;; Setting keys (constants)
(def OPENAI_API_KEY "openai_api_key")
(def MODEL "model")
(def CARD_COUNT "card_count")

;; Get functions
(defn get-openai-api-key []
  (or (db/get-setting OPENAI_API_KEY) ""))

(defn get-model []
  (or (db/get-setting MODEL) "gpt-4o"))

(defn get-card-count []
  (Integer/parseInt (or (db/get-setting CARD_COUNT) "20")))

;; Save functions with validation
(defn save-openai-api-key [api-key]
  (try
    (let [trimmed (str/trim (or api-key ""))]
      (db/set-setting OPENAI_API_KEY trimmed)
      {:success true})
    (catch Exception e
      (println "ERROR [save-openai-api-key]:" (.getMessage e))
      {:success false :error "Failed to save API key"})))
