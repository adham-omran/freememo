(ns freememo.llm-edn
  "Parse an LLM chat completion whose prompt demanded EDN. Extracted from
   freememo.cards/parse-card-response so the KG pipeline shares one parser."
  (:require
   [taoensso.telemere :as tel]
   [cheshire.core :as json]
   [clojure.edn :as edn]
   [clojure.string :as str]))

(defn parse-response
  "Parse a model message into Clojure data. Prefers EDN; falls back to JSON
   with keywordized keys for models that emit JSON despite the instructions
   (observed: Gemini 3 Flash), so the keyword-keyed shape is identical.
   Strips markdown code fences first (```clojure/edn/json).
   Pre:  raw-text is the model message content (may be nil).
   Post: returns the parsed value; throws ex-info on unparseable input."
  [raw-text]
  (let [cleaned (-> (str raw-text)
                  str/trim
                  (str/replace #"^```(?:clojure|edn|json)?\s*\n?" "")
                  (str/replace #"\n?```\s*$" "")
                  str/trim)]
    (or (try
          (edn/read-string cleaned)
          (catch Exception _
            (try (json/parse-string cleaned true)
                 (catch Exception _ nil))))
        (do (tel/error! {:id ::parse-response
                         :data {:raw raw-text :cleaned cleaned}}
              "Failed to parse model response (neither EDN nor JSON)")
            (throw (ex-info "Failed to parse model response"
                     {:raw raw-text :cleaned cleaned}))))))
