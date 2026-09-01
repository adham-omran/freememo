(ns freememo.llm-json
  "Strict JSON wire contracts for structured LLM prompts and responses.

   Prompt inputs enter through `write-prompt-input`. Model responses enter
   through `parse-response-field`. Callers own domain validation after this
   module returns the required root value."
  (:require
   [cheshire.core :as json]
   [clojure.string :as str]
   [taoensso.telemere :as tel])
  (:import [java.io StringReader]))

(defn write-prompt-input
  "Serialize one Clojure value as JSON for a structured prompt.
   Pre:  `value` contains only values that Cheshire can encode.
   Post: a JSON string that decodes to the same logical value."
  [value]
  (json/generate-string value))

(def ^:private root-type-predicates
  {:map map?
   :vector vector?})

(defn- read-single-json-value
  "Read exactly one JSON value and consume the complete input.
   Pre:  `raw-text` is a string.
   Post: a fully realized Clojure value with keywordized object keys.
   Throws a parser exception for invalid JSON or trailing non-whitespace."
  [raw-text]
  (let [values (vec (json/parsed-seq (StringReader. raw-text) true))]
    (if (= 1 (count values))
      (first values)
      (throw (ex-info "Expected one JSON value"
               {:json-value-count (count values)})))))

(defn parse-response-field
  "Parse one structured model response and return its required root field.

   Pre:  `raw-text` is a non-blank string from openrouter/message-content.
         `root-key` is a keyword. `root-type` is :map or :vector.
   Post: returns the fully realized root value with keywordized object keys.
         The response must contain exactly one top-level JSON object.
         The object must contain `root-key` with the requested type.
         Otherwise throws ::unparseable and preserves the parser exception as
         its cause when syntax or trailing content is invalid."
  [raw-text root-key root-type]
  (let [root-type? (or (get root-type-predicates root-type)
                     (throw (ex-info "Unknown structured response root type"
                              {:root-key root-key :root-type root-type})))
        response-length (count (str raw-text))]
    (try
      (when (str/blank? raw-text)
        (throw (ex-info "Structured response was blank" {})))
      (let [parsed (read-single-json-value raw-text)
            root-value (when (map? parsed) (get parsed root-key ::missing))]
        (when-not (map? parsed)
          (throw (ex-info "Structured response root must be a JSON object" {})))
        (when (= ::missing root-value)
          (throw (ex-info "Structured response field is missing" {})))
        (when-not (root-type? root-value)
          (throw (ex-info "Structured response field has the wrong type" {})))
        root-value)
      (catch Exception e
        (let [data {:type ::unparseable
                    :root-key root-key
                    :root-type root-type
                    :response-length response-length}]
          (tel/log! {:level :warn :id ::parse-response :data data}
            "The model returned invalid structured JSON")
          (throw (ex-info "The model returned invalid structured JSON" data e)))))))
