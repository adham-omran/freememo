(ns freememo.llm-edn
  "Parse an LLM chat completion whose prompt demanded EDN. Extracted from
   freememo.cards/parse-card-response so the KG pipeline shares one parser."
  (:require
   [taoensso.telemere :as tel]
   [cheshire.core :as json]
   [clojure.edn :as edn]
   [clojure.string :as str]))

(defn- parse-edn-or-json
  "Try EDN, then JSON with keywordized keys. Returns the parsed value only when
   it is a collection; every caller expects a vector or map, and edn/read-string
   reads just the first datum, so on prose it yields a bare leading symbol that
   must be rejected (falls through to bracket extraction). nil otherwise."
  [s]
  (let [v (try
            (edn/read-string s)
            (catch Exception _
              (try (json/parse-string s true)
                   (catch Exception _ nil))))]
    (when (coll? v) v)))

(defn- extract-bracketed
  "Return the substring from the earliest opener ([ or {) to the last matching
   closer (] or }), discarding any surrounding wrapper or prose the model added
   (observed: single backticks). nil if no such span exists. Does not guarantee
   the span is well-formed — the caller's parser is the real gate."
  [s]
  (let [bracket-idx (str/index-of s "[")
        brace-idx   (str/index-of s "{")
        [open-idx close-char]
        (cond
          (and bracket-idx brace-idx) (if (< bracket-idx brace-idx)
                                        [bracket-idx \]] [brace-idx \}])
          bracket-idx [bracket-idx \]]
          brace-idx   [brace-idx \}]
          :else       nil)]
    (when open-idx
      (when-let [close-idx (str/last-index-of s close-char)]
        (when (> close-idx open-idx)
          (subs s open-idx (inc close-idx)))))))

(defn parse-response
  "Parse a model message into Clojure data. Prefers EDN; falls back to JSON
   with keywordized keys for models that emit JSON despite the instructions
   (observed: Gemini 3 Flash), so the keyword-keyed shape is identical.
   Strips markdown code fences first (```clojure/edn/json); if the cleaned
   string still won't parse, retries on the bracketed payload to survive other
   wrappers/prose (observed: single-backtick-wrapped EDN).
   Pre:  raw-text is the model message content (may be nil).
   Post: returns the parsed collection (vector or map); throws ex-info when no
         collection can be recovered."
  [raw-text]
  (let [cleaned (-> (str raw-text)
                  str/trim
                  (str/replace #"^```(?:clojure|edn|json)?\s*\n?" "")
                  (str/replace #"\n?```\s*$" "")
                  str/trim)]
    (or (parse-edn-or-json cleaned)
        (some-> (extract-bracketed cleaned) parse-edn-or-json)
        (do (tel/error! {:id ::parse-response
                         :data {:raw raw-text :cleaned cleaned}}
              "Failed to parse model response (neither EDN nor JSON)")
            (throw (ex-info "Failed to parse model response"
                     {:raw raw-text :cleaned cleaned}))))))
