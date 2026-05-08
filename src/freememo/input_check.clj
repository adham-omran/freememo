(ns freememo.input-check
  "Length caps on user-supplied strings. Reject (don't truncate)."
  (:require [clojure.string :as str]))

(def title-max 500)
(def card-max 10000)
(def prompt-max 5000)

(defn check-length!
  "Throw ex-info ::length-exceeded when s exceeds max-chars.
   nil and blank pass. field is a keyword used in the error message."
  [field s max-chars]
  (when (and s (> (count s) max-chars))
    (throw (ex-info (str (name field) " exceeds " max-chars " character limit")
             {:type ::length-exceeded
              :field field
              :max max-chars
              :actual (count s)}))))

(defn length-error?
  "Predicate for catching :type ::length-exceeded ex-data."
  [data]
  (= ::length-exceeded (:type data)))

(defn sanitize-filename
  "Strip control chars (\\x00-\\x1F, \\x7F) and path separators (/, \\)
   from a user-supplied title. Trim surrounding whitespace.
   Inner whitespace is preserved.
   Returns \"Untitled\" when input is nil, blank, or fully stripped."
  [s]
  (if (nil? s)
    "Untitled"
    (let [stripped (-> s
                     (str/replace #"[\x00-\x1F\x7F/\\]" "")
                     str/trim)]
      (if (str/blank? stripped) "Untitled" stripped))))

(defn prettify-title
  "Canonicalize an import-time title for display:
   - Replace underscores with spaces.
   - Strip trailing .pdf (case-insensitive).
   - Collapse whitespace runs and trim.
   Idempotent. nil/blank returns the input unchanged so callers can guard."
  [s]
  (if (or (nil? s) (str/blank? s))
    s
    (-> s
      (str/replace #"_" " ")
      (str/replace #"(?i)\.pdf$" "")
      (str/replace #"\s+" " ")
      str/trim)))
