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
