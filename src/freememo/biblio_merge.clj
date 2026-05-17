(ns freememo.biblio-merge
  "Merge a local-harvested CSL map with a resolver-fetched CSL map.
   Resolver wins per-field when its value is non-blank; otherwise local stands."
  (:require [clojure.string :as str]))

(defn- non-blank? [v]
  (cond
    (nil? v) false
    (string? v) (not (str/blank? v))
    (coll? v) (boolean (seq v))
    :else true))

(defn merge-biblio
  "Pre:  {:local <csl> :resolved <csl>} — either may be nil or empty.
   Post: a single CSL-shaped map. For each key in :resolved with a non-blank
   value, the resolved value wins; otherwise the :local value stands.
   Invariant: never throws."
  [{:keys [local resolved]}]
  (let [base (or local {})
        top  (or resolved {})]
    (reduce-kv (fn [acc k v]
                 (if (non-blank? v) (assoc acc k v) acc))
               base
               top)))
