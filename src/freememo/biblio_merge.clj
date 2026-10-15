(ns freememo.biblio-merge
  "Merge a local-harvested CSL map with a resolver-fetched CSL map.
   Resolver wins per-field when its value is non-blank; otherwise local stands."
  (:require [clojure.string :as str]
            [clojure.walk :as walk]))

(defn- non-blank? [v]
  (cond
    (nil? v) false
    (string? v) (not (str/blank? v))
    (coll? v) (boolean (seq v))
    :else true))

(defn- strip-control-chars
  "Every string in `csl` with its C0 control characters removed, keeping tab,
   newline and carriage return. A PDF info dictionary can pad a UTF-16 title
   with NUL, and PostgreSQL rejects NUL in both TEXT and JSONB."
  [csl]
  (walk/postwalk #(if (string? %) (str/replace % #"[\x00-\x08\x0B\x0C\x0E-\x1F]" "") %)
    csl))

(defn merge-biblio
  "Pre:  {:local <csl> :resolved <csl>} — either may be nil or empty.
   Post: a single CSL-shaped map. For each key in :resolved with a non-blank
   value, the resolved value wins; otherwise the :local value stands. No
   string in it holds a C0 control character other than tab, newline and
   carriage return, so its title, its CSL and the citation rendered from it
   can all be stored.
   Invariant: never throws."
  [{:keys [local resolved]}]
  (let [base (or local {})
        top  (or resolved {})]
    (strip-control-chars
      (reduce-kv (fn [acc k v]
                   (if (non-blank? v) (assoc acc k v) acc))
                 base
                 top))))
