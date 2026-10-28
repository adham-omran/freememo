(ns freememo.kg-predicate
  "Stable ASCII identifiers for Unicode predicate labels. No database dependency."
  (:require [clojure.string :as str])
  (:import [java.math BigInteger]
           [java.nio.charset StandardCharsets]
           [java.security MessageDigest]
           [java.text Normalizer Normalizer$Form]
           [java.util Locale]))

(defn canonical-label
  "Normalize case, Unicode composition, and separators for predicate identity.
   Preserve letters, numbers, and combining marks from every script.
   Return nil when the label has no letters or numbers."
  [label]
  (let [s (-> (str label)
              (.toLowerCase Locale/ROOT)
              (Normalizer/normalize Normalizer$Form/NFC)
              (str/replace #"[^\p{L}\p{N}\p{M}]+" "-")
              (str/replace #"^-+|-+$" ""))]
    (when (re-find #"[\p{L}\p{N}]" s) s)))

(defn legacy-slug
  "Return the former ASCII-only slug to find predicates stored before Unicode support."
  [label]
  (let [s (-> (str label)
              str/lower-case
              (str/replace #"[^a-z0-9]+" "-")
              (str/replace #"^-+|-+$" ""))]
    (when-not (str/blank? s) s)))

(defn- sha256 [s]
  (format "%064x"
    (BigInteger. 1 (.digest (MessageDigest/getInstance "SHA-256")
                    (.getBytes ^String s StandardCharsets/UTF_8)))))

(defn slugify
  "Keep ordinary ASCII slugs and encode complete Unicode labels with SHA-256.
   Encode ASCII labels that resemble generated identifiers in a separate namespace.
   Return nil when the label has no letters or numbers."
  [label]
  (when-let [canonical (canonical-label label)]
    (if (re-matches #"[a-z0-9]+(?:-[a-z0-9]+)*" canonical)
      (if (re-matches #"u-(?:ascii-|c-)?[a-f0-9]{64}" canonical)
        (str "u-ascii-" (sha256 canonical))
        canonical)
      (str "u-" (sha256 canonical)))))

(defn collision-slug
  "Derive another stable candidate when a stored slug belongs to a different label."
  [label attempt]
  (str "u-c-" (sha256 (str (canonical-label label) "\u0000" attempt))))
