(ns freememo.article-split-identity
  "Content versions and stable section identities for article splitting."
  (:require [cheshire.core :as json])
  (:import [java.security MessageDigest]
           [java.nio.charset StandardCharsets]
           [java.math BigInteger]))

(defn read-provenance
  "Decode private split metadata from storage or an archive. Nil stays nil."
  [stored]
  (if (string? stored) (json/parse-string stored true) stored))

(defn source-version
  "Return the SHA256 version of stored article HTML. Nil means empty HTML."
  [html]
  (format "%064x" (BigInteger. 1 (.digest (MessageDigest/getInstance "SHA-256")
                                 (.getBytes ^String (or html "") StandardCharsets/UTF_8)))))

(defn provenance
  "Pre: options are normalized. Post: JSON-safe identity, independent of copied content."
  [version {:keys [mode splitmark]} index]
  (cond-> {:source-version version :mode (name mode) :index index}
    (= :splitmark mode) (assoc :splitmark splitmark)))

(defn section-key
  "Return a bounded key for persisted provenance. JSON decoding preserves this key."
  [{:keys [mode splitmark index] :as identity}]
  (source-version (pr-str [(:source-version identity) (keyword mode) splitmark index])))

(defn rebase-source-version
  "Pre: only reference IDs changed between the two versions.
   Post: current provenance uses the new version. Historical provenance stays unchanged."
  [identity old-version new-version]
  (if (and identity (= old-version (:source-version identity)))
    (assoc identity :source-version new-version)
    identity))
