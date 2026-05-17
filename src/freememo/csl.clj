(ns freememo.csl
  "Pure CSL-JSON transformations used by import flows that fetch bibliographic
   data out-of-band (e.g. the Zotero plugin path).

   Salvaged from the deleted freememo.zotero ns: callers now run on the client
   side and POST the parsed CSL-JSON map to the server, but the CSL → web-biblio
   shape conversion still belongs server-side (single source of truth for what
   biblio-import/prepare-biblio! consumes)."
  (:require [clojure.string :as str]))

(defn- as-string [v]
  (cond
    (string? v) v
    (number? v) (str v)
    :else nil))

(defn extract-identifiers
  "Best-effort DOI / arXiv / ISBN / PMID extraction from a CSL map.
   Looks at typed CSL fields first, then regexes over string-valued fields
   to catch identifiers stashed in `note` or `archiveLocation`.

   Pre:  csl is a map (possibly empty).
   Post: a map with some subset of {:doi :arxiv :isbn :pmid}; empty when
         no identifiers can be extracted."
  [csl]
  (let [text  (->> (vals csl) (keep as-string) (str/join "\n"))
        doi   (or (:DOI csl)
                  (some-> (re-find #"(?i)\b10\.\d{4,9}/[^\s\"]+" text)))
        arxiv (some-> (re-find #"(?i)arXiv:\s*([0-9]{4}\.[0-9]{4,5}|[a-z\-]+/[0-9]{7})" text)
                      second)
        isbn  (or (:ISBN csl)
                  (some-> (re-find #"\b97[89][- ]?[0-9X][- ]?[0-9]{3}[- ]?[0-9]{5}[- ]?[0-9X]\b" text)
                          (str/replace #"[- ]" "")))
        pmid  (some-> (re-find #"(?i)PMID:?\s*([0-9]{1,9})" text) second)]
    (into {}
          (remove (fn [[_ v]] (or (nil? v) (str/blank? (str v))))
                  {:doi   (some-> doi str)
                   :arxiv (some-> arxiv str)
                   :isbn  (some-> isbn str)
                   :pmid  (some-> pmid str)}))))

(defn csljson->web-biblio
  "Translate a per-item CSL-JSON map to the {:local :identifiers} shape
   consumed by biblio-import/prepare-biblio!.

   Pre:  csljson is a map or nil.
   Post: {:local <csl-map> :identifiers <id-map>}.
   Invariant: nil or non-map input yields {:local {} :identifiers {}}."
  [csljson]
  (if (map? csljson)
    {:local csljson :identifiers (extract-identifiers csljson)}
    {:local {} :identifiers {}}))
