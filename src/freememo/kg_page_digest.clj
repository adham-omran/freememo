(ns freememo.kg-page-digest
  "Content digest for the facts-extraction page skip.

   THE contract: a page's digest is only ever compared with an EARLIER digest of
   that same page. It never crosses pages, never crosses documents, and answers
   exactly one question — \"is this page's text the text we already extracted
   from?\". `freememo.kg-extract` re-extracts a page when the answer is no.

   The digest covers the page text and NOTHING else. Prompt version, extraction
   model and the user's predicate vocabulary are recorded on the extraction row
   and never compared, because the vocabulary grows on every curation and gating
   on it would make the skip almost never fire. Reaching already-extracted pages
   after a prompt or model change is a deliberate user action, not a side effect
   of an edit — see plans/kg-page-skip-extraction.md §3.

   This namespace MUST NOT require `freememo.db`: db.clj requires it, the same
   way it requires `freememo.anki-digest` (db.clj:13). Keep it a leaf.

   pre : `content-text` is a string or nil.
   post: `digest-of` returns a 64-character lowercase hex string, or nil for nil
         and blank input.
   inv : equal input gives equal output across restarts — no hash seed, no map
         iteration order and no timestamp enters the input."
  (:require
   [clojure.string :as str])
  (:import [java.security MessageDigest]))

(def digest-version
  "Which generation of `digest-of` a stored `kg_page_extractions.content_digest`
   came from.

   BUMP THIS whenever `digest-of` changes what it feeds the hash — a normalizing
   step, a different source column, anything at all. A change to the function
   moves every page's digest without any page changing, and the selector cannot
   tell that from an edit on every page.

   `db/pages-needing-extraction` treats a row whose stored version differs from
   this constant as needing extraction, so a bump re-extracts the affected
   documents on their next run instead of skipping them wrongly. That costs
   money, which is why the bump belongs in the same commit as the change and
   nowhere else.

   Version 1 is the first generation: the raw `topics.content_text`, SHA-256, no
   normalization. The column is NOT NULL and the table shipped with it, so unlike
   the Anki baselines there is no pre-constant generation to recognise.

   Precedent for the constant: `anki-digest/digest-version`, whose docstring
   records what shipping a digest change without one cost."
  1)

(defn digest-of
  "SHA-256 hex of a page's extraction text.

   Verbatim, with no normalization, so the digest is exactly \"the bytes the
   model saw\". A page reformatted without changing a character of prose is
   therefore a different page, which is the conservative direction: it costs one
   re-extraction rather than silently skipping changed content.

   pre : `content-text` is a string or nil.
   post: 64-character lowercase hex, or nil when there is no text to extract
         from — nil and blank both mean \"this page contributes nothing\", and a
         page with no text is never selected in the first place."
  [content-text]
  (when-not (str/blank? content-text)
    (let [md (MessageDigest/getInstance "SHA-256")
          bs (.digest md (.getBytes ^String content-text "UTF-8"))]
      (str/join (map #(format "%02x" (bit-and % 0xff)) bs)))))
