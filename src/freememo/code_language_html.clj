(ns freememo.code-language-html
  "Set the syntax language of every code block in a piece of stored HTML.

   NO DATABASE DEPENDENCY, deliberately. This namespace holds the whole
   transform, so `test/freememo/code_language_html_test.clj` runs against it
   with nothing started. The database work lives in
   `freememo.apply-code-language`.

   WHERE THE LANGUAGE LIVES. Quill 2.0.3 writes `data-language` on each
   `div.ql-code-block` — one per LINE, not one per block — and that per-line
   attribute is what the clipboard matcher in `freememo.quill-field` reads back
   on load (`quill_field.cljc`, the `div.ql-code-block` matcher). The enclosing
   `.ql-code-block-container` may carry the attribute too.

   So the two elements are treated differently, and the difference is not
   cosmetic. On a `div.ql-code-block` the attribute is SET whether or not it was
   there: that element defines the line's language, and a line missing the
   attribute reads as \"plain\", which is exactly the case a user runs this
   command to repair. On the container the attribute is only UPDATED when it is
   already present, because adding one Quill did not write would change the
   stored shape for no gain.

   `clean-html` keeps `data-language` on `div` and validates it against
   `^[a-zA-Z0-9_-]+$` (`html_cleaner.clj`), so every key in
   `freememo.code-languages` survives a later sanitize unchanged."
  (:require [freememo.code-languages :as langs])
  (:import [org.jsoup Jsoup]
           [org.jsoup.nodes Document$OutputSettings]))

(def ^:private line-selector "div.ql-code-block")
(def ^:private container-selector ".ql-code-block-container")

(defn- parse-fragment
  "Jsoup document for a stored HTML fragment, with pretty-printing off.
   Same shape every other fragment rewriter here uses — pretty printing
   reflows whitespace, and whitespace inside a code block is content."
  [html]
  (let [doc (Jsoup/parseBodyFragment html)]
    (.prettyPrint (.outputSettings doc) false)
    doc))

(defn code-block-count
  "How many code blocks `html` holds.

   Pre : `html` is a string or nil.
   Post: 0 for nil, for the empty string, and for HTML with no code block.
         A block is one `.ql-code-block-container`. A `div.ql-code-block` with
         no container ancestor counts as its own block — Quill always wraps, so
         this only catches HTML that reached storage another way."
  [html]
  (if (nil? html)
    0
    (let [doc (parse-fragment html)
          containers (.select doc container-selector)
          orphans (->> (.select doc line-selector)
                       (filter #(nil? (.closest % container-selector)))
                       count)]
      (+ (.size containers) orphans))))

(defn set-code-language
  "Return `html` with every code block's language set to `language`.

   Pre : `html` is a string or nil. `language` ∈
         `freememo.code-languages/code-language-keys` — ASSERTED, because an
         unchecked value reaches a `data-language` attribute and a stored
         document. A caller that fails this is the bug.
   Post: nil iff `html` is nil. Otherwise every `div.ql-code-block` carries
         `data-language=language`, every `.ql-code-block-container` that
         ALREADY carried the attribute carries the new value, and nothing else
         in the fragment differs — no element added or removed, no text changed.
   Inv : HTML with no code block comes back with the same content it went in
         with. It is re-serialized by Jsoup, so it is not guaranteed
         byte-identical, and callers compare for change rather than assuming."
  [html language]
  (assert (contains? langs/code-language-keys language)
    (str "set-code-language got an unknown language: " (pr-str language)))
  (when html
    (let [doc (parse-fragment html)]
      (doseq [el (.select doc line-selector)]
        (.attr el "data-language" language))
      (doseq [el (.select doc container-selector)]
        (when (.hasAttr el "data-language")
          (.attr el "data-language" language)))
      (.html (.body doc)))))

(defn relanguage-field
  "`set-code-language` for one card field, reporting whether it changed.

   Pre : as `set-code-language`.
   Post: `nil` when `html` is nil, holds no code block, or already reads
         `language` throughout. Otherwise the rewritten HTML.
         A nil return means the caller MUST NOT write this field — that is what
         keeps an unchanged card out of `card_versions`."
  [html language]
  (when (and html (pos? (code-block-count html)))
    (let [out (set-code-language html language)]
      (when (not= out html) out))))
