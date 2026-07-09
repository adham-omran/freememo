(ns freememo.markdown
  "Markdown → HTML conversion using flexmark-java with GFM extensions."
  (:require [freememo.html-cleaner :as cleaner]
            [clojure.string :as str])
  (:import [com.vladsch.flexmark.parser Parser]
           [com.vladsch.flexmark.html HtmlRenderer]
           [com.vladsch.flexmark.util.data MutableDataSet]
           [com.vladsch.flexmark.ext.tables TablesExtension]
           [com.vladsch.flexmark.ext.gfm.strikethrough StrikethroughExtension]
           [com.vladsch.flexmark.ext.gfm.tasklist TaskListExtension]))

(defonce ^:private options
  (doto (MutableDataSet.)
    (.set Parser/EXTENSIONS [(TablesExtension/create)
                             (StrikethroughExtension/create)
                             (TaskListExtension/create)])))

(defonce ^:private parser (.build (Parser/builder options)))
(defonce ^:private renderer (.build (HtmlRenderer/builder options)))

;; A "plain number" is a bare quantity — digits with optional sign, thousands
;; separators / decimal point, and a trailing percent. NOT math: no letter,
;; operator, exponent (^), subscript (_), or LaTeX command (\).
(def ^:private plain-number-re #"^[+-]?\d[\d.,]*%?$")

(defn- plain-number? [s]
  (boolean (re-matches plain-number-re (str/trim s))))

(defn unwrap-non-math-dollars
  "Strip `$…$` / `$$…$$` math delimiters that merely wrap a plain number, so a
   writer's habit of dollar-wrapping bare numbers doesn't render them as italic
   math (where e.g. `$6,245$` mis-spaces to \"6, 245\"). Real math — anything
   containing a letter, exponent, subscript, operator, or LaTeX command — is
   left wrapped and untouched.
   Pre: `s` is a Markdown string or nil. Post: nil in → nil out; otherwise the
   same string with plain-number math spans replaced by their bare text."
  [s]
  (when s
    (let [unwrap (fn [[whole inner]] (if (plain-number? inner) (str/trim inner) whole))]
      (-> s
        (str/replace #"\$\$([^$\n]+?)\$\$" unwrap)   ; display first
        (str/replace #"\$([^$\n]+?)\$" unwrap)))))    ; then inline

(defn parse-markdown
  "Convert a Markdown string to sanitized HTML.
   Strips <img> tags. Converts task list checkboxes to text markers.
   Converts <del> to <s> for clean-html compatibility. Returns cleaned HTML string."
  [markdown-string]
  (when (seq markdown-string)
    (let [doc (.parse parser markdown-string)
          raw-html (.render renderer doc)
          ;; Convert <del> to <s> (clean-html safelist allows <s> but not <del>)
          html (str/replace raw-html #"<(/?)del>" "<$1s>")
          ;; Convert task list checkboxes to text markers (strip trailing &nbsp;)
          html (str/replace html #"<input[^>]*checked[^>]*/?>(&nbsp;|\s)*" "[x] ")
          html (str/replace html #"<input[^>]*type=\"checkbox\"[^>]*/?>(&nbsp;|\s)*" "[ ] ")
          ;; Strip remaining <img> tags (self-closing and regular)
          html (str/replace html #"<img[^>]*/?>" "")]
      (cleaner/clean-html html))))

(defn extract-frontmatter-title
  "Extract title from YAML frontmatter block if present. Returns string or nil."
  [markdown-string]
  (when (and (seq markdown-string) (str/starts-with? (str/trim markdown-string) "---"))
    (let [trimmed (str/trim markdown-string)
          ;; Find closing --- (must be on its own line after the opening ---)
          end-idx (str/index-of trimmed "\n---" 3)]
      (when end-idx
        (let [frontmatter (subs trimmed 3 end-idx)]
          (when-let [m (re-find #"(?m)^title:\s*(.+)$" frontmatter)]
            (str/trim (second m))))))))
