(ns freememo.html-cleaner
  "HTML sanitization using Jsoup. Strips scripts, iframes, event handlers,
   and unsafe attributes. Allows `style` only for `background-color: <safe-value>`
   to preserve auto-extract highlights. Allows `class` only for allow-listed
   Quill format tokens (code-block, align, indent, size, syntax, ui) and the
   Quill `data-language` / `data-list` / `data-row` attributes — so the extract
   path (and every other `clean-html` consumer) preserves Quill-rendered
   formatting on round-trip."
  (:require [clojure.string :as str])
  (:import [org.jsoup Jsoup]
           [org.jsoup.nodes Document$OutputSettings]
           [org.jsoup.safety Safelist]))

;; A "safe value" is a single token: hex literal, rgb()/rgba(), or a named colour.
;; Reject anything with parentheses we don't recognise (url(), expression(), etc.),
;; backslashes, semicolons inside the value, comments, or CSS escapes.
(def ^:private safe-color-re
  #"(?xi)
    ^\s*
    (?:
       \#[0-9a-f]{3,8}
       |
       rgb\(\s*\d{1,3}\s*,\s*\d{1,3}\s*,\s*\d{1,3}\s*\)
       |
       rgba\(\s*\d{1,3}\s*,\s*\d{1,3}\s*,\s*\d{1,3}\s*,\s*(?:0|1|0?\.\d+)\s*\)
       |
       [a-z]+
    )
    \s*$")

(defn- sanitize-style-value
  "Keep only `background-color: <safe-value>` declarations. Drop everything
   else. Returns the cleaned style string, or nil if nothing remained."
  [style]
  (when style
    (let [decls (->> (str/split style #";")
                  (map str/trim)
                  (remove str/blank?)
                  (keep (fn [decl]
                          (let [[prop val] (str/split decl #":" 2)]
                            (when (and prop val
                                    (= "background-color" (str/lower-case (str/trim prop)))
                                    (re-matches safe-color-re val))
                              (str "background-color: " (str/trim val)))))))]
      (when (seq decls)
        (str/join "; " decls)))))

(defn- post-filter-styles!
  "Mutate `doc` in place: rewrite or remove every `style` attribute."
  [^org.jsoup.nodes.Document doc]
  (doseq [^org.jsoup.nodes.Element el (.select doc "[style]")]
    (let [filtered (sanitize-style-value (.attr el "style"))]
      (if filtered
        (.attr el "style" filtered)
        (.removeAttr el "style"))))
  doc)

;; Allow-listed Quill format classes. `ql-cursor` is intentionally absent —
;; it marks transient editor state that must not persist.
(def ^:private quill-class-allow-list
  #{"ql-code-block-container" "ql-code-block"
    "ql-align-center" "ql-align-justify"
    "ql-indent-1" "ql-indent-2"
    "ql-size-small" "ql-size-large" "ql-size-huge"
    "ql-syntax" "ql-ui"})

(defn- sanitize-class-value
  "Keep only allow-listed Quill format tokens. Returns the cleaned class string,
   or nil if no allow-listed tokens remained."
  [class-str]
  (when class-str
    (let [tokens (->> (str/split class-str #"\s+")
                   (remove str/blank?)
                   (filter quill-class-allow-list))]
      (when (seq tokens)
        (str/join " " tokens)))))

(defn- inside-code-block-container?
  "True iff `el` is, or is a descendant of, a `.ql-code-block-container`.
   Inside the carve-out, class and data-* attributes pass through unfiltered —
   Quill's syntax module emits `<span class=\"hljs-*\">` and arbitrary
   `data-language` values that are not in the general allow-list."
  [^org.jsoup.nodes.Element el]
  (some? (.closest el ".ql-code-block-container")))

(defn- post-filter-classes!
  "Mutate `doc` in place: rewrite or remove every `class` attribute.
   Elements inside `.ql-code-block-container` are exempt — their class values
   pass through unchanged (syntax-highlighter hljs spans live here)."
  [^org.jsoup.nodes.Document doc]
  (doseq [^org.jsoup.nodes.Element el (.select doc "[class]")]
    (when-not (inside-code-block-container? el)
      (let [filtered (sanitize-class-value (.attr el "class"))]
        (if filtered
          (.attr el "class" filtered)
          (.removeAttr el "class")))))
  doc)

(def ^:private data-language-re #"^[a-zA-Z0-9_-]+$")
(def ^:private data-row-re #"^[a-zA-Z0-9_-]+$")
(def ^:private data-list-values #{"bullet" "ordered"})

(defn- post-filter-quill-data-attrs!
  "Mutate `doc` in place: drop Quill data-attribute values that fail validation.
   `data-language` and `data-row` must match a strict identifier regex;
   `data-list` must be \"bullet\" or \"ordered\".
   Elements inside `.ql-code-block-container` are exempt — their `data-language`
   values pass through unchanged."
  [^org.jsoup.nodes.Document doc]
  (doseq [^org.jsoup.nodes.Element el (.select doc "[data-language]")]
    (when-not (inside-code-block-container? el)
      (when-not (re-matches data-language-re (.attr el "data-language"))
        (.removeAttr el "data-language"))))
  (doseq [^org.jsoup.nodes.Element el (.select doc "[data-list]")]
    (when-not (inside-code-block-container? el)
      (when-not (contains? data-list-values (.attr el "data-list"))
        (.removeAttr el "data-list"))))
  (doseq [^org.jsoup.nodes.Element el (.select doc "[data-row]")]
    (when-not (inside-code-block-container? el)
      (when-not (re-matches data-row-re (.attr el "data-row"))
        (.removeAttr el "data-row"))))
  doc)

(defn clean-html
  "Sanitize HTML using a whitelist of safe tags and attributes.
   Strips scripts, iframes, event handlers, and most attributes.
   Preserves only `style=\"background-color: <safe>\"` (used by extract highlights)."
  [html]
  (when html
    (let [;; Pre-strip `<select>...</select>` (including content) before
          ;; Jsoup. Jsoup's safelist removes the wrapper but keeps the
          ;; <option> text as siblings, corrupting card and extract HTML
          ;; with the concatenated language-picker labels Quill 2's
          ;; syntax module renders into `.ql-code-block-container`.
          pre-stripped (str/replace html #"(?is)<select\b[^>]*>.*?</select>" "")
          styled-tags (into-array String ["span" "div" "p" "h1" "h2" "h3" "h4" "h5" "h6"
                                          "li" "td" "th" "blockquote"])
          ;; Tags Quill applies format classes to. Superset of styled-tags +
          ;; <pre> (for legacy Quill 1.x `ql-syntax` blocks).
          class-tags (into-array String ["span" "div" "p" "h1" "h2" "h3" "h4" "h5" "h6"
                                         "li" "td" "th" "blockquote" "pre"])
          safelist (-> (Safelist/relaxed)
                     (.addTags (into-array String ["h1" "h2" "h3" "h4" "h5" "h6"
                                                   "p" "br" "hr"
                                                   "ul" "ol" "li"
                                                   "blockquote" "pre" "code"
                                                   "table" "thead" "tbody" "tr" "td" "th"
                                                   "a" "strong" "em" "b" "i" "u" "s"
                                                   "span" "div" "sub" "sup"
                                                   "img" "figure" "figcaption"
                                                   "dl" "dt" "dd"]))
                     (.addAttributes "a" (into-array String ["href" "title"]))
                     (.addAttributes "img" (into-array String ["src" "alt" "width" "height"]))
                     (.addAttributes "td" (into-array String ["colspan" "rowspan" "data-row"]))
                     (.addAttributes "th" (into-array String ["colspan" "rowspan"]))
                     (.addAttributes "tr" (into-array String ["data-row"]))
                     (.addAttributes "div" (into-array String ["data-language"]))
                     (.addAttributes "li" (into-array String ["data-list"]))
                     (.addProtocols "a" "href" (into-array String ["http" "https" "mailto"]))
                     (.addProtocols "img" "src" (into-array String ["http" "https" "data"]))
                     ;; Keep relative URLs (e.g. /api/media/<id>) intact —
                     ;; without this, Jsoup strips relative srcs because they
                     ;; don't match any allowed protocol.
                     (.preserveRelativeLinks true))]
      (doseq [tag styled-tags]
        (.addAttributes safelist tag (into-array String ["style"])))
      (doseq [tag class-tags]
        (.addAttributes safelist tag (into-array String ["class"])))
      ;; Pass a baseUri so Jsoup can resolve relative URLs (e.g. /api/media/<id>)
      ;; against the http allow-list; combined with preserveRelativeLinks(true),
      ;; the output keeps them as relative paths instead of stripping them.
      ;; Disable Jsoup's pretty-print on BOTH steps. Default formatting
      ;; injects newlines and indentation between sibling elements; when
      ;; the cleaned HTML is re-parsed by `parseBodyFragment`, that
      ;; whitespace becomes text nodes in the tree, then collapses into
      ;; the adjacent `.ql-code-block` line on the next Quill load,
      ;; merging multi-line code blocks into a single line. Compact
      ;; output preserves the semantic line structure unchanged for
      ;; every other element.
      (let [compact-settings (doto (Document$OutputSettings.) (.prettyPrint false))
            cleaned (Jsoup/clean pre-stripped "http://localhost" safelist compact-settings)
            doc (Jsoup/parseBodyFragment cleaned)]
        (.prettyPrint (.outputSettings doc) false)
        (post-filter-styles! doc)
        (post-filter-classes! doc)
        (post-filter-quill-data-attrs! doc)
        (.html (.body doc))))))

(defn strip-ql-tokens
  "Unwrap every `<span class=\"ql-token …\">` element, preserving its children.

   Quill 2.0.3's `clipboard.convert` reads such a span as
   `code-token: true` (boolean) instead of the actual `hljs-X` value, which
   then renders as `class=\"hljs-true\"` and collapses adjacent same-value
   inlines into a single span covering the whole code line.

   Saved topic content MUST NOT carry these wrappers; the Quill `syntax`
   module re-applies them in the browser via its 1 s debounced timer.

   Pre  : `html` is a string or nil.
   Post : returns nil iff `html` is nil; otherwise the same HTML with every
          `span.ql-token` unwrapped (children move up into the span's parent).
   Inv  : text content and all non-`ql-token` markup are unchanged."
  [html]
  (when html
    (let [doc (Jsoup/parseBodyFragment html)]
      (.prettyPrint (.outputSettings doc) false)
      (doseq [^org.jsoup.nodes.Element el (.select doc "span.ql-token")]
        (.unwrap el))
      (.html (.body doc)))))

(defn clean-html-llm
  "Sanitize HTML from LLM output. Same allow-list as `clean-html`, then strips
   every `<img>` tag — pins are the sole source of images in cards (LL3-yes).
   Preserves cloze syntax `{{cN::text}}` in text nodes."
  [html]
  (when html
    (let [cleaned (clean-html html)
          doc (Jsoup/parseBodyFragment cleaned)]
      (doseq [^org.jsoup.nodes.Element img (.select doc "img")]
        (.remove img))
      (.html (.body doc)))))
