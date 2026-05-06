(ns freememo.html-cleaner
  "HTML sanitization using Jsoup. Strips scripts, iframes, event handlers,
   and unsafe attributes. Allows `style` only for `background-color: <safe-value>`
   to preserve auto-extract highlights."
  (:require [clojure.string :as str])
  (:import [org.jsoup Jsoup]
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

(defn clean-html
  "Sanitize HTML using a whitelist of safe tags and attributes.
   Strips scripts, iframes, event handlers, and most attributes.
   Preserves only `style=\"background-color: <safe>\"` (used by extract highlights)."
  [html]
  (when html
    (let [styled-tags (into-array String ["span" "div" "p" "h1" "h2" "h3" "h4" "h5" "h6"
                                          "li" "td" "th" "blockquote"])
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
                     (.addAttributes "td" (into-array String ["colspan" "rowspan"]))
                     (.addAttributes "th" (into-array String ["colspan" "rowspan"]))
                     (.addProtocols "a" "href" (into-array String ["http" "https" "mailto"]))
                     (.addProtocols "img" "src" (into-array String ["http" "https" "data"])))]
      (doseq [tag styled-tags]
        (.addAttributes safelist tag (into-array String ["style"])))
      (let [cleaned (Jsoup/clean html safelist)
            doc (Jsoup/parseBodyFragment cleaned)]
        (post-filter-styles! doc)
        (.html (.body doc))))))
