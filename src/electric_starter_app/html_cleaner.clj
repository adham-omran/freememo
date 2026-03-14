(ns electric-starter-app.html-cleaner
  "HTML sanitization using Jsoup. Strips scripts, styles, and unsafe content."
  (:import [org.jsoup Jsoup]
           [org.jsoup.safety Safelist]))

(defn clean-html
  "Sanitize HTML using a whitelist of safe tags and attributes.
   Strips scripts, styles, iframes, event handlers, etc."
  [html]
  (when html
    (let [safelist (-> (Safelist/relaxed)
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
      (Jsoup/clean html safelist))))
