(ns freememo.text
  "Plain-text extraction for indexing (search) and analysis."
  (:require [clojure.string :as str])
  (:import [org.jsoup Jsoup]))

(defn strip-html
  "Convert HTML to plain text via Jsoup. Returns empty string for nil/blank input.
   Decodes entities and collapses whitespace."
  [html]
  (if (or (nil? html) (and (string? html) (str/blank? html)))
    ""
    (-> (Jsoup/parseBodyFragment html) .body .text)))

(defn escape-html
  "Escape the four HTML-significant characters."
  [s]
  (-> s
    (str/replace "&" "&amp;")
    (str/replace "<" "&lt;")
    (str/replace ">" "&gt;")
    (str/replace "\"" "&quot;")))

(defn text->paragraph-html
  "Convert plain text (e.g., from PDFBox or PDF.js) into paragraph HTML.
   Splits on blank lines, escapes HTML, wraps each non-empty block in <p>.
   Returns an empty string when no non-blank blocks remain."
  [text]
  (if (or (nil? text) (str/blank? text))
    ""
    (let [normalized (-> text
                       (str/replace "\r\n" "\n")
                       (str/replace "\r" "\n"))
          blocks (->> (str/split normalized #"\n[ \t]*\n+")
                   (map str/trim)
                   (remove str/blank?))]
      (->> blocks
        (map (fn [block]
               (str "<p>" (escape-html block) "</p>")))
        (str/join "\n")))))
