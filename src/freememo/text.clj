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
