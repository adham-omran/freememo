(ns electric-starter-app.util
  "Shared utility functions for display formatting."
  (:require [clojure.string :as str]))

(defn strip-html-tags
  "Remove HTML tags from a string, collapse whitespace, and trim.
   Returns empty string for nil/non-string input."
  [html]
  (if (string? html)
    (-> html
        (str/replace #"<[^>]*>" " ")
        (str/replace #"&[a-zA-Z]+;" " ")
        (str/replace #"&#\d+;" " ")
        (str/replace #"\s+" " ")
        str/trim)
    ""))

(defn display-name
  "Clean a document filename for display:
   - Replace underscores with spaces
   - Strip .pdf extension
   Returns empty string for nil input."
  [filename]
  (if (string? filename)
    (-> filename
        (str/replace #"_" " ")
        (str/replace #"(?i)\.pdf$" "")
        str/trim)
    ""))

(defn truncate
  "Truncate string to n chars, adding ellipsis if longer."
  [s n]
  (if (and (string? s) (> (count s) n))
    (str (subs s 0 n) "...")
    (or s "")))

(defn extract-preview
  "Generate a clean text preview from HTML content.
   Strips tags, then truncates to n chars (default 80)."
  ([html] (extract-preview html 80))
  ([html n]
   (truncate (strip-html-tags html) n)))
