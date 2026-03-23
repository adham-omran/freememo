(ns electric-starter-app.util
  "Shared utility functions for display formatting."
  (:require [clojure.string :as str])
  #?(:clj (:import [java.time LocalDateTime ZoneId]
                    [java.time.format DateTimeFormatter])))

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

(defn format-bytes [n]
  #?(:clj (cond
            (nil? n) "0 B"
            (< n 1024) (str n " B")
            (< n (* 1024 1024)) (format "%.1f KB" (/ (double n) 1024))
            :else (format "%.1f MB" (/ (double n) (* 1024 1024))))
     :cljs nil))

(defn format-timestamp [ts]
  #?(:clj (when ts
            (let [inst (.toInstant ts)
                  ldt (LocalDateTime/ofInstant inst (ZoneId/systemDefault))]
              (.format ldt (DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm"))))
     :cljs nil))
