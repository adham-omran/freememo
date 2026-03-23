(ns freememo.util
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

(defn mac-platform? []
  #?(:cljs (boolean (re-find #"(?i)mac" (str (.-platform js/navigator))))
     :clj false))

;; Drag helper for split-pane dividers (PointerEvent API — works for mouse, touch, and stylus)
(defn start-drag!
  "Begin a split-pane drag. Call from a pointerdown handler.
   axis: :x for horizontal, :y for vertical."
  [e axis !pct]
  #?(:clj nil
     :cljs
     (do
       (.preventDefault e)
       (let [target (.-currentTarget e)
             horizontal? (= axis :x)
             start-pos (if horizontal? (.-clientX e) (.-clientY e))
             start-pct @!pct
             parent (.-parentElement target)
             parent-size (if horizontal? (.-offsetWidth parent) (.-offsetHeight parent))
             on-move (fn [me]
                       (let [delta (- (if horizontal? (.-clientX me) (.-clientY me)) start-pos)
                             delta-pct (* (/ delta parent-size) 100)
                             new-pct (-> (+ start-pct delta-pct) (max 15) (min 85))]
                         (reset! !pct new-pct)))
             on-up (fn self [ue]
                     (.releasePointerCapture target (.-pointerId ue))
                     (.removeEventListener target "pointermove" on-move)
                     (.removeEventListener target "pointerup" self))]
         (.setPointerCapture target (.-pointerId e))
         (.addEventListener target "pointermove" on-move)
         (.addEventListener target "pointerup" on-up)))))

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
