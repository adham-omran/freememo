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

;; Pixel-width drag for side panels (PointerEvent API — mouse, touch, stylus).
;; Distinct from start-drag! (which is %-of-parent for split panes): here the
;; panel has a fixed pixel width, so the helper drives a px atom directly and
;; never reads parent size — `min`/`max` are resolved by the caller.
(defn start-drag-px!
  "Begin a panel-width drag. Call from a pointerdown handler.
   Resets `!width-px` live during the drag, clamped to [min max].
   `invert?` true when the handle is on the panel's left edge (pins): dragging
   right shrinks the panel. `on-commit` (optional) is called with the final
   width on pointerup, for persistence."
  [e !width-px {:keys [min max invert? on-commit]}]
  #?(:clj nil
     :cljs
     (do
       (.preventDefault e)
       (let [target (.-currentTarget e)
             start-pos (.-clientX e)
             start-w @!width-px
             on-move (fn [me]
                       (let [delta (- (.-clientX me) start-pos)
                             signed (if invert? (- delta) delta)
                             new-w (-> (+ start-w signed)
                                     (cljs.core/max min)
                                     (cljs.core/min max))]
                         (reset! !width-px new-w)))
             on-up (fn self [ue]
                     (.releasePointerCapture target (.-pointerId ue))
                     (.removeEventListener target "pointermove" on-move)
                     (.removeEventListener target "pointerup" self)
                     (when on-commit (on-commit @!width-px)))]
         (.setPointerCapture target (.-pointerId e))
         (.addEventListener target "pointermove" on-move)
         (.addEventListener target "pointerup" on-up)))))

(defn panel-resize-max
  "Max px this side panel may grow to without shrinking the flex content
   column below `content-floor`. The only reclaimable space is the content
   column's width above the floor; the opposite panel is flex-shrink:0 and
   never yields. `handle-node` is the resize handle (its parent is the panel
   root). `content-side` is :after (content is the next sibling, left/hierarchy
   panel) or :before (previous sibling, right/pins panel)."
  [handle-node content-side content-floor]
  #?(:clj content-floor
     :cljs
     (let [panel (.-parentElement handle-node)
           content (case content-side
                     :after (.-nextElementSibling panel)
                     :before (.-previousElementSibling panel))
           panel-w (.-offsetWidth panel)
           content-w (if content (.-offsetWidth content) 0)]
       (cljs.core/max panel-w (- (+ panel-w content-w) content-floor)))))

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

(defn format-date-short [ts]
  #?(:clj (when ts
            (let [inst (.toInstant ts)
                  ldt (LocalDateTime/ofInstant inst (ZoneId/systemDefault))]
              (.format ldt (DateTimeFormatter/ofPattern "MMM d"))))
     :cljs nil))
