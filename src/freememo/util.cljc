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
   axis: :x for horizontal, :y for vertical.
   `on-commit` (optional) is called with the final pct on pointerup, for
   persistence — mirrors start-drag-px!."
  ([e axis !pct] (start-drag! e axis !pct nil))
  ([e axis !pct {:keys [on-commit]}]
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
                      (.removeEventListener target "pointerup" self)
                      (when on-commit (on-commit @!pct)))]
          (.setPointerCapture target (.-pointerId e))
          (.addEventListener target "pointermove" on-move)
          (.addEventListener target "pointerup" on-up))))))

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

;; Keyboard analogs of the two drag helpers (WCAG 2.1.1 — resize must work
;; without a pointer). Call from a keydown handler on the divider; both
;; return nil for non-resize keys so callers can fall through.

(defn key-resize-pct!
  "Step a split percentage by ±2 on the arrow keys matching `axis`
   (:x → Left/Right, :y → Up/Down), clamped to the same [15 85] as
   start-drag!. Calls `on-commit` (nilable) with the new pct."
  [e axis !pct on-commit]
  #?(:clj nil
     :cljs
     (let [delta (case [axis (.-key e)]
                   [:x "ArrowLeft"] -2 [:x "ArrowRight"] 2
                   [:y "ArrowUp"] -2 [:y "ArrowDown"] 2
                   nil)]
       (when delta
         (.preventDefault e)
         (let [nv (-> (+ @!pct delta) (cljs.core/max 15) (cljs.core/min 85))]
           (reset! !pct nv)
           (when on-commit (on-commit nv))
           nv)))))

(defn key-resize-px!
  "Step a panel pixel width by ±16 on ArrowLeft/ArrowRight, clamped to
   [min max] like start-drag-px!. `invert?` true when the handle is on the
   panel's left edge (arrows track the DIVIDER's motion, so ArrowRight
   shrinks an invert? panel). Calls `on-commit` (nilable) with the new width."
  [e !width-px {:keys [min max invert? on-commit]}]
  #?(:clj nil
     :cljs
     (let [dir (case (.-key e) "ArrowLeft" -16 "ArrowRight" 16 nil)]
       (when dir
         (.preventDefault e)
         (let [signed (if invert? (- dir) dir)
               nv (-> (+ (or @!width-px 0) signed)
                    (cljs.core/max min)
                    (cljs.core/min max))]
           (reset! !width-px nv)
           (when on-commit (on-commit nv))
           nv)))))

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

#?(:clj
   (defn format-bytes [n]
     (cond
       (nil? n) "0 B"
       (< n 1024) (str n " B")
       (< n (* 1024 1024)) (format "%.1f KB" (/ (double n) 1024))
       :else (format "%.1f MB" (/ (double n) (* 1024 1024))))))

#?(:clj
   (defn format-timestamp [ts]
     (when ts
       (let [inst (.toInstant ts)
             ldt (LocalDateTime/ofInstant inst (ZoneId/systemDefault))]
         (.format ldt (DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm"))))))

#?(:clj
   (defn format-date-short [ts]
     (when ts
       (let [inst (.toInstant ts)
             ldt (LocalDateTime/ofInstant inst (ZoneId/systemDefault))]
         (.format ldt (DateTimeFormatter/ofPattern "MMM d"))))))

#?(:cljs
   (defn restore-scroll-after-render!
     "Hold `node`'s scrollTop at `target` across an async, data-driven re-render
      (expand/collapse of a server-sited virtual list). A single rAF fires
      before the new rows arrive and the re-render resets scrollTop to 0, so
      re-apply across a short burst of frames until the position sticks. rAF
      runs before paint, so each painted frame keeps the anchored row in place."
     [node target]
     (when (and node target)
       (let [tries (atom 0)]
         (letfn [(step []
                   (set! (.-scrollTop node) target)
                   (when (< (swap! tries inc) 20)
                     (js/requestAnimationFrame step)))]
           (js/requestAnimationFrame step))))))
