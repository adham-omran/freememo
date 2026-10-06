(ns freememo.icons
  "Lucide-derived SVG icon component. The path data lives in
   `freememo.icon-paths`, which plain-DOM callers also read. Each icon
   renders as a 24×24 viewBox SVG with `fill: none; stroke: currentColor; stroke-width: 2; stroke-linecap: round;
   stroke-linejoin: round` — Lucide's standard stroke contract. Color follows
   the surrounding `color:` via `currentColor`.

   SVG-namespaced children (path/line/circle/rect/polyline) MUST be created via
   `hyperfiddle.electric-svg3` macros — using `dom/element` from electric-dom3
   creates HTML-namespace elements which render invisibly inside an `<svg>`.

   Pre:  caller passes a known glyph keyword.
   Post: a sized SVG element is mounted; if `:title` is provided it sets
         `data-tooltip` (visible on hover via index.css) and `aria-label`
         (read by screen readers).
   Invariant: an unknown glyph keyword renders an empty placeholder rather
              than throwing, so a typo at one call site doesn't crash the
              page."
  (:require
   [freememo.icon-paths :as icon-paths]
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.electric-svg3 :as svg]
   [freememo.tooltip :as tooltip]))

(defn- icon-known? [name]
  (contains? icon-paths/paths name))

;; Pre:  `name` is a keyword present in `icon-paths/paths`. `size` defaults to 16.
;;       `title` is optional but recommended for icon-only buttons (sets
;;       data-tooltip + aria-label).
;; Post: An <svg> element mounted. Stroke attributes hardcoded per Lucide
;;       contract. Unknown names render an empty <svg> placeholder so a
;;       typo doesn't crash the page.
(e/defn Icon [name & {:keys [size class title]
                      :or {size 16}}]
  (e/client
    (svg/svg
      (dom/props
        (cond-> {:width (str size)
                 :height (str size)
                 :viewBox "0 0 24 24"
                 :fill "none"
                 :stroke "currentColor"
                 :stroke-width "2"
                 :stroke-linecap "round"
                 :stroke-linejoin "round"
                 :aria-hidden (if title "false" "true")}
          class (assoc :class class)))
      ;; Icon-only trigger: the tooltip text also names the element for a11y.
      (when title (tooltip/Tooltip! title :aria? true))
      (when (icon-known? name)
        (e/for [[tag attrs] (e/diff-by hash (get icon-paths/paths name))]
          (case tag
            :path (svg/path (dom/props attrs))
            :line (svg/line (dom/props attrs))
            :circle (svg/circle (dom/props attrs))
            :rect (svg/rect (dom/props attrs))
            :polyline (svg/polyline (dom/props attrs))
            nil))))))
