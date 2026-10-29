(ns freememo.icons
  "Lucide-derived SVG icon component. The path data lives in
   `freememo.icon-paths`, which plain-DOM callers also read. Each icon
   renders as a 24×24 viewBox SVG with `fill: none; stroke: currentColor; stroke-width: 2; stroke-linecap: round;
   stroke-linejoin: round` — Lucide's standard stroke contract. Color follows
   the surrounding `color:` via `currentColor`.

   The Electric SVG root owns its plain DOM children. The installer creates
   each child in the SVG namespace so the browser can render it.

   Pre:  caller passes a known glyph keyword.
   Post: a sized SVG element is mounted; if `:title` is provided it sets
         `data-tooltip` (visible on hover via index.css) and `aria-label`
         (read by screen readers).
   Invariant: an unknown glyph keyword renders an empty placeholder rather
              than throwing, so a typo at one call site doesn't crash the
              page."
  (:require
   [freememo.icon-dom :as icon-dom]
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.electric-svg3 :as svg]
   [freememo.tooltip :as tooltip]))

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
      (case (icon-dom/install! dom/node name size class title)
        (tooltip/Tooltip! (when title title) :aria? true)))))
