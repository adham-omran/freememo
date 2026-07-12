(ns freememo.icons
  "Lucide-derived SVG icon component. All paths copied from lucide.dev
   (ISC license). Each icon renders as a 24×24 viewBox SVG with
   `fill: none; stroke: currentColor; stroke-width: 2; stroke-linecap: round;
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
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.electric-svg3 :as svg]
   [freememo.tooltip :as tooltip]))

;; SVG inner content per icon. Each entry is a sequence of inner-SVG element
;; descriptors that Icon below renders as children of the <svg>.
;; Copied verbatim from lucide.dev / github.com/lucide-icons/lucide.
(def ^:private icon-paths
  {:sparkles
   [[:path {:d "M9.937 15.5A2 2 0 0 0 8.5 14.063l-6.135-1.582a.5.5 0 0 1 0-.962L8.5 9.936A2 2 0 0 0 9.937 8.5l1.582-6.135a.5.5 0 0 1 .963 0L14.063 8.5A2 2 0 0 0 15.5 9.937l6.135 1.581a.5.5 0 0 1 0 .964L15.5 14.063a2 2 0 0 0-1.437 1.437l-1.582 6.135a.5.5 0 0 1-.963 0z"}]
    [:path {:d "M20 3v4"}]
    [:path {:d "M22 5h-4"}]
    [:path {:d "M4 17v2"}]
    [:path {:d "M5 18H3"}]]

   :pen-line
   [[:path {:d "M12 20h9"}]
    [:path {:d "M16.376 3.622a1 1 0 0 1 3.002 3.002L7.368 18.635a2 2 0 0 1-.855.506l-2.872.838a.5.5 0 0 1-.62-.62l.838-2.872a2 2 0 0 1 .506-.854z"}]]

   ;; Composite: pen-line + small sparkle marks adjacent to the nib.
   ;; Signals "AI-assisted writing" — used for Generate-with-Prompt.
   :pen-sparkles
   [[:path {:d "M12 20h9"}]
    [:path {:d "M16.376 3.622a1 1 0 0 1 3.002 3.002L7.368 18.635a2 2 0 0 1-.855.506l-2.872.838a.5.5 0 0 1-.62-.62l.838-2.872a2 2 0 0 1 .506-.854z"}]
    [:path {:d "M5 3v3"}]
    [:path {:d "M3.5 4.5h3"}]
    [:path {:d "M22 9v3"}]
    [:path {:d "M20.5 10.5h3"}]]

   :scissors
   [[:circle {:cx "6" :cy "6" :r "3"}]
    [:path {:d "M8.12 8.12 12 12"}]
    [:path {:d "M20 4 8.12 15.88"}]
    [:circle {:cx "6" :cy "18" :r "3"}]
    [:path {:d "M14.8 14.8 20 20"}]]

   :plus
   [[:path {:d "M5 12h14"}]
    [:path {:d "M12 5v14"}]]

   :download
   [[:path {:d "M21 15v4a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2v-4"}]
    [:polyline {:points "7 10 12 15 17 10"}]
    [:line {:x1 "12" :x2 "12" :y1 "15" :y2 "3"}]]

   :upload
   [[:path {:d "M21 15v4a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2v-4"}]
    [:polyline {:points "17 8 12 3 7 8"}]
    [:line {:x1 "12" :x2 "12" :y1 "3" :y2 "15"}]]

   :mic
   [[:path {:d "M12 2a3 3 0 0 0-3 3v7a3 3 0 0 0 6 0V5a3 3 0 0 0-3-3Z"}]
    [:path {:d "M19 10v2a7 7 0 0 1-14 0v-2"}]
    [:line {:x1 "12" :x2 "12" :y1 "19" :y2 "22"}]]

   :music
   [[:path {:d "M9 18V5l12-2v13"}]
    [:circle {:cx "6" :cy "18" :r "3"}]
    [:circle {:cx "18" :cy "16" :r "3"}]]

   :rect-select
   [[:path {:d "M5 3a2 2 0 0 0-2 2"}]
    [:path {:d "M19 3a2 2 0 0 1 2 2"}]
    [:path {:d "M21 19a2 2 0 0 1-2 2"}]
    [:path {:d "M5 21a2 2 0 0 1-2-2"}]
    [:path {:d "M9 3h1"}]
    [:path {:d "M14 3h1"}]
    [:path {:d "M9 21h1"}]
    [:path {:d "M14 21h1"}]
    [:path {:d "M3 9v1"}]
    [:path {:d "M3 14v1"}]
    [:path {:d "M21 9v1"}]
    [:path {:d "M21 14v1"}]]

   :cloud-download
   [[:path {:d "M12 13v8l-4-4"}]
    [:path {:d "m12 21 4-4"}]
    [:path {:d "M4.393 15.269A7 7 0 1 1 15.71 8h1.79a4.5 4.5 0 0 1 2.436 8.284"}]]

   :refresh-cw
   [[:path {:d "M3 12a9 9 0 0 1 9-9 9.75 9.75 0 0 1 6.74 2.74L21 8"}]
    [:path {:d "M21 3v5h-5"}]
    [:path {:d "M21 12a9 9 0 0 1-9 9 9.75 9.75 0 0 1-6.74-2.74L3 16"}]
    [:path {:d "M3 21v-5h5"}]]

   :check
   [[:path {:d "M20 6 9 17l-5-5"}]]

   :rotate-ccw
   [[:path {:d "M3 12a9 9 0 1 0 9-9 9.75 9.75 0 0 0-6.74 2.74L3 8"}]
    [:path {:d "M3 3v5h5"}]]

   :trash-2
   [[:path {:d "M3 6h18"}]
    [:path {:d "M19 6v14c0 1-1 2-2 2H7c-1 0-2-1-2-2V6"}]
    [:path {:d "M8 6V4c0-1 1-2 2-2h4c1 0 2 1 2 2v2"}]
    [:line {:x1 "10" :x2 "10" :y1 "11" :y2 "17"}]
    [:line {:x1 "14" :x2 "14" :y1 "11" :y2 "17"}]]

   :x
   [[:path {:d "M18 6 6 18"}]
    [:path {:d "m6 6 12 12"}]]

   :more-vertical
   [[:circle {:cx "12" :cy "12" :r "1"}]
    [:circle {:cx "12" :cy "5" :r "1"}]
    [:circle {:cx "12" :cy "19" :r "1"}]]

   :link
   [[:path {:d "M10 13a5 5 0 0 0 7.54.54l3-3a5 5 0 0 0-7.07-7.07l-1.72 1.71"}]
    [:path {:d "M14 11a5 5 0 0 0-7.54-.54l-3 3a5 5 0 0 0 7.07 7.07l1.71-1.71"}]]

   :clipboard
   [[:rect {:width "8" :height "4" :x "8" :y "2" :rx "1" :ry "1"}]
    [:path {:d "M16 4h2a2 2 0 0 1 2 2v14a2 2 0 0 1-2 2H6a2 2 0 0 1-2-2V6a2 2 0 0 1 2-2h2"}]]

   :file-plus
   [[:path {:d "M15 2H6a2 2 0 0 0-2 2v16a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V7Z"}]
    [:polyline {:points "14 2 14 8 20 8"}]
    [:line {:x1 "12" :x2 "12" :y1 "18" :y2 "12"}]
    [:line {:x1 "9" :x2 "15" :y1 "15" :y2 "15"}]]

   :loader-2
   [[:path {:d "M21 12a9 9 0 1 1-6.219-8.56"}]]

   :library
   [[:path {:d "m16 6 4 14"}]
    [:path {:d "M12 6v14"}]
    [:path {:d "M8 8v12"}]
    [:path {:d "M4 4v16"}]]

   :scan-text
   [[:path {:d "M3 7V5a2 2 0 0 1 2-2h2"}]
    [:path {:d "M17 3h2a2 2 0 0 1 2 2v2"}]
    [:path {:d "M21 17v2a2 2 0 0 1-2 2h-2"}]
    [:path {:d "M7 21H5a2 2 0 0 1-2-2v-2"}]
    [:path {:d "M7 8h8"}]
    [:path {:d "M7 12h10"}]
    [:path {:d "M7 16h6"}]]

   :chevron-down
   [[:path {:d "m6 9 6 6 6-6"}]]

   :history
   [[:path {:d "M3 12a9 9 0 1 0 3-6.7L3 8"}]
    [:path {:d "M3 3v5h5"}]
    [:path {:d "M12 7v5l4 2"}]]

   :clock
   [[:circle {:cx "12" :cy "12" :r "10"}]
    [:polyline {:points "12 6 12 12 16 14"}]]

   :home
   [[:path {:d "m3 9 9-7 9 7v11a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2z"}]
    [:polyline {:points "9 22 9 12 15 12 15 22"}]]

   :search
   [[:circle {:cx "11" :cy "11" :r "8"}]
    [:path {:d "m21 21-4.3-4.3"}]]

   :settings
   [[:path {:d "M12.22 2h-.44a2 2 0 0 0-2 2v.18a2 2 0 0 1-1 1.73l-.43.25a2 2 0 0 1-2 0l-.15-.08a2 2 0 0 0-2.73.73l-.22.38a2 2 0 0 0 .73 2.73l.15.1a2 2 0 0 1 1 1.72v.51a2 2 0 0 1-1 1.74l-.15.09a2 2 0 0 0-.73 2.73l.22.38a2 2 0 0 0 2.73.73l.15-.08a2 2 0 0 1 2 0l.43.25a2 2 0 0 1 1 1.73V20a2 2 0 0 0 2 2h.44a2 2 0 0 0 2-2v-.18a2 2 0 0 1 1-1.73l.43-.25a2 2 0 0 1 2 0l.15.08a2 2 0 0 0 2.73-.73l.22-.39a2 2 0 0 0-.73-2.73l-.15-.08a2 2 0 0 1-1-1.74v-.5a2 2 0 0 1 1-1.74l.15-.09a2 2 0 0 0 .73-2.73l-.22-.38a2 2 0 0 0-2.73-.73l-.15.08a2 2 0 0 1-2 0l-.43-.25a2 2 0 0 1-1-1.73V4a2 2 0 0 0-2-2z"}]
    [:circle {:cx "12" :cy "12" :r "3"}]]

   :graduation-cap
   [[:path {:d "M22 10v6"}]
    [:path {:d "M2 10l10-5 10 5-10 5z"}]
    [:path {:d "M6 12v5c3 3 9 3 12 0v-5"}]]

   :book-open
   [[:path {:d "M2 3h6a4 4 0 0 1 4 4v14a3 3 0 0 0-3-3H2z"}]
    [:path {:d "M22 3h-6a4 4 0 0 0-4 4v14a3 3 0 0 1 3-3h7z"}]]

   ;; Toast level glyphs (lucide: circle-check / circle-alert / triangle-alert / info)
   :circle-check
   [[:circle {:cx "12" :cy "12" :r "10"}]
    [:path {:d "m9 12 2 2 4-4"}]]

   :circle-alert
   [[:circle {:cx "12" :cy "12" :r "10"}]
    [:line {:x1 "12" :x2 "12" :y1 "8" :y2 "12"}]
    [:line {:x1 "12" :x2 "12.01" :y1 "16" :y2 "16"}]]

   :triangle-alert
   [[:path {:d "m21.73 18-8-14a2 2 0 0 0-3.48 0l-8 14A2 2 0 0 0 4 21h16a2 2 0 0 0 1.73-3Z"}]
    [:path {:d "M12 9v4"}]
    [:path {:d "M12 17h.01"}]]

   :info
   [[:circle {:cx "12" :cy "12" :r "10"}]
    [:path {:d "M12 16v-4"}]
    [:path {:d "M12 8h.01"}]]

   :circle-help
   [[:circle {:cx "12" :cy "12" :r "10"}]
    [:path {:d "M9.09 9a3 3 0 0 1 5.83 1c0 2-3 3-3 3"}]
    [:path {:d "M12 17h.01"}]]})

(defn- icon-known? [name]
  (contains? icon-paths name))

;; Pre:  `name` is a keyword present in `icon-paths`. `size` defaults to 16.
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
        (e/for [[tag attrs] (e/diff-by hash (get icon-paths name))]
          (case tag
            :path (svg/path (dom/props attrs))
            :line (svg/line (dom/props attrs))
            :circle (svg/circle (dom/props attrs))
            :rect (svg/rect (dom/props attrs))
            :polyline (svg/polyline (dom/props attrs))
            nil))))))
