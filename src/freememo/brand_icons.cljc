(ns freememo.brand-icons
  "Third-party brand marks, rendered inline as SVG.

   Kept out of `freememo.icons` on purpose. That namespace's contract is the
   Lucide stroke contract — `fill: none; stroke: currentColor; stroke-width: 2`
   hardcoded on the `<svg>`, children drawn from a five-tag whitelist — and 89
   call sites depend on it. A brand mark carries its own fixed colours, so it
   can neither follow `currentColor` nor live in `icon-paths`.

   SVG-namespaced children MUST be created via `hyperfiddle.electric-svg3`
   macros — `dom/element` from electric-dom3 creates HTML-namespace elements,
   which render invisibly inside an `<svg>`.

   Invariant: no mark in here defines an `id`. Several instances of one mark
   mount at the same time (the Sync dropdown mounts four), and an `id` on a
   `<defs>` child would collide across them — every `url(#…)` in the document
   resolves to whichever instance mounted first, so the marks would silently
   share one paint server."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.electric-svg3 :as svg]))

;; Source: https://commons.wikimedia.org/wiki/File:Antu_anki.svg — 994 bytes,
;; Fabián Alexis, Antü Plasma Suite. Two departures from those bytes:
;;
;; 1. The disc's vertical gradient (#1cb2ff top → #1584d8 bottom, spanning
;;    45.773 user units) is flattened to its midpoint #189bec. At the 14–16px
;;    sizes every call site asks for, that ramp covers ~2 device pixels.
;;    Flattening is what lets the mark hold the no-`id` invariant above.
;; 2. `transform` separators are spaced. The source writes
;;    `1.04866-387.67-512.7`; a leading `-` starts a new number under the SVG
;;    grammar, so the spaced form is identical and much harder to mis-read.
;;
;; Coordinates are otherwise verbatim, including the `<g transform>` that maps
;; the authored space into the `0 0 48 48` viewBox. The disc is width = height
;; = 45.773 with rx = 22.886, exactly half — i.e. a circle.
(def ^:private anki-disc-fill "#189bec")
(def ^:private anki-star-fill "#bae1f9")
(def ^:private anki-star-path
  (str "m406.2 508.87c0-.404-.306-.655-.918-.754l-8.224-1.196-3.686-7.454c"
       "-.207-.448-.475-.672-.803-.672-.328 0-.595.224-.803.672l-3.686 7.454"
       "-8.224 1.196c-.611.098-.917.349-.917.754 0 .229.137.492.41.786l5.963"
       " 5.8-1.409 8.192c-.022.153-.033.262-.033.328 0 .229.057.423.172.582"
       ".115.158.287.237.516.237.197 0 .415-.065.655-.196l7.356-3.867 7.356"
       " 3.867c.23.131.448.196.655.196.219 0 .385-.079.5-.237.114-.158.172"
       "-.352.172-.582 0-.142-.005-.251-.017-.328l-1.409-8.192 5.947-5.8c.284"
       "-.284.426-.546.426-.786"))

;; Pre:  `size` is a positive number; defaults to 16.
;; Post: one <svg> of `size`×`size` is mounted, `aria-hidden="true"`.
;; Invariant: the colours are fixed, so the mark does not track theme text
;;            colour the way `icons/Icon` does. Every call site already carries
;;            a visible label or an aria-label, so the mark is never the
;;            accessible name — hence no `title` option.
(e/defn AnkiMark [& {:keys [size] :or {size 16}}]
  (e/client
    (svg/svg
      (dom/props {:width (str size)
                  :height (str size)
                  :viewBox "0 0 48 48"
                  :aria-hidden "true"})
      (svg/g
        (dom/props {:transform "matrix(1.04866 0 0 1.04866 -387.67 -512.7)"})
        (svg/rect
          (dom/props {:x "369.69" :y "488.91"
                      :width "45.773" :height "45.773"
                      :rx "22.886"
                      :fill anki-disc-fill}))
        (svg/path
          (dom/props {:d anki-star-path
                      :fill anki-star-fill}))))))
