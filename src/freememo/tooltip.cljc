(ns freememo.tooltip
  "The one way to give an element a hover tooltip. `Tooltip!` (Electric) and
   `set-tooltip!` (plain CLJS DOM) write the `data-tooltip` attribute.
   `freememo.tooltip-layer` reads it and paints one bubble in the top layer,
   shown on hover and hidden at once on hover-off.

   Never set `:data-tooltip` or a native `title` yourself.
   `test/freememo/tooltip_construct_test.clj` fails on both. A native `title`
   waits about 500 ms, looks different, and is a second mechanism to drift.

   Accessibility: `data-tooltip` alone is a visual affordance and does NOT name
   the element for assistive tech. Pass `:aria? true` ONLY on triggers with no
   visible text (icon-only buttons, glyph spans, checkboxes) — it sets
   `aria-label` to the same string. Leave it off on text-bearing triggers, whose
   visible text already provides the accessible name; forcing an aria-label
   there would override that name.

   Placement: the bubble is centered below the element by default. Callers that
   need right-edge anchoring keep the `.tooltip-right` class on the element;
   neither fn owns the class, so neither clobbers the caller's other classes."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]))

;; Pre:  called inside a DOM element body (ambient `dom/node` is bound);
;;       `text` is the tooltip string, or nil for no tooltip.
;; Post: the ambient element carries `:data-tooltip text`, plus `:aria-label
;;       text` when `:aria?` is truthy. No element is rendered.
(e/defn Tooltip! [text & {:keys [aria?]}]
  (e/client
    (dom/props (cond-> {:data-tooltip text}
                 aria? (assoc :aria-label text)))))

#?(:cljs
   (defn set-tooltip!
     "`Tooltip!` for DOM code outside Electric.
      Pre : `el` is an element; `text` is a string, or nil for no tooltip.
      Post: `el` carries `data-tooltip text`, plus `aria-label text` when
            `:aria?` is truthy. A nil `text` removes both attributes it owns."
     [^js el text & {:keys [aria?]}]
     (if (some? text)
       (do (.setAttribute el "data-tooltip" text)
           (when aria? (.setAttribute el "aria-label" text)))
       (do (.removeAttribute el "data-tooltip")
           (when aria? (.removeAttribute el "aria-label"))))))
