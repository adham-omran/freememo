(ns freememo.tooltip
  "The one hover tooltip primitive. `Tooltip!` attaches the `[data-tooltip]`
   attribute to the current (ambient) DOM element; index.css styles it into a
   ::after bubble shown on :hover and hidden instantly on hover-off (no fade,
   so a stale bubble can never linger over virtual-scroll rows).

   This is the single standardized construct: every call site passes tooltip
   text through here rather than setting `:data-tooltip` inline, so the
   attribute name, the aria coupling, and the placement affordance are decided
   in one place.

   Accessibility: `:data-tooltip` alone is a purely visual affordance and does
   NOT name the element for assistive tech. Pass `:aria? true` ONLY on triggers
   with no visible text (icon-only buttons, glyph spans, checkboxes) — it sets
   `aria-label` to the same string. Leave it off on text-bearing triggers, whose
   visible text already provides the accessible name; forcing an aria-label
   there would override that name.

   Placement: the bubble is centered below the element by default. Callers that
   need right-edge anchoring keep the `.tooltip-right` class on the element (see
   index.css); Tooltip! does not own the class, so it never clobbers the
   caller's other classes."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]))

;; Pre:  called inside a DOM element body (ambient `dom/node` is bound);
;;       `text` is the tooltip string.
;; Post: the ambient element carries `:data-tooltip text`, plus `:aria-label
;;       text` when `:aria?` is truthy. No element is rendered.
(e/defn Tooltip! [text & {:keys [aria?]}]
  (e/client
    (dom/props (cond-> {:data-tooltip text}
                 aria? (assoc :aria-label text)))))
