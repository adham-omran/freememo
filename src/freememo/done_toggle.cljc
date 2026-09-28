(ns freememo.done-toggle
  "The face of a done toggle: the icon and label inside a `.done-toggle` button.

   Every done control in the app shares one look (plans/done-state-visibility.md):
   the PDF page toggle, the PDF and score document toggle, and the extract
   Done/Restore pair. The LOOK is the `.done-toggle` class keyed on
   `aria-pressed`, in index.css. Each site keeps its own button and its own
   click behaviour, because those differ — the PDF toggles go through the
   optimistic command queue, and ExtractActions spends a token and navigates.

   The same class drives the toolbar row tint: a `.toolbar-container` that
   holds a pressed toggle is tinted by a `:has()` rule. So no container carries
   a prop derived from the done state."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.icons :as icons]))

;; Pre:  called inside a `dom/button` whose class includes `done-toggle` and
;;       whose `aria-pressed` equals `(str (boolean pressed?))`.
;; Post: renders ✓ + `label`. When `pressed?`, also renders ↺ + `restore-label`,
;;       which index.css shows in place of ✓ + `label` on hover and on
;;       :focus-visible — the preview of what a click does.
(e/defn DoneToggleFace [pressed? label restore-label]
  (e/client
    (icons/Icon :check :size 16 :class "done-toggle__state")
    (dom/span (dom/props {:class "icon-label done-toggle__state"}) (dom/text label))
    (when pressed?
      (e/amb
        (icons/Icon :rotate-ccw :size 16 :class "done-toggle__restore")
        (dom/span (dom/props {:class "icon-label done-toggle__restore"})
          (dom/text restore-label))))))
