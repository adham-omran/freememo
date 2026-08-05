(ns freememo.segmented
  "One option of a segmented control — a `role=radio` button that reports its
   selection through a caller-supplied Save.

   Extracted from content-toolbar-settings' card-type control so the Settings
   page and the pre-prompt dialog render the same widget instead of copies;
   freememo.number-stepper is the precedent for a control shared by a toolbar
   and a non-toolbar caller.

   Callers own the surrounding `role=radiogroup` div and call Segment once per
   option, exactly as the card-type control did before the extraction. Styling
   is the unscoped `.segmented*` classes in index.css."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.a11y :as a11y]))

;; Enter/Space on the focused option, as an On-all transform: returns the
;; event (truthy → enters the fork) for an activation keypress on the element
;; itself, nil otherwise (listen-some drops it). preventDefault stops Space
;; from scrolling the page.
(defn- activation-key [e]
  #?(:cljs (when (a11y/focused-enter-or-space? e)
             (.preventDefault e)
             e)
     :clj nil))

(e/defn Segment
  "One segment of a segmented control.

   Pre:  `selected` is the group's current value and `value` is this segment's,
         compared with =. `Save` is an (e/fn [t value]) that MUST spend the
         token — on server completion when it persists, immediately when the
         write is client-local.
   Post: a focusable role=radio button that invokes Save once per click, Enter
         or Space, and carries aria-busy while that Save is in flight.
   Note: mousedown preventDefault keeps a CLICK from stealing focus off the
         Quill editor (preserves the text selection); Tab-focus is deliberate
         and allowed. Selection shows via the --selected class, not a glyph."
  [selected value label Save]
  (e/client
    (let [selected? (= selected value)]
      (dom/button
        (dom/props {:class (str "segmented-option"
                             (when selected? " segmented-option--selected"))
                    :type "button" :role "radio" :tabindex "0"
                    :aria-checked (str selected?)})
        (dom/On "mousedown" (fn [e] (.preventDefault e)) nil)
        (e/for [[t e] (dom/On-all "click")]
          (when e
            (dom/props {:aria-busy "true"})
            (Save t value)))
        (e/for [[t e] (dom/On-all "keydown" activation-key)]
          (when e
            (dom/props {:aria-busy "true"})
            (Save t value)))
        (dom/text label)))))
