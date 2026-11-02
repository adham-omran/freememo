(ns freememo.result-line
  "What a finished action did, beside its control (plans/remove-toasts.md 1.5):
   \"3 created, 2 skipped as duplicates\".

   For a result that is a count the changed data does not show, such as the
   items skipped by design. It is not for a failure (`freememo.inline-error`),
   and not for a success the screen already shows by other means.

   Pre : `?text` is a string, or nil when there is no result to show.
   Post: a `span.result-line` after the control, EMPTY on nil and then hidden
         by CSS.
   Invariant: the span is always mounted, for the reasons `InlineError` states.
         The CALLER owns the clearing. It holds the text in an atom, resets the
         atom to nil in the click handler of the same control, and sets it when
         the next result arrives, so a stale count never sits beside a new run.

   Place it as a sibling after the control:

     (ResultLine (e/watch !result))"
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]))

(e/defn ResultLine [?text]
  (e/client
    (dom/span
      (dom/props {:class "result-line" :role "status"})
      (when (seq (str ?text))
        (dom/text (str ?text))))))
