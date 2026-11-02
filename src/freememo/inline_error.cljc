(ns freememo.inline-error
  "A failure shown where the user acted, beside the control that failed
   (plans/remove-toasts.md 1.4).

   Use it when the control stays on screen through the result. A control that
   unmounts when its action ends has no place to show the error, and the banner
   strip (`freememo.failure-banners`) takes that failure instead.

   Pre : `?error` is the second element of an `e/Token` pair: a string, or nil
         while nothing has failed. Anything else is shown through `str`.
   Post: a `span.inline-error` after the control. It is EMPTY on nil, and CSS
         hides an empty one, so it takes no room.
   Invariant: the span is always mounted. A live region that mounts with its
         text is not announced, and a node that mounts and unmounts with the
         error is the shape that costs a remount. Its props are static, so no
         server-derived value can null them.

   Place it as a sibling AFTER the control, never inside the control's props:

     (let [[t ?error] (e/Token click)]
       (dom/button ... )
       (InlineError ?error)
       (when t …))"
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]))

(e/defn InlineError [?error]
  (e/client
    (dom/span
      (dom/props {:class "inline-error" :role "alert"})
      (when (seq (str ?error))
        (dom/text (str ?error))))))
