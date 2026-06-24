(ns freememo.loading
  "Loading-aware UI helpers.

   Why this exists: `e/Offload` (and any unresolved `e/server` value) latches to an
   empty `e/amb` while pending — NOT nil. So the common `(if (nil? server-value)
   loading loaded)` never shows `loading`: the empty value suppresses the whole
   branch, leaving a blank. `WithLoading` captures the resolved value into a client
   atom via a one-shot token, exposing a real nil→value transition the UI can branch on."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]))

(e/defn Spinner
  "The app's CSS spinner (index.css `.spinner`), centered, with an optional label.
   Pre: `label` is a string or nil. Post: an inline spinner; label text when given."
  ([] (Spinner nil))
  ([label]
   (e/client
     (dom/div
       (dom/props {:style {:display "flex" :align-items "center" :justify-content "center"
                           :gap "8px" :padding "24px 8px" :color "var(--color-text-secondary)"}})
       (dom/span (dom/props {:class "spinner"}))
       (when label (dom/text label))))))

(e/defn WithLoading
  "Render `Loading` while the server value from `Thunk` is pending, then `(Loaded value)`.

   Bridges `e/Offload`'s pending state — an empty `e/amb`, not nil — to a usable
   nil→value by capturing the resolved value into a client atom with a one-shot
   token. Fetches once per mount; re-fetch = remount (give the call site a key).

   Pre  : `Thunk` is `(e/fn [] <server value>)`, and its resolved value is non-nil
          — a nil result reads as perpetual loading, so wrap a nil-valued source in
          a sentinel. `Loaded` is `(e/fn [value] …)`. `Loading` is `(e/fn [] …)`.
   Post : `(Loading)` renders until the value resolves, then `(Loaded value)` does."
  ([Thunk Loaded] (WithLoading Thunk Loaded Spinner))
  ([Thunk Loaded Loading]
   (e/client
     (let [!v (atom nil)
           v (e/watch !v)
           [t _] (e/Token Thunk)]
       (when t
         (let [r (Thunk)]
           (case r (do (reset! !v r) (t)))))
       (if (nil? v)
         (Loading)
         (Loaded v))))))
