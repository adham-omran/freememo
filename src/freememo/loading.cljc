(ns freememo.loading
  "Loading-aware UI helpers.

   Why this exists: `e/Offload` (and any unresolved `e/server` value) latches to an
   empty `e/amb` while pending — NOT nil. So the common `(if (nil? server-value)
   loading loaded)` never shows `loading`: the empty value suppresses the whole
   branch, leaving a blank. `WithLoading` captures the resolved value into a client
   atom via a one-shot token, exposing a real nil→value transition the UI can branch on.

   Which helper to pick: `WithLoading` and `WithLatestLoading` move the resolved
   value to the client, so they suit a BOUNDED value — a settings map, one card,
   a citation. `WithLatestServerLoading` latches a readiness flag instead and
   leaves the value on the server; it is the one to use for a COLLECTION."
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

(e/defn WithLatestLoading
  "Like WithLoading, but re-query-aware: renders `Loading` until `Thunk`'s value
   first resolves, then `(Loaded latest-value)` — and keeps tracking the latest
   value (no one-shot Token), so a re-querying Thunk (e.g. wrapping e/Offload)
   updates in place without flashing back to Loading. Pairs with e/Offload's
   latch for stale-while-revalidate.

   Takes a BOUNDED value. `(reset! !v v)` is a client-scope call, so the whole
   resolved value crosses the wire — use WithLatestServerLoading for a collection.

   Pre  : `Thunk` is `(e/fn [] <server value>)`, resolved value non-nil (a nil
          result reads as perpetual Loading — wrap a nil source in a sentinel).
          `Loaded` is `(e/fn [value] …)`. `Loading` is `(e/fn [] …)`.
   Post : `(Loading)` until the value first resolves, then `(Loaded value)` with
          the most recent value.
   Invariant: the shown value comes from a client atom, never the pending server
          value directly — that is what avoids e/Offload's empty-amb suppression."
  ([Thunk Loaded] (WithLatestLoading Thunk Loaded Spinner))
  ([Thunk Loaded Loading]
   (e/client
     (let [!v (atom nil)]
       ;; Capture each resolved value into the client atom; suppressed while the
       ;; value is pending (empty amb), so the atom stays nil → Loading — until
       ;; the first value arrives. `case` forces evaluation (anki-sync idiom).
       (let [v (Thunk)] (case v (reset! !v v)))
       (if-some [v (e/watch !v)]
         (Loaded v)
         (Loading))))))

(e/defn WithLatestServerLoading
  "Like WithLatestLoading, but the resolved value never leaves the server —
   only a readiness flag crosses. This is the helper for a COLLECTION.

   WithLatestLoading captures the value into a client atom, so the whole value
   crosses at the `reset!`. A windowed table under it then reads rows the client
   already holds, and the window buys nothing: the collection already crossed.
   Here `Thunk` is called inside e/server, so its value materializes there, and
   the client learns one boolean.

   Pre  : `Thunk` is `(e/fn [] <server value>)`, resolved value non-nil (a nil
          result reads as perpetual Loading — wrap a nil source in a sentinel).
          `Loading` is `(e/fn [] …)`.
          `Loaded` is `(e/fn [value] …)` and MUST site its own reads of `value`.
          The argument arrives server-sited; any client-scope read of it — a
          `count`, a `mapv`, a destructure — pulls the whole value across and
          gives back the cost this helper exists to remove. Write
          `(e/server (count value))` and `(e/server (nth value i nil))`.
   Post : `(Loading)` until the value first resolves, then `(Loaded value)` with
          the most recent value, and `value` stays on the server.
   Invariant: the rendered branch is chosen by a client atom, never by the
          pending server value — that is what avoids e/Offload's empty-amb
          suppression. The Loading branch reads nothing from `Thunk`, so a
          pending value cannot suppress it."
  ([Thunk Loaded] (WithLatestServerLoading Thunk Loaded Spinner))
  ([Thunk Loaded Loading]
   (e/client
     (let [!ready? (atom false)
           ;; An e/fn call materializes where it is made. Made here, on the
           ;; server, the value stays there — the same reason e/Offload is
           ;; always wrapped in e/server.
           v (e/server (Thunk))]
       ;; Statement form, mirroring WithLatestLoading's capture: suppressed while
       ;; the value is pending (empty amb), so !ready? stays false → Loading.
       ;; Only this boolean crosses the wire.
       (let [ready (e/server (some? v))] (case ready (reset! !ready? ready)))
       (if (e/watch !ready?)
         (Loaded v)
         (Loading))))))
