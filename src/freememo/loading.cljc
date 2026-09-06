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

(def ^:private no-region-value ::no-region-value)

(defn region-success
  "Create a successful result for WithLatestRegion.

   Pre : value is bounded, or the caller keeps it server-sited with
         WithLatestServerRegion.
   Post: the result has one successful value."
  ([value]
   {:ok? true :value value})
  ([request-key value]
   {:ok? true :request-key request-key :value value}))

(defn region-failure
  "Create a failed result for WithLatestRegion.

   Pre : error is safe to show to the user.
   Post: the result has no successful value."
  ([error]
   {:ok? false :error error})
  ([request-key error]
   {:ok? false :request-key request-key :error error}))

(defn region-phase
  "Return the visible phase for one progressive region.

   Pre : state came from WithLatestRegion.
   Post: returns cold, ready, refreshing, failed, or stale-error.
   Invariant: a failure never changes a successful value into an empty state."
  [state request-key]
  (let [{:keys [value settled-key error]} state
        has-value? (if (contains? state :has-value?)
                     (:has-value? state)
                     (not= no-region-value value))
        current-settled? (= settled-key request-key)]
    (cond
      (and current-settled? error has-value?) :stale-error
      (and current-settled? error) :failed
      (and has-value? (not current-settled?)) :refreshing
      has-value? :ready
      :else :cold)))

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

(e/defn SkeletonBlock
  "Render one stable placeholder block.

   Pre : height is a CSS length.
   Post: layout reserves that height while content is cold."
  [height]
  (e/client
    (dom/div
      (dom/props {:class "skeleton progressive-skeleton"
                  :aria-hidden "true"
                  :style {:height height}}))))

(e/defn SkeletonRows
  "Render fixed-height placeholders for a bounded table region."
  [row-count row-height]
  (e/client
    (dom/div (dom/props {:class "progressive-skeleton-rows" :aria-hidden "true"})
      (e/for [i (range row-count)]
        (dom/div
          (dom/props {:class "skeleton progressive-skeleton-row"
                      :style {:height row-height}
                      :key i}))))))

(e/defn RegionNotice
  "Render a local region failure without replacing successful content."
  [phase error]
  (e/client
    (when (#{:failed :stale-error} phase)
      (dom/div
        (dom/props {:class (str "progressive-region-notice "
                            (if (= phase :stale-error)
                              "progressive-region-notice-stale"
                              "progressive-region-notice-failed"))
                    :role "status"})
        (dom/text (if (= phase :stale-error)
                    (str "Could not refresh. Showing the last result. " error)
                    (str "Could not load this section. " error)))))))

(e/defn WithLatestRegion
  "Render one bounded data region with stable stale-while-refresh behavior.

   `Thunk` must return region-success or region-failure.

   Pre : request-key contains every input that changes the result.
         The successful value is bounded and can cross to the client.
   Post: Loading receives cold while no value exists.
         Loaded receives the last successful value, phase, and local error.
         Failed receives a cold-load error.
   Invariant: a refresh or refresh failure keeps the last successful value."
  [request-key Thunk Loaded Loading Failed]
  (e/client
    (let [!state (atom {:value no-region-value
                        :settled-key ::not-settled
                        :error nil})
          state (e/watch !state)]
      ;; A pending e/Offload result is empty e/amb. This statement then stops,
      ;; while the sibling render path continues from the saved client state.
      (let [outcome (Thunk)]
        (when (and (not (:pending? outcome))
                (or (not= (or (:request-key outcome) request-key)
                      (:settled-key state))
                    (not= (:error outcome) (:error state))
                    (and (:ok? outcome)
                         (not= (:value outcome) (:value state)))))
          (case outcome
            (if (:ok? outcome)
              (reset! !state {:value (:value outcome)
                              :settled-key (or (:request-key outcome) request-key)
                              :error nil})
              (swap! !state assoc
                :settled-key (or (:request-key outcome) request-key)
                :error (or (:error outcome) "Unknown error"))))))
      (let [phase (region-phase state request-key)]
        (case phase
          :cold (Loading phase)
          :failed (Failed (:error state))
          (Loaded (:value state) phase (:error state)))))))

(e/defn WithLatestServerRegion
  "Render one collection region without moving the collection to the client.

   `Thunk` must return region-success or region-failure.

   Pre : request-key contains every input that changes the result.
         Loaded sites all collection reads on the server.
   Post: Loading receives cold while no collection exists.
         Loaded receives the last successful server value, phase, and error.
         Failed receives a cold-load error.
   Invariant: only readiness metadata crosses before Loaded requests a row."
  [request-key Thunk Loaded Loading Failed]
  (e/client
    (let [!metadata (atom {:has-value? false
                           :settled-key ::not-settled
                           :error nil})
          metadata (e/watch !metadata)
          region-state
          (e/server
            (e/with-cycle [previous {:value no-region-value
                                     :settled-key ::not-settled
                                     :error nil}]
              (let [outcome (Thunk)]
                (cond
                  (:pending? outcome) previous
                  (:ok? outcome)
                  {:value (:value outcome)
                   :settled-key (or (:request-key outcome) request-key)
                   :error nil}
                  :else
                  (assoc previous
                    :settled-key (or (:request-key outcome) request-key)
                    :error (or (:error outcome) "Unknown error"))))))]
      ;; Transfer three scalar fields. The collection stays in region-state on
      ;; the server and crosses only through Loaded's explicit row reads.
      (let [next-metadata
            (e/server
              {:has-value? (not= no-region-value (:value region-state))
               :settled-key (:settled-key region-state)
               :error (:error region-state)})]
        (when (not= next-metadata metadata)
          (case next-metadata (reset! !metadata next-metadata))))
      (let [phase (region-phase metadata request-key)]
        (case phase
          :cold (Loading phase)
          :failed (Failed (:error metadata))
          (Loaded (e/server (:value region-state)) phase (:error metadata)))))))

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
