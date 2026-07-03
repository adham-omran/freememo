(ns freememo.number-stepper
  "Borderless `[−] N [+]` integer stepper shared by the toolbar's three numeric
   controls (card-count, context-pages, priority). Replaces the native browser
   spinner with flat, unbordered −/+ buttons flanking an editable number.

   Each control differs only in bounds, label, and how it persists; this
   component owns the shared UI + clamp, and delegates persistence to a `Save`
   electric-fn supplied by the call site."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]))

(defn- spend
  "Token-spend policy shared by all paths. `r` is the Save result: a settings
   map ({:success bool :error?}) or any non-nil sentinel (priority returns the
   :refresh swap! int). Spend on completion; surface an error only on an
   explicit failure map. nil = still pending → leave the token unspent."
  [t r]
  (when (some? r)
    (if (and (map? r) (false? (:success r)))
      (t (:error r))
      (t))))

(defn- read-step
  "Current input value stepped by `delta` and clamped to [lo,hi]. Reads the live
   DOM (`!node` atom), NOT the reactive `value` prop — so the handler has no
   reactive dependency on `value` and cannot loop when a save bumps it. Returns
   nil when the node is absent (pre-mount; no real click can reach here)."
  [!node lo hi delta]
  #?(:cljs
     (when-some [node @!node]
       (let [cur (js/parseInt (.-value node))
             cur (if (js/isNaN cur) lo cur)]
         (max lo (min hi (+ cur delta)))))
     :clj nil))

(e/defn NumberStepper
  "Render `label [−] N [+] suffix`. `Save` persists; `!mirror` (nilable) is a
   client atom kept in sync so sibling mounts reading the same atom update
   immediately.

   Pre:  `value` ∈ [min-val, max-val]; `mount-key` is a stable per-mount
         identity (frame isolation; a changed key remounts the input).
         `input-aria-label` names the number input for assistive tech — the
         visible `label` (\"#\", nil) is not a usable accessible name.
         `Save` :: (e/fn [clamped-int] → result-or-nil).
   Post: every −/+ click and committed edit calls `(Save clamped)` exactly once
         (the value is computed in the click/change callback, so the reactive
         re-eval that awaits Save does not recompute it); the token spends on
         completion.
   Invariant: clamp on both paths — buttons saturate at the bounds, typed input
              is clamped before Save and before the digits are re-displayed."
  [value min-val max-val mount-key label input-aria-label suffix disabled? !mirror Save]
  (e/client
    (let [!node (atom nil)]
      (dom/span
        (dom/props {:class "number-stepper"
                    :style (when disabled? {:opacity "0.5"})})
        (when label
          (dom/span (dom/props {:class "number-stepper-label"}) (dom/text label)))

        ;; − decrement. The step is computed once in the click callback (plain
        ;; JS, reads the live DOM), so the awaiting re-eval keeps the same nv.
        (dom/button
          (dom/props {:class "number-stepper-btn" :type "button"
                      :aria-label "Decrease" :disabled disabled?})
          (dom/text "−")
          (let [nv (dom/On "click" (fn [_] (read-step !node min-val max-val -1)) nil)
                nv (if (and (some? nv) !mirror) (reset! !mirror nv) nv)
                [t _] (e/Token nv)]
            (when t (spend t (Save nv)))))

        ;; N — editable, frame-isolated per CLAUDE.md inline-number pattern;
        ;; imperative set! avoids :value-prop flicker on the server snapshot.
        (e/for-by identity [_k [mount-key]]
          (dom/input
            (dom/props {:type "number" :min (str min-val) :max (str max-val)
                        :inputmode "numeric" :class "number-stepper-input"
                        :aria-label input-aria-label
                        :disabled disabled?})
            (reset! !node dom/node)
            (set! (.-value dom/node) (str value))
            (let [node dom/node
                  raw (dom/On "change"
                        (fn [e] (let [v (-> e .-target .-value)]
                                  (when (seq v) (js/parseInt v))))
                        nil)
                  clamped (when (some? raw) (max min-val (min max-val raw)))
                  clamped (if (and clamped !mirror) (reset! !mirror clamped) clamped)
                  [t _] (e/Token clamped)]
              (when (some? clamped) (set! (.-value node) (str clamped)))
              (when t (spend t (Save clamped))))))

        ;; + increment
        (dom/button
          (dom/props {:class "number-stepper-btn" :type "button"
                      :aria-label "Increase" :disabled disabled?})
          (dom/text "+")
          (let [nv (dom/On "click" (fn [_] (read-step !node min-val max-val 1)) nil)
                nv (if (and (some? nv) !mirror) (reset! !mirror nv) nv)
                [t _] (e/Token nv)]
            (when t (spend t (Save nv)))))

        (when suffix
          (dom/span (dom/props {:class "number-stepper-suffix"}) (dom/text suffix)))))))
