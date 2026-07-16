(ns freememo.number-stepper
  "Borderless `[−] N [+]` integer stepper shared by the toolbar's three numeric
   controls (card-count, context-pages, priority). Replaces the native browser
   spinner with flat, unbordered −/+ buttons flanking an editable number.

   Each control differs only in bounds, label, and how it persists; this
   component owns the shared UI + clamp, and delegates persistence to a `Save`
   electric-fn supplied by the call site."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.electric-forms5 :as forms]))

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

(defn- clamp [lo hi n] (max lo (min hi n)))

(defn- read-step
  "Current mirrored value stepped by `delta` and clamped to [lo,hi]. Reads
   `!mirror` — kept live by every path (buttons and the managed Forms5 input,
   see NumberStepper) — NOT the reactive `value` prop, so the click handler
   has no reactive dependency on `value` and cannot loop when a save bumps
   it. Falls back to `lo` before any value has landed."
  [!mirror lo hi delta]
  (clamp lo hi (+ (or (some-> !mirror deref) lo) delta)))

(defn- parse-typed
  "Parse a typed digit string to an int clamped to [lo,hi], or nil for
   blank/unparseable input. Used as Input!'s `:Parse` hook — see
   NumberStepper. nil means \"nothing to save yet\", not an error."
  [s lo hi]
  #?(:cljs (when-some [v (not-empty s)]
             (let [n (js/parseInt v)]
               (when-not (js/isNaN n) (clamp lo hi n))))
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
   Post: every −/+ click and typed edit calls `(Save clamped)` exactly once
         per commit; the token spends on completion. The middle input is a
         managed Forms5 `Input!` bound to `value` — it reflects the
         authoritative value only while the field isn't dirty (unfocused, no
         in-flight edit), so a save landing mid-type never clobbers the
         user's draft (see forms.md / patterns.md, managed vs unmanaged
         inputs — this is the A5 fix).
   Invariant: clamp on both paths — buttons saturate at the bounds, typed
              input is clamped in Input!'s `:Parse` before Save and before
              the digits are re-displayed."
  [value min-val max-val mount-key label input-aria-label suffix disabled? !mirror Save]
  (e/client
    (dom/span
      (dom/props {:class "number-stepper"
                  :style (when disabled? {:opacity "0.5"})})
      (when label
        (dom/span (dom/props {:class "number-stepper-label"}) (dom/text label)))

      ;; − decrement. The step is computed once in the click callback (plain
      ;; JS, reads !mirror), so the awaiting re-eval keeps the same nv.
      (dom/button
        (dom/props {:class "number-stepper-btn" :type "button"
                    :aria-label "Decrease" :disabled disabled?})
        (dom/text "−")
        (let [nv (dom/On "click" (fn [_] (read-step !mirror min-val max-val -1)) nil)
              nv (if (and (some? nv) !mirror) (reset! !mirror nv) nv)
              [t _] (e/Token nv)]
          (when t (spend t (Save nv)))))

      ;; N — editable, frame-isolated per CLAUDE.md inline-number pattern.
      ;; Managed Forms5 Input!: bound to `value`, "don't damage user input"
      ;; while focused/waiting/erroring — no more imperative set! stomping a
      ;; concurrent in-progress edit.
      (e/for-by identity [_k [mount-key]]
        (e/for [[t edit] (forms/Input! mount-key value
                           :type "number" :min (str min-val) :max (str max-val)
                           :inputmode "numeric" :class "number-stepper-input"
                           :aria-label input-aria-label :disabled disabled?
                           :Parse (e/fn [s] (parse-typed s min-val max-val)))]
          (if-some [clamped (get edit mount-key)]
            (do
              (when !mirror (reset! !mirror clamped))
              (spend t (Save clamped)))
            (t)))) ; blank/unparseable keystroke — release the token, nothing to save

      ;; + increment
      (dom/button
        (dom/props {:class "number-stepper-btn" :type "button"
                    :aria-label "Increase" :disabled disabled?})
        (dom/text "+")
        (let [nv (dom/On "click" (fn [_] (read-step !mirror min-val max-val 1)) nil)
              nv (if (and (some? nv) !mirror) (reset! !mirror nv) nv)
              [t _] (e/Token nv)]
          (when t (spend t (Save nv)))))

      (when suffix
        (dom/span (dom/props {:class "number-stepper-suffix"}) (dom/text suffix))))))
