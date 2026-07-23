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

(defn- clamp [lo hi n] (max lo (min hi n)))

(defn- read-step
  "Current mirrored value stepped by `delta` and clamped to [lo,hi]. Reads
   `!mirror` — kept live by every path (buttons and the typed input, see
   NumberStepper) — NOT the reactive `value` prop, so the click handler has
   no reactive dependency on `value` and cannot loop when a save bumps it.
   Falls back to `lo` before any value has landed."
  [!mirror lo hi delta]
  (clamp lo hi (+ (or (some-> !mirror deref) lo) delta)))

(defn- parse-typed
  "Parse a typed digit string to an int clamped to [lo,hi], or nil for
   blank/unparseable input. Run on commit (blur) — see NumberStepper. nil
   means \"nothing to save yet\", not an error."
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
         per commit; the token spends on completion. The middle input is an
         A1-fallback hand-rolled input (Forms5 has no commit-on-blur text
         primitive — Input!/Input fire on \"input\" = per-keystroke): managed
         via a Focused? guard — it reflects the authoritative value only
         while the field isn't focused, so a save landing mid-type never
         clobbers the user's draft (the A5 fix) — and committed once on
         \"change\" (blur).
   Invariant: clamp on both paths — buttons saturate at the bounds, typed
              input is clamped by `parse-typed` before Save and before the
              digits are re-displayed."
  [value min-val max-val mount-key label input-aria-label suffix disabled? !mirror Save]
  (e/client
    ;; Bounds-disable: −/+ disable at the respective bound. Reads the reactive
    ;; `value` prop only for the :disabled attr — NOT the click token — so a
    ;; save that bumps `value` re-renders the attr but never re-fires Save
    ;; (a disabled button can't emit a click, so the token never spends there).
    ;; `(or value min-val)` guards the pre-load nil: − starts disabled, + enabled.
    (let [at-min? (<= (or value min-val) min-val)
          at-max? (>= (or value min-val) max-val)]
    (dom/span
      (dom/props {:class "number-stepper"
                  :style (when disabled? {:opacity "0.5"})})
      (when label
        (dom/span (dom/props {:class "number-stepper-label"}) (dom/text label)))

      ;; − decrement. The step is computed once in the click callback (plain
      ;; JS, reads !mirror), so the awaiting re-eval keeps the same nv.
      (dom/button
        (dom/props {:class "number-stepper-btn" :type "button"
                    :aria-label "Decrease" :disabled (or disabled? at-min?)})
        (dom/text "−")
        (let [nv (dom/On "click" (fn [_] (read-step !mirror min-val max-val -1)) nil)
              nv (if (and (some? nv) !mirror) (reset! !mirror nv) nv)
              [t _] (e/Token nv)]
          (when t (spend t (Save nv)))))

      ;; N — editable, frame-isolated per CLAUDE.md inline-number pattern.
      ;; A1-fallback: Forms5 has no commit-on-blur text primitive (Input!/Input
      ;; fire on "input" = per-keystroke). Managed via a Focused? guard (A5 —
      ;; don't clobber the user's draft mid-edit), committed once on "change"
      ;; (blur).
      (e/for-by identity [_k [mount-key]]
        (dom/input
          (dom/props {:type "number" :min (str min-val) :max (str max-val)
                      :inputmode "numeric" :class "number-stepper-input"
                      :aria-label input-aria-label :disabled disabled?})
          (when-not (dom/Focused?) (set! (.-value dom/node) (str value)))
          (let [raw (dom/On "change" #(-> % .-target .-value) nil)
                [t _] (e/Token raw)]
            (when t
              (if-some [clamped (parse-typed raw min-val max-val)]
                (do
                  (when !mirror (reset! !mirror clamped))
                  (spend t (Save clamped)))
                (t)))))) ; blank/unparseable on blur — release the token, nothing to save

      ;; + increment
      (dom/button
        (dom/props {:class "number-stepper-btn" :type "button"
                    :aria-label "Increase" :disabled (or disabled? at-max?)})
        (dom/text "+")
        (let [nv (dom/On "click" (fn [_] (read-step !mirror min-val max-val 1)) nil)
              nv (if (and (some? nv) !mirror) (reset! !mirror nv) nv)
              [t _] (e/Token nv)]
          (when t (spend t (Save nv)))))

      (when suffix
        (dom/span (dom/props {:class "number-stepper-suffix"}) (dom/text suffix)))))))
