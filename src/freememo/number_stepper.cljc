(ns freememo.number-stepper
  "Borderless `[−] N [+]` stepper shared by the toolbar's three numeric
   controls (card-count, context-pages, priority). Replaces the native browser
   spinner with flat, unbordered −/+ buttons flanking an editable number.

   Each control differs only in bounds, precision, label, and how it persists.
   This component owns the shared UI, clamp and rounding, and delegates
   persistence to a `Save` electric-fn supplied by the call site.

   `decimals` 0 gives the integer controls their old behavior. Priority passes
   3. Rounding is delegated to freememo.priority/round-to so the client and the
   server share ONE rounding rule — a stepper that rounded differently from the
   server would leave the optimistic mirror showing a value the DB never held."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.priority :as priority]))

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

(defn- settle
  "Clamp `n` to [lo,hi] and round it to `decimals`. The round is what keeps
   repeated ±1 stepping from drifting: 12.345 is not exact in binary, so ten
   increments and ten decrements would otherwise not return to 12.345."
  [lo hi decimals n]
  (priority/round-to (clamp lo hi n) decimals))

(defn- read-step
  "Current mirrored value stepped by `delta`, then settled. Reads
   `!mirror` — kept live by every path (buttons and the typed input, see
   NumberStepper) — NOT the reactive `value` prop, so the click handler has
   no reactive dependency on `value` and cannot loop when a save bumps it.
   Falls back to `lo` before any value has landed."
  [!mirror lo hi decimals delta]
  (settle lo hi decimals (+ (or (some-> !mirror deref) lo) delta)))

(defn- parse-typed
  "Parse a typed number string, settled to [lo,hi] at `decimals` places, or
   nil for blank/unparseable input. Run on commit (blur) — see NumberStepper.
   nil means \"nothing to save yet\", not an error.
   Pre : `decimals` is the control's precision.
   Post: an integer control (`decimals` 0) parses EXACTLY as it did before
         decimals existed. parseInt is kept for it on purpose: parseFloat
         would turn a typed \"1.5\" into 2 where parseInt gave 1, and \"1e3\"
         into 1000 where parseInt gave 1 — a silent behavior change to
         card-count and context-pages, which this feature never asked for."
  [s lo hi decimals]
  #?(:cljs (when-some [v (not-empty s)]
             (let [n (if (pos? decimals) (js/parseFloat v) (js/parseInt v))]
               (when-not (js/isNaN n) (settle lo hi decimals n))))
     :clj nil))

(defn- step-attr
  "The `step` attribute for `decimals`: \"1\", \"0.1\", \"0.001\". Built as a
   string so no float formatting can round it."
  [decimals]
  (if (pos? decimals)
    (str "0." (apply str (repeat (dec decimals) "0")) "1")
    "1"))

(e/defn NumberStepper
  "Render `label [−] N [+] suffix`. `Save` persists; `!mirror` (nilable) is a
   client atom kept in sync so sibling mounts reading the same atom update
   immediately.

   Pre:  `value` ∈ [min-val, max-val]; `decimals` is the control's precision
         (0 for the integer controls, 3 for priority); `mount-key` is a stable
         per-mount identity (frame isolation; a changed key remounts the input).
         `input-aria-label` names the number input for assistive tech — the
         visible `label` (\"#\", nil) is not a usable accessible name.
         `Save` :: (e/fn [settled-number] → result-or-nil).
   Post: every −/+ click and typed edit calls `(Save settled)` exactly once
         per commit; the token spends on completion. The middle input is an
         A1-fallback hand-rolled input (Forms5 has no commit-on-blur text
         primitive — Input!/Input fire on \"input\" = per-keystroke): managed
         via a Focused? guard — it reflects the authoritative value only
         while the field isn't focused, so a save landing mid-type never
         clobbers the user's draft (the A5 fix) — and committed once on
         \"change\" (blur).
   Invariant: settle on both paths — buttons saturate at the bounds, typed
              input is clamped and rounded by `parse-typed` before Save and
              before the digits are re-displayed."
  [value min-val max-val decimals mount-key label input-aria-label suffix disabled? !mirror Save]
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
        (let [nv (dom/On "click" (fn [_] (read-step !mirror min-val max-val decimals -1)) nil)
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
          ;; A decimal control needs the wider field ("12.345" is six
          ;; characters against the 34px integer width) and the decimal
          ;; keypad. `decimals` already says which this is, so no call site
          ;; passes a class or an inputmode.
          (dom/props {:type "number" :min (str min-val) :max (str max-val)
                      :step (step-attr decimals)
                      :inputmode (if (pos? decimals) "decimal" "numeric")
                      :class (if (pos? decimals)
                               "number-stepper-input number-stepper-input-wide"
                               "number-stepper-input")
                      :aria-label input-aria-label :disabled disabled?})
          (when-not (dom/Focused?) (set! (.-value dom/node) (str value)))
          (let [raw (dom/On "change" #(-> % .-target .-value) nil)
                [t _] (e/Token raw)]
            (when t
              ;; The mirror write is a `let` binding, not a sibling in a `do`:
              ;; `do` is concurrent in Electric, so a sibling (spend …) races
              ;; the reset!. Binding sequences them, matching the −/+ paths.
              (if-some [parsed (parse-typed raw min-val max-val decimals)]
                (let [settled (if !mirror (reset! !mirror parsed) parsed)]
                  (spend t (Save settled)))
                (t)))))) ; blank/unparseable on blur — release the token, nothing to save

      ;; + increment
      (dom/button
        (dom/props {:class "number-stepper-btn" :type "button"
                    :aria-label "Increase" :disabled (or disabled? at-max?)})
        (dom/text "+")
        (let [nv (dom/On "click" (fn [_] (read-step !mirror min-val max-val decimals 1)) nil)
              nv (if (and (some? nv) !mirror) (reset! !mirror nv) nv)
              [t _] (e/Token nv)]
          (when t (spend t (Save nv)))))

      (when suffix
        (dom/span (dom/props {:class "number-stepper-suffix"}) (dom/text suffix)))))))
