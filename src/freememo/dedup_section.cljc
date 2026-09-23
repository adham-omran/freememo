(ns freememo.dedup-section
  "Settings → AI Features → Advanced → Knowledge graph → Duplicate facts. The
   three h3 thresholds and the on/off switch for freememo.kg-dedup. It was a card
   of its own under AI Features until 2026-09-23; it tunes what distillation
   produces, so it now sits with the other knowledge-graph settings.

   Its own namespace rather than another field in settings_page.cljc for the
   reason the other sections were split out: each Electric e/defn compiles to one
   JVM method against a 64KB ceiling, and settings_page already composes five
   sections.

   Every field is the A1-fallback shape (Forms5 has no commit-on-blur number
   primitive — Input!/Input fire per keystroke): a local atom seeded from the
   server, guarded by Focused? so a re-render cannot clobber a half-typed value,
   committed once on \"change\" (blur), and clamped client-side so the field snaps
   to what was actually saved rather than to what was typed."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.tooltip :as tooltip]
   #?(:clj [freememo.commands :as commands])
   #?(:clj [freememo.settings :as settings])
   #?(:clj [freememo.user-state :as us])))

#?(:clj
   (defn get-thresholds*
     "Query wrapper taking `_refresh` first so the read re-runs when
      :settings-refresh bumps. The bare atom value is unused — it exists to make
      this call reactive, matching every other query wrapper in the app."
     [_refresh user-id]
     (settings/get-kg-dedup-thresholds user-id)))

#?(:clj
   (defn reset-thresholds!*
     "Delete the three stored thresholds and invalidate the panel.
      The bump is what makes Reset visible: DedupSection reads all four values in
      one query, and a per-field save updates only that field's local atom, so a
      change touching all three has no other route back to the UI.
      Post: settings/reset-kg-dedup-thresholds' result, with :settings-refresh
            bumped on success."
     [user-id]
     (let [r (settings/reset-kg-dedup-thresholds user-id)]
       (when (:success r) (commands/bump! user-id :reset-dedup-thresholds))
       r)))

(def ^:private field-hints
  "What each threshold means in the user's terms, and the pair that motivated it.
   Kept beside the control because a bare number between 0 and 1 is unreadable
   without an example of what crossing it looks like."
  {:flat-trigram
   {:label "Whole-fact similarity"
    :hint (str "How alike two whole facts must read before the later one is "
            "marked. Catches rewording: \"hex digit represents 4 binary bits\" "
            "vs \"…exactly 4 binary bits\". Default 0.55.")}
   :object-trigram
   {:label "Object similarity (same subject)"
    :hint (str "For two facts about the same subject, how alike their objects "
            "must be. Catches a renamed relation: \"COBIT 2019 is-based-on X\" "
            "vs \"COBIT 2019 builds-on X\". Default 0.50.")}
   :containment
   {:label "Word containment"
    :hint (str "How much of the shorter fact's wording the longer one must "
            "contain. Catches an added qualifier: \"site map maps route "
            "symbol\" vs \"…maps a backticked route symbol to an HFQL "
            "expression\". Default 0.80.")}})

(e/defn ThresholdField
  "One threshold, 0.00–1.00. `k` is the settings key (:flat-trigram, …).
   Saving clamps and rounds server-side; the local atom is set from the SAVED
   value so the field shows what is actually stored.

   The input is wrapped in `e/for-by identity` keyed on the server value, which
   forces a genuine unmount/mount when that value changes externally — Reset
   rewrites all three thresholds at once, and without the keyed frame the local
   atom (created in this e/defn's let) would survive and keep showing the old
   number. A per-field save changes the same value it just wrote, so the key is
   unchanged and no remount happens there."
  [user-id k value]
  (e/client
    (let [{:keys [label hint]} (field-hints k)
          id (str "ai-dedup-" (name k))]
      (dom/div
        (dom/props {:class "field setting-row"})
        (dom/div
          (dom/props {:class "setting-row__text"})
          (dom/label (dom/props {:class "label" :for id}) (dom/text label))
          (dom/div (dom/props {:class "hint"}) (dom/text hint)))
        (dom/div
          (dom/props {:class "setting-row__control"})
          (e/for-by identity [server-value [value]]
            (let [!v (atom (str server-value))
                  v (e/watch !v)]
              (dom/input
                (dom/props {:type "number" :min "0" :max "1" :step "0.05"
                            :id id :class "setting-row__number"})
                (when-not (dom/Focused?) (set! (.-value dom/node) v))
                (let [committed (dom/On "change" #(-> % .-target .-value) nil)
                      [t ?error] (e/Token committed)]
                  (dom/props {:disabled (some? t) :aria-busy (some? t)
                              :aria-invalid (some? ?error)})
                  (when t
                    (let [parsed (js/parseFloat committed)]
                      (if (js/isNaN parsed)
                        ;; Blank or unparseable on blur — release the token and put
                        ;; the stored value back rather than saving a NaN.
                        (do (reset! !v (str server-value)) (t))
                        (let [r (e/server (e/Offload #(settings/save-kg-dedup-threshold
                                                        user-id k parsed)))]
                          (case r
                            (if (:success r)
                              (do (reset! !v (str (:value r))) (t))
                              (t (:error r)))))))))))))))))

(e/defn DedupEnabledToggle
  "Master switch. Off stops the automatic post-distill sweep AND the two manual
   sweeps — kg-dedup/sweep-document! returns zero marks without reading a pair."
  [user-id enabled]
  (e/client
    (let [!on (atom enabled) on (e/watch !on)]
      (dom/div
        (dom/props {:class "field setting-row setting-row--switch"})
        (dom/div
          (dom/props {:class "setting-row__text"})
          (dom/label (dom/props {:class "label" :for "ai-dedup-enabled"})
            (dom/text "Detect duplicate facts"))
          (dom/div (dom/props {:class "hint"})
            (dom/text (str "After each distillation, mark facts that restate one the "
                        "document already has. Marked facts leave the graph, quizzes "
                        "and Anki, and stay reversible under Knowledge → Facts → "
                        "Duplicates."))))
        (dom/div
          (dom/props {:class "setting-row__control"})
          (dom/input
            (dom/props {:type "checkbox" :id "ai-dedup-enabled"
                        :class "switch" :role "switch"})
            (set! (.-checked dom/node) on)
            (let [changed (dom/On "change" #(-> % .-target .-checked) nil)
                  [t ?error] (e/Token changed)]
              (dom/props {:disabled (some? t) :aria-busy (some? t)})
              (when t
                (let [r (e/server (e/Offload #(settings/save-kg-dedup-enabled
                                                user-id changed)))]
                  (case r
                    (if (:success r) (do (reset! !on changed) (t)) (t (:error r)))))))))))))

(e/defn ResetThresholdsButton
  "Delete the three stored thresholds so every read returns the measured
   defaults. Its own control because a user who has moved the numbers has no
   other way back to a set of values that was actually validated."
  [user-id]
  (e/client
    (dom/button
      (dom/props {:class "btn btn-sm" :aria-label "Reset thresholds to defaults"})
      (tooltip/Tooltip! "Restore 0.55 / 0.50 / 0.80")
      (dom/text "Reset to defaults")
      (let [click (dom/On "click" (fn [_] {:id (str (random-uuid))}) nil)
            [t ?error] (e/Token click)]
        (dom/props {:disabled (some? t) :aria-busy (some? t)})
        (when t
          (let [r (e/server (e/Offload #(reset-thresholds!* user-id)))]
            (case r
              (if (:success r) (t) (t (:error r))))))))))

(e/defn DedupSection
  "Settings → AI Features → Advanced → Knowledge graph → Duplicate facts.

   Reads all four values in ONE query (settings/get-kg-dedup-thresholds) and
   passes them down; four separate reads would be four round trips for one panel.

   Watches :settings-refresh so a change that touches values OTHER than the one
   the user just edited lands in the UI — Reset writes all three at once, and each
   field only ever updates its own local atom. A per-field save does NOT bump, so
   the common case still costs no re-render."
  [user-id]
  (e/client
    (let [settings-refresh (e/server (e/watch (us/get-atom user-id :settings-refresh)))
          {:keys [enabled flat-trigram object-trigram containment]}
          (e/server (get-thresholds* settings-refresh user-id))]
      (dom/div
        (dom/props {:class "ai-subgroup"})
        (dom/h5 (dom/props {:class "ai-subgroup__title"}) (dom/text "Duplicate facts"))
        (dom/p
          (dom/props {:class "ai-subgroup__lead"})
          (dom/text (str "Re-distilling a document produces facts that restate ones "
                      "it already has, because the model rewords them. The defaults "
                      "below were fitted against 234 hand-checked fact pairs and "
                      "caught 78% of the duplicates with 90% precision — about one "
                      "mark in ten is wrong, which is why marks are reversible and "
                      "nothing is deleted.")))
        (DedupEnabledToggle user-id enabled)
        (ThresholdField user-id :flat-trigram flat-trigram)
        (ThresholdField user-id :object-trigram object-trigram)
        (ThresholdField user-id :containment containment)
        (dom/div
          (dom/props {:class "field setting-row"})
          (dom/div
            (dom/props {:class "setting-row__text"})
            (dom/div (dom/props {:class "label"}) (dom/text "Reset thresholds"))
            (dom/div (dom/props {:class "hint"})
              (dom/text "Restore the measured defaults: 0.55, 0.50 and 0.80.")))
          (dom/div
            (dom/props {:class "setting-row__control"})
            (ResetThresholdsButton user-id)))))))
