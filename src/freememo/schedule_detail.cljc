(ns freememo.schedule-detail
  "The forecast panel of the Schedule tab
   (plans/study-scheduler-spec.md §6, §7, §8).

   Renders one schedule's forecast: progress today, projected progress at the
   target, lag or lead in pages, today's plan, the derived daily review load,
   the feasibility warning and its four levers, and the per-page verdict table.

   Split from `freememo.schedule-page` for the reason CLAUDE.md gives under
   'Diagnosing Method code too large!': an Electric `e/defn` is one JVM method
   and the whole tab in one namespace would report its overflow against the
   file's FIRST `e/defn`, naming nothing useful."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.electric-scroll0 :refer [Tape]]
   [freememo.scroll :refer [Scroll-window]]
   [hyperfiddle.electric-forms5 :as forms]
   [freememo.tooltip :as tooltip]
   [freememo.schedule-forecast :as fc]
   [freememo.schedule-anki :as sa]
   [freememo.schedule-anki-tags :refer [ScheduleRangeTagger]]
   [freememo.schedule :as sched]
   [freememo.icons :as icons]))

;; ---------------------------------------------------------------------------
;; Display helpers — plain fns, so each gets its own method for free
;; ---------------------------------------------------------------------------

(defn pct
  "A 0..1 fraction as a whole-percent string."
  [x]
  (str (long (Math/round (* 100.0 (double (or x 0.0))))) "%"))

(defn approx
  "§6.4 — the derived daily load is fractional, because V2 weights the
   branches that produce it. It MUST render as an approximation: a plain `34`
   would read as a count of cards that exist, and no such set of 34 cards does."
  [x]
  (str "≈" (long (Math/round (double (or x 0.0))))))

(defn pace-label
  "§6.2 — lag and lead, in pages. A document exactly on plan reads `on plan`
   rather than `0 pages ahead`, which is the same fact stated as a number
   nobody asked for."
  [delta]
  (let [d (long (Math/round (double (or delta 0.0))))]
    (cond
      (zero? d) "on plan"
      (pos? d) (str (fc/pages-label d) " ahead")
      :else (str (fc/pages-label (- d)) " behind"))))

(defn levers
  "§6.6 — the four levers, in the order the warning names them. I3 does not
   exist and 6.6.5 records why lowering `card_count` was rejected.

   The date lever names the DATE every carded page reaches known, not a day
   index. It used to read 'Move the target out to day 13' while the Plan table
   below it listed real dates, so a reader could not tell day 13 of the month
   from 13 days out, and converting it was arithmetic the surface owed them.

   Pages with no cards are outside what moving a date can deliver — they need
   reading and carding, which is the pages-per-day figure, not the date.
   Stating that number keeps the promise honest.

   **The date lever speaks only when the date is the blocker.** `short?` is
   whether `:short-of-target` fired, and without that branch this lever named a
   date for a document whose real blocker was uncarded pages — the user applied
   it, and the identical suggestion came straight back, because no date moves a
   page the projection cannot see.

   Pre : `earliest-date` is `forecast`'s `:earliest-date`, an ISO string or nil,
         and it is non-nil only when `short?`.
   Pre : `not-started` is `:not-started` — pages neither done nor carded.
   Post: four maps of {:label :apply-date :editable?}, always four and always
         in §6.6's order.
   Inv : `:apply-date` is non-nil ONLY on a lever this panel can apply, and
         `:editable? false` marks a lever no UI can reach yet. A lever that
         claims neither is reachable somewhere else — capacity, through Edit."
  [earliest-date reachable not-started short?]
  [{:label (cond
             (and short? earliest-date (pos? (long (or not-started 0))))
             (str "Move the target to " earliest-date " — that covers the "
               reachable " carded pages. The other " not-started
               " still need reading and carding.")
             (and short? earliest-date)
             (str "Move the target to " earliest-date)
             short?
             "Move the target out — even two years does not reach it"
             :else
             "The target date is not the blocker — leave it where it is")
    :apply-date (when short? earliest-date)
    :editable? true}
   {:label "Raise the daily review capacity — use Edit"
    :apply-date nil
    :editable? true}
   ;; The caveat is required, not editorial. A lower θ makes intervals longer
   ;; and reviews rarer, so the review that first clears 21 days arrives later:
   ;; measured under the default weights, the lead is 8 days at θ 0.90 and 15
   ;; at θ 0.85. A lever advertised as a way out that can make the window
   ;; smaller has to say so.
   {:label (str "Lower the retention target for this schedule — a lower target "
             "can lengthen the maturation lead, not shorten it")
    :apply-date nil
    :editable? false}
   {:label "Exclude pages from carding — they then count as known and untested"
    :apply-date nil
    :editable? false}])

;; ---------------------------------------------------------------------------
;; Small panels
;; ---------------------------------------------------------------------------

;; Pre:  `highlight?` is a predicate over the SAME number `value` renders. A
;;       tile that highlights on one figure and prints another is worse than no
;;       colour at all. Today's tile passes `schedule-forecast/quota-met?`;
;;       every other tile reports an outcome nobody earns in a day and passes
;;       false.
;; Post: one component. A second, near-identical tile would be two methods
;;       against the 64KB cap and two places to keep one layout in step.
(e/defn StatTile [label value sub highlight?]
  (e/client
    (dom/div
      (dom/props {:class (str "dash-card sched-tile" (when highlight? " sched-quota-met"))})
      (dom/div (dom/props {:class "dash-cap"}) (dom/text label))
      (dom/div (dom/props {:class "sched-tile-value"}) (dom/text value))
      (when sub
        (dom/div (dom/props {:class "sched-tile-sub"}) (dom/text sub))))))

(e/defn ProgressRow [forecast]
  (e/client
    (let [prog (e/server (:progress forecast))
          proj (e/server (:projected forecast))
          pace (e/server (:pace forecast))
          today (e/server (:today forecast))
          coverage (e/server (:coverage forecast))]
      (dom/div
        (dom/props {:class "sched-tiles"})
        (StatTile "Known now"
          (str (e/server (:known prog)) " / " (e/server (:total prog)))
          (pct (e/server (:fraction prog))) false)
        ;; Covered sits BETWEEN known and untested on purpose, and its sub-line
        ;; is load-bearing. Untested is a strict subset of covered, so two
        ;; adjacent tiles with the relation unstated invite a reader to add
        ;; them. "marked done" and "done, never carded" share the word and the
        ;; second qualifies it, which states the subset without a sentence.
        (StatTile "Covered"
          (str (e/server (:covered prog)) " / " (e/server (:total prog)))
          "marked done" false)
        ;; §5.4 — the exposure figure is its own tile. Folding it into "known"
        ;; is what would hide the cost of lever I5.
        (StatTile "Untested"
          (str (e/server (:untested prog)))
          "done, never carded" false)
        (StatTile "Projected at target"
          (str (e/server (:known proj)) " / " (e/server (:total proj)))
          (pct (e/server (:fraction proj))) false)
        (StatTile "Pace" (pace-label (e/server (:delta pace)))
          (str "needs " (long (Math/round (double (e/server (:required pace))))) " by now") false)
        (let [owed (e/server (:pages-today today))]
          (StatTile "Today"
            (fc/quota-phrase owed)
            (str (approx (e/server (:reviews-today today))) " reviews")
            (fc/quota-met? owed)))
        ;; Last, beside `Today`, because both are instructions. The five tiles
        ;; before them report retention, which is the outcome. It answers the
        ;; question the plan table could not: when must the reading be done.
        (StatTile "Cover by"
          (str (e/server (:cover-by coverage)))
          (let [d (e/server (:reading-days coverage))]
            (str d " reading day" (when (not= 1 d) "s"))) false)))))

;; Pre:  `lever` is one map from `levers`; `user-id` owns `schedule-id`.
;; Post: a list item. When `:apply-date` is set it carries an Apply control
;;       that writes that date through `sched/update-schedule!` — the SAME fn
;;       the Edit form uses, so the advice and the form cannot format a date
;;       differently. When `:editable?` is false it carries a marker instead.
;; Inv:  its own e/defn, so the token and its error live in the frame that
;;       mounts per lever rather than in FeasibilityPanel's.
(e/defn LeverRow [user-id schedule-id lever]
  (e/client
    (dom/li
      (dom/text (:label lever))
      (when-let [d (:apply-date lever)]
        (dom/button
          (dom/props {:class "btn" :style {:margin-left "8px" :font-size "11px"
                                           :padding "1px 8px"}})
          (dom/text "Apply")
          (let [[t ?error] (e/Token (dom/On "click" identity nil))]
            (when t
              ;; BOTH outcomes spend the token, for the reason ScheduleHeader
              ;; records: spending on success alone leaves a failure with the
              ;; button dead and nothing saying why.
              (let [res (e/server (e/Offload #(sched/update-schedule!
                                                user-id schedule-id
                                                {:target_date d})))]
                (if (e/server (:ok res))
                  (t)
                  (t (e/server (:error res))))))
            (when ?error
              (dom/span
                (dom/props {:style {:color "var(--color-danger, #b91c1c)"
                                    :font-size "11px"}})
                (dom/text (str " " ?error)))))))
      ;; §8 — the panel names four levers and the UI reaches two. Marking the
      ;; other two is what keeps the advice complete without pretending it is
      ;; actionable; dropping them would hide two real ways out.
      (when-not (:editable? lever)
        (dom/span
          (dom/props {:style {:margin-left "6px" :font-size "11px"
                              :color "var(--color-text-hint)"}})
          (dom/text "(not editable yet)"))))))

;; Pre:  `forecast` reports `:past-target` among its feasibility reasons.
;; Post: a REPORT and two ways to close the schedule out, not a warning. A
;;       passed target is not a plan in trouble — it is a plan whose time is up,
;;       and the two readings need different words, different colour and
;;       different actions. `FeasibilityPanel`'s red box says "this target is
;;       out of reach", which is advice about a date that can still be met.
;; Inv : this panel and `FeasibilityPanel` are MUTUALLY EXCLUSIVE. Rendering
;;       both would put a warning about a short horizon directly above a report
;;       that the horizon is over.
;; Inv : its own e/defn. One JVM method each keeps this namespace under the 64KB
;;       prod cap, and the token here lives in the frame that mounts per panel.
(e/defn PastTargetPanel [user-id schedule-id forecast navigate!]
  (e/client
    (let [feas (e/server (:feasibility forecast))
          ;; NEGATIVE by construction — `:past-target` is what put this panel on
          ;; screen. Negated once, here, so every sentence below reads forwards.
          ago (e/server (- (long (or (:days-to-target forecast) 0))))
          known (e/server (:projected-known feas))
          not-started (e/server (:not-started feas))
          covered (e/server (get-in forecast [:progress :covered]))
          total (e/server (get-in forecast [:progress :total]))
          target (e/server (str (get-in forecast [:schedule :study_schedules/target_date])))
          ;; Non-nil only when `:short-of-target` also fired, which is what pays
          ;; for the seventeen extra projections the search costs. When it is
          ;; nil the projection already reaches every carded page, and the
          ;; schedule is finished rather than late.
          finish (e/server (:earliest-date feas))]
      (dom/div
        (dom/props {:class "sched-ended"})
        (dom/div
          (dom/props {:class "sched-ended-head"})
          (icons/Icon :flag :size 16)
          (dom/text "The target date has passed"))
        ;; Three sentences, in the order a reader needs them: when it ended,
        ;; what it reached, and when the outstanding cards mature. The last one
        ;; is the question this panel exists for — a covered page is not a known
        ;; page until its cards clear the retention gate, and nothing on the tab
        ;; said when that happens.
        (dom/div
          (dom/props {:class "sched-ended-line"})
          (dom/text (str "Target was " target " — "
                      ago " day" (when (not= 1 ago) "s") " ago.")))
        ;; ONE denominator, and it is the tiles'. `:reachable` is the smaller
        ;; count the projection can move, and quoting it here would put a second
        ;; `7 of N` on the screen next to `Known now 7 / 23` with nothing saying
        ;; the two Ns mean different things. The unstarted pages are named as a
        ;; remainder instead, which is the same fact without the collision.
        (dom/div
          (dom/props {:class "sched-ended-line"})
          (dom/text (str "It reached " known " of " total
                      " page" (when (not= 1 total) "s") " known, and "
                      covered " of " total " covered."
                      (when (pos? (long (or not-started 0)))
                        (str " " not-started " page"
                          (when (not= 1 not-started) "s")
                          " still have no cards, so no projection can move "
                          (if (= 1 not-started) "it." "them."))))))
        (dom/div
          (dom/props {:class "sched-ended-line"})
          (dom/text (if finish
                      (str "Every carded page reaches the retention target on "
                        finish ". That is when this schedule finishes.")
                      (str "Every carded page in the range is already known. "
                        "This schedule is finished."))))
        (dom/div
          (dom/props {:class "sched-ended-actions"})
          ;; The date lever, as a button rather than a bullet. It is the same
          ;; write `LeverRow` and the Edit form make — `sched/update-schedule!` —
          ;; so the advice and the form cannot format a date differently.
          (when finish
            (dom/button
              (dom/props {:class "btn"})
              (dom/text (str "Move target to " finish))
              (let [[t ?error] (e/Token (dom/On "click" identity nil))]
                (when t
                  ;; BOTH outcomes spend the token, for the reason
                  ;; `ScheduleHeader` records: spending on success alone leaves
                  ;; the button dead on a failure with nothing saying why.
                  (let [res (e/server (e/Offload #(sched/update-schedule!
                                                    user-id schedule-id
                                                    {:target_date finish})))]
                    (if (e/server (:ok res))
                      (t)
                      (t (e/server (:error res))))))
                (when ?error
                  (dom/span
                    (dom/props {:class "sched-ended-error"})
                    (dom/text (str " " ?error)))))))
          ;; Archiving is the only thing in this feature that means finished,
          ;; and it lived in the header alone — a user reading a box that says
          ;; the target has passed had no reason to look up there for it. It
          ;; navigates for the reason the header's copy does: the schedule
          ;; leaves the list behind this pane.
          (dom/button
            (dom/props {:class "btn"})
            (dom/text "Archive this schedule")
            (let [[t ?error] (e/Token (dom/On "click" identity nil))]
              (when t
                (let [res (e/server (e/Offload #(sched/archive-schedule!
                                                  user-id schedule-id)))]
                  (if (e/server (:ok res))
                    (case (navigate! :schedule) (t))
                    (t (e/server (:error res))))))
              (when ?error
                (dom/span
                  (dom/props {:class "sched-ended-error"})
                  (dom/text (str " " ?error)))))))))))

;; Pre:  `forecast`'s feasibility does NOT report `:past-target` — `PastTargetPanel`
;;       owns that case, and this panel returns nothing for it. The two describe
;;       the same arithmetic from opposite sides: a horizon too short to mature
;;       a card, against a horizon that is over.
(e/defn FeasibilityPanel [user-id schedule-id forecast]
  (e/client
    (let [feas (e/server (:feasibility forecast))
          ok? (e/server (:feasible? feas))
          ;; Not folded into `ok?`. A passed target is not a feasible one, and a
          ;; binding that said so would be a lie the next reader has to unpick.
          ;; It is a case another panel owns, which is a different fact.
          past? (e/server (:past-target? feas))
          reasons (e/server (vec (:reasons feas)))
          earliest (e/server (:earliest-date feas))
          reachable (e/server (:reachable feas))
          not-started (e/server (:not-started feas))
          short? (e/server (boolean (some #{:short-of-target} (:reasons feas))))
          peak (e/server (:peak-load feas))
          cap (e/server (:capacity feas))]
      (when-not (or ok? past?)
        (dom/div
          (dom/props {:style {:padding "12px 14px" :margin-bottom "14px"
                              :background "var(--color-warning-bg, #fff3cd)"
                              :border "1px solid var(--color-border)"
                              :border-radius "4px" :font-size "13px"}})
          (dom/div
            (dom/props {:style {:display "flex" :align-items "center" :gap "8px"
                                :font-weight "600" :margin-bottom "6px"}})
            (icons/Icon :triangle-alert :size 16)
            (dom/text "This target is out of reach"))
          (dom/ul
            (dom/props {:style {:margin "0 0 8px 0" :padding-left "20px"}})
            (e/for [r (e/diff-by identity reasons)]
              (dom/li
                (dom/text
                  (case r
                    ;; Against REACHABLE, not the whole range. The projection
                    ;; cannot move a page with no cards, so stating the range
                    ;; here would report a shortfall the date lever beside it
                    ;; cannot close.
                    :short-of-target (str "The projection reaches "
                                       (e/server (:projected-known feas)) " of "
                                       (e/server (:reachable feas))
                                       " carded pages by the target.")
                    ;; `:pages-not-started` is deliberately NOT a case here.
                    ;; Unstarted pages are no longer an infeasibility, so they
                    ;; never reach this list — `NotStartedNote` states them
                    ;; beside the plan that schedules them instead.
                    :over-capacity (str "Peak day needs " (approx peak)
                                     " reviews against your sustained "
                                     (long (Math/round (double cap))) ".")
                    ;; Both numbers from `feas`. The sentence is a statement
                    ;; about the two together, and reading one from the
                    ;; forecast map would let a future refactor drift them.
                    :no-maturation-window (str "Cards need " (e/server (:maturation-lead feas))
                                            " days to mature and the target is "
                                            (e/server (:horizon feas))
                                            " days away, so nothing covered from "
                                            "today can count.")
                    :maturity-unreachable (str "No card reaches the retention target "
                                            "under this deck configuration, so no "
                                            "amount of reading produces a known page.")
                    (str r))))))
          (dom/div (dom/props {:style {:font-weight "600" :margin-bottom "4px"}})
            (dom/text "What can move:"))
          (dom/ul
            (dom/props {:style {:margin "0" :padding-left "20px"}})
            (e/for [l (e/diff-by :label (levers earliest reachable not-started short?))]
              (LeverRow user-id schedule-id l))))))))

(e/defn NotStartedNote [forecast]
  (e/client
    ;; Pages the projection cannot see: neither done nor carded. It states the
    ;; gap WITHOUT the warning box, because those pages are not a blocker — the
    ;; plan under this line is what schedules them, and the reading window ends
    ;; a full maturation lead before the target so they still count in time.
    ;;
    ;; It also explains a figure that otherwise reads as a defect: `Projected at
    ;; target` can never exceed the reachable count, so a partially-read
    ;; document shows a projection short of its own range on purpose.
    (let [n (e/server (get-in forecast [:feasibility :not-started]))]
      (when (pos? (or n 0))
        (dom/div
          (dom/props {:style {:margin "0 0 10px 0" :font-size "12px"
                              :color "var(--color-text-secondary)"}})
          (dom/text (str n " page" (when (not= 1 n) "s")
                      " not started — no cards yet, so the forecast cannot "
                      "project them. The plan below schedules the reading.")))))))

(e/defn StaleBanner [forecast anki-status]
  (e/client
    ;; §8.1 and §8.2 — each cause states which one it is. A single "Anki
    ;; problem" would leave the user with no idea whether to update
    ;; AnkiConnect, turn FSRS on, or start Anki.
    ;;
    ;; The words come from `schedule-anki/acquisition-states`, which is also
    ;; where the chip reads them. Spelling the states out here as well is how a
    ;; new one used to reach the header and silently miss this banner.
    (let [msg (:banner (get sa/acquisition-states anki-status))
          cached-at (e/server (str (:cached-at forecast)))]
      (when msg
        (dom/div
          (dom/props {:style {:padding "10px 14px" :margin-bottom "14px"
                              :background "var(--color-bg-subtle)"
                              :border "1px solid var(--color-border)"
                              :border-radius "4px" :font-size "13px"
                              :display "flex" :align-items "center" :gap "8px"}})
          (icons/Icon :triangle-alert :size 15)
          (dom/text msg)
          (when (and (= anki-status :unreachable) (seq cached-at))
            (dom/span
              (dom/props {:style {:color "var(--color-text-secondary)"}})
              (dom/text (str " Cached " cached-at ".")))))))))

;; ---------------------------------------------------------------------------
;; Daily review load (§6.3 – §6.5)
;; ---------------------------------------------------------------------------

(e/defn DailyLoadStrip [forecast]
  (e/client
    (let [;; The first fortnight is the actionable part of a plan followed by
          ;; hand. The rest of the horizon is in the peak figure above.
          days (e/server
                 (let [load (:daily-load forecast)]
                   (mapv (fn [d] {:day d :n (double (get load d 0.0))}) (range 14))))
          maxn (e/server (max 1.0 (reduce max 0.0 (map :n days))))]
      (dom/div
        ;; Shorter than it was. Every pixel this strip spends is a pixel the
        ;; table below cannot have, and the table is what decides whether the
        ;; pane has to scroll at all — the fourteen bars read the same at 56px.
        (dom/props {:class "dash-card" :style {:margin-bottom "12px" :padding "10px 14px"}})
        (dom/div (dom/props {:class "dash-cap"})
          (dom/text "Reviews due — next 14 days (derived, not estimated)"))
        (dom/div
          (dom/props {:style {:display "flex" :align-items "flex-end" :gap "4px"
                              :height "56px" :margin-top "6px"}})
          (e/for [d (e/diff-by :day days)]
            (dom/div
              (dom/props {:style {:flex "1" :display "flex" :flex-direction "column"
                                  :align-items "center" :justify-content "flex-end"
                                  :height "100%"}})
              (tooltip/Tooltip! (str (approx (:n d)) " reviews on day " (:day d)))
              (dom/div
                (dom/props {:style {:width "100%"
                                    :height (str (max 2 (long (* 42 (/ (:n d) maxn)))) "px")
                                    :background (if (zero? (:day d))
                                                  "var(--color-success)"
                                                  "var(--color-primary-text)")
                                    :opacity (if (pos? (:n d)) "1" "0.35")
                                    :border-radius "2px"}}))
              (dom/div
                (dom/props {:style {:font-size "10px" :margin-top "3px"
                                    :color "var(--color-text-secondary)"}})
                (dom/text (if (zero? (:day d)) "today" (str (:day d))))))))))))

;; ---------------------------------------------------------------------------
;; The study plan (§1.2) — the tab's primary deliverable
;; ---------------------------------------------------------------------------
;;
;; A dated row per day from today to the target. This is the artefact the
;; feature exists to produce: the user follows it by hand, the way they would a
;; training programme, and nothing here writes a queue (§1.4).

(def ^:private plan-row-height 34)

;; `--grid-cols` is NOT set inline on these tables. An inline custom property
;; beats every stylesheet rule short of `!important`, so a media query could
;; not narrow the columns on a phone. The class owns the template; index.css
;; overrides it per viewport and hides the columns that do not fit.

(e/defn PlanRow [r i]
  (e/client
    (dom/tr
      ;; `sched-mature` marks the maturation window. The row is not empty — it
      ;; carries the reviews that make the covered pages count — so it must
      ;; read as a different KIND of day, not as a day with nothing in it.
      (dom/props {:style {:--order i}
                  :class (str (when (zero? (:day r)) "sched-today ")
                           (when (= :mature (:phase r)) "sched-mature ")
                           (when (:deadline? r) "sched-deadline"))})
      (dom/td (dom/props {:class "sched-cell sched-dim"})
        (dom/text (if (zero? (:day r)) "today" (str "+" (:day r)))))
      (dom/td (dom/props {:class "sched-cell"})
        (dom/text (str (:date r) " · " (:weekday r))))
      ;; An em dash, never 0. A maturation day asks for no reading, and a 0
      ;; reads as a day the user failed to fill.
      ;;
      ;; Day 0 is a quota, so it reads `done` or `N ahead` rather than a bare
      ;; number, and turns green once it is settled. Every later day is a plain
      ;; count — `quota-cell` would render their 0 as `done`, which on a future
      ;; day would promise something nobody has earned yet.
      (dom/td (dom/props {:class (str "sched-cell sched-num"
                                   (when (and (zero? (:day r)) (fc/quota-met? (:pages r)))
                                     " sched-quota-met"))})
        (dom/text (cond
                    (= :mature (:phase r)) "—"
                    (zero? (:day r)) (fc/quota-cell (:pages r))
                    :else (str (:pages r)))))
      (dom/td (dom/props {:class "sched-cell sched-num sched-dim"})
        (dom/text (str (:cumulative r))))
      (dom/td (dom/props {:class "sched-cell sched-num"})
        ;; §6.4 — fractional, because V2 weights the branches that produce it.
        (dom/text (if (pos? (:reviews r)) (approx (:reviews r)) "—"))))))

(e/defn PlanTable [forecast covering-days-only?]
  (e/client
    ;; A server-FORM binding: a 730-row plan would otherwise cross the wire
    ;; whole at an e/defn call boundary (CLAUDE.md, 'e/defn Returns Materialize
    ;; at the Call Site').
    ;;
    ;; The filter drops days with no COVERING, not days with no work. Almost
    ;; every day carries some review load, so a "days with work" filter would
    ;; keep every row and promise a narrowing it can never deliver.
    ;;
    ;; `fc/covering-day?`, never a local predicate: the label's count is taken
    ;; with the same fn, and a rounding case makes the two disagree when they
    ;; are written twice. A slow window can put 0 pages on the deadline day,
    ;; and a bare `pos? :pages` would then hide the date the tile names.
    (let [rows (e/server
                 (let [p (vec (:plan forecast))]
                   (if covering-days-only? (filterv fc/covering-day? p) p)))
          rc (e/server (count rows))]
      ;; The scroll box sits INSIDE a bordered card rather than being the card.
      ;; A browser draws a scrollbar between the padding box and the border box,
      ;; so padding on the scroller itself cannot move its bar inward (CLAUDE.md,
      ;; 'Center a Text Column by Padding the Scroller'). Only a wrapper can, and
      ;; the inset is what stops this bar from ending up flush against the
      ;; pane's own — two bars at one edge read as a single control that half
      ;; works.
      (dom/div
        (dom/props {:class "sched-tablecard"})
        (dom/div
          (dom/props {:class "tape-scroll sched-table sched-plan-table"
                      ;; No height here. `.sched-tables` gives the card the
                      ;; pane's leftover height and the card gives it to this
                      ;; scroller, so the window grows with the browser window
                      ;; and the pane never has to scroll behind it.
                      :style {:position "relative" :overflow "auto"
                              :scrollbar-gutter "stable"
                              :--count rc
                              :--row-height (str plan-row-height "px")}})
          (dom/table
            (dom/props {:class "tx-head" :style {:width "100%"}})
            (dom/thead (dom/props {:style {:display "contents"}})
              (dom/tr (dom/props {:style {:display "contents"}})
                (dom/th (dom/props {:class "sched-th"}) (dom/text "Day"))
                (dom/th (dom/props {:class "sched-th"}) (dom/text "Date"))
                (dom/th (dom/props {:class "sched-th sched-num"}) (dom/text "Pages"))
                (dom/th (dom/props {:class "sched-th sched-num"}) (dom/text "Total"))
                (dom/th (dom/props {:class "sched-th sched-num"}) (dom/text "Reviews")))))
          ;; §7.2 — the reset key is the FILTER, never the row count.
          (let [[offset limit] (Scroll-window plan-row-height rc dom/node
                                 {:overquery-factor 2 :reset-key [covering-days-only?]})]
            (dom/table
              (dom/props {:style {:width "100%" :font-size "13px"}})
              (e/for [i (Tape offset limit)]
                (let [r (e/server (nth rows i nil))]
                  (when r (PlanRow r i))))))
          (when (zero? rc)
            (dom/div
              (dom/props {:style {:position "absolute" :inset "0" :display "flex"
                                  :align-items "center" :justify-content "center"
                                  :pointer-events "none" :font-size "13px"
                                  :color "var(--color-text-secondary)"}})
              (dom/text "Nothing scheduled — the target date has passed."))))))))

(e/defn PlanControls [forecast]
  (e/client
    ;; `cover-days` is the WINDOW and `covering-days` is the rows the filter
    ;; leaves. They differ whenever the rate falls below one page a day, and
    ;; conflating them would label the filter with a number it does not leave
    ;; behind. Both counts come from the server: `:mature-days` because the
    ;; server has every input for it, and the filter count through
    ;; `fc/covering-day?` because the filter itself uses that predicate.
    (let [coverage (e/server (:coverage forecast))
          total-pages (e/server (:pages-remaining coverage))
          cover-days (e/server (:reading-days coverage))
          mature-days (e/server (:mature-days coverage))
          covering-days (e/server (count (filter fc/covering-day? (:plan forecast))))
          covering-days-only?
          (dom/div
            (dom/props {:style {:display "flex" :align-items "baseline" :gap "10px"
                                :margin-bottom "8px" :flex-wrap "wrap"}})
            (dom/div (dom/props {:class "dash-cap"}) (dom/text "Study plan"))
            (dom/span
              (dom/props {:style {:font-size "12px" :color "var(--color-text-secondary)"
                                  :flex "1"}})
              (dom/text (str (fc/pages-label total-pages) " over " cover-days
                          " reading day" (when (not= 1 cover-days) "s")
                          (when (pos? mature-days)
                            (str ", then " mature-days " day"
                              (when (not= 1 mature-days) "s")
                              " for the cards to mature")))))
            ;; `forms/Checkbox` — the dataflow checkbox, which RETURNS its
            ;; checked state. A hand-rolled `dom/input` + `dom/On` + atom said
            ;; the same thing through a mutable cell, and the skill's rule is
            ;; that Forms5 owns inputs. No BANG: `Checkbox!` is a tracked form
            ;; field with a token, and this filter commits nothing — it never
            ;; leaves the browser.
            ;;
            ;; No `:label` option either: with one, `Checkbox` returns a
            ;; TWO-element `e/amb` (the input's value and the label), and this
            ;; binding wants the boolean. The surrounding `dom/label` associates
            ;; by containment, which needs no `for`.
            (dom/label
              (dom/props {:style {:display "inline-flex" :align-items "center" :gap "6px"
                                  :font-size "12px" :cursor "pointer"}})
              (let [checked? (forms/Checkbox false)]
                ;; The label states the count it will leave, so the user knows
                ;; before clicking whether the filter narrows anything.
                (dom/text (str "Only covering days (" covering-days ")"))
                checked?)))]
      (PlanTable forecast covering-days-only?))))

(e/defn PlanSection [forecast]
  (e/client
    ;; Keyed on the schedule, so switching schedules is an honest unmount and
    ;; mount. Without the key Electric keeps this frame alive across the
    ;; change, and the filter carries over: arriving at a fresh schedule showed
    ;; it already narrowed to its reading days, with nothing in this view
    ;; saying why. A local atom had the same lifetime and the same bug — the
    ;; state was never the problem, the frame's identity was (CLAUDE.md,
    ;; `video_pane.cljc` keying an unkeyed frame transition).
    (e/for-by identity [_k [(e/server (get-in forecast [:schedule :study_schedules/id]))]]
      (PlanControls forecast))))

;; ---------------------------------------------------------------------------
;; Page verdict table (§7.2)
;; ---------------------------------------------------------------------------

(def ^:private row-height 36)

(e/defn PageRow [v i]
  (e/client
    (dom/tr
      (dom/props {:style {:--order i}})
      (dom/td (dom/props {:class "sched-cell"}) (dom/text (str (:page-number v))))
      (dom/td (dom/props {:class "sched-cell"})
        (dom/span
          (dom/props {:class (str "type-badge sched-badge-"
                               (name (or (:reason v) :untouched)))})
          (dom/text (get fc/reason-labels (:reason v) "—"))))
      (dom/td (dom/props {:class "sched-cell sched-num"})
        (dom/text (if (pos? (:card-count v)) (pct (:retrievability v)) "—")))
      (dom/td (dom/props {:class "sched-cell sched-num"})
        (dom/text (if (pos? (:card-count v))
                    (str (long (Math/round (double (:interval-days v)))) "d")
                    "—")))
      (dom/td (dom/props {:class "sched-cell sched-num"})
        (dom/text (str (:card-count v)))))))

(e/defn PagesTable [forecast filter-reason]
  (e/client
    ;; A server-form binding, NOT an e/defn call: the whole verdict vector
    ;; would otherwise cross the wire at the call boundary (CLAUDE.md,
    ;; 'e/defn Returns Materialize at the Call Site').
    (let [rows (e/server
                 (let [vs (:verdicts forecast)]
                   (if (= "all" filter-reason)
                     (vec vs)
                     (vec (filter #(= (name (:reason %)) filter-reason) vs)))))
          rc (e/server (count rows))]
      (dom/div
        (dom/props {:class "sched-tablecard"})
        (dom/div
          (dom/props {:class "tape-scroll sched-table sched-pages-table"
                      :style {:position "relative" :overflow "auto"
                              :scrollbar-gutter "stable"
                              :--count rc
                              :--row-height (str row-height "px")}})
          (dom/table
            (dom/props {:class "tx-head" :style {:width "100%"}})
            (dom/thead (dom/props {:style {:display "contents"}})
              (dom/tr (dom/props {:style {:display "contents"}})
                (dom/th (dom/props {:class "sched-th"}) (dom/text "Page"))
                (dom/th (dom/props {:class "sched-th"}) (dom/text "Verdict"))
                (dom/th (dom/props {:class "sched-th sched-num"}) (dom/text "Recall"))
                (dom/th (dom/props {:class "sched-th sched-num"}) (dom/text "Interval"))
                (dom/th (dom/props {:class "sched-th sched-num"}) (dom/text "Cards")))))
          ;; §7.2 — the reset key is the FILTER, never the record count. A
          ;; background refresh that changes the row count must not snap a
          ;; mid-scroll reader back to the top.
          (let [[offset limit] (Scroll-window row-height rc dom/node
                                 {:overquery-factor 2 :reset-key [filter-reason]})]
            (dom/table
              (dom/props {:style {:width "100%" :font-size "13px"}})
              (e/for [i (Tape offset limit)]
                (let [v (e/server (nth rows i nil))]
                  (when v (PageRow v i))))))
          ;; The table stays mounted at count 0: tearing it down races the
          ;; cancelled query's teardown and can crash the socket with
          ;; :diff-corruption (CLAUDE.md, 'Never unmount a virtual-scroll table').
          (when (zero? rc)
            (dom/div
              (dom/props {:style {:position "absolute" :inset "0" :display "flex"
                                  :align-items "center" :justify-content "center"
                                  :pointer-events "none" :font-size "13px"
                                  :color "var(--color-text-secondary)"}})
              (dom/text "No pages match this filter."))))))))

(e/defn PagesFilter [forecast]
  (e/client
    (let [!filter (atom "all")
          filter-reason (e/watch !filter)
          horizon (e/server (:horizon-days forecast))
          projected-to (e/server (:projected-to forecast))]
      (dom/div
        ;; `flex-wrap` is required, not tidiness: the caption is now wide enough
        ;; to push the select past 320px, and §12.3.3 holds this pane to no
        ;; horizontal overflow there.
        (dom/props {:style {:display "flex" :align-items "center" :gap "10px"
                            :margin-bottom "8px" :flex-wrap "wrap"}})
        (dom/div (dom/props {:class "dash-cap"}) (dom/text "Pages"))
        ;; The date the rows below describe, stated beside the cap in the shape
        ;; `PlanControls` uses — a plain sentence next to the cap, not inside
        ;; it, because `dash-cap` uppercases and a date reads worse for it.
        ;;
        ;; Every row is the projection at the horizon, never today. A reader who
        ;; took the table for today read four `known` rows against a `Known now`
        ;; tile of `0 / 23`, with nothing on the table to reconcile the two: all
        ;; four were pages whose mean interval crosses 21 days only inside the
        ;; horizon. A horizon of 0 needs the other phrase, because "projected to
        ;; <today>" contradicts itself.
        (dom/span
          (dom/props {:style {:font-size "12px" :color "var(--color-text-secondary)"}})
          (dom/text (if (pos? horizon)
                      (str "verdicts projected to " projected-to ", not today")
                      "verdicts as of today")))
        (dom/select
          (dom/props {:class "input" :style {:max-width "220px"}})
          (dom/option (dom/props {:value "all"}) (dom/text "All pages"))
          (dom/option (dom/props {:value "mature"}) (dom/text "Known"))
          (dom/option (dom/props {:value "immature"}) (dom/text "Recall ok, interval short"))
          (dom/option (dom/props {:value "below-theta"}) (dom/text "Recall low"))
          (dom/option (dom/props {:value "uncarded-done"}) (dom/text "Done, no cards"))
          (dom/option (dom/props {:value "untouched"}) (dom/text "Not started"))
          ;; A local select driving a local atom needs no token: the effect
          ;; never leaves the browser (CLAUDE.md, 'Selection that invokes a
          ;; side effect').
          (dom/On "change" (fn [e] (reset! !filter (-> e .-target .-value))) nil)))
      (PagesTable forecast filter-reason))))

;; `PagesSection` is gone. It was `PagesFilter` under a key, and `TablesSection`
;; below now holds that key for both tables — two frames keying on the same
;; schedule id, one inside the other, is one method against the 64KB prod cap
;; for nothing.

;; Pre:  `label` names the view, `active?` is whether it is the one showing, and
;;       `pick!` is a PLAIN fn — the switch never leaves the browser, so it needs
;;       no token (CLAUDE.md, 'Selection that invokes a side effect').
;; Post: one segment of the plan/pages switch, carrying `aria-pressed` so the
;;       state a screen reader gets is the state the colour shows.
(e/defn TableTab [label count-text active? pick!]
  (e/client
    (dom/button
      (dom/props {:type "button"
                  :class (str "sched-tab" (when active? " is-active"))
                  :aria-pressed (str (boolean active?))})
      (dom/text label)
      (dom/span (dom/props {:class "sched-tab-count"}) (dom/text count-text))
      (dom/On "click" pick! nil))))

;; Pre:  `forecast` has `:status :ok`, so both tables have something to render.
;; Post: the plan and the page verdicts share ONE region, one at a time.
;;
;; Two tall scroll boxes stacked made the pane about 1250px of content in a
;; window that is rarely that tall, so the pane scrolled — and its bar landed
;; beside whichever table's bar the reader was using. Prior art on inline scroll
;; areas (Baymard) is that a scroll pane inside a scroll pane costs the reader
;; real effort; the cheapest way to keep one is to stop needing two. With one
;; table mounted the pane's own content fits an ordinary desktop window and its
;; scrollbar does not appear at all.
;;
;; Plan is the default because it is the artefact the tab exists to produce.
;; The page verdicts explain it, and that is a second question.
;;
;; Inv : keyed on the schedule, so opening another one starts at Plan rather
;;       than inheriting a view the reader chose for a different document — the
;;       same reason `PlanSection` keys its own. An unkeyed frame survives a
;;       schedule change, and the filter inside `PagesFilter` carried over that
;;       way before it was keyed: a fresh schedule arrived already narrowed,
;;       with nothing on screen saying why.
(e/defn TablesSection [forecast]
  (e/client
    (e/for-by identity [_k [(e/server (get-in forecast [:schedule :study_schedules/id]))]]
      (let [!view (atom :plan)
            view (e/watch !view)
            plan? (= :plan view)
            plan-days (e/server (count (:plan forecast)))
            page-count (e/server (count (:verdicts forecast)))]
        ;; ONE wrapper, and it is what makes the pane stop scrolling. The pane is
        ;; a flex column whose other children size to their content; this element
        ;; takes whatever height is left, and the table inside it is sized to
        ;; that rather than to a fixed 440px. So the pane's own content always
        ;; fits and only the table scrolls.
        ;;
        ;; The table growing with the window is the second half of the prior art
        ;; here: an inline scroll area is easier to live with the more rows it
        ;; shows before it needs a bar at all.
        (dom/div
          (dom/props {:class "sched-tables"})
          ;; The counts sit on the tabs, not behind them. Hiding a table behind a
          ;; switch is only safe if the switch says what it is hiding.
          (dom/div
            (dom/props {:class "sched-tabs" :role "group"
                        :aria-label "Forecast tables"})
            (TableTab "Study plan" (str plan-days " day"
                                     (when (not= 1 plan-days) "s"))
              plan? (fn [_] (reset! !view :plan)))
            (TableTab "Pages" (str page-count) (not plan?)
              (fn [_] (reset! !view :pages))))
          (if plan?
            (PlanControls forecast)
            (PagesFilter forecast)))))))

;; ---------------------------------------------------------------------------
;; Entry point
;; ---------------------------------------------------------------------------

(e/defn ScheduleForecast [user-id schedule-id forecast anki-status navigate!]
  (e/client
    (let [status (e/server (:status forecast))
          ;; The two panels below are mutually exclusive, and the test lives
          ;; HERE so a reader sees that at the call site rather than having to
          ;; open both components to find out.
          past? (e/server (boolean (get-in forecast [:feasibility :past-target?])))]
      (StaleBanner forecast anki-status)
      (case status
        :ok (e/amb
              (if past?
                (PastTargetPanel user-id schedule-id forecast navigate!)
                (FeasibilityPanel user-id schedule-id forecast))
              (ProgressRow forecast)
              ;; Between the tiles and the plan, because it explains the
              ;; `Projected at target` tile above it and points at the table
              ;; below it.
              (NotStartedNote forecast)
              ;; Above the tables rather than between them. It is a fortnight
              ;; summary that belongs with the tiles, and it used to separate
              ;; the two tall scroll boxes that are now one switched region.
              (DailyLoadStrip forecast)
              (ScheduleRangeTagger user-id schedule-id)
              ;; Plan and Pages, one at a time. `PlanSection` survives for the
              ;; degenerate branch below, which has a page plan and no verdicts
              ;; at all and so has nothing to switch between.
              (TablesSection forecast))
        ;; §8.3 — a document with no pushed cards still renders its done-page
        ;; count, with the whole known figure inside the untested one. It also
        ;; still gets a page plan: Anki governs the review column only, and a
        ;; user with no cards yet has the most reading to schedule.
        (e/amb
          (dom/div
            (dom/props {:class "sched-tiles"})
            (StatTile "Known now"
              (str (e/server (get-in forecast [:progress :known])) " / "
                (e/server (get-in forecast [:progress :total])))
              "from done pages alone" false)
            (StatTile "Untested"
              (str (e/server (get-in forecast [:progress :untested])))
              "done, never carded" false))
          (PlanSection forecast))))))
