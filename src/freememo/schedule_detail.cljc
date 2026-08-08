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

(defn lever-labels
  "§6.6 — the four levers, in the order the warning names them. I3 does not
   exist and 6.6.5 records why lowering `card_count` was rejected.

   The date lever names the day every CARDED page reaches known, and says so.
   Pages with no cards are outside what moving a date can deliver — they need
   the user to read and card them, which is the pages-per-day figure, not the
   date. Stating the number keeps the promise honest."
  [earliest-day reachable unreachable]
  [(cond
     (and earliest-day (pos? (long (or unreachable 0))))
     (str "Move the target out to day " earliest-day " — that covers the "
       reachable " carded pages. The other " unreachable
       " still need reading and carding.")
     earliest-day
     (str "Move the target out to day " earliest-day)
     :else
     "Move the target out — even two years does not reach it")
   "Raise the daily review capacity"
   "Lower the retention target for this schedule"
   "Exclude pages from carding — they then count as known and untested"])

;; ---------------------------------------------------------------------------
;; Small panels
;; ---------------------------------------------------------------------------

(e/defn StatTile [label value sub]
  (e/client
    (dom/div
      (dom/props {:class "dash-card sched-tile"})
      (dom/div (dom/props {:class "dash-cap"}) (dom/text label))
      (dom/div (dom/props {:class "sched-tile-value"}) (dom/text value))
      (when sub
        (dom/div (dom/props {:class "sched-tile-sub"}) (dom/text sub))))))

(e/defn ProgressRow [forecast]
  (e/client
    (let [prog (e/server (:progress forecast))
          proj (e/server (:projected forecast))
          pace (e/server (:pace forecast))
          today (e/server (:today forecast))]
      (dom/div
        (dom/props {:class "sched-tiles"})
        (StatTile "Known now"
          (str (e/server (:known prog)) " / " (e/server (:total prog)))
          (pct (e/server (:fraction prog))))
        ;; §5.4 — the exposure figure is its own tile. Folding it into "known"
        ;; is what would hide the cost of lever I5.
        (StatTile "Untested"
          (str (e/server (:untested prog)))
          "done, never carded")
        (StatTile "Projected at target"
          (str (e/server (:known proj)) " / " (e/server (:total proj)))
          (pct (e/server (:fraction proj))))
        (StatTile "Pace" (pace-label (e/server (:delta pace)))
          (str "needs " (long (Math/round (double (e/server (:required pace))))) " by now"))
        (StatTile "Today"
          (fc/pages-label (e/server (:pages-today today)))
          (str (approx (e/server (:reviews-today today))) " reviews"))))))

(e/defn FeasibilityPanel [forecast]
  (e/client
    (let [feas (e/server (:feasibility forecast))
          ok? (e/server (:feasible? feas))
          reasons (e/server (vec (:reasons feas)))
          earliest (e/server (:earliest-day feas))
          reachable (e/server (:reachable feas))
          unreachable (e/server (:unreachable feas))
          peak (e/server (:peak-load feas))
          cap (e/server (:capacity feas))]
      (when-not ok?
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
                    :short-of-target (str "The projection reaches "
                                       (e/server (:projected-known feas)) " of "
                                       (e/server (:required feas)) " pages.")
                    :over-capacity (str "Peak day needs " (approx peak)
                                     " reviews against your sustained "
                                     (long (Math/round (double cap))) ".")
                    (str r))))))
          (dom/div (dom/props {:style {:font-weight "600" :margin-bottom "4px"}})
            (dom/text "What can move:"))
          (dom/ul
            (dom/props {:style {:margin "0" :padding-left "20px"}})
            (e/for [l (e/diff-by identity (lever-labels earliest reachable unreachable))]
              (dom/li (dom/text l)))))))))

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
        (dom/props {:class "dash-card" :style {:margin-bottom "14px" :padding "12px 14px"}})
        (dom/div (dom/props {:class "dash-cap"})
          (dom/text "Reviews due — next 14 days (derived, not estimated)"))
        (dom/div
          (dom/props {:style {:display "flex" :align-items "flex-end" :gap "4px"
                              :height "72px" :margin-top "8px"}})
          (e/for [d (e/diff-by :day days)]
            (dom/div
              (dom/props {:style {:flex "1" :display "flex" :flex-direction "column"
                                  :align-items "center" :justify-content "flex-end"
                                  :height "100%"}})
              (tooltip/Tooltip! (str (approx (:n d)) " reviews on day " (:day d)))
              (dom/div
                (dom/props {:style {:width "100%"
                                    :height (str (max 2 (long (* 56 (/ (:n d) maxn)))) "px")
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
      (dom/props {:style {:--order i}
                  :class (when (zero? (:day r)) "sched-today")})
      (dom/td (dom/props {:class "sched-cell sched-dim"})
        (dom/text (if (zero? (:day r)) "today" (str "+" (:day r)))))
      (dom/td (dom/props {:class "sched-cell"})
        (dom/text (str (:date r) " · " (:weekday r))))
      (dom/td (dom/props {:class "sched-cell sched-num"})
        (dom/text (str (:pages r))))
      (dom/td (dom/props {:class "sched-cell sched-num sched-dim"})
        (dom/text (str (:cumulative r))))
      (dom/td (dom/props {:class "sched-cell sched-num"})
        ;; §6.4 — fractional, because V2 weights the branches that produce it.
        (dom/text (if (pos? (:reviews r)) (approx (:reviews r)) "—"))))))

(e/defn PlanTable [forecast reading-days-only?]
  (e/client
    ;; A server-FORM binding: a 730-row plan would otherwise cross the wire
    ;; whole at an e/defn call boundary (CLAUDE.md, 'e/defn Returns Materialize
    ;; at the Call Site').
    ;;
    ;; The filter drops days with no PAGES, not days with no work. Almost every
    ;; day carries some review load, so a "days with work" filter would keep
    ;; every row and promise a narrowing it can never deliver.
    (let [rows (e/server
                 (let [p (vec (:plan forecast))]
                   (if reading-days-only? (filterv #(pos? (:pages %)) p) p)))
          rc (e/server (count rows))]
      (dom/div
        (dom/props {:class "tape-scroll sched-table sched-plan-table"
                    :style {:position "relative" :max-height "440px" :overflow "auto"
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
                               {:overquery-factor 2 :reset-key [reading-days-only?]})]
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
            (dom/text "Nothing scheduled — the target date has passed.")))))))

(e/defn PlanControls [forecast]
  (e/client
    (let [total-pages (e/server (reduce + 0 (map :pages (:plan forecast))))
          days (e/server (count (:plan forecast)))
          reading-days (e/server (count (filter #(pos? (:pages %)) (:plan forecast))))
          reading-days-only?
          (dom/div
            (dom/props {:style {:display "flex" :align-items "baseline" :gap "10px"
                                :margin-bottom "8px" :flex-wrap "wrap"}})
            (dom/div (dom/props {:class "dash-cap"}) (dom/text "Study plan"))
            (dom/span
              (dom/props {:style {:font-size "12px" :color "var(--color-text-secondary)"
                                  :flex "1"}})
              (dom/text (str (fc/pages-label total-pages) " over " days
                          " day" (when (not= 1 days) "s"))))
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
                (dom/text (str "Only reading days (" reading-days ")"))
                checked?)))]
      (PlanTable forecast reading-days-only?))))

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
        (dom/props {:class "tape-scroll sched-table sched-pages-table"
                    :style {:position "relative" :max-height "420px" :overflow "auto"
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
            (dom/text "No pages match this filter.")))))))

(e/defn PagesFilter [forecast]
  (e/client
    (let [!filter (atom "all")
          filter-reason (e/watch !filter)]
      (dom/div
        (dom/props {:style {:display "flex" :align-items "center" :gap "10px"
                            :margin-bottom "8px"}})
        (dom/div (dom/props {:class "dash-cap"}) (dom/text "Pages"))
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

(e/defn PagesSection [forecast]
  (e/client
    ;; Keyed for the reason `PlanSection` states: an unkeyed frame survives a
    ;; schedule change, so the chosen filter carried into the next schedule's
    ;; table and narrowed it before the user asked.
    (e/for-by identity [_k [(e/server (get-in forecast [:schedule :study_schedules/id]))]]
      (PagesFilter forecast))))

;; ---------------------------------------------------------------------------
;; Entry point
;; ---------------------------------------------------------------------------

(e/defn ScheduleForecast [forecast anki-status]
  (e/client
    (let [status (e/server (:status forecast))]
      (StaleBanner forecast anki-status)
      (case status
        :ok (e/amb
              (FeasibilityPanel forecast)
              (ProgressRow forecast)
              ;; The plan comes FIRST among the tables. It is what the user
              ;; opened the tab for; the page verdicts explain it.
              (PlanSection forecast)
              (DailyLoadStrip forecast)
              (PagesSection forecast))
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
              "from done pages alone")
            (StatTile "Untested"
              (str (e/server (get-in forecast [:progress :untested])))
              "done, never carded"))
          (PlanSection forecast))))))
