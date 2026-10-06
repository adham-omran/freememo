(ns freememo.schedule-forecast
  "The page and document model of the Study Scheduler
   (plans/study-scheduler-spec.md §5 and §6).

   `freememo.schedule-fsrs` answers a question about ONE card. This namespace
   answers the questions the tab actually asks: is this page known, how many
   pages of the range are known, how many will be known on the target date,
   how many reviews does each day carry, and is the target reachable at all.

   Pure by the same contract as `schedule-fsrs`: no Electric, no I/O, no clock.
   Every time value is a day index counted from now, so day 0 is today.

   The definition of KNOWN, in one place, because everything here rests on it
   (§5.2, §5.8.1). A page is known when EITHER

     - it is marked done and carries no cards (§5.3 — 28% of done pages in
       production are like this: a title page, a table of contents, or a page
       the user already knew), or
     - the mean R over its Anki cards is at or above θ, AND the mean projected
       interval over those same cards is at least 21 days (§5.8.1).

   The second clause is the maturity gate, and it is what closes the cram
   hole: reviewing a page's cards the day before the target clears θ for every
   rating but Again, and reaches no interval near 21 days (§6.8)."
  (:require [freememo.schedule-fsrs :as sf]))

;; ---------------------------------------------------------------------------
;; Backlog scheduling
;; ---------------------------------------------------------------------------

(defn assign-first-review-days
  "The day each card gets its FIRST projected review.

   A card already due, or never introduced, is backlog: it cannot all happen
   today, so it is spread forward at `capacity` cards per day in due order —
   the most overdue first, then the never-introduced. A card due in the future
   keeps its own due day, because §4.4 assumes reviews happen when FSRS asks
   for them.

   This is the one place lever I2 bites (§6.6.2.1). Raising capacity drains the
   backlog sooner, which moves every downstream review sooner, which moves R.
   A card with no backlog is unaffected, which is what 6.6.2.1 records.

   Pre : `cards` is a seq of maps carrying `:due-day` (a day index, or nil for
         a card Anki has never introduced). `capacity` is a positive int.
   Post: the same maps in the same order, each with `:next-review-day` set to a
         day index >= 0. Cards are never dropped and never reordered.
   Invariant: at most `capacity` cards receive any one backlog day. Cards
         already scheduled in the future are NOT counted against capacity —
         they are not backlog, and counting them would push today's real work
         behind work the user has not reached yet."
  [cards capacity]
  (let [cap (max 1 (long capacity))
        backlog? (fn [c] (let [d (:due-day c)] (or (nil? d) (<= (long d) 0))))
        ;; nil sorts last: an overdue card is work the user already owes, and a
        ;; never-introduced card is work they have not started.
        order (->> (keep-indexed (fn [i c] (when (backlog? c) [i (:due-day c)])) cards)
                (sort-by (fn [[i d]] [(if (nil? d) 1 0) (or d 0) i]))
                (map first))
        day-of (into {} (map-indexed (fn [n i] [i (quot n cap)]) order))]
    (into []
      (map-indexed (fn [i c]
                     (assoc c :next-review-day
                       (if (backlog? c) (get day-of i) (long (:due-day c)))))
        cards))))

;; ---------------------------------------------------------------------------
;; Per-card projection over a whole document
;; ---------------------------------------------------------------------------

(defn project-cards
  "Project every card to `horizon-days`.

   Pre : `cards` carry the `project-card` shape plus `:page-id` and
         `:next-review-day` (see `assign-first-review-days`).
   Post: the same cards, each with `:projected` — the `project-card` result.
   Invariant: one `project-card` call per card, and no card is skipped. A
         never-reviewed card with no planned review still gets a projection, so
         its R of 0 and interval of 0 reach the page mean rather than being
         silently absent from it."
  [scheduler cards opts]
  (mapv #(assoc % :projected (sf/project-card scheduler % opts)) cards))

;; ---------------------------------------------------------------------------
;; Page model (§5)
;; ---------------------------------------------------------------------------

(defn- mean [xs]
  (if (seq xs) (/ (reduce + 0.0 xs) (count xs)) 0.0))

(defn carded-verdict
  "Which gate a CARDED page's means clear, as a `page-verdict` reason (§5.8.1).

   The single statement of the known gate. `page-verdict` reads it for a page
   and `maturation-lead` reads it for one probe card, so the two cannot drift
   into different definitions of known — which they would, because the lead
   exists only to answer 'how long until this gate opens'.

   Pre : `r` and `ivl` are the MEANS over the page's cards, and `theta` is the
         schedule's θ. A caller with no cards has no verdict here — zero cards
         is `page-verdict`'s own case, and passing a mean over nothing would
         read as `:below-theta` rather than as untouched.
   Post: `:below-theta`, `:immature` or `:mature`, never nil.
   Inv : θ is tested before the interval, so a page that fails both is named by
         the gate the user can act on first."
  [r ivl theta]
  (cond
    (< r theta) :below-theta
    (< ivl sf/maturity-interval-days) :immature
    :else :mature))

(defn page-verdict
  "Whether one page is known, and why (§5.1 – §5.5, §5.8).

   Pre : `page` is {:page-id :done? bool}. `cards` are that page's projected
         cards, possibly empty.
   Post: {:page-id :known? :reason :retrievability :interval-days :card-count}.
         `:reason` is one of
           :uncarded-done  — done with no cards, known and UNTESTED (§5.3, §5.4)
           :mature         — mean R >= θ and mean interval >= 21 days (§5.8.1)
           :immature       — mean R >= θ but the interval gate fails
           :below-theta    — mean R < θ
           :untouched      — neither done nor carded (§5.5)
   Post: `:done?` is the page's own mark, carried through untouched. It is NOT
         a verdict — a done page WITH cards is judged on the cards alone, so
         the mark reaches `:reason` only through `:uncarded-done`. It is
         reported anyway because `document-progress` counts it, and because a
         reader who marked a page done and read `not known` has no other way to
         see that the mark registered.
   Invariant: a page is never known on R alone. `:immature` exists to name the
         case the maturity gate rejects, so the tab can say which gate failed
         rather than reporting a bare no."
  [page cards theta]
  (let [n (count cards)
        r (mean (map #(get-in % [:projected :retrievability] 0.0) cards))
        ivl (mean (map #(get-in % [:projected :interval-days] 0.0) cards))
        reason (cond
                 (and (zero? n) (:done? page)) :uncarded-done
                 (zero? n) :untouched
                 :else (carded-verdict r ivl theta))]
    {:page-id (:page-id page)
     :page-number (:page-number page)
     :known? (contains? #{:uncarded-done :mature} reason)
     :done? (boolean (:done? page))
     ;; Carried through like `:done?`, for the same reason: `document-progress`
     ;; counts it and nothing else can recover it from a verdict.
     :done-today? (boolean (:done-today? page))
     :reason reason
     :retrievability (if (pos? n) r 0.0)
     :interval-days (if (pos? n) ivl 0.0)
     :card-count n}))

(def reason-labels
  "One label per `page-verdict` reason, beside the fn that produces them.

   Here rather than in the view, so the vocabulary and its words cannot drift:
   a reason added to `page-verdict`'s `cond` without a label rendered as an em
   dash, with nothing failing. `schedule_fsrs_test` asserts the two key sets
   are equal.

   `:immature` and `:below-theta` are named apart on purpose — the user can act
   on which gate failed, and 'not known' collapses two different pieces of
   advice into one."
  {:uncarded-done "done, no cards"
   :mature "known"
   :immature "recall ok, interval short"
   :below-theta "recall low"
   :untouched "not started"})

(defn page-verdicts
  "`page-verdict` over every page of the selected range.

   Pre : `pages` is the range's pages, `projected-cards` carry `:page-id`.
   Post: one verdict per page, in `pages` order. A page absent from
         `projected-cards` is carded-zero, which `page-verdict` reads as
         `:uncarded-done` or `:untouched` by its done flag — never as a page
         that vanishes from the denominator (§5.5)."
  [pages projected-cards theta]
  (let [by-page (group-by :page-id projected-cards)]
    (mapv #(page-verdict % (get by-page (:page-id %) []) theta) pages)))

(defn document-progress
  "Known, covered and untested counts over a set of verdicts (§5.4, §5.6).

   Post: {:total :known :covered :untested :unknown :fraction}. `:untested` is
         the known-because-uncarded count, reported SEPARATELY rather than
         folded into `:known` — §5.4 requires the exposure to stay visible, and
         §6.6.4.1 requires lever I5 to move it. `:fraction` is known over total,
         and 0.0 for an empty range rather than a division by zero.
   Post: `:covered` is the pages the reader has MARKED DONE, which is a
         different question from `:known` and answerable with no Anki at all.
         Without it a page marked done whose cards are young lands in no figure
         on the tab: it is not `:known` (the maturity gate rejects it) and not
         `:untested` (that counts done-and-uncarded only), so the reading work
         reads as zero everywhere.
   Post: `:covered-today` is the pages marked done TODAY, the one fact the
         daily quota needs and the only one no query over `status` can recover.
   Inv : `:untested` <= `:covered` <= `:total`, and `:covered-today` <=
         `:covered`. Done-and-uncarded is a strict subset of done, today is a
         subset of ever, and every verdict comes from one page of the range.
         A violation is a bug in `page-verdict`, not here."
  [verdicts]
  (let [total (count verdicts)
        known (count (filter :known? verdicts))
        covered (count (filter :done? verdicts))
        covered-today (count (filter :done-today? verdicts))
        untested (count (filter #(= :uncarded-done (:reason %)) verdicts))]
    {:total total
     :known known
     :covered covered
     :covered-today covered-today
     :untested untested
     :unknown (- total known)
     :fraction (if (pos? total) (/ (double known) total) 0.0)}))

;; ---------------------------------------------------------------------------
;; Daily review load (§6.3 – §6.5)
;; ---------------------------------------------------------------------------

(defn daily-review-load
  "Expected reviews per day, DERIVED from the projection (§6.3).

   Never estimated from a rate: every card's projection already names the days
   it expects a review on, weighted by the probability of the branch that
   reaches that day. This sums them.

   Post: a sorted map {day-index expected-count}, with fractional counts —
         V2 weights branches, so the count is an expectation. §6.4 requires the
         surface to render it as an approximation for that reason.
   Invariant: the sum over days equals the total projected reviews, which is
         what §6.3's done-condition asserts."
  [projected-cards]
  (into (sorted-map)
    (apply merge-with +
      {} (map #(get-in % [:projected :daily-reviews] {}) projected-cards))))

(defn combined-daily-load
  "One daily review number across several schedules (§1.3, §6.5).

   Concurrent schedules share one pair of hands, so their loads add. Passing
   one schedule's load is the degenerate case and returns it unchanged."
  [loads]
  (into (sorted-map) (apply merge-with + {} loads)))

(defn peak-daily-load
  "The largest single-day expected load, or 0.0 for an empty plan.
   The figure §6.7's feasibility test compares against the user's observed
   review rate, and the figure lever I2 must clear."
  [load]
  (if (seq load) (reduce max 0.0 (vals load)) 0.0))

;; ---------------------------------------------------------------------------
;; Lag and lead (§6.2)
;; ---------------------------------------------------------------------------

(defn pace
  "Lag or lead, in COVERED pages (§6.2).

   The figure is covered-now minus required-now, where required-now walks a
   straight line from the covered count at the schedule's coverage baseline to
   the whole range at its COVERAGE DEADLINE. Negative is behind, positive is
   ahead, and a document exactly on plan reads 0.

   The unit is coverage, and both endpoints move with it. Measuring a lag in
   known pages against a plan that asks for covered pages is what made a user
   who worked the table read `Behind` for the whole first stretch: known trails
   coverage by the maturation lead, by construction. `plans/schedule-pace-pill.md`
   §6.2.2 names that as its own falsifier.

   Pre : `elapsed-days` and `total-days` are day counts over the COVERAGE
         window, not the schedule's whole span. `total-days` >= 0.
   Pre : `covered-now` and `start-covered` are both done-mark counts. Mixing a
         baseline in one unit with a reading in another is the defect `pace-of`
         records against `start_known`.
   Post: {:required :actual :delta}. Before the baseline and after the coverage
         deadline the line is clamped to its endpoints, so a schedule opened
         after its deadline reports the whole range as required rather than a
         figure past 100%."
  [{:keys [covered-now total-pages start-covered elapsed-days total-days]}]
  (let [frac (cond
               (<= (long total-days) 0) 1.0
               :else (min 1.0 (max 0.0 (/ (double elapsed-days) (double total-days)))))
        required (+ (double start-covered) (* frac (- (double total-pages) (double start-covered))))]
    {:required required
     :actual (double covered-now)
     :delta (- (double covered-now) required)}))

(def ^:const pace-dead-zone-pages
  "How far off the line still reads as on plan, in pages.

   A dead zone is FORCED, not a preference. `pace`'s `:actual` is an integer
   page count while its `:required` is fractional, so the delta is almost never
   exactly 0. A zero-width threshold would therefore make `:on-plan`
   unreachable, and the state would flip between `:behind` and `:ahead`
   forever. It would also contradict `daily-page-plan`'s own invariant, which
   promises that a user who works the dated table never reports a lag."
  1.0)

(defn pace-state
  "`pace`'s `:delta` as one of three states.

   Pre : `delta` is `pace`'s `:delta` — pages ahead of the line (positive) or
         behind it (negative).
   Post: `:behind`, `:on-plan` or `:ahead`, never nil.
   Inv : the zone is symmetric and CLOSED, so a delta of exactly
         `pace-dead-zone-pages` either way still reads `:on-plan`."
  [delta]
  (let [d (double delta)]
    (cond
      (< d (- pace-dead-zone-pages)) :behind
      (> d pace-dead-zone-pages) :ahead
      :else :on-plan)))

;; ---------------------------------------------------------------------------
;; Feasibility (§6.6, §6.7)
;; ---------------------------------------------------------------------------

(defn earliest-feasible-day
  "The first day index at which the projection reaches `target-pages` known
   pages, or nil when it does not reach it inside `search-days` (§6.6.1 — the
   date lever I1 names).

   Pre : `project-at` is (fn [day] known-page-count). It MUST be monotonic in
         its argument for the search to mean anything, which §6.1 requires of
         the projection.
   Post: the smallest day in [0, search-days] whose count meets the target, or
         nil. A doubling probe followed by a binary search, so a 365-day horizon
         costs about 17 projections rather than 365."
  [project-at target-pages search-days]
  (let [limit (long search-days)
        meets? (fn [d] (>= (long (project-at d)) (long target-pages)))]
    (when (meets? limit)
      (loop [lo 0, hi limit]
        (if (>= lo hi)
          lo
          (let [mid (quot (+ lo hi) 2)]
            (if (meets? mid) (recur lo mid) (recur (inc mid) hi))))))))

(defn feasibility
  "Whether the target is reachable, and what to do when it is not (§6.6, §6.7).

   Two independent ways a target fails, and the warning names both when both
   hold:

     :short-of-target  — the projection does not reach the REACHABLE pages by
                         the target date. A date can fix this.
   Pages that are neither done nor carded are NOT a reason. `:not-started`
   reports them and `:feasible?` ignores them — see the note below.
     :over-capacity    — some day's derived load exceeds the review rate the
                         user has actually sustained (§6.7).
     :no-maturation-window — the horizon is at or under the maturation lead, so
                         a page covered today cannot clear the gate in time.
                         Every page of the plan lands on day 0 and none of them
                         can count.
     :maturity-unreachable — `maturation-lead` found no day at all. The gate
                         never opens under this deck config and θ, so no amount
                         of reading produces a known page.
     :past-target      — the target date has gone by. The schedule has run its
                         course, and what is left is a report and a decision,
                         not advice about a plan that can still be met.

   Post: {:feasible? :reasons :peak-load :capacity :projected-known :required
          :horizon :maturation-lead :levers}. `:horizon` is echoed back beside
          the lead because `:no-maturation-window` is a statement about the two
          TOGETHER. A surface that took one from here and the other from the
          forecast map would be reading one sentence out of two sources.
          `:levers` is always the four of §6.6 —
          I1 date, I2 capacity, I4 θ, I5 exclude pages — because the warning
          MUST name all four whichever reason fired. I3 does not exist and
          6.6.5 records why lower `card_count` was rejected.
   Pre : `:maturation-lead` and `:horizon` are BOTH present. A nil lead is a
         legitimate value meaning the gate is unreachable, so an absent key and
         an unreachable gate are indistinguishable here and both raise
         `:maturity-unreachable`. A caller that forgets the key gets a warning
         about the deck config, which is the loudest safe reading of it.
   Pre : `:reachable-pages` is the pages the PROJECTION can move — the ones
         that are done or carded. It is a separate input from `:required-pages`
         and the two differ for every partially-read document. It DEFAULTS to
         `:required-pages`, which reads as 'every page is reachable' — the
         right answer for a caller holding no projection to consult, and the
         one that keeps `:pages-not-started` silent rather than firing over a
         set it cannot measure.
   Post: `:maturation-lead` is reported so the panel names the number of days
         rather than a bare warning. A user told 'no maturation window' with no
         figure cannot tell whether to move the date by a day or by a month.
   Invariant: a feasible plan reports no reasons, and an infeasible one reports
         at least one. The two can never both be empty.

   **`:short-of-target` is measured against the REACHABLE pages, not the whole
   range, and that is what makes the date lever's promise keepable.** The
   projection models cards that exist in Anki; a page with no cards is
   `:untouched` at every horizon, so a test against the whole range is one the
   projection can never pass. It fired permanently on every partially-carded
   document, while `earliest-feasible-day` searched against the reachable count
   — so the lever named a date, the user applied it, and the same warning came
   back naming the same date. §6.6.1 promises that accepting the date clears
   the warning, and against the whole range it could not.

   **Pages with no cards are NOT an infeasibility, and counting them as one made
   the panel contradict itself.** It rendered `This target is out of reach`
   directly above `the plan below schedules the reading that does` — two claims
   that cannot both hold. Three reasons the plan is the one telling the truth:

     - `plan-days` has already ALLOCATED those pages. The reading window ends
       `lead` days before the target, so a page covered on the last reading day
       still gets its full maturation period. A reader who works the table
       delivers the whole range on time, which is the plan's own invariant.
     - This fn has no reading-rate input. `daily_capacity` bounds REVIEWS, and
       nothing here can judge whether four pages a day is achievable — so
       infeasible is a verdict it is not equipped to reach. Unstarted pages are
       the absence of a projection, not a projection of failure.
     - The case where they genuinely cannot make it is a horizon at or inside
       the lead, and `:no-maturation-window` already says exactly that.

   `:not-started` is therefore reported and never counted. §6.6's done-condition
   still holds: a one-day target on an uncarded document warns, through
   `:no-maturation-window`.

   **`:past-target` SUPPRESSES `:no-maturation-window`, and that suppression is
   the point of the reason.** `days-until` floors at 0, so a target three weeks
   gone arrives here with `horizon` 0 — indistinguishable from a target today,
   and every bit as short of the lead. The window reason then fired and rendered
   `Cards need 11 days to mature and the target is 0 days away`, a sentence
   about a horizon that is short when the horizon is in fact over. Both describe
   the same arithmetic; only one of them is the fact the reader needs.

   `:short-of-target` is deliberately KEPT alongside it. A passed target that
   the projection did not reach is a real shortfall, and it is what makes
   `earliest-feasible-day` run — which is the date the panel needs in order to
   say when the covered pages become known.

   Pre : `past-target?` is whether `schedule/signed-days-to-target` is negative.
         It cannot be derived here: `horizon` has already been floored, and
         floored is exactly what loses the distinction."
  [{:keys [projected-known required-pages reachable-pages peak-load
           observed-capacity earliest-day horizon past-target?]
    lead :maturation-lead}]
  (let [reachable (long (or reachable-pages required-pages))
        past? (boolean past-target?)
        short? (< (long projected-known) reachable)
        over? (and (pos? (double observed-capacity))
                (> (double peak-load) (double observed-capacity)))
        unreachable? (nil? lead)
        no-window? (and (not past?) (some? lead) (<= (long horizon) (long lead)))
        reasons (cond-> []
                  past? (conj :past-target)
                  short? (conj :short-of-target)
                  over? (conj :over-capacity)
                  no-window? (conj :no-maturation-window)
                  unreachable? (conj :maturity-unreachable))]
    {:feasible? (empty? reasons)
     :reasons reasons
     :past-target? past?
     :peak-load (double peak-load)
     :capacity (double observed-capacity)
     :projected-known (long projected-known)
     :required (long required-pages)
     :reachable reachable
     :not-started (- (long required-pages) reachable)
     :horizon (long horizon)
     :maturation-lead lead
     :earliest-day earliest-day
     :levers [:push-target-date :raise-capacity :lower-theta :exclude-pages]}))

;; ---------------------------------------------------------------------------
;; The maturation lead
;; ---------------------------------------------------------------------------
;;
;; How many days pass between covering a page and the day its cards can count.
;; The plan stops the reading work this many days before the target, because a
;; page covered later cannot clear the gate whatever the user does.

(def probe-card
  "A card introduced today: never reviewed, first review on day 0.

   The unit `maturation-lead` measures. It is the best case a covered page can
   produce — the user reads the page, cards it, pushes it and reviews it the
   same day — so the lead it yields is a floor. `assign-first-review-days`
   spreads a real push at `daily_capacity` per day, which can only move the
   first review later."
  {:state 1 :step 0 :stability nil :difficulty nil
   :interval-days 0.0 :last-review-day nil :next-review-day 0})

(defn maturation-lead
  "Days from a card's first review to the day it clears the known gate.

   Pre : `scheduler` is the SCHEDULE's own scheduler, built from its deck
         config and its I4 θ override, and `theta` is that same θ. A lead
         measured against defaults describes a different user.
   Pre : `search-days` >= 0. The caller owns the bound, so this namespace names
         no horizon constant.
   Post: the first day in [0, search-days] on which `probe-card` reads
         `:mature`, or nil when it reads `:mature` on no day in that range.
   Post: nil means the gate is unreachable under this configuration, NOT that
         the lead is zero. A caller that reads nil as zero would plan reading
         work up to the target date and promise a target the cards cannot meet.

   A LINEAR scan, and it must stay one. The gate is not monotonic in the
   horizon: R decays between reviews, so a card that clears θ on one day can
   fail it on a later one. Measured at θ 0.97 under the default weights, the
   probe clears the gate on day 49 and fails it again on 65 later days inside
   120. A binary search over a predicate that flips returns an arbitrary day.

   The cost is `lead + 1` projections of ONE card. Measured at θ 0.95, the
   worst normal case, that is 32 projections against the whole-document
   projection the forecast already runs."
  [scheduler theta search-days]
  (let [limit (long search-days)]
    (loop [d 0]
      (when (<= d limit)
        (let [p (sf/project-card scheduler probe-card {:horizon-days d})]
          (if (= :mature (carded-verdict (:retrievability p) (:interval-days p) theta))
            d
            (recur (inc d))))))))

;; ---------------------------------------------------------------------------
;; Today's plan (§1.2)
;; ---------------------------------------------------------------------------

(defn daily-page-plan
  "Pages to cover on each day of the READING window (§1.2).

   The window ends at the coverage deadline, not at the target — `plan-days`
   owns that split and this fn owns the split of pages across the days it is
   given.

   The distribution follows the SAME straight line §6.2's pace figure compares
   against, so a user who works the table reads `on plan` every day. Deriving
   it any other way would let the plan and the verdict disagree, and the user
   would have no way to tell which one to believe.

   Day d's page count is the difference between the line's cumulative target at
   d and at d−1, so the counts are whole numbers that sum EXACTLY to
   `pages-remaining`. A rate below one page per day therefore produces days of
   0 and days of 1 rather than a fraction nobody can act on.

   Pre : `pages-remaining` >= 0. `days` >= 0 — a window that has already closed
         collapses to one day, because the work is still owed.
   Post: a vector of `max(1, days)` non-negative longs whose sum is exactly
         `pages-remaining`.
   Inv : cumulative pages through day d never exceed the line, so following
         this plan never reports a lag."
  [pages-remaining days]
  (let [n (max 1 (long days))
        total (max 0 (long pages-remaining))
        cum (fn [d] (long (Math/round (/ (* (double total) (inc (long d))) (double n)))))]
    (mapv (fn [d] (- (cum d) (if (zero? d) 0 (cum (dec d))))) (range n))))

(defn coverage-window
  "How many days the plan may put pages on, given a horizon and a lead.

   Pre : `horizon` >= 0. `lead` is `maturation-lead`'s day count, or nil when
         the gate is unreachable.
   Post: >= 1 always. A horizon at or under the lead leaves no window, and the
         remainder still lands on day 0 — the reading work is owed whether or
         not it can mature in time, and `feasibility` is what says so.
   Post: nil `lead` yields the whole horizon. A user whose configuration cannot
         mature a card can still read, and the warning carries the bad news."
  [horizon lead]
  (max 1 (- (long horizon) (long (or lead 0)))))

(defn plan-days
  "Pages and phase for every day from today to the horizon (§1.2).

   The plan runs to the TARGET while the pages stop at the coverage deadline.
   Both halves are load-bearing. Stopping the table early would hide the review
   work the maturation window carries, and running the pages to the target
   would ask for reading that cannot count.

   Pre : `pages-remaining` >= 0. `days` is `coverage-window`'s count, >= 1.
         `horizon` >= 0.
   **Day 0 is a QUOTA, the rest are a rate.** Day 0 reads what is still owed
   today: the allocation set at the START of the day, minus what has been
   covered since. It falls to 0 as the reader works and goes NEGATIVE past it,
   which is the whole point — the old cell divided the remainder by the days
   left on every read, so doing three pages moved it from 3 to 2 and `done for
   today` was unreachable (plans/study-plan-daily-quota.md §1).

   Days 1..n stay a rate: the remainder after today's owed pages, spread over
   the days left. `covered-today` therefore shrinks day 0 one-for-one and
   leaves the others alone.

   Pre : `covered-today` counts pages of this range marked done TODAY, and is
         already included in the coverage that produced `pages-remaining`.
         Passing a count from another day inflates the allocation.
   Post: a vector of {:day :pages :phase :deadline?} of length
         `max(days, max(1, horizon))`. `:phase` is `:cover` for the first
         `days` entries and `:mature` after.
   Post: `:day` is carried so `covering-day?` reads the same answer here as it
         does from a `schedule/study-plan` row. Without it the predicate
         behaved differently for one logical day depending on which shape it
         was handed.
   Post: `:pages` on day 0 may be negative. Every other day is >= 0.
   Inv : `max(0, day 0) + days 1..n` sums to EXACTLY `pages-remaining`, so a
         running total over `max(0, pages)` still lands on the real remainder
         whether the reader is behind, level or ahead.
   Post: `:deadline?` marks the LAST reading day — the day the covering has to
         be finished. It is data rather than a rendering rule because the plan
         table recycles its rows, so a view cannot find that boundary by
         looking at a row's neighbours.
   Inv : a `:mature` entry always carries 0 pages. This is the whole point of
         the split, and the falsifier the plan is verified against."
  [pages-remaining days horizon covered-today]
  (let [r (max 1 (long days))
        n (max r (max 1 (long horizon)))
        done-today (max 0 (long covered-today))
        remaining (max 0 (long pages-remaining))
        ;; The allocation comes from the START-OF-DAY remainder, which is what
        ;; is left now plus what today already removed. Recomputing it from the
        ;; current remainder is exactly the defect this replaces: the number
        ;; would shrink as the reader works and never reach zero.
        allocation (long (first (daily-page-plan (+ remaining done-today) r)))
        owed (- allocation done-today)
        ;; Days 1..n get what is left after today's OWED pages, never after the
        ;; allocation. Once the reader is ahead, `owed` is negative and they owe
        ;; nothing more today, so the whole remainder belongs to the later days.
        rest (if (> r 1) (daily-page-plan (- remaining (max 0 owed)) (dec r)) [])]
    (mapv (fn [d] {:day d
                   :pages (cond
                            (zero? d) owed
                            (< d r) (nth rest (dec d) 0)
                            :else 0)
                   :phase (if (< d r) :cover :mature)
                   :deadline? (= d (dec r))})
      (range n))))

(defn covering-day?
  "Whether a plan row belongs to the covering half of the plan (§1.2).

   The filter's predicate AND the count rendered beside it, so the label cannot
   promise a number the table does not leave.

   Pre : `row` is a `plan-days` entry, or a `schedule/study-plan` row built from
         one. Both carry `:day`, `:pages` and `:deadline?`, so both give the
         same answer for the same day.
   Post: true when the row hands out pages, on the deadline whatever it hands
         out, and ALWAYS on day 0.
   Post: **the deadline survives with 0 pages.** `daily-page-plan` rounds, so
         the last day of a slow window can carry nothing — 28 pages over 120
         days puts 0 on day 119, which IS the deadline — and dropping it would
         take the one date the `Cover by` tile names out of the filtered table.
   Post: **today survives whatever it owes.** Day 0 is a quota, so it reads
         `done` or `N ahead` exactly when it owes nothing — and filtering it out
         at that moment would hide the one row the reader worked to earn."
  [row]
  (or (pos? (long (:pages row 0)))
      (boolean (:deadline? row))
      (= 0 (:day row))))

(defn pages-label
  "`n` with the noun `page` agreeing with it.
   Post: \"1 page\", \"0 pages\", \"12 pages\"."
  [n]
  (let [k (long n)]
    (str k " page" (when (not= 1 k) "s"))))

(defn quota-met?
  "Whether today's quota is settled — nothing owed, or already past it.

   The one predicate three surfaces read, so the table cell, the Today tile and
   the home-view row cannot disagree about when the day is finished."
  [owed]
  (<= (long owed) 0))

(defn quota-cell
  "Today's owed pages as the plan TABLE renders them.

   Pre : `owed` is `plan-days`' day 0, which may be negative.
   Post: a bare count while pages are owed, `done` at zero, and `N ahead` past
         it. Never a negative integer: a `-1` in a column of page counts reads
         as an error rather than as credit."
  [owed]
  (let [k (long owed)]
    (cond
      (pos? k) (str k)
      (zero? k) "done"
      :else (str (- k) " ahead"))))

(defn quota-phrase
  "Today's owed pages as the TILES render them, with the noun.

   Same three states as `quota-cell`, and the same words past zero, so a reader
   moving between the tile and the table sees one vocabulary."
  [owed]
  (let [k (long owed)]
    (cond
      (pos? k) (pages-label k)
      (zero? k) "done"
      :else (str (- k) " ahead"))))

(defn deadline-token
  "A signed day count as the deadline half of a readout.

   Pre : `days` is `schedule/signed-days-to-target`'s SIGNED count, NOT
         `days-until`'s. That one floors at 0, which reports the same value for
         today and for last month, and telling those apart is this fn's whole
         job.
   Post: \"12d\" before the target, \"today\" on it, \"12d overdue\" after it.
   Post: **the overdue side names its magnitude.** A bare \"overdue\" reads the
         same on the day after the target and four months later, and those call
         for different actions — one is a date to nudge, the other is a schedule
         to archive. The unit is the same `Nd` the live side uses, so the two
         halves of the readout stay comparable.
   Post: nil reads as \"today\" rather than throwing — a caller with no date has
         no deadline to miss."
  [days]
  (let [d (long (or days 0))]
    (cond
      (pos? d) (str d "d")
      (zero? d) "today"
      :else (str (- d) "d overdue"))))

(defn schedule-badge-face
  "The reading-view schedule badge's whole face: today's quota and the deadline.

   It lives HERE rather than in the view because it owns nothing the view knows.
   Both halves are this namespace's tokens, and the rule below is about the PAIR
   — a view composing them would have to remember it, and a second surface
   pairing them would have to remember it again.

   Pre : `owed` is `schedule/root-schedule-pace`'s `:pages-today` — day 0 of the
         plan, which may be negative. `days` is its SIGNED `:days-to-target`.
         `days-until`'s floored count reports 0 for today and for last month, and
         this face has to tell those apart.
   Post: `today <quota>, <token>`, and `<quota>, <token>` in the one case the
         token is itself `today`.
   Post: **the label drops only on that repeat.** `today done, today` names one
         word twice for two different things: the day the work is owed on, and
         the day the target falls on. Nothing else earns the drop.
   Inv : the word `today` is on the face either way, and that is what makes the
         drop safe. The label anchors the quota — a bare `done` reads as the
         whole SCHEDULE being finished, and a bare page count sits beside the
         `44/60` progress badge, whose denominator is the whole document while
         this one is the schedule's page range (`plans/schedule-pace-pill.md`
         §4 rejected that collision twice). On the repeat day the DAY TOKEN
         supplies the same word, so `3 pages, today` still reads as a count
         owed today. The anchor moves; it does not go.
   Inv : the drop tracks `deadline-token`'s output, not the quota's state. All
         three quota states lose the label on the target date, because all three
         would repeat the word.
   Inv : NO state word. The pace verdict is in the badge's TOOLTIP, through
         `pace-phrase`. Two figures on one face asked the reader to reconcile a
         cumulative lag against a daily share, and those are measured against
         different lines and need not agree: `Behind, today 16 pages` beside a
         tooltip reading `9 pages behind` was the measured case."
  [owed days]
  (let [token (deadline-token days)]
    (str (when (not= "today" token) "today ") (quota-phrase owed) ", " token)))

(defn pace-phrase
  "`pace-state`'s verdict and `pace`'s `:delta` as a phrase, for a hover
   readout. Sits here rather than beside `pace-state` because it reads
   `pages-label`, which the section above defines.

   Pre : `state` is `pace-state`'s verdict over this same `delta`. A state from
         one delta and a magnitude from another is a caller bug.
   Post: \"5 pages behind\", \"3 pages ahead\", or a bare \"on plan\".
   Inv : the dead-zone case names NO magnitude. Rounding 0.6 up to \"1 page
         behind\" beside a badge reading `On plan` would assert a lag the badge
         denies, and the reader would have no way to tell which to believe."
  [state delta]
  (if (= :on-plan state)
    "on plan"
    (str (pages-label (Math/round (Math/abs (double delta))))
      (if (= :behind state) " behind" " ahead"))))

(defn today-plan
  "What the user does today: pages to cover, cards to review (§1.2).

   `:pages-today` is day 0 of `plan-days`, never a separate calculation. One
   definition, so the tile and the dated table cannot report different numbers
   for the same day — and a user who sees two answers has no way to pick
   between them. `:reviews-today` is day 0 of the derived load, which is
   fractional by §6.4 and stays fractional here. The surface rounds it.

   Pre : `pages-remaining` counts pages NOT YET COVERED. Passing the unknown
         count instead is the defect this whole change exists to remove: it
         charges the user for pages they have already read.
   Pre : `covered-today` is REQUIRED, and no default papers over its absence.
         Every caller reads it from the same `document-progress` map that
         produced `pages-remaining`, including the degenerate paths, so 0 is
         never a stand-in for an unknown count — it means the reader has
         covered nothing today. Defaulting a missing key to 0 would silently
         restore the rate-shaped cell this fn exists to replace.
   Post: `:pages-today` is what is still OWED today and may be negative. Render
         it through `quota-phrase`, never as a bare number.
   Post: {:pages-today :reviews-today :pages-remaining}. There is no day count
         here. It used to carry `:days-remaining`, which returned the horizon
         while its name promised the days left to do the work — two different
         numbers once the plan stopped at the coverage deadline. Nothing read
         it. `:coverage`'s `:reading-days` is the figure it appeared to name."
  [{:keys [pages-remaining reading-days horizon daily-load covered-today]}]
  {:pages-today (:pages (first (plan-days pages-remaining reading-days horizon
                                 covered-today)))
   :reviews-today (double (get daily-load 0 0.0))
   :pages-remaining (long pages-remaining)})

;; ---------------------------------------------------------------------------
;; Which schedule a reading view is about
;; ---------------------------------------------------------------------------

(defn covers-page?
  "Whether `page` falls inside a schedule row's page range.

   Pre : `row` carries `:first-page` and `:last-page`, either of which may be
         nil for an open end — the same shape `db/get-schedule-pages` reads,
         where a nil first page means the start of the document and a nil last
         page means the end of it.
   Post: false for a nil `page`. A document with no page in view — a non-PDF, or
         the PDF root's own screen — is not inside any range, and answering true
         would make every schedule equally relevant instead of none."
  [row page]
  (let [p (when page (long page))
        lo (some-> (:first-page row) long)
        hi (some-> (:last-page row) long)]
    (boolean (and p
               (or (nil? lo) (>= p lo))
               (or (nil? hi) (<= p hi))))))

(defn pick-for-page
  "The one schedule a reading view should speak for, out of every active
   schedule over the same document.

   Nothing in the schema forbids two, and the reader has exactly one question:
   what does my schedule say about the pages in front of me. Ordering by target
   date answers a different one — which deadline is nearest — and on a document
   split into a finished first half and a live second half it named the half the
   user had already left, with the target long past.

   Pre : `rows` are ACTIVE schedules over ONE root, each carrying `:first-page`,
         `:last-page`, `:days-to-target` (SIGNED, negative once the target is
         past) and `:id`. Ownership is checked by whoever read them.
   Post: nil for an empty `rows`, so the caller renders no badge at all.
   Post: the row whose range holds `page`, when one does. That is the whole
         point, and it outranks urgency: a schedule over the pages being read is
         what the reader is asking about, even when its target has passed —
         `overdue` is then the true answer rather than the wrong schedule's.
   Post: among equals on that test, a LIVE schedule beats an expired one. With
         no page to match, this is the whole rule, and it is already better than
         soonest-target: an expired schedule is history and a live one is work.
   Post: the last key is `abs(days-to-target)`, which reads in both directions
         on purpose. Live, it is days remaining, so the soonest deadline wins.
         Expired, it is days since, so the schedule that ran out most recently
         wins over one that ran out in March.
   Post: `:id` breaks a full tie, so the choice is stable across reads rather
         than following whatever order the query returned."
  [rows page]
  (->> rows
    (sort-by (fn [r]
               (let [d (long (or (:days-to-target r) 0))]
                 [(if (covers-page? r page) 0 1)
                  (if (neg? d) 1 0)
                  (Math/abs d)
                  (long (or (:id r) 0))])))
    first))
