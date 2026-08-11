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
   Inv : `:untested` <= `:covered` <= `:total`. Done-and-uncarded is a strict
         subset of done, and every verdict comes from one page of the range.
         A violation is a bug in `page-verdict`, not here."
  [verdicts]
  (let [total (count verdicts)
        known (count (filter :known? verdicts))
        covered (count (filter :done? verdicts))
        untested (count (filter #(= :uncarded-done (:reason %)) verdicts))]
    {:total total
     :known known
     :covered covered
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

     :short-of-target  — the projection does not reach the required pages by
                         the target date.
     :over-capacity    — some day's derived load exceeds the review rate the
                         user has actually sustained (§6.7).
     :no-maturation-window — the horizon is at or under the maturation lead, so
                         a page covered today cannot clear the gate in time.
                         Every page of the plan lands on day 0 and none of them
                         can count.
     :maturity-unreachable — `maturation-lead` found no day at all. The gate
                         never opens under this deck config and θ, so no amount
                         of reading produces a known page.

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
   Post: `:maturation-lead` is reported so the panel names the number of days
         rather than a bare warning. A user told 'no maturation window' with no
         figure cannot tell whether to move the date by a day or by a month.
   Invariant: a feasible plan reports no reasons, and an infeasible one reports
         at least one. The two can never both be empty."
  [{:keys [projected-known required-pages peak-load observed-capacity
           earliest-day horizon] lead :maturation-lead}]
  (let [short? (< (long projected-known) (long required-pages))
        over? (and (pos? (double observed-capacity))
                (> (double peak-load) (double observed-capacity)))
        unreachable? (nil? lead)
        no-window? (and (some? lead) (<= (long horizon) (long lead)))
        reasons (cond-> []
                  short? (conj :short-of-target)
                  over? (conj :over-capacity)
                  no-window? (conj :no-maturation-window)
                  unreachable? (conj :maturity-unreachable))]
    {:feasible? (empty? reasons)
     :reasons reasons
     :peak-load (double peak-load)
     :capacity (double observed-capacity)
     :projected-known (long projected-known)
     :required (long required-pages)
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
   Post: a vector of {:pages :phase :deadline?} of length
         `max(days, max(1, horizon))`. `:phase` is `:cover` for the first
         `days` entries and `:mature` after.
   Post: `:deadline?` marks the LAST reading day — the day the covering has to
         be finished. It is data rather than a rendering rule because the plan
         table recycles its rows, so a view cannot find that boundary by
         looking at a row's neighbours.
   Post: the `:pages` column sums to EXACTLY `pages-remaining`.
   Inv : a `:mature` entry always carries 0 pages. This is the whole point of
         the split, and the falsifier the plan is verified against."
  [pages-remaining days horizon]
  (let [r (max 1 (long days))
        n (max r (max 1 (long horizon)))
        pages (daily-page-plan pages-remaining r)]
    (mapv (fn [d] {:pages (nth pages d 0)
                   :phase (if (< d r) :cover :mature)
                   :deadline? (= d (dec r))})
      (range n))))

(defn covering-day?
  "Whether a plan row belongs to the covering half of the plan (§1.2).

   The filter's predicate AND the count rendered beside it, so the label cannot
   promise a number the table does not leave.

   Pre : `row` is a `plan-days` entry, or a `schedule/study-plan` row built from
         one. Both carry `:pages`, and `:deadline?` on at most one of them.
   Post: true when the row hands out pages, and on the deadline whatever it
         hands out.
   Post: **the deadline survives with 0 pages.** `daily-page-plan` rounds, so
         the last day of a slow window can carry nothing — 28 pages over 120
         days puts 0 on day 119, which IS the deadline — and dropping it would
         take the one date the `Cover by` tile names out of the filtered table."
  [row]
  (or (pos? (long (:pages row 0))) (boolean (:deadline? row))))

(defn pages-label
  "`n` with the noun `page` agreeing with it.
   Post: \"1 page\", \"0 pages\", \"12 pages\"."
  [n]
  (let [k (long n)]
    (str k " page" (when (not= 1 k) "s"))))

(defn deadline-token
  "A signed day count as the deadline half of a readout.

   Pre : `days` is `schedule/signed-days-to-target`'s SIGNED count, NOT
         `days-until`'s. That one floors at 0, which reports the same value for
         today and for last month, and telling those apart is this fn's whole
         job.
   Post: \"12d\" before the target, \"today\" on it, \"overdue\" after it.
   Post: nil reads as \"today\" rather than throwing — a caller with no date has
         no deadline to miss."
  [days]
  (let [d (long (or days 0))]
    (cond
      (pos? d) (str d "d")
      (zero? d) "today"
      :else "overdue")))

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
   Post: {:pages-today :reviews-today :pages-remaining}. There is no day count
         here. It used to carry `:days-remaining`, which returned the horizon
         while its name promised the days left to do the work — two different
         numbers once the plan stopped at the coverage deadline. Nothing read
         it. `:coverage`'s `:reading-days` is the figure it appeared to name."
  [{:keys [pages-remaining reading-days horizon daily-load]}]
  {:pages-today (:pages (first (plan-days pages-remaining reading-days horizon)))
   :reviews-today (double (get daily-load 0 0.0))
   :pages-remaining (long pages-remaining)})
