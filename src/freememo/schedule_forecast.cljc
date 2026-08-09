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
                 (< r theta) :below-theta
                 (< ivl sf/maturity-interval-days) :immature
                 :else :mature)]
    {:page-id (:page-id page)
     :page-number (:page-number page)
     :known? (contains? #{:uncarded-done :mature} reason)
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
  "Known and untested counts over a set of verdicts (§5.4, §5.6).

   Post: {:total :known :untested :unknown :fraction}. `:untested` is the
         known-because-uncarded count, reported SEPARATELY rather than folded
         into `:known` — §5.4 requires the exposure to stay visible, and §6.6.4.1
         requires lever I5 to move it. `:fraction` is known over total, and 0.0
         for an empty range rather than a division by zero."
  [verdicts]
  (let [total (count verdicts)
        known (count (filter :known? verdicts))
        untested (count (filter #(= :uncarded-done (:reason %)) verdicts))]
    {:total total
     :known known
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
  "Lag or lead, in pages (§6.2).

   The figure is known-now minus required-now, where required-now walks a
   straight line from the known count at the schedule's start to the whole
   range at its target. Negative is behind, positive is ahead, and a document
   exactly on plan reads 0.

   Pre : `elapsed-days` and `total-days` are day counts, `total-days` >= 0.
   Post: {:required :actual :delta}. Before the start and after the target the
         line is clamped to its endpoints, so a schedule opened after its target
         reports the whole range as required rather than a figure past 100%."
  [{:keys [known-now total-pages start-known elapsed-days total-days]}]
  (let [frac (cond
               (<= (long total-days) 0) 1.0
               :else (min 1.0 (max 0.0 (/ (double elapsed-days) (double total-days)))))
        required (+ (double start-known) (* frac (- (double total-pages) (double start-known))))]
    {:required required
     :actual (double known-now)
     :delta (- (double known-now) required)}))

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

   Post: {:feasible? :reasons :peak-load :capacity :projected-known :required
          :levers}. `:levers` is always the four of §6.6 — I1 date, I2 capacity,
          I4 θ, I5 exclude pages — because the warning MUST name all four
          whichever reason fired. I3 does not exist and 6.6.5 records why lower
          `card_count` was rejected.
   Invariant: a feasible plan reports no reasons, and an infeasible one reports
         at least one. The two can never both be empty."
  [{:keys [projected-known required-pages peak-load observed-capacity
           earliest-day]}]
  (let [short? (< (long projected-known) (long required-pages))
        over? (and (pos? (double observed-capacity))
                (> (double peak-load) (double observed-capacity)))
        reasons (cond-> []
                  short? (conj :short-of-target)
                  over? (conj :over-capacity))]
    {:feasible? (empty? reasons)
     :reasons reasons
     :peak-load (double peak-load)
     :capacity (double observed-capacity)
     :projected-known (long projected-known)
     :required (long required-pages)
     :earliest-day earliest-day
     :levers [:push-target-date :raise-capacity :lower-theta :exclude-pages]}))

;; ---------------------------------------------------------------------------
;; Today's plan (§1.2)
;; ---------------------------------------------------------------------------

(defn daily-page-plan
  "Pages to work on each day from today to the target (§1.2).

   The distribution follows the SAME straight line §6.2's pace figure compares
   against, so a user who works the table reads `on plan` every day. Deriving
   it any other way would let the plan and the verdict disagree, and the user
   would have no way to tell which one to believe.

   Day d's page count is the difference between the line's cumulative target at
   d and at d−1, so the counts are whole numbers that sum EXACTLY to
   `pages-remaining`. A rate below one page per day therefore produces days of
   0 and days of 1 rather than a fraction nobody can act on.

   Pre : `pages-remaining` >= 0. `days` >= 0 — a target today or in the past
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

(defn pages-label
  "`n` with the noun `page` agreeing with it.
   Post: \"1 page\", \"0 pages\", \"12 pages\"."
  [n]
  (let [k (long n)]
    (str k " page" (when (not= 1 k) "s"))))

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
  "What the user does today: pages to work, cards to review (§1.2).

   `:pages-today` is day 0 of `daily-page-plan`, never a separate calculation.
   One definition, so the tile and the dated table cannot report different
   numbers for the same day — and a user who sees two answers has no way to
   pick between them. `:reviews-today` is day 0 of the derived load, which is
   fractional by §6.4 and stays fractional here; the surface rounds it.

   Post: {:pages-today :reviews-today :pages-remaining :days-remaining}."
  [{:keys [pages-remaining days-remaining daily-load]}]
  {:pages-today (first (daily-page-plan pages-remaining days-remaining))
   :reviews-today (double (get daily-load 0 0.0))
   :pages-remaining (long pages-remaining)
   :days-remaining (long days-remaining)})
