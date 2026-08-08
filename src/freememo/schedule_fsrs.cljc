(ns freememo.schedule-fsrs
  "Study-scheduler replay and projection — the pure half of the Schedule tab
   (plans/study-scheduler-spec.md §4).

   Two jobs, and no state between them:

   1. REPLAY. Turn an Anki revlog into FSRS memory state, by feeding the
      filtered review sequence through `freememo.fsrs/review-card`. `fsrs.cljc`
      is not modified and not re-implemented — every transition here is one
      call into it.
   2. PROJECT. Walk a card forward to a target date, branching on all four
      ratings at every projected review, weighted by the user's rating
      distribution, and report retrievability, projected interval and a daily
      review histogram at that date.

   Pure by contract (§4.7): no Electric, no I/O, no clock, no implicit time
   source. `now-ms` and the horizon are arguments. The namespace is testable
   from `clj -M:test` with no database and no Anki.

   Day arithmetic is in WHOLE days counted from `now-ms`, so day 0 is today and
   a day in the past is negative. Anki counts days against its own day cutoff
   instead, so a pair of events straddling that cutoff can differ by one day
   here. That is the first of the divergences §4.1 sets out to measure."
  (:require [freememo.fsrs :as fsrs]))

;; ---------------------------------------------------------------------------
;; Constants
;; ---------------------------------------------------------------------------

(def ^:const ms-per-day 86400000)
(def ^:const secs-per-day 86400.0)

(def measured-rating-weights
  "V2 (§4.4) — the lifetime rating distribution measured on deck `All`,
   n=69,455. Keys are FSRS ratings: Again 1, Hard 2, Good 3, Easy 4.

   Invariant: the values sum to 1. Every projection weights its branches by
   this map, and `project-card` renormalizes surviving leaves, so a map that
   does not sum to 1 skews the histogram of §6.3 without skewing R."
  {1 0.11906, 2 0.01054, 3 0.86480, 4 0.00560})

(def ^:const default-epsilon
  "§4.6.3 — a branch whose cumulative probability falls below this is dropped.
   Adjustable without a schema change: it is a `project-card` option.

   MEASURED, over a 365-day horizon on four card shapes under the weights of
   `measured-rating-weights`. R rises monotonically as ε falls, and the whole
   remaining error below 1e-4 is smaller than the error a page verdict can
   see:

     ε      R (a 2-day card)   nodes
     1e-2      0.87177            67
     1e-3      0.90811           510
     1e-4      0.92089         3 848
     1e-5      0.92513        29 117
     1e-6      0.92710       206 954
     1e-7      0.92851     1 437 848

   1e-4 costs 0.008 of R against an exhaustive walk and about 3 800 nodes.
   Each further decade buys under 0.004 of R for roughly 7× the nodes, and
   1e-7 is 1.4 MILLION nodes per card, which no document-sized run can pay.

   The residual matters only for a page whose mean R sits within 0.008 of θ.
   Lowering ε moves such a page's verdict UP, never down, because pruning
   discards the low-probability Again chains that drag the mean."
  1e-4)

(def ^:const default-node-cap
  "§4.6.4 — a hard per-card node budget, independent of ε. A backstop, not the
   working bound: at the default ε a 365-day horizon settles between 1 500 and
   3 900 nodes (measured), because Hard and Easy fall below ε within three
   levels (§4.6.5). The headroom covers a multi-year target without the cap
   silently truncating one."
  32768)

(def ^:const maturity-interval-days
  "§5.8.3 — the maturity gate, in days. A CONSTANT. No setting, no
   per-schedule field and no config key may change it."
  21)

(def ^:private answer-buttons #{1 2 3 4})

;; ---------------------------------------------------------------------------
;; Deck config → scheduler
;; ---------------------------------------------------------------------------

(def ^:const fsrs6-parameter-count 21)

(defn fsrs6-parameters
  "The 21 FSRS-6 weights in `deck-config`, or nil.

   `getDeckConfig` carries up to three generations of weights side by side, and
   only the newest matches `freememo.fsrs` (§9.2):

     `fsrsWeights`  17 values — FSRS-4.5
     `fsrsParams5`  19 values — FSRS-5
     `fsrsParams6`  21 values — FSRS-6, the one this app needs

   Pre : `deck-config` is the parsed `getDeckConfig` result, or nil.
   Post: a 21-element vector of doubles, or nil when the key is absent, is the
         wrong length, or holds a non-number. nil is what `4.3` refuses on —
         the caller renders the failure state of §8.1 rather than falling back
         to FreeMemo's default weights, which would not be the user's."
  [deck-config]
  (let [ps (:fsrsParams6 deck-config)]
    (when (and (sequential? ps)
            (= fsrs6-parameter-count (count ps))
            (every? number? ps))
      (mapv double ps))))

(defn- minutes->seconds [delays fallback]
  (if (and (sequential? delays) (seq delays) (every? number? delays))
    (mapv #(long (* 60 (double %))) delays)
    fallback))

(defn scheduler-from-deck-config
  "A `freememo.fsrs` scheduler configured from Anki's own deck config.

   Every value the algorithm reads comes from Anki, so the replay of §4.1 uses
   the user's numbers rather than this app's defaults:
   `fsrsParams6` → weights, `desiredRetention` → θ (§9.1), `new.delays` and
   `lapse.delays` → the learning and relearning steps (Anki states them in
   MINUTES, `fsrs.cljc` in seconds), `maxIvl` → the interval ceiling.

   Fuzzing is off. Fuzz spreads real due dates to stop them clumping; a
   forecast that fuzzed would report a different answer on every refresh for
   no gain.

   Pre : `(fsrs6-parameters deck-config)` is non-nil — the caller has already
         refused a config that fails §4.3.
   Post: a scheduler map. `:desired-retention` is θ1 unless the caller
         overrode it through I4 (§6.6.3)."
  ([deck-config] (scheduler-from-deck-config deck-config nil))
  ([deck-config theta-override]
   (let [params (fsrs6-parameters deck-config)]
     (assert (some? params) "deck config carries no 21-parameter fsrsParams6")
     (fsrs/make-scheduler
       {:parameters params
        :desired-retention (double (or theta-override
                                     (:desiredRetention deck-config)
                                     0.9))
        :learning-steps (minutes->seconds (get-in deck-config [:new :delays]) [60 600])
        :relearning-steps (minutes->seconds (get-in deck-config [:lapse :delays]) [600])
        :maximum-interval (long (or (:maxIvl deck-config) 36500))
        :enable-fuzzing false}))))

(defn config-fingerprint
  "§3.3 — an identity for the deck config's algorithm-bearing fields.

   A mismatch against the stored value discards cached state and replays in
   full. Three fields bear: the weights, the retention target and the revlog
   cutoff. Changing any of them changes every derived stability.

   A printed string rather than a hash. `hash` differs between the JVM and
   ClojureScript, and this value is computed wherever the config was read and
   compared wherever the cache lives, so a peer-dependent digest would read as
   a permanent mismatch and replay in full on every open.

   Pre : `deck-config` is the parsed `getDeckConfig` result.
   Post: a deterministic string, equal across peers for equal input."
  [deck-config]
  (pr-str {:params (fsrs6-parameters deck-config)
           :retention (:desiredRetention deck-config)
           :ignore-before (:ignoreRevlogsBeforeDate deck-config)}))

;; ---------------------------------------------------------------------------
;; Revlog filter (§4.2)
;; ---------------------------------------------------------------------------

(defn filtered-revlog
  "The reviews that count toward FSRS memory state.

   Two predicates, mirroring Anki's own optimizer:

   - `ease` MUST be an answer button, 1 through 4. A manual reschedule, a
     `set due date` and a forget all write `ease = 0`. Predicating on `ease`
     rather than on `type` drops all of them without this namespace carrying a
     copy of a `type` vocabulary that has grown twice.
   - the entry is at or after `ignore-before-ms`, which is Anki's
     `ignoreRevlogsBeforeDate` rendered to epoch milliseconds.

   Pre : `revlog` is a seq of {:id epoch-ms :ease int}. Order is irrelevant.
   Post: a vector sorted ascending by `:id`, holding answer-button entries at
         or after the cutoff. Possibly empty — a card whose whole history is
         manual reschedules has no state to derive, and the caller reads that
         as never-reviewed (§5.8.5).

   Divergence from Anki, recorded rather than fixed: Anki's optimizer also
   TRUNCATES a card's history at its most recent forget, so pre-forget reviews
   do not inform the new memory state. Here a forget is dropped as an entry but
   truncates nothing, so a forgotten card replays its whole history and reports
   a stability that is too high. This is the second of the divergences §4.1
   measures."
  [revlog ignore-before-ms]
  (let [cutoff (long (or ignore-before-ms 0))]
    (->> revlog
      (filter #(and (answer-buttons (:ease %))
                 (>= (long (:id %)) cutoff)))
      (sort-by #(long (:id %)))
      vec)))

(defn ->fsrs-card
  "The FSRS-card subset of `card`, normalized to `fsrs/review-card`'s
   precondition.

   That precondition is `step nil ⟺ state = 2` (`fsrs.cljc:215`), and this is
   the boundary that owes it. A card assembled from a cached row breaks it
   routinely: a card pushed to Anki but never reviewed has no replayed state,
   so it reads back as Learning with a null step, and feeding that straight to
   `review-card` throws an AssertionError from inside FSRS — a crash whose
   message names the assertion and not the row.

   Pre : `card` carries :state :step :stability :difficulty, any of which may
         be nil.
   Post: {:state :step :stability :difficulty} with `:state` in {1,2,3} and
         `:step` an int exactly when `:state` is not 2. A step defaults to 0,
         which is the first learning step — where a card with no history in
         fact stands."
  [card]
  (let [state (long (or (:state card) 1))
        state (if (#{1 2 3} state) state 1)]
    {:state state
     :step (when (not= 2 state) (long (or (:step card) 0)))
     :stability (some-> (:stability card) double)
     :difficulty (some-> (:difficulty card) double)}))

;; ---------------------------------------------------------------------------
;; Replay (§4.1)
;; ---------------------------------------------------------------------------

(def fresh-state
  "The starting point of a replay: a never-reviewed card with no gap behind it."
  (assoc fsrs/new-card :last-review-ms nil :interval-days 0.0 :reps 0))

(defn replay
  "FSRS memory state after `revlog`, or nil when no review survives the filter.

   The 3-arity CONTINUES from `initial`, which is what makes the incremental
   refresh of §2.4 correct. An incremental pull carries only the reviews after
   the stored high-water mark; replaying those from `fresh-state` would restart
   every card as if its whole history never happened, and would report a
   confident stability an order of magnitude too low.

   Pre : `revlog` is the output of `filtered-revlog` — sorted ascending, and
         holding answer buttons only. `scheduler` is a
         `freememo.fsrs/make-scheduler` map. `initial` carries :state :step
         :stability :difficulty :last-review-ms :reps.
   Post: {:state :step :stability :difficulty :last-review-ms :interval-days
          :reps}, with `:stability` a positive double and `:interval-days` the
         interval the LAST review produced. nil for an empty revlog against a
         fresh start; `initial` unchanged for an empty revlog against a
         continuation, because no new review means no new state.
   Invariant: every transition is one `fsrs/review-card` call. This fn owns the
         gap arithmetic and nothing else.

   Elapsed days are whole days of wall-clock gap. `fsrs/review-card` reads a
   gap under one day as a same-day review, so flooring is what makes that test
   fire correctly."
  ([scheduler revlog] (replay scheduler revlog nil))
  ([scheduler revlog initial]
   (if (empty? revlog)
     initial
     (reduce
       (fn [acc {:keys [id ease]}]
         (let [prev (:last-review-ms acc)
               gap (when prev (quot (- (long id) (long prev)) ms-per-day))
               ;; `initial` may come from a cached row whose step is null, so
               ;; the precondition is restored here rather than assumed.
               card (->fsrs-card acc)
               nxt (fsrs/review-card scheduler card ease (or gap 0) gap)]
           (assoc nxt
             :last-review-ms (long id)
             :interval-days (/ (double (:interval-seconds nxt)) secs-per-day)
             :reps (inc (:reps acc)))))
       (or initial fresh-state)
       revlog))))

;; ---------------------------------------------------------------------------
;; Projection (§4.4 – §4.6)
;; ---------------------------------------------------------------------------

(defn- interval->whole-days
  "An FSRS interval in whole days, floored at 1.

   A learning or relearning step is minutes, which is zero days. The
   projection's grain is a day, so the honest floor for a sub-day step is the
   next day: a card the user relapses on today is back tomorrow."
  [interval-days]
  (max 1 (long (Math/round (double interval-days)))))

(defn- leaf-retrievability
  "R at `horizon-days` for one leaf, or 0.0 for a leaf that never held a
   review. `elapsed` is clamped at 0 — a leaf whose last review is the horizon
   itself has R at its ceiling, never a negative elapsed count."
  [scheduler leaf horizon-days]
  (let [s (get-in leaf [:card :stability])
        last-day (:last-day leaf)]
    (if (and (some? s) (pos? (double s)) (some? last-day))
      (fsrs/retrievability scheduler (double s) (max 0 (- horizon-days (long last-day))))
      0.0)))

(defn project-card
  "Retrievability and projected interval for one card at `horizon-days`, plus
   the daily review histogram it contributes (§4.4 – §4.6, §5.8.2, §6.3).

   The walk. From the card's next scheduled review, branch on all four ratings;
   each child advances to its own FSRS-scheduled next review and carries the
   parent's probability times that rating's weight. A branch whose next review
   falls past the horizon is a leaf. R and interval are then read on each leaf
   and combined by a probability-weighted mean over the surviving mass — M2,
   which collapses at the target date rather than at each step.

   The bound. A child below `:epsilon` is never created (§4.6.1), the surviving
   mass is renormalized to 1 before weighting (§4.6.2), and `:node-cap` stops
   expansion outright (§4.6.4). Under the measured weights, ε-pruning collapses
   the tree toward Again-and-Good within three levels (§4.6.5).

   Pre : `card` is
           {:state :step :stability :difficulty  ; the fsrs card, or all nil
            :interval-days  double               ; the interval in force now
            :last-review-day long|nil            ; <= 0, or nil if never reviewed
            :next-review-day long|nil}           ; >= 0, or nil if none is planned
         Day indices are whole days from now; `horizon-days` >= 0.
   Post: {:retrievability 0..1
          :interval-days  >= 0.0
          :daily-reviews  {day-index expected-count}   ; day-index in [0, horizon]
          :nodes long
          :capped? boolean}
         A card with no review before the horizon reports its CURRENT interval
         (§5.8.4). A card never reviewed by the horizon reports interval 0.0
         and R 0.0, so it fails the maturity gate whatever else holds (§5.8.5).
   Invariant: R and the interval are combined by the SAME rule over the SAME
         tree (§5.8.2). `:capped?` is surfaced rather than swallowed — a capped
         projection is an understatement, and §4.6.4 forbids a silent one."
  [scheduler card {:keys [horizon-days weights epsilon node-cap]
                   :or {weights measured-rating-weights
                        epsilon default-epsilon
                        node-cap default-node-cap}}]
  (let [horizon (long horizon-days)
        root {:card (->fsrs-card card)
              :last-day (:last-review-day card)
              :interval (double (or (:interval-days card) 0.0))
              :next-day (:next-review-day card)
              :prob 1.0}
        rated (sort (keys weights))]
    (loop [queue (list root), leaves (transient []), hist {}, nodes 0]
      (if-let [b (first queue)]
        (let [q (rest queue)
              nd (:next-day b)]
          (if (or (nil? nd) (> (long nd) horizon) (>= nodes node-cap))
            (recur q (conj! leaves b) hist nodes)
            (let [;; A card that has never been reviewed has no last-review day.
                  ;; Its first projected review is its first review, so the
                  ;; elapsed count fsrs reads is 0 and `days-since` is nil —
                  ;; the same shape `replay` hands a fresh card.
                  prev (:last-day b)
                  elapsed (if (some? prev) (max 0 (- (long nd) (long prev))) 0)
                  days-since (when (some? prev) elapsed)
                  [kids nodes']
                  (reduce
                    (fn [[acc n] rating]
                      (let [p (* (:prob b) (double (get weights rating 0.0)))]
                        (if (< p epsilon)
                          [acc n]
                          (let [r (fsrs/review-card scheduler (:card b) rating elapsed days-since)
                                ivl (/ (double (:interval-seconds r)) secs-per-day)]
                            [(conj acc {:card (->fsrs-card r)
                                        :last-day (long nd)
                                        :interval ivl
                                        :next-day (+ (long nd) (interval->whole-days ivl))
                                        :prob p})
                             (inc n)]))))
                    [[] nodes] rated)]
              (if (empty? kids)
                ;; Every child fell below ε. The branch stops here rather than
                ;; vanishing — dropping it would lose its mass from the mean.
                (recur q (conj! leaves b) hist nodes')
                ;; One review happens at `nd` on each surviving child, with that
                ;; child's own probability. Accumulated HERE, at creation, never
                ;; on the path: a per-path histogram summed over leaves would
                ;; count one review once per descendant leaf.
                (recur (concat kids q) leaves
                  (reduce (fn [h k] (update h (long nd) (fnil + 0.0) (:prob k))) hist kids)
                  nodes')))))
        (let [ls (persistent! leaves)
              mass (reduce + 0.0 (map :prob ls))
              norm (if (pos? mass) mass 1.0)]
          {:retrievability (/ (reduce + 0.0 (map #(* (:prob %) (leaf-retrievability scheduler % horizon)) ls)) norm)
           :interval-days (/ (reduce + 0.0 (map #(* (:prob %) (double (:interval %))) ls)) norm)
           :daily-reviews hist
           :nodes nodes
           :capped? (>= nodes node-cap)})))))
