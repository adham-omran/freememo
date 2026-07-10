(ns freememo.fsrs
  "FSRS-6 spaced-repetition scheduler — a faithful port of the reference
   implementation (open-spaced-repetition/py-fsrs, `fsrs/scheduler.py`).

   Pure and time-free: `review-card` takes the elapsed/gap day-counts the
   caller derives from timestamps, and returns the next memory state plus the
   next interval in SECONDS. The caller owns the clock — it turns the interval
   into a `due` instant and records `last-review`. This keeps the algorithm
   independently conformance-testable against py-fsrs (see fsrs_test).

   Ratings (ints): Again=1, Hard=2, Good=3, Easy=4.
   States  (ints): Learning=1, Review=2, Relearning=3.

   A `card` here is the FSRS-managed subset only:
     {:state int, :step int|nil, :stability double|nil, :difficulty double|nil}
   `:step` is an int in Learning/Relearning and nil in Review;
   :stability/:difficulty are nil only for a never-reviewed (fresh) card."
  #?(:clj (:import [java.lang Math])))

;; ---------------------------------------------------------------------------
;; Constants (verbatim from py-fsrs `scheduler.py`)
;; ---------------------------------------------------------------------------

(def default-parameters
  "FSRS-6 default weights w0..w20 (w20 = decay)."
  [0.212 1.2931 2.3065 8.2956 6.4133 0.8334 3.0194 0.001 1.8722 0.1666 0.796
   1.4835 0.0614 0.2629 1.6483 0.6014 1.8729 0.5425 0.0912 0.0658 0.1542])

(def ^:private stability-min 0.001)
(def ^:private min-difficulty 1.0)
(def ^:private max-difficulty 10.0)

(def ^:private fuzz-ranges
  [{:start 2.5  :end 7.0  :factor 0.15}
   {:start 7.0  :end 20.0 :factor 0.1}
   {:start 20.0 :end ##Inf :factor 0.05}])

(def ^:private secs-per-day 86400)

;; ---------------------------------------------------------------------------
;; Scheduler config
;; ---------------------------------------------------------------------------

(defn make-scheduler
  "Build a scheduler config, precomputing decay/factor.
   Pre: (count parameters) = 21. Steps are seconds (ints), longest-last."
  [{:keys [parameters desired-retention learning-steps relearning-steps
           maximum-interval enable-fuzzing]
    :or   {parameters        default-parameters
           desired-retention 0.9
           learning-steps    [60 600]
           relearning-steps  [600]
           maximum-interval  36500
           enable-fuzzing    true}}]
  {:pre [(= 21 (count parameters))]}
  (let [decay  (- (nth parameters 20))
        factor (- (Math/pow 0.9 (/ 1.0 decay)) 1)]
    {:parameters parameters
     :desired-retention desired-retention
     :learning-steps (vec learning-steps)
     :relearning-steps (vec relearning-steps)
     :maximum-interval maximum-interval
     :enable-fuzzing enable-fuzzing
     :decay decay
     :factor factor}))

(def default-scheduler (make-scheduler {}))

(def new-card
  "A never-reviewed card: Learning, step 0, no memory state yet."
  {:state 1 :step 0 :stability nil :difficulty nil})

;; ---------------------------------------------------------------------------
;; Clamps
;; ---------------------------------------------------------------------------

(defn- clamp-difficulty [d]
  (-> d (max min-difficulty) (min max-difficulty)))

(defn- clamp-stability [s]
  (max s stability-min))

;; ---------------------------------------------------------------------------
;; DSR formulas (each mirrors the identically-named py-fsrs method)
;; ---------------------------------------------------------------------------

(defn retrievability
  "Probability of recall after `elapsed-days` for a card of given `stability`.
   R = (1 + FACTOR·t/S)^DECAY. Pre: stability > 0, elapsed-days >= 0."
  [scheduler stability elapsed-days]
  (Math/pow (+ 1.0 (/ (* (:factor scheduler) (double elapsed-days)) stability))
            (:decay scheduler)))

(defn- initial-stability [params rating]
  (clamp-stability (double (nth params (dec rating)))))

(defn- initial-difficulty
  "D0(g) = w4 − e^(w5·(g−1)) + 1."
  [params rating clamp?]
  (let [d (+ (- (double (nth params 4)) (Math/exp (* (double (nth params 5)) (dec rating)))) 1.0)]
    (if clamp? (clamp-difficulty d) d)))

(defn- next-difficulty
  "Linear-damped delta + mean reversion toward D0(Easy)."
  [params difficulty rating]
  (let [linear-damping (fn [delta d] (/ (* (- 10.0 d) delta) 9.0))
        w7 (double (nth params 7))
        arg1 (initial-difficulty params 4 false)               ; Easy = 4, unclamped
        delta (- (* (double (nth params 6)) (- rating 3)))
        arg2 (+ difficulty (linear-damping delta difficulty))]
    (clamp-difficulty (+ (* w7 arg1) (* (- 1.0 w7) arg2)))))

(defn- short-term-stability
  "Same-day stability update (|Δt| < 1 day)."
  [params stability rating]
  (let [inc (* (Math/exp (* (double (nth params 17)) (+ (- rating 3) (double (nth params 18)))))
               (Math/pow stability (- (double (nth params 19)))))
        inc (if (>= rating 3) (max inc 1.0) inc)]              ; Good/Easy never shrink S
    (clamp-stability (* stability inc))))

(defn- next-recall-stability
  [params difficulty stability r rating]
  (let [hard-penalty (if (= rating 2) (double (nth params 15)) 1.0)
        easy-bonus   (if (= rating 4) (double (nth params 16)) 1.0)]
    (* stability
       (+ 1.0 (* (Math/exp (double (nth params 8)))
                 (- 11.0 difficulty)
                 (Math/pow stability (- (double (nth params 9))))
                 (- (Math/exp (* (- 1.0 r) (double (nth params 10)))) 1.0)
                 hard-penalty easy-bonus)))))

(defn- next-forget-stability
  [params difficulty stability r]
  (let [long-term (* (double (nth params 11))
                     (Math/pow difficulty (- (double (nth params 12))))
                     (- (Math/pow (+ stability 1.0) (double (nth params 13))) 1.0)
                     (Math/exp (* (- 1.0 r) (double (nth params 14)))))
        short-term (/ stability (Math/exp (* (double (nth params 17)) (double (nth params 18)))))]
    (min long-term short-term)))

(defn- next-stability
  [params difficulty stability r rating]
  (clamp-stability
    (if (= rating 1)
      (next-forget-stability params difficulty stability r)
      (next-recall-stability params difficulty stability r rating))))

(defn next-interval-days
  "Days until R decays to `desired-retention`, clamped to [1, maximum-interval]."
  [scheduler stability]
  (let [{:keys [factor decay desired-retention maximum-interval]} scheduler
        ivl (* (/ stability factor) (- (Math/pow desired-retention (/ 1.0 decay)) 1.0))]
    (-> (Math/round (double ivl)) (max 1) (min (long maximum-interval)))))

;; ---------------------------------------------------------------------------
;; State / step / interval transitions
;; ---------------------------------------------------------------------------

(defn- update-sd
  "New [stability difficulty] after a review. Uniform across all states:
   fresh card → initial; same-day → short-term; else → decayed next-stability.
   Difficulty always advances (except when initialized)."
  [scheduler {:keys [stability difficulty]} rating elapsed-days same-day?]
  (let [params (:parameters scheduler)]
    (cond
      (or (nil? stability) (nil? difficulty))
      [(initial-stability params rating) (initial-difficulty params rating true)]
      same-day?
      [(short-term-stability params stability rating) (next-difficulty params difficulty rating)]
      :else
      (let [r (retrievability scheduler stability elapsed-days)]
        [(next-stability params difficulty stability r rating) (next-difficulty params difficulty rating)]))))

(defn- graduate [scheduler stability]
  [2 nil (* (next-interval-days scheduler stability) secs-per-day)])

(defn- stepped-branch
  "Learning (state 1) / Relearning (state 3) share this stepping logic.
   Returns [new-state new-step interval-seconds]."
  [scheduler stepped-state steps step rating stability]
  (if (or (empty? steps)
          (and (>= step (count steps)) (#{2 3 4} rating)))
    (graduate scheduler stability)
    (case (long rating)
      1 [stepped-state 0 (nth steps 0)]
      2 [stepped-state step
         (long (cond (and (zero? step) (= 1 (count steps))) (* (nth steps 0) 1.5)
                     (and (zero? step) (>= (count steps) 2)) (/ (+ (nth steps 0) (nth steps 1)) 2.0)
                     :else (nth steps step)))]
      3 (if (= (inc step) (count steps))
          (graduate scheduler stability)
          [stepped-state (inc step) (nth steps (inc step))])
      4 (graduate scheduler stability))))

(defn- review-branch
  "Review state (2). Again → Relearning (or stays Review if no relearning steps)."
  [scheduler rating stability]
  (if (= rating 1)
    (let [rs (:relearning-steps scheduler)]
      (if (empty? rs)
        (graduate scheduler stability)
        [3 0 (nth rs 0)]))
    (graduate scheduler stability)))

(defn review-card
  "Advance `card` by `rating`. Returns
     {:state :step :stability :difficulty :interval-seconds}
   with the UN-fuzzed interval. Caller sets due = now + interval-seconds and
   last-review = now; apply `fuzz-interval-days` at the day scale if desired.

   `elapsed-days` (>=0) and `days-since` (int or nil for a fresh card) are the
   caller-derived gaps against the card's previous last-review.

   Pre: state ∈ {1,2,3}; rating ∈ {1,2,3,4}; step is an int when state ∈ {1,3}.
   Post: stability ≥ 0.001; difficulty ∈ [1,10]; step nil ⟺ state = 2."
  [scheduler {:keys [state step] :as card} rating elapsed-days days-since]
  {:pre [(#{1 2 3} state) (#{1 2 3 4} rating)
         (or (= 2 state) (int? step))]}
  (let [same-day? (and (some? days-since) (< days-since 1))
        [s d] (update-sd scheduler card rating elapsed-days same-day?)
        [new-state new-step interval-secs]
        (case (long state)
          1 (stepped-branch scheduler 1 (:learning-steps scheduler) step rating s)
          3 (stepped-branch scheduler 3 (:relearning-steps scheduler) step rating s)
          2 (review-branch scheduler rating s))]
    {:state new-state :step new-step :stability s :difficulty d
     :interval-seconds interval-secs}))

;; ---------------------------------------------------------------------------
;; Fuzz (optional; only Review-state intervals ≥ 2.5 days). `rnd` ∈ [0,1).
;; ---------------------------------------------------------------------------

(defn fuzz-interval-days
  "Spread a whole-day interval by a small random amount to avoid due-date
   clumping. `rnd` is a double in [0,1). No-op for intervals < 2.5 days."
  [scheduler interval-days rnd]
  (if (< interval-days 2.5)
    (long interval-days)
    (let [max-ivl-cap (long (:maximum-interval scheduler))
          delta (reduce (fn [d {:keys [start end factor]}]
                          (+ d (* factor (max (- (min (double interval-days) end) start) 0.0))))
                        1.0 fuzz-ranges)
          min-ivl (max 2 (Math/round (- (double interval-days) delta)))
          max-ivl (min max-ivl-cap (Math/round (+ (double interval-days) delta)))
          min-ivl (min min-ivl max-ivl)
          fuzzed  (+ (* rnd (+ (- max-ivl min-ivl) 1)) min-ivl)]
      (min (Math/round (double fuzzed)) max-ivl-cap))))
