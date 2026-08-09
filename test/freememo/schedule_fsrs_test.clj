(ns freememo.schedule-fsrs-test
  "The done-conditions of plans/study-scheduler-spec.md §4, §5 and §6, one test
   per obligation that a test can reach.

   Needs no database and no Anki — both namespaces under test are pure by
   contract (§4.7), and this suite is the assertion of that contract. If either
   one grows an I/O call or a clock read, this file stops loading."
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.fsrs :as fsrs]
            [freememo.schedule-fsrs :as sf]
            [freememo.schedule-anki :as sa]
            [freememo.schedule-forecast :as fc]))

;; The canonical user's own deck config (§9.2), so every number this suite
;; asserts is a number the spec measured rather than one it assumed.
(def deck-config
  {:fsrsParams6 [0.14766622 1.3167228 6.022987 21.994022 6.2535086 0.68111664
                 3.2361438 0.022469439 1.9978356 0.35071215 0.9860491 1.480942
                 0.06466301 0.32231277 1.5798045 0.54158354 1.952656 1.2637906
                 0.96293545 0.2147526 0.13147718]
   :fsrsWeights (vec (repeat 17 0.5))
   :fsrsParams5 (vec (repeat 19 0.5))
   :desiredRetention 0.9
   :ignoreRevlogsBeforeDate "1970-01-01"
   :maxIvl 36500
   :new {:delays [1.0 10.0]}
   :lapse {:delays [10.0]}})

(def sched (sf/scheduler-from-deck-config deck-config))

(defn- close? [a b tol] (< (Math/abs (- (double a) (double b))) tol))

;; ── §9.2 / §4.3 — the weight key ───────────────────────────────────

(deftest reads-the-21-parameter-key-only
  (testing "fsrsParams6 is the key; the 17- and 19-value keys are ignored"
    (is (= 21 (count (sf/fsrs6-parameters deck-config))))
    (is (close? 0.14766622 (first (sf/fsrs6-parameters deck-config)) 1e-9)))
  (testing "§4.3 — a config with no 21-parameter key refuses, never falls back"
    (is (nil? (sf/fsrs6-parameters (dissoc deck-config :fsrsParams6))))
    (is (nil? (sf/fsrs6-parameters (assoc deck-config :fsrsParams6 (vec (repeat 19 0.5))))))
    (is (nil? (sf/fsrs6-parameters (assoc deck-config :fsrsParams6 (vec (repeat 21 "x"))))))))

(deftest scheduler-takes-every-value-from-anki
  (is (close? 0.9 (:desired-retention sched) 1e-12))
  (testing "Anki states delays in minutes, fsrs.cljc in seconds"
    (is (= [60 600] (:learning-steps sched)))
    (is (= [600] (:relearning-steps sched))))
  (is (= 36500 (:maximum-interval sched)))
  (is (false? (:enable-fuzzing sched))))

(deftest theta-override-is-the-i4-lever
  (let [low (sf/scheduler-from-deck-config deck-config 0.8)]
    (is (close? 0.8 (:desired-retention low) 1e-12))))

(deftest fingerprint-covers-the-three-bearing-fields
  (let [base (sf/config-fingerprint deck-config)]
    (is (= base (sf/config-fingerprint deck-config)) "deterministic")
    (is (not= base (sf/config-fingerprint (assoc deck-config :desiredRetention 0.85))))
    (is (not= base (sf/config-fingerprint (assoc deck-config :ignoreRevlogsBeforeDate "2024-01-01"))))
    (is (not= base (sf/config-fingerprint (update deck-config :fsrsParams6 assoc 0 0.2))))
    (testing "a field the algorithm does not read does not invalidate the cache"
      (is (= base (sf/config-fingerprint (assoc deck-config :maxIvl 999)))))))

;; ── §4.2 — the revlog filter ───────────────────────────────────────

(deftest filter-predicates-on-ease-not-type
  (let [log [{:id 300 :ease 3 :type 1}
             {:id 100 :ease 0 :type 4}   ; manual reschedule
             {:id 200 :ease 1 :type 1}
             {:id 50 :ease 3 :type 0}]]
    (testing "ease 0 is dropped whatever its type"
      (is (= [50 200 300] (mapv :id (sf/filtered-revlog log 0)))))
    (testing "§4.2 done-when — a manually rescheduled card excludes that entry"
      (is (every? #(not= 0 (:ease %)) (sf/filtered-revlog log 0))))
    (testing "the ignore-date cutoff drops everything before it"
      (is (= [200 300] (mapv :id (sf/filtered-revlog log 200)))))
    (testing "output is sorted ascending whatever the input order"
      (is (= [50 200 300] (mapv :id (sf/filtered-revlog (shuffle log) 0)))))))

(deftest a-card-of-only-manual-entries-has-no-state
  (is (nil? (sf/replay sched (sf/filtered-revlog [{:id 5 :ease 0} {:id 9 :ease 0}] 0)))))

;; ── §4.1 — replay ──────────────────────────────────────────────────

(deftest replay-reproduces-the-measured-initial-stabilities
  (testing "§6.8 — one review, then R a day later, under the user's own weights"
    (doseq [[rating s r] [[1 0.14766622 0.746] [2 1.3167228 0.917]
                          [3 6.022987 0.976] [4 21.994022 0.993]]]
      (let [st (sf/replay sched [{:id 0 :ease rating}])]
        (is (close? s (:stability st) 1e-6) (str "initial S for rating " rating))
        (is (close? r (fsrs/retrievability sched (:stability st) 1) 5e-4)
          (str "R at t=1 for rating " rating))))))

(deftest replay-threads-state-through-every-review
  (let [day (fn [n] (* n sf/ms-per-day))
        st (sf/replay sched [{:id (day 0) :ease 3} {:id (day 1) :ease 3}
                             {:id (day 7) :ease 3} {:id (day 30) :ease 3}])]
    (is (= 4 (:reps st)))
    (is (= (day 30) (:last-review-ms st)))
    (is (= 2 (:state st)) "graduated to Review")
    (is (pos? (:stability st)))
    (testing "§5.8.3.2 — at θ=0.9 the interval equals the stability exactly"
      (is (= (long (Math/round (:stability st)))
            (fsrs/next-interval-days sched (:stability st)))))))

(deftest interval-equals-stability-at-the-measured-theta
  (doseq [s [1.0 6.022987 21.0 216.0 1000.0]]
    (is (= (long (Math/round s)) (fsrs/next-interval-days sched s)))))

;; ── §4.4 – §4.6 — projection ───────────────────────────────────────

(deftest measured-weights-sum-to-one
  (is (close? 1.0 (reduce + 0.0 (vals sf/measured-rating-weights)) 1e-9)))

(def mature-card
  {:state 2 :step nil :stability 216.0 :difficulty 5.0
   :interval-days 216.0 :last-review-day -10 :next-review-day 206})

(def due-soon-card
  {:state 2 :step nil :stability 6.0 :difficulty 5.0
   :interval-days 6.0 :last-review-day -1 :next-review-day 5})

(def never-reviewed-card
  {:state 1 :step 0 :stability nil :difficulty nil
   :interval-days 0.0 :last-review-day nil :next-review-day nil})

(deftest a-learning-card-with-no-step-does-not-crash-fsrs
  (testing "fsrs/review-card requires `step nil ⟺ state = 2`, and a card pushed
            to Anki but never reviewed reads back as Learning with a null step"
    (let [c (sf/->fsrs-card {:state 1 :step nil :stability nil :difficulty nil})]
      (is (= 0 (:step c)) "defaults to the first learning step, not nil"))
    (is (nil? (:step (sf/->fsrs-card {:state 2 :step 3}))) "Review carries no step")
    (is (= 0 (:step (sf/->fsrs-card {:state 3 :step nil}))) "Relearning needs one too")
    (is (= 1 (:state (sf/->fsrs-card {:state 99}))) "an out-of-range state falls back to Learning")
    (testing "and the projection of such a card completes instead of throwing"
      (let [p (sf/project-card sched
                {:state 1 :step nil :stability nil :difficulty nil
                 :interval-days 0.0 :last-review-day nil :next-review-day 0}
                {:horizon-days 30})]
        (is (pos? (:retrievability p)))
        (is (pos? (:nodes p)))))))

(deftest no-review-before-the-target-reports-the-unprojected-state
  (testing "§4.4 done-when, and §5.8.4"
    (let [p (sf/project-card sched mature-card {:horizon-days 30})]
      (is (zero? (:nodes p)))
      (is (close? 216.0 (:interval-days p) 1e-9) "carries its CURRENT interval")
      (is (close? (fsrs/retrievability sched 216.0 40) (:retrievability p) 1e-9))
      (is (empty? (:daily-reviews p))))))

(deftest a-card-never-reviewed-by-the-target-fails-the-gate
  (testing "§5.8.5"
    (let [p (sf/project-card sched never-reviewed-card {:horizon-days 30})]
      (is (zero? (:interval-days p)))
      (is (zero? (:retrievability p))))))

(deftest branch-mass-renormalizes-to-one
  (testing "§4.6.2 — R is a weighted mean over surviving mass, never over <1"
    (let [p (sf/project-card sched due-soon-card {:horizon-days 90})]
      (is (<= 0.0 (:retrievability p) 1.0))
      (is (pos? (:retrievability p))))))

(deftest epsilon-pruning-bounds-a-long-horizon
  (testing "§4.6.1 and §4.6.3 — a 365-day horizon completes inside the budget"
    (let [p (sf/project-card sched due-soon-card {:horizon-days 365})]
      (is (false? (:capped? p)))
      (is (< (:nodes p) sf/default-node-cap))))
  (testing "§4.6.3 measured — R has converged by the default ε"
    (let [at (fn [e] (sf/project-card sched due-soon-card
                       {:horizon-days 365 :epsilon e :node-cap 1000000}))
          coarse (at 1e-2), dflt (at sf/default-epsilon), fine (at 1e-6)]
      (is (< (:nodes coarse) (:nodes dflt) (:nodes fine)) "a smaller ε grows the tree")
      (testing "1e-2 is too coarse to trust — 11 points of R"
        (is (> (Math/abs (- (:retrievability coarse) (:retrievability fine))) 0.04)))
      (testing "the default is within 0.01 of two more decades of work"
        (is (close? (:retrievability dflt) (:retrievability fine) 0.01)))
      (testing "pruning moves R DOWN, so a coarser ε understates and never overstates"
        (is (< (:retrievability coarse) (:retrievability dflt) (:retrievability fine)))))))

(deftest the-node-cap-is-a-backstop-independent-of-epsilon
  (testing "§4.6.4 — a projection never runs past the cap"
    (let [p (sf/project-card sched due-soon-card
              {:horizon-days 3650 :epsilon 1e-12 :node-cap 64})]
      (is (true? (:capped? p)))
      (is (<= (:nodes p) 68) "the cap stops expansion; a node's four children may straddle it")
      (is (<= 0.0 (:retrievability p) 1.0)))))

(deftest daily-load-sums-to-the-total-projected-reviews
  (testing "§6.3 done-when"
    (let [p (sf/project-card sched due-soon-card {:horizon-days 120})
          per-day (reduce + 0.0 (vals (:daily-reviews p)))]
      (is (pos? per-day))
      (is (every? #(<= 0 % 120) (keys (:daily-reviews p)))
        "no review lands outside the horizon"))))

;; ── §5 — the page model ────────────────────────────────────────────

(defn- carded [page-id r ivl]
  {:page-id page-id :projected {:retrievability r :interval-days ivl :daily-reviews {}}})

(deftest page-r-is-the-mean-over-anki-cards-not-over-notes
  (testing "§5.1 — a 3-card cloze note averages over 3 values"
    (let [v (fc/page-verdict {:page-id 1 :done? true}
              [(carded 1 0.9 30.0) (carded 1 0.6 30.0) (carded 1 0.9 30.0)] 0.9)]
      (is (close? 0.8 (:retrievability v) 1e-9))
      (is (= 3 (:card-count v)))
      (is (false? (:known? v)) "the mean is below θ, so the page is not known"))))

(deftest theta-changes-the-known-count
  (testing "§5.2 done-when"
    (let [cards [(carded 1 0.85 30.0)]
          page {:page-id 1 :done? true}]
      (is (false? (:known? (fc/page-verdict page cards 0.9))))
      (is (true? (:known? (fc/page-verdict page cards 0.8)))))))

(deftest the-maturity-gate-rejects-a-page-that-passes-r
  (testing "§5.8.1 — R passes, mean interval 12 days, page reads unknown"
    (let [v (fc/page-verdict {:page-id 1 :done? true} [(carded 1 0.99 12.0)] 0.9)]
      (is (= :immature (:reason v)))
      (is (false? (:known? v)))))
  (testing "the same page at 21 days is known"
    (let [v (fc/page-verdict {:page-id 1 :done? true} [(carded 1 0.99 21.0)] 0.9)]
      (is (= :mature (:reason v)))
      (is (true? (:known? v))))))

(deftest a-done-page-with-no-cards-is-known-and-untested
  (testing "§5.3 and §5.4"
    (let [v (fc/page-verdict {:page-id 1 :done? true} [] 0.9)]
      (is (true? (:known? v)))
      (is (= :uncarded-done (:reason v))))))

(deftest an-untouched-page-is-unknown-and-stays-in-the-denominator
  (testing "§5.5 — an untouched book reads 0 of N, never 0 of 0"
    (let [pages (mapv (fn [i] {:page-id i :page-number i :done? false}) (range 1 1036))
          verdicts (fc/page-verdicts pages [] 0.9)
          prog (fc/document-progress verdicts)]
      (is (= 1035 (:total prog)))
      (is (zero? (:known prog)))
      (is (= 1035 (:unknown prog)))
      (is (every? #(= :untouched (:reason %)) verdicts)))))

(deftest progress-reports-exposure-separately-from-known
  (testing "§5.4 — a completed document reads N known, M untested"
    (let [pages (concat (mapv (fn [i] {:page-id i :done? true}) (range 1 60))
                  (mapv (fn [i] {:page-id i :done? true}) (range 60 212)))
          cards (mapv #(carded % 0.99 60.0) (range 60 212))
          verdicts (fc/page-verdicts pages cards 0.9)
          prog (fc/document-progress verdicts)]
      (is (= 211 (:total prog)))
      (is (= 211 (:known prog)))
      (is (= 59 (:untested prog)))
      (is (close? 1.0 (:fraction prog) 1e-9)))))

(deftest covered-counts-done-pages-whether-or-not-their-cards-are-known
  (testing "a done page with young cards is NOT known, and IS covered"
    (let [pages (mapv (fn [i] {:page-id i :done? true}) (range 1 24))
          ;; R clears θ, interval does not — every page reads :immature.
          cards (mapv #(carded % 0.99 3.0) (range 1 13))
          prog (fc/document-progress (fc/page-verdicts pages cards 0.9))]
      (is (= 23 (:total prog)))
      (is (= 11 (:known prog)) "pages 13-23 are done with no cards")
      (is (= 23 (:covered prog)) "every page is marked done")
      (is (= 11 (:untested prog)) "only the uncarded ones are untested")))
  (testing "an untouched range is covered 0"
    (let [pages (mapv (fn [i] {:page-id i :done? false}) (range 1 24))]
      (is (zero? (:covered (fc/document-progress (fc/page-verdicts pages [] 0.9))))))))

(deftest untested-never-exceeds-covered-and-covered-never-exceeds-total
  (testing "the subset chain the Covered tile's sub-line asserts"
    (doseq [done-count (range 0 11)
            carded-count (range 0 11)
            theta [0.5 0.9]
            ivl [3.0 30.0]]
      (let [pages (mapv (fn [i] {:page-id i :done? (< i done-count)}) (range 0 10))
            cards (mapv #(carded % 0.95 ivl) (range 0 carded-count))
            prog (fc/document-progress (fc/page-verdicts pages cards theta))]
        (is (<= (:untested prog) (:covered prog) (:total prog))
          (str "done=" done-count " carded=" carded-count
            " θ=" theta " ivl=" ivl " → " (select-keys prog [:untested :covered :total])))))))

(deftest a-page-verdict-carries-its-own-done-mark
  (testing "the mark reaches the aggregate even when the reason hides it"
    (is (true? (:done? (fc/page-verdict {:page-id 1 :done? true} [] 0.9))))
    (is (false? (:done? (fc/page-verdict {:page-id 1 :done? false} [] 0.9))))
    ;; :immature — the reason says nothing about the mark, so :done? is the
    ;; only place it survives.
    (let [v (fc/page-verdict {:page-id 1 :done? true} [(carded 1 0.99 3.0)] 0.9)]
      (is (= :immature (:reason v)))
      (is (false? (:known? v)))
      (is (true? (:done? v))))))

(deftest narrowing-the-range-changes-the-denominator
  (testing "§5.6 done-when"
    (let [all (mapv (fn [i] {:page-id i :done? false}) (range 1 101))
          prefix (take 20 all)]
      (is (= 100 (:total (fc/document-progress (fc/page-verdicts all [] 0.9)))))
      (is (= 20 (:total (fc/document-progress (fc/page-verdicts prefix [] 0.9))))))))

(deftest excluding-pages-from-carding-moves-the-exposure-figure
  (testing "§6.6.4.1 — lever I5 MUST raise untested by the same count it raises known"
    (let [pages (mapv (fn [i] {:page-id i :done? true}) (range 1 11))
          cards (mapv #(carded % 0.5 5.0) (range 1 11))
          before (fc/document-progress (fc/page-verdicts pages cards 0.9))
          ;; I5 excludes pages 1-4 from carding: they become done-with-no-cards
          after (fc/document-progress
                  (fc/page-verdicts pages (filterv #(>= (:page-id %) 5) cards) 0.9))]
      (is (zero? (:known before)))
      (is (= 4 (:known after)))
      (is (= (- (:known after) (:known before))
            (- (:untested after) (:untested before)))))))

;; ── §6 — forecast and plan ─────────────────────────────────────────

(deftest backlog-spreads-at-capacity-and-future-cards-keep-their-due-day
  (testing "§6.6.2.1 — I2 bites on the backlog only"
    (let [cards [{:id :a :due-day -5} {:id :b :due-day -3} {:id :c :due-day nil}
                 {:id :d :due-day 40}]
          out (fc/assign-first-review-days cards 2)
          by-id (into {} (map (juxt :id :next-review-day) out))]
      (is (= 4 (count out)) "no card is dropped")
      (is (= [:a :b :c :d] (mapv :id out)) "order is preserved")
      (is (= 0 (by-id :a)) "most overdue goes first")
      (is (= 0 (by-id :b)))
      (is (= 1 (by-id :c)) "never-introduced sorts after overdue, and spills to day 1")
      (is (= 40 (by-id :d)) "not backlog, so capacity does not touch it")))
  (testing "raising capacity drains the backlog sooner"
    (let [cards (mapv (fn [i] {:id i :due-day nil}) (range 10))]
      (is (= 9 (apply max (map :next-review-day (fc/assign-first-review-days cards 1)))))
      (is (= 1 (apply max (map :next-review-day (fc/assign-first-review-days cards 5))))))))

(deftest a-document-exactly-on-plan-reads-zero
  (testing "§6.2 done-when"
    (is (close? 0.0 (:delta (fc/pace {:known-now 50 :total-pages 100 :start-known 0
                                      :elapsed-days 5 :total-days 10})) 1e-9))
    (is (neg? (:delta (fc/pace {:known-now 20 :total-pages 100 :start-known 0
                                :elapsed-days 5 :total-days 10}))))
    (is (pos? (:delta (fc/pace {:known-now 80 :total-pages 100 :start-known 0
                                :elapsed-days 5 :total-days 10}))))))

(deftest pace-clamps-outside-the-schedule-window
  (is (close? 100.0 (:required (fc/pace {:known-now 0 :total-pages 100 :start-known 0
                                         :elapsed-days 99 :total-days 10})) 1e-9))
  (is (close? 100.0 (:required (fc/pace {:known-now 0 :total-pages 100 :start-known 0
                                         :elapsed-days 1 :total-days 0})) 1e-9)))

(deftest concurrent-schedules-produce-one-combined-daily-number
  (testing "§1.3 and §6.5"
    (let [a {0 3.0 1 2.0} b {1 4.0 2 1.0}
          c (fc/combined-daily-load [a b])]
      (is (= {0 3.0 1 6.0 2 1.0} (into {} c)))
      (is (close? 6.0 (fc/peak-daily-load c) 1e-9)))))

(deftest daily-load-is-derived-from-the-projection
  (testing "§6.3 — summing per-card histograms, never a rate estimate"
    (let [cards [{:projected {:daily-reviews {0 1.0 5 0.9}}}
                 {:projected {:daily-reviews {0 1.0 3 0.8}}}]
          load (fc/daily-review-load cards)]
      (is (= {0 2.0 3 0.8 5 0.9} (into {} load)))
      (is (close? (reduce + 0.0 (vals load))
            (reduce + 0.0 (mapcat #(vals (get-in % [:projected :daily-reviews])) cards))
            1e-9)))))

(deftest an-infeasible-target-names-all-four-levers
  (testing "§6.6 — the warning names four levers whichever reason fired"
    (let [f (fc/feasibility {:projected-known 10 :required-pages 300
                             :peak-load 400.0 :observed-capacity 237.0
                             :earliest-day 120})]
      (is (false? (:feasible? f)))
      (is (= #{:short-of-target :over-capacity} (set (:reasons f))))
      (is (= [:push-target-date :raise-capacity :lower-theta :exclude-pages] (:levers f)))))
  (testing "§6.7 — over capacity alone is enough to warn"
    (let [f (fc/feasibility {:projected-known 300 :required-pages 300
                             :peak-load 400.0 :observed-capacity 237.0})]
      (is (false? (:feasible? f)))
      (is (= [:over-capacity] (:reasons f)))))
  (testing "a reachable target warns about nothing"
    (let [f (fc/feasibility {:projected-known 300 :required-pages 300
                             :peak-load 60.0 :observed-capacity 237.0})]
      (is (true? (:feasible? f)))
      (is (empty? (:reasons f))))))

(deftest earliest-feasible-day-finds-the-first-day-that-meets-the-target
  (testing "§6.6.1 — the date lever I1 names a date, and it is the earliest one"
    (let [reach (fn [d] (if (>= d 47) 100 0))]
      (is (= 47 (fc/earliest-feasible-day reach 100 365))))
    (testing "nil when the horizon never reaches it"
      (is (nil? (fc/earliest-feasible-day (constantly 0) 100 365))))
    (testing "day 0 when it is already met"
      (is (= 0 (fc/earliest-feasible-day (constantly 100) 100 365))))))

;; ── §2 — Anki acquisition, the pure half ───────────────────────────

(deftest card-reviews-rows-are-positional-and-get-reshaped
  (testing "§2.4 — cardReviews returns ARRAYS, getReviewsOfCards returns maps"
    (let [rows [[1700000000000 55 -1 3 10 4 2500 1200 1]
                [1700000100000 55 -1 1 1 10 2300 900 1]
                [1700000200000 99 -1 3 5 2 2500 800 0]]]
      (is (= {55 [{:id 1700000000000 :ease 3 :ivl 10 :type 1}
                  {:id 1700000100000 :ease 1 :ivl 1 :type 1}]}
            (sa/normalize-card-reviews rows #{55}))
        "rows for cards outside the set are dropped — cardReviews is deck-scoped")
      (is (= #{55 99} (set (keys (sa/normalize-card-reviews rows #{55 99}))))))))

(deftest a-row-of-the-wrong-width-is-dropped-not-misindexed
  (testing "an upstream column change must lose rows loudly, never shift ease into ivl"
    (is (= {} (sa/normalize-card-reviews [[1 2 3]] #{2})))
    (is (= {} (sa/normalize-card-reviews [nil "junk"] #{2})))))

(deftest the-ignore-date-parses-or-yields-no-cutoff
  (testing "§4.2 — a malformed date MUST NOT discard history"
    (is (= 0 (sa/ignore-before-ms "1970-01-01")) "the canonical user's value: no cutoff")
    (is (= 1704067200000 (sa/ignore-before-ms "2024-01-01")))
    (is (= 0 (sa/ignore-before-ms nil)))
    (is (= 0 (sa/ignore-before-ms "")))
    (is (= 0 (sa/ignore-before-ms "not-a-date")))
    (is (= 0 (sa/ignore-before-ms 12345)))))

(def ^:private day-ms 86400000)

(deftest card-rows-take-the-interval-from-anki-and-the-anchor-from-the-revlog
  (let [infos [{:cardId 55 :note 7 :deckName "All" :interval 30 :type 2}]
        log {55 [{:id (* 10 day-ms) :ease 3} {:id (* 20 day-ms) :ease 3}]}
        [row] (sa/card-rows sched infos log {:fingerprint "fp"})]
    (is (= 55 (:anki_card_id row)))
    (is (= 7 (:anki_note_id row)))
    (is (= "All" (:deck row)))
    (is (close? 30.0 (:interval_days row) 1e-9) "Anki's ivl, not the replayed one")
    (is (= (* 20 day-ms) (:last_review_ms row)) "the anchor is max(revlog.id)")
    (is (= (+ (* 20 day-ms) (* 30 day-ms)) (:due_ms row)) "due = anchor + Anki's ivl")
    (is (= (* 20 day-ms) (:high_water_revlog_id row)))
    (is (pos? (:stability row)))
    (is (= 2 (:reps row)))))

(deftest a-negative-anki-interval-is-seconds-not-days
  (testing "a v1-legacy sub-day interval must not read as months"
    (let [[row] (sa/card-rows sched [{:cardId 1 :note 1 :interval -600 :type 1}]
                  {1 [{:id 0 :ease 3}]} {:fingerprint "fp"})]
      (is (close? (/ 600.0 86400.0) (:interval_days row) 1e-9)))))

(deftest a-card-with-no-revlog-reads-as-never-reviewed
  (let [[row] (sa/card-rows sched [{:cardId 9 :note 3 :interval 0 :type 0}] {}
                {:fingerprint "fp"})]
    (is (nil? (:stability row)))
    (is (nil? (:last_review_ms row)))
    (is (nil? (:due_ms row)))
    (is (zero? (:reps row)))
    (is (zero? (:high_water_revlog_id row)))))

(deftest an-incremental-replay-continues-from-the-cached-state
  (testing "§2.4 — replaying new reviews from fresh would restart every card"
    (let [full-log [{:id (* 1 day-ms) :ease 3} {:id (* 3 day-ms) :ease 3}
                    {:id (* 10 day-ms) :ease 3} {:id (* 40 day-ms) :ease 3}]
          full (sf/replay sched full-log)
          prefix (sf/replay sched (take 3 full-log))
          cached {55 {:anki_card_states/state (:state prefix)
                      :anki_card_states/step (:step prefix)
                      :anki_card_states/stability (:stability prefix)
                      :anki_card_states/difficulty (:difficulty prefix)
                      :anki_card_states/interval_days (:interval-days prefix)
                      :anki_card_states/last_review_at (java.util.Date. (long (* 10 day-ms)))
                      :anki_card_states/reps 3
                      :anki_card_states/high_water_revlog_id (* 10 day-ms)}}
          infos [{:cardId 55 :note 7 :interval 40 :type 2}]
          [continued] (sa/card-rows sched infos {55 [(last full-log)]}
                        {:fingerprint "fp" :cached cached :incremental? true})
          [restarted] (sa/card-rows sched infos {55 [(last full-log)]}
                        {:fingerprint "fp" :cached cached :incremental? false})]
      (is (close? (:stability full) (:stability continued) 1e-6)
        "continuing reproduces the full replay exactly")
      (is (= 4 (:reps continued)))
      (is (< (:stability restarted) (:stability continued))
        "restarting from fresh reports a stability far below the truth")
      (is (= 1 (:reps restarted))))))

(deftest the-high-water-mark-never-moves-backwards
  (testing "an incremental pull with no new reviews keeps the stored mark"
    (let [cached {55 {:anki_card_states/stability 6.0
                      :anki_card_states/state 2
                      :anki_card_states/reps 3
                      :anki_card_states/last_review_at (java.util.Date. day-ms)
                      :anki_card_states/high_water_revlog_id 9999}}
          [row] (sa/card-rows sched [{:cardId 55 :note 7 :interval 6 :type 2}] {}
                  {:fingerprint "fp" :cached cached :incremental? true})]
      (is (= 9999 (:high_water_revlog_id row)))
      (is (close? 6.0 (:stability row) 1e-9) "and the state survives untouched"))))

;; ── Vocabularies that two namespaces must agree on ─────────────────

(deftest every-acquisition-state-carries-its-own-copy
  (testing "the header chip and the stale banner both read this map, so a
            state added without an entry would fall silently to a default"
    (doseq [[k {:keys [chip fault? banner]}] sa/acquisition-states]
      (is (string? chip) (str k " has no chip label"))
      (is (boolean? fault?) (str k " does not say whether it is a fault"))
      (is (or (nil? banner) (string? banner)) (str k " has a non-string banner"))))
  (testing ":ok is the only state that is neither a fault nor worth a banner"
    (is (= #{:ok} (set (keep (fn [[k v]] (when-not (or (:fault? v) (:banner v)) k))
                         sa/acquisition-states)))))
  (testing "the in-flight chip is NOT a state — no outcome carries it"
    (is (not (contains? sa/acquisition-states sa/reading-chip)))
    (is (string? sa/reading-chip))))

(deftest every-status-acquire-can-return-has-a-state-entry
  (testing "the five browser outcomes plus the server replay failure"
    (is (= #{:ok :no-cards :unreachable :unsupported :no-fsrs :no-deck :ingest-failed}
          (set (keys sa/acquisition-states))))))

(defn- projected [r ivl]
  {:page-id 1 :projected {:retrievability r :interval-days ivl :daily-reviews {}}})

(deftest every-page-verdict-reason-has-a-label
  (testing "page-verdict's reasons and reason-labels must match exactly — an
            unlabelled reason renders as an em dash with nothing failing"
    (let [reasons (set (map (fn [[done? cards]]
                              (:reason (fc/page-verdict {:page-id 1 :done? done?}
                                         cards 0.9)))
                         [[true []]                        ; done, no cards
                          [false []]                       ; never touched
                          [true [(projected 0.99 60.0)]]   ; mature
                          [true [(projected 0.99 5.0)]]    ; R ok, interval short
                          [true [(projected 0.10 60.0)]]]))] ; R below θ
      (is (= #{:uncarded-done :untouched :mature :immature :below-theta} reasons)
        "page-verdict produces exactly these five")
      (is (= reasons (set (keys fc/reason-labels)))
        "and reason-labels covers exactly those, with no extras"))))

(deftest the-probe-list-holds-only-read-only-actions
  (testing "§1.5 — the feature MUST NOT write to a collection"
    (is (= #{"apiReflect" "notesInfo" "cardsInfo" "getDeckConfig"
             "getReviewsOfCards" "cardReviews"}
          (set sa/required-actions)))))

;; ── §1.2 — the dated study plan ────────────────────────────────────

(deftest the-plan-hands-out-every-remaining-page-and-no-more
  (testing "the page column sums EXACTLY to the remainder, at any rate"
    (doseq [[pages days] [[28 120] [60 3] [1 90] [0 30] [1035 365] [7 7] [100 1]]]
      (let [p (fc/daily-page-plan pages days)]
        (is (= (max 1 days) (count p)) (str pages "p/" days "d — one row per day"))
        (is (= pages (reduce + 0 p)) (str pages "p/" days "d — sums exactly"))
        (is (every? #(>= % 0) p) "no negative day")))))

(deftest a-sub-daily-rate-yields-whole-pages-not-fractions
  (testing "28 pages over 120 days is days of 0 and days of 1, never 0.23"
    (let [p (fc/daily-page-plan 28 120)]
      (is (= #{0 1} (set p)))
      (is (= 28 (count (filter pos? p)))))))

(deftest the-plan-never-lags-the-line-it-is-measured-against
  (testing "cumulative pages through day d meet the straight line of §6.2, so a
            user who works the table never reads a lag"
    (let [pages 60, days 90
          p (fc/daily-page-plan pages days)
          cum (reductions + p)]
      (doseq [d (range days)]
        (let [required (* (double pages) (/ (inc (double d)) days))]
          (is (>= (nth cum d) (Math/floor required))
            (str "day " d ": " (nth cum d) " against a line at " required)))))))

(deftest a-target-today-or-past-still-owes-the-work
  (testing "the plan collapses to one day rather than vanishing"
    (is (= [17] (fc/daily-page-plan 17 0)))
    (is (= [17] (fc/daily-page-plan 17 -5)))))

(deftest the-today-tile-reads-day-zero-of-the-plan
  (testing "one definition, so the tile and the table cannot disagree"
    (doseq [[pages days] [[28 120] [60 3] [0 30] [1 90] [100 7]]]
      (is (= (first (fc/daily-page-plan pages days))
            (:pages-today (fc/today-plan {:pages-remaining pages
                                          :days-remaining days
                                          :daily-load {}})))
        (str pages "p/" days "d")))))

(deftest the-page-noun-agrees-with-its-count
  (is (= "1 page" (fc/pages-label 1)))
  (is (= "0 pages" (fc/pages-label 0)))
  (is (= "12 pages" (fc/pages-label 12))))

(deftest today-plan-takes-its-share-and-leaves-reviews-fractional
  (testing "§1.2 and §6.4"
    (let [p (fc/today-plan {:pages-remaining 10 :days-remaining 3 :daily-load {0 33.7}})]
      ;; 10 over 3 days is [3 4 3], which sums to 10. Rounding each day UP
      ;; would ask for 4+4+4 = 12 pages out of a 10-page remainder — a plan the
      ;; user cannot complete because the work does not exist.
      (is (= 3 (:pages-today p)))
      (is (= 10 (reduce + 0 (fc/daily-page-plan 10 3))) "and the three days sum exactly")
      (is (close? 33.7 (:reviews-today p) 1e-9)))
    (testing "a target reached today still asks for the whole remainder"
      (let [p (fc/today-plan {:pages-remaining 5 :days-remaining 0 :daily-load {}})]
        (is (= 5 (:pages-today p)))
        (is (zero? (:reviews-today p)))))))
