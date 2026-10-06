(ns freememo.schedule-end-state-test
  "Three properties of the Study Scheduler, each of which shipped broken.

   1. Which schedule a reading view speaks for, when a document has more than
      one. The pill named the soonest target, which on a book split into a
      finished first half and a live second half is the half the reader has
      already left.
   2. A target date that has passed. `days-until` floors at 0, so `feasibility`
      could not tell a target three weeks gone from one today, and reported the
      lapse as a maturation window too short — a sentence about a horizon in
      days when the horizon is over.
   3. The overdue readout names its magnitude, because the day after a target
      and four months after it call for different actions.

   No database and no Anki: every fn under test is pure, which is why they live
   in `freememo.schedule-forecast` rather than beside the queries that feed
   them."
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.schedule-forecast :as fc]))

;; ---------------------------------------------------------------------------
;; Which schedule the reading view is about
;; ---------------------------------------------------------------------------

(def ^:private first-half
  "Pages 1–100, target forty days gone."
  {:id 1 :first-page 1 :last-page 100 :days-to-target -40})

(def ^:private second-half
  "Pages 101 to the end, target twelve days out."
  {:id 2 :first-page 101 :last-page nil :days-to-target 12})

(deftest the-page-in-view-outranks-the-nearest-deadline
  (testing "a page inside a range picks that range's schedule"
    (is (= 2 (:id (fc/pick-for-page [first-half second-half] 150))))
    (is (= 1 (:id (fc/pick-for-page [first-half second-half] 50)))))
  (testing "and it does so even when that schedule's target has passed —
            `overdue` about the right pages beats on-time about the wrong ones"
    (is (= 1 (:id (fc/pick-for-page [first-half second-half] 100)))))
  (testing "the order the rows arrive in does not decide"
    (is (= 2 (:id (fc/pick-for-page [second-half first-half] 150))))))

(deftest an-open-ended-range-runs-to-the-document-end
  (is (fc/covers-page? {:first-page 101 :last-page nil} 4000))
  (is (fc/covers-page? {:first-page nil :last-page 100} 1))
  (is (fc/covers-page? {:first-page nil :last-page nil} 7))
  (is (not (fc/covers-page? {:first-page 101 :last-page nil} 100))))

(deftest a-nil-page-is-inside-no-range
  (testing "a non-PDF and the PDF root's own screen report no page. Answering
            true would make every schedule equally relevant instead of none,
            which is the same thing as having no rule at all"
    (is (not (fc/covers-page? {:first-page nil :last-page nil} nil)))))

(deftest with-no-page-a-live-schedule-beats-an-expired-one
  (testing "this is the fallback, and it is already better than soonest-target:
            the expired row has the SOONER date and must still lose"
    (is (= 2 (:id (fc/pick-for-page [first-half second-half] nil))))))

(deftest among-expired-schedules-the-most-recent-lapse-wins
  (testing "`abs` reads in both directions: days remaining while live, days
            since once expired"
    (is (= 2 (:id (fc/pick-for-page [{:id 1 :days-to-target -90}
                                     {:id 2 :days-to-target -3}]
                    nil)))))
  (testing "and among live ones the soonest deadline still wins"
    (is (= 1 (:id (fc/pick-for-page [{:id 1 :days-to-target 3}
                                     {:id 2 :days-to-target 90}]
                    nil))))))

(deftest a-full-tie-breaks-on-id-so-the-choice-is-stable
  (is (= 4 (:id (fc/pick-for-page [{:id 9 :days-to-target 5}
                                   {:id 4 :days-to-target 5}]
                  nil)))))

(deftest no-schedules-means-no-badge
  (is (nil? (fc/pick-for-page [] 12)))
  (is (nil? (fc/pick-for-page nil 12))))

;; ---------------------------------------------------------------------------
;; A target date that has passed
;; ---------------------------------------------------------------------------

(defn- feas
  "`feasibility` over the shape a real past-target schedule produced: 7 of 23
   pages known, an 11-day maturation lead, and a horizon floored to 0."
  [past?]
  (fc/feasibility {:projected-known 7 :required-pages 23 :reachable-pages 23
                   :peak-load 0.0 :observed-capacity 60 :horizon 0
                   :past-target? past? :maturation-lead 11}))

(deftest a-passed-target-suppresses-the-maturation-window-reason
  (testing "both describe a horizon shorter than the lead, and only one of them
            is the fact the reader needs"
    (let [r (set (:reasons (feas true)))]
      (is (contains? r :past-target))
      (is (not (contains? r :no-maturation-window)))))
  (testing "a target TODAY still raises it — the window really is too short,
            and the date can still move"
    (let [r (set (:reasons (feas false)))]
      (is (contains? r :no-maturation-window))
      (is (not (contains? r :past-target))))))

(deftest a-passed-target-keeps-its-shortfall
  (testing "`:short-of-target` is what makes `earliest-feasible-day` run, and
            that search is the date the panel needs in order to say when the
            covered pages become known"
    (is (contains? (set (:reasons (feas true))) :short-of-target))))

(deftest the-flag-is-echoed-so-a-surface-can-switch-panels-on-it
  (is (true? (:past-target? (feas true))))
  (is (false? (:past-target? (feas false)))))

(deftest a-passed-target-is-never-feasible
  (is (false? (:feasible? (feas true)))))

(deftest a-schedule-that-reached-everything-still-reports-the-lapse
  (testing "nothing is short and nothing is over capacity, so `:past-target` is
            the only reason — which is the panel's 'this schedule is finished'
            branch"
    (let [f (fc/feasibility {:projected-known 23 :required-pages 23
                             :reachable-pages 23 :peak-load 0.0
                             :observed-capacity 60 :horizon 0
                             :past-target? true :maturation-lead 11})]
      (is (= [:past-target] (:reasons f)))
      (is (nil? (:earliest-day f))))))

;; ---------------------------------------------------------------------------
;; The deadline readout
;; ---------------------------------------------------------------------------

(deftest the-overdue-token-names-its-magnitude
  (is (= "12d overdue" (fc/deadline-token -12)))
  (is (= "1d overdue" (fc/deadline-token -1)))
  (is (= "today" (fc/deadline-token 0)))
  (is (= "5d" (fc/deadline-token 5)))
  (testing "nil has no deadline to miss"
    (is (= "today" (fc/deadline-token nil)))))

(deftest the-badge-face-keeps-its-today-anchor-past-the-target
  (testing "the label drops only when the day token is itself `today`, so an
            overdue face still anchors its quota"
    (is (= "today 3 pages, 12d overdue" (fc/schedule-badge-face 3 -12)))
    (is (= "3 pages, today" (fc/schedule-badge-face 3 0)))))
