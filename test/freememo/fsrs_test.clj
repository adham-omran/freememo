(ns freememo.fsrs-test
  "Conformance of the FSRS-6 port against ground-truth vectors generated from
   the reference implementation (py-fsrs, fuzz disabled). If py-fsrs and this
   namespace disagree on any vector, the port is wrong — that's the whole test.
   Regenerate vectors with scratchpad/gen_vectors.py."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [freememo.fsrs :as fsrs]))

(def vectors (edn/read-string (slurp (io/resource "freememo/fsrs_vectors.edn"))))
(def params fsrs/default-parameters)
(def sched fsrs/default-scheduler)

(def ^:private eps 1e-9)
(defn- close? [a b] (< (Math/abs (- (double a) (double b))) eps))

;; private formula vars, exercised in isolation for sharp falsification
(def ^:private ->recall  #'fsrs/next-recall-stability)
(def ^:private ->forget  #'fsrs/next-forget-stability)
(def ^:private ->short   #'fsrs/short-term-stability)
(def ^:private ->nextd   #'fsrs/next-difficulty)

(deftest params-match-reference
  (is (= params (:params vectors)))
  (is (close? 0.1542 (nth params 20))))

(deftest initial-stability-and-difficulty
  (doseq [{:keys [rating stability difficulty]} (:initial vectors)]
    (let [c (fsrs/review-card sched fsrs/new-card rating 0 nil)]
      (is (close? stability (:stability c)) (str "S0 rating=" rating))
      (is (close? difficulty (:difficulty c)) (str "D0 rating=" rating)))))

(deftest retrievability-curve
  (doseq [{:keys [stability elapsed_days r]} (:retrievability vectors)]
    (is (close? r (fsrs/retrievability sched stability elapsed_days))
      (str "R S=" stability " t=" elapsed_days))))

(deftest next-difficulty-vectors
  (doseq [{:keys [difficulty rating d_out]} (:next-difficulty vectors)]
    (is (close? d_out (->nextd params difficulty rating))
      (str "D' D=" difficulty " r=" rating))))

(deftest recall-stability-vectors
  (doseq [{:keys [difficulty stability retrievability rating s_out]} (:recall-stability vectors)]
    (is (close? s_out (->recall params difficulty stability retrievability rating))
      (str "S+ D=" difficulty " S=" stability " R=" retrievability " r=" rating))))

(deftest forget-stability-vectors
  (doseq [{:keys [difficulty stability retrievability s_out]} (:forget-stability vectors)]
    (is (close? s_out (->forget params difficulty stability retrievability))
      (str "S- D=" difficulty " S=" stability " R=" retrievability))))

(deftest short-term-stability-vectors
  (doseq [{:keys [stability rating s_out]} (:short-term vectors)]
    (is (close? s_out (->short params stability rating))
      (str "Sst S=" stability " r=" rating))))

(deftest next-interval-vectors
  (doseq [{:keys [stability retention interval]} (:next-interval vectors)]
    (is (= interval (fsrs/next-interval-days (fsrs/make-scheduler {:desired-retention retention}) stability))
      (str "ivl S=" stability " ret=" retention))))

(deftest fuzz-bounds
  ;; No fuzz below 2.5 days, whatever the rng.
  (is (= 1 (fsrs/fuzz-interval-days sched 1 0.99)))
  (is (= 2 (fsrs/fuzz-interval-days sched 2 0.01)))
  ;; interval 100d → delta 6.975 → [93,107]; rnd 0.0 lands on the floor.
  (is (= 93 (fsrs/fuzz-interval-days sched 100 0.0)))
  ;; deterministic in rnd, and always ≥ 2 and within the reachable band.
  (doseq [rnd [0.0 0.25 0.5 0.75 0.999]]
    (let [v (fsrs/fuzz-interval-days sched 100 rnd)]
      (is (= v (fsrs/fuzz-interval-days sched 100 rnd)) "deterministic")
      (is (<= 93 v 108) (str "in band, rnd=" rnd)))))

(deftest review-sequences
  (doseq [{:keys [name steps]} (:sequences vectors)]
    (testing name
      (reduce
        (fn [card {:keys [rating days_since elapsed_days state_out step_out
                          stability_out difficulty_out interval_seconds]}]
          (let [ds  (when (>= days_since 0) days_since)   ; -1 sentinel == nil
                res (fsrs/review-card sched card rating elapsed_days ds)]
            (is (= state_out (:state res)) (str name " :state"))
            (is (= (when (>= step_out 0) step_out) (:step res)) (str name " :step"))
            (is (close? stability_out (:stability res)) (str name " :stability"))
            (is (close? difficulty_out (:difficulty res)) (str name " :difficulty"))
            (is (= interval_seconds (:interval-seconds res)) (str name " :interval-seconds"))
            (select-keys res [:state :step :stability :difficulty])))
        fsrs/new-card
        steps))))
