(ns freememo.work-run-test
  (:require [clojure.test :refer [deftest is]]
            [freememo.work-run :as run]))

(deftest cancel-and-retry-never-revive-an-old-identity
  (let [old (run/start) new (run/start) writes (atom [])]
    (run/cancel! old)
    (is (:cancelled? (run/commit! old #(swap! writes conj :old))))
    (is (= [:new] (run/commit! new #(swap! writes conj :new))))
    (is (= [:new] @writes))
    (is (not= (:id old) (:id new)))))

(deftest expired-attempt-rejects-the-commit
  (let [attempt (assoc (run/start) :deadline (dec (System/nanoTime)))
        writes (atom 0)]
    (is (:timed-out? (run/commit! attempt #(swap! writes inc))))
    (is (zero? @writes))))

(deftest cancellation-cannot-interrupt-an-authorized-commit
  (let [attempt (run/start) entered (promise) release (promise)
        writer (future (run/commit! attempt #(do (deliver entered true) @release :saved)))]
    @entered
    (let [cancel (future (run/cancel! attempt))]
      (deliver release true)
      (is (= :saved @writer))
      @cancel
      (is (= :committed (run/state attempt))))))
