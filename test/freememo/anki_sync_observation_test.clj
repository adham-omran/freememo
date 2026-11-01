(ns freememo.anki-sync-observation-test
  (:require [clojure.test :refer [deftest is]]
            [hyperfiddle.electric3 :as e]
            [hyperfiddle.electric-local-def3 :as local]
            [freememo.anki-lease :as lease]
            [freememo.anki-sync-observation :as observation]
            [freememo.user-state :as us]))

(defmacro with-observation [[uid revision marker step] & body]
  `(let [~uid (- (long (rand-int 1000000000)))
         ~revision (us/get-atom ~uid :sync-mutations)
         ~marker (atom 0)
         engine# (local/->engine {:seed 913})
         tap# #(local/tap engine# %)
         ~step #(local/step engine# #{%})]
     (reset! ~revision 10)
     (reset! (us/get-atom ~uid :anki-run) {})
     (local/spawn engine#
       (local/local-ngn {} engine#
         (tap# [(e/watch ~marker) (observation/SyncObservation ~uid)])))
     (try ~@body
          (finally
            (local/cancel engine#)
            (reset! (us/get-atom ~uid :anki-run) {})
            (reset! (us/get-atom ~uid :anki-run-cancellers) {})))))

(deftest sixty-batches-observe-only-the-final-sync-revision
  (with-observation [uid revision marker step]
    (is (= [0 [false 10]] (step [0 [false 10]])))
    (let [{:keys [run-id]} (lease/claim! uid)]
      (is (= [0 [true 10]] (step [0 [true 10]])))
      (dotimes [i 60]
        (swap! revision inc)
        (reset! marker (inc i))
        (is (= [(inc i) [true 10]] (step [(inc i) [true 10]]))))
      (lease/release! uid run-id nil)
      (is (= [60 [false 70]] (step [60 [false 70]])))
      (swap! revision inc)
      (is (= [60 [false 71]] (step [60 [false 71]]))))))

(deftest cancellation-holds-the-revision-until-the-current-batch-drains
  (with-observation [uid revision marker step]
    (step [0 [false 10]])
    (let [{:keys [run-id]} (lease/claim! uid)]
      (step [0 [true 10]])
      (lease/request-cancel! uid run-id)
      (reset! revision 20)
      (reset! marker 1)
      (is (= [1 [true 10]] (step [1 [true 10]])))
      (lease/release! uid run-id nil)
      (is (= [1 [false 20]] (step [1 [false 20]]))))))

(deftest an-expired-driver-resumes-observation-without-a-lease-write
  (with-observation [uid revision marker step]
    (step [0 [false 10]])
    (lease/claim! uid)
    (step [0 [true 10]])
    (swap! (us/get-atom uid :anki-run)
      #(into {} (map (fn [[key value]]
                       [key (assoc value :heartbeat-at
                              (+ (- (System/currentTimeMillis) lease/ttl-ms) 1000))])) %))
    (reset! revision 30)
    (reset! marker 1)
    (step [1 [true 10]])
    (Thread/sleep 10100)
    (is (= [1 [false 30]] (step [1 [false 30]])))))
