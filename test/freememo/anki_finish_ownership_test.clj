(ns freememo.anki-finish-ownership-test
  (:require [clojure.test :refer [deftest is]]
            [freememo.anki-reconcile-server :as recon]
            [freememo.anki-lease :as lease]
            [freememo.user-state :as us]))

(defn finish-with-verdict [verdict]
  (let [!calls (atom [])
        step (fn [k] (fn [& _] (swap! !calls conj k) {:success true :count 1}))]
    (with-redefs [lease/report-progress! (fn [uid run progress]
                                          (swap! !calls conj [uid run progress])
                                          verdict)
                  recon/apply-pulls! (step :pulls)
                  recon/backfill-baselines! (step :backfill)
                  recon/set-conflicts! (step :conflicts)
                  recon/finish-tombstones! (step :tombstones)]
      {:result (recon/finish-run! 101 "current-run" {}) :calls @!calls})))

(deftest canceled-and-replaced-drivers-cannot-finish
  (doseq [verdict [:cancel :not-owner]]
    (let [{:keys [result calls]} (finish-with-verdict verdict)]
      (is (false? (:success result)))
      (is (= verdict (:verdict result)))
      (is (= [[101 "current-run" {:phase "finishing"}]] calls)))))

(deftest the-current-driver-can-finish
  (let [{:keys [result calls]} (finish-with-verdict :continue)]
    (is (:success result))
    (is (= [[101 "current-run" {:phase "finishing"}]
            :pulls :backfill :conflicts :tombstones] calls))))

(deftest a-real-canceled-or-replaced-lease-blocks-all-finish-writes
  (let [uid -671390
        !calls (atom [])
        step (fn [& _] (swap! !calls conj :write) {:success true :count 0})]
    (try
      (with-redefs [recon/apply-pulls! step
                    recon/backfill-baselines! step
                    recon/set-conflicts! step
                    recon/finish-tombstones! step]
        (let [{run-id :run-id} (lease/claim! uid)]
          (lease/request-cancel! uid run-id)
          (is (= :cancel (:verdict (recon/finish-run! uid run-id {}))))
          (lease/release! uid run-id nil)
          (lease/claim! uid)
          (is (= :not-owner (:verdict (recon/finish-run! uid run-id {}))))
          (is (empty? @!calls))))
      (finally
        (doseq [k [:anki-run :anki-run-cancellers :anki-run-errors]]
          (reset! (us/get-atom uid k) {}))))))
