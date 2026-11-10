(ns freememo.sync-client-stop-test
  "The sync client stops for a library removal
   (plans/local-install/issues/05-one-library-one-account.md). No database."
  (:require [clojure.test :refer [deftest is]]
            [freememo.sync-client :as sc]))

(deftest stop-scheduler-waits-for-a-running-sync-and-blocks-new-ones
  (let [started (promise) release (promise) runs (atom 0)]
    (with-redefs [sc/run-sync! (fn [] (swap! runs inc) (deliver started true) @release)
                  sc/set-state! (fn [& _] nil)
                  sc/status (fn [] {})]
      (let [sync (future (sc/sync-now!))
            _ @started
            stopper (future (sc/stop-scheduler!) :stopped)]
        (is (= :still-waiting (deref stopper 300 :still-waiting))
          "stop-scheduler! does not return while a sync runs")
        (deliver release true)
        @sync
        (is (= :stopped (deref stopper 5000 :timeout)))
        (sc/sync-now!)
        (is (= 1 @runs) "no sync starts after the stop")
        (sc/start-scheduler!)
        (sc/sync-now!)
        (is (= 2 @runs) "start-scheduler! allows syncs again")
        (sc/stop-scheduler!)))))
