(ns freememo.schedule-ownership-test
  (:require [clojure.test :refer [deftest is]]
            [freememo.schedule :as schedule]
            [freememo.schedule-fsrs :as sf]
            [freememo.fsrs :as fsrs]
            [freememo.db :as db]))

(deftest a-replaced-history-worker-cannot-write-the-shared-cache
  (let [owner (atom nil)
        old-id (schedule/start-acquisition! owner)
        started (promise) release (promise) writes (atom [])
        data {:deck-config {:fsrsParams6 fsrs/default-parameters :desiredRetention 0.9}
              :fingerprint "old" :info {:cardId 5 :note 7 :interval 3}
              :reviews [] :final? true}]
    (with-redefs [sf/replay (fn [& _] (deliver started true) @release nil)
                  db/upsert-anki-card-states! (fn [& args] (swap! writes conj args))]
      (let [old (future (schedule/ingest-history-chunk! 1 data owner old-id))]
        (is (true? (deref started 2000 false)))
        (let [new-id (schedule/start-acquisition! owner)]
          (deliver release true)
          (is (false? (:ok (deref old 2000 {:ok :timeout})))))
        (is (= [] @writes))))))

(deftest canceling-an-old-frame-cannot-cancel-its-replacement
  (let [owner (atom nil)
        first-id (schedule/start-acquisition! owner)
        second-id (schedule/start-acquisition! owner)]
    (schedule/cancel-acquisition! owner first-id)
    (is (= :committed (schedule/commit-acquisition! owner second-id (constantly :committed))))
    (schedule/cancel-acquisition! owner second-id)
    (is (thrown? clojure.lang.ExceptionInfo
          (schedule/commit-acquisition! owner second-id (fn [] (throw (AssertionError. "Obsolete write"))))))))
