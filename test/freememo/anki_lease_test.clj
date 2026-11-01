(ns freememo.anki-lease-test
  "The Anki sync lease and the Activity monitor's reading of it
   (plans/anki-sync-lease-batches.md §4.1, §4.2).

   Needs no database: the lease is a per-user atom, and the reporter's skip of a
   stale reading is exercised through `registry/read-state`, which the reporter
   consults."
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [freememo.activity :as activity]
   [freememo.activity-registry :as registry]
   [freememo.anki-lease :as lease]
   [freememo.anki-reconcile :as rec]
   [freememo.user-state :as us]))

(def ^:private uid
  "A user id no real session holds — `user-state` is one registry per process."
  -4712)

(defn- clear! []
  (reset! (us/get-atom uid :anki-run) {})
  (reset! (us/get-atom uid :anki-run-cancellers) {})
  (reset! (us/get-atom uid :anki-run-errors) {})
  (reset! (us/get-atom uid :anki-run-results) {}))

(use-fixtures :each (fn [f] (clear!) (try (f) (finally (clear!)))))

(def ^:private collection
  "The work key, and so the lease key, of a whole-collection run."
  "collection")

(defn- age-heartbeat!
  "Make the lease's last heartbeat `ms` old, whichever key holds it."
  [ms]
  (swap! (us/get-atom uid :anki-run) update-vals
    #(assoc % :heartbeat-at (- (System/currentTimeMillis) ms))))

(deftest the-lease-ttl-is-the-activity-staleness
  (is (= activity/stale-after-ms lease/ttl-ms)
    "the lease and the Activity row must agree on when a driver is dead"))

(deftest one-claim-wins
  (testing "two concurrent claims give exactly one run"
    (let [results (->> (range 16)
                    (mapv (fn [_] (future (lease/claim! uid))))
                    (mapv deref))]
      (is (= 1 (count (filter map? results))))
      (is (= 15 (count (filter #{:busy} results))))))
  (testing "a live lease refuses a new claim"
    (is (= :busy (lease/claim! uid)))
    (is (lease/busy? uid))))

(deftest an-expired-lease-is-free
  (let [{old :run-id} (lease/claim! uid)]
    (age-heartbeat! (inc lease/ttl-ms))
    (is (not (lease/busy? uid)))
    (let [{new :run-id} (lease/claim! uid)]
      (is (some? new))
      (is (not= old new))
      (testing "the replaced run can no longer write"
        (is (= :not-owner (lease/heartbeat! uid old)))
        (is (= :not-owner (lease/report-progress! uid old {:phase "writing" :done 1 :total 2})))
        (is (= :not-owner (lease/release! uid old "late")))
        (is (= new (:run-id (lease/current uid))))
        (is (nil? (:done (lease/current uid))))))))

(deftest progress-and-cancel-flow-through-the-checkpoint
  (let [{run-id :run-id} (lease/claim! uid)]
    (is (= :continue (lease/report-progress! uid run-id {:phase "writing" :done 100 :total 300})))
    (is (= {:phase "writing" :done 100 :total 300}
          (select-keys (lease/current uid) [:phase :done :total])))
    (testing "the canceller the registry exposes marks the run"
      ((registry/owner-cancel-fn "anki-reconcile" uid collection))
      (is (= :cancel (lease/report-progress! uid run-id {:phase "writing" :done 200 :total 300})))
      (is (lease/busy? uid) "the lease holds until the driver releases it"))))

(deftest release-clears-every-exit
  (doseq [error [nil "Sync failed"]]
    (testing (str "release with error " (pr-str error))
      (let [{run-id :run-id} (lease/claim! uid)]
        (is (= :released (lease/release! uid run-id error)))
        (is (nil? (lease/current uid)))
        (is (not (lease/busy? uid)))
        (is (nil? (registry/owner-cancel-fn "anki-reconcile" uid collection)))
        (is (= error (registry/work-error "anki-reconcile" uid collection)))))))

(deftest a-claim-clears-the-previous-error
  (let [{run-id :run-id} (lease/claim! uid)]
    (lease/release! uid run-id "boom")
    (lease/claim! uid)
    (is (nil? (registry/work-error "anki-reconcile" uid collection)))))

(deftest release-records-the-summary-for-the-reporter
  (let [{run-id :run-id} (lease/claim! uid nil 17)]
    (is (= {:work-key collection :activity-floor 17} (lease/owned-run uid run-id)))
    (is (= :released (lease/release! uid run-id nil "12 sent to Anki")))
    (is (= "12 sent to Anki" (registry/work-summary "anki-reconcile" uid collection)))
    (is (nil? (registry/work-error "anki-reconcile" uid collection)))
    (is (nil? (lease/owned-run uid run-id)) "a released run owns nothing")))

(deftest a-claim-clears-the-previous-summary
  (let [{run-id :run-id} (lease/claim! uid)]
    (lease/release! uid run-id nil "12 sent to Anki")
    (lease/claim! uid)
    (is (nil? (registry/work-summary "anki-reconcile" uid collection)))))

(deftest a-late-release-records-no-summary
  (let [{old :run-id} (lease/claim! uid)]
    (age-heartbeat! (inc lease/ttl-ms))
    (lease/claim! uid)
    (is (= :not-owner (lease/release! uid old nil "stale words")))
    (is (nil? (registry/work-summary "anki-reconcile" uid collection)))))

(deftest the-activity-reading-follows-the-lease
  (let [{run-id :run-id} (lease/claim! uid)]
    (lease/report-progress! uid run-id {:phase "writing" :done 100 :total 300})
    (testing "a live run reads its progress, and is cancellable"
      (let [state (registry/read-state "anki-reconcile" uid collection)]
        (is (= {:phase "writing" :done 100 :total 300}
              (select-keys state [:phase :done :total])))
        (is (:cancellable state))
        (is (not (:stale? state)))))
    (testing "discovery names the run under its work key"
      (is (= [{:work-key collection :subject-id nil}]
            (vec (registry/live-work "anki-reconcile" uid)))))
    (testing "a driver that stopped heartbeating reads stale, not gone — so the
              reporter neither refreshes nor closes the row as a success"
      (age-heartbeat! (inc lease/ttl-ms))
      (is (:stale? (registry/read-state "anki-reconcile" uid collection))))
    (testing "a cancelled run reads as gone, so discovery opens no second row"
      (age-heartbeat! 0)
      (lease/request-cancel! uid run-id)
      (is (nil? (registry/read-state "anki-reconcile" uid collection))))))

(deftest batches-cover-the-plan-in-order
  (let [ids (vec (range 250))]
    (is (= 3 (rec/batch-count ids)))
    (is (= 0 (rec/batch-count [])))
    (is (= ids (vec (mapcat #(rec/batch-at ids %) (range (rec/batch-count ids))))))
    (is (= 50 (count (rec/batch-at ids 2))))
    (is (= {:phase "writing" :done 100 :total 250} (rec/batch-progress ids 0)))
    (is (= {:phase "writing" :done 250 :total 250} (rec/batch-progress ids 2)))))

(deftest a-branch-run-and-a-full-run-share-one-lease
  (let [{run-id :run-id} (lease/claim! uid 42)]
    (testing "the branch run is keyed and discovered as its branch"
      (is (= #{"branch-42"} (set (keys @(us/get-atom uid :anki-run)))))
      (is (= [{:work-key "branch-42" :subject-id 42}]
            (vec (registry/live-work "anki-reconcile" uid))))
      (is (some? (registry/owner-cancel-fn "anki-reconcile" uid "branch-42"))))
    (testing "a whole-collection run is refused while the branch run lives"
      (is (= :busy (lease/claim! uid))))
    (testing "an expired branch lease is replaced, not kept beside the new one"
      (age-heartbeat! (inc lease/ttl-ms))
      (is (map? (lease/claim! uid)))
      (is (= #{collection} (set (keys @(us/get-atom uid :anki-run)))))
      (is (= :not-owner (lease/release! uid run-id nil))))))
