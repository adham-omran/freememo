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
  (reset! (us/get-atom uid :anki-run-errors) {}))

(use-fixtures :each (fn [f] (clear!) (try (f) (finally (clear!)))))

(defn- age-heartbeat!
  "Make the lease's last heartbeat `ms` old."
  [ms]
  (swap! (us/get-atom uid :anki-run) update lease/owner-key
    assoc :heartbeat-at (- (System/currentTimeMillis) ms)))

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
      ((registry/owner-cancel-fn "anki-reconcile" uid lease/owner-key))
      (is (= :cancel (lease/report-progress! uid run-id {:phase "writing" :done 200 :total 300})))
      (is (lease/busy? uid) "the lease holds until the driver releases it"))))

(deftest release-clears-every-exit
  (doseq [error [nil "Sync failed"]]
    (testing (str "release with error " (pr-str error))
      (let [{run-id :run-id} (lease/claim! uid)]
        (is (= :released (lease/release! uid run-id error)))
        (is (nil? (lease/current uid)))
        (is (not (lease/busy? uid)))
        (is (nil? (registry/owner-cancel-fn "anki-reconcile" uid lease/owner-key)))
        (is (= error (registry/work-error "anki-reconcile" uid lease/owner-key)))))))

(deftest a-claim-clears-the-previous-error
  (let [{run-id :run-id} (lease/claim! uid)]
    (lease/release! uid run-id "boom")
    (lease/claim! uid)
    (is (nil? (registry/work-error "anki-reconcile" uid lease/owner-key)))))

(deftest the-activity-reading-follows-the-lease
  (let [{run-id :run-id} (lease/claim! uid)]
    (lease/report-progress! uid run-id {:phase "writing" :done 100 :total 300})
    (testing "a live run reads its progress, and is cancellable"
      (let [state (registry/read-state "anki-reconcile" uid lease/owner-key)]
        (is (= {:phase "writing" :done 100 :total 300}
              (select-keys state [:phase :done :total])))
        (is (:cancellable state))
        (is (not (:stale? state)))))
    (testing "discovery names the run under its work key"
      (is (= [{:work-key lease/owner-key :subject-id nil}]
            (vec (registry/live-work "anki-reconcile" uid)))))
    (testing "a driver that stopped heartbeating reads stale, not gone — so the
              reporter neither refreshes nor closes the row as a success"
      (age-heartbeat! (inc lease/ttl-ms))
      (is (:stale? (registry/read-state "anki-reconcile" uid lease/owner-key))))
    (testing "a cancelled run reads as gone, so discovery opens no second row"
      (age-heartbeat! 0)
      (lease/request-cancel! uid run-id)
      (is (nil? (registry/read-state "anki-reconcile" uid lease/owner-key))))))

(deftest batches-cover-the-plan-in-order
  (let [ids (vec (range 250))]
    (is (= 3 (rec/batch-count ids)))
    (is (= 0 (rec/batch-count [])))
    (is (= ids (vec (mapcat #(rec/batch-at ids %) (range (rec/batch-count ids))))))
    (is (= 50 (count (rec/batch-at ids 2))))
    (is (= {:phase "writing" :done 100 :total 250} (rec/batch-progress ids 0)))
    (is (= {:phase "writing" :done 250 :total 250} (rec/batch-progress ids 2)))))
