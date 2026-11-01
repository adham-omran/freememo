(ns freememo.generation-queue-test
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.generation-queue :as queue]
            [freememo.work-run :as run]))

(defn- item [uid tid text]
  (let [attempt (run/start)]
    {:id (:id attempt) :run attempt :user-id uid :topic-id tid :content text}))

(defn- admit! [scheduler work]
  (queue/admit! scheduler work (fn [])))

(defn- take! [scheduler]
  (let [result (promise)]
    ((queue/take-task scheduler) #(deliver result %) #(deliver result %))
    (deref result 1000 ::timeout)))

(deftest users-alternate-with-fifo-order-within-each-user
  (let [scheduler (queue/create {})
        a (mapv #(item 1 % "a") (range 9))
        b (item 2 0 "b")]
    (doseq [work a] (is (:ok (admit! scheduler work))))
    (is (= (mapv :id (take 3 a)) (mapv :id (repeatedly 3 #(take! scheduler)))))
    (is (:ok (admit! scheduler b)))
    (is (= (:id (nth a 3)) (:id (take! scheduler))))
    (is (= (:id b) (:id (take! scheduler))))
    (is (= (mapv :id (drop 4 a)) (mapv :id (repeatedly 5 #(take! scheduler)))))))

(deftest every-admission-budget-rejects-without-publishing-membership
  (doseq [[limits first-item next-item]
          [[{:items 1} (item 1 1 "a") (item 2 1 "a")]
           [{:user-items 1} (item 1 1 "a") (item 1 2 "a")]
           [{:bytes 4099} (item 1 1 "a") (item 2 1 "a")]
           [{:user-bytes 4099} (item 1 1 "a") (item 1 2 "a")]
           [{:item-bytes 2050} (item 1 1 "a") (item 2 1 "aa")]]]
    (let [scheduler (queue/create limits) published (atom 0)]
      (is (:ok (admit! scheduler first-item)))
      (let [before (queue/snapshot scheduler)]
        (is (= :queue-full (:error-type
                            (queue/admit! scheduler next-item #(swap! published inc)))))
        (is (zero? @published))
        (is (= before (queue/snapshot scheduler)))))))

(deftest admission-reservations-are-atomic-under-contention
  (let [scheduler (queue/create {:items 8})
        results (doall (map deref
                         (repeatedly 40 #(future (admit! scheduler (item (random-uuid) 1 "x"))))))]
    (is (= 8 (count (filter :ok results))))
    (is (= 8 (:items (queue/snapshot scheduler))))))

(deftest cancel-releases-waiting-capacity-and-reserves-draining-requests
  (let [scheduler (queue/create {:items 3})
        running (item 1 7 "a") queued (item 1 7 "b") other (item 2 8 "c")]
    (is (:ok (admit! scheduler running)))
    (is (= (:id running) (:id (take! scheduler))))
    (is (:ok (admit! scheduler queued)))
    (is (= 2 (queue/cancel-topic! scheduler 1 7)))
    (is (= {:items 1 :bytes (queue/payload-bytes running) :queued 0 :running 1}
           (queue/snapshot scheduler)))
    (is (= {:pending 0 :active-count 0 :active-id nil} (queue/topic-state scheduler 1 7)))
    (is (:cancelled? (run/commit! (:run running) #(throw (Exception. "Must not save")))))
    (is (:ok (admit! scheduler other)))
    (is (= (:id other) (:id (take! scheduler))))
    (queue/finish! scheduler (:id running))
    (queue/finish! scheduler (:id running))
    (queue/finish! scheduler (:id other))
    (is (zero? (:items (queue/snapshot scheduler))))))

(deftest a-canceled-worker-does-not-consume-the-next-item
  (let [scheduler (queue/create {}) outcomes (atom [])
        cancel! ((queue/take-task scheduler) #(swap! outcomes conj %)
                 (fn [_] (swap! outcomes conj :cancelled)))
        work (item 2 9 "c")]
    (cancel!)
    (is (= [:cancelled] @outcomes))
    (is (:ok (admit! scheduler work)))
    (is (= (:id work) (:id (take! scheduler))))))

(deftest failed-publish-rolls-back-the-reservation
  (let [scheduler (queue/create {})]
    (is (thrown? Exception
          (queue/admit! scheduler (item 1 1 "a") #(throw (Exception. "publish failed")))))
    (is (zero? (:items (queue/snapshot scheduler))))))

(deftest sustained-excess-submission-keeps-retained-payloads-within-the-budget
  (let [scheduler (queue/create {:items 1000 :user-items 100 :bytes 131072})
        admitted (atom 0)]
    (dotimes [n 5000]
      (let [result (admit! scheduler (item (mod n 20) n (apply str (repeat 10000 "x"))))]
        (when (:ok result) (swap! admitted inc))
        (is (<= (:bytes (queue/snapshot scheduler)) 131072))))
    (is (= 5 @admitted))
    (doseq [[_ {:keys [item]}] (:items @(:state scheduler))]
      (queue/cancel-topic! scheduler (:user-id item) (:topic-id item)))
    (is (= {:items 0 :bytes 0 :queued 0 :running 0} (queue/snapshot scheduler)))))
