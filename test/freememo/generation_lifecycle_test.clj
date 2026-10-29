(ns freememo.generation-lifecycle-test
  (:require [clojure.test :refer [deftest is]]
            [freememo.cards :as cards]
            [freememo.commands :as commands]
            [freememo.content-toolbar-helpers :as helpers]
            [freememo.generation-queue :as queue]
            [freememo.settings :as settings]
            [freememo.toasts :as toasts]
            [freememo.user-state :as us])
  (:import [java.util.concurrent CountDownLatch TimeUnit]))

(defn- await! [latch]
  (is (.await latch 5 TimeUnit/SECONDS)))

(deftest cancel-retry-and-late-model-completion-use-distinct-attempts
  (let [scheduler (queue/create {}) uid 930021 tid 91
        entered-a (CountDownLatch. 1) entered-b (CountDownLatch. 1)
        release-a (CountDownLatch. 1) release-b (CountDownLatch. 1)
        empty (CountDownLatch. 1) writes (atom []) notices (atom [])
        item {:content "A" :card-type "basic" :user-id uid :topic-id tid :root-topic-id tid}]
    (add-watch (:state scheduler) ::drained
      (fn [_ _ before after]
        (when (and (seq (:items before)) (empty? (:items after))) (.countDown empty))))
    (with-redefs [helpers/card-gen-queue scheduler
                  settings/get-model (constantly "test")
                  settings/get-learning-goal (constantly nil)
                  helpers/resolve-gen-content (fn [content & _] {:content content})
                  cards/generate-basic-cards
                  (fn [opts]
                    (if (= "A" (:content opts))
                      (do (.countDown entered-a) (.await release-a 5 TimeUnit/SECONDS))
                      (do (.countDown entered-b) (.await release-b 5 TimeUnit/SECONDS)))
                    {:success true :cards [(:content opts)]})
                  helpers/save-cards-for-topic
                  (fn [_ _ _ _ generated & _] (swap! writes into generated) {:success true :ids [1]})
                  commands/bump! (fn [& _])
                  us/work-started! (fn [& _])
                  toasts/push! (fn [_ notice] (swap! notices conj notice))]
      (let [stop! (helpers/start-generation-processor! scheduler)]
        (try
          (is (= :enqueued (helpers/enqueue-card-gen! item)))
          (await! entered-a)
          ((get @(us/get-atom uid :card-gen-cancellers) tid))
          (is (= :enqueued (helpers/enqueue-card-gen! (assoc item :content "B"))))
          (await! entered-b)
          (.countDown release-a)
          (.countDown release-b)
          (await! empty)
          (is (= ["B"] @writes))
          (is (empty? @notices))
          (is (zero? (:pending (get @(us/get-atom uid :card-gen-status) tid))))
          (finally (.countDown release-a) (.countDown release-b) (stop!)))))))

(deftest a-worker-failure-releases-capacity-and-advances-another-user
  (let [scheduler (queue/create {:items 2}) uid 930022 complete (CountDownLatch. 1)
        empty (CountDownLatch. 1) notices (atom [])]
    (add-watch (:state scheduler) ::drained
      (fn [_ _ before after]
        (when (and (seq (:items before)) (empty? (:items after))) (.countDown empty))))
    (with-redefs [helpers/card-gen-queue scheduler
                  helpers/generate-and-save! (fn [item]
                                               (if (= uid (:user-id item))
                                                 (throw (ex-info "stub failure" {}))
                                                 (do (.countDown complete) {:success true :ids []})))
                  commands/bump! (fn [& _])
                  us/work-started! (fn [& _])
                  toasts/push! (fn [_ notice] (swap! notices conj notice))]
      (is (= :enqueued (helpers/enqueue-card-gen! {:user-id uid :topic-id 1 :content "fail"})))
      (is (= :enqueued (helpers/enqueue-card-gen! {:user-id (inc uid) :topic-id 2 :content "next"})))
      (let [stop! (helpers/start-generation-processor! scheduler)]
        (try (await! complete) (await! empty)
             (is (zero? (:items (queue/snapshot scheduler))))
             (is (= "stub failure" (:message (first @notices))))
             (finally (stop!)))))))

(deftest rejection-never-publishes-an-activity-and-always-shows-a-toast
  (let [scheduler (queue/create {:items 1}) uid 930023 notices (atom []) started (atom 0)]
    (with-redefs [helpers/card-gen-queue scheduler
                  us/work-started! (fn [_] (swap! started inc))
                  toasts/push! (fn [_ notice] (swap! notices conj notice))]
      (is (= :enqueued (helpers/enqueue-card-gen! {:user-id uid :topic-id 1 :content "accepted"})))
      (is (= :queue-full (:error-type (helpers/enqueue-card-gen!
                                       {:user-id uid :topic-id 2 :content "rejected"}))))
      (is (= 1 @started))
      (is (= 1 (count @notices)))
      (is (= :error (:level (first @notices))))
      (is (nil? (get @(us/get-atom uid :card-gen-status) 2)))
      (is (nil? (get @(us/get-atom uid :card-gen-cancellers) 2))))))
