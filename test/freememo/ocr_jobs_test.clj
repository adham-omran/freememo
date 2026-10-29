(ns freememo.ocr-jobs-test
  (:require [clojure.test :refer [deftest is]]
            [freememo.commands :as commands]
            [freememo.db :as db]
            [freememo.ocr :as ocr]
            [freememo.ocr-jobs :as jobs]
            [freememo.toasts :as toasts]
            [freememo.user-state :as us]
            [freememo.work-run :as run])
  (:import [java.util.concurrent CountDownLatch TimeUnit]))

(defn- await! [latch]
  (is (.await latch 5 TimeUnit/SECONDS)))

(defn- drain-interrupts [gate]
  (loop []
    (when-not (try (.await gate 5 TimeUnit/SECONDS)
                  (catch InterruptedException _ false))
      (recur))))

(deftest canceled-ocr-cannot-save-or-clean-up-its-replacement
  (let [uid 930011 key [71 1] entered-a (CountDownLatch. 1)
        release-a (CountDownLatch. 1) entered-b (CountDownLatch. 1)
        release-b (CountDownLatch. 1) saved-b (CountDownLatch. 1)
        writes (atom []) call (atom 0) notices (atom [])
        old-settled (CountDownLatch. 1)
        executor jobs/executor
        observed-executor (reify java.util.concurrent.Executor
                            (execute [_ runnable]
                              (.execute executor
                                (fn [] (.run runnable) (.countDown old-settled)))))]
    (with-redefs [jobs/executor observed-executor
                  db/get-topic-file (fn [_] {:topic_files/file_data (byte-array 0)})
                  ocr/extract-text (fn [& _]
                                     (if (= 1 (swap! call inc))
                                       (do (.countDown entered-a) (drain-interrupts release-a)
                                           {:success true :text "old"})
                                       (do (.countDown entered-b) (drain-interrupts release-b)
                                           {:success true :text "new"})))
                  db/save-page-text! (fn [_ _ text] (swap! writes conj text) (.countDown saved-b))
                  commands/bump! (fn [& _])
                  us/work-started! (fn [& _])
                  toasts/push! (fn [_ notice] (swap! notices conj notice))]
      (jobs/start! uid 71 1 nil 150)
      (await! entered-a)
      ((get @(us/get-atom uid :scan-cancellers) key))
      (jobs/start! uid 71 1 nil 150)
      (await! entered-b)
      (.countDown release-a)
      (await! old-settled)
      (is (empty? @writes))
      (is (contains? @(us/get-atom uid :scanning-pages) key))
      (is (contains? @(us/get-atom uid :scan-cancellers) key))
      (.countDown release-b)
      (await! saved-b)
      (is (= ["new"] @writes))
      (is (empty? @notices)))))

(deftest late-noninterruptible-ocr-rejects-the-save-after-its-deadline
  (let [uid 930012 entered (CountDownLatch. 1) release (CountDownLatch. 1)
        settled (CountDownLatch. 1) writes (atom []) notices (atom [])]
    (with-redefs [db/get-topic-file (fn [_] {:topic_files/file_data (byte-array 0)})
                  ocr/extract-text (fn [& _]
                                     (.countDown entered)
                                     (drain-interrupts release)
                                     {:success true :text "late"})
                  db/save-page-text! (fn [& xs] (swap! writes conj xs))
                  commands/bump! (fn [& _])
                  us/work-started! (fn [& _])
                  toasts/push! (fn [_ notice] (swap! notices conj notice) (.countDown settled))]
      (binding [jobs/*timeout-ms* 1] (jobs/start! uid 72 1 nil 150))
      (await! entered)
      (Thread/sleep 5)
      (.countDown release)
      (await! settled)
      (is (empty? @writes))
      (is (= "Scan timed out after 30 seconds" (:message (first @notices)))))))

(defn- queued-attempt [action]
  (let [uid (if (= action :cancel) 930013 930014)
        doc (if (= action :cancel) 73 74)
        key [doc 1]
        executor jobs/executor
        occupied (CountDownLatch. 3)
        release (CountDownLatch. 1)
        drained (CountDownLatch. 3)
        settled (CountDownLatch. 1)
        file-reads (atom 0) model-calls (atom 0) writes (atom 0) notices (atom [])
        observed (reify java.util.concurrent.Executor
                   (execute [_ runnable]
                     (.execute executor
                       (fn []
                         (try (.run runnable)
                              (finally (.countDown settled)))))))]
    (dotimes [_ 3]
      (.execute executor
        (fn []
          (.countDown occupied)
          (try (drain-interrupts release)
               (finally (.countDown drained))))))
    (try
      (await! occupied)
      (with-redefs [jobs/executor observed
                    db/get-topic-file (fn [_]
                                        (swap! file-reads inc)
                                        {:topic_files/file_data (byte-array 0)})
                    ocr/extract-text (fn [& _]
                                       (swap! model-calls inc)
                                       {:success true :text "Queued response"})
                    db/save-page-text! (fn [& _] (swap! writes inc))
                    commands/bump! (fn [& _])
                    us/work-started! (fn [& _])
                    toasts/push! (fn [_ notice] (swap! notices conj notice))]
        (binding [jobs/*timeout-ms* (if (= action :cancel) 30000 1)]
          (jobs/start! uid doc 1 nil 150))
        (let [attempt (get @(us/get-atom uid :scan-runs) key)]
          (is (some? attempt))
          (if (= action :cancel)
            ((get @(us/get-atom uid :scan-cancellers) key))
            (loop []
              (when (= :active (run/state attempt))
                (Thread/yield)
                (recur))))
          (is (= (if (= action :cancel) :cancelled :timed-out) (run/state attempt))))
        (.countDown release)
        (await! settled)
        (is (zero? @file-reads))
        (is (zero? @model-calls))
        (is (zero? @writes))
        (is (not (contains? @(us/get-atom uid :scanning-pages) key)))
        (if (= action :cancel)
          (is (empty? @notices))
          (is (= "Scan timed out after 30 seconds" (:message (first @notices))))))
      (finally (.countDown release) (await! drained)))))

(deftest canceled-queued-ocr-starts-no-file-read-or-model-call
  (queued-attempt :cancel))

(deftest expired-queued-ocr-starts-no-file-read-or-model-call
  (queued-attempt :deadline))
