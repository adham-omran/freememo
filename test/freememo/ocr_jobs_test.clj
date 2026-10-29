(ns freememo.ocr-jobs-test
  (:require [clojure.test :refer [deftest is]]
            [freememo.commands :as commands]
            [freememo.db :as db]
            [freememo.ocr :as ocr]
            [freememo.ocr-jobs :as jobs]
            [freememo.toasts :as toasts]
            [freememo.user-state :as us])
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
