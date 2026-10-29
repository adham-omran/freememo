(ns freememo.task-owner-test
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.task-owner :as owner]))

(defn controlled-task [!callbacks !cancel-count]
  (fn [success failure]
    (reset! !callbacks {:success success :failure failure})
    (fn []
      (swap! !cancel-count inc)
      (failure (ex-info "Task canceled" {})))))

(deftest stopping-an-owner-rejects-both-late-callbacks
  (let [!callbacks (atom nil)
        !cancels (atom 0)
        !result (atom :loading)
        cancel! (owner/start! (controlled-task !callbacks !cancels)
                  #(reset! !result %) (fn [_] (reset! !result :failed)))]
    (cancel!)
    ((:success @!callbacks) :obsolete)
    ((:failure @!callbacks) (ex-info "Late failure" {}))
    (is (= :loading @!result))
    (is (= 1 @!cancels))
    (cancel!)
    (is (= 1 @!cancels))))

(deftest a-replacement-keeps-its-result-after-the-old-task-drains
  (let [!old (atom nil)
        !new (atom nil)
        !cancels (atom 0)
        !result (atom nil)
        stop-old! (owner/start! (controlled-task !old !cancels)
                    #(reset! !result %) (fn [_] (reset! !result :old-failed)))]
    (stop-old!)
    (let [stop-new! (owner/start! (controlled-task !new !cancels)
                      #(reset! !result %) (fn [_] (reset! !result :new-failed)))]
      ((:success @!new) :current)
      ((:success @!old) :obsolete)
      (is (= :current @!result))
      (stop-new!))))

(deftest live-failures-and-synchronous-completion-settle-once
  (testing "a live failure reaches its consumer"
    (let [!callbacks (atom nil)
          !seen (atom [])
          stop! (owner/start! (controlled-task !callbacks (atom 0))
                  #(swap! !seen conj %) (fn [_] (swap! !seen conj :failed)))]
      ((:failure @!callbacks) (ex-info "Failed" {}))
      ((:success @!callbacks) :duplicate)
      (is (= [:failed] @!seen))
      (stop!)))
  (testing "synchronous completion retains no live callback"
    (let [!seen (atom [])
          stop! (owner/start! (fn [success _]
                               (success :ready)
                               (fn [] (swap! !seen conj :canceled)))
                  #(swap! !seen conj %) (fn [_] (swap! !seen conj :failed)))]
      (stop!)
      (is (= [:ready] @!seen)))))

(deftest simultaneous-consumers-settle-independently
  (let [first-callbacks (atom nil) second-callbacks (atom nil)
        first-result (atom nil) second-result (atom nil)
        first-cancels (atom 0) second-cancels (atom 0)
        first-stop! (owner/start! (controlled-task first-callbacks first-cancels)
                      #(reset! first-result %) #(reset! first-result :failed))
        second-stop! (owner/start! (controlled-task second-callbacks second-cancels)
                       #(reset! second-result %) #(reset! second-result :failed))]
    (first-stop!)
    ((:success @first-callbacks) :late-first)
    ((:success @second-callbacks) :second)
    (is (nil? @first-result))
    (is (= :second @second-result))
    (is (= 1 @first-cancels))
    (is (zero? @second-cancels))
    (second-stop!)))
