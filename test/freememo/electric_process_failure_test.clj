(ns freememo.electric-process-failure-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [freememo.electric-process-failure :as process-failure]
   [taoensso.telemere :as tel])
  (:import (missionary Cancelled)))

(defn- boot-throwing
  "A boot fn whose process flow throws each value of `errors` in turn from
   its transfer, and records a cancel in `!cancelled`."
  [errors !cancelled]
  (fn [_subject]
    (fn [_notifier _terminator]
      (let [!left (atom errors)]
        (reify
          clojure.lang.IFn
          (invoke [_] (reset! !cancelled true))
          clojure.lang.IDeref
          (deref [_]
            (let [e (first @!left)]
              (swap! !left rest)
              (if e (throw e) :value))))))))

(def ^:private ring-request
  {:uri "/viewer/topic/42" :session {:user-id 7}})

(defn- run-process
  "Boot through `logged`, transfer `n` times, then cancel. Return the throws
   and the signals written."
  [errors n !cancelled]
  (let [!thrown (atom [])
        {:keys [signals]} (tel/with-signals
                  (let [flow ((process-failure/logged ring-request
                                (boot-throwing errors !cancelled))
                              nil)
                        process (flow #() #())]
                    (dotimes [_ n]
                      (try @process
                        (catch Throwable e (swap! !thrown conj e))))
                    (process)))]
    {:thrown @!thrown
     :signals (filterv #(= ::process-failure/process-failed (:id %)) signals)}))

(deftest a-failure-is-logged-once-and-rethrown
  (let [first-error (UnsupportedOperationException. "nth not supported")
        second-error (IllegalStateException. "later")
        !cancelled (atom false)
        {:keys [thrown signals]} (run-process [first-error second-error] 3 !cancelled)]
    (testing "the transfer still throws the same exceptions"
      (is (= [first-error second-error] thrown)))
    (testing "one error signal, for the first failure, with the request context"
      (is (= 1 (count signals)))
      (is (= :error (:level (first signals))))
      (is (identical? first-error (:error (first signals))))
      (is (= {:user-id 7 :uri "/viewer/topic/42"} (:data (first signals)))))
    (testing "cancel reaches the process"
      (is @!cancelled))))

(deftest cancellation-is-not-a-failure
  (let [{:keys [thrown signals]} (run-process [(Cancelled.)] 1 (atom false))]
    (is (= 1 (count thrown)))
    (is (empty? signals))))

(deftest a-value-passes-through
  (let [{:keys [thrown signals]} (run-process [] 2 (atom false))]
    (is (empty? thrown))
    (is (empty? signals))))
