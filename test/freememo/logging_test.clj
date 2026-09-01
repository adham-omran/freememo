(ns freememo.logging-test
  (:require
   [cheshire.core :as json]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [freememo.logging]
   [taoensso.telemere :as tel]))

(def ^:private loki-json-output
  @(requiring-resolve 'freememo.logging/loki-json-output))

(defn- parsed-record [signal]
  (let [output (loki-json-output signal)]
    {:output output
     :record (json/parse-string-strict output true)}))

(deftest structured-signal-is-one-json-record
  (let [session-id (random-uuid)
        signal (tel/with-signal true
                 (tel/log! {:level :info
                            :id ::structured
                            :data {:keyword :value
                                   :symbol 'sample/value
                                   :session-id session-id
                                   :opaque (Object.)}}
                   "Hello\nمرحبا"))
        {:keys [output record]} (parsed-record signal)]
    (testing "one physical line"
      (is (= 1 (count (str/split-lines output))))
      (is (str/ends-with? output "\n")))
    (testing "Loki fields"
      (is (= "info" (:level record)))
      (is (= "log" (:kind record)))
      (is (= "freememo.logging-test/structured" (:id record)))
      (is (= "Hello\nمرحبا" (:message record)))
      (is (string? (:timestamp record)))
      (is (= "freememo.logging-test" (:logger record)))
      (is (integer? (:line record)))
      (is (integer? (:column record))))
    (testing "structured values"
      (is (= "value" (get-in record [:data :keyword])))
      (is (= "sample/value" (get-in record [:data :symbol])))
      (is (= (str session-id) (get-in record [:data :session-id])))
      (is (string? (get-in record [:data :opaque]))))))

(deftest error-signal-keeps-chain-and-stacktrace-in-one-record
  (let [error (ex-info "broken\noperation" {:reason :test})
        signal (tel/with-signal true
                 (tel/error! {:id ::failure :data {:operation :probe}} error))
        {:keys [output record]} (parsed-record signal)]
    (is (= 1 (count (str/split-lines output))))
    (is (= "error" (:level record)))
    (is (= "clojure.lang.ExceptionInfo"
          (get-in record [:error :chain 0 :type])))
    (is (= "test" (get-in record [:error :chain 0 :data :reason])))
    (is (str/includes? (get-in record [:error :stacktrace])
          "broken\noperation"))
    (is (not (contains? record :message)))))

(deftest log-handlers-share-json-output
  (let [handler-options (atom {})
        install! @(requiring-resolve 'freememo.logging/install-loki-log-handlers!)]
    (with-redefs [tel/handler:console
                  (fn [options]
                    (swap! handler-options assoc :console options)
                    (fn ([] nil) ([_] nil)))
                  tel/handler:file
                  (fn [options]
                    (swap! handler-options assoc :file options)
                    (fn ([] nil) ([_] nil)))
                  tel/add-handler! (fn [& _] nil)]
      (install!))
    (is (identical? (get-in @handler-options [:console :output-fn])
                    (get-in @handler-options [:file :output-fn])))))
