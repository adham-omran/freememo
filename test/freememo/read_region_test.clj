(ns freememo.read-region-test
  (:require [clojure.test :refer [deftest is]]
            [freememo.read-region :as reads]
            [freememo.logging :as log]))

(deftest context-excludes-content-and-account-identifiers
  (let [event (reads/read-event :search-results
                {:run "audit-run" :build "abc123" :route "search" :phase "fuzzy"
                 :fixture-size 100000 :query "Private query" :user-id 7 :content "Private content"}
                "ok" 12.5)]
    (is (= "audit-run" (:run event)))
    (is (= "abc123" (:build event)))
    (is (= 100000 (:fixture-size event)))
    (is (= 12.5 (:duration-ms event)))
    (is (not-any? #(contains? event %) [:query :user-id :content]))))

(deftest timed-read-keeps-the-value-and-emits-one-record
  (let [events (atom [])
        record! (fn [message fields] (swap! events conj [message fields]))
        value (with-redefs [log/log-info record! log/log-debug record!]
                (reads/timed-read :bounded-options {:route "knowledge" :run "controlled"} #(vector :one :two)))]
    (is (= [:one :two] value))
    (is (= 1 (count @events)))
    (is (= 2 (get-in @events [0 1 :result-count])))
    (is (= "knowledge" (get-in @events [0 1 :route])))
    (is (re-find #"Timed read region=bounded-options" (ffirst @events)))))

(deftest a-failed-read-rethrows-the-original-error-and-records-its-class
  (let [error (ex-info "Controlled failure" {}) events (atom [])]
    (with-redefs [log/log-warn (fn [message fields] (swap! events conj [message fields]))]
      (is (identical? error (try (reads/timed-read :broken #(throw error)) (catch Throwable e e)))))
    (is (= 1 (count @events)))
    (is (= "error" (get-in @events [0 1 :outcome])))
    (is (= "clojure.lang.ExceptionInfo" (get-in @events [0 1 :error-class])))))
