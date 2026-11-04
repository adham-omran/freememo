(ns freememo.availability-test
  "Availability calculation and page. Prometheus is stubbed at the HTTP boundary.
   This namespace needs no database."
  (:require [cheshire.core :as json]
            [clj-http.client :as http]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [freememo.availability :as availability]
            [freememo.availability-http :as page])
  (:import [java.time Instant]
           [org.jsoup Jsoup]))

(defn- prom-answer
  "A Prometheus instant-query answer that holds one sample."
  [minutes-up]
  {:status 200
   :body (json/generate-string
          {:status "success"
           :data {:resultType "vector"
                  :result [{:metric {} :value [1700000000 (str minutes-up)]}]}})})

(def ^:private empty-answer
  {:status 200 :body (json/generate-string {:status "success" :data {:resultType "vector" :result []}})})

(defn- stub-prometheus [response f]
  (let [calls (atom [])]
    (with-redefs [http/get (fn [url opts]
                             (swap! calls conj (assoc opts :url url))
                             (if (instance? Throwable response) (throw response) response))]
      (f calls))))

(use-fixtures :each (fn [t] (availability/clear-cache!) (t)))

(defn- at [s] (Instant/parse s))

(deftest minutes-in-month-follows-the-calendar
  (is (= (* 30 1440) (availability/minutes-in-month 2026 9)))
  (is (= (* 31 1440) (availability/minutes-in-month 2026 10))))

(deftest current-month-divides-by-the-minutes-so-far
  (testing "all minutes up"
    (stub-prometheus (prom-answer 1440)
      (fn [_]
        (let [r (availability/current-month (at "2026-11-02T00:00:00Z"))]
          ;; Nov 1 00:00 to Nov 2 00:00 is 1440 minutes.
          (is (= 1440 (:minutes r)))
          (is (= 1440 (:minutes-up r)))
          (is (= "100.00%" (availability/format-percent (:percent r))))))))
  (testing "some minutes down"
    (stub-prometheus (prom-answer 1425)
      (fn [_]
        (let [r (availability/current-month (at "2026-11-02T00:00:00Z"))]
          (is (= 15 (- (:minutes r) (:minutes-up r))))
          (is (= "98.95%" (availability/format-percent (:percent r))))))))
  (testing "a gap with no samples counts as down"
    ;; The query counts only minutes that have a successful sample, so a
    ;; 60-minute gap lowers the count by 60.
    (stub-prometheus (prom-answer (- 1440 60))
      (fn [_]
        (let [r (availability/current-month (at "2026-11-02T00:00:00Z"))]
          (is (= 60 (- (:minutes r) (:minutes-up r))))))))
  (testing "the query window is the minutes so far, in a 31-day month"
    (stub-prometheus (prom-answer 100)
      (fn [calls]
        (availability/current-month (at "2026-10-31T23:59:00Z"))
        (let [q (get-in (first @calls) [:query-params "query"])]
          (is (str/includes? q "probe_success{instance=\"freememo-public\"}"))
          (is (str/includes? q "[44639m:1m]"))))))
  (testing "a 30-day month divides by its own minutes"
    (stub-prometheus (prom-answer 100)
      (fn [calls]
        (availability/current-month (at "2026-11-30T23:59:00Z"))
        (is (str/includes? (get-in (first @calls) [:query-params "query"]) "[43199m:1m]")))))
  (testing "the current month never divides by more than the minutes so far"
    (stub-prometheus (prom-answer 5000)
      (fn [_]
        (let [r (availability/current-month (at "2026-11-01T01:00:00Z"))]
          (is (= 60 (:minutes r)))
          (is (<= (:minutes-up r) 60)))))))

(deftest format-percent-has-two-decimals
  (is (= "99.02%" (availability/format-percent 99.0213)))
  (is (= "99.00%" (availability/format-percent 99.0))))

(deftest prometheus-failure-gives-no-figure
  (doseq [[label response] [["empty answer" empty-answer]
                            ["server error" {:status 500 :body "boom"}]
                            ["bad body" {:status 200 :body "not json"}]
                            ["error status" {:status 200 :body (json/generate-string {:status "error" :error "bad"})}]
                            ["network error" (java.net.ConnectException. "refused")]]]
    (testing label
      (availability/clear-cache!)
      (stub-prometheus response
        (fn [_] (is (nil? (availability/current-month (at "2026-11-02T00:00:00Z")))))))))

(deftest answers-are-cached
  (stub-prometheus (prom-answer 1000)
    (fn [calls]
      (availability/cached-current-month)
      (availability/cached-current-month)
      (availability/cached-current-month)
      (is (= 1 (count @calls))))))

(defn- doc [response] (Jsoup/parse (:body response)))

(deftest page-shows-the-current-month-when-prometheus-answers
  (stub-prometheus (prom-answer 1000000)
    (fn [_]
      (let [response (with-redefs [availability/stored-months (fn [] [])] (page/response {:uri "/availability" :request-method :get}))
            d (doc response)
            text (.text d)]
        (is (= 200 (:status response)))
        (is (= "text/html; charset=utf-8" (get-in response [:headers "Content-Type"])))
        (is (= "Availability" (.text (.selectFirst d "h1"))))
        (is (empty? (.select d "script")))
        (is (str/includes? text "FreeMemo aims for 99.0% availability in each calendar month, in UTC."))
        (is (str/includes? text "This is a target, not a guarantee, and a missed month gives no refund."))
        (is (= ["Month" "Availability" "Target" "Result"] (mapv #(.text %) (.select d "th"))))
        (let [cells (mapv #(.text %) (.select d "tbody tr td"))]
          (is (re-matches #"\d+\.\d\d%" (nth cells 1)))
          (is (= "99.0%" (nth cells 2)))
          (is (= "In progress" (nth cells 3))))
        (is (not (str/includes? text "not available now")))))))

(deftest page-shows-only-the-not-available-line-when-prometheus-fails
  (doseq [response [empty-answer {:status 500 :body "boom"}]]
    (availability/clear-cache!)
    (stub-prometheus response
      (fn [_]
        (let [d (doc (with-redefs [availability/stored-months (fn [] [])] (page/response {:uri "/availability" :request-method :get})))
              text (.text d)]
          (is (str/includes? text "The figures for the current month are not available now. Try again later."))
          (is (empty? (.select d "td")))
          (is (not (re-find #"\d\.\d\d%" text))))))))

(deftest page-answers-only-its-own-path-and-methods
  (is (nil? (page/response {:uri "/docs" :request-method :get})))
  (is (= 405 (:status (page/response {:uri "/availability" :request-method :post})))))
