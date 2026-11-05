(ns freememo.availability-months-test
  "Closed months of the availability page: the Met/Missed decision, the month
   close against a real database, and the rendered page. Prometheus is stubbed
   at the HTTP boundary. The database tests skip when PostgreSQL is down."
  (:require [cheshire.core :as json]
            [clj-http.client :as http]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [freememo.availability :as availability]
            [freememo.availability-http :as page]
            [freememo.db :as db]
            [next.jdbc :as jdbc])
  (:import [java.time Instant]
           [org.jsoup Jsoup]))

(defn- db-up? []
  (try (jdbc/execute-one! db/ds ["SELECT 1"]) true (catch Exception _ false)))

(defn- clean! []
  (#'db/setup-schema-availability!)
  (jdbc/execute! db/ds ["DELETE FROM availability_months"]))

(use-fixtures :each (fn [t] (availability/clear-cache!) (t)))

(defn- at [s] (Instant/parse s))

;; ── Seam 1: pure decisions ─────────────────────────────────────────

(deftest exactly-the-target-is-met-and-one-more-down-minute-is-missed
  ;; 30 days = 43200 minutes. 99.0% allows 432 down minutes.
  (is (= :met (availability/month-result {:minutes-up (- 43200 432) :minutes 43200 :target 99.0})))
  (is (= :missed (availability/month-result {:minutes-up (- 43200 433) :minutes 43200 :target 99.0})))
  ;; 31 days = 44640 minutes. 99.0% allows 446.4, so 446 down is met, 447 missed.
  (is (= :met (availability/month-result {:minutes-up (- 44640 446) :minutes 44640 :target 99.0})))
  (is (= :missed (availability/month-result {:minutes-up (- 44640 447) :minutes 44640 :target 99.0}))))

(deftest a-closed-month-keeps-its-own-stored-target
  (let [row {:year 2026 :month 9 :minutes-up 42777 :minutes 43200 :target 99.0}]
    (is (= :met (availability/month-result row)))
    ;; The same minutes against a stricter target would be missed, so the
    ;; decision reads the target from the row and not from configuration.
    (is (= :missed (availability/month-result (assoc row :target 99.5))))))

(deftest month-close-result-is-the-row-to-store
  (is (= {:year 2026 :month 9 :minutes-up 42777 :minutes 43200 :target 99.0}
         (availability/closed-row 2026 9 42777 99.0)))
  (is (nil? (availability/closed-row 2026 9 nil 99.0))))

(deftest the-page-lists-the-three-months-before-the-current-one-but-nothing-before-september-2026
  (let [rows (for [[y m] [[2026 10] [2026 9] [2026 8] [2026 7]]]
               {:year y :month m :minutes-up 1 :minutes 1 :target 99.0})]
    (is (= [[2026 10] [2026 9]]
           (mapv (juxt :year :month) (availability/visible-months rows (at "2026-11-04T00:00:00Z")))))
    (is (= [[2026 10]]
           (mapv (juxt :year :month) (availability/visible-months rows (at "2027-01-04T00:00:00Z")))))))

;; ── Seam 2: month close against the database ───────────────────────

(defn- prom-body [up]
  {:status 200 :body (json/generate-string {:status "success"
                                             :data {:resultType "vector"
                                                    :result [{:metric {} :value [1 (str up)]}]}})})

(defn- stub-measured-months [f]
  (with-redefs [http/get (fn [_ opts]
                           (let [q (get-in opts [:query-params "query"])]
                             (cond
                               (str/includes? q "[43200m:1m]") (prom-body (- 43200 423)) ; September
                               (str/includes? q "[44640m:1m]") (prom-body (- 44640 446)) ; October
                               :else {:status 500 :body ""})))]
    (f)))

(deftest month-close-stores-september-and-october-once
  (if-not (db-up?)
    (is true "PostgreSQL is unavailable; integration assertion skipped")
    (do
      (clean!)
      (stub-measured-months
       (fn []
         (availability/close-months! (at "2026-11-04T10:00:00Z"))
         (let [by (into {} (map (juxt (juxt :year :month) identity)) (db/availability-months))]
           (is (= 2 (count by)))
           (is (= {:year 2026 :month 9 :minutes-up 42777 :minutes 43200 :target 99.0} (by [2026 9])))
           (is (= {:year 2026 :month 10 :minutes-up 44194 :minutes 44640 :target 99.0} (by [2026 10]))))
         (testing "a second run neither changes nor duplicates a row"
           (jdbc/execute! db/ds ["UPDATE availability_months SET target = 98.0 WHERE month = 9"])
           (let [before (db/availability-months)]
             (availability/close-months! (at "2026-11-04T10:00:00Z"))
             (is (= before (db/availability-months)))))))
      (clean!))))

(deftest month-close-stores-nothing-when-prometheus-fails
  (if-not (db-up?)
    (is true "PostgreSQL is unavailable; integration assertion skipped")
    (do
      (clean!)
      (with-redefs [http/get (fn [& _] (throw (java.net.ConnectException. "refused")))]
        (availability/close-months! (at "2026-11-04T10:00:00Z")))
      (is (empty? (db/availability-months)))
      (clean!))))

(deftest month-close-skips-a-month-that-prometheus-no-longer-holds
  (if-not (db-up?)
    (is true "PostgreSQL is unavailable; integration assertion skipped")
    (do
      (clean!)
      ;; At 2027-02-01, September 2026 started 153 days ago: beyond 90 days.
      (stub-measured-months
       (fn []
         (availability/close-months! (at "2027-02-01T10:00:00Z"))
         (is (not-any? #(= [2026 9] [(:year %) (:month %)]) (db/availability-months)))))
      (clean!))))

;; ── Seam 3: the rendered page ──────────────────────────────────────

(defn- rows-of [d] (mapv (fn [tr] (mapv #(.text %) (.select tr "td"))) (.select d "tbody tr")))

(def ^:private stored
  [{:year 2026 :month 10 :minutes-up 44194 :minutes 44640 :target 99.0}
   {:year 2026 :month 9 :minutes-up 42777 :minutes 43200 :target 99.0}])

(deftest page-shows-stored-months-and-the-current-month
  (with-redefs [availability/stored-months (fn [] stored)
                availability/stored-outages (fn [] [])
                http/get (fn [& _] (prom-body 1000000))]
    (let [d (Jsoup/parse (:body (page/response {:uri "/availability" :request-method :get :now (at "2026-11-04T10:00:00Z")})))
          rows (rows-of d)]
      (is (= 3 (count rows)))
      (is (= ["Month" "Availability" "Target" "Result"] (mapv #(.text %) (.select d "th"))))
      (is (= "In progress" (last (first rows))))
      (is (= ["October 2026" "99.00%" "99.0%" "Met"] (second rows)))
      (is (= ["September 2026" "99.02%" "99.0%" "Met"] (nth rows 2))))))

(deftest page-shows-stored-months-when-prometheus-fails
  (with-redefs [availability/stored-months (fn [] stored)
                availability/stored-outages (fn [] [])
                http/get (fn [& _] {:status 500 :body "boom"})]
    (let [d (Jsoup/parse (:body (page/response {:uri "/availability" :request-method :get :now (at "2026-11-04T10:00:00Z")})))
          text (.text d)]
      (is (= [["October 2026" "99.00%" "99.0%" "Met"]
              ["September 2026" "99.02%" "99.0%" "Met"]]
             (rows-of d)))
      (is (str/includes? text "The figures for the current month are not available now. Try again later.")))))

(deftest the-shown-percent-never-reads-99-00-for-a-missed-month
  (let [html (fn [up] (page/page-html nil [{:year 2026 :month 9 :minutes-up up :minutes 43200 :target 99.0}]))
        row-of (fn [up] (re-find #"<tr><td>September 2026</td>.*?</tr>" (html up)))]
    (testing "one minute under 99.0% shows 98.99% and Missed"
      (let [row (row-of 42767)]
        (is (str/includes? row "98.99%"))
        (is (str/includes? row "Missed"))
        (is (not (str/includes? row "99.00%")))))
    (testing "exactly 99.0% shows 99.00% and Met"
      (let [row (row-of 42768)]
        (is (str/includes? row "99.00%"))
        (is (str/includes? row "Met"))))))

(deftest percent-of-rounds-down-to-two-decimals
  (is (= 98.99 (availability/percent-of 42767 43200)))
  (is (= 99.0 (availability/percent-of 42768 43200)))
  (is (= 100.0 (availability/percent-of 43200 43200))))
