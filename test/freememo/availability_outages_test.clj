(ns freememo.availability-outages-test
  "Outages of 15 minutes or more: detection over per-minute data, storage
   against a real database, and the rendered page. Terms: CONTEXT.md,
   \"Service Commitments\". The database tests skip when PostgreSQL is down."
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
  (#'db/setup-schema-availability-outages!)
  (jdbc/execute! db/ds ["DELETE FROM availability_outages"])
  (jdbc/execute! db/ds ["DELETE FROM availability_months"]))

(use-fixtures :each (fn [t] (availability/clear-cache!) (t)))

(defn- at [s] (Instant/parse s))

;; ── Seam 1: detection over per-minute data ─────────────────────────

(def ^:private t0 (* 60 (quot 1793000000 60)))

(defn- minutes
  "Seconds of the minutes from offset `from` to `to`, both inclusive."
  [from to]
  (set (map #(+ t0 (* 60 %)) (range from (inc to)))))

(defn- detect
  "`down` is a set of minute offsets that are down, inside a window of 200 minutes."
  [down]
  (availability/detect-outages (apply disj (minutes 0 199) (map #(+ t0 (* 60 %)) down))
                               t0 (+ t0 (* 60 199))))

(defn- run [from to] (range from (inc to)))

(deftest a-run-of-exactly-15-down-minutes-is-an-outage-and-14-is-not
  (is (= [{:start t0 :minutes 15}] (detect (run 0 14))))
  (is (= [] (detect (run 0 13))))
  (is (= [{:start (+ t0 (* 60 50)) :minutes 15}] (detect (run 50 64)))))

(deftest runs-whose-down-minutes-are-2-minutes-apart-or-less-are-one-outage
  ;; The outage report of scripts/prod_logs.py joins down minutes that are at
  ;; most 120 seconds apart: one up minute between them.
  ;; Down 10..19 and 21..30: minute 20 is up.
  (is (= [{:start (+ t0 (* 60 10)) :minutes 21}] (detect (concat (run 10 19) (run 21 30)))))
  ;; Two runs that would each be too short become one outage of 15 or more.
  (is (= [{:start (+ t0 (* 60 10)) :minutes 15}] (detect (concat (run 10 17) (run 19 24)))))
  ;; Down 10..19 and 22..31: minutes 20 and 21 are up, so these are two runs of 10.
  (is (= [] (detect (concat (run 10 19) (run 22 31))))))

(deftest a-failed-sample-names-the-minute-that-ended-at-its-time
  ;; Samples at 17:15:00 .. 17:33:00 (the minutes 17:14 .. 17:32) failed.
  (let [at-s (fn [hh mm] (.getEpochSecond (Instant/parse (format "2026-11-03T%02d:%02d:00Z" hh mm))))
        values (for [m (range 0 60)] ; samples at 17:00 .. 17:59
                 [(+ (at-s 17 0) (* 60 m)) (if (<= 15 m 33) "0" "1")])
        up (availability/up-minute-starts values)]
    (is (= [{:start (at-s 17 14) :minutes 19}]
           (availability/detect-outages up (at-s 16 59) (at-s 17 58))))))

(deftest a-minute-with-no-sample-counts-as-down
  (is (= [{:start t0 :minutes 15}]
         (availability/detect-outages #{} t0 (+ t0 (* 60 14))))))

;; ── Seam 2: storage against the database ───────────────────────────

(def ^:private oct-start 1790812800) ; 2026-10-01T00:00:00Z

(defn- range-body
  "A query_range answer: every minute of the queried range is up except the
   minutes from offset `from` to offset `to` (minutes after 2026-10-01T00:00Z;
   a minute's sample is labelled with the END of the minute)
   of each pair in `down-runs`."
  [down-runs]
  (fn [_ opts]
    (let [{:strs [start end]} (:query-params opts)
          down (set (mapcat (fn [[from to]] (range (+ oct-start (* 60 (inc from))) (+ oct-start (* 60 (+ 2 to))) 60))
                            down-runs))]
      {:status 200
       :body (json/generate-string
              {:status "success"
               :data {:resultType "matrix"
                      :result [{:metric {} :values (for [t (range start (inc end) 60)]
                                                     [t (if (down t) "0" "1")])}]}})})))

(def ^:private outage-minutes [[1000 1014] [2000 2013] [3000 3099]]) ; 15, 14 and 100 minutes

(def ^:private now-s "2026-11-04T10:00:00Z")

(deftest outages-are-stored-with-their-month-and-survive-the-data-going-away
  (if-not (db-up?)
    (is true "PostgreSQL is unavailable; integration assertion skipped")
    (do
      (clean!)
      (db/store-availability-month! {:year 2026 :month 10 :minutes-up 44194 :minutes 44640 :target 99.0})
      (with-redefs [http/get (range-body outage-minutes)]
        (availability/store-outages! (at now-s)))
      (let [oct (filter #(= [2026 10] [(:year %) (:month %)]) (db/availability-outages))]
        (is (= [3000 1000] (mapv #(quot (- (.getEpochSecond (:started-at %)) oct-start) 60) oct)))
        (is (= [100 15] (mapv :minutes oct)))
        (is (every? #(= "not-yet-known" (:cause %)) oct)))
      (testing "Prometheus has dropped the data: the page still lists them"
        (with-redefs [http/get (fn [& _] {:status 500 :body ""})]
          (let [html (:body (page/response {:uri "/availability" :request-method :get :now (at now-s)}))]
            ;; minute 1000 after 2026-10-01T00:00 is 16:40 on the same day
            (is (str/includes? html "2026-10-01 16:40"))
            (is (str/includes? html "1 hour 40 minutes")))
          (availability/store-outages! (at now-s))))
      (is (= 2 (count (filter #(= 10 (:month %)) (db/availability-outages)))))
      (clean!))))

(deftest a-month-that-is-already-closed-still-gets-its-outages
  (if-not (db-up?)
    (is true "PostgreSQL is unavailable; integration assertion skipped")
    (do
      (clean!)
      (db/store-availability-month! {:year 2026 :month 10 :minutes-up 44194 :minutes 44640 :target 99.0})
      (with-redefs [http/get (range-body outage-minutes)]
        (availability/store-outages! (at now-s)))
      (is (= 2 (count (filter #(= 10 (:month %)) (db/availability-outages)))))
      (clean!))))

(deftest the-operator-sets-a-cause-and-the-page-shows-it
  (if-not (db-up?)
    (is true "PostgreSQL is unavailable; integration assertion skipped")
    (do
      (clean!)
      (db/store-availability-month! {:year 2026 :month 10 :minutes-up 44194 :minutes 44640 :target 99.0})
      (with-redefs [http/get (range-body [[1000 1014]])]
        (availability/store-outages! (at now-s)))
      (let [start (str (Instant/ofEpochSecond (+ oct-start (* 60 1000))))
            causes #(mapv :cause (db/availability-outages))]
        (is (true? (availability/set-outage-cause! start :network-tunnel)))
        (is (= ["network-tunnel"] (causes)))
        (with-redefs [http/get (fn [& _] {:status 500 :body ""})]
          (is (str/includes? (:body (page/response {:uri "/availability" :request-method :get :now (at now-s)}))
                             "Network tunnel")))
        (testing "a value outside the four causes is rejected and changes nothing"
          (is (thrown? clojure.lang.ExceptionInfo (availability/set-outage-cause! start :weather)))
          (is (thrown? clojure.lang.ExceptionInfo (availability/set-outage-cause! start "Network tunnel")))
          (is (= ["network-tunnel"] (causes))))
        (testing "a start with no outage changes nothing"
          (is (false? (availability/set-outage-cause! "2026-10-02T00:00:00Z" :freememo-server))))
        (testing "a new sweep keeps the cause"
          (with-redefs [http/get (range-body [[1000 1014]])]
            (availability/store-outages! (at now-s)))
          (is (= ["network-tunnel"] (causes)))))
      (clean!))))

;; ── Seam 3: the rendered page ──────────────────────────────────────

(defn- section [d label]
  (first (filter #(= label (.text (.selectFirst % "h2"))) (.select d "section.availability-outages"))))

(def ^:private months
  [{:year 2026 :month 10 :minutes-up 44194 :minutes 44640 :target 99.0}
   {:year 2026 :month 9 :minutes-up 42000 :minutes 43200 :target 99.0}])

(defn- render [outages]
  (with-redefs [availability/stored-months (fn [] months)
                availability/stored-outages (fn [] outages)
                http/get (fn [& _] {:status 500 :body "boom"})]
    (Jsoup/parse (:body (page/response {:uri "/availability" :request-method :get :now (at now-s)})))))

(defn- cells [el] (mapv (fn [tr] (mapv #(.text %) (.select tr "td"))) (.select el "tbody tr")))

(deftest a-month-lists-its-outages-with-start-length-and-cause
  (let [d (render [{:started-at (at "2026-10-28T03:12:00Z") :year 2026 :month 10 :minutes 75 :cause "network-tunnel"}
                   {:started-at (at "2026-10-05T23:00:00Z") :year 2026 :month 10 :minutes 15 :cause "not-yet-known"}])
        oct (section d "October 2026")]
    (is (= "Outages of 15 minutes or more" (.text (.selectFirst oct "h3"))))
    (is (= ["Start (UTC)" "Length" "Cause"] (mapv #(.text %) (.select oct "th"))))
    (is (= [["2026-10-05 23:00" "15 minutes" "Not yet known"]
            ["2026-10-28 03:12" "1 hour 15 minutes" "Network tunnel"]]
           (cells oct)))))

(deftest a-month-with-no-outage-says-so
  (let [d (render [{:started-at (at "2026-10-28T03:12:00Z") :year 2026 :month 10 :minutes 75 :cause "freememo-server"}])
        sep (section d "September 2026")]
    (is (= "No outage of 15 minutes or more in this month." (.text (.selectFirst sep "p"))))
    (is (empty? (.select sep "table")))))

(deftest a-missed-month-can-list-no-outage
  ;; September: 1200 down minutes of 43200 is Missed, with only short outages.
  (let [d (render [])]
    (is (some #{["September 2026" "97.22%" "99.0%" "Missed"]}
              (cells (.selectFirst d "table.availability-table"))))
    (is (= "No outage of 15 minutes or more in this month."
           (.text (.selectFirst (section d "September 2026") "p"))))))
