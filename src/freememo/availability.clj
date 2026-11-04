(ns freememo.availability
  "Availability of FreeMemo for the current calendar month, from Prometheus.

   Availability is the share of minutes in the month in which the public probe
   reached FreeMemo. A minute with no successful sample counts as down, so the
   count comes from `sum_over_time` over a one-minute subquery, never from
   `avg_over_time` (which skips gaps). The current month divides by the minutes
   so far. Months are in UTC. An outage is a run of down minutes of 15 minutes or
   more; the sweep stores each one with its month. Terms: CONTEXT.md, \"Service Commitments\".
   Plan: plans/paying-customer-commitments.md, Step 2."
  (:require [cheshire.core :as json]
            [clj-http.client :as http]
            [freememo.config :as config]
            [freememo.db :as db]
            [taoensso.telemere :as tel])
  (:import [java.time Duration Instant YearMonth ZoneOffset]))

(def uptime-target
  "Uptime Target in percent."
  99.0)

(def ^:private probe-instance "freememo-public")
(def ^:private cache-ttl-ms 60000)
(def ^:private failure-ttl-ms 15000)

(defn prometheus-url
  "Base URL of Prometheus. PROMETHEUS_URL env, else [:deployment :prometheus-url]
   in config.edn, else the LAN address of the home server."
  []
  (or (config/env-or-config "PROMETHEUS_URL" [:deployment :prometheus-url])
      "http://192.168.0.8:31002"))

(defn minutes-in-month
  "Number of minutes in the calendar month, in UTC."
  [year month]
  (* 1440 (.lengthOfMonth (YearMonth/of (int year) (int month)))))

(defn format-percent
  "Two decimal places, for example \"99.02%\"."
  [percent]
  (format "%.2f%%" (double percent)))

(defn percent-of
  "Percent of `minutes` that `up` is, cut down (not rounded) to two decimals.
   The figure never reads 99.00 when the exact share is below 99.0, so it agrees
   with `month-result`. Integer arithmetic."
  [up minutes]
  (/ (quot (* 10000 (long up)) (long minutes)) 100.0))

(defn- probe-query [minutes]
  (str "sum_over_time((max_over_time(probe_success{instance=\"" probe-instance "\"}[1m]) > bool 0)[" minutes "m:1m])"))

(defn minutes-up
  "Read the minutes-up count from a Prometheus instant-query body (parsed JSON).
   Returns nil unless the body is a successful answer with one sample."
  [body]
  (when (= "success" (:status body))
    (some-> body :data :result first :value second parse-double)))

(defn period-availability
  "Pure calculation. `up` is the count from Prometheus, `minutes` the minutes of
   the period. Up is clamped to the period. Returns a map, or nil when either
   input is missing."
  [up minutes]
  (when (and up (pos? minutes))
    (let [up (long (Math/min (double (Math/round (double up))) (double minutes)))]
      {:minutes minutes
       :minutes-up up
       :percent (percent-of up minutes)})))

(defn- query-prometheus
  "GET `path` on Prometheus with `params`. Returns the parsed body, or nil on any
   failure. An exception is logged."
  [path params timeout-ms]
  (try
    (let [resp (http/get (str (prometheus-url) path)
                         {:query-params params
                          :throw-exceptions false
                          :socket-timeout timeout-ms
                          :connection-timeout 3000})]
      (when (= 200 (:status resp))
        (json/parse-string (:body resp) true)))
    (catch Exception e
      (tel/error! {:id ::prometheus-query-failed :data {:path path :params params}} e)
      nil)))

(def first-month
  "No month before this one shows: Prometheus has no complete month before it."
  (YearMonth/of 2026 9))

(defn- month-of ^YearMonth [^Instant instant]
  (YearMonth/from (.atOffset instant ZoneOffset/UTC)))

(defn- month-start
  "The first instant of `ym`, in UTC."
  ^Instant [^YearMonth ym]
  (.toInstant (.atStartOfDay (.atDay ym 1) ZoneOffset/UTC)))

(defn- months-from-first
  "Every month from `first-month` up to `cur`: before it when `inclusive?` is
   false, with it when true."
  [^YearMonth cur inclusive?]
  (take-while #(if inclusive?
                 (not (.isAfter ^YearMonth % cur))
                 (.isBefore ^YearMonth % cur))
              (iterate #(.plusMonths ^YearMonth % 1) first-month)))

(defn current-month
  "Availability of the month that contains `now`, divided by the minutes so far.
   Returns {:year :month :minutes :minutes-up :percent}, or nil when Prometheus
   does not answer or has no sample."
  [^Instant now]
  (let [ym (month-of now)
        start (month-start ym)
        minutes (long (Math/ceil (/ (.toSeconds (Duration/between start now)) 60.0)))]
    (when (and (pos? minutes) (not (.isBefore ym first-month)))
      (when-let [result (some-> (query-prometheus "/api/v1/query"
                                                  {"query" (probe-query minutes) "time" (.getEpochSecond now)}
                                                  5000)
                                minutes-up
                                (period-availability minutes))]
        (assoc result :year (.getYear ym) :month (.getMonthValue ym))))))

;; ── Cache ──────────────────────────────────────────────────────────────
;; Page loads share one Prometheus query per minute. A failure is cached for a
;; shorter time, so an outage of Prometheus does not make every load wait.

(defonce ^:private cache (atom nil))

(defn clear-cache! [] (reset! cache nil))

(defn cached-current-month
  "`current-month` for now, cached for a minute."
  []
  (let [t (System/currentTimeMillis)
        {:keys [at ttl value]} @cache]
    (if (and at (< (- t at) ttl))
      value
      (let [value (current-month (Instant/now))]
        (reset! cache {:at t :ttl (if value cache-ttl-ms failure-ttl-ms) :value value})
        value))))

;; ── Closed months ──────────────────────────────────────────────────────
;; Prometheus keeps 90 days, so a closed month is stored when it ends and the
;; page reads the stored row. The row keeps the target that applied.

(def ^:private retention-days
  "A month is closed from Prometheus only while its first minute is this recent.
   Prometheus keeps 90 days, and an older month would give a partial count."
  89)

(defn month-result
  "`:met` or `:missed` for a stored month, against the target stored with it.
   Integer arithmetic: the target in tenths of a percent, so exactly 99.0% is met."
  [{:keys [minutes-up minutes target]}]
  (if (>= (* 1000 minutes-up) (* minutes (Math/round (* 10.0 target)))) :met :missed))

(defn closed-row
  "The row to store for a closed month, or nil when `up` is missing."
  [year month up target]
  (when up
    (let [minutes (minutes-in-month year month)]
      {:year year :month month :minutes-up (:minutes-up (period-availability up minutes))
       :minutes minutes :target target})))

(defn visible-months
  "From stored `rows` (newest first), the closed months that the page shows at
   `now`: the three months before the current one, none before `first-month`."
  [rows ^Instant now]
  (let [cur (month-of now)]
    (filterv (fn [{:keys [year month]}]
               (let [ym (YearMonth/of (int year) (int month))]
                 (and (.isBefore ym cur)
                      (not (.isBefore ym (.minusMonths cur 3)))
                      (not (.isBefore ym first-month)))))
             rows)))

(defn stored-months
  "Every stored closed month, newest first. Empty when the database fails."
  []
  (try (db/availability-months)
       (catch Exception e
         (tel/error! {:id ::stored-months-failed} e)
         [])))

(defn- closed-month-up
  "Minutes up of a past month, asked of Prometheus at the first instant of the next month."
  [year month]
  (let [end (month-start (.plusMonths (YearMonth/of (int year) (int month)) 1))]
    (some-> (query-prometheus "/api/v1/query"
                              {"query" (probe-query (minutes-in-month year month)) "time" (.getEpochSecond end)}
                              5000)
            minutes-up)))

(defn close-months!
  "Store each month that has ended, is not stored, and that Prometheus still
   holds. A month with no answer stays unstored and the next run tries again.
   The stored row is never changed. Returns the months stored in this run."
  [^Instant now]
  (let [cur (month-of now)
        stored (into #{} (map (juxt :year :month)) (stored-months))
        floor (.minus now (Duration/ofDays retention-days))]
    (vec
     (for [ym (months-from-first cur false)
           :let [y (.getYear ym) m (.getMonthValue ym)
                 start (month-start ym)]
           :when (and (not (stored [y m])) (not (.isBefore start floor)))
           :let [row (closed-row y m (closed-month-up y m) uptime-target)]
           :when (and row (db/store-availability-month! row))]
       [y m]))))

;; ── Outages ────────────────────────────────────────────────────────────
;; An outage is a run of down minutes. Two down minutes at most 2 minutes apart
;; are one outage, as in the outage report of scripts/prod_logs.py. A minute with
;; no sample counts as down.

(def outage-minimum-minutes 15)

(def outage-cause-labels
  "Stored cause to the label that the page shows. The causes are in `db`."
  db/outage-cause-labels)

(defn detect-outages
  "Pure. `up-seconds` is the set of epoch seconds of the minutes with a
   successful probe, named by the second at which the minute starts. Looks at every minute from `start-sec` to `end-sec`
   (both inclusive, minute-aligned). Returns the outages of 15 minutes or more,
   oldest first, as {:start epoch-seconds :minutes n}."
  [up-seconds start-sec end-sec]
  (let [down (remove up-seconds (range start-sec (inc end-sec) 60))
        runs (reduce (fn [runs t]
                       (let [[s e] (peek runs)]
                         (if (and e (<= (- t e) 120))
                           (conj (pop runs) [s t])
                           (conj runs [t t]))))
                     [] down)]
    (into []
          (comp (map (fn [[s e]] {:start s :minutes (inc (quot (- e s) 60))}))
                (filter #(>= (:minutes %) outage-minimum-minutes)))
          runs)))

(def ^:private range-chunk-minutes 10000) ; Prometheus allows 11000 points

(defn up-minute-starts
  "Pure. From range-query `values` ([epoch-seconds \"1\"|\"0\"] pairs), the set
   of epoch seconds at which an up minute STARTS. A sample at time T covers the
   window (T-1m, T], so the minute it describes starts at T-60. This is the one
   place where sample times become minute starts."
  [values]
  (into #{} (keep (fn [[t v]] (when (= "1" v) (- (long t) 60)))) values))

(defn- up-minutes
  "The set of minute starts (epoch seconds) in [start-sec, end-sec] with a
   successful probe, or nil when Prometheus fails for any part of the range."
  [start-sec end-sec]
  (loop [from start-sec, acc #{}]
    (if (> from end-sec)
      acc
      (let [to (min end-sec (+ from (* 60 (dec range-chunk-minutes))))
            body (query-prometheus "/api/v1/query_range"
                                   {"query" (str "max_over_time(probe_success{instance=\"" probe-instance "\"}[1m]) > bool 0")
                                    "start" (+ from 60) "end" (+ to 60) "step" 60}
                                   15000)]
        (when (= "success" (:status body))
          (recur (+ to 60)
                 (into acc (up-minute-starts (-> body :data :result first :values)))))))))

(defn month-outages
  "Outages of 15 minutes or more of the month (UTC) up to `now`, as
   {:started-at Instant :year :month :minutes}, or nil when Prometheus fails.
   Minutes are named by their start. The last 2 minutes are left out, since
   their samples may not exist yet. An outage that crosses a month edge is cut
   at the edge."
  [year month ^Instant now]
  (let [ym (YearMonth/of (int year) (int month))
        from (.getEpochSecond (month-start ym))
        month-end (.getEpochSecond (month-start (.plusMonths ym 1)))
        to (min (- month-end 60) (* 60 (quot (- (.getEpochSecond now) 180) 60)))]
    (when (<= from to)
      (when-let [up (up-minutes from to)]
        (mapv (fn [{:keys [start minutes]}]
                {:started-at (Instant/ofEpochSecond start) :year year :month month :minutes minutes})
              (detect-outages up from to))))))

(defn store-outages!
  "Store the outages of every month from `first-month` to the month of `now`
   that Prometheus still holds in full, closed months included. A month
   without an answer stays as it is and the next run tries again. A stored
   cause is never changed. Returns the number of outages inserted or lengthened."
  [^Instant now]
  (let [floor (.minus now (Duration/ofDays retention-days))]
    (reduce (fn [n ym]
              (if (.isBefore (month-start ym) floor)
                n
                (reduce (fn [n outage] (if (db/store-availability-outage! outage) (inc n) n))
                        n
                        (month-outages (.getYear ym) (.getMonthValue ym) now))))
            0
            (months-from-first (month-of now) true))))

(defn stored-outages
  "Every stored outage, newest first. Empty when the database fails."
  []
  (try (db/availability-outages)
       (catch Exception e
         (tel/error! {:id ::stored-outages-failed} e)
         [])))

(defn set-outage-cause!
  "Operator function. Set the cause of the outage that started at `started-at`
   (an Instant, or an ISO string such as \"2026-10-28T03:12:00Z\"). `cause` is
   one of :server-internet-provider, :network-tunnel, :freememo-server,
   :not-yet-known. Returns true when an outage changed, false when none starts
   at that time. Any other cause throws and changes nothing."
  [started-at cause]
  (db/set-availability-outage-cause!
   (if (string? started-at) (Instant/parse started-at) started-at)
   (if (keyword? cause) (name cause) cause)))

(defn- close-months-job! [] (close-months! (Instant/now)))
(defn- store-outages-job! [] (store-outages! (Instant/now)))

;; Two jobs, so that a failure in one does not skip the other.
(db/register-sweep-job! ::close-months #'close-months-job!)
(db/register-sweep-job! ::store-outages #'store-outages-job!)
