(ns freememo.availability
  "Availability of FreeMemo for the current calendar month, from Prometheus.

   Availability is the share of minutes in the month in which the public probe
   reached FreeMemo. A minute with no successful sample counts as down, so the
   count comes from `sum_over_time` over a one-minute subquery, never from
   `avg_over_time` (which skips gaps). The current month divides by the minutes
   so far. Months are in UTC. Terms: CONTEXT.md, \"Service Commitments\".
   Plan: plans/paying-customer-commitments.md, Step 2."
  (:require [cheshire.core :as json]
            [clj-http.client :as http]
            [freememo.config :as config])
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

(defn- probe-query [minutes]
  (str "sum_over_time((max_over_time(probe_success{instance=\"" probe-instance "\"}[1m]) > bool 0)[" minutes "m:1m])"))

(defn minutes-up
  "Read the minutes-up count from a Prometheus instant-query body (parsed JSON).
   Returns nil unless the body is a successful answer with one sample."
  [body]
  (when (= "success" (:status body))
    (some-> body :data :result first :value second parse-double)))

(defn availability
  "Pure calculation. `up` is the count from Prometheus, `minutes` the minutes of
   the period. Up is clamped to the period. Returns a map, or nil when either
   input is missing."
  [up minutes]
  (when (and up (pos? minutes))
    (let [up (long (Math/min (double (Math/round (double up))) (double minutes)))]
      {:minutes minutes
       :minutes-up up
       :percent (* 100.0 (/ (double up) minutes))})))

(defn- query-prometheus
  "Ask Prometheus at instant `at`. Returns the parsed body, or nil on any failure."
  [promql ^Instant at]
  (try
    (let [resp (http/get (str (prometheus-url) "/api/v1/query")
                         {:query-params {"query" promql "time" (.getEpochSecond at)}
                          :throw-exceptions false
                          :socket-timeout 5000
                          :connection-timeout 3000})]
      (when (= 200 (:status resp))
        (json/parse-string (:body resp) true)))
    (catch Exception _ nil)))

(defn current-month
  "Availability of the month that contains `now`, divided by the minutes so far.
   Returns {:year :month :minutes :minutes-up :percent}, or nil when Prometheus
   does not answer or has no sample."
  [^Instant now]
  (let [ym (YearMonth/from (.atOffset now ZoneOffset/UTC))
        start (.toInstant (.atStartOfDay (.atDay ym 1) ZoneOffset/UTC))
        minutes (long (Math/ceil (/ (.toSeconds (Duration/between start now)) 60.0)))]
    (when (pos? minutes)
      (when-let [result (some-> (query-prometheus (probe-query minutes) now)
                                minutes-up
                                (availability minutes))]
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
