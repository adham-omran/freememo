(ns freememo.availability-http
  "Public availability page at /availability. Plain server-rendered HTML, like
   freememo.docs-http: no session, no WebSocket, no script. The wording is
   approved in plans/paying-customer-commitments.md, \"Availability page\"."
  (:require [clojure.string :as str]
            [freememo.availability :as availability])
  (:import [java.time Month]
           [java.time.format TextStyle]
           [java.util Locale]))

(def ^:private lead
  (str "FreeMemo aims for 99.0% availability in each calendar month, in UTC. Availability is the "
       "share of minutes in which a probe on the public internet reached FreeMemo. A minute with no "
       "measurement counts as down. This is a target, not a guarantee, and a missed month gives no refund."))

(def ^:private not-available
  "The figures for the current month are not available now. Try again later.")

(defn- esc [value]
  (str/escape (str value) {\& "&amp;" \< "&lt;" \> "&gt;" \" "&quot;" \' "&#39;"}))

(defn- month-label [{:keys [year month]}]
  (str (.getDisplayName (Month/of (int month)) TextStyle/FULL Locale/ENGLISH) " " year))

(defn- row-html [label percent target result]
  (str "<tr><td>" (esc label) "</td><td>" (esc (availability/format-percent percent)) "</td>"
       "<td>" (esc (str target "%")) "</td><td>" result "</td></tr>"))

(defn- closed-row-html [{:keys [minutes-up minutes target] :as row}]
  (row-html (month-label row) (* 100.0 (/ (double minutes-up) minutes)) target
            (if (= :met (availability/result row)) "Met" "Missed")))

(defn- table [current closed]
  (str "<table class=\"availability-table\"><thead><tr>"
       "<th scope=\"col\">Month</th><th scope=\"col\">Availability</th>"
       "<th scope=\"col\">Target</th><th scope=\"col\">Result</th></tr></thead><tbody>"
       (when current
         (row-html (month-label current) (:percent current) availability/uptime-target "In progress"))
       (apply str (map closed-row-html closed))
       "</tbody></table>"))

(def ^:private style
  (str "<style>"
       ".availability-table { width: 100%; max-width: 760px; border-collapse: collapse; margin-top: 28px; }"
       ".availability-table th, .availability-table td { text-align: left; padding: 12px 14px; border-bottom: 1px solid var(--docs-line); }"
       ".availability-table th { font-size: 13px; color: var(--docs-muted); font-weight: 650; }"
       ".availability-note { margin-top: 28px; padding: 14px 16px; border: 1px solid var(--docs-line); background: var(--docs-paper); max-width: 760px; }"
       ".availability-lead { max-width: 760px; font-size: 18px; color: var(--docs-muted); }"
       "@media (max-width: 520px) { .availability-table th, .availability-table td { padding: 10px 6px; font-size: 14px; } }"
       "</style>"))

(defn page-html
  "Render the page for `current`, a map from `availability/current-month` or nil,
   and `closed`, the stored months to show, newest first."
  [current closed]
  (str "<!DOCTYPE html><html lang=\"en\"><head><meta charset=\"utf-8\">"
       "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">"
       "<title>Availability</title>"
       "<meta name=\"description\" content=\"" (esc lead) "\">"
       "<link rel=\"canonical\" href=\"https://freememo.net/availability\">"
       "<link rel=\"icon\" href=\"/freememo/favicon-192.png\">"
       "<link rel=\"stylesheet\" href=\"/freememo/docs.css\">" style "</head><body>"
       "<header class=\"docs-header\"><a class=\"docs-brand\" href=\"/\">FreeMemo</a></header>"
       "<main id=\"content\" class=\"docs-main\"><h1>Availability</h1>"
       "<p class=\"availability-lead\">" (esc lead) "</p>"
       (when (or current (seq closed)) (table current closed))
       (when-not current
         (str "<p class=\"availability-note\" role=\"status\">" (esc not-available) "</p>"))
       "</main></body></html>"))

(defn response
  "Return the availability page for GET and HEAD /availability, 405 for another
   method, and nil for another route."
  [{:keys [uri request-method now]}]
  (when (= "/availability" uri)
    (let [allowed? (contains? #{nil :get :head} request-method)]
      {:status (if allowed? 200 405)
       :headers (cond-> {"Content-Type" "text/html; charset=utf-8"
                         "Cache-Control" "public, max-age=60"}
                  (not allowed?) (assoc "Allow" "GET, HEAD"))
       :body (cond
               (= :head request-method) ""
               allowed? (page-html (availability/cached-current-month)
                          (availability/visible-months (availability/stored-months)
                                                       (or now (java.time.Instant/now))))
               :else "Method not allowed")})))
