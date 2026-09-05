(ns freememo.client-failure-http
  "HTTP boundary for fatal Electric client reports."
  (:require
   [cheshire.core :as json]
   [freememo.geo :as geo]
   [taoensso.telemere :as tel]))

(def ^:private max-report-bytes 16384)

(defonce ^:private !recent-reports (atom {}))

(defn- json-response [status body]
  {:status status
   :headers {"Content-Type" "application/json"}
   :body (json/generate-string body)})

(defn- bounded-param [params key max-length]
  (let [value (str (or (get params key) ""))]
    (subs value 0 (min max-length (count value)))))

(defn- scalar-log-value [value]
  (cond
    (string? value) (subs value 0 (min 200 (count value)))
    (or (number? value) (boolean? value) (nil? value)) value
    :else (subs (str value) 0 (min 200 (count (str value))))))

(defn- sanitized-interactions [json-text]
  (try
    (let [entries (json/parse-string (or json-text "[]") true)]
      (if-not (sequential? entries)
        []
        (into []
          (map
            (fn [entry]
              {:kind (scalar-log-value (:kind entry))
               :detail (into {}
                         (map (fn [[key value]]
                                [key (scalar-log-value value)]))
                         (if (map? (:detail entry)) (:detail entry) {}))}))
          (take-last 20 entries))))
    (catch Exception _ [])))

(defn- accept-report?! [user-id session-id fingerprint now-ms]
  (let [accepted? (volatile! false)
        key [user-id session-id]]
    (swap! !recent-reports
      (fn [reports]
        (let [reports (into {}
                        (filter (fn [[_ {:keys [at-ms]}]]
                                  (< (- now-ms at-ms) 300000)))
                        reports)
              previous (get reports key)
              rate-limited? (and previous (< (- now-ms (:at-ms previous)) 5000))
              duplicate? (and previous (= fingerprint (:fingerprint previous))
                           (< (- now-ms (:at-ms previous)) 60000))]
          (if (or rate-limited? duplicate?)
            reports
            (do
              (vreset! accepted? true)
              (assoc reports key {:at-ms now-ms :fingerprint fingerprint}))))))
    @accepted?))

(defn handler
  "Record a fatal browser failure after the Electric connection stops.

   Pre: the request uses the authenticated app session and form parameters.
   Post: one bounded structured log is written unless rate limits reject it.
   Invariant: the payload contains no assistant or document content."
  [request]
  (if-let [user-id (get-in request [:session :user-id])]
    (let [params (:params request)
          report-bytes
          (reduce + 0
            (map #(alength (.getBytes (str %) java.nio.charset.StandardCharsets/UTF_8))
              (mapcat identity params)))]
      (if (> report-bytes max-report-bytes)
        (json-response 413 {:success false :error "Failure report is too large."})
        (let [session-id (bounded-param params "session-id" 100)
              message (bounded-param params "message" 2000)
              stack (bounded-param params "stack" 8000)
              termination (bounded-param params "termination" 40)
              fingerprint (hash [termination message stack
                                 (get params "client-version")])
              accepted? (accept-report?! user-id session-id fingerprint
                          (System/currentTimeMillis))]
          (when accepted?
            (tel/log!
              {:level :error :id ::client-reactor-failure
               :data
               {:user-id user-id
                :session-id session-id
                :termination termination
                :message message
                :stack stack
                :error-data (bounded-param params "error-data" 2000)
                :route (bounded-param params "route" 1000)
                :client-version (bounded-param params "client-version" 200)
                :topic-id (bounded-param params "topic-id" 100)
                :page-topic-id (bounded-param params "page-topic-id" 100)
                :chat-id (bounded-param params "chat-id" 100)
                :run-uuid (bounded-param params "run-uuid" 100)
                :run-kind (bounded-param params "run-kind" 40)
                :run-status (bounded-param params "run-status" 40)
                :render-phase (bounded-param params "render-phase" 40)
                :interactions (sanitized-interactions (get params "interactions"))
                :remote-ip (geo/client-ip request)
                :user-agent (get-in request [:headers "user-agent"])}}
              "Electric client process failed"))
          (json-response 202 {:success true :accepted accepted?}))))
    (json-response 401 {:success false :error "Authentication required."})))
