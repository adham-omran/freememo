(ns freememo.read-region
  "Measured server reads for progressive UI regions."
  (:require
   [freememo.loading :as loading]
   #?(:clj [freememo.config :as config])
   #?(:clj [freememo.logging :as log])))

#?(:clj
   (def slow-read-ms
     "A timed read that takes at least this many milliseconds logs at :info.
      SLOW_READ_LOG_MS overrides the default. The default matches the 250 ms
      slow-statement log in plans/server-query-performance.md 1.1.2. A value of
      0 logs every read at :info, which is what timing a code path needs."
     (or (some-> (config/env-or-config "SLOW_READ_LOG_MS" [:logging :slow-read-ms])
           str parse-long)
         250)))

#?(:clj
   (def ^:dynamic *read-context*
     "Optional aggregate context for a controlled measurement run."
     {}))

#?(:clj
   (defn read-event
     "Build safe metric fields without query text, account IDs, or content."
     [region context outcome duration-ms]
     (merge {:metric "server-read"
             :region (name region)
             :outcome outcome
             :duration-ms duration-ms
             :thread (.getName (Thread/currentThread))
             :build (or (System/getenv "READ_BUILD_REVISION") (config/git-commit))
             :run (or (System/getenv "READ_TRACE_RUN") "unassigned")
             :route "unspecified"
             :phase "query"}
       (select-keys context [:build :run :route :phase :fixture-size :result-count]))))

#?(:clj
   (defn timed-read
     "Run read! and log its duration and the name of the thread that ran it.

      The thread name is what tells a read inside e/Offload from a read that
      blocks the session.

      Pre : region is a keyword that names the read.
      Post: returns the value of read!, or rethrows what read! threw.
            Writes one log line per call: :info at or above slow-read-ms,
            :debug below it, and :warn when read! throws."
     ([region read!] (timed-read region *read-context* read!))
     ([region context read!]
     (let [started-at (System/nanoTime)
           elapsed-ms #(/ (double (- (System/nanoTime) started-at)) 1000000.0)
           line (fn [{:keys [outcome duration-ms thread build run route phase]}]
                  (str "Timed read region=" (name region)
                    " outcome=" outcome
                    " duration-ms=" duration-ms
                    " thread=" thread
                    " build=" build " run=" run " route=" route " phase=" phase))]
       (try
         (let [value (read!)
               duration-ms (elapsed-ms)
               event (read-event region
                       (cond-> context (vector? value) (assoc :result-count (count value)))
                       "ok" duration-ms)]
           (if (>= duration-ms slow-read-ms)
             (log/log-info (line event) event)
             (log/log-debug (line event) event))
           value)
         (catch Throwable error
           (let [event (assoc (read-event region context "error" (elapsed-ms))
                         :error-class (.getName (class error)))]
             (log/log-warn
               (str (line event)
                 " error=" (or (.getMessage error) (.getName (class error)))) event))
           (throw error)))))))

(defn measured-result
  "Measure one server read and convert its outcome to a region result.

   Pre : read! is side-effect-free apart from its database read.
   Post: returns region-success or region-failure. timed-read logs the duration.
   Invariant: an exception becomes local region data. It cannot close the route."
  [region read!]
  #?(:clj
     (try
       (loading/region-success (timed-read region read!))
       (catch Exception error
         (loading/region-failure
           (or (.getMessage error) "Unknown error"))))
     :cljs (loading/region-failure "Server read is unavailable on the client.")))
