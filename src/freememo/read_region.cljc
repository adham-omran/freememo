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
   (defn timed-read
     "Run read! and log its duration and the name of the thread that ran it.

      The thread name is what tells a read inside e/Offload from a read that
      blocks the session.

      Pre : region is a keyword that names the read.
      Post: returns the value of read!, or rethrows what read! threw.
            Writes one log line per call: :info at or above slow-read-ms,
            :debug below it, and :warn when read! throws."
     [region read!]
     (let [started-at (System/nanoTime)
           elapsed-ms #(long (/ (- (System/nanoTime) started-at) 1000000))
           line (fn [outcome duration-ms]
                  (str "Timed read region=" (name region)
                    " outcome=" outcome
                    " duration-ms=" duration-ms
                    " thread=" (.getName (Thread/currentThread))))]
       (try
         (let [value (read!)
               duration-ms (elapsed-ms)]
           (if (>= duration-ms slow-read-ms)
             (log/log-info (line "ok" duration-ms))
             (log/log-debug (line "ok" duration-ms)))
           value)
         (catch Throwable error
           (log/log-warn
             (str (line "error" (elapsed-ms))
               " error=" (or (.getMessage error) (.getName (class error)))))
           (throw error))))))

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
