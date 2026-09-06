(ns freememo.read-region
  "Measured server reads for progressive UI regions."
  (:require
   [freememo.loading :as loading]
   #?(:clj [freememo.logging :as log])))

(defn measured-result
  "Measure one server read and convert its outcome to a region result.

   Pre : read! is side-effect-free apart from its database read.
   Post: returns region-success or region-failure and logs server duration.
   Invariant: an exception becomes local region data. It cannot close the route."
  [region read!]
  #?(:clj
     (let [started-at (System/nanoTime)]
       (try
         (let [value (read!)
               duration-ms (long (/ (- (System/nanoTime) started-at) 1000000))]
           (log/log-debug
             (str "Progressive read region=" (name region)
               " outcome=ok duration-ms=" duration-ms))
           (loading/region-success value))
         (catch Exception error
           (let [duration-ms (long (/ (- (System/nanoTime) started-at) 1000000))]
             (log/log-warn
               (str "Progressive read region=" (name region)
                 " outcome=error duration-ms=" duration-ms
                 " error=" (or (.getMessage error) (.getName (class error)))))
             (loading/region-failure
               (or (.getMessage error) "Unknown error"))))))
     :cljs (loading/region-failure "Server read is unavailable on the client.")))
