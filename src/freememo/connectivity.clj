(ns freememo.connectivity
  "The Local Install's network check (plans/local-install.md, ticket 06).
   The JVM finds out by itself when freememo.net is reachable. Electron's
   `watchConnectivity` report is not needed.

   A check asks the origin for `/` and treats any HTTP answer as online.
   `desktop-link/set-online!` records the result. An offline to online change
   runs the `link/on-online!` listeners, and the sync scheduler starts a sync.

   Checks run every 15 seconds, and at once after each failed sync
   (`link/on-sync-failed!`). They share one thread, so two checks never
   overlap."
  (:require [clj-http.client :as http]
            [freememo.desktop-link :as link])
  (:import [java.util.concurrent Executors ScheduledExecutorService ThreadFactory TimeUnit]))

(def interval-seconds 15)

(defn reachable?
  "Post: true when `origin` answers with any HTTP status within 5 seconds."
  [origin]
  (try
    (http/head origin {:throw-exceptions false :connection-timeout 5000 :socket-timeout 5000})
    true
    (catch java.io.IOException _ false)))

(defn check!
  "Check once and record the result.
   `on-offline!` runs when the link was online and is not now.
   Post: the link state matches the answer. Returns true when online, false
         when offline, nil when the link has no origin."
  ([] (check! reachable? nil))
  ([reachable-fn on-offline!]
   (when-let [origin (link/origin)]
     (let [was-online? (link/online?)
           online? (boolean (reachable-fn origin))]
       (link/set-online! online?)
       (when (and was-online? (not online?) on-offline!) (on-offline!))
       online?))))

(defonce ^:private !executor (atom nil))

(defn start!
  "Pre:  `link/configure!` has run (the desktop boot does it).
   Post: a check runs every 15 seconds and after each failed sync. `opts`:
         `:on-offline!` as in `check!`. A second call does nothing."
  [{:keys [on-offline!]}]
  (when-not @!executor
    (let [^ScheduledExecutorService ex (Executors/newSingleThreadScheduledExecutor
                                         (reify ThreadFactory
                                           (newThread [_ r] (doto (Thread. r "freememo-connectivity")
                                                              (.setDaemon true)))))
          run (fn [] (try (check! reachable? on-offline!)
                          (catch Throwable _ nil)))]
      (reset! !executor ex)
      (.scheduleWithFixedDelay ex ^Runnable run interval-seconds interval-seconds TimeUnit/SECONDS)
      (link/on-sync-failed! #(.execute ex ^Runnable run)))))
