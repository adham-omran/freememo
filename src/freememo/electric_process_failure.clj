(ns freememo.electric-process-failure
  "Log the failure of an Electric server process when it happens.

   `hyperfiddle.electric-ring-adapter3` logs `Websocket handler failure.` from
   the failure continuation of an `m/join`. That continuation runs only after
   the process, the keepalive timeout and the heartbeat have all ended. Two
   things then hide the exception:

   - The process ends only after its teardown ends. An `e/Offload` in flight
     keeps the teardown open until its thunk returns, and an HTTP call ignores
     the interrupt. A teardown that never ends means no log at all.
   - `failure` logs only while the socket is open. The failed process has
     already cancelled its message subscription, so the next client message
     fails the `on-message-slot` assertion and Jetty closes the socket. When
     the teardown ends later, `failure` finds the socket closed and logs
     nothing.

   The exception reaches the adapter first as a throw from the process flow's
   transfer. This wrapper logs it there, before any teardown, and rethrows it.
   See plans/electric-process-failure-log.md."
  (:require [taoensso.telemere :as tel])
  (:import (java.util.concurrent.atomic AtomicBoolean)
           (missionary Cancelled)))

(defn- request-context
  "The request fields a failure line carries. The session cookie is not one.
   `:uri` is the path the tab loaded, because this is the upgrade request."
  [ring-request]
  {:user-id (get-in ring-request [:session :user-id])
   :uri (:uri ring-request)})

(defn- log-failure-once!
  [^AtomicBoolean logged? ring-request ^Throwable e]
  (when (and (not (instance? Cancelled e))
             (.compareAndSet logged? false true))
    (tel/error! {:id ::process-failed
                 :msg "Electric server process failed"
                 :data (request-context ring-request)}
      e)))

(defn logged
  "Return `boot` with each failure of its process logged once.

   Pre: `boot` is what `e/boot-server` returns, a fn from the adapter's input
   subject to the process flow.
   Post: the flow behaves as before. The first non-`Cancelled` throw from its
   transfer writes one `::process-failed` error with the stack trace, the
   `user-id` and the page path, and is then rethrown unchanged."
  [ring-request boot]
  (fn [subject]
    (let [flow (boot subject)
          logged? (AtomicBoolean. false)]
      (fn [notifier terminator]
        (let [process (flow notifier terminator)]
          (reify
            clojure.lang.IFn
            (invoke [_] (process))
            clojure.lang.IDeref
            (deref [_]
              (try @process
                (catch Throwable e
                  (log-failure-once! logged? ring-request e)
                  (throw e))))))))))
