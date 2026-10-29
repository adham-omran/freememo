(ns freememo.anki-sync-observation
  (:require [hyperfiddle.electric3 :as e]
            [freememo.anki-lease-client :as lease-client]
            #?(:clj [freememo.user-state :as us])))

(e/defn SyncObservation
  "Hold the sync revision while its lease is busy. Observe the latest revision
   after release or expiry. Cancellation stays busy while its batch drains."
  [user-id]
  (e/server
    (let [busy? (lease-client/SyncBusy user-id)
          raw-revision (e/watch (us/get-atom user-id :sync-mutations))
          revision (e/Reconcile (if busy? (e/snapshot raw-revision) raw-revision))]
      [busy? revision])))
