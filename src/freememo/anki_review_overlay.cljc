(ns freememo.anki-review-overlay
  "Live, read-only Anki state for card lists."
  (:require
   [hyperfiddle.electric3 :as e]
   [missionary.core :as m]
   [freememo.anki-overlay-reads :as overlay-reads]
   [freememo.task-owner :as owner]
   [freememo.anki-review :as review]
   [freememo.anki-sync-helpers :as helpers]
   [freememo.client-state :as cs]
   [freememo.logging :as log]
   [freememo.math :as math]
   #?(:clj [freememo.anki-sync-server :as sync-server])
   #?(:clj [freememo.user-state :as us])))

(defn start-fetch!
  "Read mark, suspension, remark, and field state from AnkiConnect.
   Return a canceller that rejects callbacks after frame teardown."
  [manifest _trigger !status !payload]
  #?(:cljs
     (let [my-gen (random-uuid)
           note-ids (mapv :note-id manifest)]
       (reset! !status :checking)
       (log/log-debug (str "[anki-overlay] fetch start gen=" my-gen
                        " notes=" (count note-ids)))
       (owner/start! (overlay-reads/read-overlay! helpers/anki-call! note-ids)
        (fn [result]
          (log/log-debug (str "[anki-overlay] fetch done gen=" my-gen
                          " present=" (count (:present result))
                          " absent=" (count (:absent result))))
          (reset! !payload result)
          (reset! !status :ready))
        (fn [err]
          (log/log-debug (str "[anki-overlay] fetch failed gen=" my-gen
                          " error=" (.-message err)))
          (reset! !status :unavailable))))
     :clj nil))

(e/defn AnkiOverlay [user-id manifest !status !payload !check-tick]
  (e/client
    (let [sync-rev (e/server (e/watch (us/get-atom user-id :sync-mutations)))
          card-rev (e/server (e/watch (us/get-atom user-id :card-mutations)))
          payload (e/watch !payload)
          check-tick (e/watch !check-tick)
          action-refresh (e/watch (cs/get-atom :anki-overlay-refresh))]
      (e/for-by identity [_key [[manifest sync-rev check-tick action-refresh]]]
        (when (seq manifest)
          (let [cancel! (e/snapshot
                          (start-fetch! manifest [sync-rev check-tick action-refresh] !status !payload))]
            (e/on-unmount cancel!))))
      ;; `e/as-vec` is load-bearing, and it must be on the CLIENT. When the
      ;; payload arrives, the `when` mounts a new server form, and the client
      ;; has no value for it until the apply call returns and its answer
      ;; crosses: an e/Offload is EMPTY before its first result, and a new
      ;; server form is empty for its round trip. `or` over an empty value is
      ;; empty too, and so was every value derived from the overlay. In
      ;; `library_cards` that is the query opts, then the query result, then
      ;; the one-batch gate: the whole view unmounted and mounted again.
      ;; `as-vec` is [] while nothing has arrived, so the overlay stays {}.
      (let [answers (e/as-vec
                      (when (some? payload)
                        (let [present (:present payload)
                              absent (:absent payload)
                              overlay-rev (+ sync-rev card-rev)]
                          (e/server
                            (:per-card (e/Offload #(sync-server/apply-anki-overlay!
                                                     overlay-rev user-id present absent)))))))]
        (or (peek answers) {})))))
