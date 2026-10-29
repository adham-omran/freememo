(ns freememo.anki-review-overlay
  "Live, read-only Anki state for card lists."
  (:require
   [hyperfiddle.electric3 :as e]
   [missionary.core :as m]
   [freememo.anki-overlay-reads :as overlay-reads]
   [freememo.anki-digest :as digest]
   [freememo.task-request :as request]
   [freememo.task-owner :as owner]
   [freememo.anki-sync-helpers :as helpers]
   [freememo.client-state :as cs]
   [freememo.logging :as log]
   #?(:clj [freememo.anki-sync-server :as sync-server])
   #?(:clj [freememo.user-state :as us])))

(defn start-fetch!
  "Read mark, suspension, remark, and field state from AnkiConnect.
   Return a canceller that rejects callbacks after frame teardown."
  [note-count _trigger !requests !status !payload]
  #?(:cljs
     (let [my-gen (random-uuid)]
       (reset! !status :checking)
       (log/log-debug (str "[anki-overlay] fetch start gen=" my-gen
                        " notes=" note-count))
       (owner/start!
         (m/sp
           (loop [offset 0 result {:present [] :absent []}]
             (if (< offset note-count)
               (let [targets (m/? (request/request! !requests {:offset offset}))
                     ids (mapv :note-id targets)
                     kinds (into {} (map (juxt :note-id :kind)) targets)
                     chunk (m/? (overlay-reads/read-overlay! helpers/anki-call! ids kinds))]
                 (recur (+ offset 500)
                   (-> result (update :present into (:present chunk))
                     (update :absent into (:absent chunk)))))
               result)))
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

(e/defn OverlayRequests [manifest !requests]
  (e/client
    (let [pending (e/watch !requests)]
      (e/for-by :id [item (if pending [pending] [])]
        (let [offset (get-in item [:data :offset])
              [t ?error] (e/Token (:id item))]
          (when t
            (let [ids (e/server (mapv #(select-keys % [:note-id :kind]) (take 500 (drop offset manifest))))]
              (case ids
                (do (e/on-unmount #(request/resolve! item ids))
                    (t))))))))))

(e/defn AnkiOverlay [user-id manifest !status !payload !check-tick]
  (e/client
    (let [sync-rev (e/server (e/watch (us/get-atom user-id :sync-mutations)))
          card-rev (e/server (e/watch (us/get-atom user-id :card-mutations)))
          payload (e/watch !payload)
          !requests (atom nil)
          note-count (e/server (count manifest))
          manifest-key (e/server (digest/text-fingerprint (pr-str manifest)))
          check-tick (e/watch !check-tick)
          action-refresh (e/watch (cs/get-atom :anki-overlay-refresh))]
      (OverlayRequests manifest !requests)
      (e/for-by identity [_key [[manifest-key sync-rev check-tick action-refresh]]]
        (when (pos? note-count)
          (let [cancel! (e/snapshot
                          (start-fetch! note-count [sync-rev check-tick action-refresh] !requests !status !payload))]
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
