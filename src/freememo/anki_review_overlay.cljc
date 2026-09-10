(ns freememo.anki-review-overlay
  "Live, read-only Anki state for card lists."
  (:require
   [hyperfiddle.electric3 :as e]
   [missionary.core :as m]
   [freememo.anki-review :as review]
   [freememo.anki-sync-helpers :as helpers]
   [freememo.client-state :as cs]
   [freememo.logging :as log]
   [freememo.math :as math]
   #?(:clj [freememo.anki-sync-server :as sync-server])
   #?(:clj [freememo.user-state :as us])))

(defonce !anki-fetch-gen (atom 0))

(defn start-fetch!
  "Read mark, suspension, remark, and field state from AnkiConnect.
   The latest request owns the result atoms."
  [manifest _trigger !status !payload]
  #?(:cljs
     (let [my-gen (swap! !anki-fetch-gen inc)
           note-ids (mapv :note-id manifest)]
       (reset! !status :checking)
       (log/log-debug (str "[anki-overlay] fetch start gen=" my-gen
                        " notes=" (count note-ids)))
       ((m/sp
          (let [notes (m/? (helpers/anki-call! "notesInfo" {:notes note-ids}))
                notes-clj (js->clj notes :keywordize-keys true)
                pairs (map vector note-ids notes-clj)
                absent (into []
                         (keep (fn [[nid note]]
                                 (when (or (nil? note) (empty? note) (nil? (:noteId note)))
                                   nid)))
                         pairs)
                present (into []
                          (keep (fn [[nid note]]
                                  (when (and (map? note) (:noteId note))
                                    {:note-id nid
                                     :stripped-fields (mapv (comp helpers/strip-html
                                                              math/anki->stored-html)
                                                        (helpers/ordered-field-values (:fields note)))
                                     :tags (vec (:tags note))
                                     :remark (review/note-remark (:fields note))
                                     :card-ids (vec (:cards note))})))
                          pairs)
                all-card-ids (into [] (mapcat :card-ids) present)
                suspended-flags (if (seq all-card-ids)
                                  (vec (js->clj (m/? (helpers/anki-call! "areSuspended"
                                                       {:cards all-card-ids}))))
                                  [])
                card-id->suspended (zipmap all-card-ids suspended-flags)]
            {:present (mapv (fn [{:keys [card-ids] :as note}]
                              (-> note
                                (dissoc :card-ids)
                                (assoc :suspended
                                  {:total (count card-ids)
                                   :suspended (count (filter #(true? (get card-id->suspended %))
                                                       card-ids))})))
                        present)
             :absent absent}))
        (fn [result]
          (when (= my-gen @!anki-fetch-gen)
            (log/log-debug (str "[anki-overlay] fetch done gen=" my-gen
                             " present=" (count (:present result))
                             " absent=" (count (:absent result))))
            (reset! !payload result)
            (reset! !status :ready)))
        (fn [err]
          (when (= my-gen @!anki-fetch-gen)
            (log/log-debug (str "[anki-overlay] fetch failed gen=" my-gen
                             " error=" (.-message err)))
            (reset! !status :unavailable)))))
     :clj nil))

(e/defn AnkiOverlay [user-id manifest !status !payload !check-tick]
  (e/client
    (let [sync-rev (e/server (e/watch (us/get-atom user-id :sync-mutations)))
          card-rev (e/server (e/watch (us/get-atom user-id :card-mutations)))
          payload (e/watch !payload)
          check-tick (e/watch !check-tick)
          action-refresh (e/watch (cs/get-atom :anki-overlay-refresh))]
      (when (seq manifest)
        (start-fetch! manifest [sync-rev check-tick action-refresh] !status !payload))
      (let [response (when (some? payload)
                       (let [present (:present payload)
                             absent (:absent payload)
                             overlay-rev (+ sync-rev card-rev)]
                         (e/server
                           (e/Offload #(sync-server/apply-anki-overlay!
                                        overlay-rev user-id present absent)))))]
        (or (:per-card response) {})))))
