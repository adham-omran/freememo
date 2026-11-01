(ns freememo.anki-note-deletion-client
  (:require [hyperfiddle.electric3 :as e]
            [freememo.anki-note-deletion :as deletion]
            [freememo.client-state :as cs]))

(defn start! [call! note-ids]
  (deletion/start-owned! (cs/get-atom :manual-note-deletion-owner) call! note-ids))

(e/defn Owner [user-id]
  (e/client
    (e/for-by identity [uid (if user-id [user-id] [])]
      (let [!current (cs/get-atom :manual-note-deletion-owner)
            owner (e/snapshot (deletion/mount-owner! !current uid))]
        (e/on-unmount #(deletion/close-owner! !current owner))
        (:id owner)))))
