(ns freememo.staged-delete
  "Forward side of staged document deletion. Stages a topic subtree + binary for
   deletion (hidden, reversible 12h), hard-deletes its cards (snapshotted), and
   surfaces an Undo toast. Reverse logic lives in freememo.undo; the background
   hard-delete lives in freememo.db/purge-staged-documents!.

   Two callers differ only in which reactive channel they may bump:
   the library tree refreshes via :tree-mutations; the topic toolbar must NOT
   bump :refresh mid-tick (it would unmount the toolbar before the client can
   navigate away), so it passes refresh? false."
  (:require
   [freememo.db :as db]
   [freememo.user-state :as us]
   [freememo.toasts :as toasts]
   [taoensso.telemere :as tel]))

(defn stage-deletion!
  "Stage topic-id (and its subtree) for deletion. Pushes an Undo toast.
   Returns {:note-ids [...] :parent-id _ :entry-id _} for the client's Anki
   cleanup + navigation, or nil when the topic isn't owned by user-id.
   When refresh? is true also bumps :refresh (library path)."
  [user-id topic-id refresh?]
  (try
    (when-let [r (db/stage-topic-for-deletion! user-id topic-id)]
      (swap! (us/get-atom user-id :tree-mutations) inc)
      (when refresh? (swap! (us/get-atom user-id :refresh) inc))
      (toasts/push! user-id {:level :success
                             :message "Document deleted"
                             :dedup? false
                             :actions [{:label "Undo" :undo-id (:entry-id r)}]})
      (tel/log! :info (str "Staged document delete topic-id=" topic-id
                        " cards=" (:card-count r)))
      (select-keys r [:note-ids :parent-id :entry-id]))
    (catch Exception e
      (tel/error! {:id ::stage-deletion} e)
      nil)))
