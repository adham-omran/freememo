(ns freememo.topic-move
  "Forward side of topic re-parenting (custom nesting). Moves a topic subtree
   under a new parent — or to the top level when new-parent-id is nil — logs one
   reversible 'move-topic' undo entry, bumps :tree-mutations so both tree views
   re-query. Reverse logic lives in freememo.undo.

   Mirrors freememo.staged-delete: db.clj owns the guarded persistence
   (reparent-topic!), this ns owns the reactive-channel bump."
  (:require
   [freememo.db :as db]
   [freememo.commands :as commands]
   [taoensso.telemere :as tel]))

(defn move-topic!
  "Re-parent topic-id under new-parent-id (nil ⇒ top level) for user-id.
   Returns the db result map (carries :entry-id) on success, or nil when the
   move is rejected (not owned / would create a cycle / cross-user target /
   no-op). On success bumps :tree-mutations."
  [user-id topic-id new-parent-id]
  (try
    (when-let [r (db/reparent-topic! user-id topic-id new-parent-id)]
      (commands/bump! user-id :move-topic)
      (tel/log! :info (str "Moved topic-id=" topic-id " parent=" new-parent-id))
      r)
    (catch Exception e
      (tel/error! {:id ::move-topic} e)
      nil)))
