(ns freememo.quick-sync
  "Background reconcile (Cmd-Shift-Opt-X): runs a full Anki reconcile without
   opening the modal, and reports the outcome as a toast.

   Own namespace per the JVM 64KB method-limit convention (Electric v3 e/defn).
   The hidden proxy button mounts only inside the toolbar, so the shortcut
   no-ops when no document is open.

   No deck or note-type configuration is fetched here. A reconcile's first Anki
   call is `notesInfo`, and the settings a push needs are resolved per root topic
   by `get-bulk-push-bundles` on the server. The connect / apply-prefs phases the
   push version needed are therefore gone."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.anki-reconcile-panels :as recon]
   [freememo.command-bus :as bus]
   #?(:clj [freememo.toasts :as toasts])))

(defn push-toast!*
  "Push a toast to the user's queue. Errors auto-sticky (toasts/push! default)."
  [user-id level message]
  #?(:clj (do (toasts/push! user-id {:level level :message message}) nil)
     :cljs nil))

(e/defn QuickSyncStartToasts
  "Start and busy toasts. One start toast per `!trigger` increment; one busy
   toast per `!busy-bump` increment (the chord pressed while a run is in flight)."
  [user-id !trigger !busy-bump]
  (e/client
    (let [trigger (e/watch !trigger)
          busy-bump (e/watch !busy-bump)]
      (when (pos? trigger)
        (let [[t _] (e/Token [:qs-start trigger])]
          (when t
            (case (e/server (push-toast!* user-id :info "Reconciling with Anki…")) (t)))))
      (when (pos? busy-bump)
        (let [[t _] (e/Token [:qs-busy busy-bump])]
          (when t
            (case (e/server (push-toast!* user-id :info "Reconcile already in progress")) (t))))))))

(e/defn QuickSyncButton
  "Hidden host button for the `:quick-sync` command (Cmd-Shift-Opt-X or the
   palette). Invoking starts a background reconcile over the whole collection.

   `selected-doc`, `card-type` and `unsynced-count` are kept in the signature for
   the toolbar call site, and unused: a reconcile has no scope to narrow. Its
   whole point is that a card deleted in a document nobody has open still gets
   its Anki note removed."
  [user-id _selected-doc _card-type _unsynced-count]
  (e/client
    (let [state (recon/fresh-state)
          !trigger (atom 0)
          !running? (atom false)
          !busy-bump (atom 0)]
      (dom/button
        (dom/props {:style {:display "none"} :aria-hidden "true" :tabindex "-1"
                    :data-quick-sync "1"})
        (let [node dom/node]
          (bus/publish-invoker! :quick-sync (fn [] (.click node)))
          (e/on-unmount (fn [] (bus/retract-invoker! :quick-sync))))
        (dom/On "click"
          (fn [_]
            (if (= :busy (recon/start! state !running?))
              (swap! !busy-bump inc)
              (swap! !trigger inc)))
          nil))
      (QuickSyncStartToasts user-id !trigger !busy-bump)
      (recon/ReconcileExecutor user-id state !running?))))
