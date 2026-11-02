(ns freememo.quick-sync
  "Background reconcile (Cmd-Shift-Opt-X): runs a full Anki reconcile without
   opening the modal. Its outcome is the Activity row's summary or error.

   Own namespace per the JVM 64KB method-limit convention (Electric v3 e/defn).

   The hidden proxy button mounts at the APP ROOT, not in the toolbar. It used to
   live in `toolbar_sync_dropdown`, which made the reconcile driver a child of a
   document's toolbar: navigating away unmounted it and tore down an in-flight
   run. A reconcile spans the whole collection and has no document scope, so
   nothing about it belonged inside one document's chrome.

   No deck or note-type configuration is fetched here. A reconcile's first Anki
   call is `notesInfo`, and the settings a push needs are resolved per root topic
   by `anki-write-bundles` on the server. The connect / apply-prefs phases the
   push version needed are therefore gone."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.anki-lease-client :as lease-client]
   [freememo.anki-reconcile-panels :as recon]
   [freememo.anki-subtree-request :as subtree]
   [freememo.command-bus :as bus]))

(e/defn QuickSyncButton
  "Hidden host button for the `:quick-sync` command (Cmd-Shift-Opt-X or the
   palette). Invoking starts a background reconcile over the whole collection.

   Mounted once at the app root in EVERY tab, so the run outlives any route.
   Only the tab whose claim wins the account's lease runs phases; the Activity
   monitor reads that lease, so this component mirrors nothing into it.

   It takes only `user-id`. The chord and the palette run the whole collection,
   because their point is that a card deleted in a document nobody has open
   still gets its Anki note removed.

   It is also the driver for a BRANCH run: `anki-subtree-request/
   request-subtree-sync!` queues a topic id, and this component collects it and
   starts a run scoped to that topic and its descendants. That run settles no
   tombstones, so a deleted card's note stays until the next full run. Both
   kinds take the same lease, so a branch request while any run is going does
   nothing: every Sync control is disabled by the lease and the Activity badge
   shows the running sync.

   While the account's lease is held, in this tab or another, the invoker is
   retracted and the command is blocked. The `:quick-sync` palette row is then
   disabled with \"Sync is running\" and the chord does nothing (plans/remove-toasts.md A-12)."
  [user-id]
  (e/client
    (let [state (recon/fresh-state)
          sync-busy? (lease-client/SyncBusy user-id)
          start-run! (fn [root-id] (recon/start! state false root-id) nil)]
      (dom/button
        (dom/props {:style {:display "none"} :aria-hidden "true" :tabindex "-1"
                    :data-quick-sync "1"})
        (let [node dom/node
              unwatch! (subtree/watch! start-run!)]
          (e/on-unmount unwatch!)
          (if sync-busy?
            (do (bus/block! :quick-sync "Sync is running")
                (e/on-unmount (fn [] (bus/unblock! :quick-sync))))
            (do (bus/publish-invoker! :quick-sync (fn [] (.click node)))
                (e/on-unmount (fn [] (bus/retract-invoker! :quick-sync))))))
        (dom/On "click" (fn [_] (start-run! nil)) nil))
      (when (some? (e/watch (:!phase state)))
        (recon/ReconcileExecutor user-id state)))))
