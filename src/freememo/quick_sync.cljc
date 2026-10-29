(ns freememo.quick-sync
  "Background reconcile (Cmd-Shift-Opt-X): runs a full Anki reconcile without
   opening the modal, and reports the outcome as a toast.

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
   [freememo.anki-reconcile-panels :as recon]
   [freememo.anki-subtree-request :as subtree]
   [freememo.command-bus :as bus]
   #?(:clj [freememo.toasts :as toasts])))

(defn push-toast!*
  "Push a toast to the user's queue. Errors auto-sticky (toasts/push! default)."
  [user-id level message]
  #?(:clj (do (toasts/push! user-id {:level level :message message}) nil)
     :cljs nil))

(e/defn QuickSyncBusyToast
  "One toast per `!busy-bump` increment: the chord pressed while THIS tab is
   already running a sync. A run in ANOTHER tab is refused by the server claim,
   which toasts for itself (`anki-reconcile-panels/ReconcileClaim`)."
  [user-id !busy-bump]
  (e/client
    (let [busy-bump (e/watch !busy-bump)]
      (when (pos? busy-bump)
        (let [[t _] (e/Token [:qs-busy busy-bump])]
          (when t
            (case (e/server (push-toast!* user-id :info "Sync already in progress")) (t))))))))

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
   kinds take the same lease, so a branch request while any run is going gets
   the \"Sync already in progress\" toast."
  [user-id]
  (e/client
    (let [state (recon/fresh-state)
          !busy-bump (atom 0)
          start-run! (fn [root-id]
                       (when (= :busy (recon/start! state false root-id))
                         (swap! !busy-bump inc)))]
      (dom/button
        (dom/props {:style {:display "none"} :aria-hidden "true" :tabindex "-1"
                    :data-quick-sync "1"})
        (let [node dom/node
              unwatch! (subtree/watch! start-run!)]
          (bus/publish-invoker! :quick-sync (fn [] (.click node)))
          (e/on-unmount (fn []
                          (bus/retract-invoker! :quick-sync)
                          (unwatch!))))
        (dom/On "click" (fn [_] (start-run! nil)) nil))
      (QuickSyncBusyToast user-id !busy-bump)
      (when (some? (e/watch (:!phase state)))
        (recon/ReconcileExecutor user-id state)))))
