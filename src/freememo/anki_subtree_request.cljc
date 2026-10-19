(ns freememo.anki-subtree-request
  "The seam between a caller that wants a branch of the tree synced to Anki and
   the one component that can do it.

   A reconcile is driven from the browser. The AnkiConnect connection is on
   localhost, so only a mounted client component can talk to it
   (`quick-sync/QuickSyncButton`, mounted once at the app root). A caller far
   from that component, such as a context-menu handler, cannot call a server fn
   to start a run, and cannot reach the component's atoms.

   So the caller leaves a request here and the driver collects it:

     caller:  (request-subtree-sync! topic-id)
     driver:  (watch! (fn [root-id] …start a run…))    returns an unwatch thunk

   This namespace has no requires and no reader conditional. It holds a
   `defonce` atom that the driver watches, so the var must resolve on BOTH peers
   or the dual compile emits mismatched frame-signal counts (the reason
   `activity-client/!tab-drivers` is written the same way). The server's copy is
   inert: nothing there ever writes it.

   A request is a root topic id, never a function and never an atom. Both would
   be unserializable if they reached an `e/server` form.

   The work key for a run lives here too, so that the client that opens an
   Activity row and the registry that declares its type compose it from one
   definition.")

(defonce ^{:doc "Root topic ids requested and not yet collected, oldest first.
                 Holding them, rather than firing, means a request made before
                 the driver mounts is not lost: `watch!` drains on subscribe."}
  !pending
  (atom []))

(defn request-subtree-sync!
  "Ask the mounted reconcile driver to sync the branch under `root-id`.

   pre : `root-id` is a topic id, the root of the branch (the topic itself and
         every descendant via `topics.parent_id`).
   post: returns :queued. The run starts when the driver collects it. If a run
         is already going, the driver answers with its \"Sync already in
         progress\" toast and drops the request, as it does for the chord.

   Plain fn, safe to call from a DOM event handler or a command. It touches no
   Electric state and makes no server call."
  [root-id]
  {:pre [(integer? root-id)]}
  (swap! !pending conj root-id)
  :queued)

(defn watch!
  "Collect requests. Calls `f` with each queued root id, oldest first, now for
   any already waiting and later for each new one.

   pre : `f` is a 1-arity fn. It runs once per request, synchronously.
   post: returns a 0-arity fn that stops watching. Call it on unmount.

   Idempotent per call site: the watch key is constant, so a second `watch!`
   replaces the first rather than stacking. Two live drivers would otherwise
   each start the run."
  [f]
  (let [drain! (fn []
                 (let [[old _] (swap-vals! !pending (constantly []))]
                   (run! f old)))]
    (add-watch !pending ::driver
      (fn [_ _ _ new-state] (when (seq new-state) (drain!))))
    (drain!)
    (fn [] (remove-watch !pending ::driver) nil)))

(defn work-key
  "The Activity monitor work key for a reconcile run.

   post: \"collection\" for the whole collection, \"branch-<id>\" for a run under
         topic `root-id`. Two branches can run in different tabs; a branch and
         the whole collection are different pieces of work too."
  ([] "collection")
  ([root-id] (if (some? root-id) (str "branch-" root-id) "collection")))
