(ns freememo.activity-client
  "Client-side half of the Activity monitor: the server-call wrappers, the
   registry of pushed-tier drivers THIS TAB owns, and the leave warning
   (plans/activity-monitor.md).

   Two things live here that cannot live on the server.

   A pushed-tier driver — a chunked upload, an Anki reconcile — runs in one
   browser tab, and only that tab can stop it. `!tab-drivers` is how the panel
   finds a canceller for work started here. It is per PAGE LOAD, like
   `freememo.client-state`: a driver cannot outlive the document that mounted
   it, so a registry that survived a reload would hand out cancellers for work
   that no longer exists.

   The leave warning reads that same registry rather than the server. Only
   pushed-tier work dies with the tab; server-owned work carries on regardless,
   so warning about it would train users to dismiss the dialog. Asking the tab
   what IT is driving is both cheaper than a round trip and correct by
   construction.

   Every server wrapper uses a body-level `#?(:clj … :cljs nil)`. These are named
   inside `(e/server (…))` in reactive bodies, and `e/server` does NOT strip the
   symbol from CLJS compilation — a whole-form `#?(:clj (defn …))` would leave
   the var missing on the CLJS peer and the dual compile would emit mismatched
   frame-signal counts."
  (:require
   [hyperfiddle.electric3 :as e]
   #?(:clj [freememo.activity :as activity])
   #?(:clj [freememo.activity-registry :as registry])))

;; ---------------------------------------------------------------------------
;; Server wrappers
;; ---------------------------------------------------------------------------

(defn user-activities* [_refresh user-id]
  #?(:clj (activity/user-activities user-id)
     :cljs []))

(defn live-count* [_refresh user-id]
  #?(:clj (activity/live-count user-id)
     :cljs 0))

(defn cancel!* [user-id activity-id]
  #?(:clj (activity/cancel! user-id activity-id)
     :cljs nil))

(defn start!* [user-id type work-key opts]
  #?(:clj (activity/start! user-id type work-key opts)
     :cljs nil))

(defn report!* [user-id activity-id progress]
  #?(:clj (activity/report! user-id activity-id progress)
     :cljs nil))

(defn terminate!* [user-id activity-id outcome opts]
  #?(:clj (activity/terminate! user-id activity-id outcome opts)
     :cljs nil))

(defn work-key*
  "Compose a work_key through the registry, so the composing side and the side
   that parses it cannot drift. Variadic because a page scan takes (doc page)
   and everything else takes one id or none."
  [type & args]
  #?(:clj (apply (:work-key (registry/activity-type type)) args)
     :cljs nil))

;; ---------------------------------------------------------------------------
;; Drivers this tab owns
;; ---------------------------------------------------------------------------

(defonce ^{:doc "activity-id -> {:cancel 0-arity fn, :unrecoverable? bool}.
                 Per page load; see the namespace docstring.

                 No reader conditional, deliberately. The panel `e/watch`es this
                 in a reactive body, so the var must resolve on BOTH peers or the
                 dual compile emits mismatched frame-signal counts and the socket
                 dies at mount with nothing naming this file. The server's copy is
                 inert — nothing ever writes it there. Same shape and same reason
                 as `navigation/!pending-page-jump`."}
  !tab-drivers
  (atom {}))

(defn register-driver!
  "Record that this tab drives `activity-id`.

   Pre : `cancel` is a 0-arity fn that stops the work, or nil when the driver
         cannot be stopped mid-flight.
   Post: the panel offers Cancel for this activity in THIS tab, and the leave
         warning arms while `unrecoverable?` drivers are registered.
   Invariant: every `register-driver!` owes an `unregister-driver!` on every
         exit path, or the warning arms forever. Drivers register on start and
         unregister in the same place they terminate the row."
  [activity-id {:keys [cancel unrecoverable?]}]
  (swap! !tab-drivers assoc activity-id
    {:cancel cancel :unrecoverable? (boolean unrecoverable?)})
  nil)

(defn unregister-driver! [activity-id]
  (swap! !tab-drivers dissoc activity-id)
  nil)

(defn tab-owns?
  "Whether this tab holds a canceller for `activity-id`. False for work driven by
   another tab, which this one genuinely cannot stop.

   Takes the DEREFERENCED map so the panel can watch `!tab-drivers` once for the
   whole list rather than deref per row."
  [drivers activity-id]
  (some? (:cancel (get drivers activity-id))))

(defn tab-cancel!
  "Stop work this tab drives. No-op when it does not.

   Does NOT terminate the row: the driver notices its own cancellation and
   writes the outcome, the same way it writes success. A cancel that terminated
   the row here would race the driver's own terminal write."
  [activity-id]
  (when-some [f (:cancel (get @!tab-drivers activity-id))]
    (f)
    :cancelled))

;; ---------------------------------------------------------------------------
;; Leave warning
;; ---------------------------------------------------------------------------

(defn- tab-has-unrecoverable? []
  (boolean (some :unrecoverable? (vals @!tab-drivers))))

#?(:cljs
   (defonce ^{:doc "Installed once per page load. `beforeunload` covers refresh
                    and tab close only — in-app router navigation fires no
                    unload event, and needs none: the drivers are mounted above
                    the router, so leaving a route does not stop them.

                    Browsers ignore any custom message, so none is supplied;
                    `preventDefault` is the whole API that still works."}
     unload-warning
     (do (.addEventListener js/window "beforeunload"
           (fn [e]
             (when (tab-has-unrecoverable?)
               (.preventDefault e)
               ;; Legacy browsers require a non-empty returnValue to prompt.
               (set! (.-returnValue e) ""))))
         :installed)))

(defn install-unload-warning!
  "Force the `beforeunload` listener to install. Idempotent — the work is a
   `defonce`, so this only exists to give a mounted component something to call.

   Plain defn, never the reactive body: Electric re-evaluates bodies
   unpredictably, and an addEventListener there would stack duplicates."
  []
  #?(:cljs (do unload-warning nil) :clj nil))

;; ---------------------------------------------------------------------------
;; The pushed-tier bridge
;; ---------------------------------------------------------------------------

(e/defn ActivityMirror
  "Mirror one pushed-tier driver's state into an `activities` row.

   ONE reactive shape for every pushed driver, rather than a hand-written
   start/report/terminate at each site. The drivers already publish their state
   as client atoms — an upload has `!progress`, a reconcile has `:!phase` — so a
   caller watches its own atoms and hands the plain values here. That is the same
   arrangement the server tier has, one peer over.

   `running?` is the whole state machine. False -> true opens the row; changes
   while true report; true -> false terminates it with `outcome`.

   `cancel` is a 0-arity fn or nil. Non-nil registers this tab as the driver, so
   the panel can offer Cancel here and the leave warning knows what is at stake.
   Cancelling does NOT terminate the row: the driver notices and reports its own
   outcome, which is why a cancelled upload ends as 'cancelled' rather than
   racing two terminal writes.

   Pre : mounted inside `e/client`; `type` is a `:pushed`-tier registry type;
         `work-key` was composed through the registry (violation = caller bug).
   Post: at most one row per (user, type, work-key) is in flight, and it
         terminates exactly once.
   Invariant: every state change is sequenced through a token, and the local
         `!id` reset lives in `e/on-unmount` inside the frame the token spend
         unmounts — a bare reset! beside the server call would tear down the
         in-flight effect that produced the id."
  [user-id type work-key running? phase done total cancel unrecoverable? subject-id outcome error]
  (e/client
    (let [!id (atom nil)
          id (e/watch !id)]

      ;; Open. Keyed on the work itself, so a second piece of work under the same
      ;; mirror gets its own row rather than reusing the last one's.
      (when (and running? (nil? id))
        (let [[t _] (e/Token [type work-key])]
          (when t
            (let [new-id (e/server (start!* user-id type work-key
                                     {:phase (or phase "starting")
                                      :subject-id subject-id
                                      :done done :total total
                                      :cancellable (some? cancel)
                                      :unrecoverable unrecoverable?}))]
              (case new-id
                (do (e/on-unmount
                      (fn []
                        (register-driver! new-id {:cancel cancel
                                                  :unrecoverable? unrecoverable?})
                        (reset! !id new-id)))
                    (t)))))))

      ;; Report. The token input is the reading itself, so an unchanged reading
      ;; costs nothing and a changed one writes exactly once.
      (when (and running? (some? id))
        (let [[t _] (e/Token [phase done total])]
          (when t
            (case (e/server (report!* user-id id {:phase (or phase "running")
                                                  :done done :total total
                                                  :cancellable (some? cancel)
                                                  :unrecoverable unrecoverable?}))
              (t)))))

      ;; Close. Unregisters the driver first so the leave warning disarms even if
      ;; the terminal write is the thing that fails.
      (when (and (not running?) (some? id))
        (let [[t _] (e/Token id)]
          (when t
            (case (e/server (terminate!* user-id id (or outcome "success")
                              {:error error :done done :total total}))
              (do (e/on-unmount
                    (fn [] (unregister-driver! id) (reset! !id nil)))
                  (t)))))))))
