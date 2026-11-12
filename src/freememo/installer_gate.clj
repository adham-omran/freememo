(ns freememo.installer-gate
  "The Runtime gate of a Local Install (ticket 20, plans/local-install.md,
   \"New: updates\").

   The Runtime is a number in the code (`freememo.desktop-version/runtime`). When
   the newest Release in the update feed needs a higher Runtime, an App Update
   cannot install it: the Local Install downloads no jar. It syncs every
   pending edit first, because installing a new installer can lose edits that
   were never sent. Then it offers \"Download the new installer\", with the link
   for this OS.

   `decide` is the pure rule. `state` gives what the web app and the tray show:

     nil                         nothing: no newer Release, or the same Runtime
     {:status :unsynced}         edits are not yet synced. No link.
     {:status :ready :version :url}   nothing is pending: show the link
     {:status :ready :version :url :pending n}
                                 edits are pending, but the sync state is
                                 `:update-required`: the server refuses this
                                 install, so no sync can send them. Show the
                                 link at once, and say that the edits stay here.

   With the sync state `:license-refused` (no Sync License) the server refuses
   every sync, so edits stay pending for good and only live on this computer.
   The link shows at once, as `{:status :ready :version :url}`. Before
   2026-11-12 this state waited for the sync, so the link never showed.

   The pending signal is the one the sync client already has: a row with
   `usn = -1` (`freememo.sync-client/pending-edit-count`)."
  (:require [freememo.desktop-version :as dv]
            [freememo.local-http :as lhttp]
            [freememo.update-feed :as feed]
            [taoensso.telemere :as tel])
  (:import [java.util.concurrent ScheduledExecutorService ScheduledFuture TimeUnit]))

(defn needed
  "Post: `{:version :url}` when `feed-map` holds a Release newer than `installed`
   that needs a higher Runtime than `runtime`, and has an installer for `os`;
   else nil. A Release with the same Runtime is an App Update, not this gate."
  [feed-map installed runtime os]
  (when (and (map? feed-map)
          (number? (get feed-map "runtime"))
          (> (get feed-map "runtime") runtime))
    (feed/available feed-map installed os)))

(defn decide
  "Pre:  `need` is the answer of `needed`. `pending` is the number of edits
         not yet synced. `sync-state` is `sync-client/sync-state` (default nil).
   Post: nil, `{:status :unsynced}` or `{:status :ready :version :url}`, with
         `:pending` when edits stay on this computer. A sync state that a
         later sync can end (offline, signed out, an error) changes nothing
         here: edits that stay pending keep the link hidden."
  ([need pending] (decide need pending nil))
  ([need pending sync-state]
   (cond
     (nil? need) nil
     (not (pos? pending)) (assoc need :status :ready)
     ;; The newest Release raised the Runtime and the Sync Protocol: sync gets
     ;; 409 for good, so waiting for it would hide the link for ever.
     (= :update-required sync-state) (assoc need :status :ready :pending pending)
     ;; No Sync License: no sync ever sends these edits. They are local only.
     (= :license-refused sync-state) (assoc need :status :ready)
     :else {:status :unsynced})))

;; ── Local Install side ─────────────────────────────────────────────

(defonce ^:private !need (atom nil))
(defonce ^:private !timer (atom nil))

(defn pending-count []
  ((requiring-resolve 'freememo.sync-client/pending-edit-count)))

(defn state
  "Post: the gate answer for now (see the namespace docstring). It counts pending
         edits on each call, so a sync that ends is seen at the next read.
         A failed count shows `{:status :unsynced}`: no link without proof."
  []
  (when-let [need @!need]
    (decide need (try (pending-count) (catch Throwable _ 1))
      ((requiring-resolve 'freememo.sync-client/sync-state)))))

(defn- stop-timer! []
  (when-let [^ScheduledFuture f (first (reset-vals! !timer nil))]
    (.cancel f false)))

(defonce ^:private !executor (delay (lhttp/daemon-scheduler "freememo-installer-gate")))

(defn on-feed!
  "Call with each feed answer (a map, or nil when it cannot be read).
   Pre:  `opts` has `:installed`, and may have `:on-change` (a fn of the new
         `state`, called when it changes) and `:sync!` (a fn, default the sync client's `sync-now!`).
   Post: with a Release that needs a higher Runtime, `state` is not nil and a sync
         of every pending edit runs, now and every minute until nothing is
         pending. Otherwise `state` is nil. A feed that cannot be read keeps the
         last answer. Never throws."
  [feed-map {:keys [installed on-change sync!]}]
  (try
    (when feed-map
      (let [need (needed feed-map installed dv/runtime (feed/os-key))
            sync! (or sync! (requiring-resolve 'freememo.sync-client/sync-now!))
            last-state (atom ::none)
            tick (fn []
                   (try
                     (when (pos? (pending-count)) (sync!))
                     (let [s (state)]
                       (when (not= s @last-state)
                         (reset! last-state s)
                         (when on-change (on-change s)))
                       (when (not= :unsynced (:status s)) (stop-timer!)))
                     (catch Throwable e
                       (tel/log! {:level :warn :id ::gate-tick} (str "installer gate: " (or (ex-message e) (str e)))))))]
        (stop-timer!)
        (reset! !need need)
        (if need
          (do (tel/log! {:level :info :id ::gate}
                (str "release " (:version need) " needs a newer Runtime: no app jar, sync first"))
              (reset! !timer (.scheduleWithFixedDelay ^ScheduledExecutorService @!executor ^Runnable tick 0 1 TimeUnit/MINUTES)))
          (when on-change (on-change nil)))))
    (catch Throwable e
      (tel/log! {:level :warn :id ::gate} (str "installer gate failed: " (or (ex-message e) (str e)))))))

(defn reset-state! [] (stop-timer!) (reset! !need nil))
