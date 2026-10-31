(ns freememo.anki-lease-client
  "The Electric side of the Anki sync lease (freememo.anki-lease): the server
   wrappers the driver calls, the heartbeat it runs, and `SyncBusy`, which every
   Sync entry point reads.

   Every server wrapper uses a body-level `#?(:clj … :cljs nil)`. They are named
   inside `(e/server (…))` in reactive bodies, and `e/server` does NOT strip the
   symbol from CLJS compilation — a whole-form `#?(:clj (defn …))` would leave
   the var missing on the CLJS peer (CLAUDE.md, server-only functions)."
  (:require
   [hyperfiddle.electric3 :as e]
   [missionary.core :as m]
   #?(:clj [freememo.activity :as activity])
   #?(:clj [freememo.anki-lease :as lease])
   #?(:clj [freememo.anki-subtree-request :as subtree])
   #?(:clj [freememo.user-state :as us])))

(def heartbeat-ms
  "How often a running driver proves it is alive. A third of
   `anki-lease/ttl-ms`, the same ratio the Activity reporter keeps against
   `activity/stale-after-ms`, so two missed beats still leave the lease live."
  20000)

(def busy-recheck-ms
  "How often `SyncBusy` re-reads the clock. A lease only EXPIRES by time
   passing — nothing writes the atom when a driver dies — so without a clock
   the Sync buttons would stay disabled until the next write."
  10000)

(defn ticks
  "Continuous flow of a tick counter: 0 at once, then +1 every `interval-ms`.

   The initial 0 is load-bearing. `e/input` throws `Uninitialized flow.` at mount
   for a flow with no value yet, and the first tick is a sleep away (CLAUDE.md,
   `e/input` needs a value at mount)."
  [interval-ms]
  (->> (m/ap (loop [n 1]
               (m/? (m/sleep interval-ms))
               (m/amb n (recur (inc n)))))
    (m/reductions (fn [_ n] n) 0)
    (m/relieve {})))

;; ---------------------------------------------------------------------------
;; Server wrappers
;; ---------------------------------------------------------------------------

(defn claim!*
  "`root-id` is the branch a branch run is scoped to, or nil for the whole
   collection.

   The newest ended Activity row is read BEFORE the claim, because the claim
   nudges the reporter, which may open this run's row at once. `release!*`
   hands it back, so a cancelled run's summary lands on this run's row only."
  [user-id root-id]
  #?(:clj (lease/claim! user-id root-id
            (activity/newest-ended-row-id user-id :anki-reconcile
              (subtree/work-key root-id)))
     :cljs nil))

(defn heartbeat!*
  "`_tick` exists only to make each beat a new call; the server's own clock
   stamps the lease."
  [_tick user-id run-id]
  #?(:clj (lease/heartbeat! user-id run-id) :cljs nil))

(defn report-progress!* [user-id run-id progress]
  #?(:clj (lease/report-progress! user-id run-id progress) :cljs nil))

(defn release!*
  "End the run: record its outcome for the reporter, drop the lease, and put
   `summary` on a row that already ended.

   A Cancel terminates the row before the run drains its last batch, so the
   reporter's close, which carries the summary for every other ending, finds
   nothing to close. `record-summary!` is the path for that row; on a row still
   in flight it changes nothing, and the reporter writes the same text.

   The call lives here, not in `anki-lease`: `activity` reaches `anki-lease`
   through the registry, so the lease requiring `activity` would be a cycle.

   Pre : `summary` is nil or non-blank.
   Post: as `lease/release!`; when it answers :released and `summary` is
         non-nil, `activity/record-summary!` ran for this run's own row."
  [user-id run-id error summary]
  #?(:clj (let [{:keys [work-key activity-floor]} (lease/owned-run user-id run-id)
                released (lease/release! user-id run-id error summary)]
            (when (and (= :released released) work-key activity-floor summary)
              (activity/record-summary! user-id :anki-reconcile work-key
                activity-floor summary))
            released)
     :cljs nil))

(defn busy?*
  "`_lease-map` and `_tick` are reactive dependencies only: the first re-runs
   this on every claim, beat and release, the second as time passes."
  [_lease-map _tick user-id]
  #?(:clj (lease/busy? user-id) :cljs false))

;; ---------------------------------------------------------------------------
;; Components
;; ---------------------------------------------------------------------------

(e/defn SyncBusy
  "Whether any tab of this account is running an Anki sync. Every Sync entry
   point disables itself on it; `claim!` still decides, so two clicks racing
   past a stale `false` cannot both start a run."
  [user-id]
  (e/server
    (busy?* (e/watch (us/get-atom user-id :anki-run))
      (e/input (ticks busy-recheck-ms))
      user-id)))

(e/defn Heartbeat
  "Beat the lease every `heartbeat-ms` while mounted.

   The tick comes from the CLIENT on purpose. A server-side clock would prove
   only that the server frame is mounted, and a server frame can outlive its
   browser — teardown stalls while an `e/Offload` drains (CLAUDE.md). A beat
   that has to cross from the browser proves the browser is still there."
  [user-id run-id]
  (e/client
    (let [tick (e/input (ticks heartbeat-ms))]
      (e/server (heartbeat!* tick user-id run-id)))))
