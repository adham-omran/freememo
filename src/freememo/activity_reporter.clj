(ns freememo.activity-reporter
  "The single timer that owns server-owned activity rows end to end — opening
   them, keeping them fresh, and closing them (plans/activity-monitor.md).

   ONE timer for every activity, not one per owner. Thirteen timers would be the
   same work at the wrong layer, and each would need its own idempotent start,
   its own executor and its own error isolation.

   DISCOVERY, not registration, and that is what keeps the owners untouched. The
   eight server-owned owners already publish their in-flight state as per-user
   atoms; this reads those atoms to learn what is running, so not one of them
   gains a `start!` or `terminate!` call to keep in step. An owner that begins
   publishing an atom gets an activity for free.

   The cost of that choice is stated where it bites: `registry/work-error` reads
   an outcome from the owners that record one, and the rest report failure only
   as a toast — so a failed run of theirs closes as 'success' in history.

   PULL, not push, and that is load-bearing. A keepalive that merely advanced
   `updated_at` for every live row would prove the SERVER is alive, not the WORK
   — so a processor that died would keep its row looking healthy forever and
   staleness could never fire. Asking the owner is what makes a quiet row
   evidence of a dead one.

   Browser-owned rows are not ticked here: no server atom describes an Anki
   reconcile or a chunked upload, so their driver pushes to `activity/report!`
   itself. `registry/server-tier-types` is the filter.

   The interval ordering this namespace sits inside:

     panel poll (~5s)  <=  tick-ms (20s)  <  activity/stale-after-ms (60s)

   A tick slower than `stale-after-ms` would mark healthy work interrupted; a
   tick much faster would write rows nobody is reading. 20s against 60s leaves
   room for two missed ticks before a row reads as stale, which absorbs a GC
   pause or a slow query without lying about the work."
  (:require
   [clojure.set :as set]
   [taoensso.telemere :as tel]
   [freememo.activity :as activity]
   [freememo.activity-registry :as registry]
   [freememo.user-state :as us])
  (:import [java.util.concurrent Executors ScheduledExecutorService TimeUnit]))

(defonce ^{:private true
           :doc "Activity rows whose owner state this process observed as live.
Pre: an id enters only after this process reads live owner state.
Post: a forgotten id can close only when it remains in this set.
Invariant: another process cannot add an id here. A false local observation is
an activity reporter bug."}
  locally-observed-activity-ids
  (atom #{}))

(def tick-ms
  "How often each in-flight server-owned row is refreshed. See the interval
   ordering in this namespace's docstring — this value is meaningful only
   relative to `activity/stale-after-ms`."
  20000)

(defn open-rows!
  "Open a row for every piece of server-owned work in flight for `user-id` that
   does not have one yet.

   Discovery, not registration. The owners publish their in-flight state as
   atoms already, so the reporter can read what is running rather than each of
   eight namespaces gaining a `start!` call it would have to keep in step. A type
   that starts publishing an atom gets an activity for nothing.

   Pre : `user-id` is a user.
   Post: every in-flight item of every server-tier type has a row. Returns the
         number opened and records each row as locally observed.
   Invariant: safe to call repeatedly. `activity/start!` is idempotent on
         (user, type, work_key), and the partial unique index — not a read here —
         is what arbitrates, so a concurrent pass cannot double-open.
   Invariant: one type's failure costs only that type. Isolation is PER TYPE, not
         per user: an owner publishing a key the schema rejects — a subject that
         is not a topic — would otherwise abort discovery for every other type
         that user has running."
  [user-id]
  (reduce
    (fn [opened type]
      (+ opened
        (try
          (count
            (for [{:keys [work-key subject-id]} (registry/live-work type user-id)
                  :let [state (and work-key (registry/read-state type user-id work-key))]
                  ;; An owner key can outlive its work — card generation leaves
                  ;; its entry behind with nothing pending. The reader is the
                  ;; authority on "in flight"; the atom's keys are only where to
                  ;; look.
                  :when (some? state)]
              (let [activity-id
                    (activity/start! user-id type work-key
                      (merge {:phase "starting" :subject-id subject-id} state))]
                (swap! locally-observed-activity-ids conj activity-id)
                activity-id)))
          (catch Throwable t
            (tel/error! {:id ::open-type-failed
                         :data {:user-id user-id :type type}}
              t)
            0))))
    0 (registry/server-tier-types)))

(defn tick-once!
  "One pass: open rows for new work, refresh locally-owned rows, and close local
   rows whose owner has finished.

   Pre : none — safe to call with no rows, an empty registry, or no scheduler
         running (the sweeps and tests call it directly).
   Post: every row with local live state has `updated_at` advanced. A forgotten
         row closes only when this process observed it live before.
         A foreign row remains untouched and becomes interrupted through staleness.
         Returns `{:opened n :refreshed n :closed n}`.
   Invariant: one process never assigns a terminal outcome to another process's
         work. Violating that boundary is a reporter implementation bug.
   Invariant: one owner's failure never costs another its tick. An adapter reads
         a live atom and can throw on a shape it did not expect; a pass that
         aborted would silently interrupt every row after the bad one, because
         staleness cannot tell a missed tick from dead work."
  []
  (let [;; Open before refreshing, so work that started since the last pass is
        ;; reported in this one rather than the next. The user set comes from
        ;; `user-state`, not from the rows: a user whose work has no row YET is
        ;; precisely the one discovery exists to catch.
        opened (reduce (fn [n uid]
                         (+ n (try (open-rows! uid)
                                (catch Throwable t
                                  (tel/error! {:id ::open-failed :data {:user-id uid}} t)
                                  0))))
                 0 (us/users-with-state))
        rows (activity/live-rows-for-tick (registry/server-tier-types))
        live-row-ids (into #{} (map :id) rows)]
    (swap! locally-observed-activity-ids set/intersection live-row-ids)
    (reduce
      (fn [acc {:keys [id user_id type work_key]}]
        (try
          (if-some [state (registry/read-state type user_id work_key)]
            (do (swap! locally-observed-activity-ids conj id)
                (activity/report! user_id id state)
                (update acc :refreshed inc))
            (if (contains? @locally-observed-activity-ids id)
              ;; This process saw the owner before it disappeared. The work is
              ;; finished, not stale, and this process can assign its outcome.
              (let [err (registry/work-error type user_id work_key)]
                (activity/terminate! user_id id (if err "failure" "success")
                  {:error err})
                (swap! locally-observed-activity-ids disj id)
                (update acc :closed inc))
              ;; Another process owns this row. Only its reporter can close it.
              ;; If that process died, the shared staleness rule interrupts it.
              acc))
          (catch Throwable t
            (tel/error! {:id ::tick-failed
                         :data {:activity-id id :user-id user_id
                                :type type :work-key work_key}}
              t)
            acc)))
      {:opened opened :refreshed 0 :closed 0}
      rows)))

;; An owner calling `us/work-started!` opens its row immediately, instead of up
;; to `tick-ms` later. Registered at namespace load, so the latency fix is live
;; as soon as anything requires this namespace — the boot path does.
;;
;; The timer stays the correctness mechanism: this only moves the moment of
;; discovery earlier. An owner that never calls it is late, never invisible.
(us/on-work-started! ::open-rows #'open-rows!)

(defonce ^:private scheduler (atom nil))

(defn start!
  "Start the reporter. Idempotent — once per process.

   Pre : the `activities` table exists (callers run after `db/setup-schema`).
   Post: `tick-once!` runs every `tick-ms` until `stop!`. Returns :started, or
         :already-running when a scheduler is already installed.
   Invariant: a throwing pass never kills the schedule.
         `scheduleAtFixedRate` cancels all future runs if its task throws, which
         would stop reporting for the life of the process and mark every activity
         interrupted 60s later — so the catch here is not defensive noise, it is
         what keeps the timer alive."
  []
  (if @scheduler
    :already-running
    (let [exec (Executors/newSingleThreadScheduledExecutor)]
      (.scheduleAtFixedRate ^ScheduledExecutorService exec
        (fn [] (try (tick-once!)
                 (catch Throwable t (tel/error! {:id ::tick-pass-failed} t))))
        tick-ms tick-ms TimeUnit/MILLISECONDS)
      (reset! scheduler exec)
      (tel/log! {:level :info :id ::reporter-started :data {:tick-ms tick-ms}}
        "Activity reporter started")
      :started)))

(defn stop!
  "Stop the reporter. Idempotent. Post: no further ticks; returns :stopped."
  []
  (when-some [^ScheduledExecutorService exec @scheduler]
    (.shutdownNow exec)
    (reset! scheduler nil)
    (tel/log! {:level :info :id ::reporter-stopped} "Activity reporter stopped"))
  :stopped)
