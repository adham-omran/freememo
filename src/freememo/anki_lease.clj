(ns freememo.anki-lease
  "The one-at-a-time lease on a user's Anki sync (plans/anki-sync-lease-batches.md).

   A sync runs in ONE browser tab, because only that tab can reach AnkiConnect
   on localhost. Its STATE lives here, on the server, because every tab of the
   account must agree on it: the Sync buttons disable themselves from it, the
   Activity reporter reads progress from it, and Cancel reaches the driver
   through it from any tab.

   The lease is a per-user atom, `:anki-run`, holding at most one entry under
   `owner-key`:

     {:run-id       string  ; minted by `claim!`, carried by every later call
      :phase        string  ; what the row shows
      :done :total  n|nil   ; cards written, cards to write
      :heartbeat-at ms      ; server clock, advanced by every owner call
      :cancel?      bool}   ; set by the canceller, read by the driver

   A lease is LIVE while its heartbeat is younger than `ttl-ms`. A dead driver
   (a closed tab, a sleeping laptop, a killed server) stops heartbeating, so its
   lease expires by itself and `claim!` treats it as free. Nothing has to notice
   the death for the account to unblock.

   Every call after `claim!` names its `run-id`, and a mismatched id changes
   nothing. That is what keeps a driver whose lease expired — a laptop that
   slept past the ttl and woke after another tab claimed — from writing progress
   into, or releasing, the run that replaced it.

   Requires only `freememo.user-state`, so `freememo.activity-registry` can read
   it without pulling the activity writer into a cycle.

   One JVM only. During a blue/green flip two tabs on different colors could
   each hold a lease. Accepted: a flip is short, and a flip mid-sync is rare."
  (:require
   [freememo.user-state :as us]))

(def owner-key
  "The one key `:anki-run` uses. A sync spans the whole collection, so there is
   one per user — and it is also the activity's work_key."
  "collection")

(def ttl-ms
  "How long a lease outlives its last heartbeat.

   MUST equal `freememo.activity/stale-after-ms`, which this namespace cannot
   require (cycle through the registry). The two answer the same question —
   has the driver stopped proving it is alive — and if they differed the row
   and the lease would disagree about it. `anki_lease_test` asserts the
   equality."
  60000)

(defn- now-ms [] (System/currentTimeMillis))

(defn- lease-atom [user-id] (us/get-atom user-id :anki-run))

(defn live?
  "Whether `lease` still binds. Pure, for the registry reader and the tests.

   Pre : `lease` is an entry of `:anki-run`, or nil.
   Post: true iff a lease exists and its heartbeat is younger than `ttl-ms`."
  [lease now]
  (boolean (and lease (< (- now (:heartbeat-at lease)) ttl-ms))))

(defn current
  "The lease entry, live or not, or nil."
  [user-id]
  (get @(lease-atom user-id) owner-key))

(defn busy?
  "Whether a sync is running for `user-id` right now. What every Sync button
   reads, and what `claim!` refuses on."
  [user-id]
  (live? (current user-id) (now-ms)))

(defn reading
  "The Activity monitor's view of one lease entry.

   Post: nil when the user cancelled — the run still drains its last batch, and
         a reading here would let discovery open a second row for work the
         panel already closed as cancelled.
         `{:stale? true …}` when the driver stopped heartbeating — the reporter
         leaves such a row alone so the shared staleness rule marks it
         interrupted, rather than closing it as a success.
         Otherwise `{:phase :done :total}`."
  [lease now]
  (cond
    (nil? lease) nil
    (:cancel? lease) nil
    :else (cond-> (select-keys lease [:phase :done :total])
            (not (live? lease now)) (assoc :stale? true))))

(defn- update-owned!
  "Apply `f` to the lease iff `run-id` owns it.

   Post: the new lease entry, or nil when `run-id` does not own the lease (the
         atom is then unchanged)."
  [user-id run-id f]
  (let [a (lease-atom user-id)]
    (loop []
      (let [old @a
            lease (get old owner-key)]
        (when (and lease (= run-id (:run-id lease)))
          (let [lease' (f lease)]
            (if (compare-and-set! a old (assoc old owner-key lease'))
              lease'
              (recur))))))))

(defn request-cancel!
  "Mark `run-id` cancelled. The driver reads the mark at its next checkpoint
   and stops before its next batch; nothing here can stop a write already sent
   to Anki.

   Post: `:cancel?` is true on the lease iff `run-id` still owns it. Returns nil."
  [user-id run-id]
  (update-owned! user-id run-id #(assoc % :cancel? true))
  nil)

(defn claim!
  "Take the lease for a new run.

   Pre : none. A dead driver's expired lease is replaced, not waited for.
   Post: `{:run-id s}` with the lease published, the canceller installed and the
         previous run's error cleared — in that order, so the reporter's nudge
         sees every atom it needs; or `:busy` when a live lease exists, with
         nothing changed.
   Invariant: at most one live lease per user. The compare-and-set, not a
         prior read, is what arbitrates two tabs clicking at once."
  [user-id]
  (let [a (lease-atom user-id)
        run-id (str (random-uuid))]
    (loop []
      (let [old @a
            now (now-ms)]
        (if (live? (get old owner-key) now)
          :busy
          (let [lease {:run-id run-id :phase "scanning" :done nil :total nil
                       :heartbeat-at now :cancel? false}]
            (if (compare-and-set! a old (assoc old owner-key lease))
              (do
                (swap! (us/get-atom user-id :anki-run-cancellers) assoc owner-key
                  (fn [] (request-cancel! user-id run-id)))
                (swap! (us/get-atom user-id :anki-run-errors) dissoc owner-key)
                (us/work-started! user-id)
                {:run-id run-id})
              (recur))))))))

(defn heartbeat!
  "Prove `run-id`'s driver is alive.
   Post: :ok with `heartbeat-at` advanced, or :not-owner with nothing changed."
  [user-id run-id]
  (if (update-owned! user-id run-id #(assoc % :heartbeat-at (now-ms)))
    :ok
    :not-owner))

(defn report-progress!
  "Record progress, heartbeat, and tell the driver whether to go on.

   The driver calls this at every checkpoint — after scan, after plan, after
   each batch — so one round trip carries progress out and the cancel mark back.

   Pre : `progress` is `{:phase s :done n? :total n?}`; `:total` only with `:done`.
   Post: :continue — recorded, carry on;
         :cancel — recorded, the user cancelled, stop before the next batch;
         :not-owner — `run-id` lost the lease, nothing recorded, stop."
  [user-id run-id progress]
  (if-some [lease (update-owned! user-id run-id
                    #(merge % (select-keys progress [:phase :done :total])
                       {:heartbeat-at (now-ms)}))]
    (if (:cancel? lease) :cancel :continue)
    :not-owner))

(defn release!
  "End `run-id`'s lease.

   Post: when `run-id` owns the lease, `error` (when non-nil) is recorded for
         the reporter's `:error-fn`, then the canceller and the lease are
         removed — error first, so the reporter never sees the work gone with
         its outcome unwritten. Returns :released, or :not-owner with nothing
         changed."
  [user-id run-id error]
  (if (= run-id (:run-id (current user-id)))
    (do
      (when error
        (swap! (us/get-atom user-id :anki-run-errors) assoc owner-key error))
      (swap! (us/get-atom user-id :anki-run-cancellers) dissoc owner-key)
      (let [a (lease-atom user-id)]
        (loop []
          (let [old @a]
            (if (= run-id (:run-id (get old owner-key)))
              (if (compare-and-set! a old (dissoc old owner-key))
                :released
                (recur))
              :not-owner)))))
    :not-owner))
