(ns freememo.anki-lease
  "The one-at-a-time lease on a user's Anki sync (plans/anki-sync-lease-batches.md).

   A sync runs in ONE browser tab, because only that tab can reach AnkiConnect
   on localhost. Its STATE lives here, on the server, because every tab of the
   account must agree on it: the Sync buttons disable themselves from it, the
   Activity reporter reads progress from it, and Cancel reaches the driver
   through it from any tab.

   The lease is a per-user atom, `:anki-run`, holding at most one entry, keyed
   by the run's work key — `collection`, or `branch-<id>` for a run scoped
   to one branch (`freememo.anki-subtree-request`). One lease covers BOTH kinds:
   a branch run and a whole-collection run write the same Anki collection, so
   they exclude each other like any two runs.

     {:run-id       string  ; minted by `claim!`, carried by every later call
      :root-id      n|nil   ; the branch, or nil for the whole collection
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

   Requires only `freememo.user-state` and `freememo.anki-subtree-request`
   (which requires nothing), so `freememo.activity-registry` can read it without
   pulling the activity writer into a cycle.

   One JVM only. The lease cannot see a run on the other color. `deploy.sh`
   therefore waits before the Caddy flip while an `anki-reconcile` activity row
   is live, because the flip closes the driving tab's WebSocket. The wait has a
   cap (`DEPLOY_SYNC_WAIT_SECONDS`), and a run still live at the cap is cut.
   During the wait Caddy still routes every tab to the old color, so a run that
   starts then takes its lease there and extends the wait, up to the cap."
  (:require
   [freememo.anki-subtree-request :as subtree]
   [freememo.user-state :as us]))

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

(defn- entry
  "The one `[work-key lease]` pair in a lease map, or nil."
  [lease-map]
  (first lease-map))

(defn current
  "The lease entry, live or not, or nil."
  [user-id]
  (some-> (entry @(lease-atom user-id)) val))

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
            [k lease] (entry old)]
        (when (and lease (= run-id (:run-id lease)))
          (let [lease' (f lease)]
            (if (compare-and-set! a old (assoc old k lease'))
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
  "Take the lease for a new run over the whole collection, or, with a
   `root-id`, over that topic and its descendants.

   Pre : none. A dead driver's expired lease is replaced, not waited for.
         `activity-floor` is opaque here: the lease keeps it for `owned-run`.
   Post: `{:run-id s}` with the lease published, the canceller installed and the
         previous run's error and summary cleared — in that order, so the
         reporter's nudge sees every atom it needs; or `:busy` when a live lease exists, with
         nothing changed.
   Invariant: at most one live lease per user. The compare-and-set, not a
         prior read, is what arbitrates two tabs clicking at once."
  ([user-id] (claim! user-id nil nil))
  ([user-id root-id] (claim! user-id root-id nil))
  ([user-id root-id activity-floor]
   (let [a (lease-atom user-id)
         run-id (str (random-uuid))
         k (subtree/work-key root-id)]
     (loop []
       (let [old @a
             now (now-ms)]
         (if (live? (some-> (entry old) val) now)
           :busy
           (let [lease {:run-id run-id :root-id root-id :phase "scanning"
                        :done nil :total nil :heartbeat-at now :cancel? false
                        :activity-floor activity-floor}]
             ;; `{k lease}`, not `assoc`: an expired lease under the OTHER key
             ;; (a dead branch run, say) must not survive as a second entry.
             (if (compare-and-set! a old {k lease})
               (do
                 (reset! (us/get-atom user-id :anki-run-cancellers)
                   {k (fn [] (request-cancel! user-id run-id))})
                 (swap! (us/get-atom user-id :anki-run-errors) dissoc k)
                 (swap! (us/get-atom user-id :anki-run-results) dissoc k)
                 (us/work-started! user-id)
                 {:run-id run-id})
               (recur)))))))))

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

(defn owned-run
  "`{:work-key k :activity-floor f}` for `run-id`'s lease — `k` is \"collection\"
   or \"branch-<id>\", `f` what `claim!` was handed — or nil when `run-id` does
   not own the lease."
  [user-id run-id]
  (when-some [[k lease] (entry @(lease-atom user-id))]
    (when (= run-id (:run-id lease))
      {:work-key k :activity-floor (:activity-floor lease)})))

(defn release!
  "End `run-id`'s lease.

   Post: when `run-id` owns the lease, `error` (when non-nil) is recorded for
         the reporter's `:error-fn` and `summary` (when non-nil) for its
         `:summary-fn`, then the canceller and the lease are removed — outcome
         first, so the reporter never sees the work gone with its outcome
         unwritten. Returns :released, or :not-owner with nothing changed."
  ([user-id run-id error] (release! user-id run-id error nil))
  ([user-id run-id error summary]
   (if-some [[k lease] (entry @(lease-atom user-id))]
     (if (= run-id (:run-id lease))
       (do
         (when error
           (swap! (us/get-atom user-id :anki-run-errors) assoc k error))
         (when summary
           (swap! (us/get-atom user-id :anki-run-results) assoc k summary))
         (swap! (us/get-atom user-id :anki-run-cancellers) dissoc k)
         (let [a (lease-atom user-id)]
           (loop []
             (let [old @a]
               (if (= run-id (:run-id (get old k)))
                 (if (compare-and-set! a old (dissoc old k))
                   :released
                   (recur))
                 :not-owner)))))
       :not-owner)
     :not-owner)))
