(ns freememo.anki-reconcile-panels
  "The reconcile run as an Electric phase machine.

   One run, in batches (plans/anki-sync-lease-batches.md), over the whole
   collection or over one branch (`anki-subtree-request`):

     :claiming → :scanning → :planning → :batch ×N → :finishing → :done
         │                       │            │
         └─ busy                 └────────────┴──→ :error | :cancelled

   `:claiming` takes the account's lease (freememo.anki-lease). Every later
   server call names the run's id, so only the tab that claimed can report
   progress, and every tab reads the lease to disable its Sync buttons.

   `:batch` loops: build one batch's bundles, write them, read them back, record
   them, report progress. Recording per batch is what makes a stopped run keep
   the batches it finished — and the report is where the run learns that the
   user cancelled, so it stops at a batch boundary with nothing unrecorded.

   Split into small `e/defn`s for the JVM 64KB method cap (CLAUDE.md), the same
   way `quick-sync` is. The groups are reactively independent, so mount order
   does not matter.

   Phase transitions out of a token frame go in `e/on-unmount` inside the frame
   `(t)` unmounts (CLAUDE.md, token lifecycle). Nothing here talks to AnkiConnect
   or to Missionary: each phase calls a plain driver in `anki-reconcile-client`."
  (:require
   [hyperfiddle.electric3 :as e]
   [freememo.activity-client :as ac]
   [freememo.anki-lease-client :as lc]
   [freememo.anki-reconcile-client :as rc]
   [freememo.anki-reconcile :as rec]
   [freememo.logging :as log]
   #?(:clj [freememo.anki-reconcile-server :as recon])
   #?(:clj [freememo.anki-sync-server :as sync])
   #?(:clj [freememo.toasts :as toasts])))

;; --- server wrappers: keep #?(:clj …) out of every reactive body -------------

(defn scan-targets* [user-id root-id]
  #?(:clj (recon/scan-targets user-id root-id) :cljs nil))

(defn plan* [user-id digests absent root-id]
  #?(:clj (recon/plan user-id digests absent root-id) :cljs nil))

(defn write-bundles* [user-id card-ids]
  #?(:clj (sync/anki-write-bundles user-id card-ids) :cljs nil))

(defn record-batch!* [user-id run-id root-id entries local-digests progress]
  #?(:clj (recon/record-batch! user-id run-id root-id entries local-digests progress)
     :cljs nil))

(defn finish-run!* [user-id run-id outcome]
  #?(:clj (recon/finish-run! user-id run-id outcome) :cljs nil))

(defn toast!* [user-id level message]
  #?(:clj (do (toasts/push! user-id {:level level :message message}) nil)
     :cljs nil))

;; --- plain transitions --------------------------------------------------------

(defn- halt!
  "Stop the run on a checkpoint verdict other than :continue.
   Post: phase `:cancelled` for :cancel, else `:error` with a message."
  [recon verdict]
  (if (= :cancel verdict)
    (reset! (:!phase recon) :cancelled)
    (rc/fail! recon "Another tab took over this sync after it stopped responding."))
  nil)

(defn- tally-batch!
  "Add one recorded batch to the run's totals. `:written` counts cards written
   AND read back, so a create Anki refused is not counted as sent."
  [recon out]
  (swap! (:!tally recon)
    (fn [t] (-> t
              (update :written + (count (:readback out)))
              (update :errors into (:errors out)))))
  nil)

(defn- after-batch!
  "Move on from a recorded batch: the next batch, finishing, or a halt."
  [recon verdict last?]
  (reset! (:!batch-out recon) nil)
  (cond
    (not= :continue verdict) (halt! recon verdict)
    last? (reset! (:!phase recon) :finishing)
    :else (swap! (:!batch-idx recon) inc))
  nil)

(defn- reset-run! [recon]
  (reset! (:!run-id recon) nil)
  (reset! (:!phase recon) nil)
  nil)

(defn outcome-message
  "The run's closing toast as `[level text]`.

   The text reports what HAPPENED, never what the plan intended. A run that
   planned 46 creates and wrote none must not say 46 — that was the shape of the
   first defect here, where `anki-write-bundles` (then filtering to already-
   pushed cards) silently dropped every create while the toast read them off the
   plan and claimed success.

   Anki-side write errors are surfaced for the same reason. `do-anki-push!`
   returns them per card, and a run in which every `addNote` failed must not
   look identical to one that worked."
  [phase {:keys [result plan tally error branch?]}]
  (let [written (:written tally 0)
        errors (count (:errors tally))
        refused (when (pos? errors)
                  (str " — " errors (if (= 1 errors) " card" " cards") " Anki refused"))]
    (case phase
      :error [:error (str "Sync failed: " error)]
      :cancelled [:info (str "Sync cancelled. " written " of "
                          (count (:write-card-ids plan)) " cards sent to Anki."
                          refused)]
      (if (nil? result)
        [:success (str "Dry run — nothing applied. " (:report plan))]
        (let [conflicts (get-in result [:counts :conflicts] 0)]
          [(if (or (not (:success result)) (pos? errors)) :warning :success)
           (str (when branch? "This branch: ")
             (rec/outcome-text (assoc (:counts result) :writes written))
             (when (pos? conflicts)
               (str " — " conflicts
                 (if (= 1 conflicts) " card needs" " cards need")
                 " a decision in Library"))
             refused)])))))

;; --- phases -----------------------------------------------------------------

(e/defn ReconcileClaim
  "Phase 0 — take the account's lease, or report that another tab holds it.

   `root-id` is deref'd, not watched, here and in every phase below: `start!`
   sets it once before the phase moves, and the value is a plain number (or nil)
   by the time it is closed over, so no server thunk holds an atom."
  [user-id recon]
  (e/client
    (when (= :claiming (e/watch (:!phase recon)))
      (let [[t _] (e/Token :reconcile-claim)]
        (when t
          (let [root-id @(:!root-id recon)
                claim (e/server (lc/claim!* user-id root-id))]
            (if (= :busy claim)
              (case (e/server (toast!* user-id :info "Sync already in progress"))
                (do (e/on-unmount #(reset! (:!phase recon) nil))
                    (t)))
              (case (e/server (toast!* user-id :info "Syncing with Anki…"))
                (do (e/on-unmount (fn []
                                    (reset! (:!run-id recon) (:run-id claim))
                                    (reset! (:!phase recon) :scanning)))
                    (t))))))))))

(e/defn ReconcileScan
  "Phase 1 — ask the server what to look at, then read all of it from Anki."
  [user-id recon]
  (e/client
    (when (= :scanning (e/watch (:!phase recon)))
      (let [[t _] (e/Token :reconcile-scan)]
        (when t
          (let [root-id @(:!root-id recon)
                targets (e/server (e/Offload #(scan-targets* user-id root-id)))]
            (case (rc/run-scan! targets recon) (t))))))))

(e/defn ReconcilePlan
  "Phase 2 — check in, classify server-side from the scan's digests, check in
   again with the run's total.

   Both check-ins can stop the run: the first catches a cancel during the scan,
   the second a cancel during planning, before anything is written.

   An aborted plan ends the run here. `bulk-absence?` reads a majority of absent
   notes as evidence that the reply describes a different Anki profile, and a
   partial run would still delete whatever it reached before noticing."
  [user-id recon]
  (e/client
    (when (= :planning (e/watch (:!phase recon)))
      (let [scan (e/watch (:!scan recon))
            run-id (e/watch (:!run-id recon))
            root-id @(:!root-id recon)
            [t _] (e/Token :reconcile-plan)]
        (when t
          (let [verdict (e/server (lc/report-progress!* user-id run-id {:phase "planning"}))]
            (if (not= :continue verdict)
              (do (e/on-unmount #(halt! recon verdict))
                  (t))
              (let [p (e/server (e/Offload #(plan* user-id (:digests scan) (:absent scan) root-id)))]
                (if (:aborted? p)
                  (case (e/server
                          (toast!* user-id :error
                            (str "Sync stopped: Anki reported most of your notes missing. "
                              "Check that the right profile is open.")))
                    (do (e/on-unmount #(rc/fail! recon "aborted — most notes absent"))
                        (t)))
                  ;; A dry run stops here: the plan IS the answer, and every phase
                  ;; after this one writes. Deref rather than `e/watch`: the flag
                  ;; is set once by `start!` before the phase moves.
                  (if @(:!dry-run? recon)
                    (case (reset! (:!plan recon) p)
                      (do (e/on-unmount #(reset! (:!phase recon) :done))
                          (t)))
                    (let [total (count (:write-card-ids p))
                          verdict (e/server
                                    (lc/report-progress!* user-id run-id
                                      (if (pos? total)
                                        {:phase "writing" :done 0 :total total}
                                        {:phase "finishing"})))]
                      ;; The `if` on `verdict` is what makes `(t)` wait for the
                      ;; server. Read only inside the unmount closure, `verdict`
                      ;; gated nothing: `(t)` spent the token at once, the frame
                      ;; unmounted before the closure could be built, and the
                      ;; phase stayed `:planning` for good (CLAUDE.md, token
                      ;; lifecycle).
                      (case (reset! (:!plan recon) p)
                        (if (not= :continue verdict)
                          (do (e/on-unmount #(halt! recon verdict))
                              (t))
                          (do (e/on-unmount
                                #(reset! (:!phase recon) (if (pos? total) :batch :finishing)))
                              (t)))))))))))))))

(e/defn ReconcileBatch
  "Phase 3, once per batch — build, write, read back, record, report.

   The bundles and their local digests are server values held in THIS frame, so
   the record call reads the digests where they already live; only the bundles
   and the read-back cross the wire. The token is keyed on the batch index, so
   advancing the index is what mounts the next batch.

   A failed record ends the run. Its notes are in Anki without note ids, and a
   next batch would only add more of them."
  [user-id recon]
  (e/client
    (when (= :batch (e/watch (:!phase recon)))
      (let [p (e/watch (:!plan recon))
            i (e/watch (:!batch-idx recon))
            run-id (e/watch (:!run-id recon))
            root-id @(:!root-id recon)
            [t _] (e/Token [:reconcile-batch i])]
        (when t
          (let [ids (rec/batch-at (:write-card-ids p) i)
                bundles (e/server (e/Offload #(write-bundles* user-id ids)))]
            (if-not (:success bundles)
              (do (e/on-unmount #(rc/fail! recon (:error bundles)))
                  (t))
              (case (rc/run-batch! (:bundles bundles) (:kind-by-card p) recon)
                (let [out (e/watch (:!batch-out recon))]
                  (when out
                    (let [progress (rec/batch-progress (:write-card-ids p) i)
                          readback (:readback out)
                          r (e/server
                              (e/Offload #(record-batch!* user-id run-id root-id readback
                                            (:local-digests bundles) progress)))
                          last? (= (inc i) (rec/batch-count (:write-card-ids p)))]
                      (if-not (:success (:recorded r))
                        (do (e/on-unmount
                              #(rc/fail! recon (str "Could not record a batch: "
                                                 (:error (:recorded r)))))
                            (t))
                        (case (tally-batch! recon out)
                          (do (e/on-unmount #(after-batch! recon (:verdict r) last?))
                              (t)))))))))))))))

(e/defn ReconcileFinish
  "Phase 4 — delete tombstoned notes, then one server call applies pulls,
   backfill, conflicts and tombstones.

   `:pull-notes` is the only place a run sends note bodies back, and only for the
   cards it decided to pull."
  [user-id recon]
  (e/client
    (when (= :finishing (e/watch (:!phase recon)))
      (let [p (e/watch (:!plan recon))
            scan (e/watch (:!scan recon))
            run-id (e/watch (:!run-id recon))
            root-id @(:!root-id recon)
            [t _] (e/Token :reconcile-finish)]
        (when t
          (let [verdict (e/server (lc/report-progress!* user-id run-id {:phase "finishing"}))]
            (if (not= :continue verdict)
              (do (e/on-unmount #(halt! recon verdict))
                  (t))
              (case (rc/run-deletes! (:note-ids-to-delete p) (:note-ids-to-drop p) recon)
                (let [finished (e/watch (:!finished-note-ids recon))]
                  (when finished
                    (let [outcome {:pull-card-ids (:pull-card-ids p)
                                   :pull-notes (rc/notes-for (:notes-by-id scan) (:pull-note-ids p))
                                   :digests (:digests scan)
                                   :backfill (:backfill p)
                                   :conflict-card-ids (:conflict-card-ids p)
                                   :finished-note-ids finished
                                   :root-id root-id}
                          result (e/server (e/Offload #(finish-run!* user-id run-id outcome)))]
                      (if (:verdict result)
                        (do (e/on-unmount #(halt! recon (:verdict result)))
                            (t))
                        (case (reset! (:!result recon) result)
                          (do (e/on-unmount #(reset! (:!phase recon) :done))
                              (t)))))))))))))))

(e/defn ReconcileOutcome
  "Reports the run, once, then releases the lease.

   Release comes after the toast and before the local reset, so no tab can see
   the account free while this one still thinks it is running. The error goes
   with it, so the Activity row closes as failed."
  [user-id recon]
  (e/client
    (let [phase (e/watch (:!phase recon))]
      (when (contains? #{:done :error :cancelled} phase)
        (let [run-id (e/watch (:!run-id recon))
              err (e/watch (:!error recon))
              [level msg] (outcome-message phase
                            {:result (e/watch (:!result recon))
                             :plan (e/watch (:!plan recon))
                             :tally (e/watch (:!tally recon))
                             :error err
                             ;; Watched: a deref would read the value from
                             ;; whenever this branch first mounted.
                             :branch? (some? (e/watch (:!root-id recon)))})
              [t _] (e/Token [:reconcile-end phase])]
          (when t
            (log/log-info (str "[reconcile] " (name phase) ": " msg))
            (case (e/server (toast!* user-id level msg))
              (case (e/server (lc/release!* user-id run-id (when (= :error phase) err)))
                (do (e/on-unmount #(reset-run! recon))
                    (t))))))))))

(e/defn LeaveGuard
  "Arm the browser's leave warning while notes are being written: leaving then
   strands at most one batch of notes in Anki without their note ids. Uses the
   Activity monitor's per-tab driver registry, under a key no activity id can
   take, with no canceller — Cancel comes from the lease, in every tab."
  []
  (e/client
    (e/on-unmount #(ac/unregister-driver! "anki-sync"))
    (ac/register-driver! "anki-sync" {:cancel nil :unrecoverable? true})))

(e/defn ReconcileExecutor
  "Mounts every phase. Headless — the caller owns the trigger and the DOM.

   Unmounting stops the in-flight AnkiConnect task. The lease is NOT released
   here: a client teardown cannot be trusted to reach the server, so a dead
   driver's lease expires by itself and its Activity row reads interrupted."
  [user-id recon]
  (e/client
    (let [phase (e/watch (:!phase recon))
          run-id (e/watch (:!run-id recon))]
      (e/on-unmount #(rc/cancel-task! recon))
      (ReconcileClaim user-id recon)
      (ReconcileScan user-id recon)
      (ReconcilePlan user-id recon)
      (ReconcileBatch user-id recon)
      (ReconcileFinish user-id recon)
      (ReconcileOutcome user-id recon)
      (when (contains? #{:batch :finishing} phase)
        (LeaveGuard))
      (when (some? run-id)
        (lc/Heartbeat user-id run-id)))))

(defn fresh-state
  "The atom map one run needs. Created per host component, never shared: these
   feed `e/Token` inputs, and a map rebuilt by a parent re-render would re-fire
   every token in it (CLAUDE.md, state maps)."
  []
  {:!phase (atom nil)
   :!run-id (atom nil)
   :!scan (atom nil)
   :!plan (atom nil)
   :!batch-idx (atom 0)
   :!batch-out (atom nil)
   :!tally (atom {:written 0 :errors []})
   :!finished-note-ids (atom nil)
   :!result (atom nil)
   :!error (atom nil)
   :!dry-run? (atom false)
   ;; The topic a branch run is scoped to, or nil for the whole collection.
   ;; Set by `start!` before the phase moves.
   :!root-id (atom nil)
   :!task-cancel (atom nil)})

(defn start!
  "Begin a run in this tab, or report that this tab is already running one.

   pre : `recon` is a `fresh-state` map. `dry-run?` true stops after planning.
   post: phase `:claiming`, or unchanged and `:busy`. Whether ANOTHER tab holds
         the lease is decided by the server in `:claiming`, never here.

   The 3-arity takes `root-id`, a topic id: the run then covers that topic and
   its descendants only, and settles no tombstones (see
   `anki-reconcile-server/scan-targets`). The shorter arities leave it nil, the
   whole collection. `:!root-id` is rewritten on every start, so a branch run
   never leaks its scope into the next full one.

   A dry run reads Anki and classifies, then reports and stops. It exists
   because the numbers a first run should produce are knowable in advance, so
   the plan can be checked before anything is written."
  ([recon] (start! recon false nil))
  ([recon dry-run?] (start! recon dry-run? nil))
  ([recon dry-run? root-id]
   (if (some? @(:!phase recon))
     :busy
     (do (reset! (:!root-id recon) root-id)
         (reset! (:!dry-run? recon) (boolean dry-run?))
         (reset! (:!run-id recon) nil)
         (reset! (:!error recon) nil)
         (reset! (:!result recon) nil)
         (reset! (:!plan recon) nil)
         (reset! (:!scan recon) nil)
         (reset! (:!batch-idx recon) 0)
         (reset! (:!batch-out recon) nil)
         (reset! (:!tally recon) {:written 0 :errors []})
         (reset! (:!finished-note-ids recon) nil)
         (reset! (:!phase recon) :claiming)
         :started))))
