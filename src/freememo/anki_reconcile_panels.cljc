(ns freememo.anki-reconcile-panels
  "The reconcile run as an Electric phase machine.

   One run, six phases, each a token-gated transition:

     :scanning → :planning → :writing → :recording → :done
                     └──────────────────────────────→ :error

   Split into small `e/defn`s for the JVM 64KB method cap (CLAUDE.md), the same
   way `quick-sync` is. The groups are reactively independent, so mount order
   does not matter.

   Nothing here talks to AnkiConnect or to Missionary. Each phase calls a plain
   driver in `anki-reconcile-client` through `case`, which is what forces the
   effect to run — a bare statement in a `do` body gets work-skipped."
  (:require
   [hyperfiddle.electric3 :as e]
   [freememo.anki-reconcile-client :as rc]
   [freememo.anki-reconcile :as rec]
   [freememo.logging :as log]
   #?(:clj [freememo.anki-reconcile-server :as recon])
   #?(:clj [freememo.anki-sync-server :as sync])
   #?(:clj [freememo.toasts :as toasts])))

;; --- server wrappers: keep #?(:clj …) out of every reactive body -------------

(defn scan-targets* [user-id]
  #?(:clj (recon/scan-targets user-id) :cljs nil))

(defn plan* [user-id digests absent]
  #?(:clj (recon/plan user-id digests absent) :cljs nil))

(defn write-bundles* [user-id card-ids]
  #?(:clj (sync/anki-write-bundles user-id card-ids) :cljs nil))

(defn finish-run!* [user-id outcome]
  #?(:clj (recon/finish-run! user-id outcome) :cljs nil))

(defn toast!* [user-id level message]
  #?(:clj (do (toasts/push! user-id {:level level :message message}) nil)
     :cljs nil))

;; --- phases -----------------------------------------------------------------

(e/defn ReconcileScan
  "Phase 1 — ask the server what to look at, then read all of it from Anki."
  [user-id recon]
  (e/client
    (when (= :scanning (e/watch (:!phase recon)))
      (let [[t _] (e/Token :reconcile-scan)]
        (when t
          (let [targets (e/server (e/Offload #(scan-targets* user-id)))]
            (case (rc/run-scan! targets recon) (t))))))))

(e/defn ReconcilePlan
  "Phase 2 — classify server-side from the digests the scan produced.

   An aborted plan ends the run here. `bulk-absence?` reads a majority of absent
   notes as evidence that the reply describes a different Anki profile, and a
   partial run would still delete whatever it reached before noticing."
  [user-id recon]
  (e/client
    (when (= :planning (e/watch (:!phase recon)))
      (let [scan (e/watch (:!scan recon))
            [t _] (e/Token :reconcile-plan)]
        (when t
          (let [p (e/server (e/Offload #(plan* user-id (:digests scan) (:absent scan))))]
            (if (:aborted? p)
              (case (e/server
                      (toast!* user-id :error
                        (str "Sync stopped: Anki reported most of your notes missing. "
                          "Check that the right profile is open.")))
                (do (reset! (:!error recon) "aborted — most notes absent")
                    (reset! (:!phase recon) :error)
                    (t)))
              (case (reset! (:!plan recon) p)
                ;; A dry run stops here: the plan IS the answer, and every phase
                ;; after this one writes. Nothing has touched Anki or the
                ;; database yet — the scan is a read and the plan is a read.
                ;; Deref rather than `e/watch`: the flag is set once by `start!`
                ;; before the phase moves, and a reactive read would re-run this
                ;; branch if it ever changed mid-run.
                (case (reset! (:!phase recon)
                        (if @(:!dry-run? recon) :done :writing))
                  (t))))))))))

(e/defn ReconcileAnkiWork
  "Phase 3 — create, update, re-read, delete. Everything that touches Anki.

   A failed bundle build ENDS the run. Passing its nil `:bundles` on would give
   an empty write, zero pairs, and a report of a clean run — the same silence
   that let a whole document's creates disappear."
  [user-id recon]
  (e/client
    (when (= :writing (e/watch (:!phase recon)))
      (let [p (e/watch (:!plan recon))
            [t _] (e/Token :reconcile-write)]
        (when t
          (let [bundles (e/server (e/Offload #(write-bundles* user-id (:write-card-ids p))))]
            (if-not (:success bundles)
              (case (reset! (:!error recon) (:error bundles))
                (case (reset! (:!phase recon) :error)
                  (t)))
              (case (rc/run-anki-work! (:bundles bundles)
                      (:kind-by-card p)
                      (:note-ids-to-delete p)
                      (:note-ids-to-drop p)
                      recon)
                (t)))))))))

(e/defn ReconcileRecord
  "Phase 4 — one server call writes every baseline, pull, conflict and tombstone.

   `:pull-notes` is the only place a run sends note bodies back, and only for the
   cards it decided to pull."
  [user-id recon]
  (e/client
    (when (= :recording (e/watch (:!phase recon)))
      (let [p (e/watch (:!plan recon))
            scan (e/watch (:!scan recon))
            readback (e/watch (:!readback recon))
            finished (e/watch (:!finished-note-ids recon))
            [t _] (e/Token :reconcile-record)]
        (when t
          (let [outcome {:writes readback
                         :pull-card-ids (:pull-card-ids p)
                         :pull-notes (rc/notes-for (:notes-by-id scan) (:pull-note-ids p))
                         :digests (:digests scan)
                         :backfill (:backfill p)
                         :conflict-card-ids (:conflict-card-ids p)
                         :finished-note-ids finished}
                result (e/server (e/Offload #(finish-run!* user-id outcome)))]
            (case (reset! (:!result recon) result)
              (case (reset! (:!phase recon) :done)
                (t)))))))))

(e/defn ReconcileOutcome
  "Reports the run, once, and releases the running flag.

   The message reports what HAPPENED, never what the plan intended. A run that
   planned 46 creates and wrote none must not say 46 — that was the shape of the
   first defect here, where `anki-write-bundles` (then filtering to already-
   pushed cards) silently dropped every create while the toast read them off the
   plan and claimed success.

   Anki-side write errors are surfaced for the same reason. `do-anki-push!`
   returns them per card and they used to be collected and discarded, so a run
   in which every `addNote` failed looked identical to one that worked."
  [user-id recon !running?]
  (e/client
    (let [phase (e/watch (:!phase recon))
          result (e/watch (:!result recon))
          plan (e/watch (:!plan recon))
          writes (e/watch (:!writes recon))
          err (e/watch (:!error recon))]
      (when (= :done phase)
        (let [[t _] (e/Token :reconcile-done)]
          (when t
            (let [dry? (nil? result)          ; a dry run stops before recording
                  errors (count (:errors writes))
                  conflicts (get-in result [:counts :conflicts] 0)
                  msg (if dry?
                        (str "Dry run — nothing applied. " (:report plan))
                        (str (rec/outcome-text (:counts result))
                          (when (pos? conflicts)
                            (str " — " conflicts
                              (if (= 1 conflicts) " card needs" " cards need")
                              " a decision in Library"))
                          (when (pos? errors)
                            (str " — " errors
                              (if (= 1 errors) " card" " cards")
                              " Anki refused"))))]
              (log/log-info (str "[reconcile] " msg
                              " plan=" (pr-str (:counts plan))))
              (case (e/server (toast!* user-id
                                (cond dry? :success
                                  (or (not (:success result)) (pos? errors)) :warning
                                  :else :success)
                                msg))
                (do (reset! !running? false)
                    (reset! (:!phase recon) nil)
                    (t)))))))
      (when (= :error phase)
        (let [[t _] (e/Token :reconcile-error)]
          (when t
            (case (e/server (toast!* user-id :error (str "Sync failed: " err)))
              (do (reset! !running? false)
                  (reset! (:!phase recon) nil)
                  (t)))))))))

(def cancellable-phases
  "Phases a run may be stopped in.

   `:recording` is excluded. It writes the baselines that say which cards are in
   sync, from bytes already read back out of Anki — stopping partway leaves some
   cards baselined and some not. That state is RECOVERABLE (the ordering
   guarantee is snapshot-then-baseline, so an unbaselined card reads as unsynced
   and is retried, never as falsely synced), but it is short-lived work and
   there is nothing to gain by interrupting it.

   `:done` and `:error` are excluded because the run is already over."
  #{:scanning :planning :writing})

(def unrecoverable-phases
  "Phases where leaving the page strands state the user cannot undo: notes are
   already being written into their Anki collection."
  #{:writing :recording})

(defn cancel!
  "Stop a run by clearing its phase, which unmounts every phase component and
   tears down whatever `e/Offload` was in flight.

   pre : `recon` is a `fresh-state` map.
   post: `!running?` false and phase nil; returns :cancelled, or :not-cancellable
         when the current phase is not in `cancellable-phases`.
   Invariant: safe against a half-written collection. Push writes the settings
         snapshot before sending and takes baselines only from bytes read back,
         so a run stopped between those leaves its cards unsynced and retried."
  [recon !running?]
  #?(:cljs
     (if (contains? cancellable-phases @(:!phase recon))
       (do (reset! (:!phase recon) nil)
         (reset! !running? false)
         :cancelled)
       :not-cancellable)
     :clj nil))

(e/defn ReconcileExecutor
  "Mounts every phase. Headless — the caller owns the trigger and the DOM."
  [user-id recon !running?]
  (e/client
    (ReconcileScan user-id recon)
    (ReconcilePlan user-id recon)
    (ReconcileAnkiWork user-id recon)
    (ReconcileRecord user-id recon)
    (ReconcileOutcome user-id recon !running?)))

(defn fresh-state
  "The atom map one run needs. Created per host component, never shared: these
   feed `e/Token` inputs, and a map rebuilt by a parent re-render would re-fire
   every token in it (CLAUDE.md, state maps)."
  []
  {:!phase (atom nil)
   :!scan (atom nil)
   :!plan (atom nil)
   :!writes (atom nil)
   :!readback (atom nil)
   :!finished-note-ids (atom nil)
   :!result (atom nil)
   :!error (atom nil)
   :!dry-run? (atom false)})

(defn start!
  "Begin a run, or report that one is already going.

   pre : `recon` is a `fresh-state` map. `dry-run?` true stops after planning.
   post: phase `:scanning`, or unchanged and `:busy`.

   A dry run reads Anki and classifies, then reports and stops. It exists
   because the numbers a first run should produce are knowable in advance — on
   the account this was built against, 26 creates, 0 pushes, 0 pulls and 0
   conflicts — so the plan can be checked before anything is written."
  ([recon !running?] (start! recon !running? false))
  ([recon !running? dry-run?]
   #?(:cljs
      (if @!running?
        :busy
        (do (reset! !running? true)
          (reset! (:!dry-run? recon) (boolean dry-run?))
          (reset! (:!error recon) nil)
          (reset! (:!result recon) nil)
          (reset! (:!plan recon) nil)
          (reset! (:!scan recon) nil)
          (reset! (:!writes recon) nil)
          (reset! (:!readback recon) nil)
          (reset! (:!finished-note-ids recon) nil)
          (reset! (:!phase recon) :scanning)
          :started))
      :clj nil)))
