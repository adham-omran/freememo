(ns freememo.user-state
  "Per-user server-side state registry.
   Returns the same atom for a given (user-id, key) pair across all sessions.
   Thread-safe via ConcurrentHashMap.computeIfAbsent."
  (:import [java.util.concurrent ConcurrentHashMap]))

(defonce ^ConcurrentHashMap registry (ConcurrentHashMap.))

(defn users-with-state
  "Every user-id this process holds any state for.

   The Activity reporter needs it: it discovers in-flight work by reading these
   atoms, so it has to know whose to read, and a user whose work has no activity
   row yet is exactly the one it must not miss. Keys are `[user-id key]` pairs.

   Post: a set of user-ids. Grows with logged-in users seen since boot, not with
         users in the database, so the scan stays proportional to activity."
  []
  (into #{} (map first) (keys registry)))

(defn get-atom
  "Get or create a per-user atom. Returns the same atom for repeated calls
   with the same user-id and key. Thread-safe."
  [user-id k]
  (.computeIfAbsent registry [(long user-id) k]
    (reify java.util.function.Function
      (apply [_ _]
        (case k
          :refresh          (atom 0)
          :credits-refresh  (atom 0)
          :meta-refresh     (atom 0)
          :settings-refresh (atom 0)
          :card-mutations   (atom 0)
          :sync-mutations   (atom 0)
          :tree-mutations   (atom 0)
          :queue-mutations  (atom 0)
          :pin-mutations    (atom 0)
          :assistant-mutations (atom 0)
          :undo-mutations   (atom 0)
          :kg-mutations     (atom 0)
          :schedule-mutations (atom 0)
          :curriculum-mutations (atom 0)
          :activity-mutations (atom 0)
          :card-gen-status  (atom {})
          ;; Card generation cancels QUEUED work only. The processor is an m/ap
          ;; over a mailbox with `m/?> 3`, which hands out no per-item cancel fn,
          ;; and an in-flight LLM call has already committed its credits — so
          ;; there is nothing a canceller could usefully stop once a run starts.
          :card-gen-cancelled (atom #{})    ; topic ids whose queued items to skip
          :card-gen-cancellers (atom {})    ; topic id -> fn marking it cancelled
          :scanning-pages   (atom #{})
          :transcribing-topics (atom #{})
          ;; URLs being fetched by a link Import, so the Activity monitor can
          ;; discover them. The member is the url: one import per url at a
          ;; time, and a second click on the same link joins the first rather
          ;; than racing it.
          :importing-urls   (atom #{})
          ;; url -> error string for a link Import that failed. Written before
          ;; membership drops, so `activity-registry`'s :error-fn can still read
          ;; it when the reporter closes the row — otherwise a failed import is
          ;; recorded in history as a success. Left behind on purpose after the
          ;; work is gone, which is what makes the outcome readable; a later
          ;; success on the same url clears its entry, so only failures persist.
          :url-import-errors (atom {})
          :processing-videos (atom #{})     ; video topic ids in the ffmpeg/Whisper pipeline
          :ocr-errors       (atom {})
          :scan-cancellers  (atom {})
          ;; Cancelling a page scan cannot abort its OCR request — clj-http's
          ;; socket read ignores Thread.interrupt() — so the missionary cancel is
          ;; only an attempt, and THIS is what makes cancelling mean something:
          ;; `extract-page-text` reads it before writing and leaves the page
          ;; alone. Also what keeps a cancelled scan from being re-discovered as
          ;; a second activity while its request drains.
          :scan-cancelled   (atom #{})      ; [doc page] pairs the user abandoned
          :distilling-docs  (atom #{})      ; root topic ids with a distill run in flight
          :distill-cancellers (atom {})     ; root topic id -> missionary cancel fn
          :distilling-repos (atom #{})      ; repo root topic ids with a code-distill run in flight
          :repo-distill-cancellers (atom {}); repo root topic id -> missionary cancel fn
          :generating-questions (atom #{})  ; root topic ids with a question run in flight
          :question-cancellers (atom {})    ; root topic id -> missionary cancel fn
          :exam-grading     (atom {})       ; session id -> [graded total] during end-of-exam grading
          ;; Account transfer publishes membership so the Activity reporter can
          ;; discover it like any other server-owned work. One at a time per
          ;; user, so the set holds a constant rather than an id.
          :exporting-accounts (atom #{})
          :importing-accounts (atom #{})
          :toasts           (atom []))))))
;; Three keys are deliberately absent, each for its own reason.
;;
;; :pending-commands and :pending-cards moved to freememo.client-state (per page
;; load). Sharing them per-user made every command execute once per open tab, so
;; re-adding them here re-creates that defect.
;;
;; :uploading-videos was declared as `session id -> {:filename :sent :total}` and
;; never read or written by anything. It was a server-side MIRROR of progress the
;; browser already holds, for a pull the browser tier does not need: an upload is
;; driven by a mounted client component, which pushes straight to its own
;; `activities` row through freememo.activity. A mirror would be a second copy of
;; state that row already carries, and the two could disagree.

;; ---------------------------------------------------------------------------
;; "Work started" notification.
;;
;; The Activity reporter discovers work by reading these atoms on a 20s timer,
;; which is correct but LATE: a row appeared up to a tick after the click, and
;; work that finished inside one tick was never seen running at all — a card
;; generation reached the panel already reading "Done".
;;
;; So an owner says when it starts. Not registration: the owner still never
;; calls `start!` or `terminate!`, and an owner that forgets to call this is
;; merely late again rather than invisible, which is what keeps the timer the
;; correctness mechanism and this only a latency fix.
;;
;; It lives HERE, behind an inversion, because every owner already requires this
;; namespace — putting it in the reporter would give eight owners a dependency
;; on it, and `user-state` cannot require the reporter without a cycle. Same
;; shape as `db/register-sweep-job!`.
;; ---------------------------------------------------------------------------

(defonce ^:private !work-started-listeners (atom {}))

(defn on-work-started!
  "Register `f` under `k` to be called with a user-id when that user may have
   new in-flight work. Idempotent per key, so a namespace reload replaces rather
   than doubles."
  [k f]
  (swap! !work-started-listeners assoc k f)
  nil)

(defn work-started!
  "Tell listeners `user-id` may have new in-flight work.

   Pre : called after EVERY atom the owner publishes for this work already
         reflects it — its membership AND its canceller, if it has one.
         Nudging between the two records the work as not cancellable, which is
         how four owners shipped a page scan and three distills whose Cancel
         never appeared. The projection now reads cancellability live, so that
         mistake is no longer visible, but the ordering is still the contract.
         (Violation = caller bug: late or under-reported, never wrong, because
         the timer still catches it.)
   Post: every listener has been handed `user-id`. Returns nil.
   Invariant: never blocks and never throws into the caller. Listeners do
         database writes, and this sits on a user's click path — an owner must
         not pay a round trip, nor fail to start work because reporting failed."
  [user-id]
  (doseq [[_ f] @!work-started-listeners]
    (future (try (f user-id) (catch Throwable _ nil))))
  nil)

(defmacro with-in-flight
  "Run `body` while `member` is published in this user's `state-key` set.

   The shape every server-owned owner needs so the Activity reporter can
   discover its work: present means running, absent means finished. Written once
   here because the `finally` is the part that is easy to omit — an owner that
   returns early, or throws, would otherwise leave its work looking alive until
   staleness closed it a minute later.

   Pre : `state-key` names a per-user SET atom.
   Post: `member` is in the set for the dynamic extent of `body` and out of it
         afterwards, on every path. Returns body's value."
  [user-id state-key member & body]
  `(let [a# (get-atom ~user-id ~state-key)
         m# ~member
         uid# ~user-id]
     (swap! a# conj m#)
     ;; After the swap, never before: a listener that looked first would find
     ;; nothing and report the work late anyway.
     (work-started! uid#)
     (try ~@body
       (finally (swap! a# disj m#)))))
