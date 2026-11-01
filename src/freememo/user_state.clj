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
          :article-source-update (atom nil)
          :queue-mutations  (atom 0)
          :pin-mutations    (atom 0)
          :assistant-mutations (atom 0)
          :assistant-stream (atom 0)
          :undo-mutations   (atom 0)
          :kg-mutations     (atom 0)
          :schedule-mutations (atom 0)
          :curriculum-mutations (atom 0)
          :comprehension-mutations (atom 0)
          ;; work member -> its current stage {:phase …}, plus :step :total :since
          ;; once it names one (comprehension/report-step!)
          :comprehension-work (atom {})
          :comprehension-errors (atom {})
          :activity-mutations (atom 0)
          :question-mutations (atom 0)
          :walkthrough-mutations (atom 0)
          :desktop-mutations (atom 0)
          :card-gen-status  (atom {})
          ;; Topic membership hides canceled work while its request drains.
          ;; Each attempt retains its own commit guard through retries.
          :card-gen-cancelled (atom #{})
          :card-gen-cancellers (atom {})
          :scanning-pages   (atom #{})
          :transcribing-topics (atom #{})
          ;; The editor uses active URLs to avoid another fetch for the same URL.
          :importing-urls   (atom #{})
          ;; Import runs retain their outcome until the reporter saves it.
          :url-import-runs  (atom {})
          :html-import-runs (atom {})
          ;; Keep abandoned IDs so a delayed start cannot revive a closed import.
          :html-import-cancelled (atom #{})
          :processing-videos (atom #{})     ; video topic ids in the ffmpeg/Whisper pipeline
          ;; video topic id -> error string for a `process-video!` run that
          ;; failed. Written before membership drops from :processing-videos,
          ;; so `activity-registry`'s :error-fn can read it when the reporter
          ;; closes the row — otherwise a failed run is recorded as a success.
          ;; Left behind after the work is gone, which is what makes the outcome
          ;; readable; the next run on the same topic clears it at its start.
          :video-process-errors (atom {})
          :ocr-errors       (atom {})
          :scan-cancellers  (atom {})
          :scan-runs        (atom {})
          ;; The attempt owns its commit guard. This marker hides canceled work
          ;; from Activity discovery while its HTTP request drains.
          :scan-cancelled   (atom #{})      ; [doc page] pairs the user abandoned
          :distilling-docs  (atom #{})      ; root topic ids with a distill run in flight
          :distill-cancellers (atom {})     ; root topic id -> missionary cancel fn
          :distilling-repos (atom #{})      ; repo root topic ids with a code-distill run in flight
          :repo-distill-cancellers (atom {}); repo root topic id -> missionary cancel fn
          :generating-questions (atom #{})  ; root topic ids with a question run in flight
          :question-cancellers (atom {})    ; root topic id -> missionary cancel fn
          ;; Each key identifies one material or block exam form request.
          ;; Cancellation cannot stop the OpenRouter request. The cancelled set
          ;; prevents that request from saving a form after the user cancels it.
          :exam-form-generations (atom #{})
          :exam-form-generation-cancellers (atom {})
          :exam-form-generation-cancelled (atom #{})
          :exam-form-generation-errors (atom {})
          :exam-grading     (atom {})       ; session id -> [graded total] during end-of-exam grading
          ;; Account transfer publishes membership so the Activity reporter can
          ;; discover it like any other server-owned work. One at a time per
          ;; user, so the set holds a constant rather than an id.
          :exporting-accounts (atom #{})
          :importing-accounts (atom #{})
          ;; A card export download (freememo.card-export-http). Holds the
          ;; constant "cards" for the same reason.
          :exporting-cards (atom #{})
          ;; Git forge imports and updates (freememo.repo-import):
          ;; `host/owner/name` -> phase while the work runs. A map rather than a
          ;; set because the phase moves — cloning or fetching, resolving,
          ;; downloading, creating topics — while one activity row stays open.
          :importing-repos  (atom {})
          ;; Google Doc Pulls (freememo.google-docs): the `gdoc` root ids with
          ;; a Pull running. Also the guard against a second Pull of one root.
          :pulling-gdocs    (atom #{})
          ;; root id -> error of the last failed Pull, written before
          ;; membership drops so the reporter closes the row as failed. The
          ;; next Pull of that root clears it at its start.
          :gdoc-pull-errors (atom {})
          ;; Commit-range walkthroughs being prepared (freememo.walkthrough,
          ;; plans/commit-range-walkthrough.md §6.5.4): walkthrough id -> phase.
          ;; Errors outlive membership so the reporter reads the outcome; the
          ;; cancel mark is cooperative, as a repository import's is.
          :preparing-walkthroughs (atom {})
          :walkthrough-errors (atom {})
          :walkthrough-cancellers (atom {})
          :walkthrough-cancelled (atom #{})
          ;; `host/owner/name` -> error, written before membership drops so the
          ;; reporter closes a failed run as failed. A later success clears it.
          :repo-import-errors (atom {})
          ;; `host/owner/name` -> fn that marks the run cancelled. Neither the
          ;; archive read nor the insert transaction can be interrupted from
          ;; outside, so cancelling is the mark below, read by the run itself.
          :repo-import-cancellers (atom {})
          :repo-import-cancelled (atom #{})
          ;; The Anki sync lease (freememo.anki-lease): "collection" -> {:run-id
          ;; :phase :done :total :heartbeat-at :cancel?}, at most one entry.
          ;; The browser tab that claimed it drives the sync; every tab reads it.
          :anki-run         (atom {})
          ;; "collection" -> fn that marks the running sync cancelled.
          :anki-run-cancellers (atom {})
          ;; "collection" -> error of the last sync that failed. Written before
          ;; the lease drops so the reporter closes the row as failed; cleared
          ;; by the next claim.
          :anki-run-errors  (atom {})
          ;; "collection" -> what the last finished or cancelled sync did, in
          ;; the words of its closing toast. Written before the lease drops so
          ;; the reporter closes the row with it; cleared by the next claim.
          :anki-run-results (atom {})
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
