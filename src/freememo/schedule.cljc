(ns freememo.schedule
  "Server-side orchestration for the Study Scheduler
   (plans/study-scheduler-spec.md §4.8, §9.4.2).

   The seam between three pure namespaces and the world:

     `freememo.schedule-anki`     — acquisition shapes, and the browser half
     `freememo.schedule-fsrs`     — replay and projection
     `freememo.schedule-forecast` — the page and document model

   Nothing here computes FSRS. It loads rows, hands them to those three, and
   writes the derived state back. Every entry point is a plain `defn` with a
   body-level reader conditional, because each is named inside an `(e/server …)`
   form in a reactive body and Electric compiles that call site on BOTH peers
   (see CLAUDE.md, 'Server-Only Functions in .cljc Files').

   Callers MUST wrap these in `e/Offload` (§4.8, §8.4). `ingest-acquisition!`
   replays a document's whole revlog and `forecast` walks a branch tree per
   card; either would stall the reactive graph for the session inside a bare
   `e/server`."
  (:require [freememo.schedule-fsrs :as sf]
            [freememo.schedule-anki :as sa]
            [freememo.schedule-forecast :as fc]
            [freememo.logging :as log]
            #?(:clj [freememo.db :as db])
            #?(:clj [freememo.settings :as settings])
            #?(:clj [freememo.commands :as cmd])))

;; ---------------------------------------------------------------------------
;; Day arithmetic
;; ---------------------------------------------------------------------------

(def ^:const max-horizon-days
  "The furthest target this forecast will walk. Two years. Past it the branch
   tree costs more than the answer is worth — every card's projection is a
   guess about a rating distribution measured on today's behaviour."
  730)

(defn days-until
  "Whole days from today to `iso-date`, floored at 0.

   Uses the SERVER's zone. The app holds no per-user time zone, so a user a
   day ahead of the server sees a horizon one day short — inside the same
   day-grain divergence `schedule-fsrs` already records for the replay.

   Post: 0 for a target today or in the past, and for an unparseable date. A
         target already past is a schedule whose horizon has run out, which
         §6.2's clamp reports as the whole range required."
  [iso-date]
  #?(:clj (try
            (let [target (java.time.LocalDate/parse (str iso-date))
                  today (java.time.LocalDate/now)]
              (max 0 (.toDays (java.time.Duration/between
                                (.atStartOfDay today) (.atStartOfDay target)))))
            (catch Exception _ 0))
     :cljs 0))

(defn signed-days-to-target
  "Whole days from today to `iso-date`, NEGATIVE once the date is past.

   `days-until` floors at 0, and rightly: it feeds a projection horizon, and a
   horizon cannot run backwards. But that floor makes 0 mean both 'today' and
   'three weeks ago', and telling those two apart is the whole job of a
   deadline readout. This is the same arithmetic without the floor.

   Post: >0 future, 0 today, <0 past.
   Post: 0 for an unparseable date, which conflates it with today. Every caller
         reads `study_schedules.target_date`, a NOT NULL DATE that
         `schedule-row->map` has already turned into an ISO string, so an
         unparseable value means the column changed type."
  [iso-date]
  #?(:clj (try
            (let [target (java.time.LocalDate/parse (str iso-date))
                  today (java.time.LocalDate/now)]
              (.toDays (java.time.Duration/between
                         (.atStartOfDay today) (.atStartOfDay target))))
            (catch Exception _ 0))
     :cljs 0))

#?(:clj
   (defn- start-of-today-ms
     "Midnight this morning, in the server's zone.

      The ORIGIN for every day index, and it must be midnight rather than the
      current instant. Measured from `now`, a review three hours old is −0.125
      days, which floors to −1: a card reviewed this morning would read as
      reviewed yesterday, and every interval computed from it would be a day
      short. Measured from midnight it reads 0, which is the day it happened
      on."
     []
     (-> (java.time.LocalDate/now)
       (.atStartOfDay (java.time.ZoneId/systemDefault))
       .toInstant .toEpochMilli)))

(defn- day-index
  "The calendar day `inst` falls on, counted from today.

   Post: 0 for today, negative for the past, positive for the future, nil for
         nil — a card with no last review has no day, and 0 would read as
         today."
  [origin-ms inst]
  (when inst
    (long (Math/floor (/ (double (- (inst-ms inst) origin-ms)) sf/ms-per-day)))))

#?(:clj
   (defn- day->iso-date
     "A day index counted from today, as an ISO date string.

      The date lever used to name its index raw — 'Move the target out to day
      13' — while the Plan table beside it showed real dates. A reader had no
      way to tell day 13 of the month from 13 days out, and converting it was
      arithmetic the surface should have done.

      Pre : `day` is a day index from `earliest-feasible-day`, or nil.
      Post: the ISO date `day` days after today, or nil for nil — a caller with
            no day has no advice to render."
     [day]
     (when day
       (str (.plusDays (java.time.LocalDate/now) (long day))))))

;; ---------------------------------------------------------------------------
;; The maturation lead, memoized
;; ---------------------------------------------------------------------------

#?(:clj
   (defonce ^:private !maturation-leads
     (atom {})))

#?(:clj
   (def ^:private ^:const maturation-lead-cache-size
     "Entries kept before the whole map is dropped. One entry per distinct
      (deck config, θ) pair, which is one per user in the ordinary case and two
      for a user who has set the I4 lever on one schedule. 64 is far above that
      and bounds the map without a reaper."
     64))

#?(:clj
   (defn- maturation-lead-for
     "`schedule-forecast/maturation-lead`, memoized on what it depends on.

      The scan is superlinear in the lead it finds, because `project-card`'s
      branch tree grows with the horizon and the scan walks a fresh tree at
      every horizon from 0 up. Measured on one card: 171 nodes at horizon 5
      against 1911 at horizon 31, so the whole scan costs 2.9 ms at θ 0.90 and
      68.7 ms at θ 0.95.

      That is why this is cached rather than called. Two readers exist that
      MUST stay cheap and both call it per schedule: `schedule-summaries` runs
      once per row of the home view, and `root-schedule-pace` runs on every
      reading view. Each states in its own docstring that it refuses `forecast`
      to avoid a 339 ms projection, and an uncached 68 ms scan would spend a
      fifth of that back.

      Pre : `scheduler` and `theta` are `schedule-inputs`' own, so the key is
            the pair the answer actually depends on.
      Post: `schedule-forecast/maturation-lead`'s value, which may be nil.
      Post: `find`, never `get` — nil is the UNREACHABLE-gate answer and it is
            the most expensive one to compute, so reading it as a cache miss
            would defeat the cache exactly where it is needed most.
      Inv : the cache cannot go stale. The key is the scheduler map itself, and
            a deck-config or θ change builds a different map, which is a
            different key. Nothing needs to evict on a config change."
     [scheduler theta]
     (let [k [scheduler theta]]
       (if-let [hit (find @!maturation-leads k)]
         (val hit)
         (let [v (fc/maturation-lead scheduler theta max-horizon-days)]
           (swap! !maturation-leads
             (fn [m] (assoc (if (> (count m) maturation-lead-cache-size) {} m) k v)))
           v)))))

#?(:clj
   (defn- coverage-span
     "The coverage line's two day counts: its whole width, and how much of it
      has already run.

      This is §6.2's denominator, moved onto the coverage axis. The line runs
      from where the user WAS when they made the plan — not from zero and not
      from today — to the COVERAGE DEADLINE, which is `lead` days before the
      target. Running it to the target instead would ask for the last page on
      the last day, which is the plan defect this change removes.

      Pre : `lead` is `schedule-inputs`' `:lead`, or nil when the gate is
            unreachable. `origin-ms` is midnight today.
      Post: {:span :elapsed}, both >= 0, `:span` >= 1 so a same-day schedule
            divides by something.
      Post: the origin is `covered_baseline_at`, and `created_at` is the
            fallback for a row written before that column existed AND missed by
            the boot backfill. The backfill covers every such row, so the
            fallback is a belt on a brace.
      Post: a baseline in the FUTURE is clamped to today rather than trusted.
            Both writers stamp `now()`, so this needs a clock that ran
            backwards or a hand-edited row — but without the clamp the two
            figures above are only >= 0 under a precondition nothing here can
            check, and a negative `:elapsed` would read as a schedule that has
            not started.
      Inv : `:elapsed` <= `:span`. Both are measured from the same origin, so a
            reader can divide one by the other without clamping."
     [row origin-ms lead]
     (let [baseline (min 0 (long (or (day-index origin-ms
                                       (or (:study_schedules/covered_baseline_at row)
                                         (:study_schedules/created_at row)))
                                   0)))
           deadline (fc/coverage-window
                      (days-until (:study_schedules/target_date row)) lead)
           span (max 1 (- deadline baseline))]
       {:span span :elapsed (- span deadline)})))

#?(:clj
   (defn- study-plan
     "The dated day-by-day plan §1.2 asks for: one row per day from today to
      the target.

      Pre : `pages-remaining` counts pages NOT YET COVERED, >= 0. `reading-days`
            is `schedule-forecast/coverage-window`'s count, `horizon` >= 0, and
            `load` is the derived daily review histogram.
      Pre : `covered-today` counts pages of the range marked done today, and is
            already inside the coverage `pages-remaining` was derived from.
      Post: a vector of {:day :date :weekday :pages :phase :reviews :cumulative},
            one per day, `:date` an ISO string in the server's zone.
      Post: `:pages` on day 0 is what is still OWED today and may be NEGATIVE.
            `:cumulative` runs over `max(0, pages)` for that reason, and still
            lands on the whole remainder — `plan-days` guarantees the sum.
      Post: rows run to the TARGET while pages stop at the coverage deadline.
            The tail rows carry `:phase :mature`, 0 pages and their own review
            load — that window is the artefact this table exists to show, and
            cutting the table at the deadline would hide it.
      Inv : `:pages` comes from `schedule-forecast/plan-days`, the same fn
            `today-plan` reads, so row 0 and the Today tile cannot drift."
     [pages-remaining reading-days horizon load covered-today]
     (let [days (fc/plan-days pages-remaining reading-days horizon covered-today)
           today (java.time.LocalDate/now)]
       (first
         (reduce (fn [[acc run] d]
                   (let [{:keys [pages phase deadline?]} (nth days d)
                         p (long pages)
                         ;; `max 0`: a reader who is ahead owes a negative
                         ;; number today, and crediting that against the
                         ;; running total would report less work left than
                         ;; there is.
                         run' (+ run (max 0 p))
                         date (.plusDays today d)]
                     ;; `:deadline?` is present on ONE row and absent from the
                     ;; rest. These maps cross the wire per windowed row, and a
                     ;; false on every row is a key paid for by every reader of
                     ;; the table to say nothing.
                     [(conj acc (cond-> {:day d
                                         :date (str date)
                                         :weekday (.getDisplayName (.getDayOfWeek date)
                                                    java.time.format.TextStyle/SHORT
                                                    java.util.Locale/ENGLISH)
                                         :pages p
                                         :phase phase
                                         :reviews (double (get load d 0.0))
                                         :cumulative run'}
                                  deadline? (assoc :deadline? true)))
                      run']))
           [[] 0] (range (count days)))))))

(def ^:const suggestion-epsilon
  "The ε the I1 date suggestion searches with (§6.6.1).

   Coarser than the forecast's own. The search costs about nine projections of
   the whole document, and at the default ε that is nine times the work the
   forecast already did. 1e-3 is roughly 7× cheaper per projection and moves R
   by under 0.013 (measured, see `schedule-fsrs/default-epsilon`) — enough to
   name a date, not enough to be the forecast."
  1e-3)

;; ---------------------------------------------------------------------------
;; Ingest — raw acquisition in, derived state out (§9.4.2, §9.4.3)
;; ---------------------------------------------------------------------------

(defn ingest-acquisition!
  "Replay one `schedule-anki/acquire!` result and store the derived state.

   Pre : `acquisition` is any `acquire!` return. `user-id` owns the cards it
         names.
   Post: ALWAYS a map, never nil and never a thrown exception — the caller
         sequences a token on this value, so anything else strands the token
         and freezes the tab. One of
           {:ok true :stored n :deck :fingerprint :theta}   a replay happened
           {:ok true :skipped status}                        nothing to replay
           {:ok false :error msg}                            the replay threw
   Post: NO revlog row is persisted anywhere (§9.4.3) — the raw rows live only
         in this call's stack.
   Post: every card's state is replayed from its WHOLE history, so the stored
         row never depends on what was stored before it. This used to continue
         from the cached state under `:incremental?`, which made the write
         order-dependent; the acquisition now always carries the full revlog.
         `fingerprint` is still written, because readers filter on it (§3.3).
   Inv : the replay runs here, on the JVM. Doing it in the browser would put
         `schedule-fsrs/replay` on the only thread the UI has (§9.4.2)."
  [user-id acquisition]
  #?(:clj
     (if (not= :ok (:status acquisition))
       ;; Not an error: the browser reports :unreachable, :no-fsrs and the
       ;; rest as ordinary outcomes, and the caller renders them. Returning a
       ;; definite value rather than nil is what lets the caller SEQUENCE on
       ;; this call and spend its token — a nil would leave the token unspent
       ;; and the branch mounted forever.
       {:ok true :skipped (:status acquisition)}
       (try
         (let [{:keys [deck deck-config fingerprint infos revlog]} acquisition
               sched (sf/scheduler-from-deck-config deck-config)
               cutoff (sa/ignore-before-ms (:ignoreRevlogsBeforeDate deck-config))
               rows (sa/card-rows sched infos revlog
                      {:cutoff-ms cutoff :fingerprint fingerprint})
               ;; The wire carries epoch millis; the columns are timestamps.
               db-rows (mapv (fn [r]
                               (-> r
                                 (dissoc :last_review_ms :due_ms)
                                 (assoc :last_review_at (some-> (:last_review_ms r)
                                                          long java.sql.Timestamp.)
                                   :due_at (some-> (:due_ms r) long java.sql.Timestamp.))))
                         rows)]
           (db/upsert-anki-deck-config! user-id deck deck-config fingerprint)
           (db/upsert-anki-card-states! user-id db-rows)
           (cmd/bump-channels! user-id #{:schedule-mutations})
           (log/log-info (str "[schedule] ingested deck=" deck
                           " cards=" (count db-rows)))
           {:ok true :stored (count db-rows) :deck deck :fingerprint fingerprint
            :theta (:desired-retention sched)})
         (catch Throwable e
           ;; A replay that throws MUST still return. The caller sequences its
           ;; token on this value, so anything propagating would leave the token
           ;; unspent, the branch mounted, and the tab stuck with no way to
           ;; retry and nothing on screen saying why.
           ;;
           ;; `Throwable`, not `Exception`. `scheduler-from-deck-config` states
           ;; its precondition with `assert`, which throws an AssertionError —
           ;; an Error, so an Exception catch walks straight past it. The
           ;; assert is right to be there: reaching it means `acquire!` sent
           ;; `:status :ok` with a config it should have refused, which is a
           ;; caller bug. It must surface as a message, not as a frozen tab.
           (log/log-error (str "[schedule] ingest failed: " (.getMessage e)))
           {:ok false :error (.getMessage e)})))
     :cljs nil))

;; ---------------------------------------------------------------------------
;; Forecast (§5, §6)
;; ---------------------------------------------------------------------------

#?(:clj
   (defn- projection-cards
     "The projection input for one document's cards: cached FSRS state joined
      to the page each card came from, with backlog days assigned.

      A flashcard can map to SEVERAL Anki cards — a cloze note yields one per
      deletion — and §5.1 averages over all of them. The join is therefore
      note-id to state rows, not card to state."
     [states cards origin-ms capacity]
     (let [by-note (group-by :anki_card_states/anki_note_id (vals states))
           rows (for [c cards
                      s (get by-note (:anki-note-id c) [])]
                  (let [last-day (day-index origin-ms (:anki_card_states/last_review_at s))
                        ivl (double (or (:anki_card_states/interval_days s) 0.0))]
                    {:page-id (:page-id c)
                     :flashcard-id (:flashcard-id c)
                     :anki-card-id (:anki_card_states/anki_card_id s)
                     :state (long (or (:anki_card_states/state s) 1))
                     :step (some-> (:anki_card_states/step s) long)
                     :stability (some-> (:anki_card_states/stability s) double)
                     :difficulty (some-> (:anki_card_states/difficulty s) double)
                     :interval-days ivl
                     :last-review-day last-day
                     ;; The due day is the last review plus Anki's own
                     ;; interval. `assign-first-review-days` then decides
                     ;; whether that is backlog or a date to keep.
                     :due-day (when (and last-day (pos? ivl))
                                (+ last-day (long (Math/round ivl))))}))]
       (fc/assign-first-review-days (vec rows) capacity))))

#?(:clj
   (defn- known-at
     "Known-page count for `pages` at `horizon`, and the verdicts behind it.
      Post: [known-count verdicts projected-cards]."
     [scheduler cards pages theta horizon opts]
     (let [projected (fc/project-cards scheduler cards (assoc opts :horizon-days horizon))
           verdicts (fc/page-verdicts pages projected theta)]
       [(count (filter :known? verdicts)) verdicts projected])))

#?(:clj
   (defn- schedule-inputs
     "Everything derived from one schedule row that a reader needs before it can
      project anything: the page rows, θ, the scheduler, and the projection
      input.

      Pre : `row` came from `db/get-study-schedule` or
            `db/active-study-schedule-for-root`, so ownership is already
            checked.
      Post: {:origin-ms :capacity :horizon :pages :cfg-row :theta :scheduler
             :pcards :lead :status}. `:status` is
              :ok         — Anki state exists for this document's cards
              :no-config  — Anki has never been read for this user (§8.1)
              :no-cards   — it has, but this document has none (§8.3)
      Post: `:lead` is the maturation lead in days, and it is measured HERE
            because three readers need it — the plan, the pace line and the
            feasibility test — and each deriving its own would let them
            disagree about when the reading has to stop.
      Post: `:lead` is 0 on every status but `:ok`. With no card state there is
            nothing to mature: `page-verdict` reads a done page as
            `:uncarded-done` and it counts the day it is marked. A lead of 0 is
            the truth on those paths, not a fallback.
      Inv : `:scheduler` and `:pcards` are non-nil EXACTLY when `:status` is
            `:ok`. Every other status has no memory state to project, which is
            what `current-progress` branches on."
     [user-id row]
     (let [origin-ms (start-of-today-ms)
           root (:study_schedules/root_topic_id row)
           excluded (set (:study_schedules/excluded_pages row))
           capacity (long (:study_schedules/daily_capacity row))
           horizon (min max-horizon-days
                     (days-until (:study_schedules/target_date row)))
           all-pages (db/get-schedule-pages user-id root
                       (:study_schedules/first_page row)
                       (:study_schedules/last_page row))
           ;; `:done-today?` is decided HERE, against `origin-ms`, because that
           ;; is the app's one definition of a day — the same midnight
           ;; `days-until` and every day index already use. Comparing in SQL
           ;; would put a second boundary in a second timezone.
           pages (mapv #(assoc % :excluded? (contains? excluded (:page-number %))
                                 :done-today? (= 0 (day-index origin-ms (:done-at %))))
                   all-pages)
           cfg-row (db/get-anki-deck-config user-id)
           deck-config (:anki_deck_configs/config cfg-row)
           base {:origin-ms origin-ms :capacity capacity :horizon horizon
                 :pages pages :cfg-row cfg-row :lead 0}]
       (if (nil? deck-config)
         (assoc base :status :no-config :theta 0.9)
         (let [theta (double (or (:study_schedules/theta row)
                               (:desiredRetention deck-config) 0.9))
               ;; Lever I5: an excluded page's cards are withheld, so the page
               ;; falls to done-with-no-cards and its cost lands in the exposure
               ;; figure rather than out of sight (§6.6.4.1).
               excluded-page-ids (set (map :page-id (filter :excluded? pages)))
               cards (remove #(contains? excluded-page-ids (:page-id %))
                       (db/get-schedule-cards user-id root))
               states (db/get-anki-card-states user-id
                        (distinct (map :anki-note-id cards))
                        (:anki_deck_configs/fingerprint cfg-row))]
           (if (empty? states)
             (assoc base :status :no-cards :theta theta)
             (let [scheduler (sf/scheduler-from-deck-config deck-config theta)]
               (assoc base :status :ok :theta theta
                 :scheduler scheduler
                 :lead (maturation-lead-for scheduler theta)
                 :pcards (projection-cards states cards origin-ms capacity)))))))))

#?(:clj
   (defn- current-progress
     "Document progress TODAY, from a `schedule-inputs` map.

      ONE definition, read by `forecast`'s `:progress` and by
      `root-schedule-pace`. Two paths over the same page count is exactly the
      shape of the defect this feature already shipped once: `start_known` was
      frozen from the DONE page count while §6.2's line was measured against
      the KNOWN count, so a schedule read '5 pages behind' on the day it was
      created. A second reader computing its own known-now would reopen it.

      Post: `schedule-forecast/document-progress` of the h=0 verdicts, so
            `:total` is the page count and `:known` is `known-at`'s own figure.
      Inv : a status other than `:ok` projects an EMPTY card list, so a page is
            known there only because it is marked done. That is the half of
            §1.2 the user can act on with no Anki connection at all."
     [{:keys [scheduler pcards pages theta status]}]
     (fc/document-progress
       (if (= :ok status)
         (second (known-at scheduler pcards pages theta 0 {}))
         (fc/page-verdicts pages [] theta)))))

#?(:clj
   (defn- pace-of
     "§6.2's lag or lead for one schedule, from its inputs and today's progress.

      THREE readers need this figure — the forecast, the toolbar badge, and the
      dashboard summary — and each assembling its own `fc/pace` call is how the
      arguments drift. `start_known` was once frozen from the DONE page count
      while the line was measured against the KNOWN count, and a fresh schedule
      read '5 pages behind' on the day it was created.

      The figure is COVERAGE, and every term moves with it: the reading is
      `:covered`, the baseline is `start_covered`, and the line ends at the
      coverage deadline. That is what the plan table asks for, so a user who
      works the table reads `on plan` — which the known-based line could never
      deliver, because known trails coverage by the maturation lead.

      Pre : `progress` is `current-progress` over `inputs`, both derived from
            `row`. Mixing a progress map from one schedule with another's row
            is a caller bug and produces a plausible wrong number.
      Post: `schedule-forecast/pace`'s {:required :actual :delta}."
     [row inputs progress]
     (let [{:keys [span elapsed]} (coverage-span row (:origin-ms inputs) (:lead inputs))]
       (fc/pace {:covered-now (:covered progress)
                 :total-pages (:total progress)
                 :start-covered (:study_schedules/start_covered row)
                 :elapsed-days elapsed
                 :total-days span}))))

(def forecast-status-labels
  "One label per status `forecast` and `schedule-inputs` can report.

   Here rather than in the view, for the reason `schedule-forecast/reason-labels`
   states about its own: this namespace decides which key to emit, so it owns
   what each one means. A status added without a label would render blank.

   This is NOT `schedule-anki/acquisition-states`. That map describes the Anki
   CONNECTION — offline, too old, FSRS off — and this one describes what a
   schedule has to forecast WITH. They overlap on `:no-cards` alone, and
   reading one where the other belongs is how the dashboard would report a
   connection fault as a document problem.

   A plain `def`, no reader conditional: a CLJ-only `def` read from client
   scope compiles with zero warnings, emits nothing on the CLJS peer, and kills
   the WebSocket at mount when the two disagree on frame shape (CLAUDE.md,
   `transcribe_language.cljc`)."
  {:ok "forecast ready"
   :no-config "Anki not read yet"
   :no-cards "no cards pushed"
   :not-found "schedule is gone"})

(defn forecast
  "The whole forecast for one schedule (§1.1, §1.2, §5, §6).

   Pre : `user-id` owns `schedule-id`. Ownership is checked in
         `db/get-study-schedule`, not here.
   Post: ALWAYS a map with a `:status`, never nil. nil conflated two states the
         surface must tell apart: Electric holds a frame pending until an
         `e/Offload` resolves, so a nil return never meant still-loading — it
         meant the schedule does not exist, and rendering it as a loading
         message left a stale bookmark waiting forever with no way out.
         `:status` is one of
           :ok         — a forecast, from state cached at `:cached-at`
           :not-found  — no such schedule, or it belongs to another user
           :no-config  — Anki has never been read for this user (§8.1)
           :no-cards   — the document has no pushed cards (§8.3)
         and which always carries `:progress`, so a document with no cards
         still renders its done-page count rather than an error.
   Post: `:progress` is TODAY and `:projected` is the target date. §1.1 needs
         both, and the two differ only by the horizon the projection ran at.
   Inv : `:daily-load` is DERIVED from the projection, never estimated from a
         rate (§6.3). Its sum over days is the total projected reviews.

   The feasibility test compares the peak load against the schedule's own
   `daily_capacity` — see the note above `combined-today-load` for why that is
   a declared figure rather than a measured one."
  [_refresh user-id schedule-id]
  #?(:clj
     (if-let [row (db/get-study-schedule user-id schedule-id)]
       (let [inputs (schedule-inputs user-id row)
             {:keys [origin-ms capacity horizon pages cfg-row theta
                     scheduler pcards status lead]} inputs
             progress (current-progress inputs)
             ;; The plan's unit. Pages the reader has not marked done is the
             ;; only remainder a reading action can reduce today — the unknown
             ;; count charges them again for every page whose cards are still
             ;; young, and no amount of reading removes one of those.
             uncovered (- (:total progress) (:covered progress))
             ;; Pages covered TODAY. Day 0 of the plan is a quota that counts
             ;; down by this, so it must come from the same progress map as
             ;; `uncovered` — a count taken separately could describe a
             ;; different instant and make the allocation drift.
             covered-today (:covered-today progress)
             reading-days (fc/coverage-window horizon lead)
             base {:schedule row
                   :horizon-days horizon
                   :cached-at (db/anki-state-cached-at user-id)
                   :pages pages
                   ;; §1.2 — what the plan table's split is made of, named once
                   ;; so the tile, the caption and the table cannot disagree.
                   ;; `:cover-by` is the LAST day of the reading window, which
                   ;; is the date the covering must meet. It is deliberately
                   ;; not the last day that happens to carry a page: falling
                   ;; behind redistributes the remainder across the days left,
                   ;; so the window's end is the load-bearing date and the last
                   ;; page's day is an artefact of today's rounding.
                   ;;
                   ;; `:mature-days` is the width of the maturation window, and
                   ;; it equals the number of `:mature` rows `plan-days` emits
                   ;; at every input — `schedule_fsrs_test` asserts that, which
                   ;; is what lets the caption read this instead of counting
                   ;; the plan on the client.
                   ;; `:pages-remaining` is the plan's own headline figure, and
                   ;; it is reported rather than left to be summed off the
                   ;; table. Day 0 is a quota and goes negative once the reader
                   ;; is ahead, so `(reduce + (map :pages plan))` undercounts by
                   ;; the surplus — measured reading `0 pages` with five still
                   ;; to cover. `study-plan`'s cumulative already clamps; this
                   ;; removes the second place that had to remember to.
                   :coverage {:lead lead
                              :reading-days reading-days
                              :mature-days (max 0 (- horizon reading-days))
                              :pages-remaining uncovered
                              :cover-by (day->iso-date (dec reading-days))}}]
         (if (not= :ok status)
           ;; Anki has never been read, or this document has no pushed cards.
           ;; Either way there is no R and no review load. The PAGE plan needs
           ;; neither, and it is the half of §1.2 the user can act on today —
           ;; so it renders here rather than waiting on a connection they may
           ;; not have.
           (assoc base :status status :theta theta
             :progress progress
             :plan (study-plan uncovered reading-days horizon {} covered-today))

           (let [[known-end verdicts-end projected]
                 (known-at scheduler pcards pages theta horizon {})
                 load (fc/daily-review-load projected)
                 peak (fc/peak-daily-load load)
                 total (:total progress)
                 projected-progress (fc/document-progress verdicts-end)
                 ;; What moving the date can in fact deliver. A page with no
                 ;; cards and no done mark cannot become known by reviewing —
                 ;; the projection models reviews, not future carding — so it
                 ;; is outside every question the date lever can answer.
                 reachable (count (remove #(= :untouched (:reason %)) verdicts-end))
                 base-feas (fc/feasibility
                             {:projected-known known-end
                              :required-pages total
                              :reachable-pages reachable
                              :peak-load peak
                              :observed-capacity capacity
                              :horizon horizon
                              :maturation-lead lead})
                 ;; §6.6.1 — the date lever names a date, and ONLY when the
                 ;; date is what is blocking. The search costs nine more
                 ;; projections of the whole document, so no other reason pays
                 ;; for it, and — the reason this gate exists — a lever that
                 ;; named a date for a blocker no date can move re-suggested
                 ;; the date the user had just applied, forever.
                 feas (if-not (some #{:short-of-target} (:reasons base-feas))
                        base-feas
                        ;; `:earliest-date` is DERIVED from `:earliest-day`
                        ;; here, in one place, so the two cannot name
                        ;; different days.
                        (let [earliest (when (pos? reachable)
                                         (fc/earliest-feasible-day
                                           (fn [d] (first (known-at scheduler pcards pages theta d
                                                            {:epsilon suggestion-epsilon})))
                                           reachable max-horizon-days))]
                          (assoc base-feas
                            :earliest-day earliest
                            :earliest-date (day->iso-date earliest))))]
             (assoc base
               :status :ok
               :theta theta
               :deck (:anki_deck_configs/deck cfg-row)
               :capped? (boolean (some #(get-in % [:projected :capped?]) projected))
               :progress progress
               :projected projected-progress
               :verdicts verdicts-end
               :pace (pace-of row inputs progress)
               :daily-load load
               :peak-load peak
               ;; §1.2 — the plan the user actually follows. Pages come
               ;; from the straight line, reviews from the projection.
               :plan (study-plan uncovered reading-days horizon load covered-today)
               :today (fc/today-plan {:pages-remaining uncovered
                                      :reading-days reading-days
                                      :horizon horizon
                                      :covered-today covered-today
                                      :daily-load load})
               :feasibility feas))))
       ;; No such schedule, or it belongs to somebody else. A distinct status,
       ;; not nil: the surface must say "this is gone" rather than "still
       ;; working", and the two are indistinguishable once both are nil.
       {:status :not-found})
     :cljs nil))

;; ---------------------------------------------------------------------------
;; Query wrappers named inside reactive bodies
;; ---------------------------------------------------------------------------
;; Each takes `_refresh` first so an `e/watch` on a channel atom creates the
;; dependency, and each carries a body-level reader conditional because
;; `(e/server (f …))` is compiled on the CLJS peer too.

(defn refresh-inputs
  "What the browser needs to acquire from Anki (§2.4).

   Post: {:note-ids [long …] :push-deck string-or-nil}. The note ids span EVERY
         active schedule's document, so one acquisition serves the whole tab
         rather than one per schedule.
   Post: `:push-deck` is the deck this user's Anki sync writes to. It is the
         LAST resort for finding a preset, used only when every deck the cards
         are actually in turns out to be a filtered deck (`acquire!`). Read
         from the global preference rather than a per-document preset, because
         the note ids here span every schedule and no single document owns
         them.
   Post: `:high-water` is gone. It fed the deck-scoped incremental pull, which
         lost reviews for any card in a filtered deck; the revlog is read whole
         and by card id now (`schedule-anki`)."
  [_refresh user-id]
  #?(:clj
     (let [roots (distinct (map :study_schedules/root_topic_id
                             (db/list-study-schedules user-id)))
           note-ids (distinct (mapcat #(map :anki-note-id
                                         (db/get-schedule-cards user-id %))
                                roots))]
       {:note-ids (vec note-ids)
        :push-deck (settings/get-anki-deck user-id)})
     :cljs {:note-ids [] :push-deck nil}))

#?(:clj
   (defn- display-rows
     "Schedule rows as plain display data.

      Keeps the raw DB row out of client scope: it carries a `java.sql.Date`
      and an array column that mean nothing there.

      The joined columns come back as `:topics/doc_title` and
      `:topics/doc_kind`: next.jdbc qualifies an aliased column by the table it
      was selected FROM, not by the alias. Reading a bare `:doc_title` returns
      nil, which renders as 'Untitled' with nothing in any log."
     [rows]
     (mapv (fn [r] {:id (:study_schedules/id r)
                    :title (or (:topics/doc_title r) "Untitled")
                    :kind (:topics/doc_kind r)
                    :root-topic-id (:study_schedules/root_topic_id r)
                    :target-date (:study_schedules/target_date r)
                    :days-left (days-until (:study_schedules/target_date r))
                    :first-page (:study_schedules/first_page r)
                    :last-page (:study_schedules/last_page r)
                    :capacity (:study_schedules/daily_capacity r)
                    :theta (:study_schedules/theta r)
                    :archived? (some? (:study_schedules/archived_at r))})
       rows)))

(defn list-archived-schedules
  "Archived schedules as display rows, for the sidebar's archived section."
  [_refresh user-id]
  #?(:clj (display-rows (db/list-archived-study-schedules user-id))
     :cljs []))

(defn restore-schedule!
  "Return an archived schedule to the active set. Returns {:ok bool}.

   Post: ALWAYS a map, for the reason `archive-schedule!` states — the caller
         sequences a token on it."
  [user-id schedule-id]
  #?(:clj
     (try
       (db/restore-study-schedule! user-id schedule-id)
       (cmd/bump-channels! user-id #{:schedule-mutations})
       {:ok true}
       (catch Throwable e
         (log/log-error (str "[schedule] restore failed: " (.getMessage e)))
         {:ok false :error (.getMessage e)}))
     :cljs nil))

(defn list-schedules
  "Active schedules as plain display rows. Keeps the raw DB row out of client
   scope: it carries a `java.sql.Date` and an array column that mean nothing
   there."
  [_refresh user-id]
  #?(:clj (display-rows (db/list-study-schedules user-id))
     :cljs []))

(defn root-schedule-pace
  "Pace and deadline for the active schedule over `root-topic-id`, or nil.

   What the reading view's badge shows, and nothing more. It deliberately does
   NOT call `forecast`: that projects the whole document at the full horizon
   (339 ms at 90 days, 1.2 s at 365, measured) and searches up to seventeen
   more horizons for the date lever, while a pace figure needs only h=0.
   Every figure here comes from `current-progress` and the row.

   Still about 6 ms after this feature, and that depends on
   `maturation-lead-for` being a CACHE. The pace line ends at the coverage
   deadline, so this needs the lead, and computing it costs 2.9 ms at θ 0.90
   and 68.7 ms at θ 0.95 — which on this path, once per reading view, would
   spend back a fifth of what refusing `forecast` saves.

   Pre : nothing — a nil or unscheduled `root-topic-id` is the ordinary case,
         not a caller bug. Ownership is checked in
         `db/active-study-schedule-for-root`.
   Post: nil when no ACTIVE schedule covers the root, so the caller renders
         nothing rather than an empty badge.
   Post: otherwise {:state :days-to-target :tooltip}, all plain data. `:state`
         is `schedule-forecast/pace-state`'s three-way verdict, which the badge
         names, and `:tooltip` is the finished hover string.
   Post: the tooltip is built HERE, not on the client, for the reason
         `topic-state/get-page-info*` states about its own: the client only
         ever displays the string, so shipping the raw figures and formatting
         them there is wire waste and a second place for the wording to live.
   Post: `:days-to-target` is SIGNED — negative once the target is past. The
         badge has to tell 'today' from 'overdue', and `days-until`'s floor
         reports 0 for both.
   Inv : `:state` is derived from the same `pace` call `forecast` makes, over
         the same `current-progress`, so the badge and the Schedule tab cannot
         report different pace for one schedule."
  [_refresh user-id root-topic-id]
  #?(:clj
     (when root-topic-id
       (when-let [row (db/active-study-schedule-for-root user-id root-topic-id)]
         (let [inputs (schedule-inputs user-id row)
               progress (current-progress inputs)
               p (pace-of row inputs progress)
               state (fc/pace-state (:delta p))]
           {:state state
            :days-to-target (signed-days-to-target (:study_schedules/target_date row))
            :tooltip (str (fc/pace-phrase state (:delta p))
                       " · target " (:study_schedules/target_date row))})))
     :cljs nil))

(defn schedule-summaries
  "One row per ACTIVE schedule, at TODAY's horizon only — the tab's home view.

   Every figure comes from `current-progress` at h=0 and the row itself, so a
   summary costs 3.8 ms per row, measured. Calling `forecast` per schedule
   instead would cost 339 ms at a 90-day horizon and 1.2 s at a year, for a
   review column the home view does not show.

   Per row is what makes `maturation-lead-for`'s cache load-bearing here. Every
   row needs the lead, and every row of one user resolves to the SAME lead — one
   deck config, one θ unless a schedule sets the I4 lever. Uncached, an
   N-schedule home view would run N identical 2.9 ms scans at θ 0.90 and N
   identical 68.7 ms scans at θ 0.95.

   Pre : nothing. A user with no schedules gets an empty vector, which is what
         the view renders its empty state from.
   Post: a vector of plain display maps, ordered as `db/list-study-schedules`
         orders them — soonest target first, ties by lowest id — so the home
         view and the sidebar agree on order.
   Post: `:status` is `schedule-inputs`' verdict, labelled through
         `forecast-status-labels`. It is per-SCHEDULE, which is why the tab
         header needs no global Anki chip: with FSRS off nothing ingests, so no
         deck config is cached and every row reports `:no-config` on its own.
   Inv : ONE query for the list, then one `schedule-inputs` per row. The view
         MUST NOT map a per-schedule reader over the list — that would put the
         round trip inside the loop."
  [_refresh user-id]
  #?(:clj
     (mapv
       (fn [row]
         (let [inputs (schedule-inputs user-id row)
               progress (current-progress inputs)
               p (pace-of row inputs progress)
               uncovered (- (:total progress) (:covered progress))]
           {:id (:study_schedules/id row)
            :title (or (:topics/doc_title row) "Untitled")
            :target-date (:study_schedules/target_date row)
            :days-to-target (signed-days-to-target (:study_schedules/target_date row))
            :status (:status inputs)
            :status-label (get forecast-status-labels (:status inputs))
            :pace-state (fc/pace-state (:delta p))
            :pace-phrase (fc/pace-phrase (fc/pace-state (:delta p)) (:delta p))
            :covered (:covered progress)
            :known (:known progress)
            :total (:total progress)
            ;; Through `today-plan`, not through its own split: that fn is the
            ;; definition of 'pages today', and the home view must not answer
            ;; the question differently from the tab it links to. The load is
            ;; empty because this view shows no review column and projecting
            ;; one would cost the 339 ms this fn exists to avoid.
            :pages-today (:pages-today
                          (fc/today-plan
                            {:pages-remaining uncovered
                             :reading-days (fc/coverage-window (:horizon inputs) (:lead inputs))
                             :horizon (:horizon inputs)
                             :covered-today (:covered-today progress)
                             :daily-load {}}))}))
       (db/list-study-schedules user-id))
     :cljs []))

(defn documents-for-picker
  "Root documents a schedule can be built over, newest first.

   PDFs only, and only those with pages. `db/schedulable-kinds` states why:
   this feature counts pages, and no other root kind has any."
  [_refresh user-id]
  #?(:clj (db/list-schedulable-documents user-id)
     :cljs []))

(defn create-schedule!
  "Create a schedule and freeze its starting known count (§6.2).

   Pre : `user-id` owns `:root-topic-id`.
   Post: {:ok true :id n} or {:ok false :error msg}. The error shape is the
         project's, so the form's commit handler spends its token either way
         rather than leaving a submit button spinning.
   Post: `start_covered` is the covered count TODAY, measured by the SAME rule
         the pace figure of §6.2 compares against, so a schedule opened the day
         it was made reads `on plan`. `start_known` is the known count at the
         same instant, kept for the record.

   Both baselines come from ONE `current-progress` map, read by running the
   forecast once at horizon 0 after the row exists. Two measurements would let
   the pace line's two ends describe different moments, and taking the covered
   count from a second query is how that drift starts."
  [user-id attrs]
  #?(:clj
     (try
       (let [row (db/create-study-schedule! user-id (assoc attrs :start-known 0))
             id (:study_schedules/id row)
             progress (:progress (forecast 0 user-id id))
             known-now (or (:known progress) 0)
             covered-now (or (:covered progress) 0)]
         (db/set-schedule-baselines! user-id id known-now covered-now)
         (cmd/bump-channels! user-id #{:schedule-mutations})
         {:ok true :id id :start-known known-now :start-covered covered-now})
       ;; Throwable for the reason `ingest-acquisition!` states: this calls
       ;; `forecast`, which builds a scheduler behind an `assert`.
       (catch Throwable e
         (log/log-error (str "[schedule] create failed: " (.getMessage e)))
         {:ok false :error (.getMessage e)}))
     :cljs nil))

(defn update-schedule!
  "Apply one lever (§6.6). Returns {:ok bool}."
  [user-id schedule-id fields]
  #?(:clj
     (try
       (db/update-study-schedule! user-id schedule-id fields)
       (cmd/bump-channels! user-id #{:schedule-mutations})
       {:ok true}
       (catch Throwable e
         (log/log-error (str "[schedule] update failed: " (.getMessage e)))
         {:ok false :error (.getMessage e)}))
     :cljs nil))

(defn archive-schedule!
  "Retire a schedule (§1.3 stops counting its load).

   Post: ALWAYS {:ok bool}, never a thrown exception. The caller sequences a
         token on this value and navigates away on success, so a propagating
         exception would leave the Archive button disabled with nothing said."
  [user-id schedule-id]
  #?(:clj
     (try
       (db/archive-study-schedule! user-id schedule-id)
       (cmd/bump-channels! user-id #{:schedule-mutations})
       {:ok true}
       (catch Throwable e
         (log/log-error (str "[schedule] archive failed: " (.getMessage e)))
         {:ok false :error (.getMessage e)}))
     :cljs nil))

(def ^:const default-daily-capacity
  "The daily review budget a new schedule starts with (§6.6.2).

   60, because the canonical user's measured rate on deck `All` over the last
   69 days was 61.0 reviews per day (lifetime mean 40.2, peak 289). A default
   near what the user already does makes the first feasibility verdict mean
   something; a default of 0 or of 9999 would make it meaningless in opposite
   directions."
  60)

;; §6.7 — WHOSE rate the feasibility test compares against.
;;
;; The capacity is the user's DECLARED budget, `study_schedules.daily_capacity`,
;; which lever I2 raises. It is not measured, and this is deliberate.
;;
;; The measurement is not available. FreeMemo's own tables hold topic advances
;; (`topic_repetitions`) and quiz answers (`kg_reviews`); neither is an Anki
;; review, and the user reviews these cards IN ANKI. No action in the read-only
;; set of §1.5 reports a collection's review rate — `getNumCardsReviewedByDay`
;; exists in AnkiConnect and is deliberately outside that set. Reporting a
;; count of topic advances under the name "review rate" would be a number
;; wearing another number's name, which is the failure R3 was rejected for in
;; the design log (§10.2).
;;
;; So the warning of §6.7 fires against a figure the user stated and can
;; change, and `default-daily-capacity` seeds it from their measured behaviour.

(defn combined-today-load
  "One review number across every active schedule (§1.3, §6.5).

   Post: {:reviews-today double :schedules n}. Concurrent schedules share one
         pair of hands, so their day-0 loads add; a user with no schedule reads
         0 rather than nil."
  [_refresh user-id]
  #?(:clj
     (let [rows (db/list-study-schedules user-id)
           loads (keep (fn [r]
                         (some-> (forecast 0 user-id (:study_schedules/id r))
                           :daily-load))
                   rows)]
       {:reviews-today (reduce + 0.0 (map #(get % 0 0.0) loads))
        :schedules (count rows)})
     :cljs nil))
