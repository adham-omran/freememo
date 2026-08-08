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
   (defn- schedule-span
     "Days from a schedule's creation to its target, floored at 1.

      This is §6.2's denominator: the straight line runs from where the user
      WAS when they made the plan, not from zero and not from today. Floored at
      1 so a same-day schedule divides by something."
     [row origin-ms]
     (let [created (day-index origin-ms (:study_schedules/created_at row))
           horizon (days-until (:study_schedules/target_date row))]
       (max 1 (- horizon (long (or created 0)))))))

#?(:clj
   (defn- study-plan
     "The dated day-by-day plan §1.2 asks for: one row per day from today to
      the target.

      Pre : `pages-remaining` >= 0, `horizon` >= 0, `load` is the derived daily
            review histogram.
      Post: a vector of {:day :date :weekday :pages :reviews :cumulative},
            one per day, `:date` an ISO string in the server's zone. The
            `:pages` column sums to `pages-remaining` exactly, and
            `:cumulative` is its running total, so the last row always reads
            the whole remainder.
      Inv : `:pages` comes from `schedule-forecast/daily-page-plan`, the same
            fn `today-plan` reads, so row 0 and the Today tile cannot drift."
     [pages-remaining horizon load]
     (let [pages (fc/daily-page-plan pages-remaining horizon)
           today (java.time.LocalDate/now)]
       (first
         (reduce (fn [[acc run] d]
                   (let [p (long (nth pages d))
                         run' (+ run p)
                         date (.plusDays today d)]
                     [(conj acc {:day d
                                 :date (str date)
                                 :weekday (.getDisplayName (.getDayOfWeek date)
                                            java.time.format.TextStyle/SHORT
                                            java.util.Locale/ENGLISH)
                                 :pages p
                                 :reviews (double (get load d 0.0))
                                 :cumulative run'})
                      run']))
           [[] 0] (range (count pages)))))))

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
   Post: a mismatch between the acquisition's fingerprint and the cache's
         discards the cached state, because `db/get-anki-card-states` filters
         on the fingerprint and returns nothing to continue from (§3.3).
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
         (let [{:keys [deck deck-config fingerprint infos revlog incremental?]} acquisition
               sched (sf/scheduler-from-deck-config deck-config)
               cutoff (sa/ignore-before-ms (:ignoreRevlogsBeforeDate deck-config))
               cached (when incremental?
                        (db/get-anki-card-states user-id
                          (distinct (keep :note infos)) fingerprint))
               rows (sa/card-rows sched infos revlog
                      {:cutoff-ms cutoff
                       :fingerprint fingerprint
                       :cached (or cached {})
                       :incremental? (boolean (and incremental? (seq cached)))})
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
                           " cards=" (count db-rows)
                           " incremental=" (boolean incremental?)))
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
       (let [origin-ms (start-of-today-ms)
             root (:study_schedules/root_topic_id row)
             excluded (set (:study_schedules/excluded_pages row))
             capacity (long (:study_schedules/daily_capacity row))
             horizon (min max-horizon-days
                       (days-until (:study_schedules/target_date row)))
             all-pages (db/get-schedule-pages user-id root
                         (:study_schedules/first_page row)
                         (:study_schedules/last_page row))
             pages (mapv #(assoc % :excluded? (contains? excluded (:page-number %)))
                     all-pages)
             cfg-row (db/get-anki-deck-config user-id)
             deck-config (:anki_deck_configs/config cfg-row)
             base {:schedule row
                   :horizon-days horizon
                   :cached-at (db/anki-state-cached-at user-id)
                   :pages pages}]
         (cond
           (nil? deck-config)
           ;; Anki has never been read, so there is no R and no review load.
           ;; The PAGE plan needs neither, and it is the half of §1.2 the user
           ;; can act on today — so it renders here rather than waiting on a
           ;; connection they may not have.
           (let [prog (fc/document-progress (fc/page-verdicts pages [] 0.9))]
             (assoc base :status :no-config
               :progress prog
               :plan (study-plan (- (:total prog) (:known prog)) horizon {})))

           :else
           (let [theta (double (or (:study_schedules/theta row)
                                 (:desiredRetention deck-config) 0.9))
                 scheduler (sf/scheduler-from-deck-config deck-config theta)
                 ;; Lever I5: an excluded page's cards are withheld, so the
                 ;; page falls to done-with-no-cards and its cost lands in the
                 ;; exposure figure rather than out of sight (§6.6.4.1).
                 excluded-page-ids (set (map :page-id (filter :excluded? pages)))
                 cards (remove #(contains? excluded-page-ids (:page-id %))
                         (db/get-schedule-cards user-id root))
                 states (db/get-anki-card-states user-id
                          (distinct (map :anki-note-id cards))
                          (:anki_deck_configs/fingerprint cfg-row))]
             (if (empty? states)
               (let [prog (fc/document-progress (fc/page-verdicts pages [] theta))]
                 (assoc base :status :no-cards :theta theta
                   :progress prog
                   :plan (study-plan (- (:total prog) (:known prog)) horizon {})))
               (let [pcards (projection-cards states cards origin-ms capacity)
                     [known-now verdicts-now _] (known-at scheduler pcards pages theta 0 {})
                     [known-end verdicts-end projected]
                     (known-at scheduler pcards pages theta horizon {})
                     load (fc/daily-review-load projected)
                     peak (fc/peak-daily-load load)
                     total (count pages)
                     progress (fc/document-progress verdicts-now)
                     projected-progress (fc/document-progress verdicts-end)
                     base-feas (fc/feasibility
                                 {:projected-known known-end
                                  :required-pages total
                                  :peak-load peak
                                  :observed-capacity capacity})
                     ;; §6.6.1 — the date lever names a date, and only when the
                     ;; plan needs one. The search is nine more projections of
                     ;; the whole document, so a feasible plan never pays it.
                     ;;
                     ;; It searches against the REACHABLE count, not the whole
                     ;; range. A page with no cards and no done mark cannot
                     ;; become known by reviewing — the projection models
                     ;; reviews, not future carding — so searching for the full
                     ;; range returns nil for every partially-read document, and
                     ;; the lever would say "never" in the one case it exists
                     ;; for. The reachable set is what moving the date can in
                     ;; fact deliver, and `:reachable` is reported beside it so
                     ;; the surface never presents that date as "finished".
                     reachable (count (remove #(= :untouched (:reason %)) verdicts-end))
                     feas (if (:feasible? base-feas)
                            base-feas
                            (assoc base-feas
                              :reachable reachable
                              :unreachable (- total reachable)
                              :earliest-day
                              (when (pos? reachable)
                                (fc/earliest-feasible-day
                                  (fn [d] (first (known-at scheduler pcards pages theta d
                                                   {:epsilon suggestion-epsilon})))
                                  reachable max-horizon-days))))
                     span (schedule-span row origin-ms)]
                 (assoc base
                   :status :ok
                   :theta theta
                   :deck (:anki_deck_configs/deck cfg-row)
                   :capped? (boolean (some #(get-in % [:projected :capped?]) projected))
                   :progress progress
                   :projected projected-progress
                   :verdicts verdicts-end
                   :pace (fc/pace {:known-now known-now
                                   :total-pages total
                                   :start-known (:study_schedules/start_known row)
                                   :elapsed-days (- span horizon)
                                   :total-days span})
                   :daily-load load
                   :peak-load peak
                   ;; §1.2 — the plan the user actually follows. Pages come
                   ;; from the straight line, reviews from the projection.
                   :plan (study-plan (- total (:known progress)) horizon load)
                   :today (fc/today-plan {:pages-remaining (- total (:known progress))
                                          :days-remaining horizon
                                          :daily-load load})
                   :feasibility feas))))))
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

   Post: {:note-ids [long …] :high-water long}. The note ids span EVERY active
         schedule's document, so one acquisition serves the whole tab rather
         than one per schedule.
   Post: `:high-water` is 0 whenever no config is cached — a config change
         invalidates the mark along with the state (§3.3), and 0 is what makes
         the next pull a full one."
  [_refresh user-id]
  #?(:clj
     (let [roots (distinct (map :study_schedules/root_topic_id
                             (db/list-study-schedules user-id)))
           note-ids (distinct (mapcat #(map :anki-note-id
                                         (db/get-schedule-cards user-id %))
                                roots))
           fp (:anki_deck_configs/fingerprint (db/get-anki-deck-config user-id))]
       {:note-ids (vec note-ids)
        :high-water (if fp (db/anki-state-high-water user-id fp) 0)})
     :cljs {:note-ids [] :high-water 0}))

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
   Post: `start_known` is the known count TODAY, measured by the SAME rule the
         pace figure of §6.2 compares against, so a schedule opened the day it
         was made reads `on plan`.

   The baseline is measured by running the forecast once at horizon 0, after
   the row exists. Counting DONE pages instead is the obvious shortcut and it
   is wrong: a done page whose cards fail the maturity gate is done but not
   known, so the baseline would exceed the known count and a brand-new
   schedule would open reporting itself several pages behind."
  [user-id attrs]
  #?(:clj
     (try
       (let [row (db/create-study-schedule! user-id (assoc attrs :start-known 0))
             id (:study_schedules/id row)
             known-now (or (get-in (forecast 0 user-id id) [:progress :known]) 0)]
         (db/set-schedule-start-known! user-id id known-now)
         (cmd/bump-channels! user-id #{:schedule-mutations})
         {:ok true :id id :start-known known-now})
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
