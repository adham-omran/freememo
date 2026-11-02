(ns freememo.activity
  "The ONLY writer of the `activities` table — the Activity monitor's durable
   record of one long-running operation (plans/activity-monitor.md).

   Why a single writer. Every write here needs the same three things: the
   `user_id` ownership clause, a statement that cannot leave the terminal CHECK
   transiently violated, and the channel bump that follows a real state change.
   Spreading that across the thirteen owners would be thirteen chances to omit
   the ownership clause — which is how `write-anki-settings-snapshot!` first
   shipped, writing card columns with no ownership clause at all. Owners hand in
   a `work_key` and a progress map; this namespace decides everything else.

   This table is a SNAPSHOT, never the source of truth. The per-user atoms in
   `freememo.user-state` stay authoritative for live state, and `report!` writes
   this behind them on a timer. A missed write is therefore self-healing: the
   next tick corrects it. Nothing may read this table to decide what work to do.

   Three intervals must hold an ordering, and `stale-after-ms` is the one both
   halves of the interruption story read:

     poll (panel, ~5s)  <=  tick (reporter, ~20s)  <  stale-after (60s)

   A tick slower than `stale-after-ms` would mark healthy work interrupted.
   Card generation's single LLM call runs up to 120s with nothing to report
   (`content_toolbar_helpers.cljc`), which is exactly why the reporter ticks on a
   timer rather than on progress change.

   Interruption is never read off a WebSocket close event. Electric reconnects
   transparently and a browser-side upload promise chain survives a reconnect, so
   a close would mark live work dead. Staleness cannot make that mistake: it
   observes only whether anything is still reporting."
  (:require
   [cheshire.core :as json]
   [clojure.edn :as edn]
   [next.jdbc :as jdbc]
   [next.jdbc.result-set :as rs]
   [clojure.string :as str]
   [taoensso.telemere :as tel]
   [freememo.db :as db]
   [freememo.commands :as commands]
   [freememo.desktop-link :as link]
   [freememo.activity-registry :as registry])
  (:import [org.postgresql.util PGobject]))

;; ---------------------------------------------------------------------------
;; Vocabulary
;; ---------------------------------------------------------------------------

(defn- progress-ok?
  "A progress pair the table will accept. `total` with no `done` is meaningless —
   a proportion needs both halves — and the `activities_total_needs_done` CHECK
   rejects it. Asserting here instead blames the caller by name rather than
   surfacing as a constraint violation from inside a SQL statement."
  [done total]
  (and (or (nil? done) (nat-int? done))
       (or (nil? total) (and (nat-int? total) (some? done)))))

(def terminal-outcomes
  "The four ways an activity ends. `outcome` is NULL while it runs; the table's
   `activities_terminal_agrees` CHECK ties that to `terminated_at`.

   `cancelled` and `interrupted` are deliberately distinct: the first is the
   user's decision, the second is work that stopped reporting. Retention keeps
   them for different lengths, and only `interrupted` is ever written by a
   sweep rather than by the owner."
  #{"success" "failure" "cancelled" "interrupted"})

(def stale-after-ms
  "How long an in-flight row may go unreported before it reads as interrupted.

   60s, matching Electric's own `ELECTRIC-CONNECTION-TIMEOUT` (59000ms): past
   that the server has already closed the socket of a client that stopped
   talking, so no browser-owned reporter can still be alive. Read by BOTH the
   projection's read-time derivation and `sweep-interrupt-stale!`, so the fast
   path and the backstop cannot disagree about what stale means."
  60000)

(def retention-ms
  "How long a terminal row survives, by outcome. Pruned by
   `sweep-prune-terminal!`.

   Failures and interruptions outlive successes because they are the rows a
   user comes back to read. A success is a receipt for something that already
   worked."
  {"success"     (* 24 60 60 1000)
   "cancelled"   (* 24 60 60 1000)
   "failure"     (* 30 24 60 60 1000)
   "interrupted" (* 30 24 60 60 1000)})

(defn- bump!
  "Announce a real state change to the panel. Routed through
   `commands/bump-channels!` so the `swap!` stays in the single bump authority
   `freememo.commands`, which asserts the channel is registered.

   NOT called for a plain progress tick: a 20s keepalive per live activity would
   re-query every consumer watching the channel for no state change."
  [user-id]
  (commands/bump-channels! user-id #{:activity-mutations}))

;; ---------------------------------------------------------------------------
;; Failure vocabulary (plans/remove-toasts.md 1.1.2)
;; ---------------------------------------------------------------------------

(def insufficient-credits-text
  "The start of the message `credits/check-cost-billed-balance!` returns when the
   balance is empty. A failure that reaches this namespace as a plain string
   has only its text to say why, so the text is the test for that one shape."
  "Out of credits.")

(defn- cause-chain [^Throwable t]
  (take-while some? (iterate #(.getCause ^Throwable %) t)))

(defn- credits-type?
  "True for an `:type` value that names the credits gate. Matches the name only,
   because `cards` and `kg-llm` each throw their own namespaced keyword and this
   namespace must not require either."
  [t]
  (and (keyword? t) (= "insufficient-credits" (name t))))

(defn error-kind
  "Why a failure happened, in the two cases the banner has an action for.

   Pre : `error` is a string, a result map, an exception, or nil.
   Post: \"insufficient-credits\" when the failure is an empty balance,
         \"needs-account\" when it is a feature that needs a freememo.net account
         (the `link/sign-in-message` case), else nil. Total: never throws.

   Three shapes, because owners report in three ways. A result map is
   `{:insufficient-credits true}` or `{:error-type :insufficient-credits}`. An
   exception is read through its cause chain, since the gate's ex-info is often
   wrapped. A string is read by its text."
  [error]
  (cond
    (nil? error) nil

    (instance? Throwable error)
    (let [chain (cause-chain error)]
      (cond
        (some #(credits-type? (:type (ex-data %))) chain) "insufficient-credits"
        (some #(str/includes? (str (ex-message %)) link/sign-in-message) chain) "needs-account"
        :else nil))

    (map? error)
    (cond
      (or (:insufficient-credits error)
          (credits-type? (:error-type error))) "insufficient-credits"
      (string? (:error error)) (error-kind (:error error))
      :else nil)

    :else
    (let [text (str error)]
      (cond
        (str/includes? text link/sign-in-message) "needs-account"
        (str/starts-with? text insufficient-credits-text) "insufficient-credits"
        :else nil))))

(defn error-message
  "The text to store in `activities.error` for an error value of any of the
   shapes `error-kind` reads. Nil when the value holds no text."
  [error]
  (let [text (cond
               (nil? error) nil
               (instance? Throwable error) (or (ex-message error) (.getName (class error)))
               (map? error) (some-> (:error error) str)
               :else (str error))]
    (when-not (str/blank? text) text)))

(defn- ->jsonb [v]
  (doto (PGobject.)
    (.setType "jsonb")
    (.setValue (json/generate-string v))))

(defn- retry-param
  "A `{:command kw :payload map}` as a JSONB parameter, or nil.

   The payload is stored as an EDN string inside the JSON, because JSON turns
   keyword values into strings: `{:kind :topic}` would replay as
   `{:kind \"topic\"}`. `read-retry` reads it back with `clojure.edn`, so keys
   and values, nested ones included, round-trip. The payload must be EDN data
   (keywords, strings, numbers, collections), which is what a command payload is."
  [retry]
  (when (and (map? retry) (some? (:command retry)))
    (let [command (:command retry)]
      (->jsonb {:command (if-some [ns-part (namespace command)]
                           (str ns-part "/" (name command))
                           (name command))
                :payload-edn (pr-str (:payload retry))}))))

(defn- read-retry [^PGobject v]
  (when v
    (let [m (json/parse-string (.getValue v) true)]
      {:command (keyword (:command m))
       :payload (some-> (:payload-edn m) edn/read-string)})))

;; ---------------------------------------------------------------------------
;; Writes
;; ---------------------------------------------------------------------------

(defn- open-row-id
  "Post: the id of the in-flight row for (`user-id`, `type`, `work-key`), or nil."
  [user-id type work-key]
  (:id (jdbc/execute-one! db/ds
         ["SELECT id FROM activities
            WHERE user_id = ? AND type = ? AND work_key = ?
              AND terminated_at IS NULL"
          user-id (name type) work-key]
         {:builder-fn rs/as-unqualified-maps})))

(defn start!
  "Open an activity, or return the id of the one already in flight.

   `work-key` is the owner's own identity for this piece of work, unique within
   (user, type) while in flight — a topic id for card generation, \"<doc>/<page>\"
   for a page scan, an upload session id, the constant \"collection\" for an Anki
   reconcile (which has no scope to narrow). `subject-id` is a topic to navigate
   to and is nil for the two types with nowhere to go; what it MEANS is declared
   per type in the activity registry, not here.

   **`:cancellable` is for SERVER-tier callers only.** A pushed-tier row's Cancel
   button comes from the driving tab's own registry (`activity-client/tab-owns?`),
   not from this column — so a pushed caller that sends `true` here paints a
   button in every OTHER tab, where `cancel!` can only answer `:client-owned`.
   Sending it also cost the client peer: computing the flag needed the canceller
   FUNCTION on the server, which is unserializable. See
   `plans/anki-sync-client-peer-crash.md`. The `:pre` below assigns the blame.

   Pre : `user-id` is a user; `type`, `work-key` and `phase` are non-blank;
         `subject-id` is nil or a topic that user owns (violation = caller bug);
         `:cancellable` is falsy for a `:pushed`-tier `type`.
   Post: exactly one row with `terminated_at IS NULL` for
         (user-id, type, work-key); returns its id.
   Invariant: idempotent. Two concurrent calls yield ONE row and the same id —
         the partial unique index arbitrates, not a read-then-write here, so
         there is no window between the check and the insert."
  [user-id type work-key {:keys [phase subject-id done total cancellable unrecoverable]}]
  {:pre [(some? user-id)
         (not (str/blank? type))
         (not (str/blank? work-key))
         (not (str/blank? phase))
         (progress-ok? done total)
         (not (and cancellable (= :pushed (:tier (registry/activity-type type)))))]}
  (let [inserted (jdbc/execute-one! db/ds
                   ["INSERT INTO activities
                       (user_id, type, work_key, subject_id, phase, done, total,
                        cancellable, unrecoverable)
                     VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
                     ON CONFLICT (user_id, type, work_key) WHERE terminated_at IS NULL
                     DO NOTHING
                     RETURNING id"
                    user-id type work-key subject-id phase done total
                    (boolean cancellable) (boolean unrecoverable)]
                   {:builder-fn rs/as-unqualified-maps})
        id (or (:id inserted)
             ;; Lost the race, or this work was already in flight. Either way the
             ;; caller wants the live row's id, not a failure.
             (open-row-id user-id type work-key))]
    (when (:id inserted)
      (tel/log! {:level :info :id ::activity-started
                 :data {:user-id user-id :type type :work-key work-key :activity-id id}}
        "activity started")
      (bump! user-id))
    id))

(defn report!
  "Write one adapter reading, and bump the channel only if the phase changed.

   Deciding whether the phase changed belongs here rather than in the reporter:
   this statement already touches the row, so it can return the previous phase
   in the same round trip, and no caller has to remember what it last wrote.

   **`:cancellable` is for SERVER-tier callers only**, for the reasons `start!`
   states. This fn takes an `activity-id` rather than a type, so it cannot check
   the tier and cannot assign the blame — the rule holds by discipline here, and
   by assertion there.

   Pre : `activity-id` is an id; `progress` carries a non-blank `:phase`;
         `:done`/`:total` are nil or non-negative; `:cancellable` is absent for a
         pushed-tier row (violation = caller bug, unchecked).
   Post: `phase`, progress and flags written; `updated_at` advanced to now(),
         which is what keeps the row out of `stale-after-ms`; `terminated_at`
         and `outcome` untouched. Returns :done.
   Invariant: a terminated row is never resurrected — the ownership clause
         includes `terminated_at IS NULL`, so a late tick from a reporter that
         has not noticed the end is a silent no-op, not a reopened activity."
  [user-id activity-id {:keys [phase done total cancellable unrecoverable]}]
  {:pre [(some? activity-id) (not (str/blank? phase)) (progress-ok? done total)]}
  (let [row (jdbc/execute-one! db/ds
              ["WITH prev AS (
                  SELECT id, phase FROM activities
                  WHERE id = ? AND user_id = ? AND terminated_at IS NULL)
                UPDATE activities a
                   SET phase = ?, done = ?, total = ?,
                       cancellable = ?, unrecoverable = ?, updated_at = now()
                  FROM prev
                 WHERE a.id = prev.id
                RETURNING prev.phase AS old_phase"
               activity-id user-id
               phase done total (boolean cancellable) (boolean unrecoverable)]
              {:builder-fn rs/as-unqualified-maps})]
    ;; No FOR UPDATE above: a lost update here costs at most a missed bump, and
    ;; the panel's own poll covers that within one interval. Locking a row on
    ;; every 20s tick would be a heavier price than the thing it prevents.
    (when (and row (not= (:old_phase row) phase))
      (bump! user-id))
    :done))

(def summary-max-chars
  "Longest `summary` stored. A summary is one sentence for one panel row; the
   cap guards against a future type passing something unbounded."
  500)

(defn- bounded-summary [summary]
  (when (some? summary)
    (let [s (str summary)]
      (if (> (count s) summary-max-chars) (subs s 0 summary-max-chars) s))))

(defn terminate!
  "End an activity with `outcome`, unless it has already ended.

   Pre : `outcome` ∈ `terminal-outcomes` (violation = caller bug).
   Post: `outcome` and `terminated_at` set in ONE statement, so
         `activities_terminal_agrees` is never transiently violated. Returns
         :done whether or not this call was the one that ended it.
         `error_kind` is derived from `error` here, never passed in. `:retry`,
         `{:command kw :payload map}`, is kept for the Retry action and is
         written only when given.
   Invariant: FIRST WRITER WINS. `terminated_at IS NULL` guards the update, so
         a staleness promoter cannot overwrite a 'success' that landed
         concurrently with its sweep. This is what makes lazy promotion safe to
         run from a read path."
  [user-id activity-id outcome {:keys [error summary done total retry]}]
  {:pre [(contains? terminal-outcomes outcome) (progress-ok? done total)]}
  (let [row (jdbc/execute! db/ds
              ["UPDATE activities
                   SET outcome = ?, error = ?, error_kind = ?, summary = ?,
                       terminated_at = now(), updated_at = now(),
                       done = COALESCE(?, done), total = COALESCE(?, total),
                       retry = COALESCE(?, retry),
                       cancellable = FALSE, unrecoverable = FALSE
                 WHERE id = ? AND user_id = ? AND terminated_at IS NULL
                RETURNING id, type"
               outcome (error-message error) (error-kind error)
               (bounded-summary summary) done total (retry-param retry)
               activity-id user-id]
              {:builder-fn rs/as-unqualified-maps})
        row (first row)]
    (when row
      (tel/log! {:level :info :id ::activity-terminated
                 :data {:user-id user-id :activity-id activity-id
                        :type (:type row) :outcome outcome}}
        "activity terminated")
      (bump! user-id))
    :done))

(defn newest-ended-row-id
  "The id of the newest ENDED row for one piece of work, or 0 when there is none.

   An owner reads this before it publishes new work, and hands it back to
   `record-summary!`. Every row with a greater id is then the new work's own: a
   row still in flight at that moment has a greater id than every ended row,
   because only one row per work key is in flight at a time, and the new work
   adopts it.

   `type` is the registry key or the string `activities.type` holds."
  [user-id type work-key]
  (:id (jdbc/execute-one! db/ds
         ["SELECT COALESCE(max(id), 0) AS id FROM activities
            WHERE user_id = ? AND type = ? AND work_key = ?
              AND terminated_at IS NOT NULL"
          user-id (name type) work-key]
         {:builder-fn rs/as-unqualified-maps})))

(defn record-summary!
  "Add `summary` to the row of one run, once that row has ended.

   For an owner whose outcome is written before its result is known. A Cancel
   terminates the row at once (`cancel!`), and the run reaches its own report
   only after it drains its last batch, so `terminate!`, first writer wins,
   cannot carry the text.

   Pre : `type` and `work-key` name the work as `start!` received them;
         `after-row-id` is `newest-ended-row-id`'s answer, read before the run
         was published; `summary` is non-blank (violation = caller bug).
   Post: the newest row for (user-id, type, work-key) with an id above
         `after-row-id` holds `summary` iff that row has ended and had no
         summary. Returns :recorded, else :unchanged. A row still in flight is
         left to `terminate!`, which writes the same text from the owner's
         `:summary-fn`. A run that ended before any row was opened for it has
         no row, and nothing is written — never an earlier run's row.
   Invariant: never writes `outcome` or `error`, so the outcome's first writer
         still wins."
  [user-id type work-key after-row-id summary]
  {:pre [(some? type) (not (str/blank? (name type))) (not (str/blank? work-key))
         (nat-int? after-row-id) (not (str/blank? summary))]}
  (let [row (jdbc/execute-one! db/ds
              ["UPDATE activities SET summary = ?
                 WHERE id = (SELECT id FROM activities
                              WHERE user_id = ? AND type = ? AND work_key = ?
                                AND id > ?
                              ORDER BY id DESC LIMIT 1)
                   AND terminated_at IS NOT NULL
                   AND summary IS NULL
                RETURNING id"
               (bounded-summary summary) user-id (name type) work-key after-row-id]
              {:builder-fn rs/as-unqualified-maps})]
    (if row
      (do (bump! user-id) :recorded)
      :unchanged)))

;; ---------------------------------------------------------------------------
;; Reads. All `activities` SQL lives in this namespace, so the table's shape is
;; known in one place — the single-writer property above is about WRITES, and a
;; read elsewhere would just scatter column knowledge.
;; ---------------------------------------------------------------------------

(defn live-rows-for-tick
  "Every in-flight row the reporter is responsible for, across all users.

   Deliberately NOT scoped to one user: the reporter is a process-wide timer, and
   one query per user would be a query per session for work most sessions do not
   have. Ordering is unspecified — each row is ticked independently.

   Pre : `types` is a set of type strings (violation = caller bug).
   Post: `[{:id :user_id :type :work_key} ...]` for rows with
         `terminated_at IS NULL` whose type is in `types`; empty when `types` is."
  [types]
  {:pre [(set? types)]}
  (if (empty? types)
    ;; An empty IN list is a SQL syntax error, and an empty registry is a real
    ;; state during namespace load ordering.
    []
    (jdbc/execute! db/ds
      (into [(str "SELECT id, user_id, type, work_key FROM activities
                    WHERE terminated_at IS NULL
                      AND type IN ("
                (str/join ", " (repeat (count types) "?")) ")")]
        types)
      {:builder-fn rs/as-unqualified-maps})))

;; ---------------------------------------------------------------------------
;; The projection — the one thing the panel reads.
;; ---------------------------------------------------------------------------

(defn- promote-stale-for-user!
  "Mark this user's in-flight rows that stopped reporting as interrupted.

   The FAST half of interruption; `sweep-interrupt-stale!` is the slow backstop
   for users who never open the panel. Scoped to one user because it runs on a
   read path and must not walk the whole table to answer one panel.

   Post: none of this user's rows has `terminated_at IS NULL` and `updated_at`
         older than `stale-after-ms`. Returns the number promoted.
   Invariant: cannot overwrite a real outcome. `terminated_at IS NULL` guards the
         update, so a `success` landing between this statement and the read that
         follows stays a success."
  [user-id]
  (let [promoted (jdbc/execute! db/ds
                   ["UPDATE activities
                        SET outcome = 'interrupted', terminated_at = now(),
                            cancellable = FALSE, unrecoverable = FALSE
                      WHERE user_id = ? AND terminated_at IS NULL
                        AND updated_at < now() - make_interval(secs => ?)
                     RETURNING id"
                    user-id (/ stale-after-ms 1000.0)])]
    (count promoted)))

(def history-window-ms
  "How far back the panel shows finished activities. Shorter than any retention
   window in `retention-ms` on purpose: retention decides what is KEPT for later
   reading, this decides what is worth showing unprompted beside live work."
  (* 24 60 60 1000))

(def history-limit
  "Most finished rows the panel lists. A burst of card generation can produce
   hundreds in a day, and a panel is not a log viewer."
  50)

(defn user-activities
  "Everything the Activity panel shows for `user-id`: live work first, then
   recently finished, newest first.

   Promotes this user's stale rows BEFORE reading, rather than deriving
   interruption per row as it renders. Promoting first means the row's own
   `outcome` is the answer, so what the panel displays and what the table holds
   cannot disagree — and no caller has to remember to apply the same staleness
   rule.

   `:label` is attached here, server-side. The registry holds functions and is
   CLJ-only, so a client that read it would be reading a var that does not exist
   on its peer — which kills the WebSocket at mount rather than raising a
   missing-var error.

   Pre : `user-id` is a user.
   Post: a vector of plain-data maps, safe to send over the wire — no atoms, no
         functions. In-flight rows sort before terminal ones."
  [user-id]
  (let [promoted (promote-stale-for-user! user-id)]
    ;; Bump only when something actually changed, and only for OTHER sessions'
    ;; benefit: this caller is about to receive the promoted rows anyway. The
    ;; bump re-enters this fn once for the caller too, which terminates — the
    ;; second pass finds nothing stale and bumps nothing.
    (when (pos? promoted) (bump! user-id))
    (mapv
      (fn [r]
        ;; The momentary flags are read LIVE for in-flight work, not taken from
        ;; the row. Cancellability is genuinely momentary — the owner's cancel fn
        ;; appears when its task is submitted and is gone the instant it settles
        ;; — so a value written by the last 20s tick is stale by construction.
        ;;
        ;; Not a style choice. A page scan lives 30s, its owner registers the
        ;; canceller AFTER publishing membership, and discovery therefore wrote
        ;; `cancellable = false`: the panel offered no Cancel for a run the
        ;; server could have stopped the whole time. Reading through the registry
        ;; means no owner has to order its two swaps correctly for the button to
        ;; appear, which is an ordering four of them got wrong.
        ;;
        ;; Phase and progress deliberately still come from the row. Those are
        ;; display continuity rather than affordances, and sourcing them here
        ;; would give the panel a second, disagreeing read path.
        (let [live (when (nil? (:outcome r))
                     (try (registry/read-state (:type r) user-id (:work_key r))
                       ;; An adapter can throw on a shape it did not expect. One
                       ;; row must not cost the user the whole panel.
                       (catch Throwable _ nil)))]
          {:id (:id r)
           :type (:type r)
           :label (registry/label (:type r))
           :progress-unit (registry/progress-unit (:type r))
           :phase (:phase r)
           :done (:done r)
           :total (:total r)
           :cancellable (boolean (if live (:cancellable live) (:cancellable r)))
           :unrecoverable (boolean (if live (:unrecoverable live) (:unrecoverable r)))
           :outcome (:outcome r)
           :error (:error r)
           :summary (:summary r)
           :subject-id (:subject_id r)
           :started-at (some-> (:started_at r) .toInstant .toEpochMilli)
           :terminated-at (some-> (:terminated_at r) .toInstant .toEpochMilli)}))
      (jdbc/execute! db/ds
        ;; `work_key` is selected for the live flag read above, never sent to the
        ;; client — it is an owner's internal identity, not something to render.
        ["SELECT id, type, work_key, phase, done, total, cancellable, unrecoverable,
                 outcome, error, summary, subject_id, started_at, terminated_at
            FROM activities
           WHERE user_id = ?
             AND (terminated_at IS NULL
                  OR terminated_at > now() - make_interval(secs => ?))
           ORDER BY (terminated_at IS NULL) DESC, started_at DESC
           LIMIT ?"
         user-id (/ history-window-ms 1000.0) history-limit]
        {:builder-fn rs/as-unqualified-maps}))))

(defn live-count
  "How many activities are running for `user-id` — the nav badge's number.

   Reads the same rows `user-activities` would, so badge and panel cannot
   disagree, but returns only the count so a closed panel costs one integer.
   Does NOT promote: the panel's own read does that, and a badge is refreshed
   often enough that paying for an UPDATE each time would be the wrong trade."
  [user-id]
  (or (:c (jdbc/execute-one! db/ds
            ["SELECT count(*) AS c FROM activities
               WHERE user_id = ? AND terminated_at IS NULL
                 AND updated_at >= now() - make_interval(secs => ?)"
             user-id (/ stale-after-ms 1000.0)]
            {:builder-fn rs/as-unqualified-maps}))
    0))

;; ---------------------------------------------------------------------------
;; Failures the user has not seen (plans/remove-toasts.md 1.1). The banner strip
;; reads these. A failure stays here until the user acknowledges it, however
;; many times the page reloads and in whatever tab.
;; ---------------------------------------------------------------------------

(def failure-error-max-chars
  "Longest `error` that `record-failure!` stores. An exception message can hold
   a whole response body."
  2000)

(def unseen-failures-limit
  "Most unseen failures the strip reads. Past this the oldest stay unread until
   a newer one is dismissed."
  100)

(defn record-failure!
  "Record a failure that has no run of its own: a command, a menu action, an undo.
   The strip shows it until the user acknowledges it.

   Pre : `user-id` is a user. `:error` is a string, a result map or an exception
         (see `error-kind`) that holds non-blank text (violation = caller bug).
         `:what` names the action in a few words (\"Delete card\"). `:subject-id`
         is a topic to open, or nil. `:retry` is `{:command kw :payload map}`
         for the Retry action, or nil. The payload must be JSON data.
   Post: exactly one terminated row of type `action-failure` with outcome
         `failure`, `error`, `error_kind`, `seen_at` NULL, and `:activity-mutations`
         bumped once. Returns the new row id. A `:subject-id` that is not this
         user's topic is stored as NULL, so a stale id cannot lose the failure."
  [user-id {:keys [error what subject-id retry]}]
  {:pre [(some? user-id) (some? (error-message error))]}
  (let [text (error-message error)
        text (if (> (count text) failure-error-max-chars)
               (subs text 0 failure-error-max-chars)
               text)
        row (jdbc/execute-one! db/ds
              ["INSERT INTO activities
                  (user_id, type, work_key, subject_id, phase, outcome, error,
                   error_kind, retry, terminated_at)
                VALUES (?, 'action-failure', ?,
                        (SELECT id FROM topics WHERE id = ? AND user_id = ?),
                        ?, 'failure', ?, ?, ?, now())
                RETURNING id"
               user-id (str (random-uuid)) subject-id user-id
               (if (str/blank? what) "Action" what)
               text (error-kind error) (retry-param retry)]
              {:builder-fn rs/as-unqualified-maps})]
    (tel/log! {:level :info :id ::failure-recorded
               :data {:user-id user-id :activity-id (:id row) :what what
                      :error-kind (error-kind error)}}
      "failure recorded")
    (bump! user-id)
    (:id row)))

(defn unseen-failures
  "The failed rows `user-id` has not acknowledged, newest first.

   Post: only rows with `outcome = 'failure'` AND `seen_at IS NULL`, as plain
         data safe for the wire. `:retry?` says a Retry exists; the stored
         command itself never leaves the server."
  [user-id]
  (mapv
    (fn [r]
      {:id (:id r)
       :type (:type r)
       :label (registry/label (:type r))
       :what (:phase r)
       :error (:error r)
       :error-kind (:error_kind r)
       :subject-id (:subject_id r)
       :retry? (boolean (:retry r))
       :terminated-at (some-> (:terminated_at r) .toInstant .toEpochMilli)})
    (jdbc/execute! db/ds
      ["SELECT id, type, phase, error, error_kind, subject_id,
               (retry IS NOT NULL) AS retry, terminated_at
          FROM activities
         WHERE user_id = ? AND outcome = 'failure' AND seen_at IS NULL
         ORDER BY terminated_at DESC, id DESC
         LIMIT ?"
       user-id unseen-failures-limit]
      {:builder-fn rs/as-unqualified-maps})))

(defn- mark-seen!
  "Set `seen_at` on the unseen failures among `ids` that belong to `user-id`.
   Returns the ids it changed. The ownership clause makes a foreign id a no-op."
  [user-id ids]
  (let [ids (vec (filter some? ids))]
    (if (empty? ids)
      []
      (mapv :id
        (jdbc/execute! db/ds
          (into [(str "UPDATE activities SET seen_at = now()
                        WHERE user_id = ? AND outcome = 'failure' AND seen_at IS NULL
                          AND id IN (" (str/join ", " (repeat (count ids) "?")) ")
                       RETURNING id")
                 user-id]
            ids)
          {:builder-fn rs/as-unqualified-maps})))))

(defn acknowledge!
  "Acknowledge failures: set `seen_at` so they leave the strip.

   Pre : `ids` is a collection of activity ids.
   Post: every id that is this user's unseen failure has `seen_at` set, and
         `:activity-mutations` is bumped once, through the `:acknowledge-failure`
         command, when at least one changed. Another user's id changes nothing.
         Idempotent. Returns :done."
  [user-id ids]
  (when (seq (mark-seen! user-id ids))
    (commands/bump! user-id :acknowledge-failure))
  :done)

(defonce ^:private !retry-handlers (atom {}))

(defn register-retry-handler!
  "Register how a stored retry command runs.

   `f` is `(fn [user-id payload])` and returns the optimistic contract
   `{:ok? bool :error str?}`. It MUST NOT record a failure itself: `retry!`
   does that when `:ok?` is false, so one failed Retry makes one banner.
   Last registration wins. A command with no handler runs through
   `optimistic/run-command-effect!` when it is a registered command."
  [command f]
  (swap! !retry-handlers assoc command f)
  nil)

(defn- run-retry [user-id command payload]
  (if-some [f (get @!retry-handlers command)]
    (f user-id payload)
    (if (commands/command command)
      ((requiring-resolve 'freememo.optimistic/run-command-effect!)
       user-id {:id (random-uuid) :type command :payload payload})
      {:ok? false :error (str "No way to retry " (name command))})))

(defn retry!
  "Run the stored retry command of one failed row.

   Pre : `activity-id` is any id (a foreign or unknown id is not a violation).
   Post: when the row is this user's unseen failure with a stored retry, the row
         is acknowledged in one atomic UPDATE, so a double click dispatches once,
         and the command runs. The strip is told once, after the command ends. A command that returns `{:ok? false}` or throws
         records a new failure with the same retry. Returns the command's result
         map, or :gone when there was nothing to retry. Another user's id
         returns :gone and changes nothing."
  [user-id activity-id]
  (if-some [row (first
                  (jdbc/execute! db/ds
                    ["UPDATE activities SET seen_at = now()
                       WHERE id = ? AND user_id = ? AND outcome = 'failure'
                         AND seen_at IS NULL AND retry IS NOT NULL
                      RETURNING retry, subject_id, phase"
                     activity-id user-id]
                    {:builder-fn rs/as-unqualified-maps}))]
    (let [{:keys [command payload] :as retry} (read-retry (:retry row))
          what (:phase row)]
      (let [result (try
                     (run-retry user-id command payload)
                     (catch Throwable t
                       (tel/error! {:id ::retry-failed
                                    :data {:user-id user-id :activity-id activity-id
                                           :command command}}
                         t)
                       {:ok? false :error (or (error-message t) "Retry failed")}))]
        (when-not (:ok? result)
          (record-failure! user-id {:error (or (:error result) "Retry failed")
                                    :what what
                                    :subject-id (:subject_id row)
                                    :retry retry}))
        ;; Bumped AFTER the command, never before. The strip re-queries on this
        ;; bump and unmounts the Retry button, which cancels its `e/Offload`
        ;; thread. Bumping first would interrupt the command it just started.
        (commands/bump! user-id :retry-failure)
        result))
    :gone))

;; ---------------------------------------------------------------------------
;; Cancellation
;; ---------------------------------------------------------------------------

(defn cancel!
  "Cancel a pulled-tier activity: stop the owner's task, then terminate the row.

   Pushed-tier work is NOT cancellable from here and returns :client-owned. Only
   the browser tab holding the upload or the AnkiConnect connection can stop it,
   so the panel calls that tab's own canceller and the driver terminates its own
   row. A server that pretended otherwise would report cancelled work that is
   still running.

   Ordering is deliberate: cancel the task FIRST, terminate second. The reverse
   would let a `cancel-fn` that throws leave a row reading cancelled over work
   that is still running, which is the worse lie of the two.

   The cost of that order is a narrow race, named here because it is invisible
   at the call site. An owner's cancel callbacks run asynchronously and clear its
   in-flight atom; if the reporter's tick lands between `(cancel-fn)` and the
   `terminate!` below, it sees the work gone and closes the row as 'success'.
   First-writer-wins then keeps that, so a cancelled run can be recorded as a
   successful one. The window is the microseconds between two adjacent
   statements against a 20s tick, and the failure is cosmetic — the work did
   stop — so it is documented rather than locked.

   Pre : `activity-id` belongs to `user-id` (enforced, not assumed — a foreign id
         returns :gone rather than cancelling anything).
   Post: :cancelled — task stopped and row terminated as 'cancelled';
         :not-cancellable — no canceller right now (already settling, or this
         type never had one), row untouched;
         :client-owned — pushed tier, nothing done here;
         :gone — no such in-flight row for this user."
  [user-id activity-id]
  (if-some [row (jdbc/execute-one! db/ds
                  ["SELECT id, type, work_key FROM activities
                     WHERE id = ? AND user_id = ? AND terminated_at IS NULL"
                   activity-id user-id]
                  {:builder-fn rs/as-unqualified-maps})]
    (let [{:keys [type work_key]} row]
      (if (= :pushed (:tier (registry/activity-type type)))
        :client-owned
        (if-some [cancel-fn (registry/owner-cancel-fn type user-id work_key)]
          (do (cancel-fn)
              (terminate! user-id activity-id "cancelled" {})
              (tel/log! {:level :info :id ::activity-cancelled
                         :data {:user-id user-id :activity-id activity-id :type type}}
                "activity cancelled")
              :cancelled)
          :not-cancellable)))
    :gone))

(defn cancel-work!
  "Cancel pulled-tier work from its owner's own UI, where the caller knows the
   work key and not the row.

   A mark alone is not enough: the reporter closes a row whose owner forgot the
   work as 'success', so the row must be terminated here, as `cancel!` does.

   Pre : `type` is a registered pulled-tier type.
   Post: `cancel!`'s result when a row is open. With no row yet, the owner's
         canceller runs and :cancelled is returned — its `:cancelled-key` keeps
         discovery from opening a row. :not-cancellable when neither exists."
  [user-id type work-key]
  (if-some [activity-id (open-row-id user-id type work-key)]
    (cancel! user-id activity-id)
    (if-some [cancel-fn (registry/owner-cancel-fn type user-id work-key)]
      (do (cancel-fn) :cancelled)
      :not-cancellable)))

;; ---------------------------------------------------------------------------
;; Sweeps. Registered into the hourly sweep at namespace load, the same
;; inversion `freememo.storage-meter` uses — so `db.clj` keeps no dependency on
;; this namespace and there is no require cycle.
;; ---------------------------------------------------------------------------

(defn sweep-interrupt-stale!
  "Promote in-flight rows that stopped reporting to 'interrupted'.

   The BACKSTOP, not the primary path. The projection derives interruption at
   read time from the same `stale-after-ms`, so a user looking at the panel sees
   it within one poll; this exists for the rows nobody ever looks at, which
   would otherwise stay non-terminal forever and leave holes in history.

   Set-based on purpose: one UPDATE regardless of row count, so it binds a
   constant number of parameters and cannot approach the 65535 bound that
   multi-row inserts have to respect.

   Post: no row has `terminated_at IS NULL` and `updated_at` older than
         `stale-after-ms`. Returns the number promoted."
  []
  (let [promoted (jdbc/execute! db/ds
                   ["UPDATE activities
                        SET outcome = 'interrupted', terminated_at = now(),
                            cancellable = FALSE, unrecoverable = FALSE
                      WHERE terminated_at IS NULL
                        AND updated_at < now() - make_interval(secs => ?)
                     RETURNING user_id"
                    (/ stale-after-ms 1000.0)]
                   {:builder-fn rs/as-unqualified-maps})
        users (into #{} (map :user_id) promoted)]
    ;; One bump per affected user, not per row — the panel re-queries the whole
    ;; list either way.
    (doseq [uid users] (bump! uid))
    (when (seq promoted)
      (tel/log! {:level :info :id ::interrupted-stale-activities
                 :data {:count (count promoted) :users (count users)}}
        "Promoted stale activities to interrupted"))
    (count promoted)))

(defn sweep-prune-terminal!
  "Delete terminal rows past their outcome's retention window.

   Per-outcome rather than one uniform age: a failure a fortnight old is still
   worth reading and a success from yesterday is not. One statement per outcome,
   each set-based.

   Post: no terminal row is older than `retention-ms` for its outcome. Returns
         the number deleted."
  []
  (reduce
    (fn [total [outcome ms]]
      (+ total
        (or (some-> (jdbc/execute-one! db/ds
                      ["DELETE FROM activities
                         WHERE outcome = ?
                           AND terminated_at < now() - make_interval(secs => ?)"
                       outcome (/ ms 1000.0)])
              :next.jdbc/update-count)
          0)))
    0
    retention-ms))

(db/register-sweep-job! ::interrupt-stale-activities #'sweep-interrupt-stale!)
(db/register-sweep-job! ::prune-terminal-activities #'sweep-prune-terminal!)
