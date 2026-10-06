(ns freememo.activity-registry
  "What each kind of activity IS: its label, its tier, what its `subject_id`
   means, how its `work_key` is composed, and — for server-owned work — which
   per-user atom carries its live state (plans/activity-monitor.md).

   Requires only `freememo.user-state`, and that is the point. Every server-owned
   owner already publishes its in-flight state as a per-user atom, so reading it
   needs the ATOM, never the owning namespace. Reading `:distilling-docs` here
   costs nothing; requiring `freememo.kg-extract` would pull missionary, the
   OpenRouter client and half the graph subsystem onto the reporter's load path
   and invite cycles. The atom shapes are the contract, and `user_state.clj`
   documents them.

   Entries are DATA, not functions. `state-reader` derives the read from an
   entry's own `:state-key`/`:shape`/`:phase`, so nothing is declared twice and
   `:canceller-key` serves both the cancellability flag and `activity/cancel!`.
   The one thing a data entry cannot express — card generation's three-way
   phase, exam grading's progress pair — is an `:entry->state` fn.

   Server-only on purpose. A reader is a function and cannot cross the wire, and
   a CLJ-only def read from client scope kills the WebSocket at mount rather
   than raising a missing-var error. The panel therefore never sees this map:
   the projection attaches `:label` to each activity as plain data, server-side.

   TIERS name HOW A ROW IS KEPT FRESH, which is the only distinction that
   changes behaviour here. Who drives the work is a consequence, not the axis.

   `:server` work publishes its state as a per-user atom and runs in a missionary
   processor with no client involved, so a tab death cannot touch it. The reporter
   PULLS on a timer.

   `:pushed` work has no server atom to pull from, so whatever drives it reports
   to `activity/report!` itself. Every member of this tier is driven by a mounted
   CLIENT component that holds something the server cannot reach — an AnkiConnect
   connection on localhost, or the file being chunk-uploaded.

   Account export and import are `:server` despite looking like the pushed tier:
   an export streams on a Jetty request thread and an import runs in an
   `e/Offload` thread, and both are server threads no browser owns. Publishing
   membership was all they needed — discovery then opens and closes their rows
   and the reporter keeps them alive, where a pushed-tier version would have
   needed a watchdog thread each just to prove itself alive.

   This is also why no `:uploading-videos` mirror exists: that atom was declared
   for a pull the pushed tier does not need, and a mirror would be a second copy
   of state the row already holds.

   A reader returning nil means this process does not know the work. The reporter
   closes it only when this process observed the same row as live. A row owned by
   another process remains untouched and becomes interrupted if its owner stops
   reporting."
  (:require
   [clojure.string :as str]
   [freememo.user-state :as us]))

;; ---------------------------------------------------------------------------
;; Work-key encodings. A work_key is a string because one column has to carry
;; five different owner keys; each type owns BOTH directions of its own
;; encoding, so the composing side and the reading side cannot drift apart.
;; ---------------------------------------------------------------------------

(defn- id->key [id] (str id))
(defn- key->id [work-key] (parse-long work-key))

(defn- doc-page->key
  "A page scan is identified by (document, page): one document has many pages
   scanning independently, and `:scanning-pages` holds the pair itself."
  [doc page]
  (str doc "/" page))

(defn- key->doc-page [work-key]
  (let [[doc page] (str/split work-key #"/")]
    (when (and doc page)
      (let [d (parse-long doc) p (parse-long page)]
        (when (and d p) [d p])))))

;; ---------------------------------------------------------------------------
;; Deriving a reader from a declarative entry
;; ---------------------------------------------------------------------------

(defn- cancellable?
  "Whether this owner currently holds a cancel fn for `owner-key`.

   Read per tick rather than declared statically because it is genuinely
   momentary: the fn is stored when the task is submitted and dissoc'd the
   instant it settles."
  [uid canceller-key owner-key]
  (boolean (and canceller-key (contains? @(us/get-atom uid canceller-key) owner-key))))

(defn- state-reader
  "Build an entry's reader from its own declared fields. Returns nil for a
   pushed-tier entry, which has no `:state-key` and is kept fresh by its driver.

   Post: `(reader uid work-key)` is nil when the owner has forgotten this work,
         else a map carrying at least `:phase`."
  [{:keys [state-key phase canceller-key cancelled-key key-fn entry->state
           unrecoverable?]}]
  (when state-key
    (let [key-fn (or key-fn key->id)]
      (fn [uid work-key]
        (when-some [owner-key (key-fn work-key)]
          (let [state @(us/get-atom uid state-key)]
            ;; `contains?` reads membership for a set and key-presence for a map,
            ;; so both shapes answer "is this work in flight" the same way.
            ;;
            ;; `:cancelled-key` subtracts from that, and every cooperatively
            ;; cancellable owner needs it. Their membership OUTLIVES the work:
            ;; neither a page scan's OCR request nor a card generation's model
            ;; call can be aborted, so the atom still names the work while the
            ;; request drains. Without this subtraction, terminating the row as
            ;; cancelled leaves live-looking membership behind and discovery
            ;; opens a SECOND row — which is what users saw for both types.
            (when (and (contains? state owner-key)
                    (not (and cancelled-key
                           (contains? @(us/get-atom uid cancelled-key) owner-key))))
              (let [base {:cancellable (cancellable? uid canceller-key owner-key)
                          :unrecoverable (boolean unrecoverable?)}]
                (if entry->state
                  (when-some [extra (entry->state (get state owner-key))]
                    (merge base extra))
                  (assoc base :phase phase))))))))))

;; ---------------------------------------------------------------------------
;; The registry
;; ---------------------------------------------------------------------------

(def ^:private entries
  "One entry per activity type.

   :label       human name, attached to each activity by the projection
   :tier        :server (reporter pulls) | :pushed (its driver reports)
   :subject     what `activities.subject_id` holds — :topic or :none. Declared
                once here rather than as a per-row column, so a type cannot
                disagree with itself between rows.
   :work-key    composes the work_key; callers MUST use it rather than build the
                string, so the composing and parsing sides stay in step
   :state-key   server tier — the per-user atom carrying live state
   :phase       the phase to report, for entries with one fixed phase
   :entry->state  for the two owners whose entry decides phase or progress
   :key-fn      work_key -> the key the owner's atom uses (default: parse-long)
   :canceller-key  the per-user atom of missionary cancel fns, when the owner
                has one. Holds cancel FUNCTIONS — not to be confused with
                :cancelled-key, which is why neither is called :cancel-key
   :cancelled-key  the per-user set of work the user abandoned, for an owner
                whose request cannot be aborted and whose membership therefore
                outlives the work. Read as NOT in flight, so a cancelled run is
                not re-discovered as a second activity while it drains.

                Known window, accepted: both owners CLEAR this set when new work
                for the same key is enqueued, so cancelling and then immediately
                re-running the same page lets the still-draining first request
                save after all. The user asked for that content twice in a row,
                so the outcome is an extra result rather than a wrong one, and
                closing it would mean identifying runs individually in a set
                whose whole granularity is per-key
   :unrecoverable?  SERVER TIER ONLY — leaving mid-run strands state the user
                cannot recover. Read by `state-reader`, so a pushed-tier entry
                declaring it would be config nothing reads: those drivers pass
                the flag to `ActivityMirror` themselves, being the only thing
                that knows the moment it becomes true.
   :progress-unit  :count | :bytes — how the panel should READ done/total. A
                unit column would be per row, and a type's unit cannot vary by
                run; without it the panel prints an upload's byte counts raw."
  {:card-gen
   {:label "Generating cards"
    :tier :server :subject :topic :work-key id->key
    :state-key :card-gen-status
    :canceller-key :card-gen-cancellers :cancelled-key :card-gen-cancelled
    ;; `forget-card-gen-item!` leaves the entry behind with `:pending` 0 and its
    ;; `:error` intact, which is exactly what makes the outcome readable after
    ;; the work is gone.
    :error-fn (fn [uid work-key]
                (:error (get @(us/get-atom uid :card-gen-status) (key->id work-key))))
    ;; `:card-gen-status` is keyed by TOPIC and aggregates that topic's runs into
    ;; one `:pending` count, so one activity per topic is the granularity the
    ;; owner already chose. `:pending` counts work OUTSTANDING, not work done,
    ;; and no total is ever known — so this reports no proportion. Rendering
    ;; `:pending` as `done` would make the bar run backwards.
    ;;
    ;; Cancellable while ANY work is outstanding, the running one included. This
    ;; withheld Cancel from a run with nothing queued behind it, reasoning that
    ;; an in-flight LLM call has already committed its credits so the button
    ;; would do nothing. That reasoning was incomplete: the call cannot be
    ;; recalled, but the CARDS can be left uninserted, and not receiving cards
    ;; you asked to abandon is the part still available.
    ;; `generate-and-save!` reads `:card-gen-cancelled` immediately before its
    ;; insert — the same cooperative shape a page scan uses, for the same
    ;; reason: neither request can be aborted.
    ;;
    ;; The phase carries the outstanding count, because this entry AGGREGATES.
    ;; Three generations on one page are one row, and without the count three
    ;; runs and one read identically — which is exactly what a user who ran
    ;; three and saw one activity reported.
    :entry->state (fn [{:keys [active-id pending]}]
                    (let [pending (or pending 0)
                          queued (- pending (if active-id 1 0))]
                      (cond
                        (some? active-id)
                        {:phase (if (pos? queued)
                                  (str "generating, " queued " queued")
                                  "generating")
                         :cancellable true}

                        (pos? pending)
                        {:phase (if (> pending 1) (str pending " queued") "queued")
                         :cancellable true}

                        ;; Entry left behind with nothing outstanding: the owner
                        ;; is done with it even though it has not cleared the key.
                        :else nil)))}

   :page-ocr
   {:label "Scanning page"
    :tier :server :subject :topic :work-key doc-page->key
    :state-key :scanning-pages :phase "scanning"
    ;; The owner key is already the (doc page) pair `work-key` takes, so it needs
    ;; no wrapping — and `doc` is the subject, being the topic the viewer opens.
    :key-fn key->doc-page :owner-key->args identity
    :canceller-key :scan-cancellers :cancelled-key :scan-cancelled
    :error-fn (fn [uid work-key]
                (get @(us/get-atom uid :ocr-errors) (key->doc-page work-key)))}

   :transcribe
   {:label "Transcribing"
    :tier :server :subject :topic :work-key id->key
    :state-key :transcribing-topics :phase "transcribing"}

   :video-process
   {:label "Processing video"
    :tier :server :subject :topic :work-key id->key
    :state-key :processing-videos :phase "processing"}

   :distill-doc
   {:label "Distilling facts"
    :tier :server :subject :topic :work-key id->key
    :state-key :distilling-docs :phase "distilling"
    :canceller-key :distill-cancellers}

   :distill-repo
   {:label "Distilling code"
    :tier :server :subject :topic :work-key id->key
    :state-key :distilling-repos :phase "distilling"
    :canceller-key :repo-distill-cancellers}

   :generate-questions
   {:label "Generating questions"
    :tier :server :subject :topic :work-key id->key
    :state-key :generating-questions :phase "generating"
    :canceller-key :question-cancellers}

   :exam-form-generation
   {:label "Generating exam form"
    :tier :server :subject :none :work-key identity
    :state-key :exam-form-generations :phase "generating"
    :key-fn identity
    :canceller-key :exam-form-generation-cancellers
    :cancelled-key :exam-form-generation-cancelled
    :error-fn (fn [uid work-key]
                (get @(us/get-atom uid :exam-form-generation-errors) work-key))}

   :exam-grade
   {:label "Grading exam"
    :tier :server
    ;; A sitting is not a topic, and the popover has nowhere useful to send a
    ;; user mid-grade — the session page is what they are already looking at.
    :subject :none :work-key id->key
    :state-key :exam-grading
    ;; Determinate: `grade-exam-session!` publishes [graded total] precisely so a
    ;; progress line can render, and one LLM call per answer makes the total
    ;; known before the first call.
    :progress-unit :count
    :entry->state (fn [[graded total]] {:phase "grading" :done graded :total total})}

   ;; ── Pushed tier: no :state-key, because no server atom describes these. ──
   ;; Client-component drivers: the browser holds the file or the AnkiConnect
   ;; connection, so it is the only thing that knows the progress.
   :video-upload
   {:label "Uploading video"
    :tier :pushed
    ;; No topic until the upload lands; the row gets a subject_id then.
    :subject :topic
    ;; The upload RUN id, minted per transfer. Not a chunked-upload session id:
    ;; one run may cover several files and so several sessions.
    :work-key identity
    :progress-unit :bytes}

   ;; The two chunked STAGING uploads. Their subject is :none, unlike a video's:
   ;; the transfer only stages bytes, and the topic does not exist until the user
   ;; confirms the import afterwards.
   :archive-upload
   {:label "Uploading archive"
    :tier :pushed :subject :none :work-key identity
    :progress-unit :bytes}

   :epub-upload
   {:label "Uploading EPUB"
    :tier :pushed :subject :none :work-key identity
    :progress-unit :bytes}

   :anki-reconcile
   {:label "Syncing with Anki"
    :tier :pushed
    ;; A reconcile spans the whole collection — quick-sync's docstring is
    ;; explicit that it has no scope to narrow — so there is one at a time per
    ;; user and nowhere to navigate.
    :subject :none :work-key (constantly "collection")}

   ;; ── Account transfer. SERVER tier, not pushed. ─────────────────────────
   ;; An export streams on a Jetty request thread and an import runs in an
   ;; e/Offload thread; both are server threads that outlive no browser. Once
   ;; they publish membership, discovery opens and closes their rows and the
   ;; reporter keeps them alive — so neither needs a driver of its own, and
   ;; neither needs the watchdog thread a pushed-tier version would have.
   ;;
   ;; One at a time per user, so the set holds the constant `"account"` rather
   ;; than an id: a second concurrent export of the same account would double
   ;; the read load for an identical archive.
   :account-export
   {:label "Exporting account"
    :tier :server :subject :none
    ;; Phase-only, and that is a documented property rather than an omission —
    ;; the response cannot declare its size in advance, so no proportion exists
    ;; to report. Nothing declares that here: a row with nil done/total IS
    ;; indeterminate, and a second declaration could disagree with the data.
    :work-key (constantly "account")
    :state-key :exporting-accounts :phase "exporting"
    :key-fn identity :owner-key->args (constantly [])}

   :account-import
   {:label "Importing account"
    :tier :server :subject :none :work-key (constantly "account")
    :state-key :importing-accounts :phase "importing"
    :key-fn identity :owner-key->args (constantly [])}

   ;; A page fetched by the editor's link Import. SERVER tier for the same
   ;; reason as the two above: it runs on a thread that outlives no browser,
   ;; so discovery opens and closes the row and no driver is needed.
   ;;
   ;; `:subject :none` although the work produces a topic — the topic does not
   ;; exist while the fetch runs, and by the time it does the user has already
   ;; been navigated or toasted. A subject that is null for the row's whole
   ;; life is worse than declaring none.
   ;;
   ;; The member is the URL, so two links to the same page share one row. The
   ;; member IS the work key, so the default `vector` hands it to `identity`
   ;; unchanged — `(constantly [])` here called `identity` with no arguments.
   :url-import
   {:label "Importing page"
    :tier :server :subject :none :work-key identity
    :state-key :importing-urls :phase "fetching"
    :key-fn identity
    ;; Without this a failed fetch closes as a success, because the reporter
    ;; reads outcomes here and nowhere else. `start-url-import!*` records the
    ;; error before membership drops, and clears it on a later success for the
    ;; same url, so the map holds failures only.
    :error-fn (fn [uid work-key]
                (get @(us/get-atom uid :url-import-errors) work-key))}

   ;; A git forge import or update. SERVER tier: it runs on an e/Offload thread
   ;; that no browser owns. The member is `host/owner/name`, so one repository
   ;; has one row at a time, and the value is the phase, which moves while the
   ;; row stays open. A staged import is two rows in history — the download, then
   ;; the import — because the picker wait between them is not work.
   ;; `owner-key->args` stays the default: the member IS the work key.
   :repo-import
   {:label "Importing repository"
    :tier :server :subject :none :work-key identity
    :state-key :importing-repos :key-fn identity
    :entry->state (fn [phase] {:phase phase})
    ;; Cooperative: the run reads the mark per archive buffer and per insert
    ;; batch (plans/repo-import-progress.md §1).
    :canceller-key :repo-import-cancellers :cancelled-key :repo-import-cancelled
    ;; Without this a failed download closes as a success (§4.3 3.8).
    :error-fn (fn [uid work-key]
                (get @(us/get-atom uid :repo-import-errors) work-key))}

   ;; A commit-range walkthrough being grouped (plans/commit-range-walkthrough.md
   ;; §6.5.4). SERVER tier: a future no browser owns runs it. The member is the
   ;; walkthrough id, and the value its phase. Cooperative cancel: the
   ;; canceller marks the walkthrough failed at once, and the thread reads the
   ;; mark before each model call and before its write (§6.5.5).
   :walkthrough-prepare
   {:label "Preparing walkthrough"
    :tier :server :subject :none :work-key id->key
    :state-key :preparing-walkthroughs
    :entry->state (fn [phase] {:phase phase})
    :canceller-key :walkthrough-cancellers :cancelled-key :walkthrough-cancelled
    :error-fn (fn [uid work-key]
                (get @(us/get-atom uid :walkthrough-errors) (key->id work-key)))}})

(def registry
  "`entries`, with each server-tier entry's `:read` derived from its own data."
  (reduce-kv (fn [m t entry]
               (assoc m t (cond-> entry
                            (:state-key entry) (assoc :read (state-reader entry)))))
    {} entries))

(defn activity-type
  "Registry entry for `type`, or nil. `type` may be a keyword or the string the
   `activities.type` column holds."
  [type]
  (get registry (if (keyword? type) type (keyword type))))

(defn label
  "Human name for `type`, falling back to the raw type so an unregistered row
   still renders as something rather than blank."
  [type]
  (or (:label (activity-type type)) (name (if (keyword? type) type (keyword type)))))

(defn progress-unit
  "How the panel should read this type's done/total — :count, :bytes, or nil
   when the type reports no proportion."
  [type]
  (:progress-unit (activity-type type)))

;; ---------------------------------------------------------------------------
;; Discovery. The reporter opens and closes server-tier rows itself, reading the
;; owners' atoms — so no owner namespace gains a `start!`/`terminate!` call, and
;; a type that already publishes its in-flight state gets an activity for free.
;; ---------------------------------------------------------------------------

(defn live-work
  "Every piece of `type`'s work currently in flight for `uid`, as
   `[{:work-key :subject-id} …]`.

   Reads the owner's atom directly: its KEYS are the work in flight, which is the
   same fact `read-state` consults one key at a time. `contains?` and `keys` agree
   for both shapes — a set of ids and a map of id -> entry.

   Pre : `type` is a registered server-tier type.
   Post: one entry per in-flight item; empty when the owner has none."
  [type uid]
  (let [{:keys [state-key work-key owner-key->args subject]} (activity-type type)]
    (when state-key
      (let [->args (or owner-key->args vector)
            state @(us/get-atom uid state-key)
            ;; `keys` is nil on a set — a set IS its own key sequence, and these
            ;; owners publish both shapes.
            owner-keys (if (set? state) (seq state) (keys state))]
        (for [owner-key owner-keys
              :let [args (->args owner-key)]]
          {:work-key (apply work-key args)
           ;; The subject is the first argument by construction: a topic id for
           ;; the id-keyed types, the DOCUMENT for a page scan (never the page,
           ;; which is not a topic the viewer can open).
           :subject-id (when (= :topic subject) (first args))})))))

(defn work-error
  "The owner's recorded error for one piece of work, or nil.

   Only two owners publish an error where anything else can read it. The rest
   report failure as a toast and nothing more, so a run of theirs that failed
   terminates as 'success' in history — a known limitation, not an oversight.
   Closing it for a type means giving that owner an `:error-fn` here."
  [type uid work-key]
  (when-some [{:keys [error-fn]} (activity-type type)]
    (when error-fn (error-fn uid work-key))))

(defn server-tier-types
  "The type strings the reporter is responsible for ticking. Pushed-tier rows are
   excluded because no server atom describes them — their driver reports instead,
   so ticking them here would advance `updated_at` on evidence of nothing."
  []
  (into #{} (keep (fn [[t {:keys [tier read]}]]
                    (when (and (= :server tier) read) (name t))))
    registry))

(defn read-state
  "Live state of one activity, or nil when its owner has forgotten it.

   Pre : `type` is registered and server-tier (violation = caller bug — the
         reporter filters by `server-tier-types`).
   Post: nil, or a map carrying at least `:phase`.
   Invariant: never terminates anything. A nil value proves only that this
         process has no owner state. The reporter must establish local ownership
         before it assigns a terminal outcome."
  [type uid work-key]
  (when-some [read (:read (activity-type type))]
    (read uid work-key)))

(defn owner-cancel-fn
  "The owner's missionary cancel fn for this work, or nil.

   Pulled-tier cancellation needs no owner namespace for the same reason reading
   does not: the fn itself sits in a per-user atom. Nil means not cancellable
   right now — the task already settled, or this type never had a canceller.

   Pre : `type` is registered (violation = caller bug).
   Post: nil, or a 0-arity fn that cancels the task."
  [type uid work-key]
  (when-some [{:keys [canceller-key key-fn]} (activity-type type)]
    (when canceller-key
      (when-some [owner-key ((or key-fn key->id) work-key)]
        (get @(us/get-atom uid canceller-key) owner-key)))))
