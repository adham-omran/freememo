(ns freememo.anki-reconcile-server
  "Server half of Anki reconcile — planning, and the writes that follow.

   The split with the client is chosen for payload size. The server owns every
   local digest and both baselines, so it can classify the whole collection
   alone; the client owns the AnkiConnect connection, so it computes the Anki
   digests. What crosses the wire is one digest per note in each direction —
   46 bytes per note measured, against 635 for the notes themselves.

   The one invariant every write here upholds:

     an `anki_baseline_digest` is only ever written from bytes READ BACK from
     Anki, never from the payload sent to it.

   Anki normalises HTML on write. A baseline taken from what was sent would
   disagree with what the next run reads, so the card would report an Anki-side
   change forever and, once the local side moved too, become a permanent
   conflict."
  (:require
   [clojure.string :as str]
   [freememo.db :as db]
   [freememo.commands :as commands]
   [freememo.anki-digest :as digest]
   [freememo.anki-reconcile :as rec]
   [freememo.anki-sync-helpers :as helpers]
   [freememo.anki-sync-server :as sync-server]
   [taoensso.telemere :as tel]))

;; ---------------------------------------------------------------------------
;; Planning
;; ---------------------------------------------------------------------------

(defn scan-targets
  "Everything the client must ask Anki about, and how to digest each answer.

   pre : none beyond ownership.
   post: {:note-ids [n …] :kind-by-note {note-id kind}}. Tombstoned notes appear
         in `:note-ids` with no entry in `:kind-by-note` — they have no card left
         to take a kind from, so `anki-digest` reads every field for them, which
         is the conservative choice for a note we only want to delete.

   Deliberately small: ids and kind strings, roughly 20 bytes a note against the
   635 the notes themselves cost. The full notes never reach the server.

   The 2-arity scopes the run to topic `root-id` and its descendants (a branch
   run). Then the scan covers only the notes of cards under the root, and NO
   tombstones. A tombstone is a card that no longer exists, so no topic can be
   named for it: it cannot be shown to lie under the root, and a branch run that
   deleted its Anki note could delete a note outside the root. The full run
   settles tombstones. With a nil `root-id` this is the 1-arity."
  ([user-id] (scan-targets user-id nil))
  ([user-id root-id]
   (let [manifest (db/get-pushed-card-manifest user-id root-id)
         kinds (into {} (map (juxt :flashcards/anki_note_id :flashcards/kind))
                 (db/get-cards-for-reconcile user-id nil root-id))
         tombstone-ids (if (some? root-id)
                         []
                         (mapv :anki_note_tombstones/anki_note_id
                           (db/list-anki-tombstones user-id)))]
     {:note-ids (vec (distinct (concat (map :note-id manifest) tombstone-ids)))
      :kind-by-note (select-keys kinds (map :note-id manifest))})))

(defn plan
  "Classify every card and tombstone `user-id` owns.

   pre : `anki-digest-by-note` is {note-id hex-digest} and `absent-notes` is a
         set of note ids, both from ONE client-side notesInfo batch over
         `db/get-pushed-card-manifest` plus the outstanding tombstones.
   post: id lists only — the card rows never leave the server. Every key is what
         one later phase consumes:

           :write-card-ids     create ∪ push, for `anki-write-bundles`
           :kind-by-card       so `read-back!` can digest by the right fields
           :pull-card-ids      what `apply-pulls!` will write
           :pull-note-ids      which notes the client must send back for it
           :backfill           [{:card-id :note-id}] for `:needs-baseline`
           :conflict-card-ids  the exact set `set-conflicts!` will store
           :note-ids-to-delete tombstones whose note must go
           :note-ids-to-drop   tombstones already satisfied

   `:aborted?` true means the reply did not describe this collection. The caller
   MUST apply nothing; a partial run would still delete what it reached.

   The 4-arity takes `root-id` and plans a branch run: only the cards under that
   topic and its descendants, and no tombstones (see `scan-targets`). So
   `:note-ids-to-delete` and `:note-ids-to-drop` are empty, and no note outside
   the root can be named for deletion. `:aborted?` is then judged over the
   branch's synced cards alone; a small branch gives `bulk-absence?` a small
   sample, and its floor of three absent notes stands. With a nil `root-id` this
   is the 3-arity."
  ([user-id anki-digest-by-note absent-notes]
   (plan user-id anki-digest-by-note absent-notes nil))
  ([user-id anki-digest-by-note absent-notes root-id]
   (let [manifest {:cards (db/get-cards-for-reconcile user-id nil root-id)
                   :tombstones (if (some? root-id) [] (db/list-anki-tombstones user-id))}
         {:keys [cards tombstones counts aborted?]}
         (rec/plan-run manifest anki-digest-by-note (set absent-notes))
         by-outcome (group-by :outcome cards)
         tomb-by    (group-by :outcome tombstones)
         writes     (concat (get by-outcome :create) (get by-outcome :push))]
     {:aborted?           aborted?
      :counts             counts
      :report             (rec/report-line counts)
      :write-card-ids     (mapv :card-id writes)
      :kind-by-card       (into {} (map (juxt :card-id :kind)) writes)
      :pull-card-ids      (mapv :card-id (get by-outcome :pull))
      :pull-note-ids      (mapv :note-id (get by-outcome :pull))
      :backfill           (mapv #(select-keys % [:card-id :note-id])
                            (get by-outcome :needs-baseline))
      :conflict-card-ids  (mapv :card-id (get by-outcome :conflict))
      :note-ids-to-delete (mapv :note-id (get tomb-by :delete-anki))
      :note-ids-to-drop   (mapv :note-id (get tomb-by :drop-tombstone))})))

;; ---------------------------------------------------------------------------
;; Writes
;; ---------------------------------------------------------------------------

(defn- fresh-local-digests
  "Local digests for `card-ids` as the rows stand NOW, in one query.

   Re-read rather than carried over from the plan: the plan plus the Anki round
   trip is a window in which another tab can edit a card, and stamping a
   baseline from a stale row would hide that edit from every later run."
  [user-id card-ids]
  (into {}
    (map (juxt :flashcards/id digest/local-digest))
    (db/get-cards-for-reconcile user-id (vec card-ids))))

(defn record-anki-writes!
  "Stamp baselines after the client created or updated notes in Anki.

   pre : `entries` = [{:card-id n :note-id n :anki-digest hex :anki-mod n?} …],
         where `:anki-digest` was computed from a notesInfo read taken AFTER the
         write — see the namespace invariant.
   post: each card holds its note id and both baselines, written together.
         Returns {:success true :count n}.

   Deliberately NOT in the same transaction as
   `anki-sync-server/anki-write-bundles`' settings-snapshot refresh: that runs
   before the client sends, and a WebSocket round trip to Anki sits between the
   two. The ordering is what carries the guarantee — see the comment there."
  [user-id entries]
  (try
    (let [locals (fresh-local-digests user-id (map :card-id entries))]
      (db/set-anki-note-ids user-id
        (into [] (keep (fn [{:keys [card-id note-id]}]
                         (when note-id [card-id note-id])))
          entries))
      (db/set-card-baselines! user-id
        (mapv (fn [{:keys [card-id anki-digest anki-mod]}]
                {:card-id card-id
                 :anki-digest anki-digest
                 :local-digest (get locals card-id)
                 :anki-mod anki-mod})
          entries))
      (commands/bump! user-id :anki-sync)
      {:success true :count (count entries)})
    (catch Exception e
      (tel/error! {:id ::record-anki-writes!} e)
      {:success false :error (.getMessage e)})))

(defn apply-pulls!
  "Write Anki's content into the local cards, then stamp both baselines.

   pre : `card-ids` are the plan's `:pull` decisions. `notes-by-id` holds the
         notesInfo note for each of those cards' notes — the client sends only
         these, not the whole batch. `digests` is the scan's digest map.
   post: content applied, baselines stamped from the row as written.
         Returns {:success true :count n}.

   The update is built HERE, not on the client, so `note->pull-update` runs once
   for the whole app. `sync-server/sanitized-pull-update` is the last step before
   the write, for the reason it documents: `db/update-flashcard!` is a bare UPDATE
   with no sanitisation, and pull is the only path by which HTML this app did not
   author reaches `flashcards`.

   A card whose tracked fields turn out not to differ is skipped entirely,
   baseline included. Stamping one would record agreement with content this run
   never compared."
  [user-id card-ids notes-by-id digests]
  (try
    (let [cards (db/get-cards-for-reconcile user-id (vec card-ids))
          applied
          (into []
            (keep (fn [card]
                    (let [nid (:flashcards/anki_note_id card)
                          note (get notes-by-id nid)
                          upd (some-> (when note
                                        (helpers/note->pull-update card (:fields note)))
                                sync-server/sanitized-pull-update)]
                      (when upd
                        (let [{:keys [question answer cloze io-fields]} upd]
                          (if (seq io-fields)
                            (db/merge-flashcard-io-fields! user-id (:flashcards/id card) io-fields)
                            (let [flds (cond-> {}
                                         question (assoc :question question)
                                         answer   (assoc :answer answer)
                                         cloze    (assoc :cloze cloze))]
                              (when (seq flds)
                                (db/update-flashcard! user-id (:flashcards/id card) flds))))
                          {:card-id (:flashcards/id card) :anki-digest (get digests nid)})))))
            cards)
          locals (fresh-local-digests user-id (map :card-id applied))]
      (db/set-card-baselines! user-id
        (mapv (fn [{:keys [card-id anki-digest]}]
                {:card-id card-id :anki-digest anki-digest
                 :local-digest (get locals card-id)})
          applied))
      (commands/bump! user-id :anki-sync)
      {:success true :count (count applied)})
    (catch Exception e
      (tel/error! {:id ::apply-pulls!} e)
      {:success false :error (.getMessage e)})))

(defn backfill-baselines!
  "Give `:needs-baseline` cards a baseline from the current state of both sides.

   pre : `entries` = [{:card-id n :anki-digest hex} …] for cards whose note Anki
         DID report. A card whose note is absent is not backfillable — it
         classifies `:create` and gets its baseline from the write.
   post: both baselines set. Returns {:success true :count n}.

   This accepts whatever Anki holds right now as the truth. Measured on the
   production account before the first run: 56 of 56 comparable cards whose
   `mod` had moved held identical content, so the backfill discards nothing
   there. Other accounts were never measured."
  [user-id entries]
  (try
    (let [locals (fresh-local-digests user-id (map :card-id entries))]
      (db/set-card-baselines! user-id
        (mapv (fn [{:keys [card-id anki-digest]}]
                {:card-id card-id
                 :anki-digest anki-digest
                 :local-digest (get locals card-id)})
          entries))
      (tel/log! {:level :info :id ::backfill-baselines!
                 :data {:user-id user-id :count (count entries)}}
        "reconcile baselines backfilled")
      {:success true :count (count entries)})
    (catch Exception e
      (tel/error! {:id ::backfill-baselines!} e)
      {:success false :error (.getMessage e)})))

(defn set-conflicts!
  "Make the stored conflict set exactly `conflict-card-ids`.

   pre : the ids come from ONE run's plan, over the user's whole collection —
         this clears the flag on every card not named, so a partial list would
         silently forget conflicts the run did not look at. A branch run passes
         its `root-id` for exactly that reason: the list is complete only for
         the subtree, and the clearing stops there.
   post: those cards carry `anki_conflict_at`, no others do (no others under
         `root-id`, when given).
         Returns {:success true :count n}."
  ([user-id conflict-card-ids] (set-conflicts! user-id conflict-card-ids nil))
  ([user-id conflict-card-ids root-id]
   (try
     (db/set-anki-conflicts! user-id conflict-card-ids root-id)
     (commands/bump! user-id :anki-sync)
     {:success true :count (count conflict-card-ids)}
     (catch Exception e
       (tel/error! {:id ::set-conflicts!} e)
       {:success false :error (.getMessage e)}))))

(defn finish-tombstones!
  "Forget tombstones whose notes are gone from Anki.
   pre : `note-ids` were deleted by this run, or were already absent.
   post: the rows are removed. Returns {:success true :count n}."
  [user-id note-ids]
  (try
    (let [n (db/drop-anki-tombstones! user-id note-ids)]
      (when (pos? n) (commands/bump! user-id :anki-sync))
      {:success true :count n})
    (catch Exception e
      (tel/error! {:id ::finish-tombstones!} e)
      {:success false :error (.getMessage e)})))

(defn conflicts-for-resolution
  "{card-id {:note-id n :kind s}} for every conflicted card `user-id` owns.

   One value serves all three jobs the resolution UI has: deciding whether a
   selected card is conflicted at all, finding the note whose current digest the
   FreeMemo side needs, and telling the scan WHICH FIELDS that digest covers.

   The kind is not optional. `anki-digest` reads a kind's content fields and
   falls back to reading every field for an unknown one, so a resolution that
   scanned without kinds would store a baseline the next run cannot reproduce —
   and the card would come straight back as a conflict."
  [user-id]
  (let [conflicted (set (db/list-anki-conflicts user-id))]
    (into {}
      (comp (filter #(contains? conflicted (:flashcards/id %)))
        (filter :flashcards/anki_note_id)
        (map (juxt :flashcards/id
               (fn [c] {:note-id (:flashcards/anki_note_id c)
                        :kind (:flashcards/kind c)}))))
      (db/get-cards-for-reconcile user-id))))

(defn resolve-conflicts!
  "Settle conflicts by declaring one side the winner.

   pre : `side` is `:freememo` or `:anki`. `card-ids` are conflicted cards.
         `anki-digests` is {note-id hex} read from Anki JUST NOW — required for
         `:freememo`, unused for `:anki`.
   post: the flag is cleared and ONE baseline is moved, so the next run carries
         the choice out. Returns {:success bool :count n}.

   Nothing is written to Anki here, and no content is overwritten. A conflict is
   `anki-now ≠ anki-baseline AND local-now ≠ local-baseline`; resolving it means
   retiring one of those two inequalities:

     :freememo  anki_baseline := Anki's CURRENT digest. Anki then reads
                unchanged, FreeMemo still reads changed, and the next run
                classifies `:push`.
     :anki      local_baseline := the row's CURRENT digest. FreeMemo then reads
                unchanged, Anki still reads changed, and the next run
                classifies `:pull`.

   Doing it this way rather than writing content immediately keeps ONE code path
   that moves data — the run — so a resolution cannot apply an edit by a rule the
   run does not share. `:anki` needs no Anki connection at all."
  [user-id card-ids side anki-digests]
  (try
    (let [cards (db/get-cards-for-reconcile user-id (vec card-ids))
          entries
          (into []
            (keep (fn [card]
                    (let [cid (:flashcards/id card)
                          nid (:flashcards/anki_note_id card)]
                      (case side
                        :anki {:card-id cid
                               :anki-digest (:flashcards/anki_baseline_digest card)
                               :local-digest (digest/local-digest card)}
                        :freememo (when-let [d (get anki-digests nid)]
                                    {:card-id cid
                                     :anki-digest d
                                     :local-digest (:flashcards/local_baseline_digest card)})
                        nil))))
            cards)]
      (db/set-card-baselines! user-id entries)
      (db/clear-anki-conflicts! user-id (mapv :card-id entries))
      (commands/bump! user-id :anki-sync)
      {:success true :count (count entries)})
    (catch Exception e
      (tel/error! {:id ::resolve-conflicts!} e)
      {:success false :error (.getMessage e)})))

(defn- confine-outcome
  "Narrow a branch run's outcome to the cards under `root-id`.

   pre : `outcome` is `finish-run!`'s argument. `root-id` is nil for a full run.
   post: for a full run, `outcome` unchanged. For a branch run, every card id
         list holds only cards `user-id` owns under `root-id`, and
         `:finished-note-ids` is empty.

   The ids come back from the client. The plan already scoped them, but this is
   the write path: a stale tab, a plan from before a re-parent or a hand-built
   call would otherwise stamp a baseline on a card outside the branch, and a
   baseline is the one thing that hides a change. The tombstone list is emptied
   because a branch run plans none (see `scan-targets`)."
  [user-id root-id {:keys [writes pull-card-ids backfill conflict-card-ids] :as outcome}]
  (if (nil? root-id)
    outcome
    (let [inside (set (db/card-ids-in-subtree user-id root-id
                        (concat (map :card-id writes) pull-card-ids
                          (map :card-id backfill) conflict-card-ids)))]
      (assoc outcome
        :writes            (filterv (comp inside :card-id) writes)
        :pull-card-ids     (filterv inside pull-card-ids)
        :backfill          (filterv (comp inside :card-id) backfill)
        :conflict-card-ids (filterv inside conflict-card-ids)
        :finished-note-ids []))))

(defn finish-run!
  "Every server write a run makes, in one call and one order.

   pre : `outcome` =
           {:writes            [{:card-id :note-id :anki-digest :anki-mod} …]
            :pull-card-ids     [n …]
            :pull-notes        {note-id note}
            :digests           {note-id hex}
            :backfill          [{:card-id :note-id} …]
            :conflict-card-ids [n …]
            :finished-note-ids [n …]
            :root-id           n?}
         The caller has already done the Anki-side work and re-read what it
         wrote — see the namespace invariant on where a baseline comes from.
         A `:root-id` marks a branch run: the outcome is confined to cards under
         that topic and its tombstone list is dropped (`confine-outcome`), and
         conflicts are replaced only inside the branch.
   post: {:success bool :report str :counts {…}}.

   The order is load-bearing. Conflicts are stored LAST, because a card this run
   pushed or pulled must not keep a flag raised by the plan that preceded the
   fix. Tombstones are forgotten last for the same reason: a delete that failed
   leaves its note id out of `:finished-note-ids`, so the tombstone survives and
   the next run retries it.

   Best-effort by design (plan item 6.7): each step reports its own failure and
   the rest still run. A failure in one phase must not strand the baselines of
   another, or the next run would re-do work it cannot see was done."
  [user-id {:keys [root-id] :as outcome}]
  (let [{:keys [writes pull-card-ids pull-notes digests backfill
                conflict-card-ids finished-note-ids]}
        (confine-outcome user-id root-id outcome)
        steps [[:writes      #(record-anki-writes! user-id writes)]
               [:pulls       #(apply-pulls! user-id pull-card-ids pull-notes digests)]
               [:backfilled  #(backfill-baselines! user-id
                                (mapv (fn [{:keys [card-id note-id]}]
                                        {:card-id card-id
                                         :anki-digest (get digests note-id)})
                                  backfill))]
               [:conflicts   #(set-conflicts! user-id conflict-card-ids root-id)]
               [:tombstones  #(finish-tombstones! user-id finished-note-ids)]]
        results (reduce (fn [acc [k f]] (assoc acc k (f))) {} steps)
        failures (into {} (remove (comp :success val)) results)]
    (tel/log! {:level :info :id ::finish-run!
               :data {:user-id user-id
                      :root-id root-id
                      :counts (update-vals results :count)
                      :failures (update-vals failures :error)}}
      "reconcile run recorded")
    {:success (empty? failures)
     :counts  (update-vals results #(or (:count %) 0))
     :report  (if (seq failures)
                (str "Sync finished with errors: "
                  (str/join ", " (map (fn [[k v]] (str (name k) " — " (:error v)))
                                   failures)))
                "Sync complete")}))
