(ns freememo.anki-reconcile
  "Reconcile classification — one pure function, no I/O.

   `classify` decides one card's outcome from four digests and nothing else. It
   reads no clock, no database and no AnkiConnect reply, so every outcome in the
   plan's contract table is reachable from literals in a test.

   Each side is compared ONLY against its own baseline (see `freememo.anki-digest`).
   That is what separates \"Anki changed\" from \"FreeMemo changed\"; comparing the
   two sides against each other, as the shipped sync does, answers neither.

   `plan-run` lifts `classify` over a whole run. It is pure too: it takes the
   server's manifest and the client's digest map and returns decisions, so a run
   can be planned in a test with neither a database nor AnkiConnect."
  (:require [clojure.string :as str]
            [freememo.anki-digest :as digest]
            [freememo.anki-fields :as fields]))

(def outcomes
  "Every value `classify` can return. A caller that dispatches on the result owes
   a branch for each — the set is closed on purpose so a new outcome breaks
   exhaustiveness at the call site rather than falling through silently."
  #{:unchanged :push :pull :conflict :create :delete-local :delete-anki
    :drop-tombstone :needs-baseline})

(defn classify
  "The outcome for one card.

   pre  : `:anki-now`  — hex digest, or `:absent` when notesInfo did not report
                         the note (deleted in Anki, or never created).
          `:local-now` — hex digest, or `:absent` when the row is gone and a
                         tombstone stands in for it.
          `:anki-baseline` / `:local-baseline` — hex digest, or nil when this card
                         has never completed a sync.
   post : a member of `outcomes`.
   inv  : each side's question is answered by that side's OWN baseline, and by
          nothing else. Both sides present asks both questions and so needs both
          baselines. A tombstone asks only \"did Anki's copy move?\", which needs
          only `:anki-baseline`. A missing note asks only \"did the local copy
          move?\", which needs only `:local-baseline`. Requiring both everywhere
          made a half-written baseline drop a tombstone without deleting the Anki
          note — recreating the orphan this design exists to remove.
   inv  : an absent baseline never yields a destructive outcome. Where the
          question cannot be answered, the answer is `:conflict`.

   Outcomes and what each one obliges the caller to do:

     :unchanged       nothing
     :push            write FreeMemo's content to Anki, then re-read and restamp
     :pull            write Anki's content locally, then restamp
     :conflict        apply nothing, mark the card
     :create          add the note, record its id and both baselines
     :delete-local    remove the local card
     :delete-anki     delete the Anki note, then drop the tombstone
     :drop-tombstone  both sides are already gone
     :needs-baseline  a synced card with no baseline — backfill, do not classify

   `:ignore` from the plan's contract table is NOT produced here. A FreeMemo-model
   note with no local card and no tombstone has no card to classify; the run's
   Anki-side scan owns it, and reports it (10 such notes exist in production).

   A one-sided deletion meeting a change on the other side is a conflict rather
   than a delete, so no run can destroy an edit it can see."
  [{:keys [anki-now anki-baseline local-now local-baseline]}]
  (let [anki-gone?  (= :absent anki-now)
        local-gone? (= :absent local-now)]
    (cond
      (and local-gone? anki-gone?) :drop-tombstone

      ;; Tombstone. Only Anki's baseline can say whether Anki's copy moved —
      ;; there is no local side left for a local baseline to describe. With no
      ;; Anki baseline the question is unanswerable, so the note stays.
      local-gone?
      (if (and (some? anki-baseline) (= anki-now anki-baseline))
        :delete-anki
        :conflict)

      ;; The note is gone from Anki. Only the local baseline can say whether the
      ;; local copy moved.
      anki-gone?
      (cond
        (and (nil? anki-baseline) (nil? local-baseline))          :create
        (and (some? local-baseline) (= local-now local-baseline))  :delete-local
        :else                                                      :conflict)

      ;; Both sides present, so both questions are asked.
      (or (nil? anki-baseline) (nil? local-baseline)) :needs-baseline

      :else (let [anki-moved?  (not= anki-now anki-baseline)
                  local-moved? (not= local-now local-baseline)]
              (cond
                (and anki-moved? local-moved?) :conflict
                anki-moved?                    :pull
                local-moved?                   :push
                :else                          :unchanged)))))

(defn apply-kind-limits
  "Downgrade a `:pull` to a `:conflict` for a kind that cannot be pulled.

   pre : `outcome` is a member of `outcomes`; `kind` is a card kind.
   post: `:pull` becomes `:conflict` for the kinds in
         `anki-fields/no-inverse-kinds`. Every other pair passes through
         unchanged.

   Kept separate from `classify` so the digest logic stays free of kind
   knowledge, and so the limit is stated once instead of at each call site.
   `note->pull-update` reads the same set and returns nil for those kinds, so a
   pull that this did not downgrade would apply nothing and silently look
   successful."
  [outcome kind]
  (if (and (= :pull outcome) (not (fields/pullable-kind? kind)))
    :conflict
    outcome))

(def ^:private min-absent-for-guard
  "Below this many absent notes, a majority-absent reply is taken at face value.

   The guard reads a majority as evidence that the reply describes a different
   collection. That inference needs a sample. Without a floor, one synced card
   whose note the user really did delete gives 1 > 0 and the guard blocks that
   user's every future run, with no way past it. Three is the smallest count
   where a majority is more than one card's worth of coincidence.

   The residual risk is stated rather than removed: a collection of one or two
   cards pointed at the wrong profile loses them."
  3)

(defn bulk-absence?
  "True when too much of the collection is missing to trust the reply.

   pre : `synced-count` is how many cards hold an `anki_note_id`;
         `absent-count` is how many of those notesInfo did not report.
   post: true iff at least `min-absent-for-guard` notes are absent AND they are
         more than half of the synced cards.

   Pointing the app at an empty or foreign Anki profile makes every note read as
   absent, which classifies every card `:delete-local` and wipes the collection.
   The shipped overlay documents that wipe as accepted risk. A run that decides
   deletions per card cannot accept it, because it deletes without a further
   click. A caller that sees true MUST apply nothing at all — a partial run would
   still delete the cards it reached."
  [synced-count absent-count]
  (and (pos? synced-count)
    (>= absent-count min-absent-for-guard)
    (> absent-count (quot synced-count 2))))

(defn summarize
  "Counts per outcome, for the run report and the conflict toast.
   pre : `results` is a seq of members of `outcomes`.
   post: a map from every outcome in `results` to its count."
  [results]
  (frequencies results))

;; ---------------------------------------------------------------------------
;; Run planning — classify over a whole manifest. Still pure.
;; ---------------------------------------------------------------------------

(defn- anki-state
  "The Anki-side digest for `note-id`, or `:absent`.
   A card with no note id has no Anki side at all, which is the same observation
   as a note Anki failed to report."
  [note-id anki-digest-by-note absent-notes]
  (if (or (nil? note-id)
        (contains? absent-notes note-id)
        (not (contains? anki-digest-by-note note-id)))
    :absent
    (get anki-digest-by-note note-id)))

(defn card-decision
  "One card's decision.

   pre : `card` is a `db/get-cards-for-reconcile` row — JSONB parsed, both
         baselines present as columns. `anki-digest-by-note` maps note id to the
         digest the client computed from notesInfo. `absent-notes` is the set of
         note ids notesInfo did not report.
   post: {:card-id :note-id :kind :outcome :anki-digest :local-digest}, where
         `:outcome` is a member of `outcomes` already narrowed by
         `apply-kind-limits`, and `:anki-digest` is nil when the note is absent.

   `:local-digest` is carried out so the caller can write it as the new baseline
   without recomputing it — the row it came from may be mutated by an applied
   pull before the baseline write."
  [card anki-digest-by-note absent-notes]
  (let [note-id   (:flashcards/anki_note_id card)
        kind      (:flashcards/kind card)
        anki-now  (anki-state note-id anki-digest-by-note absent-notes)
        local-now (digest/local-digest card)]
    {:card-id      (:flashcards/id card)
     :note-id      note-id
     :kind         kind
     :anki-digest  (when (string? anki-now) anki-now)
     :local-digest local-now
     :outcome      (-> (classify {:anki-now       anki-now
                                  :anki-baseline  (:flashcards/anki_baseline_digest card)
                                  :local-now      local-now
                                  :local-baseline (:flashcards/local_baseline_digest card)})
                     (apply-kind-limits kind))}))

(defn tombstone-decision
  "One tombstone's decision.

   pre : `tombstone` is a `db/list-anki-tombstones` row.
   post: {:note-id :outcome}, where `:outcome` is `:delete-anki`,
         `:drop-tombstone` or `:conflict`.

   The local side is `:absent` by construction — that is what a tombstone means.
   `apply-kind-limits` is not consulted: the kind only limits `:pull`, and a
   tombstone can never produce one."
  [tombstone anki-digest-by-note absent-notes]
  (let [note-id (:anki_note_tombstones/anki_note_id tombstone)]
    {:note-id note-id
     :outcome (classify {:anki-now       (anki-state note-id anki-digest-by-note absent-notes)
                         :anki-baseline  (:anki_note_tombstones/anki_baseline_digest tombstone)
                         :local-now      :absent
                         :local-baseline nil})}))

(defn plan-run
  "Decisions for a whole run.

   pre : `manifest` = {:cards [...] :tombstones [...]} from the server.
         `anki-digest-by-note` and `absent-notes` come from the client's single
         notesInfo batch.
   post: {:cards [...] :tombstones [...] :counts {outcome n} :aborted? bool}.

   `:aborted?` is `bulk-absence?` over the SYNCED cards only. Cards that were
   never pushed have no note for Anki to be missing, so counting them would
   dilute the ratio and let a wrong profile through — a collection of 20 synced
   and 200 unsynced cards would need 111 absent notes to trip a guard that
   should trip at 11.

   When `:aborted?` is true the decisions are still returned, for the report.
   The caller MUST apply none of them."
  [{:keys [cards tombstones]} anki-digest-by-note absent-notes]
  (let [card-decisions (mapv #(card-decision % anki-digest-by-note absent-notes) cards)
        tomb-decisions (mapv #(tombstone-decision % anki-digest-by-note absent-notes) tombstones)
        synced         (filterv :note-id card-decisions)
        absent-synced  (count (remove :anki-digest synced))]
    {:cards      card-decisions
     :tombstones tomb-decisions
     :counts     (summarize (concat (map :outcome card-decisions)
                              (map :outcome tomb-decisions)))
     :aborted?   (bulk-absence? (count synced) absent-synced)}))

(def ^:private report-order
  "Every outcome the run reports, in the order a reader cares about.

   Covers `outcomes` except `:unchanged`, which is the silent majority and would
   bury the rest. `outcomes-are-all-reportable` asserts the coverage, so a new
   outcome cannot be added and then go unmentioned in every run report."
  [[:push "pushed"] [:pull "pulled"] [:create "created"]
   [:delete-anki "deleted in Anki"] [:delete-local "deleted locally"]
   [:conflict "conflicts"] [:drop-tombstone "tombstones cleared"]
   [:needs-baseline "need a baseline"]])

(def reportable-outcomes
  "The outcomes `report-line` can name. `outcomes` minus `:unchanged`."
  (set (map first report-order)))

(defn report-line
  "One-line run summary.
   pre : `counts` is a `summarize` result.
   post: a string naming every non-zero outcome except `:unchanged`, in
         `report-order`, or \"nothing to do\" when none is non-zero."
  [counts]
  (let [parts (for [[k label] report-order
                    :let [n (get counts k 0)]
                    :when (pos? n)]
                (str n " " label))]
    (if (seq parts) (str/join ", " parts) "nothing to do")))
