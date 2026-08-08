(ns freememo.schedule-anki
  "AnkiConnect acquisition for the Study Scheduler
   (plans/study-scheduler-spec.md §2 and §9.4.1).

   Every call here runs in the BROWSER. The FreeMemo server cannot reach the
   user's AnkiConnect — `freememo.anki-sync-helpers/anki-call!` posts to
   127.0.0.1:8765 and is cljs-only — so the browser acquires and ships raw
   rows to the server, which replays them (§9.4.2). Nothing here persists
   anything: raw revlog is never stored (§9.4.3).

   READ-ONLY (§1.5). Six actions, and no others: `apiReflect`, `notesInfo`,
   `cardsInfo`, `getDeckConfig`, `getReviewsOfCards`, `cardReviews`. Nothing in
   this namespace writes to a collection.

   The shapes returned by AnkiConnect differ between the two revlog actions,
   and the difference is load-bearing:

     `getReviewsOfCards(cards)`  → {card-id [{:id :usn :ease :ivl :lastIvl
                                              :factor :time :type} …]}
     `cardReviews(deck, startID)` → [[id cid usn ease ivl lastIvl factor time
                                      type] …]   ← positional ARRAYS, not maps

   `normalize-card-reviews` is what makes the second look like the first.
   Verified against the AnkiConnect source, `plugin/__init__.py`
   `cardReviews` and `getReviewsOfCards`."
  (:require [freememo.schedule-fsrs :as sf]
            [freememo.logging :as log]
            ;; `anki-call!` is cljs-only, so a plain `:refer` fails to resolve
            ;; on the JVM peer and this whole namespace stops loading. The pure
            ;; half — `normalize-card-reviews`, `ignore-before-ms`, `card-rows`
            ;; — runs on the SERVER (§9.4.2), so that failure is not academic.
            #?@(:cljs [[freememo.anki-sync-helpers :refer [anki-call!]]
                       [missionary.core :as m]])))

(def acquisition-states
  "Every state acquiring Anki's memory into the cache can end in, with the
   words the surface shows for it.

   Five come from `acquire!` in the browser. `:ingest-failed` comes from the
   SERVER replay afterwards, and belongs in the same map because the user sees
   one pipeline, not two halves.

   ONE definition. The keys were written out separately in the header chip, its
   icon, and the stale banner, so adding a status compiled clean and fell
   silently to a default in whichever site was forgotten — the same failure
   CLAUDE.md records for topic kinds needing an entry in all four badge
   switches. `schedule_fsrs_test` asserts every render site covers this map.

   `:chip` is the header's short label. `:banner` is §8.1/§8.2's explanation,
   and nil means the state needs none. `:fault?` separates a problem the user
   can act on from a state they are merely IN — an account before its first
   Anki push is not at fault, and a warning icon there reads as breakage.

   The copy lives beside the keys rather than in the view, because this
   namespace decides which key to emit and therefore what each one means."
  {:ok {:chip "Anki read" :fault? false :banner nil}

   :no-cards {:chip "no cards pushed" :fault? false
              :banner "No cards from this document have been pushed to Anki yet."}

   :unreachable {:chip "Anki offline" :fault? true
                 :banner "Anki is not answering. Showing the last forecast."}

   :unsupported {:chip "AnkiConnect too old" :fault? true
                 :banner "This AnkiConnect is too old — it has no getReviewsOfCards."}

   :no-fsrs {:chip "FSRS unavailable" :fault? true
             :banner (str "FSRS is off in Anki, or its deck preset carries no "
                       "21-parameter fsrsParams6.")}

   :no-deck {:chip "cards in no deck" :fault? true
             :banner "None of this document's cards is in an Anki deck."}

   :ingest-failed {:chip "replay failed" :fault? true
                   :banner (str "Anki answered, but replaying its review history "
                             "failed. The forecast below is from the last "
                             "successful read.")}})

(def ^:const reading-chip
  "The chip shown while a pull is in flight. Not a member of
   `acquisition-states`: no outcome carries it, and folding it in would let a
   render site treat 'still working' as a result."
  "reading Anki…")

(def required-actions
  "The actions §2.1 probes for before anything else runs.

   `getReviewsOfCards` is the one that matters: it is absent from API v5, and
   an AnkiConnect without it cannot serve this feature at all. Probing rather
   than calling-and-catching is what makes an old AnkiConnect render the
   refusal of §8.1 instead of an error with no cause in it."
  ["apiReflect" "notesInfo" "cardsInfo" "getDeckConfig"
   "getReviewsOfCards" "cardReviews"])

(def ^:const notes-info-batch
  "§2.2 — note ids per `notesInfo` call. 999 is sqlite's variable cap, which
   AnkiConnect also honors internally, so this bound is about the JSON: the
   response carries every note's full field HTML, and a document's whole note
   set in one response is megabytes for a card-id map."
  999)

;; ---------------------------------------------------------------------------
;; Shape normalization
;; ---------------------------------------------------------------------------

(defn normalize-card-reviews
  "`cardReviews` rows as `getReviewsOfCards` maps, grouped by card id.

   Pre : `rows` is the raw `cardReviews` result — a seq of 9-element positional
         vectors in the column order the action's SQL selects:
         id, cid, usn, ease, ivl, lastIvl, factor, time, type.
   Post: {card-id [{:id :ease :ivl :type} …]}, holding only the fields the
         replay reads. A row of the wrong width is dropped rather than
         mis-indexed — a column order that changed upstream must lose rows
         loudly rather than shift `ease` into `ivl` silently.
   Post: rows for cards outside `keep-card-ids` are dropped. `cardReviews` is
         DECK-scoped, so an incremental pull returns every review in the deck,
         and a deck like `All` holds 38 000 cards this schedule does not track."
  [rows keep-card-ids]
  (let [keep? (set keep-card-ids)]
    (reduce (fn [acc row]
              (if-not (and (sequential? row) (= 9 (count row)))
                acc
                (let [[id cid _usn ease ivl _last _factor _time type] row]
                  (if-not (keep? cid)
                    acc
                    (update acc cid (fnil conj []) {:id id :ease ease :ivl ivl :type type})))))
      {} rows)))

(defn- anki-interval->days
  "Anki's `card.ivl` in days. A NEGATIVE value is seconds, a scheduler-v1
   legacy that survives in old collections; treating it as days would report a
   sub-day learning card as months overdue."
  [ivl]
  (cond
    (nil? ivl) 0.0
    (neg? (double ivl)) (/ (- (double ivl)) 86400.0)
    :else (double ivl)))

(defn ignore-before-ms
  "`ignoreRevlogsBeforeDate` as epoch milliseconds, or 0 (§4.2).

   Anki writes the field as `YYYY-MM-DD`, and `1970-01-01` means no cutoff —
   which is what the canonical user's config carries, so this is a no-op there
   today. It is still read, because the field can change and §3.3 fingerprints
   it.

   Post: 0 for nil, for the empty string, and for anything that does not
         parse. A malformed date MUST NOT discard history: a cutoff nobody
         asked for would silently reset every card's memory state."
  [date-str]
  (if (or (nil? date-str) (not (string? date-str)) (empty? date-str))
    0
    (try
      #?(:clj (-> (java.time.LocalDate/parse date-str)
                (.atStartOfDay java.time.ZoneOffset/UTC)
                .toInstant .toEpochMilli)
         :cljs (let [t (.parse js/Date date-str)]
                 (if (js/isNaN t) 0 (long t))))
      (catch #?(:clj Exception :cljs :default) _ 0))))

(defn- cached->state
  "One cached `anki_card_states` row as a `schedule-fsrs/replay` start state,
   or nil when the row carries no memory state to continue from."
  [row]
  (when (and row (some? (:anki_card_states/stability row)))
    {:state (long (:anki_card_states/state row))
     :step (some-> (:anki_card_states/step row) long)
     :stability (double (:anki_card_states/stability row))
     :difficulty (some-> (:anki_card_states/difficulty row) double)
     :last-review-ms (some-> (:anki_card_states/last_review_at row) inst-ms)
     :interval-days (double (or (:anki_card_states/interval_days row) 0.0))
     :reps (long (or (:anki_card_states/reps row) 0))}))

(defn card-rows
  "Merge `cardsInfo` and the replayed revlog into the rows the cache stores.

   Pre : `infos` is the `cardsInfo` result and `revlog-by-card` is keyed by
         Anki card id. `scheduler` is configured from the deck config. `cached`
         is {anki-card-id row} from `db/get-anki-card-states`, already filtered
         to the current fingerprint, and `incremental?` says whether
         `revlog-by-card` is the whole history or only what followed the mark.
   Post: one row per card in `infos`, carrying derived FSRS state plus the
         anchors the projection needs. A card whose whole revlog is manual
         entries has nil stability and reads as never-reviewed (§5.8.5), never
         as an error.
   Inv : an INCREMENTAL replay continues from the cached state; a full one
         starts fresh. Getting this backwards is silent — every card would
         report the stability of its last few reviews alone.

   The interval comes from ANKI, not from the replay. Anki's `ivl` is the
   interval actually in force — fuzzed, load-balanced, possibly hand-set — and
   it is what decides when the user next sees the card. The replayed stability
   decides every review AFTER that one. Using the replayed interval for the
   first step would forecast a schedule the user is not on.

   The last-review anchor comes from the REVLOG, not from `due`. `cardsInfo`
   reports `due` for a review card in days since the collection was created,
   and no action in the allowed set of §1.5 exposes that origin. `max(revlog.id)`
   is an absolute epoch timestamp and needs no anchor, which is what §2.5's
   cross-check asserts."
  [scheduler infos revlog-by-card
   {:keys [cutoff-ms fingerprint cached incremental?]
    :or {cutoff-ms 0 cached {}}}]
  (mapv
    (fn [info]
      (let [cid (:cardId info)
            prior (when incremental? (cached->state (get cached cid)))
            log (sf/filtered-revlog (get revlog-by-card cid []) cutoff-ms)
            state (sf/replay scheduler log prior)
            anki-ivl (anki-interval->days (:interval info))
            last-ms (:last-review-ms state)
            hw (max (long (or (some-> (get cached cid)
                                :anki_card_states/high_water_revlog_id long)
                            0))
                 (if (seq log) (long (:id (peek log))) 0))]
        {:anki_card_id cid
         :anki_note_id (:note info)
         :deck (:deckName info)
         ;; Normalized so the stored row already satisfies
         ;; `fsrs/review-card`'s `step nil ⟺ state = 2`. A card pushed to Anki
         ;; but never reviewed has no replayed state at all, and writing it as
         ;; Learning with a null step makes every later projection of it throw
         ;; from inside FSRS.
         :state (:state (sf/->fsrs-card state))
         :step (:step (sf/->fsrs-card state))
         :stability (:stability state)
         :difficulty (:difficulty state)
         ;; A card Anki reports at interval 0 is new or in learning. It has no
         ;; interval in force, so the replayed one is the only figure there is.
         :interval_days (if (pos? anki-ivl) anki-ivl (or (:interval-days state) 0.0))
         :last_review_ms last-ms
         :due_ms (when (and last-ms (pos? anki-ivl))
                   (+ (long last-ms) (long (* anki-ivl sf/ms-per-day))))
         :reps (or (:reps state) 0)
         :config_fingerprint fingerprint
         :high_water_revlog_id hw}))
    infos))

;; ---------------------------------------------------------------------------
;; Acquisition (browser only)
;; ---------------------------------------------------------------------------

;; `anki-call!` already returns CLOJURE data: it runs
;; `(js->clj json :keywordize-keys true)` on the whole response and hands back
;; `(:result …)`. So no call site below re-converts, and none reaches for a JS
;; property accessor — `(.-actions res)` on a Clojure map reads nil, which
;; would make the capability probe of §2.1 report every action missing.

#?(:cljs
   (defn probe!
     "§2.1 — which of `required-actions` this AnkiConnect serves.
      Post: a task of {:ok? bool :missing [action …]}."
     []
     (m/sp
       (let [res (m/? (anki-call! "apiReflect" {:scopes ["actions"]
                                                :actions required-actions}))
             present (set (:actions res))
             missing (vec (remove present required-actions))]
         {:ok? (empty? missing) :missing missing}))))

#?(:cljs
   (defn note-card-ids!
     "§2.2 — {note-id [card-id …]} for `note-ids`, batched at `notes-info-batch`.

      Post: a task of the map. A note Anki no longer holds is absent from the
            map rather than mapped to an empty vector — a deleted note and a
            note with no cards are different facts, and only the first is a
            reason to re-push."
     [note-ids]
     (m/sp
       (loop [batches (partition-all notes-info-batch (vec note-ids))
              acc {}]
         (if-let [b (first batches)]
           (let [notes (m/? (anki-call! "notesInfo" {:notes (vec b)}))]
             (recur (rest batches)
               (reduce (fn [m n]
                         (if-let [nid (:noteId n)]
                           (assoc m nid (vec (:cards n)))
                           m))
                 acc notes)))
           acc)))))

#?(:cljs
   (defn cards-info!
     "§2.5 — `cardsInfo` for `card-ids`, keeping only the fields the forecast
      reads. Batched at `notes-info-batch` for the same JSON-size reason:
      `cardsInfo` renders every card's question and answer HTML.

      Post: a task of a vector of {:cardId :note :deckName :interval :due
            :type :queue}. A card id Anki does not hold yields `{}` from
            AnkiConnect and is dropped here."
     [card-ids]
     (m/sp
       (loop [batches (partition-all notes-info-batch (vec card-ids))
              acc []]
         (if-let [b (first batches)]
           (let [infos (m/? (anki-call! "cardsInfo" {:cards (vec b)}))]
             (recur (rest batches)
               (into acc (comp (filter :cardId)
                           (map #(select-keys % [:cardId :note :deckName :interval
                                                 :due :type :queue])))
                 infos)))
           acc)))))

#?(:cljs
   (defn deck-config!
     "§2.3 — the deck config, or nil when Anki does not know the deck.
      `getDeckConfig` returns literal `false` for an unknown deck name, so a
      non-map result is a missing deck rather than a missing key."
     [deck]
     (m/sp
       (let [cfg (m/? (anki-call! "getDeckConfig" {:deck deck}))]
         (when (map? cfg) cfg)))))

#?(:cljs
   (defn revlog!
     "§2.4 — the revlog for `card-ids`, grouped by card id.

      Two paths, and `high-water` picks between them. At 0 the first refresh
      reads the whole history through `getReviewsOfCards`. Above 0 the
      incremental refresh asks `cardReviews` for what came after the mark, and
      `normalize-card-reviews` reshapes its positional rows and drops the rest
      of the deck.

      Post: a task of {card-id [{:id :ease :ivl :type} …]}. Under the
            incremental path the map holds ONLY new reviews, so the caller must
            merge it onto the cached state rather than replace with it — a
            replay of new reviews alone would restart every card from fresh."
     [deck card-ids high-water]
     (m/sp
       (if (pos? (long high-water))
         (normalize-card-reviews
           (m/? (anki-call! "cardReviews" {:deck deck :startID high-water}))
           card-ids)
         (let [res (m/? (anki-call! "getReviewsOfCards" {:cards (vec card-ids)}))]
           ;; getReviewsOfCards keys its result by card id, which JSON renders
           ;; as an object KEY — a string, which `anki-call!`'s keywordizing
           ;; turns into a keyword. The rest of this feature keys on the
           ;; numeric id, so the key is read back out of the keyword's name.
           (reduce-kv (fn [m k v] (assoc m (js/parseInt (name k) 10) (vec v)))
             {} res))))))

#?(:cljs
   (defn acquire!
     "The whole client-side acquisition, in one task (§2).

      This stops at RAW rows. §9.4.1 puts the AnkiConnect I/O in the browser
      because only the browser can reach localhost:8765, and §9.4.2 puts the
      replay on the server because the branch tree of §4.6 would occupy the
      browser's only thread. So this returns `:infos` and `:revlog` verbatim
      and calls `card-rows` nowhere.

      Pre : `note-ids` are `flashcards.anki_note_id` values for the document's
            pushed cards. `high-water` is the stored mark, or 0 when the cache
            is empty or its fingerprint no longer matches (§3.3).
      Post: a task of one of
              {:status :ok :deck :deck-config :fingerprint :infos :revlog
               :incremental? :card-count :note-card-ids}
              {:status :unsupported :missing [action …]}   ; §8.1, cause 1
              {:status :no-deck}                           ; the cards are in no deck
              {:status :no-fsrs :deck :param-counts {…}}   ; §8.1, causes 2 and 3
              {:status :no-cards}                          ; §8.3
      Post: `:incremental?` tells the server whether `:revlog` is the whole
            history or only what followed the mark. Replaying an incremental
            pull as if it were whole would restart every card from fresh, which
            is the one way this cache can report confident nonsense.

      The deck is the MODE over the cards' own decks, not a user choice.
      FreeMemo pushes to one deck, and asking the user to name it again would
      let the forecast read a config that governs none of their cards."
     [note-ids high-water]
     (m/sp
       (let [probe (m/? (probe!))]
         (if-not (:ok? probe)
           {:status :unsupported :missing (:missing probe)}
           (let [n->c (m/? (note-card-ids! note-ids))
                 card-ids (vec (mapcat val n->c))]
             (if (empty? card-ids)
               {:status :no-cards}
               (let [infos (m/? (cards-info! card-ids))
                     deck (->> infos (keep :deckName) frequencies
                            (sort-by (comp - val)) ffirst)]
                 (if-not deck
                   {:status :no-deck}
                   (let [cfg (m/? (deck-config! deck))]
                     (if-not (sf/fsrs6-parameters cfg)
                       {:status :no-fsrs
                        :deck deck
                        :param-counts {:fsrsWeights (count (:fsrsWeights cfg))
                                       :fsrsParams5 (count (:fsrsParams5 cfg))
                                       :fsrsParams6 (count (:fsrsParams6 cfg))}}
                       (let [incremental? (pos? (long high-water))
                             log (m/? (revlog! deck card-ids high-water))]
                         (log/log-debug
                           (str "[schedule-anki] deck=" deck
                             " notes=" (count note-ids)
                             " cards=" (count card-ids)
                             " revlog-cards=" (count log)
                             " high-water=" high-water))
                         {:status :ok
                          :deck deck
                          :deck-config cfg
                          :fingerprint (sf/config-fingerprint cfg)
                          :infos (vec infos)
                          :revlog log
                          :incremental? incremental?
                          :card-count (count card-ids)
                          :note-card-ids n->c}))))))))))))

#?(:cljs
   (defn run-acquire!
     "Invoke `acquire!` and write its outcome into `!result`.

      The project's `run-*!` shape: a task plus two callbacks, never a promise
      chain, so a failure reaches an atom the reactive graph is watching rather
      than an unhandled rejection. Returns nil — the atom is the output.

      ONE atom, not two. A failure used to write an error atom AND
      `{:status :unreachable}`, which is one fact in two places; the status
      already says the pull failed and carries the message with it, so no
      reader can see one without the other.

      An unreachable Anki is not an error state here. §8.2 requires the tab to
      serve cached state with a staleness marker, so the failure callback
      writes `{:status :unreachable}` and the caller decides what to render."
     [note-ids high-water !result]
     ((acquire! note-ids high-water)
      (fn [res] (reset! !result res))
      (fn [err]
        (log/log-debug (str "[schedule-anki] unreachable: " (.-message err)))
        (reset! !result {:status :unreachable :message (.-message err)})))
     nil))

(defn start-acquire!
  "Reactive-body-safe entry to `run-acquire!`.

   A bare call to the cljs-only var above would not resolve on the JVM peer,
   and Electric compiles an `e/defn` body on BOTH (CLAUDE.md, 'Server-Only
   Functions in .cljc Files'). The body-level reader conditional is what keeps
   the symbol resolvable on CLJ while the work stays on CLJS.

   Callers MUST wrap this in `e/snapshot` inside a keyed frame. It writes to
   an atom and starts a fetch, and Electric re-evaluates reactive bodies
   unpredictably — the same rule the JS-library-init section states."
  [note-ids high-water !result]
  #?(:cljs (run-acquire! note-ids high-water !result)
     :clj nil))
