(ns freememo.schedule-anki
  "AnkiConnect acquisition for the Study Scheduler
   (plans/study-scheduler-spec.md §2 and §9.4.1).

   Every call here runs in the BROWSER. The FreeMemo server cannot reach the
   user's AnkiConnect — `freememo.anki-sync-helpers/anki-call!` posts to
   127.0.0.1:8765 and is cljs-only — so the browser acquires and ships raw
   rows to the server, which replays them (§9.4.2). Nothing here persists
   anything: raw revlog is never stored (§9.4.3).

   READ-ONLY (§1.5). Five actions, and no others: `apiReflect`, `notesInfo`,
   `cardsInfo`, `getDeckConfig`, `getReviewsOfCards`. Nothing in this namespace
   writes to a collection.

   THE REVLOG IS READ BY CARD, NEVER BY DECK. `cardReviews(deck, startID)` was
   the incremental path, and AnkiConnect scopes it by the card's CURRENT deck.
   Measured against a live collection: `cardReviews(\"All\", 0)` returned 73 670
   rows and NONE of the 126 cards the user had gathered into a filtered deck to
   study. Two silent losses followed. A card being studied through a filtered
   deck contributed no reviews at all, and a document spanning two decks lost
   every card outside the modal one — 11 of 137 on the collection that surfaced
   this. `getReviewsOfCards(cards)` names the cards it wants, so neither can
   happen, and it costs less: 137 cards in 30 ms / 12 KB against 365 ms for one
   deck-wide sweep."
  (:require [freememo.schedule-fsrs :as sf]
            [freememo.logging :as log]
            [freememo.task-owner :as owner]
   [freememo.task-request :as request]
   [freememo.schedule-history :as history]
            ;; `anki-call!` is cljs-only, so a plain `:refer` fails to resolve
            ;; on the JVM peer and this whole namespace stops loading. The pure
            ;; half — `preset?`, `config-deck-candidates`, `ignore-before-ms`,
            ;; `card-rows` — runs on the SERVER (§9.4.2) or in tests, so that
            ;; failure is not academic.
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

   ;; This banner used to read \"FSRS is off in Anki, or its deck preset carries
   ;; no 21-parameter fsrsParams6\". Nothing here reads a global FSRS flag, and
   ;; no action in §1.5's set exposes one, so the first half blamed a cause the
   ;; code never checked. It said so on a collection where FSRS was on and the
   ;; preset carried all 21 parameters.
   :no-fsrs {:chip "FSRS unavailable" :fault? true
             :banner (str "This deck's Anki preset carries no 21-parameter "
                       "fsrsParams6. Optimize its FSRS parameters in Anki.")}

   :filtered-deck {:chip "cards in a filtered deck" :fault? true
                   :banner (str "Every card of this document is in a filtered "
                             "deck, which has no preset and therefore no FSRS "
                             "parameters. Empty the filtered deck, or set this "
                             "document's Anki deck in the sync form.")}

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
   refusal of §8.1 instead of an error with no cause in it.

   `cardReviews` was a sixth entry. Nothing calls it now, and requiring it
   would refuse an AnkiConnect that can serve every read this feature makes."
  ["apiReflect" "notesInfo" "cardsInfo" "getDeckConfig" "getReviewsOfCards"])

(def ^:const notes-info-batch
  "§2.2 — note ids per `notesInfo` call. 999 is sqlite's variable cap, which
   AnkiConnect also honors internally, so this bound is about the JSON: the
   response carries every note's full field HTML, and a document's whole note
   set in one response is megabytes for a card-id map."
  999)

;; ---------------------------------------------------------------------------
;; Which deck's config governs the forecast (§2.3)
;; ---------------------------------------------------------------------------
;; Pure, so the rule is testable on the JVM. `acquire!` below is cljs-only and
;; does I/O, and a selection rule buried inside it could only be checked by
;; hand against a live collection — which is how it shipped wrong.

(defn preset?
  "Whether a `getDeckConfig` result is a deck PRESET rather than a filtered deck.

   `getDeckConfig` answers with the config group for a normal deck, and with
   the FILTERED DECK'S OWN object for a filtered one. Both are maps, so `map?`
   cannot separate them — only `dyn` can. A filtered deck belongs to no preset
   and therefore carries no FSRS parameters, so one gathered for study made the
   whole forecast report `:no-fsrs` while the collection's real preset held all
   21 (measured: `Compilers book`, `dyn` 1, against `AnKing`, `dyn` false).

   Pre : `cfg` is a parsed `getDeckConfig` result, or nil.
   Post: true only for a map whose `dyn` is absent, false, or 0. A preset
         reports false and a filtered deck reports 1, both observed live. 0 is
         admitted because it is JSON's other spelling of false, and CLJS reads
         0 as TRUTHY — a bare `(:dyn cfg)` test would call such a deck filtered."
  [cfg]
  (and (map? cfg)
    (let [d (:dyn cfg)]
      (not (and (number? d) (pos? d))))))

(defn config-deck-candidates
  "The decks to try `getDeckConfig` on, best first.

   The forecast needs ONE preset, and a document's cards may sit in several
   decks at once. Frequency ranks them because FreeMemo pushes a document to a
   single deck, so the deck holding most of its cards is the one whose settings
   govern them.

   Pre : `infos` is the `cardsInfo` result.
   Post: distinct `:deckName`s, most cards first, ties broken by name so the
         order is TOTAL — two decks holding equal counts must not pick a
         different preset from one refresh to the next. A card carrying no deck
         name is dropped."
  [infos]
  (->> infos
    (keep :deckName)
    frequencies
    (sort-by (fn [[deck n]] [(- n) deck]))
    (mapv first)))

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

(defn card-row
  "Project one replayed state into a Schedule cache row."
  [info state fingerprint high-water]
  (let [cid (:cardId info)
        anki-ivl (anki-interval->days (:interval info))
        last-ms (:last-review-ms state)
        hw high-water]
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

(defn card-rows
  "Replay each complete card history and project its cache row."
  [scheduler infos revlog-by-card {:keys [cutoff-ms fingerprint] :or {cutoff-ms 0}}]
  (mapv (fn [info]
          (let [rows (sf/filtered-revlog (get revlog-by-card (:cardId info) []) cutoff-ms)
                state (sf/replay scheduler rows)]
            (card-row info state fingerprint (if (seq rows) (long (:id (peek rows))) 0))))
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
   (defn resolve-deck-config!
     "§2.3 — walk `deck-names` and stop at the first that governs the cards.

      A name qualifies when `getDeckConfig` answers with a `preset?` carrying 21
      FSRS-6 parameters. `getDeckConfig` returns literal `false` for a name Anki
      does not know, and the filtered deck's own object for a filtered one, so
      both a missing deck and a preset-less one are skipped rather than accepted.

      Pre : `deck-names` is ordered best-first (`config-deck-candidates`, then
            the user's configured push deck).
      Post: a task of {:deck :config} for the first qualifying name, or
            {:filtered-only? bool :param-counts {…}} when none qualifies.
      Post: `:filtered-only?` is true only when NO preset was read at all. It
            separates two failures that need different words: every deck the
            cards are in is filtered and has no preset, against a real preset
            that carries no FSRS-6 parameters. The first is fixed by emptying
            the filtered deck, the second by optimizing parameters in Anki.
      Post: `:param-counts` describes the FIRST preset that fell short, so the
            log can tell an FSRS-4.5 or FSRS-5 collection from an unoptimized
            one. Nil when no preset was read."
     [deck-names]
     (m/sp
       (loop [names (seq deck-names)
              saw-preset? false
              counts nil]
         (if-not names
           {:filtered-only? (not saw-preset?) :param-counts counts}
           (let [deck (first names)
                 cfg (m/? (anki-call! "getDeckConfig" {:deck deck}))]
             (cond
               (not (preset? cfg)) (recur (next names) saw-preset? counts)
               (sf/fsrs6-parameters cfg) {:deck deck :config cfg}
               :else (recur (next names) true
                       (or counts
                         {:fsrsWeights (count (:fsrsWeights cfg))
                          :fsrsParams5 (count (:fsrsParams5 cfg))
                          :fsrsParams6 (count (:fsrsParams6 cfg))})))))))))


#?(:cljs
   (defn acquire!
     "Read card metadata and send complete histories through acknowledged replay chunks.
      Each history starts from fresh state. Filtered decks preserve every named card.
      The server receives at most 1,000 reviews per replay request.
      Return compact acquisition metadata after every card finishes."
     [note-ids push-deck consume!]
     (m/sp
       (let [probe (m/? (probe!))]
         (if-not (:ok? probe)
           {:status :unsupported :missing (:missing probe)}
           (let [n->c (m/? (note-card-ids! note-ids))
                 card-ids (vec (mapcat val n->c))]
             (if (empty? card-ids)
               {:status :no-cards}
               (let [infos (m/? (cards-info! card-ids))
                     candidates (config-deck-candidates infos)]
                 (if (empty? candidates)
                   {:status :no-deck}
                   (let [tries (if (and push-deck (not (some #{push-deck} candidates)))
                                 (conj candidates push-deck)
                                 candidates)
                         found (m/? (resolve-deck-config! tries))]
                     (if-not (:deck found)
                       (do
                         ;; The counts are logged rather than dropped. They were
                         ;; assembled and read nowhere, so the one fact that
                         ;; separates an FSRS-5 collection from an unoptimized
                         ;; preset never reached anybody.
                         (log/log-debug
                           (str "[schedule-anki] no usable deck config"
                             " tried=" (pr-str tries)
                             " filtered-only=" (:filtered-only? found)
                             " param-counts=" (pr-str (:param-counts found))))
                         {:status (if (:filtered-only? found) :filtered-deck :no-fsrs)
                          :deck (first candidates)
                          :param-counts (:param-counts found)})
                       (let [deck (:deck found)
                             cfg (:config found)
                             base {:deck deck :deck-config cfg :fingerprint (sf/config-fingerprint cfg)}
                             result (m/? (history/read-history! anki-call! consume! base infos))]
                         (log/log-debug
                           (str "[schedule-anki] deck=" deck
                             " notes=" (count note-ids)
                             " cards=" (count card-ids)
                             " decks-seen=" (count candidates)
                             " streamed-reviews=" (:review-count result)))
                         (merge base result {:status :ok :card-count (count card-ids)})))))))))))))

#?(:cljs
   (defn run-acquire!
     "Invoke `acquire!` and write its outcome into `!result`.

      The project's `run-*!` shape: a task plus two callbacks, never a promise
      chain, so a failure reaches an atom the reactive graph is watching rather
      than an unhandled rejection. Returns its canceller for frame teardown.

      ONE atom, not two. A failure used to write an error atom AND
      `{:status :unreachable}`, which is one fact in two places; the status
      already says the pull failed and carries the message with it, so no
      reader can see one without the other.

      An unreachable Anki is not an error state here. §8.2 requires the tab to
      serve cached state with a staleness marker, so the failure callback
      writes `{:status :unreachable}` and the caller decides what to render."
     [note-ids push-deck !result !request run-id]
     (reset! !result nil)
     (owner/start! (acquire! note-ids push-deck #(request/request! !request (assoc % :run-id run-id)))
      (fn [res] (reset! !result (assoc res :run-id run-id)))
      (fn [err]
        (log/log-debug (str "[schedule-anki] unreachable: " (.-message err)))
        (reset! !result {:status :unreachable :message (.-message err) :run-id run-id})))))

(defn start-acquire!
  "Reactive-body-safe entry to `run-acquire!`.

   A bare call to the cljs-only var above would not resolve on the JVM peer,
   and Electric compiles an `e/defn` body on BOTH (CLAUDE.md, 'Server-Only
   Functions in .cljc Files'). The body-level reader conditional is what keeps
   the symbol resolvable on CLJ while the work stays on CLJS.

   Start once inside a keyed frame with `e/snapshot`.
   Register the returned canceller with that frame's `e/on-unmount`."
  [note-ids push-deck !result !request run-id]
  #?(:cljs (run-acquire! note-ids push-deck !result !request run-id)
     :clj nil))
