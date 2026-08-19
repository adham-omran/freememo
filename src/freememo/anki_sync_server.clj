(ns freememo.anki-sync-server
  "Server-side Anki sync operations — fetching cards for sync,
   recording pushed note IDs, and applying pull updates."
  (:require
   [clojure.string :as str]
   [freememo.db :as db]
   [freememo.settings :as settings]
   [freememo.commands :as commands]
   [freememo.anki-sync-helpers :as helpers]
   [freememo.bibliography-form :as bibform]
   [freememo.html-cleaner :as cleaner]
   [freememo.math :as math]
   [taoensso.telemere :as tel]))

(defn load-anki-preferences
  "Load saved Anki sync preferences for a user (global). Note types are
   app-owned (FreeMemo Basic/Cloze), so no model/field ordering is stored.

   Deck and allow-dupes only. Header and tags are per-document and resolve
   through `settings/resolve-anki-header` and `settings/resolve-anki-tags`, which
   have no account-wide fallback — see plans/anki-tags-per-document.md."
  [user-id]
  (try
    {:success true
     :prefs {:scope (settings/get-anki-scope user-id)
             :deck (settings/get-anki-deck user-id)
             :allow-dupes (settings/get-anki-allow-dupes user-id)}}
    (catch Exception e
      (tel/error! {:id ::load-anki-preferences} e)
      {:success false :prefs {}})))

(defn load-item-preset
  "Load per-item Anki sync preset. Returns the preset map or nil."
  [user-id root-topic-id]
  (settings/get-anki-preset user-id root-topic-id))

(defn save-item-preset
  "Save per-item Anki sync preset."
  [user-id root-topic-id preset-map]
  (settings/save-anki-preset user-id root-topic-id preset-map))

(defn resolve-modal-prefs
  "One-shot resolution of the modal's initial selection (deck / scope / dupes),
   so the form paints once with correct values instead of first-defaulting then
   overriding.

   Precedence (deck is applied client-side, since the deck list comes from
   AnkiConnect and needs validation there):
     deck         : per-doc preset > last-used (skipped in 'none')
     dupes        : per-doc preset > global last-used
     scope        : global last-used only (per-push choice, not per-item —
                    the modal saves it to global at handoff and it must win)
   'none' only skips the per-doc preset and the last-used deck.

   Tags are NOT resolved here. They are per-document with no account-wide
   fallback, so `TagSettings` reads `settings/resolve-anki-tags` directly, the
   way `HeaderSettings` reads the header. Returning them from this map is what
   let one document's tag prefill the box on the next document, and a Sync press
   then bound that tag to a document the user never chose it for.

   :preset? reports whether a per-doc preset was applied (drives the
   'saved settings for this document' indicator)."
  [user-id root-topic-id]
  (try
    (let [mode   (settings/get-anki-auto-load-mode user-id)
          preset (when (= mode "per-item")
                   (settings/get-anki-preset user-id root-topic-id))
          global (:prefs (load-anki-preferences user-id))]
      {:mode        mode
       :preset?     (some? preset)
       ;; Scope is a per-push choice (self/subtree/document relative to the
       ;; in-view topic), NOT per-item config — resolve from global last-used
       ;; so the modal's fresh selection (saved to global at handoff) always
       ;; wins. Reading preset-first let a stale preset scope override it.
       :scope       (settings/normalize-scope (:scope global))
       :deck        (or (:deck preset)
                      (when (not= mode "none") (:deck global)))
       :allow-dupes (if (some? (:allow-dupes preset)) (:allow-dupes preset) (:allow-dupes global))})
    (catch Exception e
      (tel/error! {:id ::resolve-modal-prefs} e)
      {:mode "global" :preset? false})))

(defn- card-topic-id
  "The topic a card cites and links to: its own, else its root. One definition,
   because the bibliography and the page locator MUST key off the same walk —
   a card whose citation came from topic X must not take its page from topic Y."
  [card]
  (or (:flashcards/topic_id card) (:flashcards/root_topic_id card)))

(defn- attach-card-bibliography
  "Assoc :fm/bibliography-html onto each card, resolved from the card's OWN
   topic via the effective-source walk (own source_id, else nearest ancestor).
   Resolves once per distinct topic, so a whole-document push is O(topics),
   not O(cards).
   Pre:  cards carry :flashcards/topic_id and/or :flashcards/root_topic_id.
   Post: every card gains :fm/bibliography-html (string or nil)."
  [cards]
  (let [html-by-topic (reduce (fn [m tid]
                                (if (contains? m tid)
                                  m
                                  (assoc m tid
                                    (some-> (db/resolve-effective-source-id tid)
                                      db/get-source
                                      :sources/csl
                                      helpers/format-bibliography-html))))
                        {}
                        (keep card-topic-id cards))]
    (mapv #(assoc % :fm/bibliography-html (get html-by-topic (card-topic-id %))) cards)))

(defn- attach-card-pdf-page
  "Assoc :fm/pdf-page onto each card — the PDF page number the card's own topic
   sits on, resolved through the whole ancestor chain, or nil. One query for the
   whole card set, so a document push costs one statement here.
   Pre:  cards carry :flashcards/topic_id and/or :flashcards/root_topic_id.
   Post: every card gains :fm/pdf-page (integer or nil). nil for every non-PDF
         document, for an EPUB chapter, and for a score card — see
         db/resolve-pdf-page-numbers for why the gate lives there and not here."
  [cards]
  (let [page-by-topic (db/resolve-pdf-page-numbers (keep card-topic-id cards))]
    (mapv #(assoc % :fm/pdf-page (get page-by-topic (card-topic-id %))) cards)))

(defn attach-occlusion-groups
  "Attach plain-data group info (:occlusion-group {:anki-key :mode :geometry
   :image-media-id}) to every occlusion card row — everything the client-side
   push needs to generate mask SVGs and IO note fields. Non-occlusion rows
   pass through untouched."
  [cards]
  (let [gids (into #{} (keep :flashcards/occlusion_group_id) cards)]
    (if (empty? gids)
      cards
      (let [groups (db/get-occlusion-groups-by-ids gids)]
        (mapv (fn [card]
                (if-let [g (get groups (:flashcards/occlusion_group_id card))]
                  (assoc card :occlusion-group
                    {:anki-key (:occlusion_groups/anki_key g)
                     :mode (:occlusion_groups/mode g)
                     :geometry (:occlusion_groups/geometry g)
                     :image-media-id (:occlusion_groups/image_media_id g)})
                  card))
          cards)))))

(defn attach-score-groups
  "Attach plain-data group info (:score-group {:anki-key :start-ms :end-ms
   :clip-media-id :geometry}) to every score card row — everything the
   client-side push needs to build Score note fields. Non-score rows pass
   through untouched."
  [cards]
  (let [gids (into #{} (keep :flashcards/score_group_id) cards)]
    (if (empty? gids)
      cards
      (let [groups (db/get-score-groups-by-ids gids)]
        (mapv (fn [card]
                (if-let [g (get groups (:flashcards/score_group_id card))]
                  (assoc card :score-group
                    {:anki-key (:score_groups/anki_key g)
                     :start-ms (:score_groups/start_ms g)
                     :end-ms (:score_groups/end_ms g)
                     :clip-media-id (:score_groups/clip_media_id g)
                     :geometry (:score_groups/geometry g)})
                  card))
          cards)))))

(defn get-cards-for-sync
  "Get flashcards for Anki sync. Also returns the current root topic title/kind
   and pre-resolved bibliography (text + HTML) so the Source field and the
   Bibliography field/append reflect edits made since the modal was opened.
   Each card additionally carries :fm/bibliography-html resolved from its OWN
   topic (own source, else nearest ancestor), so extract cards cite the
   extract's bibliography, not the root's.
   Occlusion rows carry :occlusion-group (attach-occlusion-groups).
   opts: {:user-id N, :topic-id N, :root-topic-id N, :scope <key>}
   scope resolves the card set: 'self' → topic-id only; 'subtree' → topic-id +
   descendants; 'document' → whole root tree. When scope is absent/legacy,
   falls back to topic-id-or-root (the pre-scope contract; pull uses this)."
  [{:keys [user-id topic-id root-topic-id scope]}]
  (tel/log! {:level :info
             :id ::get-cards-for-sync.entry
             :data {:user-id user-id
                    :topic-id topic-id
                    :root-topic-id root-topic-id
                    :scope scope}}
    "get-cards-for-sync invoked")
  (try
    (if-not (and user-id
              (or (nil? topic-id) (db/owns-topic? user-id topic-id))
              (or (nil? root-topic-id) (db/owns-topic? user-id root-topic-id)))
      {:success false :error "Not authorized for this topic"}
      (let [cards (attach-score-groups
                  (attach-occlusion-groups
                    (attach-card-pdf-page
                      (attach-card-bibliography
                        (case scope
                          "self"     (db/get-flashcards topic-id)
                          "subtree"  (db/get-flashcards-for-subtree topic-id)
                          "document" (db/get-all-flashcards root-topic-id)
                          (if topic-id
                            (db/get-flashcards topic-id)
                            (db/get-all-flashcards root-topic-id)))))))
          root-topic (when (and user-id root-topic-id)
                       (db/get-topic-for-user user-id root-topic-id))
          source-id (:topics/source_id root-topic)
          source (when source-id (db/get-source source-id))
          csl (:sources/csl source)
          bib-text (bibform/format-citation csl)
          bib-html (helpers/format-bibliography-html csl)]
      (tel/log! {:level :info
                 :id ::get-cards-for-sync.resolved
                 :data {:topic-title (:topics/title root-topic)
                        :topic-kind (:topics/kind root-topic)
                        :card-count (count cards)
                        :has-bibliography (some? bib-text)}}
        "get-cards-for-sync resolved")
      {:success true
       :cards cards
       :topic-title (:topics/title root-topic)
       :topic-kind (:topics/kind root-topic)
       :bibliography-text bib-text
       :bibliography-html bib-html}))
    (catch Exception e
      (tel/error! {:id ::get-cards-for-sync} e)
      {:success false :error (.getMessage e)})))

(def ^:private pull-card-keys
  "The only card fields the client-side pull diff reads (see
   anki-sync-helpers/do-anki-pull!). :io_fields carries the occlusion text
   fields and MUST stay — the occlusion branch of the diff reads it."
  [:flashcards/id :flashcards/anki_note_id :flashcards/kind
   :flashcards/question :flashcards/answer :flashcards/cloze
   :flashcards/io_fields])

(defn get-cards-for-pull
  "Cards a pull can act on: the pushed cards under root-topic-id, projected to
   the fields the diff reads.

   Pre : user-id is non-nil. root-topic-id is nil or owned by user-id.
   Post: {:success true :cards [...]}, where every card carries a non-nil
         :flashcards/anki_note_id — or {:success false :error msg}.

   Separate from get-cards-for-sync on purpose, and push still uses that one.
   A pull sent every card of the document across the wire, with bibliography
   HTML, occlusion masks and score geometry attached, and then discarded the
   unpushed rows in the browser one line later."
  [{:keys [user-id root-topic-id]}]
  (try
    (if-not (and user-id
              (or (nil? root-topic-id) (db/owns-topic? user-id root-topic-id)))
      {:success false :error "Not authorized for this topic"}
      (let [cards (into []
                    (comp (filter #(some? (:flashcards/anki_note_id %)))
                      (map #(select-keys % pull-card-keys)))
                    (db/get-all-flashcards root-topic-id))]
        (tel/log! {:level :info
                   :id ::get-cards-for-pull
                   :data {:user-id user-id
                          :root-topic-id root-topic-id
                          :card-count (count cards)}}
          "get-cards-for-pull resolved")
        {:success true :cards cards}))
    (catch Exception e
      (tel/error! {:id ::get-cards-for-pull} e)
      {:success false :error (.getMessage e)})))

(defn log-client-sync-error!
  "Record a client-side Anki sync exception in the server log. Push/pull run
   entirely in the browser, so a CLJS throw never reaches server logs on its
   own — the client ships the caught error here from its failure handler.
   Gates on :source :client: server-origin failures are already logged at
   their throw site and must NOT be re-recorded, so a :server (or nil) detail
   is a no-op. Returns nil.
   detail: {:message :stack :source :phase :deck :card-count :topic-kind ...}"
  [user-id topic-id detail]
  (when (= :client (:source detail))
    (tel/log! {:level :error
               :id ::client-sync-error
               :data (assoc detail :user-id user-id :topic-id topic-id)}
      "client sync error"))
  nil)

;; `finalize-push!` lived here. It recorded pushed note ids, saved the global
;; last-used prefs, and merged the whole prefs map into `anki_preset_<root-id>`.
;; It lost its only caller when `AnkiSyncExecutor` gave way to
;; `anki-reconcile-panels/ReconcileExecutor`, and its absence is what made this
;; namespace's tag fallback reachable: with nothing writing a per-item preset,
;; `anki-write-bundles` fell through to the account-wide tag row for every
;; document. Reconcile records its own writes through
;; `anki-reconcile-server/record-anki-writes!`, and tags now persist per document
;; on edit, so nothing here needs to run at handoff.
;; See plans/anki-tags-per-document.md.

;; ---------------------------------------------------------------------------
;; Library cards Anki overlay (anki-modified / marked / suspended + F4 deletes)
;; ---------------------------------------------------------------------------

(defn- stripped=
  "Compare a client-stripped Anki field value against local field HTML,
   stringing the local side through the same normalisation. nil and \"\" are equal.

   `kind` is required because the push side spaces TeX close braces for
   cloze-bearing kinds ONLY (`math/stored->anki-html`). Normalising the local side
   unconditionally would make every BASIC card whose TeX nests braces
   (`x^{a^{b}}`) compare unequal forever — flagging it Anki-modified, which
   `library_cards` also treats as a delete-danger row. `math/stored->compare-html`
   owns the same kind predicate as the push path.

   pre : `kind` is the local card's kind; the caller has already converted Anki's
         side out of `<anki-mathjax>` and stripped it.
   post: true iff the two fields carry the same visible text."
  [anki-val local-html kind]
  (= (or anki-val "")
    (or (helpers/strip-html (math/stored->compare-html local-html kind)) "")))

(defn apply-anki-overlay!
  "Flag live Anki note state against local cards, for the Library rows.

   pre:  present-notes [{:note-id long
                         :stripped-fields [str ...]   ; strip-html'd, Anki field order
                         :tags [str ...]
                         :suspended {:total n :suspended n}} ...]
         absent-note-ids [long ...] — accepted and IGNORED, see below.
         Both derive from ONE notesInfo batch on the client.
   post: {:success true
          :per-card {card-id {:anki-modified bool, :marked bool,
                              :suspended {:total n :suspended n}}}  ; SPARSE — flagged cards only
          :deleted-count 0}
   invariants: reads only user-id's cards; deletes nothing.
   _rev is unused — callers pass a mutation revision so Electric re-runs
   the diff when LOCAL content changes under an unchanged Anki payload
   (work-skipping would otherwise hold stale flags).

   THIS NO LONGER DELETES. It used to remove every local card whose note
   `notesInfo` did not report, and documented the consequence as accepted: a
   successful reply from a wrong or empty collection wiped the pushed cards.
   Reconcile makes that unnecessary and unacceptable at once — it decides
   deletions per card behind `anki-reconcile/bulk-absence?`, which stops the whole
   run when a majority of notes are missing rather than believing the reply.
   Merely relocating the delete would have reproduced the wipe, since \"absent in
   Anki, unchanged locally\" is a delete row in the contract; the guard is what
   makes the move an improvement.

   `deleted-count` stays in the reply, always 0, because the client's
   `:sync-mutations` bump reads it. `absent-note-ids` stays in the signature so
   the client's one batch keeps its shape."
  [_rev user-id present-notes absent-note-ids]
  (try
    (let [note-ids (mapv :note-id present-notes)
          local-by-note (into {}
                          (map (juxt :flashcards/anki_note_id identity))
                          (db/get-flashcards-by-anki-note-ids user-id note-ids))
          per-card
          (into {}
            (keep (fn [{:keys [note-id stripped-fields tags suspended]}]
                    (when-let [card (get local-by-note note-id)]
                      (let [kind (:flashcards/kind card)
                            basic? (= "basic" kind)
                            anki-modified?
                            (cond
                              ;; Occlusion content diff is owned by the pull
                              ;; path (io_fields); score + overlapping fields are
                              ;; FM-derived (media, or the expanded list) with no
                              ;; single-field inverse. The overlay only surfaces
                              ;; marked/suspended for these rows.
                              (contains? #{"occlusion" "score" "overlapping"} kind) false
                              basic? (or (not (stripped= (first stripped-fields) (:flashcards/question card) kind))
                                       (not (stripped= (second stripped-fields) (:flashcards/answer card) kind)))
                              :else (not (stripped= (first stripped-fields) (:flashcards/cloze card) kind)))
                            marked? (boolean (some #{"marked"} tags))
                            suspended-n (or (:suspended suspended) 0)
                            flags (cond-> {}
                                    anki-modified? (assoc :anki-modified true)
                                    marked? (assoc :marked true)
                                    (pos? suspended-n) (assoc :suspended suspended))]
                        (when (seq flags)
                          [(:flashcards/id card) flags])))))
            present-notes)]
      (tel/log! {:level :debug
                 :id ::apply-anki-overlay!
                 :data {:user-id user-id
                        :present (count present-notes)
                        :flagged (count per-card)
                        :absent (count absent-note-ids)}}
        "anki overlay applied")
      {:success true
       :per-card per-card
       :deleted-count 0})
    (catch Exception e
      (tel/error! {:id ::apply-anki-overlay!} e)
      {:success false :error (.getMessage e)})))

;; ---------------------------------------------------------------------------
;; Library cards bulk actions (selection-based push / pull)
;; ---------------------------------------------------------------------------

(defn- snapshot-entries
  "The settings snapshot rows implied by a set of write bundles.

   pre : `bundles` is `anki-write-bundles`' output — each bundle's `:settings`
         carries the root's resolved header, each of its `:cards` carries
         `:fm/bibliography-html` resolved from that card's OWN topic.
   post: [{:card-id n :header-text s-or-nil :bibliography-html s-or-nil} …], one
         per card.

   The header is read through `helpers/effective-header-text`, the same condition
   `helpers/prepend-header` applies. Storing the raw override instead would make
   the snapshot say \"header: Chapter 1\" for a note whose checkbox is off, and
   the digest would then disagree with the note forever.

   The citation is per CARD, not per bundle: `attach-card-bibliography` walks each
   card's own topic to its nearest source, so two cards under one root can cite
   different things."
  [bundles]
  (into []
    (mapcat (fn [{:keys [settings cards]}]
              (let [header (helpers/effective-header-text
                             (:use-header settings) (:header-text settings))]
                (map (fn [card]
                       {:card-id (:flashcards/id card)
                        :header-text header
                        :bibliography-html (:fm/bibliography-html card)})
                  cards))))
    bundles))

(defn anki-write-bundles
  "Group `card-ids` by root topic and resolve each root's push settings — the
   per-root preset wins over user-level prefs, mirroring what the sync modal
   would use for that document.

   pre:  card-ids — a reconcile's `:create` and `:push` decisions together;
         ownership enforced in the lookup.
   post: {:success true :bundles [{:root-topic-id N :cards [...] :settings {…}}]}
         Every card given is in a bundle. Settings carry everything BOTH
         `build-note` and `build-update-fields` read.

   Two things this used to get wrong, when it served the bulk Push-updates
   button and was named `get-bulk-push-bundles`:

   1. It dropped every card with no `anki_note_id`. That button could only
      update, so unpushed cards were noise to it. To a reconcile they are the
      `:create` set — filtering them made a run report cards it had created and
      add nothing to Anki.
   2. It omitted `:deck`, on the reasoning that an update does not move cards
      between decks. True, and `addNote` requires one: without it every create
      would go to `deckName nil`.

   `:allow-dupes` joins it for the same reason — `build-note` reads it, and only
   the add path does.

   `:tags` comes from `settings/resolve-anki-tags` per root, NOT from `pick`.
   There is no account-wide tag row to fall back to, deliberately: this fn is the
   only settings builder on the write path, and a reconcile writes across the
   whole collection, so a global fallback here applied one document's tag to
   every note the run created. `:use-tags` false yields `[]` — the checkbox was
   stored and never read until now, which is the second half of the same defect.
   See plans/anki-tags-per-document.md."
  [user-id card-ids]
  (try
    (let [global (:prefs (load-anki-preferences user-id))
          rows (attach-score-groups
                 (attach-occlusion-groups (db/get-flashcards-by-ids user-id card-ids)))
          ;; Page locator resolved ONCE for the whole selection, before the
          ;; group-by — one statement however many roots the selection spans.
          with-page (attach-card-pdf-page rows)
          by-root (group-by :flashcards/root_topic_id with-page)
          bundles
          (mapv
            (fn [[root-id cards]]
              (let [preset (or (load-item-preset user-id root-id) {})
                    pick (fn [k] (let [v (get preset k)] (if (some? v) v (get global k))))
                    header (settings/resolve-anki-header user-id root-id)
                    tags (settings/resolve-anki-tags user-id root-id)
                    root-topic (db/get-topic-for-user user-id root-id)
                    source (when-let [sid (:topics/source_id root-topic)]
                             (db/get-source sid))
                    bib-html (helpers/format-bibliography-html (:sources/csl source))]
                {:root-topic-id root-id
                 :cards (attach-card-bibliography (vec cards))
                 :settings {:deck (pick :deck)
                            :allow-dupes (boolean (pick :allow-dupes))
                            :use-header (:use-header header)
                            :header-text (:header-text header)
                            :tags (if (:use-tags tags) (:tags tags) [])
                            :bibliography-html bib-html
                            :topic-title (:topics/title root-topic)
                            :topic-kind (:topics/kind root-topic)
                            :root-topic-id root-id
                            :app-base-url settings/app-base-url}}))
            by-root)
          ;; A create needs a deck; an update does not. Reported once, before
          ;; anything is sent, because the alternative is one AnkiConnect refusal
          ;; per card and a toast that names a count instead of a cause. The
          ;; modal gates its button on `helpers/pushable?` for the same reason —
          ;; a headless run has no button to gray out.
          deckless (filterv (fn [b]
                              (and (str/blank? (get-in b [:settings :deck]))
                                (some #(nil? (:flashcards/anki_note_id %)) (:cards b))))
                     bundles)]
      (if (seq deckless)
        {:success false
         :error (str "No Anki deck is set"
                  (when-let [t (:topic-title (:settings (first deckless)))]
                    (str " for \"" t "\""))
                  ". Open Sync with Anki and choose one.")}
        (do
          ;; Record what these notes are ABOUT to be pushed with, before anything
          ;; is sent and so before `record-anki-writes!` takes baselines from the
          ;; rows. This is what makes a missed settings fan-out self-healing: push
          ;; reads the header and the citation from their real homes, so a card
          ;; whose snapshot went stale still sends the right bytes — and without
          ;; this write the baseline would be computed from the stale snapshot and
          ;; disagree with what Anki then holds, permanently.
          ;;
          ;; A failed send leaves the snapshot moved and the baseline untouched,
          ;; which is the safe direction: the card still reads unsynced and the
          ;; next run retries.
          ;;
          ;; THIS WRITE AND THE BASELINE STAMP CANNOT SHARE A TRANSACTION, and the
          ;; separation is not an oversight to close. `record-anki-writes!` runs
          ;; after the client has written to Anki and read the notes back, so a
          ;; WebSocket round trip sits between the two. Ordering carries the
          ;; guarantee instead: snapshot first, baseline only from bytes read back,
          ;; so a crash anywhere between leaves the card unsynced and retried —
          ;; never the reverse.
          ;;
          ;; `refresh-anki-settings-snapshot!` is itself transactional, which is
          ;; what this reactive call site needs: `anki-reconcile-panels` runs it
          ;; inside `e/Offload`, whose latest-wins cancellation can abandon a
          ;; half-finished attempt when the plan changes.
          (db/refresh-anki-settings-snapshot! user-id (snapshot-entries bundles))
          {:success true :bundles bundles})))
    (catch Exception e
      (tel/error! {:id ::anki-write-bundles} e)
      {:success false :error (.getMessage e)})))

;; `get-cards-for-bulk-pull` lived here. It lost its only caller when the
;; reconcile replaced the bulk Pull button (`plans/anki-reconcile.md` §8.1, and
;; the note at `library_cards.cljc:582`), and it held the LAST timestamp-based
;; staleness test in the codebase — `updated_at > anki_synced_at`, in a private
;; `local-edit?`. Removed with the rest of that predicate rather than left as an
;; invitation to revive it: `anki-reconcile/classify` decides direction per card
;; from the two digests, and `db/unsynced-sql` is the only definition of unsynced.

(defn finalize-bulk-push!
  "Record a bulk push: refresh anki_note_id + anki_synced_at for pairs and
   bump :sync-mutations ONCE. Deliberately saves no preferences of any kind —
   a bulk update is not a preference-setting event. (The `finalize-push!` this
   contrasted with is gone; see the note above `apply-anki-overlay!`.)
   pairs: [{:card-id N :anki-note-id M} ...]"
  [user-id pairs]
  (try
    (db/set-anki-note-ids user-id
      (mapv (fn [{:keys [card-id anki-note-id]}] [card-id anki-note-id]) pairs))
    (commands/bump! user-id :anki-sync)
    {:success true :count (count pairs)}
    (catch Exception e
      (tel/error! {:id ::finalize-bulk-push!} e)
      {:success false :error (.getMessage e)})))

(defn- clean-pulled-field
  "Sanitize one pulled field value, and say so when it removed something.

   pre : `html` is stored-form HTML — decorations already off, `<anki-mathjax>`
         already converted by `helpers/note->pull-update`.
   post: HTML whose every tag, class token and style declaration is allow-listed.

   `clean-html-fragment`, not `clean-html`, and the difference is load-bearing.
   A card is a fragment, so `card-modals/sanitize-card-field` strips `id` on the
   way in. Admitting one on the way OUT would give the pulled field a token the
   local side can never hold, and the reconcile digest is markup-sensitive — the
   card would differ on every run and push straight back, for ever.

   The `:warn` is not decoration, for the same reason: `clean-html` drops what
   `quill-class-allow-list` and `safe-color-props` do not admit, so a format Anki
   holds and this app does not becomes a local difference on the very next run.
   That would read as a card which never settles, with nothing to grep for."
  [card-id field html]
  (let [cleaned (cleaner/clean-html-fragment html)]
    (when (not= cleaned html)
      (tel/log! {:level :warn
                 :id ::pulled-html-sanitized
                 :data {:card-id card-id :field field
                        :before (count (or html "")) :after (count (or cleaned ""))}}
        "clean-html changed a pulled Anki field"))
    cleaned))

(defn sanitized-pull-update
  "One `helpers/note->pull-update` map with every HTML value sanitized.

   pre : `pull-update` is that fn's output. Stored-form HTML sits in `:question`,
         `:answer`, `:cloze`, or in the string values of `:io-fields`.
   post: the same keys, every HTML value through `clean-html`. Non-string
         `:io-fields` values are untouched, because the map shape is the
         caller's.
   inv : nothing reaches `db/update-flashcard!` or
         `db/merge-flashcard-io-fields!` unsanitized. Both are bare UPDATEs.

   Pull is the only path by which HTML this app did not author enters
   `flashcards`. Every other writer already sanitizes — `card-modals/
   sanitize-card-field`, `content-toolbar-helpers/create-extract-topic-safe!`.
   Omitting it here was safe only while `note->pull-update` stripped every tag,
   which is exactly what made a `<code>` edit undetectable.

   `.clj`, not `.cljc`: `clean-html` is Jsoup. The two apply sites are both
   server-side, so one definition serves both without putting the sanitizer on
   the CLJS peer."
  [pull-update]
  (let [card-id (:card-id pull-update)]
    (cond-> pull-update
      (contains? pull-update :question)
      (update :question #(clean-pulled-field card-id :question %))

      (contains? pull-update :answer)
      (update :answer #(clean-pulled-field card-id :answer %))

      (contains? pull-update :cloze)
      (update :cloze #(clean-pulled-field card-id :cloze %))

      (contains? pull-update :io-fields)
      (update :io-fields
        (fn [io]
          (reduce-kv (fn [m k v]
                       (assoc m k (if (string? v) (clean-pulled-field card-id k v) v)))
            {} io))))))

(defn apply-pull-updates
  "After pull, update card content from Anki edits and delete locally cards removed from Anki.
   Bumps :sync-mutations so the client toolbar refreshes counts.
   updates: [{:card-id N :question Q :answer A :cloze C} ...] or
            [{:card-id N :io-fields {partial map}} ...] for occlusion cards
   deleted-card-ids: [N ...] — cards whose Anki notes no longer exist

   `sanitized-pull-update` is the last step before the write, and it is the whole
   protection: `db/update-flashcard!` is a bare UPDATE with no sanitization.

   `<anki-mathjax>` is already stored form by the time it arrives —
   `helpers/note->pull-update` converts it, because its comparison needs the
   stored form anyway. Converting again here would re-wrap nothing and was
   removed. The brace spacing applied on push is deliberately NOT reversed: the
   extra space is valid TeX, and undoing it would have to guess which spaces the
   user typed."
  [user-id updates deleted-card-ids]
  (try
    (doseq [{:keys [card-id question answer cloze io-fields]} (map sanitized-pull-update updates)]
      (if (seq io-fields)
        ;; Occlusion: shallow JSONB merge — unchanged keys keep local values.
        (db/merge-flashcard-io-fields! user-id card-id io-fields)
        (let [fields (cond-> {}
                       question (assoc :question question)
                       answer (assoc :answer answer)
                       cloze (assoc :cloze cloze))]
          (when (seq fields)
            (db/update-flashcard! user-id card-id fields)
            (db/mark-anki-synced user-id card-id)))))
    (when (seq deleted-card-ids)
      (doseq [id deleted-card-ids]
        (db/delete-flashcard! user-id id)))
    (commands/bump! user-id :anki-sync)
    {:success true :count (count updates) :deleted (count (or deleted-card-ids []))}
    (catch Exception e
      (tel/error! {:id ::apply-pull-updates} e)
      {:success false :error (.getMessage e)})))
