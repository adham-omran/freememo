(ns freememo.anki-sync-server
  "Server-side Anki sync operations — fetching cards for sync,
   recording pushed note IDs, and applying pull updates."
  (:require
   [clojure.string :as str]
   [freememo.db :as db]
   [freememo.settings :as settings]
   [freememo.user-state :as us]
   [freememo.anki-sync-helpers :as helpers]
   [freememo.bibliography-form :as bibform]
   [taoensso.telemere :as tel])
  (:import [java.util Base64]))

(defn mime-type->ext
  "Derive a file extension from a MIME type string. Defaults to \"bin\" for unknowns."
  [mime-type]
  (case (str/lower-case (or mime-type ""))
    "image/png" "png"
    "image/jpeg" "jpg"
    "image/jpg" "jpg"
    "image/gif" "gif"
    "image/webp" "webp"
    "image/svg+xml" "svg"
    "image/bmp" "bmp"
    "image/tiff" "tiff"
    "bin"))

(defn get-media-base64
  "Fetch a media row by id and return {:filename \"<id>.<ext>\" :data \"<base64>\"}
   or nil if the row does not exist or the media table is not yet available.
   Called server-side from the push pipeline.
   Uses requiring-resolve so the compile succeeds even before db/get-media is defined."
  [media-id]
  (try
    (when-let [get-media-fn (requiring-resolve 'freememo.db/get-media)]
      (when-let [row (get-media-fn media-id)]
        (let [^bytes raw-bytes (:media/bytes row)
              mime (:media/mime_type row)
              ext (mime-type->ext mime)
              filename (str media-id "." ext)
              b64 (.encodeToString (Base64/getEncoder) raw-bytes)]
          {:filename filename :data b64})))
    (catch Exception e
      (tel/error! {:id ::get-media-base64 :data {:media-id media-id}} e)
      nil)))

(defn load-anki-preferences
  "Load saved Anki sync preferences for a user (global).
   :basic-fields / :cloze-fields are user-level field ordering defaults; they
   feed run-fetch-fields! after AnkiConnect returns the model's actual fields."
  [user-id]
  (try
    {:success true
     :prefs {:scope (settings/get-anki-scope user-id)
             :deck (settings/get-anki-deck user-id)
             :basic-model (settings/get-anki-basic-model user-id)
             :cloze-model (settings/get-anki-cloze-model user-id)
             :allow-dupes (settings/get-anki-allow-dupes user-id)
             :use-header (settings/get-anki-use-header user-id)
             :header-text (settings/get-anki-header-text user-id)
             :use-tags (settings/get-anki-use-tags user-id)
             :tags (settings/get-anki-tags user-id)
             :basic-fields (settings/get-anki-basic-fields user-id)
             :cloze-fields (settings/get-anki-cloze-fields user-id)}}
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
  "One-shot resolution of the modal's initial selection (deck / note types /
   scope / dupes / tags), so the form paints once with correct values instead
   of first-defaulting then overriding.

   Precedence (the first list item is applied client-side, since deck/model
   lists come from AnkiConnect and need validation there):
     deck         : per-doc preset > last-used (skipped in 'none')
     basic/cloze  : per-doc preset > Settings default
     dupes/tags   : per-doc preset > global last-used
     scope        : global last-used only (per-push choice, not per-item —
                    the modal saves it to global at handoff and it must win)
   Settings note-type/field defaults always seed (even in 'none'); 'none' only
   skips the per-doc preset and the last-used deck.

   :preset? reports whether a per-doc preset was applied (drives the
   'saved settings for this document' indicator). Field ordering is resolved
   separately by resolve-preferred-fields."
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
       :basic-model (or (:basic-model preset) (:basic-model global))
       :cloze-model (or (:cloze-model preset) (:cloze-model global))
       :allow-dupes (if (some? (:allow-dupes preset)) (:allow-dupes preset) (:allow-dupes global))
       :use-tags    (if (some? (:use-tags preset)) (:use-tags preset) (:use-tags global))
       :tags        (or (:tags preset) (:tags global))})
    (catch Exception e
      (tel/error! {:id ::resolve-modal-prefs} e)
      {:mode "global" :preset? false})))

(defn resolve-preferred-fields
  "Resolve preferred field ordering for a doc. Lookup order:
     per-doc preset → user-level setting → empty.
   kind is :basic or :cloze. Called ONCE at modal open from
   AnkiSyncSyncBody; the result is passed into run-fetch-fields! so it
   overrides the fetched ordering when valid against the model's fields."
  [user-id root-topic-id kind]
  (try
    (let [preset (when root-topic-id (settings/get-anki-preset user-id root-topic-id))
          from-preset (case kind
                        :basic (:basic-fields preset)
                        :cloze (:cloze-fields preset))]
      (if (seq from-preset)
        (vec from-preset)
        (case kind
          :basic (settings/get-anki-basic-fields user-id)
          :cloze (settings/get-anki-cloze-fields user-id))))
    (catch Exception e
      (tel/error! {:id ::resolve-preferred-fields :data {:kind kind}} e)
      [])))

(defn- attach-card-bibliography
  "Assoc :fm/bibliography-html onto each card, resolved from the card's OWN
   topic via the effective-source walk (own source_id, else nearest ancestor).
   Resolves once per distinct topic, so a whole-document push is O(topics),
   not O(cards).
   Pre:  cards carry :flashcards/topic_id and/or :flashcards/root_topic_id.
   Post: every card gains :fm/bibliography-html (string or nil)."
  [cards]
  (let [topic-of      (fn [c] (or (:flashcards/topic_id c) (:flashcards/root_topic_id c)))
        html-by-topic (reduce (fn [m tid]
                                (if (contains? m tid)
                                  m
                                  (assoc m tid
                                    (some-> (db/resolve-effective-source-id tid)
                                      db/get-source
                                      :sources/csl
                                      helpers/format-bibliography-html))))
                        {}
                        (keep topic-of cards))]
    (mapv #(assoc % :fm/bibliography-html (get html-by-topic (topic-of %))) cards)))

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
    (let [cards (attach-occlusion-groups
                  (attach-card-bibliography
                    (case scope
                      "self"     (db/get-flashcards topic-id)
                      "subtree"  (db/get-flashcards-for-subtree topic-id)
                      "document" (db/get-all-flashcards root-topic-id)
                      (if topic-id
                        (db/get-flashcards topic-id)
                        (db/get-all-flashcards root-topic-id)))))
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
       :bibliography-html bib-html})
    (catch Exception e
      (tel/error! {:id ::get-cards-for-sync} e)
      {:success false :error (.getMessage e)})))

(defn record-pushed-notes
  "After push, bulk-save Anki note IDs.
   pairs: [{:card-id N :anki-note-id M} ...]"
  [pairs]
  (try
    (db/set-anki-note-ids
      (mapv (fn [{:keys [card-id anki-note-id]}]
              [card-id anki-note-id])
        pairs))
    {:success true :count (count pairs)}
    (catch Exception e
      (tel/error! {:id ::record-pushed-notes} e)
      {:success false :error (.getMessage e)})))

(defn finalize-push!
  "Atomically (per-server-call) record pushed notes, save global last-used,
   save per-item preset (when mode=\"per-item\"), and bump :sync-mutations.
   One e/server round-trip from the client so the reactive graph observes
   one settled result — Electric drops unused intermediate side-effects
   when multiple sibling e/server calls sit in a do-body."
  [user-id root-topic-id pairs prefs-map auto-load-mode]
  (tel/log! {:level :info
             :id ::finalize-push!
             :data {:user-id user-id
                    :root-topic-id root-topic-id
                    :pair-count (count pairs)
                    :auto-load-mode auto-load-mode
                    :prefs-keys (vec (keys prefs-map))
                    :deck (:deck prefs-map)
                    :basic-model (:basic-model prefs-map)
                    :cloze-model (:cloze-model prefs-map)}}
    "finalize-push! invoked")
  (try
    (db/set-anki-note-ids
      (mapv (fn [{:keys [card-id anki-note-id]}]
              [card-id anki-note-id])
        pairs))
    (tel/log! {:level :info :id ::finalize-push!.note-ids-saved} "anki note ids saved")
    (settings/save-anki-sync-settings user-id prefs-map)
    (tel/log! {:level :info :id ::finalize-push!.global-saved} "global last-used saved")
    ;; Header is per-PDF, auto-saved on edit by HeaderSettings — not persisted here.
    (when (= auto-load-mode "per-item")
      (let [existing (or (settings/get-anki-preset user-id root-topic-id) {})
            new-preset (merge existing prefs-map)]
        (settings/save-anki-preset user-id root-topic-id new-preset))
      (tel/log! {:level :info
                 :id ::finalize-push!.per-item-saved
                 :data {:root-topic-id root-topic-id}}
        "per-item preset saved"))
    (swap! (us/get-atom user-id :sync-mutations) inc)
    (tel/log! {:level :info :id ::finalize-push!.done} "finalize-push! complete")
    {:success true :count (count pairs)}
    (catch Exception e
      (tel/error! {:id ::finalize-push!} e)
      {:success false :error (.getMessage e)})))

;; ---------------------------------------------------------------------------
;; Library cards Anki overlay (anki-modified / marked / suspended + F4 deletes)
;; ---------------------------------------------------------------------------

(defn- stripped=
  "Compare a client-stripped Anki field value against local field HTML,
   stripping the local side the same way. nil and \"\" are equal."
  [anki-val local-html]
  (= (or anki-val "")
    (or (helpers/strip-html (or local-html "")) "")))

(defn apply-anki-overlay!
  "Diff live Anki note state against local cards; delete cards whose
   notes no longer exist in Anki.

   pre:  present-notes [{:note-id long
                         :stripped-fields [str ...]   ; strip-html'd, Anki field order
                         :tags [str ...]
                         :suspended {:total n :suspended n}} ...]
         absent-note-ids [long ...] — note ids notesInfo reported missing.
         Both derive from ONE notesInfo batch on the client.
   post: {:success true
          :per-card {card-id {:anki-modified bool, :marked bool,
                              :suspended {:total n :suspended n}}}  ; SPARSE — flagged cards only
          :deleted-count n}
   invariants: reads/deletes only user-id's cards; bumps :sync-mutations
   iff deleted-count > 0 (loop prevention — a no-delete pass must not
   re-trigger the overlay); idempotent for the same input (absent cards
   already deleted produce a no-op second pass).
   Deletion is unconditional by product decision: a successful notesInfo
   reply from a wrong/empty collection (fresh install, other profile)
   will wipe the pushed cards — that risk is on the user.
   _rev is unused — callers pass a mutation revision so Electric re-runs
   the diff when LOCAL content changes under an unchanged Anki payload
   (work-skipping would otherwise hold stale flags)."
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
                              ;; path (io_fields); the overlay only surfaces
                              ;; marked/suspended for these rows.
                              (= "occlusion" kind) false
                              basic? (or (not (stripped= (first stripped-fields) (:flashcards/question card)))
                                       (not (stripped= (second stripped-fields) (:flashcards/answer card))))
                              :else (not (stripped= (first stripped-fields) (:flashcards/cloze card))))
                            marked? (boolean (some #{"marked"} tags))
                            suspended-n (or (:suspended suspended) 0)
                            flags (cond-> {}
                                    anki-modified? (assoc :anki-modified true)
                                    marked? (assoc :marked true)
                                    (pos? suspended-n) (assoc :suspended suspended))]
                        (when (seq flags)
                          [(:flashcards/id card) flags])))))
            present-notes)
          ;; F4: delete locally what Anki no longer has.
          absent-rows (db/get-flashcards-by-anki-note-ids user-id absent-note-ids)
          deleted-count (do (doseq [r absent-rows]
                              (db/delete-flashcard! (:flashcards/id r)))
                          (count absent-rows))]
      (when (pos? deleted-count)
        (swap! (us/get-atom user-id :sync-mutations) inc))
      (tel/log! {:level :debug
                 :id ::apply-anki-overlay!
                 :data {:user-id user-id
                        :present (count present-notes)
                        :flagged (count per-card)
                        :deleted deleted-count}}
        "anki overlay applied")
      {:success true
       :per-card per-card
       :deleted-count deleted-count})
    (catch Exception e
      (tel/error! {:id ::apply-anki-overlay!} e)
      {:success false :error (.getMessage e)})))

;; ---------------------------------------------------------------------------
;; Library cards bulk actions (selection-based push / pull)
;; ---------------------------------------------------------------------------

(defn get-bulk-push-bundles
  "Group the user's selected, already-pushed cards by root topic and resolve
   each root's push settings — the per-root preset wins over user-level
   prefs, mirroring what the sync modal would use for that document.
   pre:  card-ids — the selection; ownership enforced in the lookup.
   post: {:success true
          :bundles [{:root-topic-id N :cards [...] :settings {...}}]
          :skipped-unpushed n}
   Settings carry everything build-update-fields reads (fields, header,
   source/bibliography/image modes, topic title/kind, app-base-url) plus
   :tags and the model names (client falls back to modelFieldNames when a
   stored field ordering is empty). Deck is omitted — updateNote does not
   move cards between decks."
  [user-id card-ids]
  (try
    (let [global (:prefs (load-anki-preferences user-id))
          rows (attach-occlusion-groups (db/get-flashcards-by-ids user-id card-ids))
          pushed (filterv :flashcards/anki_note_id rows)
          by-root (group-by :flashcards/root_topic_id pushed)
          bundles
          (mapv
            (fn [[root-id cards]]
              (let [preset (or (load-item-preset user-id root-id) {})
                    pick (fn [k] (let [v (get preset k)] (if (some? v) v (get global k))))
                    root-topic (db/get-topic-for-user user-id root-id)
                    source (when-let [sid (:topics/source_id root-topic)]
                             (db/get-source sid))
                    bib-html (helpers/format-bibliography-html (:sources/csl source))]
                {:root-topic-id root-id
                 :cards (attach-card-bibliography (vec cards))
                 :settings {:basic-model (pick :basic-model)
                            :cloze-model (pick :cloze-model)
                            :basic-fields (vec (resolve-preferred-fields user-id root-id :basic))
                            :cloze-fields (vec (resolve-preferred-fields user-id root-id :cloze))
                            :use-header (pick :use-header)
                            :header-text (pick :header-text)
                            :tags (vec (or (pick :tags) []))
                            :source-display-mode (settings/get-source-display-mode user-id)
                            :source-field (settings/get-anki-source-field user-id)
                            :bibliography-display-mode (settings/get-bibliography-display-mode user-id)
                            :bibliography-field-name (settings/get-bibliography-field-name user-id)
                            :bibliography-html bib-html
                            :images-front-field (settings/get-anki-images-front-field user-id)
                            :images-back-field (settings/get-anki-images-back-field user-id)
                            :image-display-mode (settings/get-image-display-mode user-id)
                            :topic-title (:topics/title root-topic)
                            :topic-kind (:topics/kind root-topic)
                            :root-topic-id root-id
                            :app-base-url settings/app-base-url}}))
            by-root)]
      {:success true
       :bundles bundles
       :skipped-unpushed (- (count rows) (count pushed))})
    (catch Exception e
      (tel/error! {:id ::get-bulk-push-bundles} e)
      {:success false :error (.getMessage e)})))

(defn get-cards-for-bulk-pull
  "Selected cards eligible for a bulk pull.
   pre:  card-ids — the selection; anki-modified-ids — card ids the client
   overlay marked as edited in Anki.
   include-conflicts? false skips only TRUE conflicts (edited on both
   sides). A card edited only in FreeMemo pulls bare — pulling it is an
   explicit discard of the local edit in favor of Anki's version.
   post: {:success true :cards [...] :skipped-conflicts n}; cards without
   an anki_note_id are dropped silently (nothing to pull)."
  [user-id card-ids include-conflicts? anki-modified-ids]
  (try
    (let [anki-mod (set anki-modified-ids)
          rows (db/get-flashcards-by-ids user-id card-ids)
          pushed (filterv :flashcards/anki_note_id rows)
          local-edit? (fn [c]
                        (let [u (:flashcards/updated_at c)
                              s (:flashcards/anki_synced_at c)]
                          (and u s (pos? (compare (str u) (str s))))))
          conflict? (fn [c]
                      (and (local-edit? c)
                        (contains? anki-mod (:flashcards/id c))))
          eligible (if include-conflicts?
                     pushed
                     (filterv (complement conflict?) pushed))]
      {:success true
       :cards eligible
       :skipped-conflicts (- (count pushed) (count eligible))})
    (catch Exception e
      (tel/error! {:id ::get-cards-for-bulk-pull} e)
      {:success false :error (.getMessage e)})))

(defn finalize-bulk-push!
  "Record a bulk push: refresh anki_note_id + anki_synced_at for pairs and
   bump :sync-mutations ONCE. Unlike finalize-push!, deliberately does NOT
   save global last-used prefs or per-item presets — a bulk update is not a
   preference-setting event.
   pairs: [{:card-id N :anki-note-id M} ...]"
  [user-id pairs]
  (try
    (db/set-anki-note-ids
      (mapv (fn [{:keys [card-id anki-note-id]}] [card-id anki-note-id]) pairs))
    (swap! (us/get-atom user-id :sync-mutations) inc)
    {:success true :count (count pairs)}
    (catch Exception e
      (tel/error! {:id ::finalize-bulk-push!} e)
      {:success false :error (.getMessage e)})))

(defn apply-pull-updates
  "After pull, update card content from Anki edits and delete locally cards removed from Anki.
   Bumps :sync-mutations so the client toolbar refreshes counts.
   updates: [{:card-id N :question Q :answer A :cloze C} ...] or
            [{:card-id N :io-fields {partial map}} ...] for occlusion cards
   deleted-card-ids: [N ...] — cards whose Anki notes no longer exist"
  [user-id updates deleted-card-ids]
  (try
    (doseq [{:keys [card-id question answer cloze io-fields]} updates]
      (if (seq io-fields)
        ;; Occlusion: shallow JSONB merge — unchanged keys keep local values.
        (db/merge-flashcard-io-fields! card-id io-fields)
        (let [fields (cond-> {}
                       question (assoc :question question)
                       answer (assoc :answer answer)
                       cloze (assoc :cloze cloze))]
          (when (seq fields)
            (db/update-flashcard! card-id fields)
            (db/mark-anki-synced card-id)))))
    (when (seq deleted-card-ids)
      (doseq [id deleted-card-ids]
        (db/delete-flashcard! id)))
    (swap! (us/get-atom user-id :sync-mutations) inc)
    {:success true :count (count updates) :deleted (count (or deleted-card-ids []))}
    (catch Exception e
      (tel/error! {:id ::apply-pull-updates} e)
      {:success false :error (.getMessage e)})))
