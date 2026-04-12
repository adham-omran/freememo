(ns freememo.anki-sync-server
  "Server-side Anki sync operations — fetching cards for sync,
   recording pushed note IDs, and applying pull updates."
  (:require
   [freememo.db :as db]
   [freememo.settings :as settings]
   [taoensso.telemere :as tel]))

(defn load-anki-preferences
  "Load saved Anki sync preferences for a user (global)."
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
             :source-field (settings/get-anki-source-field user-id)}}
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

(defn get-cards-for-sync
  "Get flashcards for Anki sync.
   opts: {:topic-id N, :root-topic-id N}
   When topic-id is nil, returns all cards for the root topic."
  [{:keys [topic-id root-topic-id]}]
  (try
    (let [cards (if topic-id
                  (db/get-flashcards topic-id)
                  (db/get-all-flashcards root-topic-id))
          cards (mapv (fn [c]
                        (let [synced (:flashcards/anki_synced_at c)
                              updated (:flashcards/updated_at c)]
                          (assoc c :needs-update?
                            (and (some? (:flashcards/anki_note_id c))
                              (or (nil? synced)
                                (and (some? updated)
                                  (pos? (compare (str updated)
                                          (str synced)))))))))
                  cards)]
      {:success true :cards cards})
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

(defn apply-pull-updates
  "After pull, update card content from Anki edits and delete locally cards removed from Anki.
   updates: [{:card-id N :question Q :answer A :cloze C} ...]
   deleted-card-ids: [N ...] — cards whose Anki notes no longer exist"
  [updates deleted-card-ids]
  (try
    (doseq [{:keys [card-id question answer cloze]} updates]
      (let [fields (cond-> {}
                     question (assoc :question question)
                     answer (assoc :answer answer)
                     cloze (assoc :cloze cloze))]
        (when (seq fields)
          (db/update-flashcard! card-id fields)
          (db/mark-anki-synced card-id))))
    (when (seq deleted-card-ids)
      (doseq [id deleted-card-ids]
        (db/delete-flashcard! id)))
    {:success true :count (count updates) :deleted (count (or deleted-card-ids []))}
    (catch Exception e
      (tel/error! {:id ::apply-pull-updates} e)
      {:success false :error (.getMessage e)})))
