(ns electric-starter-app.anki-sync-server
  "Server-side Anki sync operations — fetching cards for sync,
   recording pushed note IDs, and applying pull updates."
  (:require
    [electric-starter-app.db :as db]
    [electric-starter-app.settings :as settings]))

(defn load-anki-preferences
  "Load saved Anki sync preferences for a user."
  [user-id]
  (try
    {:success true
     :prefs {:scope       (settings/get-anki-scope user-id)
             :deck        (settings/get-anki-deck user-id)
             :basic-model (settings/get-anki-basic-model user-id)
             :cloze-model (settings/get-anki-cloze-model user-id)
             :allow-dupes (settings/get-anki-allow-dupes user-id)
             :use-header  (settings/get-anki-use-header user-id)
             :header-text (settings/get-anki-header-text user-id)}}
    (catch Exception e
      (println "ERROR [load-anki-preferences]:" (.getMessage e))
      {:success false :prefs {}})))

(defn get-cards-for-sync
  "Get flashcards for Anki sync.
   opts: {:document-id N, :page-number N-or-nil}
   When page-number is nil, returns all cards for the document."
  [{:keys [document-id page-number]}]
  (try
    (let [cards (if page-number
                  (db/get-flashcards document-id page-number)
                  (db/get-all-flashcards document-id))]
      {:success true :cards cards})
    (catch Exception e
      (println "ERROR [get-cards-for-sync]:" (.getMessage e))
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
      (println "ERROR [record-pushed-notes]:" (.getMessage e))
      {:success false :error (.getMessage e)})))

(defn apply-pull-updates
  "After pull, update card content from Anki edits.
   updates: [{:card-id N :question Q :answer A :cloze C} ...]"
  [updates]
  (try
    (doseq [{:keys [card-id question answer cloze]} updates]
      (let [fields (cond-> {}
                     question (assoc :question question)
                     answer   (assoc :answer answer)
                     cloze    (assoc :cloze cloze))]
        (when (seq fields)
          (db/update-flashcard card-id fields)
          (db/mark-anki-synced card-id))))
    {:success true :count (count updates)}
    (catch Exception e
      (println "ERROR [apply-pull-updates]:" (.getMessage e))
      {:success false :error (.getMessage e)})))
