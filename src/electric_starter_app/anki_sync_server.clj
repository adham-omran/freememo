(ns electric-starter-app.anki-sync-server
  "Server-side Anki sync operations — fetching cards for sync,
   recording pushed note IDs, and applying pull updates."
  (:require
    [electric-starter-app.db :as db]))

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
