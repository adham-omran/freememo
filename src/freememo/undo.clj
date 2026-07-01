(ns freememo.undo
  "Reverse side of the Undo feature: replay a logged delete from its snapshot.
   Forward logging lives at each delete site (see cards/delete-card,
   pin-side-panel/remove-pin!*, settings/reset-*-prompt); this ns only undoes.

   Contract: undo-entry! is the single dispatch point. It restores the
   snapshot, stamps undone_at, bumps the matching per-user mutation channel so
   live views re-query, and bumps :undo-mutations so the Actions modal
   refreshes. Anki notes deleted client-side cannot be re-added server-side, so
   restoring an Anki-synced card surfaces a divergence warning toast."
  (:require
   [freememo.db :as db]
   [freememo.user-state :as us]
   [freememo.toasts :as toasts]
   [taoensso.telemere :as tel]))

(defn- anki-divergence?
  "True when restoring would leave FreeMemo cards without their Anki notes.
   For documents the cards live under the snapshot's :cards key."
  [entry]
  (case (:entity_type entry)
    "flashcard" (boolean (some :anki_note_id (:snapshot entry)))
    "document"  (boolean (some :anki_note_id (:cards (:snapshot entry))))
    false))

(defn- restore!
  "Apply the inverse of entry. Returns the db result map for documents
   (carries :over-quota?), nil otherwise."
  [user-id entry]
  (case (:action_type entry)
    ("delete-card" "bulk-delete-cards") (do (db/restore-flashcards! (:snapshot entry)) nil)
    "remove-pin"      (do (db/restore-pin! (first (:snapshot entry))) nil)
    "reset-prompt"    (let [{:keys [key value]} (first (:snapshot entry))]
                        (db/set-setting user-id key value) nil)
    "delete-document" (db/restore-staged-document! user-id entry)
    "move-topic"      (do (db/restore-topic-parent! entry) nil)))

(defn- bump-views! [user-id entry]
  (case (:entity_type entry)
    "flashcard" (swap! (us/get-atom user-id :card-mutations) inc)
    "pin"       (swap! (us/get-atom user-id :pin-mutations) inc)
    "setting"   (swap! (us/get-atom user-id :settings-refresh) inc)
    "document"  (do (swap! (us/get-atom user-id :card-mutations) inc)
                  (swap! (us/get-atom user-id :tree-mutations) inc)
                  (swap! (us/get-atom user-id :refresh) inc))))

(defn undo-entry!
  "Reverse a single undo entry owned by user-id.
   Returns {:success bool, :nothing? bool, :warning? bool, :error str}.
   Idempotent: undoing an already-undone or missing entry is a safe no-op."
  [user-id entry-id]
  (try
    (let [entry (db/get-undo-entry entry-id)]
      (cond
        (or (nil? entry) (not= (:user_id entry) user-id))
        {:success false :error "Action not found"}

        (:undone_at entry)
        {:success true :nothing? true}

        :else
        (let [warn? (boolean (anki-divergence? entry))
              result (restore! user-id entry)
              over-quota? (boolean (:over-quota? result))]
          (db/mark-undone! entry-id)
          (bump-views! user-id entry)
          (swap! (us/get-atom user-id :undo-mutations) inc)
          (when warn?
            (toasts/push! user-id
              {:level :warning
               :message "Restored in FreeMemo, but the Anki note(s) were not re-added."}))
          (when over-quota?
            (toasts/push! user-id
              {:level :warning
               :message "Document restored — you're now over your storage quota."}))
          {:success true :warning? warn? :over-quota? over-quota?})))
    (catch Exception e
      (tel/error! {:id ::undo-entry} e)
      {:success false :error "Undo failed"})))

(defn undo-newest!
  "Reverse the user's most recent live action (LIFO). When the log is empty,
   surfaces a 'Nothing to undo' toast and returns {:success true :nothing? true}."
  [user-id]
  (if-let [newest (first (db/get-undo-entries user-id))]
    (undo-entry! user-id (:id newest))
    (do (toasts/push! user-id {:level :info :message "Nothing to undo"})
        {:success true :nothing? true})))
