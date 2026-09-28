(ns freememo.undo
  "Reverse side of the Undo feature: replay a logged delete from its snapshot.
   Forward logging lives at each delete site (see cards/delete-card-row!,
   pin-side-panel/remove-pin!*, settings/reset-*-prompt); this ns only undoes.

   Contract: undo-entry! is the single dispatch point. It restores the
   snapshot, stamps undone_at, bumps the matching per-user mutation channel so
   live views re-query, and bumps :undo-mutations so the Actions modal
   refreshes. Anki notes deleted client-side cannot be re-added server-side, so
   restoring an Anki-synced card surfaces a divergence warning toast."
  (:require
   [freememo.db :as db]
   [freememo.commands :as commands]
   [freememo.curriculum :as curriculum]
   [freememo.optimistic :as opt]
   [freememo.toasts :as toasts]
   [freememo.logging :as log]
   [taoensso.telemere :as tel]))

(defn- anki-divergence?
  "True when restoring would leave FreeMemo cards without their Anki notes.
   For documents the cards live under the snapshot's :cards key."
  [entry]
  (case (:entity_type entry)
    "flashcard" (boolean (some :anki_note_id (db/snapshot-cards (:snapshot entry))))
    "document"  (boolean (some :anki_note_id (:cards (:snapshot entry))))
    false))

(defn- restore!
  "Apply the inverse of entry. Returns the db result map for documents
   (carries :over-quota?), nil otherwise."
  [user-id entry]
  (case (:action_type entry)
    ;; Map snapshot = cards plus any group the delete destroyed, restored group
    ;; row first (FK order). Vector = the pre-collapse shape, cards only.
    ("delete-card" "bulk-delete-cards")
    (let [s (:snapshot entry)]
      (if (map? s) (db/restore-card-rows! s) (db/restore-flashcards! s))
      nil)
    "remove-pin"      (do (db/restore-pin! (first (:snapshot entry))) nil)
    ;; The prompt and its base stamp, restored together. Entries written before
    ;; stamps existed hold the prompt alone.
    "reset-prompt"    (do (db/set-settings! user-id
                            (into {} (map (juxt :key :value)) (:snapshot entry)))
                          nil)
    "delete-document" (db/restore-staged-document! user-id entry)
    "delete-curriculum" (curriculum/restore-staged-curriculum! user-id entry)
    "move-topic"      (do (db/restore-topic-parent! entry) nil)
    "reject-question" (do (db/restore-rejected-question! (:snapshot entry)) nil)
    "reject-fact"     (do (db/restore-rejected-fact! (:snapshot entry)) nil)
    ;; One entry covering many facts — the snapshot is per-fact, so each gets its
    ;; own prior status back rather than a shared one. A duplicate sweep writes
    ;; the same snapshot shape through the same db/withdraw-kg-facts!, so it
    ;; restores identically; the two action types stay distinct only so the undo
    ;; history can name which one the user is reversing.
    ("bulk-reject-facts" "mark-duplicate-facts")
    (do (db/restore-withdrawn-facts! (:snapshot entry)) nil)))

(defn- bump-views!
  "The one runtime-dynamic bump: the channels depend on the undone entry's
   entity type (freememo.commands/entity-views), plus :undo-mutations so the
   Actions modal refreshes. Registry :views for the undo commands are empty."
  [user-id entry]
  (commands/bump-channels! user-id
    (conj (commands/entity-views (:entity_type entry) #{}) :undo-mutations)))

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

        (not (db/undo-entry-within-window? entry-id))
        {:success false :error "The 12-hour recovery window has ended."}

        :else
        (let [warn? (boolean (anki-divergence? entry))
              result (restore! user-id entry)
              over-quota? (boolean (:over-quota? result))]
          (db/mark-undone! entry-id)
          (log/audit! {:id ::undo :user-id user-id :action :restore
                       :entity (keyword (:entity_type entry)) :entity-id entry-id})
          (bump-views! user-id entry)
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

;; ── Command methods ─────────────────────────────────────────────────────────
;; Effects only: execute! owns queue removal; bumps happen inside undo-entry!
;; because the channels are only known once the entry's entity type is read.

(defmethod opt/run-command! :undo-newest [user-id _command]
  (undo-newest! user-id)
  {:ok? true})

(defmethod opt/run-command! :undo-entry [user-id command]
  (undo-entry! user-id (get-in command [:payload :id]))
  {:ok? true})
