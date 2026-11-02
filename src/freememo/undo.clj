(ns freememo.undo
  "Reverse side of the Undo feature: replay a logged delete from its snapshot.
   Forward logging lives at each delete site (see cards/delete-card-row!,
   pin-side-panel/remove-pin!*, settings/reset-*-prompt); this ns only undoes.

   Contract: undo-entry! is the single dispatch point. It restores the
   snapshot, stamps undone_at, bumps the matching per-user mutation channel so
   live views re-query, and bumps :undo-mutations so the Actions modal
   refreshes. Anki notes deleted client-side cannot be re-added server-side, so
   restoring an Anki-synced card returns a divergence warning.

   undo-entry! returns {:ok? bool :error str? :warning str?} and shows no
   message itself. The caller shows the result: UndoRow on its row, the Cmd-Z command
   through the activity banner."
  (:require
   [clojure.string :as str]
   [freememo.activity :as activity]
   [freememo.db :as db]
   [freememo.article-split :as article-split]
   [freememo.commands :as commands]
   [freememo.curriculum :as curriculum]
   [freememo.optimistic :as opt]
   [freememo.undo-scope :as scope]
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
    "delete-assistant-chat" (do (db/restore-staged-assistant-chat! user-id entry) nil)
    "move-topic"      (do (db/restore-topic-parent! entry) nil)
    "split-article"   (article-split/undo-batch! user-id entry)
    "reject-question" (do (db/restore-rejected-question! (:snapshot entry)) nil)
    "reject-fact"     (do (db/restore-rejected-fact! (:snapshot entry)) nil)
    ;; One entry covering many questions; each gets its own prior status back.
    "bulk-reject-questions" (do (db/restore-rejected-questions! (:snapshot entry)) nil)
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
    (conj (if (= "split-article" (:action_type entry))
            #{:card-mutations :tree-mutations :pin-mutations}
            (commands/entity-views (:entity_type entry) #{}))
      :undo-mutations)))

(defn- join-warnings [& ws]
  (when-let [ws (seq (remove nil? ws))]
    (str/join " " ws)))

(defn undo-entry!
  "Reverse a single undo entry owned by user-id.
   Returns {:ok? false :error str} when nothing was restored,
   {:ok? true :nothing? true} when the entry was already undone,
   {:ok? true :warning str} on a partial restore, and {:ok? true} otherwise.
   :success mirrors :ok? for callers that predate it.
   Idempotent: undoing an already-undone entry is a safe no-op."
  [user-id entry-id]
  (try
    (let [entry (db/get-undo-entry entry-id)
          fail (fn [msg] {:ok? false :success false :error msg})]
      (cond
        (or (nil? entry) (not= (:user_id entry) user-id))
        (fail "Action not found")

        (:undone_at entry)
        {:ok? true :success true :nothing? true}

        (not (db/undo-entry-within-window? entry-id))
        (fail "The 12-hour recovery window has ended.")

        :else
        (let [anki-warn? (boolean (anki-divergence? entry))
              result (restore! user-id entry)
              source-highlights-kept? (boolean (:source-highlights-kept? result))
              over-quota? (boolean (:over-quota? result))
              warning (join-warnings
                        (when anki-warn?
                          "Restored in FreeMemo, but the Anki notes were not added again.")
                        (when source-highlights-kept?
                          "Topics removed. Source highlights were kept because the article changed.")
                        (when over-quota?
                          "Document restored. You are now over your storage quota."))]
          (db/mark-undone! entry-id)
          (log/audit! {:id ::undo :user-id user-id :action :restore
                       :entity (keyword (:entity_type entry)) :entity-id entry-id})
          (bump-views! user-id entry)
          (cond-> {:ok? true :success true :over-quota? over-quota?}
            warning (assoc :warning warning :warning? true)
            source-highlights-kept? (assoc :source-highlights-kept? true)))))
    (catch Exception e
      (tel/error! {:id ::undo-entry} e)
      {:ok? false :success false :error "Undo failed"})))

(defn undo-newest!
  "Reverse the newest live action whose subject the view shows.
   `shown` is the collection of subjects from undo-scope/shown-undo-subjects.
   With no entry in view it does nothing and returns
   {:ok? true :nothing? true}: no message, because the view never offered one."
  [user-id shown]
  (let [shown (set shown)
        parent-of (memoize db/topic-parent-id)
        match (when (seq shown)
                (first (filter #(scope/shown? % shown parent-of)
                         (db/get-undo-entries user-id))))]
    (if match
      (undo-entry! user-id (:id match))
      {:ok? true :nothing? true})))

;; ── Command methods ─────────────────────────────────────────────────────────
;; Effects only: execute! owns queue removal; bumps happen inside undo-entry!
;; because the channels are only known once the entry's entity type is read.
;; The dispatcher records {:ok? false}. A Cmd-Z partial restore has no control
;; on screen, so :undo-newest records its warning itself.

(defmethod opt/run-command! :undo-newest [user-id command]
  (let [r (undo-newest! user-id (get-in command [:payload :shown]))]
    (when-let [w (:warning r)]
      (activity/record-failure! user-id {:error w :what "Part of the undo"}))
    (select-keys r [:ok? :error])))

(defmethod opt/run-command! :undo-entry [user-id command]
  (let [r (undo-entry! user-id (get-in command [:payload :id]))]
    (select-keys r [:ok? :error :warning])))
