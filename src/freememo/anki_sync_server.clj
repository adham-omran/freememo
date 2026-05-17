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
             :tags (settings/get-anki-tags user-id)}}
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
  "Get flashcards for Anki sync. Also returns the current root topic title/kind
   and pre-resolved bibliography (text + HTML) so the Source field and the
   Bibliography field/append reflect edits made since the modal was opened.
   opts: {:user-id N, :topic-id N, :root-topic-id N}
   When topic-id is nil, returns all cards for the root topic."
  [{:keys [user-id topic-id root-topic-id]}]
  (tel/log! {:level :info
             :id ::get-cards-for-sync.entry
             :data {:user-id user-id
                    :topic-id topic-id
                    :root-topic-id root-topic-id}}
    "get-cards-for-sync invoked")
  (try
    (let [cards (if topic-id
                  (db/get-flashcards topic-id)
                  (db/get-all-flashcards root-topic-id))
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
    (when (= auto-load-mode "per-item")
      (settings/save-anki-preset user-id root-topic-id prefs-map)
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

(defn apply-pull-updates
  "After pull, update card content from Anki edits and delete locally cards removed from Anki.
   Bumps :sync-mutations so the client toolbar refreshes counts.
   updates: [{:card-id N :question Q :answer A :cloze C} ...]
   deleted-card-ids: [N ...] — cards whose Anki notes no longer exist"
  [user-id updates deleted-card-ids]
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
    (swap! (us/get-atom user-id :sync-mutations) inc)
    {:success true :count (count updates) :deleted (count (or deleted-card-ids []))}
    (catch Exception e
      (tel/error! {:id ::apply-pull-updates} e)
      {:success false :error (.getMessage e)})))
