(ns freememo.neoreader-sync
  "Run one NeoReader sync for one EPUB document.

   The single orchestration layer: read the bound export from Drive, decode it,
   place every highlight in the document's chapters, and write back the chapters
   that changed. The decode lives in `freememo.neoreader-export` and the
   placement in `freememo.quote-anchor`; both are pure and neither knows about
   Drive or the database.

   Marks only. The `USER NOTES` section is not written here — how a re-sync finds
   an existing section in order to replace it is an open decision
   (plans/boox-neoreader-google-drive-sync.md item 6.4), and writing a section
   with no way to find it again would append a second copy on the next sync.
   Notes ARE decoded and counted, so nothing is lost by waiting.

   Marks are regenerated, never merged. Each sync strips every existing tint of
   `quote-anchor/highlight-color` from a chapter before applying the current
   export, so a highlight deleted on the device loses its mark — the export is
   the truth, per the sync's decisions. A tint the user applied by hand with the
   Quill popover's Orange is indistinguishable from a synced one and is stripped
   too; that is the cost of carrying no identity in the content."
  (:require
    [clojure.string :as str]
    [taoensso.telemere :as tel]
    [freememo.commands :as commands]
    [freememo.db :as db]
    [freememo.google-drive :as drive]
    [freememo.neoreader-export :as export]
    [freememo.quote-anchor :as anchor])
  (:import [org.jsoup Jsoup]
           [org.jsoup.nodes Element]))

(def file-id-setting-prefix
  "Settings-key prefix binding an EPUB document to a Drive file.

   The suffix is the root topic id, so this prefix MUST have an entry in
   `freememo.account-import/settings-key-prefixes`. Without one, an imported
   preference points at another user's topic (§5.4.8)."
  "boox_file_")

(defn file-id-setting-key [topic-id] (str file-id-setting-prefix topic-id))

(defn bound-file-id
  "Pre : `topic-id` is an EPUB root topic owned by `user-id`.
   Post: the Drive file id bound to it, or nil when it was never bound."
  [user-id topic-id]
  (let [v (db/get-setting user-id (file-id-setting-key topic-id))]
    (when-not (str/blank? v) v)))

(defn bind-file!
  "Pre : `file-id` was picked by this user through the Google Picker, so the app
         holds per-file access to it. `topic-id` is an EPUB root topic.
   Post: later syncs of `topic-id` read that file with no Picker interaction."
  [user-id topic-id file-id]
  (db/set-setting user-id (file-id-setting-key topic-id) file-id))

;; ── Chapters ───────────────────────────────────────────────────────

(defn- epub-chapters
  "The chapter children of an EPUB root, in reading order.

   Pre : `topic-id` is a topic of kind `epub` owned by `user-id`.
   Post: a vector of {:id :title :html}, ordered by `page_number` — the order
         `db/create-epub-topic!` assigned from the spine. Converting the
         namespace-qualified row keys is this function's job, because
         `quote-anchor` takes plain `:title`/`:html` and owes no knowledge of
         topic rows."
  [topic-id]
  (into []
    (map (fn [c] {:id (:topics/id c)
                  :title (:topics/title c)
                  :html (:topics/content c)}))
    (db/get-children topic-id)))

(defn- strip-marks
  "Remove every synced tint span from `html`, keeping its text.

   Pre : `html` is a chapter's stored content, or nil.
   Post: the same content with each `background-color: <highlight-color>` span
         unwrapped. Returns nil unchanged, so an empty chapter stays empty.
   Inv : only spans carrying exactly that colour are touched. An extract
         highlight (`#44C2FF`) and every other inline colour survive."
  [html]
  (when (some? html)
    (if (str/blank? html)
      html
      (let [doc (Jsoup/parseBodyFragment html)
            body (.body doc)]
        (doseq [^Element el (.select body (str "span[style*=" (subs anchor/highlight-color 1) "]"))]
          (.unwrap el))
        (.html body)))))

;; ── One sync ───────────────────────────────────────────────────────

(defn- outcome-counts
  "Pre : `placements` is `quote-anchor/place-highlights` output.
   Post: {:placed n :fallback n :unmatched n}, every key present."
  [placements]
  (merge {:placed 0 :fallback 0 :unmatched 0}
    (frequencies (map :outcome placements))))

(defn sync-document!
  "Sync one EPUB document against its bound NeoReader export.

   Pre : `user-id` owns `topic-id`, and `topic-id` is a topic of kind `epub`
         holding chapter children. A Drive file is bound and the user's Drive
         authorization is live.
   Post: {:ok true :counts {…} :chapters-written n :notes n} on success, or
         {:ok false :error msg :reauthorize? bool}. On success every chapter that
         holds a placement has been rewritten with its marks, and no other
         chapter was touched.
   Post: `:reauthorize?` is true only when the stored Drive credential is gone or
         Google rejected it. The caller shows a re-authorize action for that case
         and a plain error for every other.
   Inv : idempotent. Two runs against the same export leave identical content,
         because every chapter is stripped of synced tints before the current
         export is applied.
   Inv : the invalidation bump goes through `freememo.commands`, the single bump
         authority. Nothing here swap!s a channel atom."
  [user-id topic-id]
  (if-let [file-id (bound-file-id user-id topic-id)]
    (if-let [token (drive/access-token user-id)]
      (let [{:keys [ok html error]} (drive/export-html token file-id)]
        (if-not ok
          {:ok false :error error}
          (let [highlights (export/parse-highlights html)]
            (if (empty? highlights)
              {:ok false :error "That file holds no NeoReader highlights"}
              (let [chapters (epub-chapters topic-id)]
                (if (empty? chapters)
                  {:ok false :error "This document has no chapters to sync into"}
                  ;; Strip first, then place against the stripped text. Placing
                  ;; against marked HTML would work — a tint span adds no text —
                  ;; but the offsets would then index text the write no longer
                  ;; holds, and every chapter has to be stripped anyway.
                  (let [clean (mapv #(update % :html strip-marks) chapters)
                        placements (anchor/place-highlights clean highlights)
                        marked (anchor/mark-chapters clean placements)
                        ;; A chapter that lost marks and gained none still needs
                        ;; its stripped HTML written, or a deleted highlight keeps
                        ;; its tint. Compare stripped against stored to find those.
                        cleaned-only (into {}
                                       (keep (fn [[pos ch]]
                                               (let [before (:html (nth chapters pos))
                                                     after (:html ch)]
                                                 (when (and (not (contains? marked pos))
                                                         (not= before after))
                                                   [pos after]))))
                                       (map-indexed vector clean))
                        writes (merge cleaned-only marked)]
                    (doseq [[pos html'] writes]
                      (db/update-topic-content! (:id (nth chapters pos)) html'))
                    (commands/bump! user-id :neoreader-sync)
                    (let [counts (outcome-counts placements)]
                      (tel/log! {:level :info :id ::synced
                                 :data {:user-id user-id :topic-id topic-id
                                        :counts counts :chapters (count writes)}}
                        "NeoReader sync complete")
                      {:ok true
                       :counts counts
                       :chapters-written (count writes)
                       :notes (reduce + (map #(count (:notes %)) highlights))}))))))))
      {:ok false :reauthorize? true
       :error "Google Drive access has expired — reconnect to sync"})
    ;; `:needs-pick?` and not an error: the browser answers it by opening the
    ;; Picker and calling back, so the first sync of a document is one click.
    {:ok false :needs-pick? true
     :error "Choose the NeoReader file for this document"}))

(defn summary
  "One line describing a finished sync, for a toast.

   Pre : `result` is a `sync-document!` map with `:ok` true.
   Post: a sentence naming what was placed, what was searched for, what was not
         found, and how many notes were read but not yet shown — notes are
         decoded and counted while item 6.4 is open, and saying so keeps a user
         from thinking they were lost."
  [{:keys [counts chapters-written notes]}]
  (let [{:keys [placed fallback unmatched]} counts]
    (str "Synced " (+ placed fallback) " highlights"
      (when (pos? fallback) (str ", " fallback " found by search"))
      " across " chapters-written (if (= 1 chapters-written) " chapter" " chapters")
      (when (pos? unmatched) (str ". " unmatched " could not be located"))
      (when (pos? notes) (str ". " notes " notes read, not shown yet")))))
