(ns freememo.neoreader-sync
  "Run one NeoReader sync for one EPUB document.

   The single orchestration layer: read the bound export from Drive, decode it,
   place every highlight in the document's chapters, and write back the chapters
   that changed. The decode lives in `freememo.neoreader-export` and the
   placement in `freememo.quote-anchor`; both are pure and neither knows about
   Drive or the database.

   One sync writes in four places, and `quote-anchor` owns the shape of all four:
   a tint on each placed passage, a `[n]` marker on each noted passage, a
   `USER NOTES` section at the end of each chapter that has notes, and an
   `UNPLACED HIGHLIGHTS` section on the document ROOT naming what nothing could
   place. It also records one row per highlight, which is what an account export
   carries and what a later view would read.

   Everything is regenerated, never merged. Each sync strips every previous mark,
   marker and notes section from a chapter before applying the current export, so
   a highlight deleted on the device loses its mark — the export is the truth, per
   the sync's decisions. A tint the user applied by hand with the Quill popover's
   Orange is indistinguishable from a synced one and is stripped too; that is the
   cost of carrying no identity in the content."
  (:require
    [clojure.string :as str]
    [taoensso.telemere :as tel]
    [freememo.commands :as commands]
    [freememo.db :as db]
    [freememo.google-drive :as drive]
    [freememo.neoreader-export :as export]
    [freememo.quote-anchor :as anchor]))

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

;; ── One sync ───────────────────────────────────────────────────────

(defn- outcome-counts
  "Pre : `placements` is `quote-anchor/place-highlights` output.
   Post: a count for every one of the five outcomes, each key present."
  [placements]
  (merge {:placed 0 :fallback 0 :out-of-order 0 :partial 0 :unmatched 0}
    (frequencies (map :outcome placements))))

(def ^:private max-unplaced-quote-chars
  "How much of an unplaced quote the root's section shows. The line exists so a
   reader can find the passage in their own Boox export, and a few sentences
   identify it. A whole page of them would bury the root topic's own content."
  300)

(defn- unplaced-lines
  "Pre : `highlights` is `parse-highlights` output, and `placements` is
         `place-highlights` output for it, positionally matched.
   Post: one line per :unmatched highlight, in the export's order, naming its
         chapter, its device timestamp and its quote. Empty when every highlight
         placed, which removes the section rather than leaving an empty heading."
  [highlights placements]
  (into []
    (keep (fn [[h p]]
            (when (= :unmatched (:outcome p))
              (let [q (str (:quote h))]
                (str (or (:chapter h) "?") " - " (:timestamp h) " - "
                  (if (> (count q) max-unplaced-quote-chars)
                    (str (subs q 0 max-unplaced-quote-chars) "...")
                    q))))))
    (map vector highlights placements)))

(defn- highlight-rows
  "Pre : `chapters` is `epub-chapters` output, `highlights` is
         `parse-highlights` output, and `placements` matches it positionally.
   Post: one row per highlight for `db/replace-neoreader-highlights!`.
         `chapter_topic_id` and `ordinal` are nil for an :unmatched highlight,
         which has no chapter to sit in.
   Inv : `ordinal` counts placements within a chapter from 1 in READING order —
         the order the marks appear, which is the order the `[n]` markers use. It
         is NOT the export's own order, because the export lists by device
         timestamp and a reader meets the passages in the text's order.
   Note: no resolved offset is stored. That settles spec item 5.2: a stored offset
         goes stale the moment the reader edits the chapter, and every sync
         re-matches anyway because it strips its own marks first — so the column
         would hold a value nothing may trust and nothing reads."
  [chapters highlights placements]
  (let [ordinals (into {}
                   (mapcat (fn [[_ ps]]
                             (map-indexed (fn [i p] [(:index p) (inc i)])
                               (sort-by :start ps))))
                   (group-by :chapter-pos
                     (filter #(some? (:chapter-pos %)) placements)))]
    (mapv (fn [[h p]]
            {:chapter_topic_id (when-let [pos (:chapter-pos p)] (:id (nth chapters pos)))
             :chapter_label (:chapter h)
             :highlighted_at (:timestamp h)
             :quote (:quote h)
             :note (let [joined (str/join " " (remove str/blank? (:notes h)))]
                     (when-not (str/blank? joined) joined))
             :ordinal (get ordinals (:index p))
             :outcome (name (:outcome p))})
      (map vector highlights placements))))

(defn sync-document!
  "Sync one EPUB document against its bound NeoReader export.

   Pre : `user-id` owns `topic-id`, and `topic-id` is a topic of kind `epub`
         holding chapter children. A Drive file is bound and the user's Drive
         authorization is live.
   Post: {:ok true :counts {…} :chapters-written n :notes n} on success, or
         {:ok false :error msg :reauthorize? bool}. On success every chapter that
         holds a placement has been rewritten with its marks, its `[n]` markers
         and its `USER NOTES` section, and no other chapter was touched. The root
         carries an `UNPLACED HIGHLIGHTS` section when anything went unplaced, and
         carries none when nothing did.
   Post: `:reauthorize?` is true only when the stored Drive credential is gone or
         Google rejected it. The caller shows a re-authorize action for that case
         and a plain error for every other.
   Inv : idempotent. Two runs against the same export leave identical content and
         identical rows, because every chapter is stripped of everything a sync
         wrote before the current export is applied.
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
                  ;; Strip first, then place against the stripped text. A tint
                  ;; span adds no text, but a `[n]` marker and a notes section
                  ;; both do — leaving them in would let a quote match its own
                  ;; note instead of its passage.
                  (let [clean (mapv #(update % :html anchor/strip-sync-content) chapters)
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
                        writes (merge cleaned-only marked)
                        ;; The root is not a chapter, so it is a separate write.
                        ;; `replace-section` removes a stale section on its own,
                        ;; which is what makes an empty list the right way to say
                        ;; "everything placed this time".
                        root (db/get-topic topic-id)
                        root-html (anchor/replace-section (:topics/content root)
                                    anchor/unplaced-heading
                                    (unplaced-lines highlights placements))]
                    (doseq [[pos html'] writes]
                      (db/update-topic-content! (:id (nth chapters pos)) html'))
                    (when (not= root-html (:topics/content root))
                      (db/update-topic-content! topic-id root-html))
                    (db/replace-neoreader-highlights! user-id topic-id
                      (highlight-rows chapters highlights placements))
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
   Post: a sentence naming how many highlights were marked, how many needed a
         weaker pass, and how many nothing could place.
   Note: `:fallback` and `:out-of-order` are folded into one \"found by search\"
         figure. The distinction is why a pass was needed, which the reader cannot
         act on. `:partial` stays separate, because it is the one outcome where
         the mark covers LESS than the reader highlighted.
   Note: the unmatched count names the root's section, because a count with no
         place to look is not actionable (spec 7.2)."
  [{:keys [counts chapters-written notes]}]
  (let [{:keys [placed fallback out-of-order partial unmatched]} counts
        searched (+ fallback out-of-order)]
    (str "Marked " (+ placed searched partial) " highlights across "
      chapters-written (if (= 1 chapters-written) " chapter" " chapters")
      (when (pos? searched) (str ". " searched " found by search"))
      (when (pos? partial) (str ". " partial " matched in part only"))
      (when (pos? notes) (str ". " notes " notes written"))
      (when (pos? unmatched)
        (str ". " unmatched " unplaced — see UNPLACED HIGHLIGHTS on the document")))))
