(ns freememo.neoreader-sync
  "Run one NeoReader sync for one document.

   The single orchestration layer: read the bound export from Drive, decode it,
   place every highlight in the document's sections, and write back the sections
   that changed. The decode lives in `freememo.neoreader-export` and the
   placement in `freememo.quote-anchor`; both are pure and neither knows about
   Drive or the database.

   EPUB and PDF, and the difference is narrow. A section is a chapter child for an
   EPUB and a page child for a PDF, and `document-sections` returns both in the
   same shape — `topics.page_number` is the ordinal either way. Only two things
   branch on the kind: WHICH topic carries the unplaced list (`unplaced-write`),
   and what a section is CALLED in reported text (`neoreader-kinds/section-noun`).

   One sync writes in four places, and `quote-anchor` owns the shape of all four:
   a tint on each placed passage, a `[n]` marker on each noted passage, a
   `USER NOTES` section at the end of each section that has notes, and an
   `UNPLACED HIGHLIGHTS` section naming what nothing could place. It also records
   one row per highlight, which is what an account export carries and what a later
   view would read.

   Everything is regenerated, never merged. Each sync strips every previous mark,
   marker and section from a topic before applying the current export, so a
   highlight deleted on the device loses its mark — the export is the truth, per
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
    [freememo.neoreader-kinds :as kinds]
    [freememo.quote-anchor :as anchor]))

(def file-id-setting-prefix
  "Settings-key prefix binding a document to a Drive file.

   The suffix is the root topic id, so this prefix MUST have an entry in
   `freememo.account-import/settings-key-prefixes`. Without one, an imported
   preference points at another user's topic (§5.4.8)."
  "boox_file_")

(defn file-id-setting-key [topic-id] (str file-id-setting-prefix topic-id))

(defn bound-file-id
  "Pre : `topic-id` is a syncable document root owned by `user-id`.
   Post: the Drive file id bound to it, or nil when it was never bound."
  [user-id topic-id]
  (let [v (db/get-setting user-id (file-id-setting-key topic-id))]
    (when-not (str/blank? v) v)))

(defn bind-file!
  "Pre : `file-id` was picked by this user through the Google Picker, so the app
         holds per-file access to it. `topic-id` is a syncable document root.
   Post: later syncs of `topic-id` read that file with no Picker interaction."
  [user-id topic-id file-id]
  (db/set-setting user-id (file-id-setting-key topic-id) file-id))

;; ── Chapters ───────────────────────────────────────────────────────

(defn- document-sections
  "The structural children of a document root, in reading order.

   Pre : `topic-id` is a root whose kind `neoreader-kinds/syncable-kind?` accepts.
   Post: a vector of {:id :title :html}, ordered by `page_number` — the chapter
         index `db/create-epub-topic!` assigned from the spine, or the page number
         `db/insert-page-stubs!` assigned from the PDF. One field name serves both
         because `topics.page_number` is the generic ordinal.
   Post: converting the namespace-qualified row keys is this function's job.
         `quote-anchor` takes plain `:title`/`:html` and owes no knowledge of
         topic rows.
   Inv : only children carrying a `page_number` are sections. `db/get-children`
         has no kind filter, and a kind filter could not do this job anyway — an
         EPUB chapter and an extract are BOTH kind `basic` (db.clj:3003,
         content_toolbar_helpers/create-extract-topic-safe!). Only the ordinal
         separates them: a chapter or page has one, an extract never does.
   Inv : without that filter an extract holding a copy of a passage is a second
         place a highlight can match, and it sorts last (`:asc-nulls-last`) so it
         would win whenever the real chapter had already been passed."
  [topic-id]
  (into []
    (comp (filter #(some? (:topics/page_number %)))
      (map (fn [c] {:id (:topics/id c)
                    :title (:topics/title c)
                    :html (:topics/content c)})))
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

(defn- count-noun
  "Pre : `n` is a non-negative count and `noun` its singular form.
   Post: \"1 page\", \"3 pages\". Regular English plural only — every noun this
         namespace counts takes one."
  [n noun]
  (str n " " noun (when (not= 1 n) "s")))

(defn- no-text-line
  "The diagnosis line opening an unplaced list.

   Pre : `kind` is a syncable kind. 0 < `blank` <= `total`.
   Post: a sentence naming how many of the document's sections hold no text yet,
         in the reader's own vocabulary for them.
   Note: the noun agrees with `total` and the verb with `blank` — \"1 of 12 pages
         holds no text yet\".
   Note: this is the whole of spec 10.5.3, and it exists because an unplaced list
         with no cause is not actionable. On a PDF an unscanned page is the
         DOMINANT cause, and the reader cannot see a page's text is missing from
         the list alone."
  [kind blank total]
  (str blank " of " (count-noun total (get kinds/section-noun kind "section"))
    (if (= 1 blank) " holds" " hold") " no text yet"))

(defn- unplaced-section-lines
  "The body of the `UNPLACED HIGHLIGHTS` section.

   Pre : `sections` is `document-sections` output ALREADY STRIPPED, so a section
         holding nothing but a previous sync's own writing reads as blank.
         `highlights` is `parse-highlights` output, and `placements` is
         `place-highlights` output for it, positionally matched.
   Post: one line per :unmatched highlight, in the export's order, naming its
         chapter, its device timestamp and its quote — preceded by
         `no-text-line` when any section holds no text.
   Post: EMPTY when every highlight placed. `quote-anchor/append-section` then
         writes no heading, so a document that no longer needs the section does
         not keep an empty one.
   Inv : the blank count comes from `sections` already in memory. No query
         (spec 10.5.4)."
  [kind sections highlights placements]
  (let [rows (into []
               (keep (fn [[h p]]
                       (when (= :unmatched (:outcome p))
                         (let [q (str (:quote h))]
                           (str (or (:chapter h) "?") " - " (:timestamp h) " - "
                             (if (> (count q) max-unplaced-quote-chars)
                               (str (subs q 0 max-unplaced-quote-chars) "...")
                               q))))))
               (map vector highlights placements))
        blank (count (filter #(str/blank? (:html %)) sections))]
    (cond
      (empty? rows) []
      (pos? blank) (into [(no-text-line kind blank (count sections))] rows)
      :else rows)))

(defn- highlight-rows
  "Pre : `sections` is `document-sections` output, `highlights` is
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
  [sections highlights placements]
  (let [ordinals (into {}
                   (mapcat (fn [[_ ps]]
                             (map-indexed (fn [i p] [(:index p) (inc i)])
                               (sort-by :start ps))))
                   (group-by :chapter-pos
                     (filter #(some? (:chapter-pos %)) placements)))]
    (mapv (fn [[h p]]
            {:chapter_topic_id (when-let [pos (:chapter-pos p)] (:id (nth sections pos)))
             :chapter_label (:chapter h)
             :highlighted_at (:timestamp h)
             :quote (:quote h)
             :note (let [joined (str/join " " (remove str/blank? (:notes h)))]
                     (when-not (str/blank? joined) joined))
             :ordinal (get ordinals (:index p))
             :outcome (name (:outcome p))})
      (map vector highlights placements))))

(defn- unplaced-write
  "Which topic carries the unplaced list, and what its new content is.

   Pre : `kind` is a syncable kind. `sections` is `document-sections` output as
         STORED, `clean` the same list stripped, `marked` `mark-chapters` output,
         `root` the root topic row, `lines` `unplaced-section-lines` output.
         `sections` is non-empty — the caller returns early otherwise.
   Post: {:topic-id :stored :content}. The caller writes only when `:content`
         differs from `:stored`, which is what keeps a re-sync from touching a
         topic (spec 10.4.4).
   Inv : `:content` went through `strip-sync-content` before `append-section`,
         on BOTH branches. A PDF's first section was stripped as a section; an
         EPUB's root is not a section and is stripped here (spec 10.4.2).
   Note: a PDF root's own content is NEVER rendered — `topic_state.cljc:174`
         fetches it only `(when (or (not is-pdf?) is-score?))`, and a PDF's editor
         shows the current page's text instead. So the list goes on the first
         section, which `db/get-children` orders first (`:asc-nulls-last`) and
         which a PDF that imported always has.
   Note: an EPUB root's content IS rendered, and `db/create-epub-topic!` sets none
         at import — so the list is the only thing in that editor. Moving it to
         chapter 1 for uniformity would bury it under the chapter's prose, which is
         why the two kinds differ here on purpose."
  [kind sections clean marked root lines]
  (if (= "pdf" kind)
    (let [base (or (get marked 0) (:html (nth clean 0)))]
      {:topic-id (:id (nth sections 0))
       :stored (:html (nth sections 0))
       :content (anchor/append-section base anchor/unplaced-heading lines)})
    {:topic-id (:topics/id root)
     :stored (:topics/content root)
     :content (anchor/append-section
                (anchor/strip-sync-content (:topics/content root))
                anchor/unplaced-heading lines)}))

(defn sync-document!
  "Sync one document against its bound NeoReader export.

   Pre : `user-id` owns `topic-id`, and `topic-id` is a root whose kind
         `neoreader-kinds/syncable-kind?` accepts, holding structural children. A
         Drive file is bound and the user's Drive authorization is live.
   Post: {:ok true :counts {…} :sections-written n :notes n :titles-resolved n
         :kind s} on success, or {:ok false :error msg :reauthorize? bool}. On
         success every section that holds a placement has been rewritten with its
         marks, its `[n]` markers and its `USER NOTES` section, and no other
         section was touched. One topic carries an `UNPLACED HIGHLIGHTS` section
         when anything went unplaced, and carries none when nothing did — see
         `unplaced-write` for which topic and why.
   Post: `:reauthorize?` is true only when the stored Drive credential is gone or
         Google rejected it. The caller shows a re-authorize action for that case
         and a plain error for every other.
   Inv : idempotent. Two runs against the same export leave identical content and
         identical rows, because every topic a sync writes is stripped of
         everything a sync wrote before the current export is applied.
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
              (let [root (db/get-topic topic-id)
                    kind (:topics/kind root)
                    sections (document-sections topic-id)
                    noun (get kinds/section-noun kind "section")]
                (if (empty? sections)
                  {:ok false :error (str "This document has no " noun "s to sync into")}
                  ;; Strip first, then place against the stripped text. A tint
                  ;; span adds no text, but a `[n]` marker and both sections do —
                  ;; leaving them in would let a quote match a copy of itself.
                  (let [clean (mapv #(update % :html anchor/strip-sync-content) sections)
                        placements (anchor/place-highlights clean highlights)
                        marked (anchor/mark-chapters clean placements)
                        ;; A section that lost marks and gained none still needs
                        ;; its stripped HTML written, or a deleted highlight keeps
                        ;; its tint. Compare stripped against stored to find those.
                        cleaned-only (into {}
                                       (keep (fn [[pos ch]]
                                               (let [before (:html (nth sections pos))
                                                     after (:html ch)]
                                                 (when (and (not (contains? marked pos))
                                                         (not= before after))
                                                   [pos after]))))
                                       (map-indexed vector clean))
                        ;; Keyed by TOPIC id, not by position: the unplaced list
                        ;; may land on a topic already in this map (a PDF's first
                        ;; section) or on one that is not a section at all (an
                        ;; EPUB's root), and one key space lets both merge.
                        section-writes (into {}
                                         (map (fn [[pos html']] [(:id (nth sections pos)) html']))
                                         (merge cleaned-only marked))
                        unplaced (unplaced-write kind sections clean marked root
                                   (unplaced-section-lines kind clean highlights placements))
                        writes (cond-> section-writes
                                 (not= (:content unplaced) (:stored unplaced))
                                 (assoc (:topic-id unplaced) (:content unplaced)))]
                    (doseq [[id html'] writes]
                      (db/update-topic-content! id html'))
                    (db/replace-neoreader-highlights! user-id topic-id
                      (highlight-rows sections highlights placements))
                    (commands/bump! user-id :neoreader-sync)
                    (let [counts (outcome-counts placements)
                          section-ids (into #{} (map :id) sections)]
                      (tel/log! {:level :info :id ::synced
                                 :data {:user-id user-id :topic-id topic-id :kind kind
                                        :counts counts :topics-written (count writes)}}
                        "NeoReader sync complete")
                      {:ok true
                       :kind kind
                       :counts counts
                       ;; Sections only. The root is not one, so counting every
                       ;; write would report an EPUB as one chapter wider than it is.
                       :sections-written (count (filter section-ids (keys writes)))
                       :titles-resolved (anchor/resolved-title-count clean highlights)
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
         weaker pass, and how many nothing could place. Counts of sections use the
         kind's own noun, so a PDF reader is told about pages.
   Note: `:fallback` and `:out-of-order` are folded into one \"found by search\"
         figure. The distinction is why a pass was needed, which the reader cannot
         act on. `:partial` stays separate, because it is the one outcome where
         the mark covers LESS than the reader highlighted.
   Note: that clause is SUPPRESSED when no chapter label resolved to a section
         (spec 10.5.2). On a PDF that is permanent — every page is titled `Page N`
         — so the clause would fire for every highlight of every PDF sync and
         carry no information. A toast the reader learns to ignore is also the
         toast carrying the unplaced count.
   Note: the unmatched count names WHERE to look, because a count with no place to
         look is not actionable (spec 7.2)."
  [{:keys [counts sections-written notes titles-resolved kind]}]
  (let [{:keys [placed fallback out-of-order partial unmatched]} counts
        searched (+ fallback out-of-order)]
    (str "Marked " (count-noun (+ placed searched partial) "highlight") " across "
      (count-noun sections-written (get kinds/section-noun kind "section"))
      (when (and (pos? searched) (pos? titles-resolved))
        (str ". " searched " found by search"))
      (when (pos? partial) (str ". " partial " matched in part only"))
      (when (pos? notes) (str ". " (count-noun notes "note") " written"))
      (when (pos? unmatched)
        (str ". " unmatched " unplaced — see UNPLACED HIGHLIGHTS on "
          (if (= "pdf" kind) "page 1" "the document"))))))
