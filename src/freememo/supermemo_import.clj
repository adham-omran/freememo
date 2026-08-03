(ns freememo.supermemo-import
  "Map a decoded SuperMemo collection onto FreeMemo rows.

   Shape of the mapping (plans/supermemo-import.md):
     collection root element  -> the imported topic tree's root
     ConceptGroup (non-root)  -> dropped; children reparent to the nearest
                                 non-ConceptGroup ancestor
     Topic, Task              -> topics row, kind 'supermemo'
     Item                     -> flashcards row on its nearest Topic ancestor
     reference                -> sources row, linked via topics.source_id
     image component          -> media row, appended to the topic's HTML
     repetition               -> topic_repetitions row, event_type 'import'

   Transaction boundary: media rows are written first, outside the main
   transaction, because upsert-media! meters quota in a transaction of its own
   and the blobs are far too large to hold one open across. The rollback
   therefore cannot reach them, so import-collection!'s failure path deletes
   them explicitly and refunds the bytes — content-addressing alone would leave
   the user billed for a collection that never imported. Only rows THIS import
   inserted are deleted; bytes the user already held are left alone. Everything
   else — sources, topics, flashcards, repetitions — commits or rolls back as
   one unit.

   Every element the mapping drops, moves or truncates is counted into the
   returned report. Nothing is discarded silently."
  (:require [clojure.string :as str]
            [freememo.cloze :as cloze]
            [freememo.db :as db]
            [freememo.html-cleaner :as cleaner]
            [freememo.input-check :as input]
            [freememo.supermemo-format :as smf]
            [freememo.text :as text]
            [next.jdbc :as jdbc]
            [taoensso.telemere :as tel])
  (:import [java.io File]
           [java.time LocalDateTime LocalTime]
           [org.jsoup Jsoup]
           [org.jsoup.nodes Element TextNode]))

;; ── HTML: SuperMemo presentation -> Quill-compatible ───────────────
;; clean-html keeps `class` only for allow-listed Quill tokens and `style`
;; only for `color` / `background-color`. SuperMemo expresses all four of its
;; formats in ways that allow-list drops, so each is rewritten into a form
;; that survives — no change to the sanitizer's allow-list is required.

(def ^:private extract-highlight
  "SuperMemo paints extracts yellow. background-color is already the property
   FreeMemo's own auto-extract highlights use, so this lands on an existing
   affordance rather than a new one."
  "#ffff88")

(defn- font-size->quill-class
  "SuperMemo <FONT size> is 1-7 with 3 as the body default.
   Post: an allow-listed Quill size token, or nil for body size."
  [size]
  (when-let [n (try (Long/parseLong (str/trim (str size))) (catch NumberFormatException _ nil))]
    (cond (<= n 2) "ql-size-small"
          (= n 5) "ql-size-large"
          (>= n 6) "ql-size-huge"
          :else nil)))

(defn- font-face->quill-class
  "Post: an allow-listed Quill font token, or nil when the family has no
   FreeMemo equivalent (sans-serif is the default and needs no class)."
  [face]
  (let [f (str/lower-case (str face))]
    (cond
      (re-find #"mono|courier|consol" f) "ql-font-monospace"
      (re-find #"serif|times|georgia|garamond" f) (when-not (re-find #"sans" f) "ql-font-serif")
      :else nil)))

(defn- rewrite-font-element!
  "Rewrite one <font> into a <span> carrying the same intent in allow-listed
   form. Mutates `el` in place; Jsoup keeps its children."
  [^Element el]
  (let [color (not-empty (str/trim (.attr el "color")))
        size-class (font-size->quill-class (.attr el "size"))
        face-class (font-face->quill-class (.attr el "face"))
        classes (str/join " " (remove nil? [size-class face-class]))]
    (.tagName el "span")
    (.removeAttr el "size")
    (.removeAttr el "face")
    (.removeAttr el "color")
    (when color (.attr el "style" (str "color: " color)))
    (when-not (str/blank? classes) (.attr el "class" classes))))

(defn supermemo-html->quill-html
  "Rewrite SuperMemo presentation markup, then sanitize.
   Pre : `html` is an element body from the collection, or nil.
   Post: nil iff `html` is nil or blank; otherwise HTML that survives
         clean-html and re-saves through Quill unchanged.
   Invariant: no information is moved out of the document — colour, size,
         family and extract highlighting each land on a form the allow-list
         keeps, so nothing depends on widening that list."
  [html]
  (when-not (str/blank? html)
    (let [doc (Jsoup/parseBodyFragment html)]
      (doseq [^Element el (.select doc "span.extract, span.extract-highlight")]
        (.attr el "style" (str "background-color: " extract-highlight)))
      (doseq [^Element el (.select doc "font")]
        (rewrite-font-element! el))
      (cleaner/clean-html (.html (.body doc))))))

(defn- append-images
  "Append one <img> per media id to an element's HTML.
   SuperMemo stores images as sibling components rather than inline tags, so
   there is no src to rewrite — the images are added after the text.
   Post: `html` unchanged when `media-ids` is empty."
  [html media-ids]
  (if (empty? media-ids)
    html
    (str (or html "")
      (apply str
        (for [id media-ids]
          (str "<p><img src=\"/api/media/" id "\"></p>"))))))

;; ── Titles ─────────────────────────────────────────────────────────

(defn- clamp
  "Post: `s` unchanged, or its first `n` characters. nil-safe."
  [s n]
  (when s (if (> (count s) n) (subs s 0 n) s)))

(defn- import-title
  "Post: a non-blank title within title-max.
   Imported titles pass through verbatim — prettify-title exists to clean up
   filenames a user uploaded, and would rewrite real titles from a foreign
   registry. Over-length titles are truncated here rather than in
   insert-topic-rows!, because only this layer can count the truncation."
  [raw fallback]
  (let [t (-> (or raw "") (str/replace #"\s+" " ") str/trim)]
    (clamp (if (str/blank? t) fallback t) input/title-max)))

;; ── Tree shape ─────────────────────────────────────────────────────

(defn- collection-root-id
  "Post: the id of the element with no parent — the collection's own root.
   Falls back to the first live element when no element declares parent 0,
   which would mean a corrupt contents.dat."
  [elements]
  (or (some (fn [{:keys [id parent-id deleted?]}]
              (when (and (not deleted?) (or (nil? parent-id) (zero? (long (or parent-id 0)))))
                id))
        elements)
      (:id (first (remove :deleted? elements)))))

(defn- topic-element?
  "True for elements that become a topics row: everything live except Items
   (which become cards), Templates (not imported) and non-root ConceptGroups
   (flattened). The root is a ConceptGroup in every observed collection and
   is exempt — it carries the collection's name."
  [root-id {:keys [id type deleted?]}]
  (and (not deleted?)
    (or (= id root-id)
      (contains? #{:topic :task} type))))

(defn- effective-parent-id
  "Walk up until an ancestor that becomes a topics row.
   Pre : `by-id` maps element id -> element; `id` is a topic element.
   Post: the ancestor's element id, or nil for the root itself.
   Invariant: terminates — every step moves strictly toward the root, and the
         root satisfies topic-element?."
  [by-id root-id id]
  (when (not= id root-id)
    (loop [p (:parent-id (by-id id)), guard 0]
      (let [pe (by-id p)]
        (cond
          (or (nil? pe) (> guard 64)) root-id
          (topic-element? root-id pe) p
          :else (recur (:parent-id pe) (inc guard)))))))

(defn- card-owner-id
  "The topic element an Item's flashcard attaches to.
   Items may nest under other Items; a flashcard cannot own children, so the
   card climbs to the nearest ancestor that became a topic.
   Post: a topic element's id; never nil (the root always qualifies)."
  [by-id root-id item-id]
  (or (effective-parent-id by-id root-id item-id) root-id))

(defn- depth-of
  [by-id root-id id]
  (loop [cur id, d 0]
    (if (or (= cur root-id) (nil? cur) (> d 64))
      d
      (recur (effective-parent-id by-id root-id cur) (inc d)))))

;; ── Scheduling ─────────────────────────────────────────────────────

(defn- day->timestamp
  "Post: midnight on the calendar day SuperMemo's day number names, or nil
   when the collection's epoch could not be calibrated."
  [epoch-day day]
  (when (and epoch-day day)
    (LocalDateTime/of (smf/day->local-date epoch-day day) LocalTime/MIDNIGHT)))

(defn- priority-of
  "Map a position in the collection's priority order onto FreeMemo's 0-100.
   Pre : `rank` is 0-based; `n` is the number of ranked elements.
   Post: 0 for the most important element, 100 for the least, 50 when the
         element carries no rank (matching topics.priority's own default)."
  [rank n]
  (if (or (nil? rank) (< n 2))
    50
    (long (Math/round (* 100.0 (/ (double rank) (double (dec n))))))))

;; ── Report ─────────────────────────────────────────────────────────

(def ^:private not-carried
  "Fields present in a SuperMemo collection that this import does not
   reproduce. Surfaced verbatim so the report never implies more fidelity
   than the decode supports."
  ["Per-repetition grade — not recoverable from the collection format"
   "Lapse counts — not recoverable from the collection format"
   "Repetition history of items — a flashcard has no per-card repetition log"
   "U-factor (interval-growth ratio) — no FreeMemo equivalent"
   "Component geometry (position and size) — no FreeMemo equivalent"
   "Concepts, tasklists and saved subsets — out of scope"
   "SuperMemo algorithm state (stability, difficulty, forgetting-index matrices)"])

;; ── Import ─────────────────────────────────────────────────────────

(defn- store-images!
  "Write every distinct image file in the collection to `media`, once.
   Runs before the main transaction; see the namespace docstring.
   Pre : `user-id` owns the import.
   Post: {:media-by-file {File media-id} :created-ids #{media-id}}. A file that
         fails (unreadable, over quota) is absent from `:media-by-file` and
         counted by the caller, rather than aborting the import.
         `:created-ids` holds only the rows THIS import inserted — a file whose
         bytes the user already had contributes to `:media-by-file` but not to
         `:created-ids`, so a failed import's compensation cannot delete
         content that predates it."
  [user-id collection elements]
  (let [files (distinct (mapcat #(smf/element-images collection %) elements))]
    (reduce (fn [acc ^File f]
              (try
                (let [bytes (with-open [in (clojure.java.io/input-stream f)] (.readAllBytes in))
                      row (db/upsert-media-row!
                            {:user-id user-id :kind "image" :bytes bytes
                             :mime-type (or (java.net.URLConnection/guessContentTypeFromName (.getName f))
                                          "application/octet-stream")})]
                  (cond-> (assoc-in acc [:media-by-file f] (:media/id row))
                    (:media/inserted? row) (update :created-ids conj (:media/id row))))
                (catch Exception e
                  (tel/log! {:level :warn :id ::image-skipped
                             :data {:file (.getName f) :error (.getMessage e)}}
                    "SuperMemo image skipped")
                  acc)))
      {:media-by-file {} :created-ids #{}} files)))

(defn- item-text-components
  "The element's textual components, in component order, raw.
   Pre : `element` came from `read-collection`.
   Post: a vector of non-blank HTML strings. Index 0 is SuperMemo's question
         and index 1 its answer; image components are absent, which is why a
         four-component item can still yield a two-entry vector."
  [collection element]
  (->> (:components element)
    (filter #(#{:html :text :rtf} (:kind %)))
    (keep #(smf/element-html collection (assoc element :components [%])))
    (remove str/blank?)
    vec))

;; ── Cloze items ────────────────────────────────────────────────────
;; A SuperMemo cloze item is two components: the sentence, carrying
;; <SPAN class=cloze>[...]</SPAN> where the gap is, and the deleted text.
;; That is exactly a FreeMemo cloze card once the span is replaced by
;; {{c1::<deleted text>}}.
;;
;; SuperMemo does not always leave a gap that survives the substitution. The
;; span often sits inside a <dt> whose answer is a sibling <dt> of the same
;; definition list, so splicing the answer in place puts the {{c1:: opener and
;; its }} closer on opposite sides of a block boundary. The card still passes
;; cloze/validate — that scan reads characters, not tags — but the masked
;; prompt renders with an unclosed element. `cloze-renders-cleanly?` therefore
;; tests the artifacts the learner sees, not the marker text.

(def ^:private cloze-slot
  "Placeholder that stands in for the marker span between the Jsoup pass that
   finds it and the string pass that fills it. Jsoup gives class-token
   matching, which a regex over `class=cloze` would get wrong for
   `class=\"cloze extra\"`; the string pass keeps the answer's HTML from being
   re-parsed into the question's tree."
  "FREEMEMOCLOZESLOT")

(def ^:private void-elements
  "HTML elements with no closing tag. `tag-balanced?` must not expect one."
  #{"br" "img" "hr" "input" "wbr" "col" "area" "base" "embed" "source" "track"
    "param" "meta" "link"})

(defn- tag-balanced?
  "Post: true when every closing tag in `html` has a matching opener.
   Approximate by design — it counts tag names and ignores order, which is
   enough to catch a deletion that was spliced across an element boundary.
   Kept private: a partial check invites misuse if it looks general."
  [html]
  (let [s (or html "")
        opens (->> (re-seq #"<([a-zA-Z]+)[^>]*>" s)
                (map (comp str/lower-case second))
                (remove void-elements))
        closes (->> (re-seq #"</([a-zA-Z]+)>" s) (map (comp str/lower-case second)))]
    (= (frequencies opens) (frequencies closes))))

(def ^:private typed-gap
  "The gap an author typed by hand instead of marking. It is the same string
   SuperMemo renders a marked deletion as, and the same string `cloze/blank`
   uses, so a reader cannot tell the two apart — and neither should the import."
  "[...]")

(defn- mark-gap-slot!
  "Put `cloze-slot` where the deletion belongs. Mutates `doc`.
   Post: true when a gap was found. A marker span wins; failing that, the
         first text node holding a typed gap is split around the slot.
   Working on text nodes rather than the serialized markup keeps a `[...]`
   inside an attribute value from being mistaken for a gap."
  [doc]
  (if-let [^Element span (.selectFirst doc "span.cloze")]
    (do (.replaceWith span (TextNode. cloze-slot)) true)
    (boolean
      (when-let [^TextNode node (->> (.textNodes (.body doc))
                                  (concat (mapcat #(.textNodes ^Element %) (.select doc "*")))
                                  (filter #(str/includes? (.getWholeText ^TextNode %) typed-gap))
                                  first)]
        (let [t (.getWholeText node)
              i (str/index-of t typed-gap)]
          (.text node (str (subs t 0 i) cloze-slot (subs t (+ i (count typed-gap)))))
          true)))))

(defn- substitute-cloze-deletion
  "Replace the gap in `question-html` with an Anki deletion holding
   `answer-html`.
   Pre : both are raw component HTML from the collection.
   Post: the question with `{{c1::<answer>}}` where the gap was, or nil when
         the question has no gap.
   Invariant: only the FIRST gap is substituted. Every item in the reference
         collection has exactly one; a second would have no answer component
         to fill it."
  [question-html answer-html]
  (when-not (str/blank? question-html)
    (let [doc (Jsoup/parseBodyFragment question-html)]
      (when (mark-gap-slot! doc)
        (str/replace (.html (.body doc))
          cloze-slot
          (java.util.regex.Matcher/quoteReplacement
            (str "{{c1::" answer-html "}}")))))))

(defn- cloze-renders-cleanly?
  "Post: true when `text` is a cloze field the quiz can render.
   Three conditions, in order: the syntax parses; masking the deletion leaves
   balanced HTML; the deletion has a non-empty answer. The second is the one
   that matters — cloze/validate passes text whose marker was spliced across a
   tag boundary, because its scan reads characters and ignores markup."
  [text]
  (and (some? text)
    (nil? (cloze/validate text))
    (tag-balanced? (cloze/mask-ord text 1))
    (not (str/blank? (cloze/answer-for-ord text 1)))))

(defn- has-gap?
  "Post: true when `html` holds a deletion gap — a marker span, or a typed
   `[...]` in text. Attribute values are not text, so a `[...]` inside one
   does not count."
  [html]
  (boolean
    (when-not (str/blank? html)
      (let [doc (Jsoup/parseBodyFragment html)]
        (or (some? (.selectFirst doc "span.cloze"))
          (str/includes? (.text (.body doc)) typed-gap))))))

(defn- marker-span?
  "Post: true when `html` contains a SuperMemo cloze marker span, ignoring
   typed gaps. Used only to report the one item whose marker sits too late."
  [html]
  (boolean
    (when-not (str/blank? html)
      (some? (.selectFirst (Jsoup/parseBodyFragment html) "span.cloze")))))

(defn cloze-candidate?
  "Post: true when an Item's first text component has a deletion gap and the
   item has an answer component to fill it.
   Both gap kinds count. SuperMemo marks a deletion it made itself, and leaves
   a hand-typed one as plain text; both render as `[...]` to the reader, and
   every hand-typed instance in the reference collection is a real deletion
   whose answer component fills it exactly.
   The caller counts candidates against emitted cloze rows to report how many
   fell back — this answers 'was it meant to be cloze', not 'did it work'."
  [collection element]
  (let [htmls (item-text-components collection element)]
    (and (has-gap? (first htmls))
      (some? (second htmls)))))

(defn cloze-marker-after-first-component?
  "Post: true when an Item's marker span sits in a component other than the
   first, which the substitution does not reach.
   One element in the reference collection is shaped this way. It imports as a
   basic card; this fn exists so the report can say so rather than leaving the
   item indistinguishable from one that was never cloze."
  [collection element]
  (let [htmls (item-text-components collection element)]
    (and (not (has-gap? (first htmls)))
      (boolean (some marker-span? (rest htmls))))))

(defn- item->card-row
  "Build the flashcards row for an Item.
   Pre : `topic-id` names the topic the card hangs off; `media-ids` are the
         media rows for this item's image components.
   Post: a cloze row when the item carries a marker span AND the result
         renders cleanly, otherwise the basic row — question from component 0,
         remaining text components joined into the answer. nil when the item
         has no readable text.
   Invariant: a cloze row sets `:question` nil, which the partial unique index
         idx_flashcards_unique_cloze_digest requires.
   Images append after the card's content, outside any deletion, so the same
   rule applies whichever shape the item takes."
  [collection element topic-id root-topic-id media-ids]
  (let [htmls (item-text-components collection element)
        cloze-text (some-> (substitute-cloze-deletion (get htmls 0) (get htmls 1))
                     supermemo-html->quill-html)]
    (if (cloze-renders-cleanly? cloze-text)
      {:topic_id topic-id
       :root_topic_id root-topic-id
       :kind "cloze"
       :question nil
       :answer nil
       :cloze (clamp (append-images cloze-text media-ids) input/card-max)}
      (let [question (supermemo-html->quill-html (get htmls 0))
            extras (seq (keep supermemo-html->quill-html (subvec htmls (min 1 (count htmls)))))
            answer (when extras (str/join "<hr>" extras))]
        (when (or question answer)
          {:topic_id topic-id
           :root_topic_id root-topic-id
           :kind "basic"
           :question (clamp question input/card-max)
           :answer (clamp (append-images answer media-ids) input/card-max)})))))

(defn import-collection!
  "Import an extracted SuperMemo collection for `user-id`.
   Pre : `dir` is a readable directory holding a collection (this fn locates
         the collection folder within it); the caller owns `dir` and deletes
         it afterwards, on both the success and failure paths.
   Post: {:ok true :topic-id N :report {...}} when the tree committed, or
         {:ok false :error S} when it did not. On failure NOTHING persists:
         topics, sources, flashcards and repetitions roll back with the
         transaction, and the media rows written before it opened are deleted
         and their bytes refunded. A failed import therefore costs the user no
         storage — it trades the old reuse-on-retry shortcut, which billed for
         a collection that was never imported, for re-storing those images if
         they retry (the archive is re-extracted on every attempt anyway).
         Media whose bytes the user already held is untouched.
   Invariant: every element that is skipped, flattened, moved or truncated is
         counted in :report — the report is the import's account of itself,
         not a summary of the happy path."
  [user-id ^File dir]
  (let [root-dir (smf/find-collection-root dir)]
    (if-not root-dir
      {:ok false :error "No SuperMemo collection found in the archive (expected info/contents.dat)."}
      (let [collection (smf/read-collection root-dir)
            elements (:elements collection)
            by-id (into {} (map (juxt :id identity)) elements)
            root-id (collection-root-id elements)
            epoch (:epoch-day collection)
            live (remove :deleted? elements)
            topic-els (filterv #(topic-element? root-id %) elements)
            item-els (filterv #(and (not (:deleted? %)) (= :item (:type %))) elements)
            rank-of (into {} (map-indexed (fn [i id] [id i])) (:priority-order collection))
            n-ranked (count (:priority-order collection))
            next-rep (:next-rep-day collection)
            ;; Items carry images too, and their cards append them, so both
            ;; element classes go in — one pass, deduped by sha256.
            ;; `created-media-ids` is what the failure path gives back; see the
            ;; catch below.
            {media-by-file :media-by-file created-media-ids :created-ids}
            (store-images! user-id collection (concat topic-els item-els))
            ;; Counters the report owes the user.
            !truncated (atom 0)
            !missing-images (atom 0)]
        (try
          (jdbc/with-transaction [tx db/ds]
            (let [;; 1. Bibliography. Only references an element actually
                  ;;    points at are worth a row.
                  used-refs (into (sorted-set)
                              (keep #(let [r (:reference-id %)] (when (pos? (long (or r 0))) r)))
                              (concat topic-els item-els))
                  ref-rows (vec (for [rid used-refs
                                      :let [r (get (:references collection) rid)]
                                      :when r]
                                  {:rid rid :ref r}))
                  source-ids (db/insert-source-rows! tx
                               (mapv (fn [{:keys [ref]}]
                                       {:user_id user-id
                                        :csl_type (if (str/starts-with? (str (:url ref)) "http") "webpage" "document")
                                        :csl (cond-> {}
                                               (:title ref) (assoc :title (:title ref))
                                               (:url ref) (assoc :URL (:url ref))
                                               (:source ref) (assoc :container-title (:source ref)))
                                        :url (:url ref)
                                        :title (:title ref)
                                        :container_title (:source ref)})
                                 ref-rows))
                  source-by-ref (zipmap (map :rid ref-rows) source-ids)

                  ;; 2. Topics, breadth-first so a parent id always exists
                  ;;    before its children reference it.
                  by-depth (group-by #(depth-of by-id root-id (:id %)) topic-els)
                  topic-row (fn [db-parent-id el]
                              (let [raw-html (smf/element-html collection el)
                                    imgs (smf/element-images collection el)
                                    media-ids (keep media-by-file imgs)
                                    _ (swap! !missing-images + (- (count imgs) (count media-ids)))
                                    html (-> (supermemo-html->quill-html raw-html)
                                           (append-images media-ids))
                                    raw-title (:title el)
                                    title (import-title raw-title (str "Element " (:id el)))
                                    _ (when (and raw-title (> (count raw-title) input/title-max))
                                        (swap! !truncated inc))
                                    day (nth next-rep (dec (:id el)) 0)
                                    rank (rank-of (:id el))]
                                (cond-> {:user_id user-id
                                         :kind "supermemo"
                                         :title title
                                         :status "active"
                                         :priority (priority-of rank n-ranked)
                                         :sm_rank rank
                                         :sm_element_id (:id el)
                                         :interval_days (double (max 1 (:interval-days el)))
                                         :a_factor (double (:a-factor el))
                                         :next_review_at (when (pos? (long (or day 0)))
                                                           (day->timestamp epoch day))
                                         :last_review_at (or (smf/tdatetime->local-date-time (:last-touch el))
                                                           (day->timestamp epoch (:last-rep-day el)))}
                                  db-parent-id (assoc :parent_id db-parent-id)
                                  html (assoc :content html :content_text (text/strip-html html))
                                  (source-by-ref (:reference-id el))
                                  (assoc :source_id (source-by-ref (:reference-id el))))))
                  db-id-by-element
                  (reduce (fn [acc depth]
                            (let [els (get by-depth depth)
                                  rows (mapv #(topic-row (get acc (effective-parent-id by-id root-id (:id %))) %) els)
                                  ids (db/insert-topic-rows! tx rows)]
                              (merge acc (zipmap (map :id els) ids))))
                    {} (sort (keys by-depth)))
                  root-topic-id (get db-id-by-element root-id)

                  ;; 3. Cards. An Item nested under another Item climbs to the
                  ;;    nearest ancestor that became a topic.
                  reparented (count (filter #(= :item (:type (by-id (:parent-id %)))) item-els))
                  card-rows (vec (keep (fn [el]
                                         (let [owner (card-owner-id by-id root-id (:id el))]
                                           (when-let [tid (get db-id-by-element owner)]
                                             (item->card-row collection el tid root-topic-id
                                               (vec (keep media-by-file (smf/element-images collection el)))))))
                                   item-els))
                  card-ids (db/insert-flashcards! tx card-rows)
                  cloze-candidates (count (filter #(cloze-candidate? collection %) item-els))
                  cloze-rows (count (filter #(= "cloze" (:kind %)) card-rows))
                  cloze-marker-late (count (filter #(cloze-marker-after-first-component? collection %) item-els))

                  ;; 4. Repetition log. SuperMemo records when, not what state
                  ;;    was replaced, so the three snapshot columns stay nil.
                  history (:repetition-history collection)
                  dated (filterv :at history)
                  ;; topic_repetitions keys on topic_id, so a repetition whose
                  ;; element became a flashcard has nowhere to go. Counted
                  ;; separately from undated placeholders — conflating the two
                  ;; would understate what the import dropped.
                  rep-rows (vec (keep (fn [{:keys [element-id at]}]
                                        (when-let [tid (get db-id-by-element element-id)]
                                          {:topic_id tid :user_id user-id
                                           :event_type "import" :event_at at
                                           :status_before nil :priority_before nil
                                           :interval_days_before nil :a_factor_before nil
                                           :next_review_at_before nil :last_review_at_before nil}))
                                  dated))
                  rep-count (or (db/insert-topic-repetitions! tx rep-rows) 0)
                  undated (- (count history) (count dated))
                  reps-on-cards (- (count dated) (count rep-rows))]

              {:ok true
               :topic-id root-topic-id
               :report
               {:collection (str (.getName root-dir))
                :checks (:checks collection)
                :epoch-agreement (:epoch-agreement collection)
                :counts {:elements-read (count elements)
                         :skipped-deleted (- (count elements) (count live))
                         :topics-created (count db-id-by-element)
                         :containers-flattened (count (filter #(and (not (:deleted? %))
                                                                 (= :concept-group (:type %))
                                                                 (not= (:id %) root-id))
                                                        elements))
                         :items-as-cards (count card-ids)
                         :items-as-cloze-cards cloze-rows
                         :items-as-basic-cards (- (count card-rows) cloze-rows)
                         ;; A marked cloze item whose deletion could not be
                         ;; spliced without breaking the markup. It still
                         ;; imports, as a question/answer card.
                         :cloze-fallback-to-basic (- cloze-candidates cloze-rows)
                         :cloze-marker-after-first-component cloze-marker-late
                         :items-skipped (- (count item-els) (count card-rows))
                         :nested-items-reparented reparented
                         :repetitions-imported rep-count
                         :repetitions-undated-skipped undated
                         :repetitions-on-cards-skipped reps-on-cards
                         :sources-created (count source-ids)
                         :images-stored (count (set (vals media-by-file)))
                         :images-unresolved @!missing-images
                         :titles-truncated @!truncated}
                :not-carried not-carried}}))
          (catch Exception e
            (tel/error! {:id ::import-failed :data {:user-id user-id}} e)
            ;; Media is written before the transaction opens, so the rollback
            ;; cannot reach it. Give back only the rows this import created,
            ;; or the user is billed for content nothing references. Guarded
            ;; separately so a failed compensation cannot mask the real error.
            (try
              (let [r (db/delete-media-rows! user-id created-media-ids)]
                (when (pos? (long (:deleted r)))
                  (tel/log! {:level :info :id ::import-media-reverted
                             :data {:user-id user-id :deleted (:deleted r)
                                    :refunded (:refunded r)}}
                    "Reverted media stored by a failed SuperMemo import")))
              (catch Exception ce
                (tel/error! {:id ::import-media-revert-failed
                             :data {:user-id user-id
                                    :media-ids (count created-media-ids)}} ce)))
            {:ok false :error (or (.getMessage e) "Import failed")}))))))
