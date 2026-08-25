(ns freememo.page-merge
  "Merge a PDF's page rows into one document, and split one document back into
   page rows. The boundary half of focus reader's merged reading surface —
   see plans/merged-page-reading.md.

   THE MARKER IS PRESENTATION ONLY. `merge-pages` inserts one
   `<h6 id=\"fm-page-N\">Page N</h6>` before each page's slice, and
   `split-merged` strips every marker before returning a slice. So `fm-page-`
   never reaches `topics.content`, and therefore never reaches `content_text`,
   `content_preview`, the card-generation prompt, a search snippet, or
   knowledge-graph extraction — all of which read a page row's stored content.

   That also makes the design self-healing. A reload re-derives every marker
   from the page rows, so a session's boundary drift cannot compound.

   WHY A HEADING AND NOT A CONTAINER. Quill 2.0.3 registers no blot for `div`,
   so `clipboard.convert` keeps a wrapper's text and drops the wrapper — a
   per-page container cannot survive one keystroke. A BLOCK-scoped `id`
   Attributor IS registered (`quill_field.cljc`), and it round-trips on `h1`
   through `h6` and on `p`. A heading also gives the id real text to attach to,
   which an empty sentinel paragraph does not.

   WHY THE NUMBER IS IN THE MARKER. An `<hr>` would round-trip too, and it
   carries nothing — boundaries would be positional, so deleting one would
   shift every later page and silently reattribute its cards. The number makes
   a lost boundary a local problem.

   THE BOUNDARY-LOSS RULE IS ABSORB-UPWARD. A page whose marker the reader
   destroyed contributes its text to the page above, and its own slice comes
   back empty. Its row keeps its id, its page number, its status and its cards:
   `split-merged` never reports a page as absent, so no caller can be tempted to
   delete a row. The scheduler counts page rows and 252 cards sit on page rows
   in one dev database alone.

   Splitting is not the inverse of merging once the reader has edited across a
   boundary. That is the accepted cost of the feature, recorded in the plan."
  (:require [clojure.string :as str])
  #?(:clj (:import [org.jsoup Jsoup]
                   [org.jsoup.nodes Element Node])))

;; ── The marker vocabulary ──────────────────────────────────────────────────

(def marker-id-prefix
  "The `id` prefix that identifies a page boundary. Public because the probes
   and the tests assert against it, and because a stored slice containing it is
   the failure this namespace exists to prevent."
  "fm-page-")

(def marker-tag
  "The boundary element's tag. `h6` is the smallest heading, so it inherits the
   least prose weight while still being a block the `id` Attributor covers."
  "h6")

(defn marker-id
  "Pre : `page-number` is a positive integer.
   Post: the `id` for that page's boundary marker."
  [page-number]
  (str marker-id-prefix page-number))

(defn parse-marker-id
  "The page number an `id` names, or nil.

   Post: nil for any id that is not exactly the prefix followed by digits. A
   corrupted id is ABSENT, never page 0 — a marker that parsed to 0 would sort
   before every real page and collect the document's opening text."
  [id]
  (when (and (string? id) (str/starts-with? id marker-id-prefix))
    (let [tail (subs id (count marker-id-prefix))]
      (when (re-matches #"[1-9][0-9]*" tail)
        #?(:clj (Long/parseLong tail) :cljs (js/parseInt tail 10))))))

(defn marker-html
  "The boundary marker for `page-number`, as stored-form HTML.

   The text is `Page N` and nothing else. A page's title is literally \"Page N\"
   today, so showing the title would add nothing.

   NO CLASS. The `id` is the marker's only identifier, and CSS selects it with
   `[id^=\"fm-page-\"]`. A class would be a second name for one thing, and Quill
   would drop it on the first keystroke anyway: `clipboard.convert` keeps only
   registered formats, and no format owns a `fm-page-marker` class. The `id`
   survives because a BLOCK-scoped Attributor is registered for it."
  [page-number]
  (str "<" marker-tag " id=\"" (marker-id page-number) "\">"
       "Page " page-number
       "</" marker-tag ">"))

;; ── Merge ──────────────────────────────────────────────────────────────────

(defn merge-pages
  "One document from many page rows.

   Pre : `pages` is a seq of maps carrying a page number and stored HTML, under
         either the namespaced keys `db` returns (`:topics/page_number`,
         `:topics/content`) or the plain keys a test supplies. Rows with no page
         number are dropped — they cannot be addressed by a marker.
   Post: marker, then slice, per page, in ascending page order. A page with no
         content still gets its marker, so an unscanned page keeps a boundary
         the reader can navigate to and the split can find.
   Invariant: the returned string contains one marker per input page."
  [pages]
  (->> pages
       (keep (fn [p]
               (when-let [n (or (:topics/page_number p) (:page-number p) (:page_number p))]
                 [n (or (:topics/content p) (:content p) "")])))
       (sort-by first)
       (map (fn [[n html]] (str (marker-html n) html)))
       (str/join)))

;; ── Split ──────────────────────────────────────────────────────────────────

#?(:clj
   (defn- marker-page
     "The page number `el` is a boundary marker for, or nil."
     [^Element el]
     (when (= marker-tag (str/lower-case (.tagName el)))
       (parse-marker-id (.id el)))))

#?(:clj
   (defn- serialize-nodes
     "Stored-form HTML for a seq of Jsoup nodes.

      Pre : every node's owner document already has prettyPrint off —
            `split-merged` sets it once, and Jsoup resolves output settings by
            walking to the owner document.
      Post: the nodes' outer HTML, concatenated, with no inserted whitespace.
            Pretty-printing here would add newlines that Quill renders as text."
     [nodes]
     (str/join (map (fn [^Node n] (.outerHtml n)) nodes))))

#?(:clj
   (defn split-merged
     "Split one merged document back into per-page stored HTML.

      Pre : `merged` is HTML read out of the editor. `page-numbers` is every
            page number the document was merged from — the caller owns that
            list, because the editor cannot report a page whose marker the
            reader deleted.
      Post: a map from EVERY page number in `page-numbers` to its slice. A page
            whose marker is absent maps to \"\". Markers are stripped from every
            slice.
      Post: content above the FIRST marker joins the lowest page number. There
            is no earlier page to absorb it, and dropping it would lose the
            reader's text.
      Invariant: no page gains text that sat below its own marker. That is the
            absorb-upward rule, and it is what a test can check directly.

      Text nodes and inline elements at the top level are kept with whatever
      page is open at that point, so a reader who types loose text between two
      blocks does not lose it."
     [merged page-numbers]
     (let [wanted (into (sorted-set) (remove nil? page-numbers))
           lowest (first wanted)
           doc (Jsoup/parseBodyFragment (or merged ""))
           _ (.prettyPrint (.outputSettings doc) false)
           nodes (vec (.childNodes (.body doc)))
           ;; Walk once, opening a new bucket at each marker whose page is one
           ;; the caller asked for. A marker for an unknown page is dropped
           ;; rather than trusted: it can only come from a paste or a hand edit,
           ;; and honouring it would write a page row that does not exist.
           buckets (reduce
                     (fn [{:keys [current acc] :as st} node]
                       (let [page (when (instance? Element node) (marker-page node))]
                         (cond
                           (and page (contains? wanted page))
                           (assoc st :current page)

                           page
                           st                              ; unknown page: drop the marker

                           :else
                           (assoc st :acc (update acc (or current lowest) (fnil conj []) node)))))
                     {:current nil :acc {}}
                     nodes)]
       (into {}
         (for [n wanted]
           [n (let [own (get (:acc buckets) n)]
                (if (seq own) (serialize-nodes own) ""))])))))

#?(:clj
   (defn strip-markers
     "Remove every page-boundary marker from `html`, leaving its other content.

      Pre : `html` is stored-form or editor-form HTML.
      Post: no element whose `id` parses as a page marker remains. Every other
            node is byte-identical.

      Content LEAVING the merged surface needs this. A selection can span a
      boundary, so an extract or a card made from one would otherwise carry the
      literal text `Page 7` into its own row — and `clean-html`, which the
      extract path runs, strips the `id` that identifies the heading while
      keeping the text. That is the same ordering trap
      `sphinx_import/strip-headerlinks!` records: after the sanitizer the
      construct is unrecognizable, so this must run BEFORE it."
     [html]
     (if (or (nil? html) (not (str/includes? html marker-id-prefix)))
       html
       (let [doc (Jsoup/parseBodyFragment html)]
         (.prettyPrint (.outputSettings doc) false)
         (doseq [^Element el (vec (.select doc (str "[id^=" marker-id-prefix "]")))]
           (when (parse-marker-id (.id el))
             (.remove el)))
         (.html (.body doc))))))

;; ── The page-number bound ──────────────────────────────────────────────────

(defn clamp-page
  "`page` bounded to the pages a document actually has.

   Pre : `total` is the document's page count. 0 or nil means the count is not
         known — a score topic has no page children, and `!total` is seeded 0
         before the viewer reports its own count.
   Post: 1 <= result <= total when `total` is a positive number. `page`
         unchanged when it is nil or when `total` is not positive.
   Inv : no caller can put a page number on screen, in the URL, or in
         `last_page_<doc>` that the document does not have.

   WHY THIS EXISTS. A scroll report leaked from another document set a 5-page
   document to page 40. The number reached the page field, the address bar and
   the stored last page, and it survived a reload — after which the document
   showed `40` beside `of 5`, a dead Next button, and a control reading
   \"Mark page 40 as done\" that targeted the ROOT topic, because
   `get-page-info*` answers nil for a page that is not there and
   `page-topic-id` falls back to the topic id.

   `freememo.topic-state` calls it in two places, and both are needed for
   different reasons. `on-page-change!` is the single funnel every live page
   change passes through, so one call there covers the toolbar, the viewer and
   the scroll report. `initial-page` covers a value ALREADY STORED by the
   shipped defect — clamping on read is what makes those rows self-healing,
   which is why no migration over `settings` exists.

   Tagging the scroll report (`editor-actions/!scrolled-page`) removes the one
   source known to have produced a bad number. This removes the consequence for
   every source, including sources not yet written."
  [page total]
  (if (and page (number? total) (pos? total))
    (max 1 (min total page))
    page))

;; ── The restore rule ───────────────────────────────────────────────────────

(defn next-missing-page
  "The page a restore control should re-create at a point in the document.

   Pre : `present` is the set of page numbers whose markers are still in the
         document. `page-above` is the page the insertion point currently
         belongs to, or nil when the point sits above every marker.
         `all` is every page number the document was merged from.
   Post: the LOWEST page number that is missing from `present`, is in `all`, and
         is greater than `page-above`. nil when there is none.
   Post: with markers 7 and 9 present, a point inside 7 answers 8. With 7 and 10
         present it answers 8, and answers 9 only after 8 is restored — which is
         why the rule is `lowest` rather than `the one below`.

   nil is the refusal signal. A caller MUST NOT insert a marker on nil: every
   page between the neighbours already has one, so there is nothing to restore
   and this control never creates a page."
  [present page-above all]
  (let [present (set present)
        floor (or page-above 0)]
    (->> (sort all)
         (filter #(and (> % floor) (not (contains? present %))))
         first)))
