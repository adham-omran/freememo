(ns freememo.neoreader-export
  "Boox NeoReader reading-note export decoder. Pure: takes the export HTML,
   returns data. No database, no HTTP, no side effects.

   NeoReader writes one file per book to Google Drive under
   `onyx/<DEVICE>/Read/<Title> - <Author>(Author <Author>).html`. The format is
   undocumented and carries no anchor — no CFI, no spine index, no character
   offset. A highlight is identified only by its chapter heading and its quoted
   text, which is why `freememo.quote-anchor` exists.

   The document is FLAT: every chapter heading, highlight and note is a direct
   child of `<body>`, in reading-position order. A note is a SIBLING that
   follows its highlight, never a child of it, so the association is positional
   and nothing else.

   Recognition keys on semantics, not on Boox's palette. A highlight is a block
   holding a `yyyy-MM-dd HH:mm:ss` timestamp; a note is a block whose text opens
   with `注`. The inline colours (`#48b4c1` headings, `#ed6c00` timestamp rule)
   would also identify them, and were rejected: a NeoReader restyle would break
   the decode silently, and the sample cannot prove those colours are stable.

   Reading order is NOT timestamp order, and this decoder never sorts. Measured
   in the reference export (plans/boox-neoreader-google-drive-sync.md): chapter
   5 carries `20:59:56` before `20:59:33`, and the `Notes` chapter carries June
   stamps after an August stamp in chapter 8."
  (:require [clojure.string :as str])
  (:import [org.jsoup Jsoup]
           [org.jsoup.nodes Document Element]))

;; ── Recognition ────────────────────────────────────────────────────

(def ^:private timestamp-re
  "NeoReader stamps a highlight with local wall-clock time and no zone.
   The raw string is therefore the durable value — parsing it to an instant
   would invent a zone the export does not state."
  #"\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2}")

(def ^:private note-marker
  "The glyph NeoReader puts before a reader's own note. CJK `注` = note."
  "注")

(defn- timestamp-of
  "The timestamp a highlight block carries, or nil.

   Pre : `el` is a direct child of the export body, and the stamp — when there
         is one — sits in a `div`. The reference export puts it in a `div` at
         every one of its ~200 highlights.
   Post: a `yyyy-MM-dd HH:mm:ss` string from a descendant whose OWN text is
         exactly a timestamp, or nil. Matching on own-text and not on the
         block's full text stops a quote that mentions a date being read as
         the stamp.
   Risk: a NeoReader version that moved the stamp to a `span` would return nil
         here, and `parse-highlights` would then read the block as a chapter
         heading — corrupting the chapter of every later highlight rather than
         losing one. One sample cannot prove the `div` is stable."
  [^Element el]
  (some (fn [^Element d]
          (let [t (str/trim (.ownText d))]
            (when (re-matches timestamp-re t) t)))
        (.select el "div")))

(defn- note-text
  "The reader's own note carried by `el`, or nil.

   Pre : `el` is a direct child of the export body.
   Post: the note with its `注 |` marker and surrounding space removed, or nil
         when `el` is not a note block. Blank notes return nil — an empty note
         block carries nothing to place."
  [^Element el]
  (let [t (str/trim (.text el))]
    (when (str/starts-with? t note-marker)
      (let [stripped (-> t
                       (subs (count note-marker))
                       (str/replace #"^\s*\|?\s*" "")
                       str/trim)]
        (when (seq stripped) stripped)))))

(defn- quote-text
  "The quoted passage in a highlight block.

   Pre : `el` is a highlight block, so `timestamp-of` returned non-nil.
   Post: the passage text with the timestamp element removed first, so the
         stamp can never leak into the quote. Whitespace is left exactly as
         Jsoup renders it, INCLUDING however it renders `<br/>` — a quote can
         splice two source paragraphs, and `quote-anchor` compares with all
         whitespace removed, so the rendering cannot change the outcome."
  [^Element el]
  (let [clone (.clone el)]
    (doseq [^Element d (.select clone "div")]
      (when (re-matches timestamp-re (str/trim (.ownText d)))
        (.remove d)))
    (str/trim (.text clone))))

(defn- footer?
  "True when `el` is the trailing `BOOX Reading Notes from …` band.

   Pre : `el` is a direct child of the export body.
   Post: true iff `el` holds an `<hr>`. The band is the only rule in the
         reference export. A skip, not a stop: an `<hr>` appearing earlier
         must not truncate the decode."
  [^Element el]
  (some? (.selectFirst el "hr")))

(def ^:private book-header-tags
  "Tags carrying the book's own title and author, never a chapter heading.

   The export opens with `<h2>` for the title and `<h5>` for the author, both
   direct children of `<body>`. A chapter heading is a `<div>`. Without this
   exclusion the author's name becomes the chapter in force, and every
   highlight before the first real heading claims a chapter that no EPUB holds.

   Risk: a NeoReader version emitting chapter headings as h-tags would lose
   every chapter to this set. That failure is the recoverable one — a nil
   chapter sends its highlights down `quote-anchor`'s forward search, which
   still places them. Guessing the other way is not recoverable, because the
   author's name resolves to no chapter and never will."
  #{"h1" "h2" "h3" "h4" "h5" "h6"})

;; ── Decode ─────────────────────────────────────────────────────────

(defn parse-highlights
  "Decode a NeoReader export into an ordered highlight vector.

   Pre : `html` is the text of one `onyx/<DEVICE>/Read/*.html` export, or nil.
   Post: a vector of {:chapter :timestamp :quote :notes :index}, where
         `:index` counts from 0 in DOCUMENT order, `:chapter` is the heading in
         force at that point (nil before the first heading), and `:notes` is a
         vector holding every note block that follows the highlight. nil or
         blank input yields [].
   Post: `:chapter` never holds the book's own title or author. Those arrive as
         `book-header-tags` and are skipped.
   Inv : the vector is in document order. Nothing here sorts, and a caller
         that sorts by `:timestamp` breaks `quote-anchor`'s reading-order
         cursor — see the namespace docstring.
   Inv : a note reaching the vector always belongs to the highlight it follows
         in the file, EVEN when a chapter heading sits between the two. The note
         belongs to its highlight, not to the heading it happens to follow, so
         its entry lands in the chapter the highlight did. Hypothetical — in the
         reference export a note always follows its highlight immediately.
   Inv : a note appearing before any highlight is dropped, having nothing to
         attach to."
  [html]
  (if (str/blank? html)
    []
    (let [^Document doc (Jsoup/parse html)
          children (.children (.body doc))]
      (loop [remaining (seq children)
             chapter nil
             acc []]
        (if-not remaining
          acc
          (let [^Element el (first remaining)]
            (cond
              ;; Tag and `<hr>` checks come first: both are cheap, and they
              ;; skip the two Jsoup selects below for every block they match.
              (book-header-tags (.tagName el))
              (recur (next remaining) chapter acc)

              (footer? el)
              (recur (next remaining) chapter acc)

              (note-text el)
              (let [note (note-text el)
                    i (dec (count acc))]
                (recur (next remaining) chapter
                  (if (neg? i)
                    acc
                    (update-in acc [i :notes] conj note))))

              (timestamp-of el)
              (recur (next remaining) chapter
                (conj acc {:chapter chapter
                           :timestamp (timestamp-of el)
                           :quote (quote-text el)
                           :notes []
                           :index (count acc)}))

              :else
              (let [t (str/trim (.text el))]
                (recur (next remaining) (if (seq t) t chapter) acc)))))))))
