(ns freememo.poem-cards
  "Pure HTML builders for the three cards a poem word panel creates, plus the
   source-chip label the cards and the panel both render.

   No requires beyond clojure.string, deliberately. `test/freememo/poem_cards_test.clj`
   loads this namespace and needs nothing started. Everything here is a string
   in, a string out.

   Every block element carries `dir=\"rtl\"` (spec row 6.5). The Anki basic and
   cloze templates set no `dir`, so a card's own markup is the only thing that
   makes it render right to left.

   A verse's words come from the package's `words` vector and are NEVER re-split
   here. A note's `word` index and a rendered word agree only while ONE
   tokenization exists (`plans/poem-package-format.md` §3), and this namespace
   reads that vector by its `:i` field rather than by vector position, because a
   token the builder dropped leaves a gap.

   The tapped word is identified by that index, so the same word twice in one
   verse yields two DIFFERENT questions. That is load-bearing: the flashcard
   uniqueness index is (topic_id, kind, md5(question)) with ON CONFLICT DO
   NOTHING, so two questions that came out equal would silently save one row
   (spec §4, C4.2)."
  (:require [clojure.string :as str]))

;; ---------------------------------------------------------------------------
;; Primitives
;; ---------------------------------------------------------------------------

(defn escape
  "HTML-escape `s`. Verse words and source names are TEXT from a book; a note's
   `html` is package HTML the validator already constrained (format §4) and is
   never escaped."
  [s]
  (-> (str s)
    (str/replace "&" "&amp;")
    (str/replace "<" "&lt;")
    (str/replace ">" "&gt;")))

(defn p-rtl
  "One right-to-left block. `inner` is HTML, already escaped or already trusted."
  [inner]
  (str "<p dir=\"rtl\">" inner "</p>"))

(def half-gap
  "The gap between صدر and عجز inside one line. Three non-breaking spaces, so a
   renderer that collapses whitespace still shows the caesura — the same three
   spaces the package's stored `content` fallback uses (format §7)."
  "&nbsp;&nbsp;&nbsp;")

(defn source-label
  "Chip text for a note: the source's short name and its printed page.
   `printed-page` may be nil for a book with no printed numbering."
  [short printed-page]
  (if (some? printed-page)
    (str short "، ص " printed-page)
    (str short)))

;; ---------------------------------------------------------------------------
;; Verse rendering
;; ---------------------------------------------------------------------------

(defn word-at
  "The word map whose `:i` is `index`, or nil. Reads by field, not by position."
  [words index]
  (some #(when (= index (:i %)) %) words))

(defn word-text
  "The text of the word at `index`, or nil."
  [words index]
  (:text (word-at words index)))

(defn words-html
  "The verse's words as inline HTML, صدر then عجز with `half-gap` between them.
   The word whose `:i` is `strong-index` is wrapped in `<strong>`; pass nil to
   wrap none."
  [words strong-index]
  (let [render (fn [w]
                 (let [t (escape (:text w))]
                   (if (= (:i w) strong-index) (str "<strong>" t "</strong>") t)))
        sadr (filter #(= "sadr" (:half %)) words)
        ajuz (filter #(= "ajuz" (:half %)) words)
        join (fn [ws] (str/join " " (map render ws)))]
    (cond
      (empty? ajuz) (join sadr)
      (empty? sadr) (join ajuz)
      :else (str (join sadr) half-gap (join ajuz)))))

(defn verse-question-html
  "The verse as one rtl block, with the tapped word in `<strong>`. This is the
   first half of a word card's question; the asking line follows it."
  [verse strong-index]
  (p-rtl (words-html (:words verse) strong-index)))

;; ---------------------------------------------------------------------------
;; Answers
;; ---------------------------------------------------------------------------

(defn notes-answer-html
  "The answer side of a word card: each note's own HTML, followed by one rtl
   block naming where it came from.

   `notes` is a seq of {:html <package html> :label <source-label string>}. The
   caller resolves the label, because a source's short name lives in
   `sources.json` and this namespace reads no package."
  [notes]
  (str/join
    (for [{:keys [html label]} notes]
      (str (str html)
        (p-rtl (str "— " (escape label)))))))

;; ---------------------------------------------------------------------------
;; The three cards
;; ---------------------------------------------------------------------------

(defn verse-card
  "Card data for the verse card: kind \"cloze\", عجز hidden (spec row 6.1).
   Reads `:sadr` and `:ajuz` verbatim, so the card carries the book's own
   punctuation — the word vector is a tokenization, not the text."
  [verse]
  {:c (p-rtl (str (escape (:sadr verse))
               " {{c1::" (escape (:ajuz verse)) "}}"))
   :a nil})

(defn meaning-card
  "Card data for the word-meaning card: kind \"basic\" (spec row 6.2).
   `notes` are the word's gloss notes, in {:html :label} form."
  [verse word-index notes]
  (let [w (escape (word-text (:words verse) word-index))]
    {:q (str (verse-question-html verse word-index)
          (p-rtl (str "ما معنى «" w "»؟")))
     :a (notes-answer-html notes)}))

(defn iraab-card
  "Card data for the إعراب card: kind \"basic\" (spec row 6.3).
   `notes` are the word's iraab notes, in {:html :label} form."
  [verse word-index notes]
  (let [w (escape (word-text (:words verse) word-index))]
    {:q (str (verse-question-html verse word-index)
          (p-rtl (str "أعرب «" w "»")))
     :a (notes-answer-html notes)}))

;; ---------------------------------------------------------------------------
;; Extract body (spec row 6.6)
;; ---------------------------------------------------------------------------

(defn verse-extract-html
  "The body of the extract child topic حفظ البيت creates: the verse as one rtl
   block, with no word emphasised."
  [verse]
  (p-rtl (str (escape (:sadr verse)) half-gap (escape (:ajuz verse)))))

(defn verse-extract-title
  "Title for that extract. Trimmed, because `topics.title` is shown in the
   hierarchy and the Learn queue."
  [verse]
  (let [s (str/trim (str (:sadr verse)))]
    (if (< 80 (count s)) (subs s 0 80) s)))

;; ---------------------------------------------------------------------------
;; Saved-state matching (spec row 6.9)
;; ---------------------------------------------------------------------------

(defn normalize-for-match
  "Attribute-stripped, whitespace-collapsed form of a card field, for telling
   whether the topic already holds this card.

   Comparing the raw HTML would be wrong: `:add-card` runs every field through
   `sanitize-card-field` before the insert, and Jsoup re-serializes attributes
   and whitespace — so the stored question is equal in meaning and unequal in
   bytes to the one built here.

   TAG NAMES SURVIVE, and that is the whole difficulty. An earlier version
   stripped tags outright, which erased the `<strong>` marking the tapped word —
   and that emphasis is the ONLY thing separating two questions about the same
   word in two places within one verse. The two then matched, so saving one card
   reported the other as already saved, while the database held two distinct
   rows. Keeping `<strong>` costs nothing: the sanitizer rewrites attributes and
   whitespace, never a tag name."
  [html]
  (-> (str html)
    (str/replace "&nbsp;" " ")
    (str/replace #"<\s*(/?)\s*([A-Za-z0-9]+)[^>]*>"
      (fn [[_ slash tag]] (str "<" slash (str/lower-case tag) ">")))
    (str/replace #"\s+" " ")
    (str/replace #"\s+<" "<")
    (str/replace #">\s+" ">")
    str/trim))
