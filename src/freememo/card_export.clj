(ns freememo.card-export
  "Cards as rows of a text file that another flashcard app imports.
   See plans/card-export-csv-tsv.md.

   Pure: no database, no HTTP. Three steps, each a separate fn so a test can
   reach it:

   1. `card-sides` expands one flashcard row into zero or more [front back]
      pairs of STORED html. A cloze card gives one pair per deletion, an
      overlapping card one per item plus its reveal-all card. Occlusion and
      score cards give none, because a text file cannot hold an image mask or
      a clip.
   2. A preset turns each stored-html side into the target's field text.
   3. An encoder joins the fields into a line.

   The Mnemosyne rules come from reading its importer (`tsv.py`, 2.11), not
   from documentation, which no longer exists online. Each rule names the
   failure it prevents, because none of them is visible from this side:
   Mnemosyne aborts the WHOLE import on one bad line."
  (:require [clojure.string :as str]
            [freememo.card-export-presets :as presets]
            [freememo.cloze :as cloze]
            [freememo.math :as math]
            [freememo.util :as util])
  (:import (java.io Writer)
           (org.jsoup.parser Parser)))

;; ---------------------------------------------------------------------------
;; Presets
;; ---------------------------------------------------------------------------

(def header-columns ["Front" "Back" "Document" "Kind"])

(defn resolve-preset
  "The preset for `preset-id`, with images on when asked for and allowed.
   Post: a preset map, or nil for an unknown id."
  [preset-id images?]
  (when-let [p (get presets/presets preset-id)]
    (assoc p :id preset-id
      :images? (boolean (and images? (presets/images-allowed? preset-id))))))

;; ---------------------------------------------------------------------------
;; 1. Card kinds → sides
;; ---------------------------------------------------------------------------

(def exported-kinds #{"basic" "cloze" "overlapping"})

(defn- join-blocks
  "Join non-blank html blocks, one per line."
  [& blocks]
  (str/join "<br>" (remove str/blank? blocks)))

(defn- overlapping-sides
  "One [front back] per item, plus the reveal-all card when the card has one.
   Reads the materialized Anki fields (`freememo.overlapping/expand`, stored on
   every save), so the export shows exactly the cards Anki would.
   Pre: `ol` is the parsed `overlapping` JSONB."
  [ol mark]
  (let [{:keys [question fields items]} ol
        n (count items)
        item-sides (for [k (range 1 (inc n))
                         :let [text (get fields (keyword (str "Text" k)))]
                         :when (not (str/blank? text))]
                     [(join-blocks question (cloze/front-for-ord text k))
                      (join-blocks question (cloze/back-for-ord text k mark))])
        full (:Full fields)]
    (cond-> (vec item-sides)
      (not (str/blank? full))
      (conj [(join-blocks question (cloze/front-for-ord full 21))
             (join-blocks question (cloze/back-for-ord full 21 mark))]))))

(defn card-sides
  "The [front back] pairs of stored html that `card` exports as.

   pre : `card` is a flashcard row with `:flashcards/*` keys and a parsed
         `:flashcards/overlapping`. `mark` wraps a revealed deletion's answer.
   post: [] for a kind outside `exported-kinds`. A cloze card gives one pair
         per distinct deletion number, in ascending order."
  [card mark]
  (case (:flashcards/kind card)
    "basic"       [[(or (:flashcards/question card) "")
                    (or (:flashcards/answer card) "")]]
    "cloze"       (let [text (or (:flashcards/cloze card) "")]
                    (mapv (fn [n] [(cloze/front-for-ord text n)
                                   (cloze/back-for-ord text n mark)])
                      (cloze/ords text)))
    "overlapping" (overlapping-sides (:flashcards/overlapping card) mark)
    []))

;; ---------------------------------------------------------------------------
;; 2. Stored html → field text
;; ---------------------------------------------------------------------------

(defn- decode-entities [s] (Parser/unescapeEntities s false))

(def ^:private block-end-re
  "A run of adjacent block-end tags. A block that closes inside another block
   (`</p></div>`) ends ONE line, so the run is one break. Only spaces and tabs
   may sit between them, so a `<br>` already turned into a newline keeps its own."
  #"(?i)(?:</(?:p|div|li|h[1-6]|tr|blockquote|pre|ul|ol|table)\s*>[ \t]*)+")

(defn html->text
  "Plain text of stored `html`: a line per block and per `<br>`, tags removed,
   entities decoded, runs of blank lines folded to one.
   Post: no `<tag>` survives; no line has trailing whitespace; nesting depth
   adds no blank line."
  [html]
  (-> (or html "")
    (str/replace #"(?i)<br\s*/?>" "\n")
    (str/replace block-end-re "\n")
    (str/replace #"(?i)<li[^>]*>" "- ")
    (str/replace #"<[^>]*>" "")
    decode-entities
    (str/replace " " " ")
    (str/replace #"[ \t]+\n" "\n")
    (str/replace #"\n{3,}" "\n\n")
    str/trim))

(defn- text-math
  "Rewrite `\\(tex\\)` regions in already-decoded text for `mode`."
  [text mode]
  (case mode
    :noji (str/replace text math/tex-region-re (fn [[_ tex]] (str "$$" tex "$$")))
    text))

(defn- mnemosyne-math
  "Rewrite `\\(tex\\)` regions in stored html as Mnemosyne's `<$>tex</$>`.
   The body is entity-decoded, because Mnemosyne hands it to LaTeX verbatim and
   `&lt;` is not TeX."
  [html]
  (str/replace html math/tex-region-re
    (fn [[_ tex]] (str "<$>" (decode-entities tex) "</$>"))))

(def ^:private img-re #"(?i)<img\b[^>]*>")
(def ^:private media-src-re #"(?i)src\s*=\s*[\"']/api/media/(\d+)[\"']")

(defn media-ids
  "Ids of the app media that `html`'s `<img>` tags show, in document order."
  [html]
  (vec (keep (fn [tag] (some-> (re-find media-src-re tag) second parse-long))
         (re-seq img-re (or html "")))))

(defn media-filename
  "The file a media id is written as. Same rule as Anki push
   (`anki-sync-helpers/fetch-and-store-media!`), so one image has one name."
  [media-id mime-type]
  (str media-id "." (util/mime->ext mime-type)))

(defn- mnemosyne-images
  "Each `<img>` of app media as `<img src=\"media/<file>\">` when `names` holds
   its file, and every other `<img>` removed. Mnemosyne reads only a
   double-quoted src, and tags a card MISSING_MEDIA for a file it cannot find.
   `names` is {media-id filename}; nil removes every image."
  [html names]
  (str/replace html img-re
    (fn [tag]
      (let [id (some-> (re-find media-src-re tag) second parse-long)
            file (when id (get names id))]
        (if file (str "<img src=\"media/" file "\">") "")))))

(def ^:private numeric-ref-re #"&#(?:[xX]([0-9a-fA-F]+)|([0-9]+));")

(defn- neutralize-break-refs
  "Write the numeric references that Mnemosyne decodes into a tab or a line
   break as their replacement. It decodes them BEFORE it splits the line, so
   `&#9;` would split a field and `&#10;` would end a line."
  [s]
  (str/replace s numeric-ref-re
    (fn [[ref hex dec]]
      (let [code (if hex (Long/parseLong hex 16) (Long/parseLong dec))]
        (case code
          9 " "
          (10 13) "<br>"
          ref)))))

(defn- mnemosyne-field
  [html names]
  (-> (or html "")
    (mnemosyne-images names)
    mnemosyne-math
    neutralize-break-refs
    (str/replace "\t" " ")
    (str/replace #"\r\n|\r|\n" "<br>")
    str/trim))

(defn- visible?
  "Whether a field shows anything: text, or an image."
  [field preset]
  (if (= :html (:fields preset))
    (or (str/includes? field "<img")
        (not (str/blank? (html->text field))))
    (not (str/blank? field))))

(defn field
  "Stored `html` as `preset`'s field text. `names` is {media-id filename} for a
   preset with images, else nil."
  [html preset names]
  (case (:fields preset)
    :html (mnemosyne-field html names)
    :text (text-math (html->text html) (:math preset))))

;; ---------------------------------------------------------------------------
;; 3. Encoders
;; ---------------------------------------------------------------------------

(defn csv-cell
  "One RFC 4180 field: quoted when it holds a comma, a quote or a line break,
   with every quote doubled."
  [s]
  (if (re-find #"[\",\r\n]" s)
    (str "\"" (str/replace s "\"" "\"\"") "\"")
    s))

(defn- tsv-cell
  "A plain TSV field: no tab and no line break, because TSV has no quoting."
  [s]
  (-> s (str/replace #"[\t\r\n]+" " ") str/trim))

(defn encode-line
  "One line of `preset`'s file, terminator included."
  [preset cells]
  (case (:encoding preset)
    :csv           (str (str/join "," (map csv-cell cells)) "\r\n")
    :plain-tsv     (str (str/join "\t" (map tsv-cell cells)) "\n")
    ;; Two cells exactly, a non-empty back, and no tab or line break inside a
    ;; cell (`mnemosyne-field` guarantees the last). Mnemosyne rstrips the line
    ;; before it splits, so an empty back drops the separator and the line
    ;; reads as one column, which aborts the import. A named entity is not
    ;; decoded there, so `&nbsp;` keeps the back non-empty and renders blank.
    :mnemosyne-tsv (let [[front back] cells]
                     (str front "\t" (if (str/blank? back) "&nbsp;" back) "\n"))))

;; ---------------------------------------------------------------------------
;; The whole file
;; ---------------------------------------------------------------------------

(defn- mark-for [preset]
  (if (= :html (:fields preset))
    (fn [answer] (str "<b>" answer "</b>"))
    identity))

(defn card-lines
  "The encoded lines `card` exports as, and why any side was dropped.

   pre : `card` as for `card-sides`, plus `:root_title`.
   post: {:lines [s] :dropped n}. `:dropped` counts sides whose front shows
         nothing after the preset's transform (an image-only front in a text
         preset)."
  [card preset names]
  (let [mark (mark-for preset)
        document (or (:root_title card) "")
        kind (:flashcards/kind card)]
    (reduce
      (fn [acc [front-html back-html]]
        (let [front (field front-html preset names)
              back (field back-html preset names)]
          (if (visible? front preset)
            (update acc :lines conj
              (encode-line preset (if (:header? preset)
                                    [front back document kind]
                                    [front back])))
            (update acc :dropped inc))))
      {:lines [] :dropped 0}
      (card-sides card mark))))

(def empty-summary {:cards 0 :rows 0 :dropped 0 :skipped {}})

(defn- tally
  "Add one card's outcome to `summary`."
  [summary card {:keys [lines dropped]}]
  (let [kind (:flashcards/kind card)]
    (if (contains? exported-kinds kind)
      (-> summary
        (update :cards inc)
        (update :rows + (count lines))
        (update :dropped + dropped))
      (update-in summary [:skipped kind] (fnil inc 0)))))

(defn write-cards!
  "Write the file for `cards` to `w`, a line at a time.

   pre : `cards` as for `card-lines`. `names` is {media-id filename} for a
         preset with images, else nil. `w` is open.
   post: `w` holds the header (when the preset has one) then every line. No
         BOM is written: Mnemosyne's BOM check is broken, so a BOM would become
         part of the first card's front. Returns the summary
         {:cards :rows :dropped :skipped {kind n}}.
   Inv : the heap holds one card's lines at a time beyond `cards` itself."
  [^Writer w preset cards names]
  (when (:header? preset)
    (.write w ^String (encode-line preset header-columns)))
  (reduce (fn [summary card]
            (let [out (card-lines card preset names)]
              (doseq [line (:lines out)] (.write w ^String line))
              (tally summary card out)))
    empty-summary cards))

(defn summarize
  "The summary `write-cards!` would return, without writing anything."
  [preset cards]
  (reduce (fn [summary card] (tally summary card (card-lines card preset nil)))
    empty-summary cards))

(defn cards-media-ids
  "Distinct app media ids shown by the exported sides of `cards`, in order."
  [cards]
  (into [] (comp (mapcat #(card-sides % identity))
             (mapcat identity)
             (mapcat media-ids)
             (distinct))
    cards))

(defn download-filename
  "A download name from `base` and the preset.
   Post: only ASCII letters, digits and `-` before the extension, so no
         Content-Disposition parser and no file system treats it specially.
         A title with no ASCII letter falls back to `freememo-cards`."
  [base preset]
  (let [stem (-> (or base "")
               (str/replace #"[^A-Za-z0-9]+" "-")
               (str/replace #"^-+|-+$" ""))
        stem (if (str/blank? stem) "freememo-cards" (subs stem 0 (min 80 (count stem))))]
    (str stem "-" (:id preset) "." (if (:images? preset) "zip" (:ext preset)))))
