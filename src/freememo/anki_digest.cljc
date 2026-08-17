(ns freememo.anki-digest
  "Content digests for Anki reconcile — one per side, per card.

   THE contract, and the reason there are two of them:

     `anki-digest` is only ever compared with an earlier `anki-digest`.
     `local-digest` is only ever compared with an earlier `local-digest`.

   The two are NEVER compared with each other. That is what separates \"did Anki
   change?\" from \"did FreeMemo change?\". The shipped
   `anki-sync-server/stripped=` compares the two SIDES, which answers neither —
   it reports that they differ, so a card edited only in FreeMemo reads as
   Anki-modified and both bulk buttons skip it.

   Three consequences of the cross-side comparison being gone:

   1. `score` and `overlapping` participate fully. Their Anki fields are
      app-derived and have no Anki→FreeMemo inverse, which is why the pull path
      skips them and the library overlay reports them as never modified. A
      same-side digest needs no inverse.
   2. Nothing here has to agree across the CLJ/CLJS boundary. `anki-digest` runs
      on the client, where notesInfo lands; `local-digest` runs on the server,
      where the row lives. SHA-256 agrees anyway.
   3. Media filenames, header text and the Links field cannot desynchronise the
      comparison, because neither digest sees the other side's rendering.

   pre  (both fns): `kind` is a card kind string.
   post (both fns): a 64-character lowercase hex string.
   inv : equal inputs give equal digests on both platforms and across restarts —
         no hash seed, no map iteration order and no timestamp enters the input."
  (:require
   [clojure.string :as str]
   [freememo.math :as math]
   [freememo.anki-fields :as fields]
   #?(:cljs [goog.crypt :as gcrypt]))
  #?(:clj (:import [java.security MessageDigest])
     :cljs (:import [goog.crypt Sha256])))

;; ---------------------------------------------------------------------------
;; Hash
;; ---------------------------------------------------------------------------

(defn- sha256-hex
  "Hex SHA-256 of `s`, UTF-8 encoded.
   Both branches implement the same standard, so a digest computed on either peer
   is readable by the other even though the contract does not require it."
  [s]
  #?(:clj (let [md (MessageDigest/getInstance "SHA-256")]
            (->> (.digest md (.getBytes ^String s "UTF-8"))
              (map #(format "%02x" (bit-and % 0xff)))
              (apply str)))
     :cljs (let [h (Sha256.)]
             (.update h (gcrypt/stringToUtf8ByteArray s))
             (gcrypt/byteArrayToHex (.digest h)))))

(def ^:private unit-separator
  "Field boundary inside a digest input, U+001F.

   A control character, so no field value can contain one and forge a boundary:
   Jsoup drops it, Quill cannot produce it, and `strip-html` never introduces
   one. A printable delimiter could be forged — a card whose answer starts with
   it would digest identically to one whose question ends with it.

   Written as an escape, not as a raw byte. A raw control character in source
   survives neither every editor nor every copy-paste, and if it silently became
   \"\" the digest would still be 64 valid hex characters with every field
   boundary collapsed."
  "\u001f")

(defn- digest-of
  "Digest a card's comparable parts. `kind` leads the input, so a kind change is
   a change on both sides even when every field survives it."
  [kind parts]
  (sha256-hex (str/join unit-separator (cons (or kind "") (map #(or % "") parts)))))

;; ---------------------------------------------------------------------------
;; Anki side
;; ---------------------------------------------------------------------------

(defn- anki-parts
  "The Anki-side comparable values for `kind`, read BY NAME.

   `anki-fields/content-field-names` owns the selection and the reason it is not
   positional. An unrecognised kind digests EVERY field the note carries, in
   name order: a sixth card kind added without an entry there would otherwise
   digest by another kind's fields and silently mis-classify, whereas reading
   everything can only report a change that is not one — noisy, never lossy.
   `CLAUDE.md` records the same failure shape for topic kinds missing from the
   badge switches.

   A field the note does not carry digests as \"\", which is what makes a foreign
   or half-migrated model classify as changed rather than stop the run.

   `strip-push-decorations`, NOT `strip-html`. Markup is content here: a word
   restyled to `<code>` in Anki has to move this digest, and stripping tags made
   it identical. Only what push itself prepended comes off, so a header change
   does not read as an Anki-side edit — which would classify `:pull` where
   `:push` is correct.

   Deliberately asymmetric with `local-parts`: this side does NOT cover the
   bibliography, because `Links` is outside `content-field-names` and no Anki-side
   edit of it is ours to detect. The two digests never meet (see the ns
   docstring), so they need not cover the same content — only the same content as
   their own earlier selves."
  [kind anki-fields]
  (if-let [names (get fields/content-field-names kind)]
    (mapv (fn [n] (fields/strip-push-decorations (get-in anki-fields [(keyword n) :value]))) names)
    (mapv (fn [k] (fields/strip-push-decorations (get-in anki-fields [k :value])))
      (sort (keys anki-fields)))))

(defn anki-digest
  "Digest of what Anki currently holds for a note.

   pre : `anki-fields` is a notesInfo `:fields` map,
         {:FieldName {:value s :order n}}, keywordized. `kind` is the LOCAL
         card's kind — the note's model name is not consulted, so a note migrated
         between app-owned models digests by what it is for, not by what it sits
         on.
   post: a hex digest over the kind's content fields only."
  [anki-fields kind]
  (digest-of kind (anki-parts kind anki-fields)))

;; ---------------------------------------------------------------------------
;; Local side
;; ---------------------------------------------------------------------------

(defn- parsed-jsonb
  "The value of a JSONB column, asserted to be already parsed.

   pre : `v` is a Clojure map, or nil.
   post: `v` unchanged.

   next.jdbc hands back a `PGobject` unless the query ran it through
   `db/pgobject->clj`, and a `PGobject` answers every `get` with nil — which
   would make the digest blind to the whole column while still returning a valid
   64-character result. Two existing queries parse it (`db.clj:3531`, `4056`);
   a new one that forgets is a caller bug, and this says so loudly instead of
   digesting nothing."
  [v column]
  (if (or (nil? v) (map? v))
    v
    (throw (ex-info "flashcards JSONB column reached anki-digest unparsed — the query must apply db/pgobject->clj"
             {:column column :type (str (type v))}))))

(defn- stable-str
  "Deterministic string for a parsed JSONB value.

   Sorts by the printed key at every level, then reads the value through the
   ORIGINAL key. Sorting a copy of the keys and re-deriving them (`keyword` of a
   `str`ed keyword yields `:\":k\"`, not `:k`) reads nil for every entry, which
   digests every distinct map identically."
  [v]
  (cond
    (map? v) (str "{"
                  (str/join "," (for [[k value] (sort-by (comp pr-str key) v)]
                                  (str (pr-str k) ":" (stable-str value))))
                  "}")
    (sequential? v) (str "[" (str/join "," (map stable-str v)) "]")
    (nil? v) ""
    :else (pr-str v)))

(def ^:private io-text-keys
  "The `io_fields` keys the digest covers, in a fixed order.
   The same six the pull path diffs (`io-pull-field-names`), so an occlusion
   edit pulled from Anki clears the flag it raised."
  [:header :footer :remarks :sources :extra1 :extra2])

(def ^:private settings-part-count
  "How many parts `settings-parts` contributes. Stated so a reader of
   `local-parts` can see that the tail is fixed-width and kind-independent."
  2)

(defn- settings-parts
  "The push settings the card's Anki note embeds, read from the row's snapshot
   columns.

   Pre : `card` carries `:flashcards/anki_header_text` and
         `:flashcards/anki_bibliography_html` — nil is legal and means \"none\".
   Post: exactly `settings-part-count` strings.

   These are NOT the source of truth. Push resolves the header from
   `settings/get-anki-header-for-topic` and the bibliography from the source
   walk; `db/refresh-anki-settings-snapshot!` copies what it resolved onto the
   row. The columns exist so that a change to either one moves this digest, which
   is the only way a settings edit can dirty a card — a `settings` write and a
   `sources` write touch no flashcards row, so the timestamp predicate never saw
   them either.

   Appended to EVERY kind's parts, occlusion included. Occlusion's own
   `io_fields[:header]` is a different value with a different owner (the user
   types it per card in the authoring modal), and it stays a separate part, so
   editing one cannot mask the other.

   NULL and \"\" digest alike, and that is correct: `prepend-header` writes
   nothing for a blank header, so the two produce the same Anki note."
  [card]
  [(or (:flashcards/anki_header_text card) "")
   (or (:flashcards/anki_bibliography_html card) "")])

(defn- local-parts
  "The row's comparable content, plus the push settings its note embeds.

   Content is normalised ONLY by `math/stored->compare-html` — the cloze brace
   spacing that push applies. Tags are NOT stripped: markup is content here, and
   a word restyled to `<code>` has to move this digest. `strip-html` used to run
   after the spacing, which made every markup-only edit invisible and left the
   card reading `:unchanged` while the card table counted it unsynced.

   `math/stored->compare-html` is gated on `math/cloze-bearing-kinds`, THE single
   definition of which kinds get that spacing. Applying it unconditionally would
   make every basic card with nested TeX braces digest differently after each
   push.

   Nothing here strips push decorations, because the row never holds one — the
   header lives in `anki_header_text`, not inside `question`/`cloze`.
   `anki-parts` does strip them, for the opposite reason.

   Known limitation, `score`: the digest covers which direction the card runs and
   which group it belongs to. Anki's Front and Back for a score card are media
   generated from that group, so re-cutting the audio or re-rendering the sheet
   changes neither part and no local change is reported. Fixing that needs the
   group's media identity, which is not on the card row — it would change this
   function's input contract, so it is stated rather than guessed at.

   An unrecognised kind digests every content column, for the reason `anki-parts`
   gives."
  [card]
  (let [kind (:flashcards/kind card)
        norm #(math/stored->compare-html % kind)
        io   #(parsed-jsonb (:flashcards/io_fields card) :io_fields)
        ovl  #(parsed-jsonb (:flashcards/overlapping card) :overlapping)
        content
        (case kind
          "basic"       [(norm (:flashcards/question card))
                         (norm (:flashcards/answer card))]
          "cloze"       [(norm (:flashcards/cloze card))]
          "occlusion"   (mapv #(norm (get (io) %)) io-text-keys)
          "score"       [(str (:flashcards/score_direction card))
                         (str (:flashcards/score_group_id card))]
          "overlapping" [(norm (:flashcards/question card))
                         (stable-str (ovl))]
          (into [(norm (:flashcards/question card))
                 (norm (:flashcards/answer card))
                 (norm (:flashcards/cloze card))
                 (str (:flashcards/score_direction card))
                 (str (:flashcards/score_group_id card))
                 (stable-str (ovl))]
            (mapv #(norm (get (io) %)) io-text-keys)))]
    (into content (settings-parts card))))

(defn local-digest
  "Digest of what FreeMemo currently holds for a card.

   pre : `card` is a flashcards row with namespaced keys, carrying at least
         `:flashcards/kind`, the content columns its kind reads, and the two
         snapshot columns `anki_header_text` / `anki_bibliography_html`. JSONB
         columns are parsed (see `parsed-jsonb`). A query that omits a snapshot
         column silently digests it as \"\", so a settings change would stop
         dirtying the card — select both, or add them to the query's column list.
   post: a hex digest over that kind's editable content AND the push settings its
         note embeds.

   Sensitive to markup. `<code>x</code>` and `x` are different cards here, which
   is the whole point of the fn.

   Deliberately blind to `updated_at`. The timestamp moves whenever a write
   touches the row, including a write that changes nothing after normalisation.
   `db.flashcards.local_current_digest` holds this value for every row, and every
   staleness surface reads that column — so the card table and the reconcile can
   no longer disagree about whether a card is synced."
  [card]
  (digest-of (:flashcards/kind card) (local-parts card)))
