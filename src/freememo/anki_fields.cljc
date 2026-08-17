(ns freememo.anki-fields
  "Anki note field text — stripping and ordering. Two pure functions, no deps.

   Extracted from `freememo.anki-sync-helpers` so `freememo.anki-digest` can
   reach them without pulling in missionary, the CLJS push pipeline and
   `occlusion-svg`. The digest is what every other reconcile namespace depends
   on, so it must sit BELOW the sync machinery, not above it.

   `anki-sync-helpers` re-exports both names, so its existing callers are
   unaffected."
  (:require [clojure.string :as str]))

(def ^:private push-decoration-patterns
  "THE definition of what push adds to a field body, in one place.

   `anki-sync-helpers/prepend-header` writes the first one. The second is the
   legacy bibliography suffix, superseded by the `Links` field but still present
   on notes pushed before it existed.

   Two readers, and both need the same list: `strip-push-decorations` (the
   digest and the pull differ on everything EXCEPT these) and `strip-html`
   (which then removes markup too). A second copy would let one path treat a
   header as content while the other treats it as decoration, which reads as a
   card that changed and can never be made to stop."
  [#"<p[^>]*class=\"fm-header\"[^>]*>.*?</p>"
   #"\n?<hr[^>]*class=\"fm-(?:source|bibliography)\"[^>]*>[\s\S]*$"])

(defn strip-push-decorations
  "Remove what push added to an Anki field body. Keep every other tag.

   Pre : `text` is an Anki field value, or nil.
   Post: `text` without the `fm-header` paragraph and without the legacy
         `fm-source`/`fm-bibliography` block. Markup is otherwise byte-identical
         — `<code>`, `<b>`, `<table>` and their attributes all survive.
   Inv : idempotent. A field with no decoration comes back unchanged, so a
         second application cannot remove content.

   This is the half of `strip-html` the digest and the pull still need. Push
   prepends the header INSIDE `Front`/`Text` rather than carrying it in a field
   of its own, so a comparison that does not remove it reports a difference on
   every header-bearing card — 738 of 1538 notes carried one when this was
   written. `strip-html` cannot serve that purpose any more, because its third
   step removes the markup the comparison now exists to see.

   Callers must run this BEFORE `html-cleaner/clean-html`. `fm-header` is not in
   `quill-class-allow-list`, so the sanitizer removes the class that identifies
   the paragraph and leaves the header text behind as content, unrecoverably.
   `CLAUDE.md` records the same ordering trap for
   `sphinx-import/strip-headerlinks!`."
  [text]
  (if (str/blank? text)
    text
    (reduce (fn [s re] (str/replace s re "")) text push-decoration-patterns)))

(defn strip-html
  "Remove all HTML tags and push-time decorations from Anki field text.
   Strips: fm-header tagged headers, source/bibliography suffix (<hr>...),
   then all remaining HTML tags.

   Still the right fn for a CROSS-side comparison (`anki-sync-server/stripped=`,
   the library overlay) and for display. The reconcile digests use
   `strip-push-decorations` instead — they compare each side against its own
   earlier value, so they can afford to see markup, and must."
  [text]
  (if (str/blank? text)
    text
    (-> text
      strip-push-decorations
      (str/replace #"<br\s*/?>" "\n") ;; preserve line breaks
      (str/replace #"<[^>]*>" "") ;; strip all remaining tags
      str/trim)))

;; ---------------------------------------------------------------------------
;; Media srcs across the pull boundary
;; ---------------------------------------------------------------------------
;;
;; `anki-sync-helpers/rewrite-media-srcs` turns every `/api/media/<id>` into the
;; filename push uploaded to Anki, and it is the ONE push transform with no
;; inverse the pull path can reach — the id→filename map lives in the push run
;; and is gone by the time anything reads the note back.
;;
;; So an image-bearing field is compared with its `<img>` tags masked, and the
;; value written locally keeps the LOCAL tags. An Anki-side text edit then lands
;; without turning `/api/media/412` into `412.png`, which this app cannot serve.
;;
;; The previous behaviour was worse than either: `strip-html` removed the `<img>`
;; outright, so any pull of an image-bearing card wrote bare text over the image.

(def ^:private img-tag-re
  "One `<img …>` tag, self-closing or not."
  #"<img\b[^>]*>")

(def ^:private img-mask-token
  "Stands in for one `<img …>` tag while two sides are compared.

   U+001E, a control character, for the reason `anki-digest/unit-separator`
   gives: no field value can contain one and forge a match. Jsoup drops it and
   Quill cannot produce it. This token never reaches the database — it exists
   only inside a comparison.

   Written as escapes, not as raw bytes: a raw control character in source
   survives neither every editor nor every copy-paste, and if it silently
   became \"img\" the mask would collide with the literal word."
  "\u001eimg\u001e")

(defn mask-img-tags
  "Replace every `<img …>` tag with `img-mask-token`.

   Pre : `html` is a string, or nil.
   Post: a string in which no `<img>` tag survives, and everything else is
         unchanged.
   Inv : two fields that differ ONLY in their image srcs mask to the same string.

   For comparison only. Never store the result."
  [html]
  (str/replace (or html "") img-tag-re img-mask-token))

(defn img-tags
  "The `<img …>` tags of `html`, in document order.
   Pre : `html` is a string, or nil.  Post: a vector of tag strings."
  [html]
  (vec (re-seq img-tag-re (or html ""))))

(defn restore-img-tags
  "`anki-html` with its i-th `<img>` tag replaced by `local-html`'s i-th.

   Pre : both are strings, or nil.
   Post: the text of `anki-html` with local media references. An `<img>` beyond
         the local count keeps its Anki tag, because there is no local tag to
         take — that is the case where the user added an image in Anki, and
         Anki's own reference is the only one that resolves there.
   Inv : with no `<img>` on either side, returns `anki-html` unchanged.
   Inv : the returned tag count equals `anki-html`'s. A local tag with no Anki
         counterpart is dropped, which is correct — the user deleted that image
         in Anki, and the pull is applying that deletion.

   Positional. The counts can genuinely disagree, which is why both surplus cases
   are defined rather than asserted away. Matching by src is not available: the
   Anki side's srcs are exactly the ones this fn exists because we cannot map
   back."
  [anki-html local-html]
  (let [locals (img-tags local-html)]
    (if (empty? locals)
      (or anki-html "")
      (let [i (volatile! -1)]
        (str/replace (or anki-html "") img-tag-re
          (fn [tag] (or (nth locals (vswap! i inc) nil) tag)))))))

(def no-inverse-kinds
  "Card kinds whose Anki fields are derived from app state with no
   Anki→FreeMemo inverse.

   Score fields are media references generated from the score group. Overlapping
   fields are the expanded per-cloze list. Reading either back would have to
   guess the state it came from, so nothing is pulled for them.

   THE single definition. `note->pull-update` returns nil for these, and
   `anki-reconcile/apply-kind-limits` turns their pulls into conflicts. Two
   copies would let one path silently drop what the other reports.
   Occlusion is NOT here — its six text fields do pull, through `io_fields`."
  #{"score" "overlapping"})

(defn pullable-kind?
  "True iff Anki's version of a `kind` card can be written back locally."
  [kind]
  (not (contains? no-inverse-kinds kind)))

(def content-field-names
  "Per card kind, the Anki fields whose text the app is responsible for.

   Read BY NAME, never by position. `modelFieldAdd` appends, so a model that
   existed before `Links` was introduced holds it at a different index from one
   created after, and no single index map serves both — `io-pull-field-names`
   records the same lesson for the pull path, where the failure was writing
   `Extra 2`'s text into `:extra1`. A digest taken over the wrong positions
   fails more quietly still: the card simply reports a change forever.

   The omissions are deliberate. Each is a field the user owns in Anki, or one
   the app regenerates from other state:
     - `Remarks` / `Back Extra` are the user's Anki-side space (see
       `migrate-carry-fields`). An edit there must not read as a change.
     - `Links` is regenerated from the topic and its bibliography on every push.
     - occlusion `Image`, the three masks and `ID (hidden)` are app-owned media,
       tracked by `anki_key`, not by text.
   Occlusion keeps its own `Remarks`, because `io-pull-field-names` pulls it —
   the digest and the pull must cover the same fields, or a pulled edit would
   not clear the flag it raised.

   Guarded by `anki-reconcile-test/t26-digest-field-names-exist-in-their-model`."
  {"basic"       ["Front" "Back"]
   "cloze"       ["Text"]
   "occlusion"   ["Header" "Footer" "Remarks" "Sources" "Extra 1" "Extra 2"]
   "score"       ["Front" "Back"]
   "overlapping" (into ["Question"]
                   (concat (map #(str "Text" %) (range 1 21))
                     ["Full" "Original"]))})

(defn ordered-field-values
  "Field values of an Anki notesInfo :fields map, sorted by Anki's :order.
   Shared by pull (field-update detection), the library cards Anki overlay
   (anki-modified diff) and the reconcile digest — one definition of 'Anki's
   field order'."
  [fields-map]
  (->> fields-map
    (sort-by (fn [[_ v]] (or (:order v) 0)))
    (mapv (fn [[_ v]] (:value v)))))
