(ns freememo.audio-clip
  "An audio clip inside stored HTML, and every conversion out of that form.

   THE stored form is one `<audio>` element carrying an `/api/media/<id>` src:

     <audio controls src=\"/api/media/412\"></audio>
     <audio controls loop src=\"/api/media/412\"></audio>

   `loop` present means this clip loops, and nothing else records that setting.
   `freememo.html-cleaner` allow-lists the tag and those three attributes, and
   `quill-field`'s `audio-clip` blot is what keeps Quill from dropping the
   element on load. Both halves must agree: widening one without the other
   stores a clip that cannot survive an edit.

   Anki holds the same clip as `[sound:412.mp3]`, which is the only form Anki's
   Replay button plays (dae, 2024-11-23: \"[sound:…] tags are the only
   officially-supported way to use audio in Anki\"). An `<audio>` element pushed
   instead would play and loop, and lose Replay. Loop therefore does NOT survive
   a push, by choice — see plans/inline-audio-clips.md §3.4.

   Two boundaries, and this ns owns both:

   | Boundary   | Function             | Called from                        |
   |------------|----------------------|------------------------------------|
   | Anki push  | `stored->anki-html`  | `anki-sync-helpers` field builders  |
   | Anki pull  | `mask-clips`, `restore-clip-tags` | `note->pull-update`   |

   The pull pair mirrors `anki-fields/mask-img-tags` and `restore-img-tags`, for
   the same reason: push resolves a media id to an uploaded filename and that
   map dies with the run, so the conversion has no inverse. The comparison takes
   clips OUT on both sides, and the write keeps the LOCAL elements.

   No deps but `clojure.string`, so `freememo.anki-digest` and the CLJS push
   pipeline can both reach it."
  (:require [clojure.string :as str]))

(def ^:private clip-re
  "One stored clip element, with or without its closing tag.

   Jsoup writes `<audio …></audio>` and Quill's blot emits the same, but a
   hand-written `<audio …>` is equally a clip, so the closing tag is optional."
  #"(?is)<audio\b[^>]*>(?:\s*</audio>)?")

(def ^:private media-src-re
  "The media id inside a clip's src. Same shape as
   `anki-sync-helpers/extract-media-ids`, restricted to one element."
  #"/api/media/(\d+)")

(def ^:private sound-token-re
  "One `[sound:…]` token, Anki's own form. Mirrors Anki's `AV_TAGS`
   (rslib/src/text.rs), which is also non-greedy on the filename."
  #"\[sound:(.+?)\]")

(def ^:private clip-mask-token
  "Stands in for one clip while two sides are compared.

   U+001E for the reason `anki-fields/img-mask-token` gives: no field value can
   contain one and forge a match. Written as escapes, never as raw bytes. This
   token never reaches the database — it exists only inside a comparison.

   It differs from the image token, so a clip and an image cannot mask to the
   same string and hide a swap of one for the other."
  "\u001eclip\u001e")

(defn clip-tags
  "The clip elements of `html`, in document order.
   Pre : `html` is a string, or nil.  Post: a vector of element strings."
  [html]
  (vec (re-seq clip-re (or html ""))))

(defn replace-clips
  "Every clip in `html` replaced by the same `replacement` string.

   Pre : `html` is a string, or nil. `replacement` carries no `$1` group
         reference and no backslash — `str/replace` reads both.
   Post: nil iff `html` is nil; otherwise no clip element survives.

   The one way a caller outside this ns may swap a clip for something else, so
   the element's shape keeps one definition. `card-components` renders the card
   row's `[audio]` chip through it."
  [html replacement]
  (when html
    (str/replace html clip-re replacement)))

(defn media-id
  "The media id inside one clip element, as a string, or nil.
   Pre : `clip` is one element string, or nil.
   Post: the digits of its `/api/media/<id>` src, or nil when it carries none."
  [clip]
  (second (re-find media-src-re (or clip ""))))

(defn clip-src
  "The `src` of one clip element, or nil.
   Pre : `clip` is one element string, or nil.
   Post: the attribute's value, from either quoting style."
  [clip]
  (second (re-find #"(?is)\bsrc\s*=\s*[\"']([^\"']*)[\"']" (or clip ""))))

(defn sound-tag
  "Anki's `[sound:<filename>]` token.

   THE definition of that token. `anki-sync-helpers/score-sound-html` builds a
   score clip's token through this fn, so the two cannot drift.

   Pre : `filename` is the name the media file was stored under in Anki.
   Post: the token Anki's Replay button plays."
  [filename]
  (str "[sound:" filename "]"))

(defn stored->anki-html
  "Every clip in `html` replaced by its `[sound:<filename>]` token.

   Pre : `rewrite-media-srcs` has ALREADY run on `html`, so a clip's src is the
         filename its media was uploaded to Anki under. Running BEFORE it would
         need the filename map here, and every field builder would have to
         remember the order; after it, the filename is in the src and this fn
         takes no map at all.
   Post: no `<audio>` element survives, and every other tag is unchanged.
   Inv : a src still shaped like `/api/media/<id>` means that upload failed, and
         it falls back to `<id>.mp3` — the name the upload would have used, and
         the fallback score clips already take.
   Inv : a clip with no src is dropped rather than written as an empty token. A
         `[sound:]` with no filename is a broken reference in Anki.
   Inv : the loop setting does not survive. `[sound:]` cannot carry one, and it
         is the only form Anki's Replay button plays."
  [html]
  (if (str/blank? html)
    html
    (str/replace html clip-re
      (fn [clip]
        (let [src (clip-src clip)]
          (cond
            (str/blank? src) ""
            (re-find media-src-re src) (sound-tag (str (media-id clip) ".mp3"))
            :else (sound-tag src)))))))

(defn mask-clips
  "Replace every clip with `clip-mask-token`, in EITHER spelling.

   Pre : `html` is a string, or nil. It may be a local field (`<audio>`) or an
         Anki field (`[sound:…]`).
   Post: a string in which neither spelling survives, and everything else is
         unchanged.
   Inv : a local field and the Anki field push made from it mask to the same
         string, so a clip alone never reads as an Anki-side edit.

   For comparison only. Never store the result."
  [html]
  (-> (or html "")
    (str/replace clip-re clip-mask-token)
    (str/replace sound-token-re clip-mask-token)))

(defn restore-clip-tags
  "`anki-html` with its i-th `[sound:…]` token replaced by `local-html`'s i-th
   clip element.

   Pre : both are strings, or nil.
   Post: the text of `anki-html` carrying local clip elements, which this app
         can serve. A token beyond the local count keeps its Anki form, because
         there is no local element to take — the user added that sound in Anki,
         and only Anki can resolve it.
   Inv : with no clip on either side, returns `anki-html` unchanged.
   Inv : the returned count of clip-shaped things equals `anki-html`'s.

   Positional, for the reason `anki-fields/restore-img-tags` is: the filename
   Anki holds is the only trace of the media id, and matching on it would make
   this fn depend on a naming scheme a user can rename out from under it."
  [anki-html local-html]
  (let [locals (clip-tags local-html)]
    (if (empty? locals)
      (or anki-html "")
      (let [i (volatile! -1)]
        (str/replace (or anki-html "") sound-token-re
          (fn [[token & _]]
            (or (nth locals (vswap! i inc) nil) token)))))))
