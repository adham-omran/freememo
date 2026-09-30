(ns freememo.editor-actions
  "Editor mutation actions shared across Quill instances — cloze-deletion
   insertion, formula insert/edit, and the pasted/dropped/picked image re-host
   path that keeps base64 out of both editors (`install-attachment-upload!`).

   It also owns focus reader's page boundaries inside the editor: where they
   are (`page-at-scroll-top`, `!marker-offsets`), moving to one
   (`scroll-to-page!`, `restore-page-on-install!`), reporting the one the reader
   reached (`!scrolled-page`, `install-page-marker-tracking!`), and putting a
   deleted one back (`insert-page-break!`, `restorable-page`). The vocabulary
   itself — ids, merging, splitting — is `freememo.page-merge`, which this
   requires and which knows nothing about a DOM.

   Extracted from quill_field so both the QuillField lifecycle and the custom
   format-menu drive the same behavior without a namespace cycle (format-menu
   requires this ns; this ns requires neither). CLJS side effects; the CLJ
   branch is inert so the ns loads on both peers. `cloze-chords` is the one
   member that is neither — plain data, carried without a reader conditional so
   the two peers cannot disagree about it."
  (:require [clojure.string :as str]
            [freememo.client-errors :as ce]
            [freememo.math :as math]
            [freememo.page-merge :as page-merge]
            [freememo.util :as util]))

;; ---------------------------------------------------------------------------
;; Cloze deletion — number tracking + selection wrapping (CLJS).
;; ---------------------------------------------------------------------------

(defn cloze-max-n
  "Highest existing cloze index in `text` (scans for {{cN::); 0 if none."
  [text]
  #?(:cljs
     (let [matches (re-seq #"\{\{c(\d+)::" (or text ""))]
       (if (seq matches)
         (apply max (map #(js/parseInt (second %) 10) matches))
         0))
     :clj 0))

(defn- inline-format-reset
  "A Quill formats object clearing every inline format the editors can carry.

   Do NOT reach for `removeFormat` instead: Quill 2.0.3's `Editor.removeFormat`
   rebuilds the remainder of the line ending in a bare `.insert(\"\\n\")`, and the
   newline is where BLOCK formats live (header, list, code-block, align,
   indent) — so clozing a word inside a heading or a list item would silently
   un-heading or un-list that line. `formatText` with inline-only keys cannot
   reach the newline.

   Fresh object per call: Quill's `overload` treats the formats argument as its
   own to reshape, so a shared `#js` literal must not be handed to it.

   Post: every key is an inline format registered by Quill's default registry;
         no key is block-scoped."
  []
  #?(:cljs #js {:bold false :italic false :underline false :strike false
                :code false :color false :background false
                :size false :font false :link false :script false}
     :clj nil))

(defn insert-cloze!
  "Wrap the current Quill selection in an Anki cloze deletion {{cN::...}}.
     mode :inc → next cloze number (inc of the current max in the editor);
     mode :eq  → reuse the current max (min 1).
   Collapsed selection inserts an empty {{cN::}} with the cursor between
   :: and }}. No selection (editor never focused) → no-op. CLJS only.

   The selected content is NEVER read, deleted, or re-inserted — only the two
   marker strings are inserted, at the selection's boundaries. A prior version
   round-tripped the selection through `(.getText ed index length)` +
   `deleteText` + `insertText`, which silently destroyed everything Quill's
   text API cannot represent: bold/italic/code/link/colour inside the range,
   any <img> (getText drops non-string inserts outright), and the block format
   of every line after the first. Bolding a word and then clozing it lost the
   bold — the symptom that motivated this shape.

   The markers themselves are reset to plain via `inline-format-reset`:
   `insertText` at a boundary inherits the adjacent leaf's formats, and at the
   two ends of a bold run those leaves differ — the opener lands in the plain
   text before it, the closer inside the <strong> — emitting
   `{{c1::<strong>www}}</strong>`, a closer trapped in the tag. Plain markers
   keep the braces out of the formatting entirely.

   Pre  : `ed` is a Quill instance or nil; `mode` ∈ {:inc :eq}.
   Post : the selected range is byte-identical in the Delta apart from being
          surrounded by two unformatted marker runs; the cursor sits after }}
          (selection) or between :: and }} (collapsed); the user-source edits
          fire text-change → the field's :on-change → !cloze sync.
   Inv  : end-boundary insert precedes start-boundary insert, so `index` stays
          valid for the second insert."
  [ed mode]
  #?(:cljs
     (when ed
       (let [^js ed ed
             sel (.getSelection ed)]
         (js/console.log "[cloze] insert-cloze!" (name mode) "selection:" sel)
         (when sel
           (let [n      (case mode
                          :inc (inc (cloze-max-n (.getText ed)))
                          :eq  (max 1 (cloze-max-n (.getText ed))))
                 index  (.-index sel)
                 length (.-length sel)
                 prefix (str "{{c" n "::")
                 suffix "}}"
                 insert-plain! (fn [at text]
                                 (.insertText ed at text "user")
                                 (.formatText ed at (count text)
                                   (inline-format-reset) "user"))]
             (if (pos? length)
               (do (insert-plain! (+ index length) suffix)   ; end first — see Inv
                   (insert-plain! index prefix)
                   (.setSelection ed (+ index (count prefix) length (count suffix)) 0 "user"))
               (do (insert-plain! index (str prefix suffix))
                   (.setSelection ed (+ index (count prefix)) 0 "user")))))))
     :clj nil))

(def cloze-chords
  "The two cloze chords as `freememo.commands` bind specs, keyed by
   `insert-cloze!` mode — the DISPLAY side of what `install-cloze-bindings!`
   registers. Both halves live here so the tooltip in `freememo.format-menu`
   cannot drift from the binding that fires.

   `meta` reads as Cmd on macOS and Ctrl elsewhere, which is what BOTH sides
   already do independently: `commands/effective-bind` rewrites the string, and
   Quill's own `shortKey` resolves to `metaKey` on macOS and `ctrlKey` elsewhere
   (`/Mac/i.test(navigator.platform)`, quill 2.0.3). No platform branch is
   written in this codebase for these chords.

   No reader conditional: this is shared vocabulary read from client scope, so
   both peers must see plain literals (CLAUDE.md, `transcribe_language.cljc`)."
  {:inc "meta+shift+c"
   :eq  "meta+shift+alt+c"})

(def ^:private cloze-chord-key-code
  "`C`'s keyCode, the form the cloze bindings register under.

   Quill's `Keyboard.match` accepts `binding.key === evt.key || binding.key ===
   evt.which`, and its `listen` looks a stroke up in BOTH `bindings[evt.key]` and
   `bindings[evt.which]` (quill 2.0.3). A string key would work for the plain
   chord and silently fail for the Option one: macOS rewrites `evt.key` when
   Option is held — ⌥C yields \"ç\" — so only the numeric form matches through
   `evt.which`, which is layout- and modifier-independent.

   Quill's own history module shows the string-key cost: it registers redo as
   `{key: [\"z\", \"Z\"]}` because Shift changes `evt.key`'s case."
  67)

(def ^:private clear-format-chord-key-code
  "`\\`'s keyCode. The numeric form keeps the binding stable across keyboard
   layouts, as with cloze-chord-key-code."
  220)

(defn install-clear-format-binding!
  "Register mod+\\ on one Quill editor.

   Pre: `ed` is a constructed Quill instance. Post: a non-empty selected range
   loses its Quill formatting exactly once. A collapsed selection is a no-op.
   The binding belongs to Quill rather than the global command registry because
   formatting applies only to the focused editor."
  [ed]
  #?(:cljs
     (let [^js ed ed
           ^js keyboard (.getModule ed "keyboard")]
       (.addBinding keyboard
         #js {:key clear-format-chord-key-code :shortKey true}
         (fn [range _context _binding]
           (when (and range (pos? (.-length range)))
             (.removeFormat ed (.-index range) (.-length range) "user"))
           nil)))
     :clj nil))

(defn install-cloze-bindings!
  "Register the two cloze-insert chords on `ed`'s keyboard module — the keyboard
   twins of the format-menu's {+} / {=} buttons.

     mod+shift+C      → insert-cloze! :inc (next cloze number)
     mod+shift+alt+C  → insert-cloze! :eq  (reuse the current max)

   Editor-local by design: the action is meaningless without a focused cloze
   field, so these chords carry no `freememo.commands` entry, no palette row and
   no `:bind`. That is this feature's answer to CLAUDE.md's \"a new feature owes
   the command palette a decision\".

   Pre  : `ed` is a Quill instance whose keyboard module is constructed; the
          caller has decided this editor holds a cloze field (violation = caller
          bug — see `quill_field/init-quill-field!`'s `cloze?` gate).
   Post : with focus in `ed`, each chord runs `insert-cloze!` exactly once and
          the keydown is preventDefault'ed. The second half is not cosmetic: it
          is what stops Chrome opening inspect-element mode, whose accelerator
          is this same chord (`IDC_DEV_TOOLS_INSPECT`, reachable because
          `BrowserCommandController::IsReservedCommandOrKey` excludes it, so the
          renderer gets the event first). Returning nil rather than `true` is
          what asks Quill for that preventDefault — its `listen` prevents unless
          a matched handler returns literally `true`.
   Inv  : the two bindings are mutually exclusive. `match` requires an omitted
          modifier to be ABSENT (`!!binding[k] !== evt[k]`, with `null` the only
          don't-care), so `:altKey false` on the :inc binding is what keeps
          mod+shift+alt+C from firing it too — stated rather than left implicit.
   Note : no teardown. The bindings live on the module instance, which dies with
          the editor when `destroy-quill-field!` empties its container. This is
          `a11y/install-editor-escape!`'s argument, one layer in."
  [ed]
  #?(:cljs
     (let [^js ed ed
           ^js keyboard (.getModule ed "keyboard")
           bind! (fn [alt? mode]
                   (.addBinding keyboard
                     #js {:key cloze-chord-key-code
                          :shortKey true :shiftKey true :altKey alt?}
                     (fn [_range _context _binding]
                       (insert-cloze! ed mode)
                       nil)))]
       (bind! false :inc)
       (bind! true :eq))
     :clj nil))

;; ---------------------------------------------------------------------------
;; Formula — document mutations (CLJS). The UI is freememo.formula-ui.
;; ---------------------------------------------------------------------------
;; There is no insert dialog. Typing `\(…\)` is the only way to create a formula,
;; and `install-typed-formula-conversion!` turns it into a rendered embed the
;; moment the closing delimiter completes a region. Editing goes through the
;; anchored popover in freememo.formula-ui, which calls `replace-formula!` here.
;;
;; Quill's own formula tooltip is not an option: both editors use the bubble
;; theme and `index.css` hides `.ql-bubble .ql-tooltip` outright, because the
;; custom format-menu is the formatting UI.

(defn replace-formula!
  "Replace the formula embed rendered at `el` with `tex`, or delete it when `tex`
   is blank. The one mutation behind every popover exit.

   Pre  : `ed` is a Quill instance; `el` is a `span.ql-formula` still connected to
          `ed`'s document; KaTeX is on window (`Formula.create` throws without it).
   Post : with a non-blank `tex`, one embed carrying it sits where `el` was and the
          caret is after it. With a blank `tex`, nothing sits there and the caret
          is where the formula was. Source \"user\", so text-change fires and the
          caller's on-change persists it. No-op if `el` is no longer a live blot.
   Inv  : the index is resolved from `el` at call time, not cached — the document
          may have changed since the popover opened."
  [ed el tex]
  #?(:cljs
     (let [^js ed ed
           ^js Quill (.-Quill js/window)
           blot (.find Quill el)]
       (when blot
         (let [index (.getIndex ed blot)]
           (.deleteText ed index 1 "user")
           (if (str/blank? tex)
             (.setSelection ed index 0 "user")
             (do (.insertEmbed ed index "formula" tex "user")
                 (.setSelection ed (inc index) 0 "user"))))))
     :clj nil))

(defn formula-caret-index
  "The index just after the formula embed rendered at `el`, or nil when `el` is no
   longer a live blot. Used to restore the caret on a cancelled edit.

   Pre  : `ed` is a Quill instance; `el` is a `span.ql-formula`.
   Post : nil, or an index in [1, document length]."
  [ed el]
  #?(:cljs
     (let [^js ed ed
           ^js Quill (.-Quill js/window)
           blot (.find Quill el)]
       (when blot (inc (.getIndex ed blot))))
     :clj nil))

(defn- code-region?
  "True iff the range [index, index+length) carries a code format, so a `\\(…\\)`
   there is source rather than math.

   Quill's `getFormat` over a RANGE reports only the formats common to all of it,
   which is what we want: a region wholly inside a code block or an inline `<code>`
   run reports the format and is skipped. A region straddling a code boundary
   reports neither and converts — pathological, and no cheaper reading of
   `getFormat` distinguishes it.

   Mirrors the `pre, code, .ql-code-block-container` exclusion that
   `freememo.math/stored->editor-html` applies on the load path; Clojure's `\\(`
   character literal is the motivating case."
  [ed index length]
  #?(:cljs
     (let [^js ed ed
           ^js fmt (.getFormat ed index length)]
       ;; hasOwnProperty, not `(.-code-block fmt)`: CLJS munges `-` to `_` in
       ;; property access, so that form would read `fmt.code_block` and the
       ;; exclusion would silently never fire. Quill's key is literally
       ;; \"code-block\", and getFormat omits keys that are not set.
       (or (.hasOwnProperty fmt "code-block")
         (.hasOwnProperty fmt "code")))
     :clj nil))

(defn- typed-formula-regions
  "Every `\\(…\\)` region in `ed`'s content as {:index :length :tex}, in document
   order, excluding regions inside code.

   Walks the Delta rather than `getText`: embeds already in the document occupy
   one index each but are not text, and `getText`'s placeholder characters make
   index arithmetic over them unreliable."
  [ed]
  #?(:cljs
     (let [^js ed ed
           ops (.-ops (.getContents ed))]
       (loop [i 0, idx 0, out []]
         (if (>= i (.-length ops))
           out
           (let [^js op (aget ops i)
                 ins (.-insert op)]
             (if (string? ins)
               (let [re (js/RegExp. (.-source math/tex-region-re) "g")]
                 (recur (inc i) (+ idx (count ins))
                   (loop [acc out]
                     (if-let [m (.exec re ins)]
                       (let [start (+ idx (.-index m))
                             len (count (aget m 0))]
                         (recur (if (code-region? ed start len)
                                  acc
                                  (conj acc {:index start :length len :tex (aget m 1)}))))
                       acc))))
               (recur (inc i) (inc idx) out))))))
     :clj nil))

(defn convert-typed-formulas!
  "Turn every typed `\\(…\\)` in `ed` into a rendered formula embed. Idempotent.

   Applied in reverse document order so each replacement leaves the indices of the
   ones still to come untouched. Source \"user\", so the change enters undo history
   and Ctrl+Z can reach it — the escape hatch when a `\\(` typed as prose (writing
   ABOUT LaTeX) converts unintentionally.

   Pre  : `ed` is a Quill instance or nil; KaTeX is on window.
   Post : `ed`'s text contains no `\\(…\\)` region outside code; each became one
          embed, with the caret left after the last one converted.
   Inv  : self-terminating — after conversion no region remains, so the
          text-change this fires finds nothing on its next pass."
  [ed]
  #?(:cljs
     (when ed
       (let [^js ed ed]
         (doseq [{:keys [index length tex]} (reverse (typed-formula-regions ed))]
           (.deleteText ed index length "user")
           (.insertEmbed ed index "formula" tex "user")
           (.setSelection ed (inc index) 0 "user"))))
     :clj nil))

(defn install-typed-formula-conversion!
  "Convert typed `\\(…\\)` to a formula embed as soon as the closing delimiter
   completes a region. Returns a teardown fn.

   Runs in a microtask rather than inline: the conversion modifies the document
   from within a handler for that document's own text-change, which Quill does not
   invite. A microtask defers past the handler without costing a frame, so the
   formula still appears in the same frame as the keystroke.

   Pre  : `ed` is a Quill instance; KaTeX is on window.
   Post : a `\\(…\\)` typed outside code renders immediately; one inside a code
          block stays text. The returned fn detaches the listener AND marks the
          install dead, so a microtask already queued cannot write into a
          torn-down editor.
   Inv  : only `\"user\"` changes are considered. Programmatic writes (`set-content!`,
          the load path) have already been through
          `freememo.math/stored->editor-html`."
  [ed]
  #?(:cljs
     (let [^js ed ed
           !live? (atom true)
           on-text-change (fn [_delta _old source]
                            (when (= source "user")
                              (.then (js/Promise.resolve)
                                (fn [] (when @!live? (convert-typed-formulas! ed))))))]
       (.on ed "text-change" on-text-change)
       (fn teardown []
         (reset! !live? false)
         (.off ed "text-change" on-text-change)))
     :clj nil))

;; ---------------------------------------------------------------------------
;; Image upload — data-URI blob → /api/upload-media, then insert / rewrite src.
;; ---------------------------------------------------------------------------

(defn- data-uri->blob
  "Decode a base64 data: URI into a Blob typed by its declared MIME type.
   Pre : data-uri starts with \"data:\" and carries a base64 payload.
   Post: a Blob whose .type is the URI's MIME type.
   Inv : synchronous; performs no I/O."
  [data-uri]
  #?(:cljs
     (let [parts (str/split data-uri #",")
           mime (-> (first parts)
                  (str/replace "data:" "")
                  (str/replace ";base64" ""))
           bin-str (js/atob (second parts))
           n (count bin-str)
           buf (js/Uint8Array. n)]
       (dotimes [i n]
         (aset buf i (.charCodeAt bin-str i)))
       (js/Blob. (clj->js [buf]) (clj->js {:type mime})))
     :clj nil))

(defn upload-media-blob!
  "POST a Blob or File to /api/upload-media.

   Pre  : blob carries image bytes and its .type is the MIME type (used for the
          upload filename's extension; the server reads the Blob's own type).
   Post : resolves to \"/api/media/<id>\"; rejects with a js/Error whose message
          is safe to show a user — the server's own :error text when it sent one
          (e.g. the per-file size cap), else the status code.
   Inv  : mutates no DOM."
  [blob]
  #?(:cljs
     (let [^js blob blob
           form (js/FormData.)]
       (.append form "file" blob (str "image." (util/mime->ext (.-type blob))))
       (-> (js/fetch "/api/upload-media"
             (clj->js {:method "POST"
                       :credentials "same-origin"
                       :body form}))
         (.then (fn [^js r]
                  (-> (.json r)
                    (.catch (fn [_] #js {}))
                    (.then (fn [^js j]
                             (if (and (.-ok r) (.-id j))
                               (str "/api/media/" (.-id j))
                               (throw (js/Error. (or (.-error j)
                                                   (str "Upload failed ("
                                                     (.-status r) ")"))))))))))))
     :clj nil))

(defn upload-pasted-image!
  "Upload a data-URI blob to /api/upload-media.
   Calls on-uploaded with the /api/media/<id> URL on success, nil on error.
   CLJS only."
  [data-uri on-uploaded]
  #?(:cljs
     (-> (upload-media-blob! (data-uri->blob data-uri))
       (.then (fn [url] (on-uploaded url)))
       (.catch (fn [err]
                 (js/console.warn "[editor-actions] image upload failed:" (str err))
                 (ce/report! :editor/image-upload err)
                 (on-uploaded nil))))
     :clj nil))

;; ---------------------------------------------------------------------------
;; Paste / drop image re-host — replaces Quill's own Uploader module behaviour.
;; ---------------------------------------------------------------------------

(defn- read-data-url!
  "Promise of `blob` read as a base64 data: URL.
   Pre : blob is a Blob or File.
   Post: resolves to a \"data:<mime>;base64,…\" string, or rejects.
   Inv : mutates no DOM."
  [blob]
  #?(:cljs
     (js/Promise.
       (fn [resolve reject]
         (let [^js reader (js/FileReader.)]
           (set! (.-onload reader) (fn [_] (resolve (.-result reader))))
           (set! (.-onerror reader) (fn [_] (reject (js/Error. "Image could not be read"))))
           (.readAsDataURL reader blob))))
     :clj nil))

(defn- rehost-embed-at!
  "Upload `file` and replace the image embed at `index` with its /api/media URL.

   Holds the BLOT rather than the index: the user can type above the image while
   the upload is in flight, so the position is re-read with getIndex at swap
   time. One updateContents (delete+insert) keeps it to a single commit, so the
   host editor's text-change → on-change fires once.

   `index` is the embed's OWN position, not the position after it. Parchment's
   LinkedList/find matches the first child with `index < length` and getLeaf
   passes inclusive=false, so getLeaf(index + 1) on a length-1 embed skips past
   it and returns the FOLLOWING leaf.

   Pre  : ed is a live Quill instance; an image embed occupies `index`.
   Post : resolves when the embed carries its media URL. On failure the data URI
          is left in place — image-rehost/rehost-data-uris! stores it at save
          time — and one entry is reported.
   Inv  : the document's length is unchanged by the swap."
  [ed index file]
  #?(:cljs
     (let [^js ed ed
           blot (aget (.getLeaf ed index) 0)]
       (-> (upload-media-blob! file)
         (.then (fn [url]
                  (let [Quill (.-Quill js/window)
                        Delta (.import Quill "delta")
                        at (.getIndex ed blot)]
                    (.updateContents ed
                      (-> (new Delta)
                        (.retain at)
                        (.delete 1)
                        (.insert (clj->js {:image url})))
                      "user"))))
         (.catch (fn [err]
                   (js/console.warn "[editor-actions] image re-host failed:" (str err))
                   (ce/report! :editor/image-rehost err
                     (str "Image could not be stored: " (.-message err)))))))
     :clj nil))

(defn- insert-and-rehost-image!
  "Insert `file` at `index` as its own data: URI, then start its re-host.

   The data URI is deliberately the placeholder. `URL.createObjectURL` would be
   cheaper, but a blob: URL cannot be resolved server-side, so a Save that raced
   the upload would persist a dead src. A data: URI in the same race is stored
   by image-rehost/rehost-data-uris! instead.

   Pre  : ed is a live Quill instance; index is a 0-based position; file's type
          is in util/storable-image-mime-types.
   Post : resolves to the index after the inserted embed, as soon as the embed
          is in the document — the upload and swap continue in the background.
          On a read failure nothing is inserted and `index` is returned.
   Inv  : at most one embed is inserted."
  [ed index file]
  #?(:cljs
     (let [^js ed ed]
       (-> (read-data-url! file)
         (.then (fn [data-url]
                  (.insertEmbed ed index "image" data-url "user")
                  (.setSelection ed (inc index) 0 "user")
                  (rehost-embed-at! ed index file)
                  (inc index)))
         (.catch (fn [err]
                   (ce/report! :editor/image-read err
                     (str "Image could not be read: " (.-message err)))
                   index))))
     :clj nil))

(defn- insert-audio-clip!
  "Upload `file`, THEN insert one audio-clip embed at `index`.

   A clip gets no placeholder, which is the one way it differs from an image.
   An image's placeholder is a `data:` URI, and a Save that races the upload is
   caught by `image-rehost/rehost-data-uris!` — a fn that matches `img[src]`
   alone. A `data:` clip would be caught by nothing: it would sit in
   `topics.content` at base64 size forever, and `clean-html` allow-lists no
   `data:` on an audio src, so the first card save would drop it silently.

   Pre  : ed is a live Quill instance; index is a 0-based position; file's type
          is in util/storable-audio-mime-types.
   Post : resolves to the index after the inserted embed. On a failed upload
          nothing is inserted, `index` is returned, and one message is reported.
   Inv  : no `data:` URI ever reaches the document."
  [ed index file]
  #?(:cljs
     (let [^js ed ed]
       (-> (upload-media-blob! file)
         (.then (fn [url]
                  (.insertEmbed ed index "audio-clip" #js {:src url :loop false} "user")
                  (.setSelection ed (inc index) 0 "user")
                  (inc index)))
         (.catch (fn [err]
                   (ce/report! :editor/audio-upload err
                     (str "Audio could not be stored: " (.-message err)))
                   index))))
     :clj nil))

(defn insert-audio!
  "Open a file picker, upload the chosen MP3, and insert it as a clip.

   The audio twin of `insert-image!`, and it shares that fn's contract about a
   cached index. `accept` states MP3 alone because `api/upload-media-handler`
   accepts exactly `audio/mpeg`; a picker offering more would earn a 400.

   Pre  : `editor` is a Quill instance; `index` is a cached 0-based position or
          nil (nil → editor end).
   Post : on a successful upload one clip embed is inserted at the index
          (clamped to the live length) with source \"user\", which fires
          text-change so the field's on-change persists it. A cancelled pick or
          a failed upload inserts nothing.
   Inv  : only the /api/media URL is ever stored. Async: the picker and upload
          resolve after this returns."
  [editor index]
  #?(:cljs
     (let [^js ed editor
           ^js input (js/document.createElement "input")]
       (set! (.-type input) "file")
       (set! (.-accept input) "audio/mpeg,.mp3")
       (.addEventListener input "change"
         (fn [_]
           (when-let [file (aget (.-files input) 0)]
             (insert-audio-clip! ed (min (or index (.getLength ed)) (.getLength ed)) file))))
       (.click input))
     :clj nil))

(defn install-attachment-upload!
  "Replace Quill's Uploader so pasted and dropped images become /api/media URLs
   instead of inline base64.

   Overrides `uploader.upload`, not `uploader.handler`, for two reasons. Quill
   2.0.3's `upload` silently discards every file whose type falls outside
   `Uploader.DEFAULTS.mimetypes` (png and jpeg only) BEFORE the handler runs, so
   a pasted WebP produced no image and no message. And `Clipboard.onCapturePaste`
   routes any clipboard payload carrying files straight to `upload` — which is
   why the clipboard matcher this replaces could never fire on a pasted image.

   Pre  : ed is a live Quill instance whose uploader module is constructed.
   Post : every later paste or drop of a util/storable-image-mime-types file
          inserts one image embed, in clipboard order, which becomes
          /api/media/<id> once its upload resolves; every other file reports one
          user-visible message and mutates the document not at all.
   Inv  : no data: URI survives a successful upload."
  [ed]
  #?(:cljs
     ;; ^js on the rebinding, not the parameter — the CLJ compiler reads the
     ;; parameter list of this .cljc defn (same rule as init-quill-field!).
     (let [^js ed ed]
       (when-let [^js uploader (.-uploader ed)]
         (set! (.-upload uploader)
           (fn [^js range files]
             (let [image? (fn [^js f] (util/storable-image-mime-types (.-type f)))
                   clip? (fn [^js f] (util/storable-audio-mime-types (.-type f)))
                   attachable? (fn [^js f] (or (image? f) (clip? f)))
                   fs (vec (array-seq files))]
               (doseq [^js f (remove attachable? fs)]
                 (ce/report! :editor/attachment-format
                   (str "Unsupported attachment format: " (.-type f))
                   (str "Cannot paste "
                     (if (str/blank? (.-type f)) "this file" (.-type f))
                     " — supported formats are PNG, JPEG, GIF, WebP and AVIF"
                     " images, and MP3 audio.")))
               ;; Chained, not concurrent: each insert awaits the previous one so
               ;; multiple files land in clipboard order. Only the INSERT is
               ;; serialised — the uploads run in parallel behind it.
               (reduce (fn [p file]
                         (.then p (fn [i] (if (image? file)
                                            (insert-and-rehost-image! ed i file)
                                            (insert-audio-clip! ed i file)))))
                 (js/Promise.resolve (.-index range))
                 (filter attachable? fs))
               nil)))))
     :clj nil))

(defn install-clip-menu!
  "Right-click an audio clip to toggle whether it loops. Returns a teardown fn.

   Lives here rather than in `freememo.editor-image-menu` for two reasons. That
   menu takes a container element plus app callbacks and is installed on the
   document editor alone, through `editor-pane`. A clip needs the QUILL
   INSTANCE, because the toggle is a document edit, and card fields hold clips
   too — every per-editor installer in this ns already has that shape.

   The toggle REPLACES the embed instead of setting the attribute on its node.
   A DOM write Quill did not make is invisible to the Delta: the document would
   look changed and save nothing, and the next load would restore the old
   setting. One `updateContents` delete+insert is a single commit, so
   text-change fires once and the field's on-change persists it — the shape
   `rehost-embed-at!` above uses to swap an image's src.

   Pre  : `ed` is a live Quill instance.
   Post : a contextmenu on an `<audio>` inside this editor opens one menu, and
          the toggle rewrites that clip with `loop` flipped, source \"user\".
          A contextmenu anywhere else is left to the browser.
   Inv  : at most one clip menu exists at a time, and teardown leaves behind no
          menu and no listener."
  [ed]
  #?(:cljs
     (let [^js ed ed
           !menu (atom nil)
           !dismiss (atom nil)
           close! (fn []
                    (when-let [^js m @!menu] (.remove m))
                    (reset! !menu nil)
                    (when-let [f @!dismiss] (f))
                    (reset! !dismiss nil))
           toggle-loop!
           (fn [^js node]
             (when-let [^js blot (.find (.-Quill js/window) node)]
               (let [Quill (.-Quill js/window)
                     Delta (.import Quill "delta")
                     at (.getIndex ed blot)]
                 (.updateContents ed
                   (-> (new Delta)
                     (.retain at)
                     (.delete 1)
                     (.insert (clj->js {:audio-clip {:src (.getAttribute node "src")
                                                     :loop (not (.hasAttribute node "loop"))}})))
                   "user"))))
           button (fn [label on-click]
                    (let [b (js/document.createElement "button")]
                      (set! (.-textContent b) label)
                      (.addEventListener b "click"
                        (fn [_] (close!) (when on-click (on-click))))
                      b))
           handler
           (fn [e]
             (let [^js target (.-target e)]
               (when (and target
                       (= "AUDIO" (.-tagName target))
                       (.closest target ".ql-editor"))
                 (.preventDefault e)
                 (.stopPropagation e)
                 (close!)
                 (let [menu (js/document.createElement "div")
                       looping? (.hasAttribute target "loop")]
                   (.setAttribute menu "class" "clip-context-menu")
                   (set! (.-style.left menu) (str (.-pageX e) "px"))
                   (set! (.-style.top menu) (str (.-pageY e) "px"))
                   (.appendChild menu
                     (button (if looping? "Stop looping" "Loop this clip")
                       #(toggle-loop! target)))
                   (.appendChild menu (button "Cancel" nil))
                   (.appendChild js/document.body menu)
                   (reset! !menu menu)
                   ;; Dismissal, as the image menu does it: Escape, or a click
                   ;; outside. The outside listener is registered a tick later,
                   ;; so the contextmenu that opened this does not close it.
                   (let [on-key (fn [ev] (when (= "Escape" (.-key ev))
                                           (.stopPropagation ev)
                                           (close!)))
                         on-doc-click (fn [ev] (when-not (.contains menu (.-target ev))
                                                 (close!)))
                         timer (js/setTimeout
                                 #(.addEventListener js/document "click" on-doc-click)
                                 0)]
                     (.addEventListener js/document "keydown" on-key)
                     (reset! !dismiss
                       (fn []
                         (js/clearTimeout timer)
                         (.removeEventListener js/document "keydown" on-key)
                         (.removeEventListener js/document "click" on-doc-click))))))))
           container (.-container ed)]
       (.addEventListener container "contextmenu" handler)
       (fn []
         (.removeEventListener container "contextmenu" handler)
         (close!)))
     :clj nil))

(defn install-clip-drag!
  "Drag an audio clip by its grip to move it. Returns a teardown fn.

   A grip shows at the clip's right edge while the pointer is on the clip. A
   drop in the text moves the clip to the line boundary nearest the drop point:
   before the line under the pointer, or after it from the line's second half.
   A block embed inserted mid-line would split that line in two.

   Three choices, each forced:
   - The grip is the drag source, not the clip. Every point of the clip is the
     browser's media controls, where a press plays, seeks or sets the volume.
   - The grip and the drop line live in `quill.container`, outside the root, so
     `quill-field/editor-html` never reads them into the stored form.
   - The drop listener runs in the CAPTURE phase on the root. Quill's uploader
     calls preventDefault on every drop there and handles files only, so no
     drop that reaches it can move anything.

   The move is ONE `updateContents`, the delete+insert shape
   `install-clip-menu!` uses, so text-change fires once and autosave persists
   it. The drop index comes from `caretRangeFromPoint` through the two
   `quill.selection` methods Quill's own file drop uses. They are not public
   API; Quill is pinned at 2.0.3.

   Pre  : `ed` is a live Quill instance with the audio-clip blot registered.
   Post : a drag the grip started, dropped inside this editor's root, moves
          that clip with its src and loop, source \"user\". Every other drag
          and drop is left to Quill and the browser.
   Inv  : teardown removes the grip, the drop line and every listener."
  [ed]
  #?(:cljs
     (let [^js ed ed
           ^js root (.-root ed)
           ^js container (.-container ed)
           Quill (.-Quill js/window)
           Delta (.import Quill "delta")
           ^js grip (js/document.createElement "div")
           ^js drop-line (js/document.createElement "div")
           !hovered (atom nil)   ; the <audio> the grip sits beside
           !dragged (atom nil)   ; the <audio> a drag is moving
           px (fn [n] (str n "px"))
           ;; A viewport y as a `top` inside the container, which scrolls in
           ;; some editors — its absolutely placed children scroll with it.
           container-y (fn [y] (+ (- y (.-top (.getBoundingClientRect container)))
                                 (.-scrollTop container)))
           hide-grip! (fn []
                        (set! (.. grip -style -display) "none")
                        (reset! !hovered nil))
           hide-drop-line! #(set! (.. drop-line -style -display) "none")
           place-grip!
           (fn [^js clip]
             (let [^js r (.getBoundingClientRect clip)]
               (reset! !hovered clip)
               (set! (.. grip -style -display) "block")
               (set! (.. grip -style -left)
                 (px (+ (- (.-right r) (.-left (.getBoundingClientRect container)))
                       (.-scrollLeft container))))
               (set! (.. grip -style -top)
                 (px (container-y (+ (.-top r)
                                    (/ (- (.-height r) (.-offsetHeight grip)) 2)))))))
           ;; The document index under the pointer, as Quill's uploader finds
           ;; it for a dropped file.
           point-index
           (fn [^js e]
             (let [x (.-clientX e)
                   y (.-clientY e)
                   ^js native
                   (cond
                     (.-caretRangeFromPoint js/document)
                     (.caretRangeFromPoint js/document x y)
                     ;; Firefox has only the position form.
                     (.-caretPositionFromPoint js/document)
                     (when-let [^js p (.caretPositionFromPoint js/document x y)]
                       (doto (.createRange js/document)
                         (.setStart (.-offsetNode p) (.-offset p))
                         (.setEnd (.-offsetNode p) (.-offset p)))))
                   ^js sel (.-selection ed)
                   ^js normalized (when native (.normalizeNative sel native))]
               (when normalized (.-index ^js (.normalizedToRange sel normalized)))))
           nearest-boundary
           (fn [i]
             (let [[^js line offset] (.getLine ed i)]
               (if line
                 (let [start (.getIndex ed line)
                       len (.length line)]
                   (if (< offset (/ len 2)) start (+ start len)))
                 i)))
           show-drop-line!
           (fn [boundary]
             (let [^js r (.getBoundingClientRect root)
                   ^js cs (js/getComputedStyle root)
                   pad-l (js/parseFloat (.-paddingLeft cs))
                   pad-r (js/parseFloat (.-paddingRight cs))
                   [^js line offset] (.getLine ed boundary)
                   ;; The top of the line a clip will precede, or the bottom
                   ;; of the last block when it will close the document.
                   y (if (and line (zero? offset))
                       (.-top (.getBoundingClientRect (.-domNode line)))
                       (.-bottom (.getBoundingClientRect (.-lastElementChild root))))]
               (set! (.. drop-line -style -display) "block")
               (set! (.. drop-line -style -left)
                 (px (+ (- (+ (.-left r) pad-l) (.-left (.getBoundingClientRect container)))
                       (.-scrollLeft container))))
               (set! (.. drop-line -style -width) (px (- (.-width r) pad-l pad-r)))
               (set! (.. drop-line -style -top) (px (container-y y)))))
           move!
           (fn [^js clip to]
             (when-let [^js blot (.find Quill clip)]
               (let [from (.getIndex ed blot)
                     clip-op (clj->js {:audio-clip {:src (.getAttribute clip "src")
                                                    :loop (.hasAttribute clip "loop")}})]
                 ;; `to` and `to` - 1 both leave the clip where it is.
                 (when-not (<= from to (inc from))
                   (.updateContents ed
                     (if (< to from)
                       (-> (new Delta) (.retain to) (.insert clip-op)
                         (.retain (- from to)) (.delete 1))
                       (-> (new Delta) (.retain from) (.delete 1)
                         (.retain (- to from 1)) (.insert clip-op)))
                     "user")))))
           on-mousemove
           (fn [^js e]
             (let [^js t (.-target e)]
               (cond
                 (= t grip) nil
                 (and (= "AUDIO" (.-tagName t)) (.contains root t)) (place-grip! t)
                 :else (hide-grip!))))
           on-dragstart
           (fn [^js e]
             (when-let [^js clip @!hovered]
               (let [^js dt (.-dataTransfer e)]
                 (reset! !dragged clip)
                 ;; Firefox starts no drag without data.
                 (.setData dt "application/x-freememo-clip" "")
                 (set! (.-effectAllowed dt) "move")
                 (.setDragImage dt clip (.-offsetWidth clip) (/ (.-offsetHeight clip) 2)))))
           on-dragend (fn [_]
                        (reset! !dragged nil)
                        (hide-drop-line!)
                        (hide-grip!))
           on-dragover
           (fn [^js e]
             (when @!dragged
               (.preventDefault e)
               (set! (.. e -dataTransfer -dropEffect) "move")
               (when-let [i (point-index e)]
                 (show-drop-line! (nearest-boundary i)))))
           on-dragleave
           (fn [^js e]
             (when-not (.contains root (.-relatedTarget e))
               (hide-drop-line!)))
           on-drop
           (fn [^js e]
             (when-let [^js clip @!dragged]
               (.preventDefault e)
               (.stopPropagation e)
               (reset! !dragged nil)
               (hide-drop-line!)
               (when-let [i (point-index e)]
                 (move! clip (nearest-boundary i)))))
           ;; Scrolling moves the clip out from under the grip, and an edit
           ;; can remove the clip the grip belongs to.
           on-scroll (fn [_] (hide-grip!))
           on-mouseleave (fn [_] (hide-grip!))
           on-text-change (fn [_ _ _] (hide-grip!))]
       (.setAttribute grip "class" "clip-drag-handle")
       (.setAttribute grip "aria-hidden" "true")
       (set! (.-draggable grip) true)
       (.setAttribute drop-line "class" "clip-drop-line")
       (.appendChild container grip)
       (.appendChild container drop-line)
       (.addEventListener container "mousemove" on-mousemove)
       (.addEventListener container "mouseleave" on-mouseleave)
       (.addEventListener container "scroll" on-scroll)
       (.addEventListener root "scroll" on-scroll)
       (.addEventListener grip "dragstart" on-dragstart)
       (.addEventListener grip "dragend" on-dragend)
       (.addEventListener root "dragover" on-dragover true)
       (.addEventListener root "dragleave" on-dragleave true)
       (.addEventListener root "drop" on-drop true)
       (.on ed "text-change" on-text-change)
       (fn []
         (.removeEventListener container "mousemove" on-mousemove)
         (.removeEventListener container "mouseleave" on-mouseleave)
         (.removeEventListener container "scroll" on-scroll)
         (.removeEventListener root "scroll" on-scroll)
         (.removeEventListener root "dragover" on-dragover true)
         (.removeEventListener root "dragleave" on-dragleave true)
         (.removeEventListener root "drop" on-drop true)
         (.off ed "text-change" on-text-change)
         (.remove grip)
         (.remove drop-line)))
     :clj nil))

(defn insert-image!
  "Open a file picker, upload the chosen image, and insert it into `editor`
   as an <img> pointing at the returned /api/media URL.

   Pre  : `editor` is a Quill instance; `index` is a cached 0-based position
          or nil (nil → editor end).
   Post : on a successful upload, an image embed is inserted at the index
          (clamped to the live length) with source \"user\" — this fires
          text-change so the field's on-change persists it; a cancelled pick
          or failed upload inserts nothing.
   Inv  : no data-URI ever reaches the persisted document — only the
          /api/media URL. Async: the picker/upload resolve after this returns."
  [editor index]
  #?(:cljs
     (let [^js ed editor
           ^js input (js/document.createElement "input")]
       (set! (.-type input) "file")
       (set! (.-accept input) "image/*")
       (.addEventListener input "change"
         (fn [_]
           (when-let [file (aget (.-files input) 0)]
             (let [^js reader (js/FileReader.)]
               (.addEventListener reader "load"
                 (fn [_]
                   (upload-pasted-image! (.-result reader)
                     (fn [url]
                       (when url
                         (let [at (min (or index (.getLength ed)) (.getLength ed))]
                           (.insertEmbed ed at "image" url "user")
                           (.setSelection ed (inc at) 0 "user")))))))
               (.readAsDataURL reader file)))))
       (.click input))
     :clj nil))

;; ---------------------------------------------------------------------------
;; Page-boundary restore (plans/merged-page-reading.md §4)
;; ---------------------------------------------------------------------------

;; Whether the reader can restore a page boundary here, and the page numbers
;; they can restore.
;;
;; nil = unavailable, which is every editor outside focus reader's merged mode.
;; Otherwise `{:pages [ints]}` — every page number the document was merged from,
;; which item 4.6 needs as the bound on what may be restored.
;;
;; WHY AN ATOM AND NOT A FORMAT-MENU OPT. `format-menu/install!` takes its opts
;; once per editor construction, and the document editor is persistent — so opts
;; cannot carry a flag that changes when the reader toggles the mode. The control
;; is built always and reads this. Re-installing the menu on every toggle is the
;; alternative, and it costs an editor teardown.
;;
;; Not a boolean, because the control needs the page list anyway: with markers 7
;; and 10 present it must answer 8, and it cannot know 8 exists without it.
;;
;; `defonce` takes no docstring — clojure.core/defonce is [name expr].
(defonce !page-break-context (atom nil))

(defn markers-in-document
  "Every live page marker, as `{:page <n> :index <document index>}`, ascending
   by document index.

   Pre : `ed` is a Quill instance.
   Post: one entry per element whose `id` parses as a page marker and whose
         blot is still live. A duplicate page number can appear — splitting a
         marker heading copies its id to both halves — and the caller decides
         what that means.

   Reads the DOM and resolves each index through `Quill.find`, the same way
   `replace-formula!` does: an index cached when the control opened would be
   stale the moment the document changed."
  [ed]
  #?(:cljs
     (let [^js ed ed
           ^js Quill (.-Quill js/window)
           els (.querySelectorAll (.-root ed) (str "[id^=\"" page-merge/marker-id-prefix "\"]"))]
       (->> (array-seq els)
            (keep (fn [^js el]
                    (when-let [page (page-merge/parse-marker-id (.-id el))]
                      (when-let [blot (.find Quill el)]
                        {:page page :index (.getIndex ed blot)}))))
            (sort-by :index)
            vec))
     :clj nil))

(defn page-above
  "The page the document position `index` belongs to, or nil when it sits above
   every marker.

   Pre : `markers` is `markers-in-document`'s result.
   Post: the page of the LAST marker at or before `index`."
  [markers index]
  (->> markers
       (filter #(<= (:index %) index))
       last
       :page))

(defn restorable-page
  "The page a restore at `index` would re-create, or nil to refuse.

   Pre : `ed` is a Quill instance. `!page-break-context` holds the merged
         document's page list, or nil.
   Post: nil when the mode is off, when the context is missing, or when every
         page between the neighbours already has a marker. Otherwise the LOWEST
         missing page above the insertion point.

   nil is the refusal signal, and a caller MUST NOT insert on it. This control
   restores a boundary and never creates a page: `topics.page_number` is an
   integer under a unique index, so a new page would mean renumbering every
   later one — which desynchronizes the text pages from the PDF and invalidates
   `p. N` on notes already pushed to Anki."
  [ed index]
  #?(:cljs
     (when-let [{:keys [pages]} @!page-break-context]
       (let [markers (markers-in-document ed)]
         (page-merge/next-missing-page (map :page markers)
           (page-above markers index)
           pages)))
     :clj nil))

(defn marker-safe-index
  "`index`, moved out of any marker heading it falls inside.

   Pre : `ed` is a Quill instance.
   Post: `index` when it is in body text. Otherwise the first position AFTER the
         enclosing heading's line, which is that page's first body position.

   WHY. A selection start inside another marker's heading splits that heading,
   and the new marker lands BEFORE it. Measured: restoring page 2 from a
   selection inside page 1's heading produced the document order 2, 1, 3, 4, 5.
   `split-merged` walks in document order, so that swaps two pages' content —
   corruption, not the drift the design accepts.

   A marker at document index `mi` whose text is `len` characters occupies
   `[mi, mi + len]`, and its line ends with the newline at `mi + len`."
  [ed index]
  #?(:cljs
     (let [^js ed ed
           ^js root (.-root ed)]
       (or (->> (markers-in-document ed)
                (keep (fn [{:keys [page] mi :index}]
                        (let [el (.querySelector root (str "#" (page-merge/marker-id page)))
                              len (if el (count (.-textContent el)) 0)]
                          (when (and (<= mi index) (<= index (+ mi len)))
                            (+ mi len 1)))))
                first)
           index))
     :clj nil))

(defn insert-page-break!
  "Restore a page boundary at `index`. Returns the page restored, or nil when
   there was nothing to restore.

   Pre : `ed` is a Quill instance; `index` is a document position.
   Post: on a page number, one marker heading for it sits at `index` and the
         caret is after it. On nil, the document is untouched.
   Inv  : inserts at most one marker, and never for a page outside the merged
         document's page list.

   Source \"user\", so `text-change` fires and the editor's auto-save persists
   the split. `formatLine` after `insertText` is what makes the line a heading
   carrying the id — the `id` Attributor is BLOCK-scoped, so the format has to
   land on the line rather than on the text."
  [ed index]
  #?(:cljs
     (when-let [page (restorable-page ed index)]
       (let [^js ed ed
             label (str "Page " page)
             at (marker-safe-index ed index)]
         (.insertText ed at (str label "\n") "user")
         (.formatLine ed at 1
           #js {:header 6 :id (page-merge/marker-id page)}
           "user")
         (.setSelection ed (+ at (count label) 1) 0 "user")
         page))
     :clj nil))

;; The page marker tracking installer's callback, and the scroll container it
;; watches. Both are per-editor and set by install-page-marker-tracking!.
;;
;; Kept as one atom rather than two so a teardown cannot leave half a wiring
;; behind: `install!` returns a fn that clears it.
(defonce ^:private !marker-tracking (atom nil))

;; The page the reader has scrolled to, as `{:doc-id … :page …}`, or nil.
;;
;; The editor writes; Electric watches. Same direction as
;; `rich-text-editor/!dirty-html`, and for the same reason: the scroll listener
;; lives with the Quill instance, and the page-change funnel lives in the
;; reactive graph.
;;
;; TAGGED WITH THE DOCUMENT, like `!dirty-html`, and for the same reason. This
;; is a `defonce`, so it outlives every editor, and no teardown clears it — the
;; tracking teardown clears `!marker-offsets` and `!marker-tracking` and leaves
;; this one. An untagged page number therefore leaked across documents: reading
;; a 200-page document to page 40 and then opening a 5-page one in-app set the
;; second to page 40, wrote `/viewer/topic/<id>/40` into the address bar, and
;; stored 40 as its last page. Reopening the SAME document hides this, because
;; the stored last page equals the stale value and the consumer's inequality
;; guard never fires.
;;
;; Tagging beats clearing on teardown: a tag makes a stale value unusable by
;; construction, where clearing is a discipline every future teardown has to
;; remember. `merged-page-bridge` checks `:doc-id` against `editor-doc-id`.
(defonce !scrolled-page (atom nil))

(defonce ^:private !scroll-target
  ;; Where the last programmatic scroll asked to go, as `{:page n :scroll-top px}`.
  ;;
  ;; `scroll-to-page!` writes it; `page-at-scroll-top` reads it and answers
  ;; `:page` while the scroller is still at `:scroll-top`. One pixel of reader
  ;; scrolling makes it stop matching, and the top-edge rule takes over.
  ;;
  ;; WHY IT IS NEEDED. A document's last pages are shorter than the viewport, so
  ;; their markers cannot reach the top edge — the scroller runs out first.
  ;; Measured on a 200-page document: markers ~392px apart, `max-scroll` 77500,
  ;; marker 199 at offset ~78008. So `scroll-to-page!` succeeds for 199 and 200
  ;; alike and both land at the same scrollTop, where the top-edge rule answers
  ;; 198 for both. Every page inside the final viewport was unreportable.
  ;;
  ;; POSITION, NOT A TIME WINDOW. The record is compared against the live
  ;; scrollTop, so there is no suppression period to get wrong and no ordering
  ;; against the scroll event it causes. It also needs no special case for the
  ;; bottom: a reader who scrolls to the end THEMSELVES matches no record and
  ;; gets 198, which is the page they are reading.
  ;;
  ;; Cleared by `invalidate-marker-offsets!`, because the two go stale together:
  ;; an edit or a resize can leave scrollTop equal while the page under it
  ;; changed.
  (atom nil))

(defn scroll-to-page!
  "Scroll the editor so page `page`'s boundary marker sits at the top of its
   scroll container. Returns true when a marker for that page was found.

   Pre : `ed` is a Quill instance.
   Post: on true, the marker is as close to the top as the scroller allows, and
         `page-at-scroll-top` answers `page` until the reader scrolls. On false,
         nothing moved and nothing was recorded — a page whose marker the reader
         deleted has nowhere to scroll to, and guessing would land the reader on
         the wrong text.

   This is what page navigation means in merged mode: the editor already holds
   every page, so a flip is a scroll rather than a fetch."
  [ed page]
  #?(:cljs
     (let [^js ed ed
           el (.querySelector (.-root ed) (str "#" (page-merge/marker-id page)))]
       (boolean
         (when el
           ;; `scrollIntoView` on the element rather than arithmetic on
           ;; offsetTop: the scroll container is Quill's own, and its identity
           ;; differs between the two editors in this app.
           (.scrollIntoView el #js {:block "start" :behavior "auto"})
           ;; `behavior "auto"` is instant, so scrollTop is already final here.
           ;; Recording it is what lets page-at-scroll-top answer a page whose
           ;; marker the scroller could not bring all the way up — see
           ;; !scroll-target.
           (reset! !scroll-target {:page page :scroll-top (.-scrollTop (.-root ed))})
           true)))
     :clj nil))

(defonce ^:private !marker-offsets
  ;; The page markers' scroll offsets, or nil when the table needs rebuilding.
  ;;
  ;; `[[page offset] …]` ascending by offset, where `offset` is the marker's
  ;; distance from the top of the scroller's content — scroll-invariant, so one
  ;; build serves every scroll position.
  ;;
  ;; WHY A TABLE AT ALL. The scroll handler used to call `getBoundingClientRect`
  ;; on EVERY marker, per scroll event. Each call forces layout, so the largest
  ;; real PDF here — 883 pages — cost 883 forced layouts per event, on the same
  ;; main thread the reader is scrolling with. `{passive: true}` does not help:
  ;; it keeps the gesture unblocked, not the layout out of the handler.
  ;;
  ;; This is memoization of a pure DOM read, not hysteresis. It is rebuilt FROM
  ;; the DOM and holds no previous answer, so the rule `page-at-scroll-top`
  ;; documents still holds: a remount drops it and the next call rebuilds.
  (atom nil))

(defn invalidate-marker-offsets!
  "Mark the offset table stale, and drop the programmatic-scroll record with it.

   Post: the next `page-at-scroll-top` rebuilds the table, and answers from the
         top edge until the next `scroll-to-page!`.

   Called on `text-change` and on a resize of the scroller, which are the two
   things that move a marker. Deliberately NOT a rebuild: an edit would then pay
   for every marker on every keystroke, which is worse than what the table
   replaced. The rebuild is deferred to the next scroll that needs an answer.

   `!scroll-target` goes at the same moment because it goes stale for the same
   reason: an edit above the caret leaves scrollTop equal while a different page
   sits under it, and the record would then name the wrong one."
  []
  #?(:cljs (do (reset! !marker-offsets nil)
               (reset! !scroll-target nil))
     :clj nil))

#?(:cljs
   (defn- build-marker-offsets!
     "Measure every marker once and cache the result.

      Pre : `root` is the scroller (`.ql-editor`).
      Post: `!marker-offsets` holds `[[page offset] …]` ascending by offset, and
            the same vector is returned. One layout pass for all markers."
     [^js root]
     (let [top (.-top (.getBoundingClientRect root))
           scrolled (.-scrollTop root)
           table (->> (array-seq (.querySelectorAll root
                                   (str "[id^=\"" page-merge/marker-id-prefix "\"]")))
                      (keep (fn [^js el]
                              (when-let [page (page-merge/parse-marker-id (.-id el))]
                                ;; Distance from the content top, so the value
                                ;; does not depend on where we are scrolled now.
                                [page (+ (- (.-top (.getBoundingClientRect el)) top)
                                        scrolled)])))
                      (sort-by second)
                      vec)]
       (reset! !marker-offsets table)
       table)))

(defn page-at-scroll-top
  "The page whose marker is the last one at or above the scroll container's top
   edge, or nil when the document holds no marker.

   Pre : `ed` is a Quill instance.
   Post: a page number present in the document, or nil.

   READS ONLY `scrollTop` on the hot path. The marker offsets come from a table
   built once per document shape (see `!marker-offsets`), so a scroll event
   forces no layout however many pages the document holds.

   NO REMEMBERED ANSWER. The table is derived from the DOM and the answer is
   recomputed from `scrollTop` every call, so a remount — which wipes the table —
   changes nothing but the cost of the next call. That is the rule the vendored
   `Scroll-window` follows for its offset.

   A PROGRAMMATIC SCROLL'S OWN ANSWER WINS, while the scroller is still where it
   put us. A document's last pages are shorter than the viewport, so their
   markers cannot reach the top edge and `scroll-to-page!` leaves several of them
   at the SAME scrollTop — the top-edge rule alone answers the same page for all
   of them, so none was reachable. `!scroll-target` records which one was asked
   for, and that record is compared by position, so a reader who scrolls one
   pixel gets the top-edge answer back."
  [ed]
  #?(:cljs
     (let [^js ed ed
           ;; `.ql-editor` IS the scroller. Measured on the document editor:
           ;; scrollHeight 4419 against clientHeight 636, while the
           ;; `.ql-container` above it — which is the same element as
           ;; `.quill-editor-wrapper` — never overflows. Both carry
           ;; `overflow-y: auto`, so the computed style cannot tell them apart;
           ;; only the scroll extent can. Listening on the container gave NO
           ;; scroll events at all and the page field never moved.
           ^js root (.-root ed)
           table (or @!marker-offsets (build-marker-offsets! root))]
       (when (seq table)
         (let [scrolled (.-scrollTop root)
               target @!scroll-target]
           (if (and target (= (:scroll-top target) scrolled))
             ;; Still exactly where a programmatic scroll left us — see
             ;; !scroll-target.
             (:page target)
             ;; A small tolerance, so a marker flush with the top edge counts as
             ;; reached rather than as one line away.
             (let [limit (+ scrolled 4)]
               (or (->> table (take-while (fn [[_ offset]] (<= offset limit))) last first)
                   ;; Scrolled above every marker: the document's own first page.
                   (first (first table))))))))
     :clj nil))

(defonce ^:private !restored-generation
  ;; The content generation this editor was last returned to a page for.
  ;;
  ;; A plain atom read by a plain fn, NOT a guard inside an e/defn body:
  ;; Electric re-evaluates reactive bodies unpredictably, and CLAUDE.md's
  ;; `init-viewer!` rule is that a generation guard belongs in normally
  ;; compiled code. Same shape as `!init-gen`.
  ;;
  ;; UNTAGGED, unlike `!scrolled-page` two screens up, and the difference is
  ;; real rather than an oversight. A generation is a global monotonic counter,
  ;; so a value left behind by one document can never equal one the next
  ;; document produces — staleness is self-defeating. A page NUMBER is drawn
  ;; from a small range every document shares, so a stale one is indistinguishable
  ;; from a fresh one and needs the document to tell them apart.
  (atom nil))

(defn restore-page-on-install!
  "Return the editor to `page` after a body install, once per generation.

   Pre : `generation` is `rich-text-editor/!content-generation`'s current value.
         `get-editor` is a no-argument fn answering the live Quill instance, or
         nil while one is being built — it is required, not optional. `page` is
         the page the reader is on.
   Post: nil when this generation was already handled, or when the editor or
         `page` is nil — nothing moved and nothing was recorded, so the next
         call for the same generation still gets its chance.
   Post: otherwise the returned page is the one the editor is NOW showing, and
         the generation is recorded. That is `page` when its marker was found,
         and the document's real topmost page when it was not.
   Inv : at most one restore per install. A page change with no new install
         returns nil, so a reader who scrolls is never pulled back.

   THE FALLBACK IS THE POINT, not an error path. A reader can delete a page's
   boundary, and `scroll-to-page!` then refuses rather than guessing at text.
   Answering with the page actually on screen keeps ONE source of truth: the
   caller feeds it to the page-change funnel, so the toolbar and the URL agree
   with what the reader can see. Holding the requested page instead would leave
   the field naming a page nothing on screen belongs to, until the reader
   happened to scroll.

   TAKES A FN, NOT THE EDITOR, and that is not a style choice. A zero-argument
   call inside an Electric body has no inputs, so work-skipping evaluates it
   once and holds the result forever — `(editor/current-editor)` written at the
   call site stayed nil from before the editor was built, and no install ever
   restored anything. Measured: the sibling `e/watch` fired for generation 0 and
   1 while this saw nil both times. A var read is a stable input, so the call
   re-runs when the generation changes and this reads the editor that exists
   now. It is also why the fn is passed rather than `!editor-state`: the map's
   shape belongs to the editor namespace, which this one cannot require."
  [generation get-editor page]
  #?(:cljs
     (let [ed (get-editor)]
       (when (and ed page (not= generation @!restored-generation))
         ;; Record only ON an answer, so the postcondition above holds: a nil
         ;; return means this generation is still unhandled and the next
         ;; evaluation gets its turn.
         (let [landed (if (scroll-to-page! ed page) page (page-at-scroll-top ed))]
           (when landed
             (reset! !restored-generation generation)
             (when js/goog.DEBUG
               (js/console.log "[merged] restored generation" generation
                 "to page" landed (when (not= landed page) (str "(asked " page ")")))))
           landed)))
     :clj nil))

(defn install-page-marker-tracking!
  "Report the page the reader has scrolled to. Returns a teardown fn.

   Pre : `ed` is a Quill instance. `on-page` takes a page number.
   Post: `on-page` fires with a page number whenever the topmost marker
         changes, and never with the same page twice in a row. The teardown
         removes every listener and observer it installed.

   `on-page` is the app's one page-change funnel, so a scroll records the page
   as current AND requests the last-page write — the same path the toolbar
   arrows and the PDF viewer use.

   THREE LISTENERS, and each earns its place. `scroll` answers the question.
   `text-change` and a `ResizeObserver` only INVALIDATE the offset table — they
   are the two things that move a marker, and invalidating is one atom write.

   The last-reported page is remembered ONLY to suppress a repeat call. It is
   not read to compute the answer."
  [ed on-page]
  #?(:cljs
     (let [^js ed ed
           ;; The scroller is `.ql-editor` itself — see page-at-scroll-top.
           ^js scroller (.-root ed)
           !last (atom nil)
           handler (fn []
                     (when-let [page (page-at-scroll-top ed)]
                       (when (not= page @!last)
                         (reset! !last page)
                         (on-page page))))
           invalidate (fn [& _] (invalidate-marker-offsets!))
           resize-obs (js/ResizeObserver. invalidate)]
       (.addEventListener scroller "scroll" handler #js {:passive true})
       (.on ed "text-change" invalidate)
       (.observe resize-obs scroller)
       (invalidate-marker-offsets!)
       (reset! !marker-tracking {:scroller scroller :handler handler})
       (fn []
         (.removeEventListener scroller "scroll" handler)
         (.off ed "text-change" invalidate)
         (.disconnect resize-obs)
         (invalidate-marker-offsets!)
         (reset! !marker-tracking nil)))
     :clj (fn [] nil)))
