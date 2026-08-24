(ns freememo.editor-actions
  "Editor mutation actions shared across Quill instances — cloze-deletion
   insertion, formula insert/edit, and the pasted/dropped/picked image re-host
   path that keeps base64 out of both editors (`install-image-rehost!`).

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
          `a11y/install-quill-tab-escape!`'s argument, one layer in."
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

(defn upload-image-blob!
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
     (-> (upload-image-blob! (data-uri->blob data-uri))
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
       (-> (upload-image-blob! file)
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

(defn install-image-rehost!
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
             (let [storable? (fn [^js f] (util/storable-image-mime-types (.-type f)))
                   fs (vec (array-seq files))]
               (doseq [^js f (remove storable? fs)]
                 (ce/report! :editor/image-format
                   (str "Unsupported image format: " (.-type f))
                   (str "Cannot paste "
                     (if (str/blank? (.-type f)) "this file" (.-type f))
                     " — supported image formats are PNG, JPEG, GIF, WebP and AVIF.")))
               ;; Chained, not concurrent: each insert awaits the previous one so
               ;; multiple files land in clipboard order. Only the INSERT is
               ;; serialised — the uploads run in parallel behind it.
               (reduce (fn [p file]
                         (.then p (fn [i] (insert-and-rehost-image! ed i file))))
                 (js/Promise.resolve (.-index range))
                 (filter storable? fs))
               nil)))))
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

;; The page the reader has scrolled to in a merged document, or nil.
;;
;; The editor writes; Electric watches. Same direction as
;; `rich-text-editor/!dirty-html`, and for the same reason: the scroll listener
;; lives with the Quill instance, and the page-change funnel lives in the
;; reactive graph.
(defonce !scrolled-page (atom nil))

(defn scroll-to-page!
  "Scroll the editor so page `page`'s boundary marker sits at the top of its
   scroll container. Returns true when a marker for that page was found.

   Pre : `ed` is a Quill instance.
   Post: on true, the marker is the topmost visible line. On false, nothing
         moved — a page whose marker the reader deleted has nowhere to scroll
         to, and guessing would land the reader on the wrong text.

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
           true)))
     :clj nil))

(defn page-at-scroll-top
  "The page whose marker is the last one at or above the scroll container's top
   edge, or nil when no marker is at or above it.

   Pre : `ed` is a Quill instance.
   Post: a page number present in the document, or nil.

   PURE PER-CALL. It reads only the live DOM — no remembered previous answer and
   no hysteresis. Electric frames remount on a WebSocket reconnect and on a hot
   reload, which wipes any state kept across frames, so a derivation that
   depended on one could not hold. Same rule the vendored `Scroll-window`
   follows for its offset."
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
           edge (.-top (.getBoundingClientRect root))
           els (.querySelectorAll root (str "[id^=\"" page-merge/marker-id-prefix "\"]"))
           markers (array-seq els)]
       (or (->> markers
                (keep (fn [^js el]
                        (when-let [page (page-merge/parse-marker-id (.-id el))]
                          ;; A small tolerance, so a marker flush with the top
                          ;; edge counts as reached rather than as one line away.
                          (when (<= (.-top (.getBoundingClientRect el)) (+ edge 4))
                            page))))
                last)
           ;; Scrolled above every marker: the document's own first page.
           (some-> markers first .-id page-merge/parse-marker-id)))
     :clj nil))

(defn install-page-marker-tracking!
  "Report the page the reader has scrolled to. Returns a teardown fn.

   Pre : `ed` is a Quill instance. `on-page` takes a page number.
   Post: `on-page` fires with a page number whenever the topmost marker
         changes, and never with the same page twice in a row. The teardown
         removes the listener.

   `on-page` is the app's one page-change funnel, so a scroll records the page
   as current AND requests the last-page write — the same path the toolbar
   arrows and the PDF viewer use.

   The last-reported page is remembered ONLY to suppress a repeat call. It is
   not read to compute the answer: `page-at-scroll-top` is pure, so a remount
   that loses this simply re-reports once."
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
                         (on-page page))))]
       (.addEventListener scroller "scroll" handler #js {:passive true})
       (reset! !marker-tracking {:scroller scroller :handler handler})
       (fn []
         (.removeEventListener scroller "scroll" handler)
         (reset! !marker-tracking nil)))
     :clj (fn [] nil)))
