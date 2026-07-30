(ns freememo.editor-actions
  "Editor mutation actions shared across Quill instances — cloze-deletion
   insertion, formula insert/edit, and the pasted/dropped/picked image re-host
   path that keeps base64 out of both editors (`install-image-rehost!`).

   Extracted from quill_field so both the QuillField lifecycle and the custom
   format-menu drive the same behavior without a namespace cycle (format-menu
   requires this ns; this ns requires neither). CLJS side effects; the CLJ
   branch is inert so the ns loads on both peers."
  (:require [clojure.string :as str]
            [freememo.client-errors :as ce]
            [freememo.math :as math]
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
          `ed`'s document; the formula module is enabled.
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

   Pre  : `ed` is a Quill instance or nil; the formula module is enabled.
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

   Pre  : `ed` is a Quill instance; the formula module is enabled (KaTeX present).
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
