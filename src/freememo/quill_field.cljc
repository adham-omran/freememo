(ns freememo.quill-field
  "Parameterized Quill editor field — reusable across card-edit modal and any
   other site needing an inline HTML editor. NOT coupled to topic auto-save.
   Callers pass a string value and an on-change callback.

   API (positional: value-string on-change placeholder field-key
        !editor-atom cloze? autofocus?):
     (QuillField \"<p>Hello</p>\"
                 (fn [html] ...)
                 \"Enter text...\" :some-stable-key !editor nil nil)"
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.quill-table-ui :as table-ui]
   [clojure.string :as str]))

;; ---------------------------------------------------------------------------
;; Quill config — shared with rich_text_editor.cljc via this defn.
;; ---------------------------------------------------------------------------

;; Mirror of the icon override in rich_text_editor.cljc — idempotent (same SVG
;; string; either ns loading first sets it). See that ns for context.
#?(:cljs
   (defonce ^:private code-block-icon-installed?
     (let [icons (.import js/Quill "ui/icons")]
       (aset icons "code-block"
         (str "<svg viewbox=\"0 0 18 18\">"
           "<rect class=\"ql-stroke\" x=\"2\" y=\"4\" width=\"14\" height=\"10\" rx=\"1\" ry=\"1\" fill=\"none\"/>"
           "<polyline class=\"ql-stroke\" points=\"5 7 7 9 5 11\" fill=\"none\"/>"
           "<line class=\"ql-stroke\" x1=\"9\" y1=\"11\" x2=\"13\" y2=\"11\"/>"
           "</svg>"))
       true)))

;; Cloze toolbar glyphs — a brace pair { } framing + (new cloze) and = (same
;; cloze), mirroring the {{cN::}} markup. Same convention as the code-block
;; override: ql-stroke paths in an 18x18 viewbox so they inherit toolbar
;; hover/active theming. Buttons are wired to handlers in init-quill-field!.
#?(:cljs
   (defonce ^:private cloze-icons-installed?
     (let [icons  (.import js/Quill "ui/icons")
           braces (str "<path class=\"ql-stroke\" fill=\"none\" d=\"M6.5 3.5 Q4.5 3.5 4.5 6.5 Q4.5 8 3 9 Q4.5 10 4.5 11.5 Q4.5 14.5 6.5 14.5\"/>"
                    "<path class=\"ql-stroke\" fill=\"none\" d=\"M11.5 3.5 Q13.5 3.5 13.5 6.5 Q13.5 8 15 9 Q13.5 10 13.5 11.5 Q13.5 14.5 11.5 14.5\"/>")]
       (aset icons "cloze-inc"
         (str "<svg viewbox=\"0 0 18 18\">" braces
           "<line class=\"ql-stroke\" x1=\"9\" y1=\"6.5\" x2=\"9\" y2=\"11.5\"/>"
           "<line class=\"ql-stroke\" x1=\"6.8\" y1=\"9\" x2=\"11.2\" y2=\"9\"/>"
           "</svg>"))
       (aset icons "cloze-eq"
         (str "<svg viewbox=\"0 0 18 18\">" braces
           "<line class=\"ql-stroke\" x1=\"6.8\" y1=\"7.5\" x2=\"11.2\" y2=\"7.5\"/>"
           "<line class=\"ql-stroke\" x1=\"6.8\" y1=\"10.5\" x2=\"11.2\" y2=\"10.5\"/>"
           "</svg>"))
       true)))

;; ---------------------------------------------------------------------------
;; Cloze deletion — number tracking + selection wrapping (CLJS).
;; Defined before quill-config because the toolbar handlers there call
;; insert-cloze! (avoids a CLJS undeclared-var warning on a forward ref).
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

(defn insert-cloze!
  "Wrap the current Quill selection in an Anki cloze deletion {{cN::...}}.
     mode :inc → next cloze number (inc of the current max in the editor);
     mode :eq  → reuse the current max (min 1).
   Collapsed selection inserts an empty {{cN::}} with the cursor between
   :: and }}. No selection (editor never focused) → no-op. CLJS only.

   Pre  : `ed` is a Quill instance or nil; `mode` ∈ {:inc :eq}.
   Post : on a live selection, editor text gains the wrapper and the cursor
          sits after }} (selection) or inside (collapsed); the user-source
          edit fires text-change → the field's :on-change → !cloze sync."
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
                 suffix "}}"]
             (if (pos? length)
               (let [selected (.getText ed index length)]
                 (.deleteText ed index length "user")
                 (.insertText ed index (str prefix selected suffix) "user")
                 (.setSelection ed (+ index (count prefix) (count selected) (count suffix)) 0 "user"))
               (do
                 (.insertText ed index (str prefix suffix) "user")
                 (.setSelection ed (+ index (count prefix)) 0 "user")))))))
     :clj nil))

(defn quill-config
  "Returns the Quill constructor options map (passed via clj->js).
   Same toolbar, syntax module, and modules as the main editor so the card
   modals offer the identical formatting surface, including code-block with
   highlight.js syntax colouring.
   When `cloze?` is true, the toolbar gains a cloze-deletion button group
   (cloze-inc / cloze-eq) with construction-time handlers that wrap the
   current selection via insert-cloze!. The handlers MUST be supplied here,
   not added after construction: Quill's toolbar refuses to attach a click
   listener to a button whose format is unknown AND has no handler at
   construction time (logs \"ignoring attaching to nonexistent format\")."
  [placeholder cloze?]
  (let [base [["bold" "italic" "underline" "strike"]
              [{"header" 1} {"header" 2} {"header" 3}]
              [{"size" ["small" false "large" "huge"]}]
              [{"color" []} {"background" []}]
              [{"list" "ordered"} {"list" "bullet"}]
              [{"align" []}]
              [{"direction" "rtl"}]
              ["code" "code-block"]
              ["clean"]
              ["image"]
              ["table"]]]
    {:theme "snow"
     :modules {:toolbar (if cloze?
                          {:container (conj base ["cloze-inc" "cloze-eq"])
                           :handlers #?(:cljs {"cloze-inc" (fn [_] (this-as ^js tb (insert-cloze! (.-quill tb) :inc)))
                                               "cloze-eq"  (fn [_] (this-as ^js tb (insert-cloze! (.-quill tb) :eq)))}
                                        :clj {})}
                          base)
               ;; Syntax module — requires window.hljs + clojure pack (loaded in
               ;; index*.html). Same `:languages` list as the main editor for
               ;; consistent picker UX across modals and the page editor.
               :syntax {:languages [{:key "plain" :label "Plain"}
                                    {:key "bash" :label "Bash"}
                                    {:key "cpp" :label "C++"}
                                    {:key "cs" :label "C#"}
                                    {:key "clojure" :label "Clojure"}
                                    {:key "css" :label "CSS"}
                                    {:key "diff" :label "Diff"}
                                    {:key "xml" :label "HTML/XML"}
                                    {:key "java" :label "Java"}
                                    {:key "javascript" :label "JavaScript"}
                                    {:key "markdown" :label "Markdown"}
                                    {:key "php" :label "PHP"}
                                    {:key "python" :label "Python"}
                                    {:key "ruby" :label "Ruby"}
                                    {:key "rust" :label "Rust"}
                                    {:key "sql" :label "SQL"}]}
               :table true}
     :placeholder (or placeholder "Enter text...")}))

;; ---------------------------------------------------------------------------
;; Image-upload paste helper — CLJS only, defined at the module level.
;; ---------------------------------------------------------------------------

(defn upload-pasted-image!
  "Upload a data-URI blob to /api/upload-media.
   Calls on-uploaded with the /api/media/<id> URL on success, nil on error.
   CLJS only."
  [data-uri on-uploaded]
  #?(:cljs
     (let [parts (str/split data-uri #",")
           header (first parts)
           b64 (second parts)
           mime (-> header
                  (str/replace "data:" "")
                  (str/replace ";base64" ""))
           bin-str (js/atob b64)
           n (count bin-str)
           buf (js/Uint8Array. n)
           _ (dotimes [i n]
               (aset buf i (.charCodeAt bin-str i)))
           blob (js/Blob. (clj->js [buf]) (clj->js {:type mime}))
           ext (last (str/split mime #"/"))
           form (js/FormData.)]
       (.append form "file" blob (str "image." ext))
       (-> (js/fetch "/api/upload-media"
             (clj->js {:method "POST"
                       :credentials "same-origin"
                       :body form}))
         (.then (fn [r]
                  (if (.-ok r)
                    (.json r)
                    (throw (js/Error. (str "Upload failed: " (.-status r)))))))
         (.then (fn [json]
                  (let [id (.-id json)]
                    (when id
                      (on-uploaded (str "/api/media/" id))))))
         (.catch (fn [err]
                   (js/console.warn "[QuillField] image upload failed:" (str err))
                   (on-uploaded nil)))))
     :clj nil))

;; ---------------------------------------------------------------------------
;; Quill instance lifecycle — plain defns so side effects are stable.
;; ---------------------------------------------------------------------------

(defn init-quill-field!
  "Initialize a standalone Quill editor in container with initial-html.
   Wires text-change to call on-change with updated innerHTML on each user edit.
   Wires a clipboard matcher that uploads data-URI images before inserting them.
   Returns the Quill instance (CLJS) or nil (CLJ).
   No ^js on parameters — CLJ compiler sees the parameter list."
  [container initial-html placeholder on-change cloze?]
  #?(:cljs
     (when (and container (.-Quill js/window))
       (let [Quill (.-Quill js/window)
             cfg (clj->js (quill-config placeholder cloze?))
             ed (new Quill container cfg)
             cb (.-clipboard ed)
             ;; Tell Quill's clipboard pipeline to ignore the syntax module's
             ;; language-picker <select>. Without this, Quill's default
             ;; handling extracts <option> labels as text and emits a <p> with
             ;; the concatenated picker labels on every reload. Returning an
             ;; empty Delta makes the converter treat the <select> subtree as
             ;; if it didn't exist; container + child line divs are processed
             ;; by their own matchers and continue to render as multi-line
             ;; code blocks.
             Delta (.import (.-Quill js/window) "delta")
             _ (.addMatcher cb "select.ql-ui"
                 (fn [_node _delta] (new Delta)))
             ;; Strip Quill syntax-module token wrappers. CodeToken.formats
             ;; in Quill 2.0.3 returns boolean `true` instead of the actual
             ;; hljs-X value (rendering as `class="hljs-true"` and merging
             ;; adjacent same-value inlines). Returning a Delta with just
             ;; the textContent hands Quill clean text; the syntax module
             ;; re-applies fresh tokens on its 1 s timer.
             _ (.addMatcher cb "span.ql-token"
                 (fn [^js node _delta]
                   (-> (new Delta) (.insert (.-textContent node)))))
             ;; Preserve whitespace inside code blocks. Quill 2.0.3's
             ;; default text-node walker collapses leading whitespace at
             ;; element boundaries (HTML parser whitespace normalization),
             ;; dropping user-typed indentation on reload. Reading the
             ;; div's textContent verbatim sidesteps that path; the
             ;; syntax module re-applies hljs spans after setContents.
             _ (.addMatcher cb "div.ql-code-block"
                 (fn [^js node _delta]
                   (let [text (.-textContent node)
                         lang (or (.getAttribute node "data-language") "plain")]
                     (-> (new Delta)
                       (.insert text)
                       (.insert "\n" #js {"code-block" lang})))))
             raw (or initial-html "")
             cleaned (-> raw
                       (str/replace (js/RegExp. "^```html\\s*\\n?" "") "")
                       (str/replace (js/RegExp. "^```\\s*\\n?" "") "")
                       (str/replace (js/RegExp. "\\n?```\\s*$" "") "")
                       ;; Cleanup of paragraphs already corrupted by a prior
                       ;; reload — exact label-concatenation string from both
                       ;; pre-Clojure and current pickers. (Future corruption
                       ;; is prevented by the `select.ql-ui` clipboard matcher
                       ;; registered below.)
                       (str/replace (js/RegExp. "<p>PlainBashC\\+\\+C#(?:Clojure)?CSSDiffHTML/XMLJavaJavaScriptMarkdownPHPPythonRubySQL</p>" "g") "")
                       str/trim)
             delta (.convert cb (clj->js {:html cleaned}))]
         (when (seq cleaned)
           (.setContents ed delta))
         ;; See rich_text_editor.cljc for the dir="auto" rationale.
         (.setAttribute (.-root ed) "dir" "auto")
         ;; text-change: call on-change on user edits. Quill 2.0.3's syntax
         ;; module mutates the DOM under quill.update(SILENT) without firing
         ;; any text-change event observable by listeners, so on-change
         ;; cannot be used to capture the post-tokenize HTML — see
         ;; `flush-syntax-tokens!` and the Save-side callers in
         ;; `freememo.card-modals` for the explicit capture path.
         (.on ed "text-change"
           (fn [_delta _old source]
             (when (and (= source "user") on-change)
               (let [root (.-root ed)]
                 (on-change (.-innerHTML root))))))
         ;; Paste matcher: intercept data-URI images, upload, rewrite src
         (let [placeholder-gif "data:image/gif;base64,R0lGODlhAQABAIAAAAAAAP///yH5BAEAAAAALAAAAAABAAEAAAIBRAA7"]
           (.addMatcher cb js/Node.ELEMENT_NODE
             (fn [node delta-arg]
               (let [imgs (.querySelectorAll node "img")]
                 (doseq [img (array-seq imgs)]
                   (let [src (.getAttribute img "src")]
                     (when (and src (str/starts-with? src "data:image/"))
                       (.setAttribute img "src" placeholder-gif)
                       (upload-pasted-image!
                         src
                         (fn [new-src]
                           (when new-src
                             (let [root (.-root ed)
                                   sel (str "img[src=\"" placeholder-gif "\"]")
                                   found (.querySelector root sel)]
                               (when found
                                 (.setAttribute found "src" new-src)
                                 (when on-change
                                   (on-change (.-innerHTML root))))))))))))
               delta-arg)))
         ;; Wire the table toolbar handler + contextual action bar.
         ;; The QuillField's toolbar config includes "table", so without
         ;; this the button is rendered but inert. Cleanup is paired in
         ;; destroy-quill-field! via table-ui/teardown!.
         (table-ui/init! ed)
         ed))
     :clj nil))

(defn destroy-quill-field!
  "Destroy a QuillField Quill instance and clean up its DOM. No ^js params —
   ^js hints live on let-bindings inside the :cljs branch."
  [ed]
  #?(:cljs
     (when ed
       (let [^js ed ed]
         ;; Remove table action bar from document.body and unregister
         ;; window scroll/resize listeners. Idempotent.
         (table-ui/teardown! ed)
         (.off ed "text-change")
         (let [^js container (.-container ed)]
           (when container
             (let [^js parent (.-parentNode container)]
               (when parent
                 (let [^js toolbar (.querySelector parent ".ql-toolbar")]
                   (when toolbar
                     (.remove toolbar)))))
             (set! (.-innerHTML container) "")))))
     :clj nil))

(defn schedule-quill-init!
  "Schedule Quill init on the next tick (CLJS-only side effect).
   Wrapper exists so the reader conditional lives in a plain defn — keeps
   CLJ/CLJS signal counts identical inside the e/defn reactive body
   (CLAUDE.md frame-mismatch rule)."
  [!ed-state container value-string placeholder on-change !editor-atom cloze? autofocus?]
  #?(:cljs (js/setTimeout
             (fn []
               (let [ed (init-quill-field! container value-string placeholder on-change cloze?)]
                 (reset! !ed-state ed)
                 (when !editor-atom (reset! !editor-atom ed))
                 ;; Focus after construction — Quill has no autofocus option and
                 ;; is built on this deferred tick, so the modal can't focus it at
                 ;; mount. Focusing here lands the cursor in the editor on open.
                 (when (and autofocus? ed) (.focus ^js ed))))
             0)
     :clj nil))

;; ---------------------------------------------------------------------------
;; Electric component
;; ---------------------------------------------------------------------------

(defn flush-syntax-tokens!
  "Synchronously run the Quill syntax module's highlight() pass on the given
   editor, then return the post-flush innerHTML of the editor's root.

   Quill 2.0.3's syntax module mutates the DOM via formatAt and wraps its
   work in quill.update(SILENT) — but no text-change event reaches any
   listener for that mutation pass. Verified empirically by attaching a
   text-change listener and observing zero events fire when highlight()
   runs. Callers therefore CANNOT rely on the QuillField's `:on-change`
   callback to learn that tokens have been applied; they must take the
   returned HTML and persist it directly.

   Pre  : `ed` is a Quill instance (CLJS) or nil.
   Post : DOM under `ed.root` carries up-to-date hljs-* spans for every
          `.ql-code-block-container`; returned string is the editor's
          innerHTML after the flush.
   Inv  : returns nil iff `ed` is nil; never throws on nil input."
  [ed]
  #?(:cljs
     (when ed
       (let [^js ed ed
             ^js syntax (.getModule ed "syntax")]
         (when syntax (.highlight syntax))
         (.-innerHTML (.-root ed))))
     :clj nil))

(e/defn QuillField
  "Standalone Quill editor field.

   Parameters: single opts map.
     :value-string  string  Initial HTML content (mount-time only).
     :on-change     fn      Called with updated HTML string on every user edit
                             AND on every syntax-module re-tokenize.
     :placeholder   string  Editor placeholder text.
     :field-key     any     Stable key for e/for-by frame isolation.
                             When this changes the Quill instance remounts.
                             Use card-id + field-name e.g. [:question card-id].
     :!editor-atom  atom    OPTIONAL. If supplied, QuillField will reset! it
                             to the Quill instance on mount and to nil on
                             unmount. Callers use it to invoke
                             `flush-syntax-tokens!` before reading the
                             captured HTML — necessary when the user may
                             click Save inside the syntax module's 1 s
                             debounce window.
     :cloze?        bool    OPTIONAL. When true, the toolbar shows the
                             cloze-deletion buttons (c+ / c=) wired to
                             insert-cloze!. Used by the card modals' cloze
                             field only.
     :autofocus?    bool    OPTIONAL. When true, focus the editor once it is
                             constructed (cursor lands in the field on open).

   Usage:
     (QuillField (or (:question card) \"\")
                 (fn [html] (reset! !question html))
                 \"Question...\" [:question card-id] !q-editor nil true)"
  [value-string on-change placeholder field-key !editor-atom cloze? autofocus?]
  (e/client
    ;; e/for-by provides frame isolation: Quill remounts when field-key changes.
    (e/for-by identity [_k [(or field-key :quill-field)]]
      (dom/div
        (dom/props {:class "quill-editor-wrapper quill-field-wrapper"})
        (let [!ed-state (atom nil)
              ;; Freeze the mount-time value. Callers may pass a live watched
              ;; atom (the card modals seed the editor from !primary/!answer so
              ;; content carries across a Basic↔Cloze switch); without the
              ;; snapshot, each keystroke would change value-string and re-run
              ;; schedule-quill-init!, mounting a second Quill instance.
              seed (e/snapshot value-string)]
          ;; Schedule init via plain defn — reader conditional lives there,
          ;; not in this reactive body (avoids CLJ/CLJS signal-count mismatch).
          (schedule-quill-init! !ed-state dom/node seed placeholder on-change !editor-atom cloze? autofocus?)
          (e/on-unmount
            (fn []
              (destroy-quill-field! @!ed-state)
              (reset! !ed-state nil)
              (when !editor-atom (reset! !editor-atom nil)))))))))
