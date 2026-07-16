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
   [freememo.a11y :as a11y]
   [freememo.editor-actions :as editor-actions]
   #?(:cljs [freememo.format-menu :as format-menu])
   #?(:cljs [freememo.code-lang-picker :as code-lang-picker])
   [clojure.string :as str]))

;; ---------------------------------------------------------------------------
;; Quill config — single source of truth, required by rich_text_editor.cljc
;; (the main document editor) rather than redefined there. Both editors need
;; the identical toolbar/syntax/table setup, the code-block icon override,
;; and the clipboard matchers below; a prior copy-paste of all three into
;; rich_text_editor.cljc had drifted the icon's line by 1px.
;; ---------------------------------------------------------------------------

;; Quill 2.0.3 ships one `code.svg` aliased to both "code" and "code-block",
;; making the two toolbar buttons visually identical. Override "code-block"
;; with a terminal-frame glyph so users can distinguish inline from block.
;; Inline "code" stays on Quill's default `</>` icon.
#?(:cljs
   (defonce ^:private code-block-icon-installed?
     (let [icons (.import js/Quill "ui/icons")]
       (aset icons "code-block"
         (str "<svg viewbox=\"0 0 18 18\">"
           "<rect class=\"ql-stroke\" x=\"2\" y=\"4\" width=\"14\" height=\"10\" rx=\"1\" ry=\"1\" fill=\"none\"/>"
           "<polyline class=\"ql-stroke\" points=\"5 7 7 9 5 11\" fill=\"none\"/>"
           "<line class=\"ql-stroke\" x1=\"9\" y1=\"12\" x2=\"13\" y2=\"12\"/>"
           "</svg>"))
       true)))

(defn quill-config
  "Returns the Quill constructor options map (passed via clj->js).
   Bubble theme with the SAME modules as the main document editor
   (freememo.rich-text-editor) so a card field offers the identical formatting
   surface via the custom format-menu, including code-block highlight.js
   colouring and tables. Image insertion and cloze deletion are provided by
   format-menu opts in init-quill-field! (not the Quill toolbar), so the hidden
   bubble toolbar carries neither.
   `extra` (optional) is merged in last, letting a caller add ctor options
   this ns has no use for — e.g. rich_text_editor.cljc passes {:bounds
   container} to clamp its floating tooltip to the editor pane; QuillField
   has no such ancestor to clamp to and omits it."
  ([placeholder] (quill-config placeholder nil))
  ([placeholder extra]
   (merge
     {:theme "bubble"
      :modules {:toolbar [["bold" "italic" "underline" "strike"]
                          [{"header" 1} {"header" 2} {"header" 3}]
                          [{"size" ["small" false "large" "huge"]}]
                          [{"color" []} {"background" []}]
                          [{"list" "ordered"} {"list" "bullet"}]
                          [{"align" []}]
                          [{"direction" "rtl"}]
                          ["code" "code-block"]
                          ["clean"]
                          ["table"]]
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
      :placeholder (or placeholder "Enter text...")}
     extra)))

(defn install-clipboard-matchers!
  "Register the clipboard matchers that keep Quill's syntax-highlighted code
   blocks round-trip-safe through HTML. Shared by rich_text_editor.cljc's
   init-editor! and this ns's init-quill-field! — CLJS-only, no-op on CLJ.

   - `select.ql-ui`: the syntax module's per-block language <select>. Quill's
     default clipboard handling extracts the <option> labels as text and
     emits a <p> with the concatenated picker labels on every reload,
     growing one corrupted paragraph per save cycle. Returning a fresh empty
     Delta tells the converter this subtree contributes nothing — the
     surrounding container + child line divs are processed by their own
     matchers and continue to render as multi-line code blocks.
   - `span.ql-token`: CodeToken.formats in Quill 2.0.3 returns boolean `true`
     instead of the actual hljs-X value, producing `class=\"hljs-true\"` and
     collapsing adjacent same-value inlines into one span. Returning a Delta
     with just the textContent (no code-token attribute) hands Quill clean
     text; the syntax module's 1 s timer re-applies fresh, correctly-classed
     tokens.
   - `div.ql-code-block`: preserves leading/trailing/inter-character
     whitespace inside code blocks. Quill 2.0.3's default clipboard text-node
     walker runs through the HTML parser's whitespace normalization, which
     collapses leading runs of spaces at element boundaries — losing
     user-typed indentation on every reload. Reading the div's textContent
     verbatim and emitting one text op + one code-block-attributed newline
     op per line hands Quill clean, whitespace-faithful text; the syntax
     module re-applies hljs spans on its 1 s timer after setContents."
  [clipboard]
  #?(:cljs
     (let [^js clipboard clipboard
           Delta (.import (.-Quill js/window) "delta")]
       (.addMatcher clipboard "select.ql-ui"
         (fn [_node _delta] (new Delta)))
       (.addMatcher clipboard "span.ql-token"
         (fn [^js node _delta]
           (-> (new Delta) (.insert (.-textContent node)))))
       (.addMatcher clipboard "div.ql-code-block"
         (fn [^js node _delta]
           (let [text (.-textContent node)
                 lang (or (.getAttribute node "data-language") "plain")]
             (-> (new Delta)
               (.insert text)
               (.insert "\n" #js {"code-block" lang}))))))
     :clj nil))

;; ---------------------------------------------------------------------------
;; Quill instance lifecycle — plain defns so side effects are stable.
;; ---------------------------------------------------------------------------

(defn init-quill-field!
  "Initialize a standalone Quill editor (bubble theme) in container with
   initial-html. Wires text-change → on-change on each user edit, a clipboard
   matcher that uploads data-URI images, and the custom format-menu +
   code-lang-picker (the same components as the main document editor). When
   `cloze?` is true the format-menu gains the cloze-deletion row.
   Returns {:editor <Quill> :teardowns [fn ...]} (CLJS) or nil (CLJ); the
   teardowns remove the format-menu / code-lang picker on destroy.
   No ^js on parameters — CLJ compiler sees the parameter list."
  [container initial-html placeholder on-change cloze?]
  #?(:cljs
     (when (and container (.-Quill js/window))
       (let [Quill (.-Quill js/window)
             cfg (clj->js (quill-config placeholder))
             ed (new Quill container cfg)
             cb (.-clipboard ed)
             ;; See install-clipboard-matchers! above for what each matcher
             ;; does and why (shared with rich_text_editor.cljc's main editor).
             _ (install-clipboard-matchers! cb)
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
                       (editor-actions/upload-pasted-image!
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
         ;; Bubble hides its native tooltip (CSS); the custom format-menu is
         ;; the formatting UI and the code-lang-picker owns code-block language
         ;; selection. label-quill-toolbar! stays for the (hidden) tooltip
         ;; pickers — inert but harmless.
         (a11y/label-quill-toolbar! (.-parentElement container))
         ;; Tab moves focus out (Quill's indent bindings removed); Escape
         ;; also blurs, as belt-and-suspenders for nested contexts.
         (a11y/free-quill-tab! ed)
         (a11y/install-quill-tab-escape! ed)
         ;; Custom components — image on every card field, cloze row only when
         ;; cloze?. Teardowns are returned so destroy-quill-field! can remove
         ;; the body cards + document listeners on modal close.
         {:editor ed
          :teardowns [(format-menu/install! ed {:image? true :cloze? (boolean cloze?)})
                      (code-lang-picker/install! ed)]}))
     :clj nil))

(defn destroy-quill-field!
  "Destroy a QuillField instance and clean up its DOM + custom components.
   Takes the map init-quill-field! returned ({:editor :teardowns}) or nil.
   Pre  : `state` is that map or nil.
   Post : format-menu + code-lang picker cards/listeners removed, table action
          bar torn down, editor listeners detached, container emptied."
  [state]
  #?(:cljs
     (when-let [^js ed (:editor state)]
       ;; Remove the custom format-menu + code-lang picker (body cards +
       ;; document listeners) before the editor DOM is torn down.
       (doseq [teardown (:teardowns state)]
         (when teardown (teardown)))
       ;; Remove table action bar from document.body and unregister
       ;; window scroll/resize listeners. Idempotent.
       (table-ui/teardown! ed)
       (.off ed "text-change")
       (.off ed "selection-change")
       (let [^js container (.-container ed)]
         (when container
           (let [^js parent (.-parentNode container)]
             (when parent
               (let [^js toolbar (.querySelector parent ".ql-toolbar")]
                 (when toolbar
                   (.remove toolbar)))))
           (set! (.-innerHTML container) ""))))
     :clj nil))

(defn schedule-quill-init!
  "Schedule Quill init on the next tick (CLJS-only side effect).
   Wrapper exists so the reader conditional lives in a plain defn — keeps
   CLJ/CLJS signal counts identical inside the e/defn reactive body
   (CLAUDE.md frame-mismatch rule)."
  [!ed-state container value-string placeholder on-change !editor-atom cloze? autofocus?]
  #?(:cljs (js/setTimeout
             (fn []
               (let [result (init-quill-field! container value-string placeholder on-change cloze?)
                     ^js ed (:editor result)]
                 ;; !ed-state holds the {:editor :teardowns} map for destroy;
                 ;; !editor-atom + focus use the raw Quill instance.
                 (reset! !ed-state result)
                 (when !editor-atom (reset! !editor-atom ed))
                 ;; Focus after construction — Quill has no autofocus option and
                 ;; is built on this deferred tick, so the modal can't focus it at
                 ;; mount. Focusing here lands the cursor in the editor on open.
                 (when (and autofocus? ed) (.focus ed))))
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
     :cloze?        bool    OPTIONAL. When true, the format-menu shows the
                             cloze-deletion buttons wired to
                             editor-actions/insert-cloze!. Used by the card
                             modals' cloze field only.
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
