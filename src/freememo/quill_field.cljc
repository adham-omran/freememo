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
   [freememo.code-languages :refer [code-languages]]
   [freememo.editor-actions :as editor-actions]
   [freememo.quill-markdown :as quill-markdown]
   [freememo.math :as math]
   #?(:cljs [freememo.format-menu :as format-menu])
   #?(:cljs [freememo.code-lang-picker :as code-lang-picker])
   #?(:cljs [freememo.formula-ui :as formula-ui])
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

;; Quill 2.0.3 registers no blot for `hr`, so `clipboard.convert` drops every
;; horizontal rule a stored page holds — 1878 rows across the database carry
;; one. This is the ONE construct `freememo.doc-html` cannot answer at the
;; import seam: Quill discards the tag whatever the database stores, so the
;; only fix is to give Delta a representation for it.
;;
;; A `tagName` of "HR" is sufficient on its own. Parchment's registry maps the
;; tag back to this blot, so the default clipboard walker finds it and NO
;; matcher is needed (measured: `<p>a</p><hr><p>b</p>` round-trips
;; byte-identically, and all 6 rules on a real imported page survive both a
;; reload and a user edit).
;;
;; It lives here, at namespace load, because this ns already owns the Quill
;; globals both editors share — the icon override above is the precedent, and
;; `install-clipboard-matchers!` below is called by rich_text_editor.cljc too.
;; Quill arrives on an eager <script> tag, so `js/Quill` exists at load.
;;
;; There is deliberately no toolbar button and no key chord. Import is the only
;; source of a divider, which is the same argument that leaves the formula blot
;; without an insert affordance.
;;
;; `js/Reflect.construct` is required and not incidental. Quill's BlockEmbed is
;; an ES6 class, ClojureScript emits ES5-style constructors, and an ES5
;; constructor cannot call an ES6 one — `(.call BlockEmbed this …)` throws
;; "Class constructor cannot be invoked without 'new'", the same failure
;; CLAUDE.md records for `new` in a call position. Reflect.construct builds the
;; instance through BlockEmbed while keeping Divider as the new.target, so the
;; result gets Divider's prototype. The two setPrototypeOf calls give Divider
;; BlockEmbed's instance methods and its statics, which is how Parchment finds
;; `create`, `value` and `length`.
;; `letfn` rather than `let`, so the constructor can name itself as new.target.
;; `arguments.callee` would be the other way to reach it and is illegal in the
;; strict-mode output Closure emits.
;;
;; **`aset` for the two statics, never `set!` on a dot-property.** Parchment
;; reads `blotName` off the class by that literal name, and `blotName` appears
;; in no Closure extern, so `(set! (.-blotName Divider) "divider")` is a write
;; Closure believes nothing reads — under `:advanced` it DELETED the assignment
;; outright. The emitted bundle carried
;; `x.tagName="HR";Quill.register(x)` with no blotName, so `register` saw a
;; class with neither `blotName` nor `attrName` and registered nothing: every
;; `<hr>` dropped in production while dev worked. `tagName` survived only
;; because it happens to be a DOM extern.
;;
;; String-keyed access cannot be renamed or elided, which is exactly why the
;; icon override above writes `(aset icons "code-block" …)`. Verify a change
;; here by grepping the `:advanced` bundle for `blotName` — a successful build
;; proves nothing.
#?(:cljs
   (defonce ^:private divider-blot-registered?
     (let [BlockEmbed (.import js/Quill "blots/block/embed")]
       (letfn [(Divider [scroll node]
                 (js/Reflect.construct BlockEmbed #js [scroll node] Divider))]
         (js/Object.setPrototypeOf (.-prototype Divider) (.-prototype BlockEmbed))
         (js/Object.setPrototypeOf Divider BlockEmbed)
         (aset Divider "blotName" "divider")
         (aset Divider "tagName" "HR")
         (.register js/Quill Divider)
         true))))

;; An audio clip — `<audio controls src="/api/media/<id>">`, plus `loop` when
;; that clip loops. Quill 2.0.3 registers no audio blot, so without this one
;; `clipboard.convert` drops the element on load and the first keystroke writes
;; the flattened DOM back over the stored clip. `freememo.audio-clip` owns the
;; stored shape and `html-cleaner`'s safelist admits it; all three must agree.
;;
;; The VALUE carries src and loop together, rather than the src alone with loop
;; as a Parchment format. Two reasons. A value that dropped `loop` would lose
;; the user's setting on every reload, which is the whole feature. And an embed
;; whose value is a map needs no format plumbing: the loop toggle replaces the
;; embed with `updateContents` delete+insert, the shape `editor-actions`
;; already uses to swap an image's src after its upload.
;;
;; `create` builds the element instead of calling super. Parchment's own
;; `create` is `document.createElement(this.tagName)` plus an optional
;; className this blot does not set, and an ES5 constructor calling into an ES6
;; class is the `new`-in-call-position hazard CLAUDE.md records.
;;
;; `aset` for every static, never `set!` — see the divider comment above.
;; `bundle-check/verify!` asserts that all four reached the `:advanced` bundle.
#?(:cljs
   (defonce ^:private audio-clip-blot-registered?
     (let [BlockEmbed (.import js/Quill "blots/block/embed")]
       (letfn [(AudioClip [scroll node]
                 (js/Reflect.construct BlockEmbed #js [scroll node] AudioClip))]
         (js/Object.setPrototypeOf (.-prototype AudioClip) (.-prototype BlockEmbed))
         (js/Object.setPrototypeOf AudioClip BlockEmbed)
         (aset AudioClip "blotName" "audio-clip")
         (aset AudioClip "tagName" "AUDIO")
         (aset AudioClip "create"
           (fn [value]
             (let [node (js/document.createElement "audio")
                   v (if (string? value) #js {:src value} value)
                   src (aget v "src")]
               (.setAttribute node "controls" "")
               (when (seq (str src)) (.setAttribute node "src" (str src)))
               (when (aget v "loop") (.setAttribute node "loop" ""))
               node)))
         (aset AudioClip "value"
           (fn [node]
             #js {:src (.getAttribute node "src")
                  :loop (.hasAttribute node "loop")}))
         (.register js/Quill AudioClip)
         true))))

;; Quill 2.0.3 models no `id`, so `clipboard.convert` drops it from every
;; element — measured on h1-h6, p, span and a. That makes an in-document anchor
;; a link to nothing the moment the document is edited: `clean-html` now stores
;; the attribute, but the first keystroke rewrites the flattened DOM back.
;;
;; A Parchment Attributor gives the Delta somewhere to keep it. BLOCK scope is
;; what works: `{"insert":"Intro\n","attributes":{"id":"intro","header":2}}`
;; round-trips on h1-h6 and p. An INLINE-scoped one does NOT rescue `<span id>`
;; or `<a id>` — those tags have no blot to hang an attribute on — which is why
;; `freememo.html-cleaner` allow-lists `id` on the block tags only. The two
;; halves must agree; widening one without the other stores a target that
;; cannot survive an edit.
;;
;; Splitting a block copies its `id` to both halves, so a heading cut in two
;; yields a duplicate id and `getElementById` finds the first. That is accepted
;; rather than de-duplicated: the anchor still lands on the right heading's
;; first half, and a save-time sweep would have to renumber ids the document's
;; own links point at.
;;
;; The blot above needs `aset` because Parchment READS `blotName` off a class we
;; WRITE, and Closure deletes a write it believes nothing reads. Nothing here is
;; that shape: `attrName` and `keyName` are written by Parchment's own
;; constructor, inside vendored JavaScript Closure never compiles, and our own
;; accesses are reads on a value from an unknown global. Closure canonicalises
;; the string keys below back to dot access and keeps the names — the emitted
;; bundle carries `new Bjb.Attributor("id","id",{scope:Bjb.Scope.BLOCK})`.
;; They stay string-keyed anyway, because that is the form this file's divider
;; comment argues for and it costs nothing. `bundle-check/verify!` asserts the
;; emitted names either way.
#?(:cljs
   (defonce ^:private block-id-attributor-registered?
     (let [Parchment (.import js/Quill "parchment")
           Attributor (aget Parchment "Attributor")
           block-scope (aget (aget Parchment "Scope") "BLOCK")]
       (.register js/Quill
         (new Attributor "id" "id" #js {:scope block-scope})
         true)
       true)))

(defn quill-config
  "Returns the Quill constructor options map (passed via clj->js).
   Bubble theme with the SAME modules as the main document editor
   (freememo.rich-text-editor) so a card field offers the identical formatting
   surface via the custom format-menu, including code-block highlight.js
   colouring and tables. Image insertion and cloze deletion are provided by
   format-menu opts in init-quill-field! (not the Quill toolbar), so the hidden
   bubble toolbar carries neither.
   `extra` is merged in last, letting a caller add ctor options this ns has no
   use for — e.g. rich_text_editor.cljc passes {:bounds container} to clamp its
   floating tooltip to the editor pane; QuillField has no such ancestor to clamp
   to and passes nil.

   Nothing here is conditional on KaTeX. Math needs no module and no toolbar
   entry — see the note inside :modules below — so the callers' `math?` never
   reaches this fn."
  [placeholder extra]
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
                ;; NO math entry belongs in this map, in either half of it.
                ;;
                ;; Quill 2 has no `modules/formula`. The registered module names
                ;; are clipboard, history, input, keyboard, syntax, table,
                ;; toolbar, uiNode and uploader; formula is a FORMAT there, and
                ;; `formats/formula` is registered when quill.js loads. So the
                ;; Formula blot resolves for the typed-source converter and the
                ;; formula-ui popover with nothing asked for here.
                ;;
                ;; A `:formula` key does not fail loudly. expandConfig logs
                ;; `Cannot import modules/formula` plus `Cannot load formula
                ;; module` and then DROPS the entry, so one arrived with math
                ;; support and printed two lines per editor for its whole life —
                ;; six pairs on the occlusion modal's Fields tab alone.
                ;;
                ;; A "formula" toolbar entry is just as pointless: both editors
                ;; use the bubble theme and index.css hides
                ;; `.ql-bubble .ql-tooltip`, so the button it renders is
                ;; invisible. Formulas are created by typing `\(…\)` and edited
                ;; through freememo.formula-ui's popover.
                ;;
                ;; Syntax module — requires window.hljs + the clojure and zig packs (loaded in
                ;; index*.html). `code-languages` is the shared vocabulary —
                ;; the main editor builds from this same fn, so both editors and
                ;; the Apply-language command offer one list.
                :syntax {:languages code-languages}
                :table true
                ;; Drag-to-resize images. quill-resize-module 2.0.3+ is loaded
                ;; from /freememo/vendor/ in index*.html and self-registers `modules/resize`
                ;; on script load, so there is no Quill.register call anywhere.
                ;;
                ;; `:modules` omits the module's own "Toolbar" (its align
                ;; buttons write `ql-resize-style` classes, which `clean-html`
                ;; strips from <img> — so alignment would persist in topic
                ;; content and vanish on cards). "DisplaySize" is the live
                ;; WxH readout.
                ;;
                ;; "Keyboard" is arrow-key caret movement past a selected image
                ;; plus Backspace/Delete to remove it — AND a keydown listener on
                ;; `document` in the CAPTURE phase, registered for as long as the
                ;; module holds a selection. That listener is a focus thief for
                ;; the whole page; `install-resize-release-on-focus-out!` below is
                ;; what bounds it. Dropping "Keyboard" from this vector would
                ;; remove the listener outright, at the cost of both editing
                ;; affordances — the reason it is still here.
                ;;
                ;; `:embedTags []` skips initializeEmbed!, which otherwise
                ;; injects a <style> node and a pointer-events class for
                ;; VIDEO/IFRAME blots this editor never contains.
                ;;
                ;; `:attribute ["width"]` is the whole persistence contract:
                ;; Quill 2.0.3's image blot keeps only alt/height/width, and
                ;; `clean-html` allow-lists the same, so a width ATTRIBUTE is
                ;; the only size representation that survives a reload or the
                ;; card path. The module writes it as a bare integer and
                ;; clears inline style. `.ql-editor img{height:auto}` would
                ;; override any height, so height is deliberately not written.
                ;;
                ;; `:minWidth 40` (module default 100) — handles are withheld
                ;; below this rendered width. Wikipedia infobox thumbnails
                ;; render ~50px and must stay resizable; 20px marker icons
                ;; must not, since four 12px handles would swallow them.
                :resize {:modules ["DisplaySize" "Resize" "Keyboard"]
                         :embedTags []
                         :parchment {:image {:attribute ["width"]
                                             :limit {:minWidth 40}}}}}
      :placeholder (or placeholder "Enter text...")}
     extra))

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

(defn install-cmd-y-redo!
  "Add Cmd-Y and Ctrl-Y as Quill redo shortcuts."
  [ed]
  #?(:cljs
     (when ed
       (let [^js ed ed]
         (.addBinding (.-keyboard ed)
           #js {:key "y" :shortKey true}
           (fn []
             (.redo (.-history ed))
             false))))
     :clj nil))

;; ---------------------------------------------------------------------------
;; Resize-module focus containment — shared by both editors, like the clipboard
;; matchers above.
;; ---------------------------------------------------------------------------

(defn- release-resize-selection!
  "Drop the resize module's hold on the blot it has selected, if it holds one.
   Private: the installer below is the only sanctioned caller, because a release
   that is not tied to focus leaving is the per-opener discipline this replaced.

   `hide()` removes the overlay, clears `activeEle`, and calls `onDestroy` on the
   module's sub-modules — which is what unregisters the document keydown listener
   described beside `:resize` in `quill-config`.

   Pre  : `ed` is a Quill instance or nil.
   Post : that editor's resize module holds nothing and owns no document listener.
          Idempotent, and a no-op when nothing is selected."
  [ed]
  #?(:cljs
     (when ed
       (let [^js ed ed
             ^js resizer (.-resizer ed)]
         (when (and resizer (.-activeEle resizer))
           (.hide resizer))))
     :clj nil))

(defn install-resize-release-on-focus-out!
  "Release `ed`'s resize selection as soon as focus leaves its container. Returns
   a teardown fn removing the listener.

   This is the whole defense against the resize module's document-level keydown
   listener, and it belongs here rather than at each site that opens something
   over an editor. While the module holds a selection it calls
   `quill.setSelection` from that listener, which focuses this editor's root — so
   the FIRST keystroke on any other surface is stolen. Capture-phase-on-document
   runs before the real target, so the surface being typed into cannot defend
   itself; its own `stopPropagation` is already too late. Releasing on focus-out
   is early enough for every case, because focus always moves before a keystroke
   arrives.

   Two details make the guard exact. `relatedTarget` is the element RECEIVING
   focus, and a resize handle cannot be it: the module appends its overlay to
   `quill.container` (`this.quill.container.appendChild(this.overlay)`), so a
   handle click stays inside and is filtered out by the containment test. And a
   nil `relatedTarget` — focus falling to `<body>` — is deliberately left alone:
   nothing is typing there, so the selection is still useful, and hiding on it
   would drop a selection the user can still see.

   Pre  : `ed` is a Quill instance whose container is mounted.
   Post : no keystroke aimed at an element outside `ed`'s container can move focus
          into `ed`.
   Blame: a stolen keystroke means this was never installed, or its teardown ran
          early."
  [ed]
  #?(:cljs
     (let [^js ed ed
           ^js container (.-container ed)
           handler (fn [^js e]
                     (let [^js next (.-relatedTarget e)]
                       (when (and next (not (.contains container next)))
                         (release-resize-selection! ed))))]
       (.addEventListener container "focusout" handler)
       (fn [] (.removeEventListener container "focusout" handler)))
     :clj nil))

;; ---------------------------------------------------------------------------
;; Quill instance lifecycle — plain defns so side effects are stable.
;; ---------------------------------------------------------------------------

(defn editor-html
  "The editor's content in STORED form — the single read accessor for every
   caller that persists or compares Quill's HTML.

   Quill renders math as `span.ql-formula` with a KaTeX subtree inside it; the
   stored form is `\\(TeX\\)` text. Reading `.root.innerHTML` directly would
   persist the rendered subtree, which `clean-html` then strips down to KaTeX's
   duplicated fallback text. Every read goes through here so that cannot happen
   at one site and not another.

   Pre  : `ed` is a Quill instance or nil.
   Post : stored-form HTML, or nil iff `ed` is nil."
  [ed]
  #?(:cljs (when ed (math/editor->stored-html (.-innerHTML (.-root ed))))
     :clj nil))

(defn init-quill-field!
  "Initialize a standalone Quill editor (bubble theme) in container with
   initial-html. Wires text-change → on-change on each user edit, a clipboard
   matcher that uploads data-URI images, and the custom format-menu +
   code-lang-picker (the same components as the main document editor). When
   `cloze?` is true the format-menu gains the cloze-deletion row and the two
   cloze chords are bound on this editor's keyboard module. When `math?`
   is true (KaTeX present — see freememo.math/on-katex-ready!) stored `\\(TeX\\)`
   is promoted to formula embeds on load and the formula popover is installed.
   When it is false the delimiters are deliberately left as text so a save cannot
   lose them. It reaches neither quill-config nor the Quill options: math needs no
   module entry, which the note inside quill-config's :modules map explains.
   Returns {:editor <Quill> :teardowns [fn ...]} (CLJS) or nil (CLJ); the
   teardowns remove the format-menu / code-lang picker / formula listeners on
   destroy.
   No ^js on parameters — CLJ compiler sees the parameter list."
  [container initial-html placeholder on-change cloze? math?]
  #?(:cljs
     (when (and container (.-Quill js/window))
       (let [Quill (.-Quill js/window)
             cfg (clj->js (quill-config placeholder nil))
             ed (new Quill container cfg)
             _redo (install-cmd-y-redo! ed)
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
             ;; Stored `\(TeX\)` → span.ql-formula[data-value], which Parchment
             ;; resolves to Quill's Formula blot during convert. Skipped without
             ;; KaTeX: an unresolvable span would lose its data-value on save.
             for-editor (if math? (math/stored->editor-html cleaned) cleaned)
             delta (.convert cb (clj->js {:html for-editor}))]
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
               (on-change (editor-html ed)))))
         ;; Pasted/dropped images → /api/media instead of inline base64. This
         ;; REPLACES a clipboard matcher that could never fire: Quill routes any
         ;; clipboard payload carrying files straight to uploader.upload, and on
         ;; the html path matchBlot has already read the src into the delta
         ;; before an appended matcher runs. See install-attachment-upload!.
         (editor-actions/install-attachment-upload! ed)
         ;; Cmd/Ctrl+\\ mirrors the custom format menu's Clear formatting
         ;; action for a selected range. It is editor-local, so it never reaches
         ;; another contenteditable surface through the global chord handler.
         (editor-actions/install-clear-format-binding! ed)
         ;; Convert Markdown markers as the user types. This local extension
         ;; targets Quill 2 and shares the editor's teardown lifecycle.
         (let [markdown-teardown (quill-markdown/install! ed)]
           ;; Wire the table toolbar handler and the contextual action bar.
           ;; The QuillField toolbar includes "table", so it needs this handler.
           (table-ui/init! ed)
           ;; The custom menu replaces the hidden Bubble tooltip.
           (a11y/label-quill-toolbar! (.-parentElement container))
           ;; Tab moves focus out. Escape also blurs the editor.
           (a11y/free-quill-tab! ed)
           (a11y/install-quill-tab-escape! ed)
           ;; Add cloze shortcuts only to a cloze field.
           (when cloze? (editor-actions/install-cloze-bindings! ed))
           {:editor ed
            :teardowns [markdown-teardown
                        (format-menu/install! ed {:image? true
                                                  :audio? true
                                                  :cloze? (boolean cloze?)})
                        (code-lang-picker/install! ed)
                        ;; Right-click an audio clip to toggle its loop setting.
                        ;; The image context menu is installed by editor-pane on
                        ;; the document editor alone; a card field holds clips
                        ;; too, so this one is installed per editor.
                        (editor-actions/install-clip-menu! ed)
                        ;; Release the resize module when focus leaves the editor.
                        (install-resize-release-on-focus-out! ed)
                        ;; Install the formula popover only when KaTeX is ready.
                        (when math? (formula-ui/install! ed))]})))
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
  "Schedule editor initialization and return its cleanup function.
   Cleanup cancels the timer and ignores a pending KaTeX callback.
   Keep the reader conditional outside the Electric body to preserve peer signal counts."
  [!ed-state container value-string placeholder on-change !editor-atom cloze? autofocus?]
  #?(:cljs
     (let [!active? (atom true)
           timer (js/setTimeout
                   (fn []
                     (when (and @!active? (.-isConnected container))
                       (math/on-katex-ready!
                         (fn [math?]
                           ;; A type switch can remove this field while KaTeX loads.
                           (when (and @!active? (.-isConnected container))
                             (let [result (init-quill-field! container value-string placeholder on-change cloze? math?)
                                   ^js ed (:editor result)]
                               (reset! !ed-state result)
                               (when !editor-atom (reset! !editor-atom ed))
                               (when (and autofocus? ed (.-isConnected container))
                                 (.focus ed))))))))
                   0)]
       (fn []
         (reset! !active? false)
         (js/clearTimeout timer)
         (let [state @!ed-state
               ed (:editor state)]
           (destroy-quill-field! state)
           (reset! !ed-state nil)
           ;; An old field must not clear the replacement field's editor.
           (when (and !editor-atom (identical? ed @!editor-atom))
             (reset! !editor-atom nil)))))
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
          `.ql-code-block-container`; returned string is the editor's content in
          stored form after the flush (see `editor-html`).
   Inv  : returns nil iff `ed` is nil; never throws on nil input."
  [ed]
  #?(:cljs
     (when ed
       (let [^js ed ed
             ^js syntax (.getModule ed "syntax")]
         (when syntax (.highlight syntax))
         (editor-html ed)))
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
                             editor-actions/insert-cloze!, and the two cloze
                             chords (editor-actions/cloze-chords) are bound on
                             this editor. Used by the card modals' cloze field
                             only.
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
              seed (e/snapshot value-string)
              ;; Keep the client reader conditional inside the plain function.
              cleanup! (schedule-quill-init! !ed-state dom/node seed placeholder on-change !editor-atom cloze? autofocus?)]
          (e/on-unmount cleanup!))))))
