(ns freememo.quill-table-ui
  "Quill table authoring UI — toolbar handler + contextual action bar.

   The action bar (`.ql-table-actions`) lives in `document.body` and is
   positioned absolutely against the editor wrapper's top-right corner.
   Anchoring to the body decouples the bar from `.ql-container` ancestry
   (Quill mutates the DOM in ways that vary by call site: in some setups
   `ed.container` IS the `.quill-editor-wrapper`, in others it is the
   inner `.ql-container` child). Positioning math uses
   `getBoundingClientRect` plus `window.scrollX/Y` so the bar tracks the
   wrapper through page scroll and window resize.")

;; Per-editor cleanup registry. Each Quill instance is its own key so
;; multiple editors (main editor + N card-edit fields) coexist without
;; colliding. WeakMap entries are eligible for GC if an editor is dropped
;; without `teardown!` — an accidental leak doesn't pin the closure
;; (and its captured listeners/bar node) forever.
#?(:cljs (defonce ^:private cleanup-by-editor (js/WeakMap.)))

(defn- find-wrapper
  "Resolve the editor's `.quill-editor-wrapper` element.

   QuillField passes the wrapper element itself as Quill's constructor
   container — Quill mutates it in place to also carry `.ql-container`.
   `rich_text_editor`'s main editor passes an inner element whose
   parent is the wrapper. This handles both, falling back to the parent
   so we always have a containing block to anchor against."
  [editor]
  #?(:clj nil
     :cljs
     (let [^js ed editor
           ^js inner (.-container ed)
           ^js parent (when inner (.-parentNode inner))]
       (cond
         (and inner (.contains (.-classList inner) "quill-editor-wrapper")) inner
         (and parent (.contains (.-classList parent) "quill-editor-wrapper")) parent
         :else parent))))

(defn- position-bar!
  "Place `bar` so its top-right corner aligns with `wrapper`'s top-right
   corner in document coordinates. Caller MUST ensure the bar is
   currently displayed (the `.visible` class is on) before invoking —
   `offsetWidth` is 0 for `display:none` elements and the alignment math
   would collapse."
  [bar wrapper]
  #?(:clj nil
     :cljs
     (let [^js w wrapper
           ^js b bar
           rect (.getBoundingClientRect w)
           bw (.-offsetWidth b)
           left-doc (- (+ (.-right rect) (.-scrollX js/window)) bw)
           top-doc (+ (.-top rect) (.-scrollY js/window))]
       (set! (.. b -style -position) "absolute")
       ;; "auto" (not "") so the CSS rule `.ql-table-actions { right: var(--sp-2) }`
       ;; is overridden, not just unset. Otherwise inline left + cascade right
       ;; both apply, stretching the bar; each repositioning reads the stretched
       ;; offsetWidth and drifts left by the gap on every call.
       (set! (.. b -style -right) "auto")
       (set! (.. b -style -bottom) "")
       (set! (.. b -style -left) (str left-doc "px"))
       (set! (.. b -style -top) (str top-doc "px")))))

(defn- build-action-bar!
  "Create the floating div with buttons wired to the Quill table module.
   `on-delete-table` is invoked synchronously after `deleteTable` so the
   caller can hide the bar immediately — before any `text-change`
   recheck has a chance to race the cursor's new position."
  [editor on-delete-table]
  #?(:clj nil
     :cljs
     (let [^js ed editor
           bar (js/document.createElement "div")
           actions [["Row Above" "insertRowAbove" false]
                    ["Row Below" "insertRowBelow" false]
                    ["Col Left" "insertColumnLeft" false]
                    ["Col Right" "insertColumnRight" false]
                    ["Del Row" "deleteRow" false]
                    ["Del Col" "deleteColumn" false]
                    ["Del Table" "deleteTable" true]]]
       (set! (.-className bar) "ql-table-actions")
       (doseq [[label method del?] actions]
         (let [btn (js/document.createElement "button")]
           (set! (.-type btn) "button")
           (set! (.-textContent btn) label)
           ;; mousedown.preventDefault keeps editor focus so the table
           ;; module sees the current cell when the click handler runs.
           (.addEventListener btn "mousedown"
             (fn [^js e] (.preventDefault e)))
           (.addEventListener btn "click"
             (fn [^js e]
               (.preventDefault e)
               (when-let [^js table-mod (.getModule ed "table")]
                 (js-invoke table-mod method)
                 (when del? (on-delete-table)))))
           (.appendChild bar btn)))
       bar)))

(defn init!
  "Wire the Quill table toolbar handler and the contextual action bar.

   Side effects:
     - Adds the toolbar `table` handler (3x3 insertion).
     - Appends `.ql-table-actions` to `document.body`.
     - Registers `selection-change` and `text-change` listeners on
       `editor` — the latter handles staleness (cursor leaves the table
       via cut/keyboard-delete, or returns to the table via undo).
     - Registers `window` `scroll` (capture) and `resize` listeners to
       reposition the bar while it is visible.

   Returns a 0-arity cleanup function (CLJS) or nil (CLJ). The cleanup
   is ALSO recorded in `cleanup-by-editor` keyed by the editor — callers
   that retain only the editor instance should invoke `teardown!` to
   release everything."
  [editor]
  #?(:clj nil
     :cljs
     (let [^js ed editor
           ^js toolbar (.getModule ed "toolbar")
           ^js table-mod (.getModule ed "table")
           ^js wrapper (find-wrapper ed)]
       (when (and wrapper toolbar table-mod)
         (.addHandler toolbar "table"
           (fn [_] (.insertTable table-mod 3 3)))
         (let [!bar (volatile! nil)
               hide-now! (fn []
                           (when-let [^js b @!bar]
                             (.remove (.-classList b) "visible")))
               bar (build-action-bar! ed hide-now!)
               _ (vreset! !bar bar)
               _ (.appendChild js/document.body bar)
               recheck!
               (fn []
                 (let [^js range (.getSelection ed)
                       ^js info (when range (.getTable table-mod range))]
                   (if (and info (aget info 0))
                     (do (.add (.-classList bar) "visible")
                         (position-bar! bar wrapper))
                     (.remove (.-classList bar) "visible"))))
               selection-handler (fn [_range _old _source] (recheck!))
               ;; text-change recheck handles two staleness paths:
               ;;   1. cursor leaves the table via cut / keyboard delete
               ;;      → bar hides without a separate selection-change
               ;;   2. undo (Ctrl+Z) restores a deleted table at the
               ;;      cursor → bar re-shows even though selection didn't
               ;;      move.
               text-handler (fn [_delta _old _source] (recheck!))
               window-handler
               (fn [_e]
                 (when (.contains (.-classList bar) "visible")
                   (position-bar! bar wrapper)))]
           (.on ed "selection-change" selection-handler)
           (.on ed "text-change" text-handler)
           (.addEventListener js/window "scroll" window-handler true)
           (.addEventListener js/window "resize" window-handler false)
           (let [cleanup
                 (fn []
                   (.off ed "selection-change" selection-handler)
                   (.off ed "text-change" text-handler)
                   (.removeEventListener js/window "scroll" window-handler true)
                   (.removeEventListener js/window "resize" window-handler false)
                   (when (.-parentNode bar) (.remove bar)))]
             (.set cleanup-by-editor ed cleanup)
             cleanup))))))

(defn teardown!
  "Run the cleanup stored on `editor` by `init!`. Idempotent — safe to
   call without a prior `init!` (e.g. when teardown is wired
   defensively in a destroy path that may run before init or twice)."
  [editor]
  #?(:clj nil
     :cljs
     (when editor
       (when-let [cleanup (.get cleanup-by-editor editor)]
         (cleanup)
         (.delete cleanup-by-editor editor)))))
