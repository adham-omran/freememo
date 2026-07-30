(ns freememo.formula-ui
  "Formula authoring UI for both Quill editors: an anchored edit popover, plus the
   typed-source conversion that creates formulas in the first place.

   There is no insert dialog and no insert button. Typing `\\(…\\)` is the only way
   to create a formula — `editor-actions/install-typed-formula-conversion!` renders
   it the moment the closing delimiter completes a region. Clicking a rendered
   formula opens the popover: a LaTeX field, a live KaTeX preview, and Delete.

   The popover lives on `document.body`, NOT inside the editor. Injecting a node
   into Quill's subtree fights its DOM reconciliation (see code_lang_picker for the
   same reasoning), and `position: fixed` on the body avoids depending on which
   element Quill ended up treating as its container.

   Two deliberate departures from the other four anchored popovers in this app,
   both from `plans/math-support.md` round two — do not \"fix\" them:

   - **It prefers ABOVE the anchor.** `format_menu` and `code_lang_picker` both
     prefer below.
   - **An outside click COMMITS.** The others all dismiss without acting; this one
     holds a text field, and Forms5 fields commit on blur. That is also why scroll
     REPOSITIONS rather than dismissing: an incidental scroll must not silently
     commit half-typed TeX.

   install! wires everything and returns one teardown fn."
  (:require [freememo.editor-actions :as editor-actions]
            [freememo.math :as math]))

(defn- el [tag class]
  (let [n (js/document.createElement tag)]
    (set! (.-className n) class)
    n))

(defn- render-preview!
  "Typeset `tex` into `node`.

   `math/render-math!` is the wrong tool here — it auto-renders delimiters found in
   text, and this is a bare TeX body. Options match Quill's own `Formula` blot, so
   the preview and the committed formula fail the same way on bad input:
   `throwOnError:false` paints KaTeX's message inline instead of blanking.

   pre : `node` is in the document; `tex` is a string. post: `node` shows `tex`
   typeset, or KaTeX's inline error."
  [node tex]
  (when-let [^js katex (.-katex js/window)]
    (try
      (.render katex tex node #js {:throwOnError false :errorColor "#f00"})
      (catch :default _
        ;; Defensive: throwOnError:false covers parse errors, not every failure.
        (set! (.-textContent node) tex)))))

(defn install!
  "Wire the formula popover and the typed-source converter for `editor`.
   Returns a teardown fn removing both.

   Pre:  `editor` is an initialized Quill instance whose formula module is enabled
         — i.e. KaTeX is present (`freememo.math/on-katex-ready!` resolved true).
         Callers MUST NOT install this otherwise: `Formula.create` throws without
         KaTeX.
   Post: clicking a rendered formula opens a popover anchored above it, prefilled
         with its TeX; Enter or an outside click commits, Escape cancels, Delete
         removes it, and an empty commit removes it too. Every exit returns focus
         to the editor with a defined caret. Typing `\\(…\\)` outside code renders
         on the closing delimiter. Teardown leaves no node and no listener.
   Inv:  one popover per editor instance — class-only, no `id` — because a
         card-edit modal's Quill coexists with the document editor's."
  [^js editor]
  (let [root      (.-root editor)
        popover   (el "div" "ql-formula-popover")
        input     (el "input" "ql-formula-input")
        preview   (el "div" "ql-formula-preview")
        actions   (el "div" "ql-formula-actions")
        delete-btn (el "button" "ql-formula-delete")
        ;; The anchored formula element, or nil when closed. The document index is
        ;; deliberately NOT cached — it is resolved from this element at exit time.
        !anchor   (atom nil)
        open?     #(some? @!anchor)
        tex-of    #(math/unwrap-tex (.-value input))
        place!    (fn []
                    (when-let [^js a @!anchor]
                      (let [r   (.getBoundingClientRect a)
                            m   8
                            gap 6
                            vw  (.-innerWidth js/window)
                            vh  (.-innerHeight js/window)
                            pw  (.-offsetWidth popover)
                            ph  (.-offsetHeight popover)
                            ;; Prefer above; fall below only when above would
                            ;; clear the viewport top.
                            above (- (.-top r) gap ph)
                            top   (if (>= above m)
                                    above
                                    (min (+ (.-bottom r) gap) (- vh ph m)))
                            ;; Centred on the formula, then clamped.
                            left  (max m (min (- (+ (.-left r) (/ (.-width r) 2)) (/ pw 2))
                                           (- vw pw m)))]
                        (set! (.. popover -style -top) (str (js/Math.round top) "px"))
                        (set! (.. popover -style -left) (str (js/Math.round left) "px")))))
        close!    (fn []
                    (set! (.. popover -style -display) "none")
                    (reset! !anchor nil))
        ;; Exits. Each restores the caret itself, so no path leaves focus stranded
        ;; on the popover after it hides.
        commit!   (fn []
                    (when-let [a @!anchor]
                      (editor-actions/replace-formula! editor a (tex-of)))
                    (close!))
        delete!   (fn []
                    (when-let [a @!anchor]
                      (editor-actions/replace-formula! editor a ""))
                    (close!))
        cancel!   (fn []
                    (when-let [a @!anchor]
                      (when-let [i (editor-actions/formula-caret-index editor a)]
                        (.setSelection editor i 0 "user")))
                    (close!))
        open!     (fn [^js a]
                    (reset! !anchor a)
                    (let [tex (or (.getAttribute a "data-value") "")]
                      (set! (.-value input) tex)
                      (render-preview! preview tex)
                      ;; Display before place! and focus: offsetWidth/Height read 0
                      ;; while hidden, and a hidden input cannot take focus.
                      (set! (.. popover -style -display) "flex")
                      (place!)
                      (.focus input)
                      ;; Caret at the end, NOT select-all — an edit is usually a
                      ;; character or two, and select-all makes the first keystroke
                      ;; destroy the formula.
                      (let [n (count tex)]
                        (.setSelectionRange input n n))))
        on-root-click (fn [^js e]
                        (when-let [^js a (some-> (.-target e) (.closest "span.ql-formula"))]
                          ;; isConnected: an outside-click commit replaces the blot
                          ;; before this click resolves, so re-clicking the formula
                          ;; that was just committed hands us a detached node.
                          (when (.-isConnected a)
                            (open! a))))
        on-input  (fn [_] (render-preview! preview (tex-of)))
        on-input-key (fn [^js e]
                       (case (.-key e)
                         "Enter" (do (.preventDefault e) (commit!))
                         "Escape" (do (.preventDefault e) (.stopPropagation e) (cancel!))
                         nil))
        reposition (fn [_] (when (open?) (place!)))
        on-doc-down (fn [^js e]
                      (when (and (open?) (not (.contains popover (.-target e))))
                        (commit!)))
        on-doc-key (fn [^js e]
                     (when (and (open?) (= "Escape" (.-key e))) (cancel!)))
        stop-conversion (editor-actions/install-typed-formula-conversion! editor)]
    (set! (.-type input) "text")
    (.setAttribute input "spellcheck" "false")
    (.setAttribute input "aria-label" "LaTeX")
    (.setAttribute input "placeholder" "LaTeX, e.g. x^2 + y^2")
    (set! (.-type delete-btn) "button")
    (set! (.-textContent delete-btn) "Delete")
    (.setAttribute delete-btn "aria-label" "Delete formula")
    (.appendChild actions delete-btn)
    (.appendChild popover input)
    (.appendChild popover preview)
    (.appendChild popover actions)
    (set! (.. popover -style -display) "none")
    (.appendChild js/document.body popover)
    (.addEventListener input "input" on-input)
    (.addEventListener input "keydown" on-input-key)
    (.addEventListener delete-btn "click" delete!)
    (.addEventListener root "click" on-root-click)
    (.addEventListener root "scroll" reposition false)
    (.addEventListener js/document "scroll" reposition true)  ; capture: outer-ancestor scroll
    (.addEventListener js/window "resize" reposition)
    (.addEventListener js/document "mousedown" on-doc-down true)
    (.addEventListener js/document "keydown" on-doc-key)
    (fn teardown []
      (stop-conversion)
      (.removeEventListener input "input" on-input)
      (.removeEventListener input "keydown" on-input-key)
      (.removeEventListener delete-btn "click" delete!)
      (.removeEventListener root "click" on-root-click)
      (.removeEventListener root "scroll" reposition false)
      (.removeEventListener js/document "scroll" reposition true)
      (.removeEventListener js/window "resize" reposition)
      (.removeEventListener js/document "mousedown" on-doc-down true)
      (.removeEventListener js/document "keydown" on-doc-key)
      (when (.-parentNode popover) (.remove popover)))))
