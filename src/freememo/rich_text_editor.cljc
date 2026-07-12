(ns freememo.rich-text-editor
  "Quill rich text editor integration — global singleton."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.logging :as log]
   [freememo.quill-table-ui :as table-ui]
   [freememo.a11y :as a11y]
   [clojure.string :as str]))

;; Global singleton — mirrors pdf_viewer.cljc pattern
(defonce !editor-state (atom nil))

;; Destination node for the relocated Quill toolbar, set by
;; freememo.editor-toolbar/EditorToolbar (the unified top-bar slot). nil when
;; that bar isn't mounted — init-editor! then leaves the toolbar in place.
;; Defined on both peers (no reader conditional) — referenced in an e/defn body.
(defonce !toolbar-node (atom nil))

;; Set by Import button click in Quill tooltip — picked up by e/Token in extract_page
;; Shape: {:url "https://en.wikipedia.org/wiki/..." :ts <timestamp>} or nil
(defonce !import-url (atom nil))

;; Import status feedback — set by Electric wiring, read by tooltip JS
;; :idle | :importing | :done | :already-exists | :error
(defonce !import-status (atom :idle))

;; Set to the current HTML on every user edit (immediate, no debounce).
;; nil means "no pending user edits" — the auto-save guard checks for non-nil.
;; Reset to nil on init/page-switch so navigating doesn't trigger a save.
;; Shape: {:html "..." :topic-id N}
(defonce !dirty-html (atom nil))

;; Most recent non-empty in-editor selection range. Used as fallback when
;; (.getSelection ed) returns null/empty — happens in Chrome when clicking
;; the toolbar Extract button focuses the button and blurs Quill before the
;; click handler reads selection. Shape: {:index N :length N} or nil.
(defonce !last-selection (atom nil))

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

(defn destroy-editor!
  "Destroy the current global editor instance (if any)."
  []
  #?(:clj nil
     :cljs
     (do
       (when-let [{:keys [editor container toolbar import-popover-teardown tooltip-teardown a11y-observer]} @!editor-state]
         (let [^js ed editor
               ^js ct container
               ^js tb toolbar]
           ;; Stop the content-a11y MutationObserver before the DOM is torn down.
           (when a11y-observer (.disconnect ^js a11y-observer))
           ;; Tear down the wiki-link import popover (element + document/window
           ;; listeners + status watch). Must run before innerHTML clear below.
           (when import-popover-teardown (import-popover-teardown))
           ;; Remove the bubble-tooltip fixed-positioning scroll/resize listeners.
           (when tooltip-teardown (tooltip-teardown))
           ;; Tear down the table action bar (appended to document.body)
           ;; and its window scroll/resize listeners. Idempotent; safe
           ;; even if table-ui/init! never ran.
           (table-ui/teardown! ed)
           (.off ed "text-change")
           (.off ed "selection-change")
           ;; Remove Quill's generated toolbar. It was relocated into the
           ;; unified top bar (!toolbar-node), so query by the stored ref, not
           ;; from the container's parent. Guard on isConnected — EditorToolbar's
           ;; own unmount may have already removed it with its host div.
           (when (and tb (.-isConnected tb))
             (.remove tb))
           (when ct
             (set! (.-innerHTML ct) "")))
         (reset! !editor-state nil)
         (reset! !last-selection nil)))))

(defn- wikipedia-url? [href]
  #?(:cljs (and (string? href)
             (or (.includes href "wikipedia.org/wiki/")
               (.includes href "wikipedia.org/w/")))
     :clj false))

(defn- wiki-slug
  "Human-readable article title decoded from a wikipedia /wiki/<slug> href."
  [href]
  #?(:cljs
     (try
       (if-let [m (re-find #"/wiki/([^#?]+)" (str href))]
         (-> (second m) js/decodeURIComponent (str/replace "_" " "))
         "")
       (catch :default _ ""))
     :clj ""))

(defn- setup-link-import-popover!
  "Standalone, link-anchored Import popover for Wikipedia links in the article.
   A click on an internal wiki <a> shows a small popover anchored to the link;
   clicking Import drives the existing !import-url pipeline. Decoupled from
   Quill's bubble tooltip (which only appears on a text selection, so the old
   tooltip-button affordance was invisible on a plain link click). Body-anchored
   + position:fixed to avoid positioned-ancestor assumptions. Returns a teardown
   fn that removes the element, document/window listeners, and the status watch."
  [editor]
  #?(:cljs
     (let [^js ed   editor
           ^js root (.-root ed)                 ; the .ql-editor content element
           ^js pop  (js/document.createElement "div")
           ^js label (js/document.createElement "span")
           ^js btn  (js/document.createElement "button")
           !href    (atom nil)
           visible? (fn [] (not= "none" (.. pop -style -display)))
           hide!    (fn [] (set! (.. pop -style -display) "none"))
           wiki-anchor (fn [^js target]
                         (let [^js a (when (and target (.-closest target)) (.closest target "a"))]
                           (when (and a (wikipedia-url? (.getAttribute a "href"))) a)))
           show-at! (fn [^js a href]
                      (reset! !href href)
                      (set! (.-textContent label) (wiki-slug href))
                      (set! (.-textContent btn) "Import")
                      (.remove (.-classList btn) "importing")
                      (let [r (.getBoundingClientRect a)]
                        (set! (.. pop -style -top) (str (+ (.-bottom r) 4) "px"))
                        (set! (.. pop -style -left) (str (.-left r) "px"))
                        (set! (.. pop -style -display) "flex")))
           on-link-click (fn [e]
                           (when-let [^js a (wiki-anchor (.-target e))]
                             (show-at! a (.getAttribute a "href"))))
           on-doc-click (fn [e]
                          (when (and (visible?)
                                  (not (.contains pop (.-target e)))
                                  (not (wiki-anchor (.-target e))))
                            (hide!)))
           on-key    (fn [e] (when (and (= "Escape" (.-key e)) (visible?)) (hide!)))
           on-scroll (fn [_] (when (visible?) (hide!)))]
       (set! (.-className pop) "wiki-import-popover")
       (.setAttribute pop "style"
         (str "position:fixed;display:none;z-index:1000;align-items:center;gap:8px;"
           "padding:6px 8px;background:var(--color-bg-card);"
           "border:1px solid var(--color-border);border-radius:var(--radius-sm);"
           "box-shadow:0 2px 8px rgba(0,0,0,0.18);font-size:13px;max-width:320px;"))
       (.setAttribute label "style"
         "overflow:hidden;text-overflow:ellipsis;white-space:nowrap;max-width:200px;color:var(--color-text-secondary);")
       (set! (.-className btn) "btn btn-sm btn-primary")
       (set! (.-type btn) "button")
       (set! (.-textContent btn) "Import")
       (.appendChild pop label)
       (.appendChild pop btn)
       (.appendChild (.-body js/document) pop)
       ;; Import → existing !import-url pipeline.
       (.addEventListener btn "click"
         (fn [e]
           (.preventDefault e)
           (when-not (= @!import-status :importing)
             (let [href @!href]
               (when (wikipedia-url? href)
                 (reset! !import-status :importing)
                 (set! (.-textContent btn) "Importing...")
                 (.add (.-classList btn) "importing")
                 (reset! !import-url {:url href :ts (js/Date.now)}))))))
       ;; Status → button text (same states as the old tooltip button).
       (add-watch !import-status ::import-popover-btn
         (fn [_ _ _ st]
           (case st
             :importing      (do (set! (.-textContent btn) "Importing...") (.add (.-classList btn) "importing"))
             :done           (do (set! (.-textContent btn) "Imported!") (.remove (.-classList btn) "importing")
                                  (js/setTimeout (fn [] (hide!) (reset! !import-status :idle)) 1500))
             :already-exists (do (set! (.-textContent btn) "Already imported") (.remove (.-classList btn) "importing")
                                  (js/setTimeout (fn [] (hide!) (reset! !import-status :idle)) 1500))
             :error          (do (set! (.-textContent btn) "Error") (.remove (.-classList btn) "importing")
                                  (js/setTimeout (fn [] (reset! !import-status :idle)) 2000))
             nil)))
       (.addEventListener root "click" on-link-click)
       (.addEventListener js/document "click" on-doc-click)
       (.addEventListener js/document "keydown" on-key)
       (.addEventListener js/document "scroll" on-scroll true) ; capture: catch editor-pane scroll
       (.addEventListener js/window "resize" on-scroll)
       (fn teardown []
         (remove-watch !import-status ::import-popover-btn)
         (.removeEventListener root "click" on-link-click)
         (.removeEventListener js/document "click" on-doc-click)
         (.removeEventListener js/document "keydown" on-key)
         (.removeEventListener js/document "scroll" on-scroll true)
         (.removeEventListener js/window "resize" on-scroll)
         (when (.-parentNode pop) (.remove pop))))
     :clj nil))

(defn- install-unclipped-tooltip-positioning!
  "Make the bubble format tooltip escape every ancestor's overflow clipping.

   Quill's `.ql-tooltip` is position:absolute inside `.ql-container`, which has
   overflow-y:auto (→ overflow-x:auto per CSS spec); every editor ancestor up to
   the document-view row is overflow:hidden. So any part of the tooltip past the
   editor column is clipped — including the top edge, which Quill's native
   position() never guards: it flips a container-bottom-overflowing tooltip ABOVE
   the selection (themes/bubble.ts) but never clamps that flipped box against the
   container top. A first-line or Cmd-A selection then shows a clipped toolbar.

   Fix: after Quill positions the tooltip in its native (absolute) frame, freeze
   the placed box into position:fixed at the same viewport coordinates and clamp
   it on-screen. Fixed elements are not clipped by overflow ancestors — none of
   the four clip boxes is a transform/filter/contain containing block — so the
   tooltip floats freely over the panes/toolbars, fully visible, without covering
   the document text below the selection. Quill's horizontal clamp to `bounds`
   (the container) keeps it over the editor column; its arrow is untouched and
   the flip is preserved (its negative margin-top is folded into the frozen
   `top`, then zeroed — see freeze-to-fixed! — so it is not applied twice).

   Fixed no longer rides the container's scroll, so this also re-positions the
   tooltip on editor scroll and window resize while it is visible.

   Pre:  `editor` is an initialized bubble-theme Quill whose theme.tooltip.position
         is a fn (init-editor! just constructed it).
   Post: tooltip.position is wrapped; three listeners are installed — a bubble-
         phase `scroll` on ed.root (runs after Quill's own to re-zero its
         margin), a capture-phase document `scroll` (outer-ancestor scroll), and
         a window `resize`. Returns a teardown fn removing all three; the tooltip
         element itself dies with the editor in destroy-editor!."
  [editor]
  #?(:clj nil
     :cljs
     (let [^js ed      editor
           ^js theme   (.-theme ed)
           ^js tooltip (and theme (.-tooltip theme))]
       (if-not (and tooltip (fn? (.-position tooltip)))
         (fn [])                                       ; nothing installed → no-op teardown
         (let [^js orig-position (.-position tooltip)
               ^js root          (.-root tooltip)
               margin            8                     ; px kept from the viewport edge
               freeze-to-fixed!  ; pin fixed at viewport coords: horizontal from
                                 ; Quill's clamp, vertical from the selection itself
               (fn []
                 (let [^js rr (.getBoundingClientRect root)
                       vw     (.-innerWidth js/window)
                       vh     (.-innerHeight js/window)
                       w      (.-width rr)
                       h      (.-height rr)
                       left   (max margin (min (.-left rr) (- vw w margin)))
                       ;; Vertical: anchor to the selection's FIRST visual line, not
                       ;; Quill's flipped box. Quill flips against the (non-scrolling)
                       ;; .ql-container, so on a short pane or a soft-wrapped block it
                       ;; mis-decides and drops the tooltip far from the selection
                       ;; (detached — sometimes below WITH ql-flip set). Place below
                       ;; the first line if the box fits the viewport, else above;
                       ;; ql-flip (the arrow side) is set to match. No selection rect
                       ;; ⇒ fall back to Quill's clamped top.
                       gap    10
                       ^js selection  (.getSelection js/window)
                       ^js range      (when (pos? (.-rangeCount selection)) (.getRangeAt selection 0))
                       ^js line-rects (when range (.getClientRects range))
                       ^js first-line (when (and line-rects (pos? (.-length line-rects)))
                                        (aget line-rects 0))
                       below? (and first-line (<= (+ (.-bottom first-line) gap h) (- vh margin)))
                       above? (and first-line (>= (- (.-top first-line) gap h) margin))
                       flip?  (boolean (and first-line (not below?) above?))
                       top    (cond
                                (nil? first-line) (max margin (min (.-top rr) (- vh h margin)))
                                below?            (+ (.-bottom first-line) gap)
                                above?            (- (.-top first-line) gap h)
                                :else             (max margin (min (+ (.-bottom first-line) gap)
                                                                 (- vh h margin))))]
                   (set! (.. root -style -position) "fixed")
                   (set! (.. root -style -left) (str left "px"))
                   (set! (.. root -style -top)  (str top "px"))
                   ;; Point the arrow at the side we placed on.
                   (if flip?
                     (.add (.-classList root) "ql-flip")
                     (.remove (.-classList root) "ql-flip"))
                   ;; Quill's scroll listener writes margin-top = -scrollTop for its
                   ;; native absolute tooltip; on our fixed box that is a spurious
                   ;; shift, so zero it (the editor-scroll listener re-pins after
                   ;; Quill re-writes it).
                   (set! (.. root -style -margin) "0")))]
           (set! (.-position tooltip)
             (fn [reference]
               (this-as this
                 ;; Revert last frame's fixed override so Quill's own clamp math
                 ;; (getBoundingClientRect vs bounds) runs in the absolute frame
                 ;; it assumes.
                 (set! (.. root -style -position) "")
                 ;; Restore Quill's scroll-compensation margin (margin-top =
                 ;; -scrollTop) BEFORE orig-position runs its flip check. Quill's
                 ;; flip reads root.getBoundingClientRect() to decide whether the
                 ;; box overflows the container; that read is only in the correct
                 ;; frame when this margin is present. Quill sets it via a scroll
                 ;; listener, so it is stale on any non-scroll re-pin (resize, a
                 ;; same-tick second call). Setting it here makes the flip decision
                 ;; — and thus the frozen `top` — deterministic on EVERY re-pin;
                 ;; freeze-to-fixed! zeros it again after reading the placed box.
                 (set! (.. root -style -marginTop)
                   (str (- (.-scrollTop (.-root ed))) "px"))
                 (let [^js range (.getSelection ed)
                       ;; A tall selection makes Quill anchor the tooltip to the
                       ;; selection's BOTTOM, dropping it far from where the
                       ;; selection starts (detached, and off-screen for very tall
                       ;; spans). getLines counts only BLOCK lines — 1 for a
                       ;; soft-wrapped paragraph — so it misses multi-visual-line
                       ;; selections inside one <p>. Compare the whole selection's
                       ;; box height to a single caret's height instead: taller
                       ;; than ~1.5 lines ⇒ multi-line ⇒ anchor to the FIRST line
                       ;; (selection top) so the tooltip stays by where the user
                       ;; began selecting. Covers Cmd-A and multi-block too.
                       ^js sel-box (when (and range (pos? (.-length range)))
                                     (.getBounds ed (.-index range) (.-length range)))
                       ^js first-line (when sel-box (.getBounds ed (.-index range) 0))
                       ^js the-ref (if (and sel-box first-line
                                         (> (.-height sel-box) (* 1.5 (.-height first-line))))
                                     first-line
                                     reference)
                       shift (.call orig-position this the-ref)]
                   (freeze-to-fixed!)
                   shift))))
           (let [reposition!
                 (fn [_]
                   (when-not (.contains (.-classList root) "ql-hidden")
                     (let [^js range (.getSelection ed)]
                       (when (and range (pos? (.-length range)))
                         (let [^js b (.getBounds ed (.-index range) (.-length range))]
                           (.position tooltip b))))))]
             ;; Editor-scroll listener on ed.root (.ql-editor), registered HERE —
             ;; after Quill's constructor scroll listener that sets
             ;; margin-top = -scrollTop. Same target + bubble phase ⇒ insertion
             ;; order ⇒ ours runs last, re-pinning (and re-zeroing the margin) so
             ;; Quill's write never survives the tick.
             (.addEventListener (.-root ed) "scroll" reposition! false)
             (.addEventListener js/document "scroll" reposition! true) ; capture: catch outer-ancestor scroll
             (.addEventListener js/window "resize" reposition!)
             (fn teardown []
               (.removeEventListener (.-root ed) "scroll" reposition! false)
               (.removeEventListener js/document "scroll" reposition! true)
               (.removeEventListener js/window "resize" reposition!))))))))

(defn- setup-mobile-keyboard-suppression!
  "On touch devices, suppress virtual keyboard until double-tap.
   Sets inputmode=\"none\" on .ql-editor; double-tap removes it and
   refocuses to trigger keyboard. Blur re-adds the attribute."
  [editor]
  #?(:cljs
     (let [mq (.matchMedia js/window "(pointer: coarse)")]
       (when (.-matches mq)
         (let [^js root (.-root editor)
               !last-tap (atom 0)
               !refocusing (atom false)
               suppress! (fn [] (.setAttribute root "inputmode" "none"))
               enter-edit! (fn []
                             (.removeAttribute root "inputmode")
                             (reset! !refocusing true)
                             (.blur root)
                             (js/requestAnimationFrame
                               (fn []
                                 (.focus root)
                                 (reset! !refocusing false))))]
           ;; Double-tap detection via touchend timing
           (.addEventListener root "touchend"
             (fn [^js e]
               (when (zero? (.-length (.-touches e)))
                 (let [now (js/Date.now)
                       elapsed (- now @!last-tap)]
                   (reset! !last-tap now)
                   (when (< elapsed 300)
                     (enter-edit!)))))
             #js {:passive true})
           ;; Re-suppress on blur (tap outside, back button, app switch)
           (.addEventListener root "blur"
             (fn [_] (when-not @!refocusing (suppress!))))
           ;; Initial suppression
           (suppress!))))
     :clj nil))

;; ---------------------------------------------------------------------------
;; Content accessibility pass (WCAG 1.1.1 / 2.4.4 / 4.1.2). Quill's Delta
;; conversion drops <img alt> (the blot only stores the src), imported content
;; can carry text-less anchors, and the syntax module's per-code-block
;; language <select> renders unlabeled. Patch the rendered DOM: it is the only
;; layer that sees all three, and it covers documents imported before the fix.
;; Re-applied via MutationObserver — the syntax module re-renders code blocks
;; on its own timer, and edits can insert new images/links.
;; ---------------------------------------------------------------------------

#?(:cljs
   (defn- apply-content-a11y! [^js root]
     ;; Images whose alt was dropped → explicit decorative marker.
     (doseq [^js img (array-seq (.querySelectorAll root "img:not([alt])"))]
       (.setAttribute img "alt" ""))
     ;; Anchors with no accessible name → name them from their target.
     (doseq [^js a (array-seq (.querySelectorAll root "a[href]:not([aria-label])"))]
       (when (and (str/blank? (.-textContent a))
                  (nil? (.querySelector a "img[alt]:not([alt=''])")))
         (.setAttribute a "aria-label" (str "Link: " (.-href a)))))
     ;; Quill syntax-module language pickers.
     (doseq [^js sel (array-seq (.querySelectorAll root "select.ql-ui:not([aria-label])"))]
       (.setAttribute sel "aria-label" "Code block language"))))

#?(:cljs
   (defn- install-content-a11y!
     "Run the a11y pass now and on every subtree change. Returns the
      MutationObserver (disconnect it on editor teardown). Attribute writes in
      the pass don't re-trigger the observer — it only watches childList."
     [^js editor]
     (let [root (.-root editor)
           obs (js/MutationObserver. (fn [_ _] (apply-content-a11y! root)))]
       (apply-content-a11y! root)
       (.observe obs root #js {:childList true :subtree true})
       obs)))

(defn init-editor!
  "Initialize Quill editor in the given container with initial HTML.
   Destroys any existing editor first (singleton).
   Wires an immediate text-change listener that sets !dirty-html on user edits.
   topic-id tags dirty-html so auto-save targets the correct topic."
  [container initial-html topic-id]
  #?(:clj nil
     :cljs
     (when (and container (.-Quill js/window))
       (log/log-debug (str "init-editor! topic-id=" topic-id " html-len=" (count initial-html)))
       ;; Destroy previous editor if any
       (destroy-editor!)
       ;; Clear dirty flag — fresh content, no pending saves
       (reset! !dirty-html nil)
       (let [Quill (.-Quill js/window)
             cleaned-html (-> (or initial-html "")
                            (str/replace #"^```html\s*\n?" "")
                            (str/replace #"^```\s*\n?" "")
                            (str/replace #"\n?```\s*$" "")
                            ;; Cleanup of paragraphs already corrupted by a
                            ;; previous load — the literal label string from
                            ;; both pre-Clojure and current pickers. (Future
                            ;; corruption is prevented by the `select.ql-ui`
                            ;; clipboard matcher registered below — stripping
                            ;; the <select> from the HTML pre-convert was
                            ;; collapsing multi-line code blocks because
                            ;; Quill's container matcher iterates children in
                            ;; place and the regex disturbed that structure.)
                            (str/replace #"<p>PlainBashC\+\+C#(?:Clojure)?CSSDiffHTML/XMLJavaJavaScriptMarkdownPHPPythonRubySQL</p>" "")
                            str/trim)
             t0 (when js/goog.DEBUG (js/performance.now))
             ^js editor (new Quill container
                          ;; bubble theme (E1): floating-on-selection toolbar
                          ;; (Notion-style). The toolbar module config below is
                          ;; still honored — bubble builds its tooltip buttons
                          ;; from it. Card-field editors stay snow (quill_field).
                          (clj->js {:theme "bubble"
                                    ;; Clamp the floating tooltip to the editor
                                    ;; container (the narrow pane), not the
                                    ;; default wide ancestor — otherwise it is
                                    ;; positioned past the pane edge and clipped
                                    ;; by the PDF pane.
                                    :bounds container
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
                                              ;; Quill 2 syntax module — requires window.hljs (loaded in index*.html).
                                              ;; Renders `<span class="hljs-*">` spans inside `.ql-code-block` line divs;
                                              ;; html-cleaner carves out `.ql-code-block-container` so the spans survive.
                                              ;; `:languages` overrides Quill's 14-item default to insert Clojure
                                              ;; (its language pack is loaded in index*.html alongside highlight.min.js).
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
                                    :placeholder "Enter text..."}))
             ^js clipboard (.-clipboard editor)
             ;; Tell Quill's clipboard pipeline to ignore the syntax module's
             ;; language-picker <select>. Without this, Quill's default
             ;; handling extracts the <option> labels as text and emits a <p>
             ;; with the concatenated picker labels on every reload, growing
             ;; one corrupted paragraph per save cycle. Returning a fresh
             ;; empty Delta tells the converter this subtree contributes
             ;; nothing — the surrounding container + child line divs are
             ;; processed by their own matchers and continue to render as
             ;; multi-line code blocks.
             Delta (.import (.-Quill js/window) "delta")
             _ (.addMatcher clipboard "select.ql-ui"
                 (fn [_node _delta] (new Delta)))
             ;; Strip Quill syntax-module token wrappers. CodeToken.formats
             ;; in Quill 2.0.3 returns boolean `true` instead of the actual
             ;; hljs-X value, producing `class="hljs-true"` and collapsing
             ;; adjacent same-value inlines into one span. By returning a
             ;; Delta with just the textContent (no code-token attribute),
             ;; we hand Quill clean text; the syntax module's 1 s timer
             ;; re-applies fresh, correctly-classed tokens.
             _ (.addMatcher clipboard "span.ql-token"
                 (fn [^js node _delta]
                   (-> (new Delta) (.insert (.-textContent node)))))
             ;; Preserve leading/trailing/inter-character whitespace inside
             ;; code blocks. Quill 2.0.3's default clipboard text-node walker
             ;; runs through the HTML parser's whitespace normalization,
             ;; which collapses leading runs of spaces at element boundaries
             ;; — losing user-typed indentation on every reload. By reading
             ;; the div's textContent verbatim and emitting one text op +
             ;; one code-block-attributed newline op per line, we hand Quill
             ;; clean, whitespace-faithful text; the syntax module re-applies
             ;; hljs spans on its 1 s timer after setContents.
             _ (.addMatcher clipboard "div.ql-code-block"
                 (fn [^js node _delta]
                   (let [text (.-textContent node)
                         lang (or (.getAttribute node "data-language") "plain")]
                     (-> (new Delta)
                       (.insert text)
                       (.insert "\n" #js {"code-block" lang})))))
             delta (.convert clipboard #js {:html cleaned-html})]
         (when (seq cleaned-html)
           (.setContents editor delta))
         ;; dir="auto" → browser resolves direction from first strong
         ;; directional character. Paired with .ql-editor{text-align:start}
         ;; in index.css so RTL imports (Arabic, Hebrew) render right-aligned
         ;; without needing a stored language column. Per-block direction
         ;; from Quill's toolbar still overrides this on its own block.
         (.setAttribute (.-root editor) "dir" "auto")
         (when (and js/goog.DEBUG t0)
           (js/console.log "[Editor perf] init TOTAL:" (.toFixed (- (js/performance.now) t0) 1) "ms, html-len:" (count cleaned-html)))
         ;; Immediate text-change: sets !dirty-html on every user edit
         ;; e/Offload-latch in callers handles rapid updates via "latest-wins" semantics
         ;; Text-change listener reads topic-id from !editor-state (mutable ref)
         ;; so it stays correct across page navigations without reinit
         (.on editor "text-change"
           (fn [_delta _oldDelta source]
             (when (= source "user")
               (let [^js root (.-root editor)
                     current-topic-id (:topic-id @!editor-state)]
                 (reset! !dirty-html {:html (.-innerHTML root)
                                      :topic-id current-topic-id})
                 ;; User edits invalidate cached range — indices may now be stale
                 (reset! !last-selection nil)))))
         ;; Cache last non-empty selection so toolbar buttons (Extract, etc.)
         ;; can read it after focus moves to the button (Chrome blurs Quill on
         ;; mousedown for buttons; Safari/Firefox on macOS do not).
         (.on editor "selection-change"
           (fn [^js range _old _source]
             (when (and range (> (.-length range) 0))
               (reset! !last-selection {:index (.-index range)
                                        :length (.-length range)}))))
         ;; Bubble theme has no fixed `.ql-toolbar` to relocate — its formatting
         ;; controls live in the floating tooltip shown on selection. So no
         ;; toolbar node is stored (:toolbar nil; destroy-editor! skips removal).
         (reset! !editor-state {:editor editor :container container
                                :topic-id topic-id :toolbar nil
                                :a11y-observer (install-content-a11y! editor)
                                :import-popover-teardown (setup-link-import-popover! editor)
                                :tooltip-teardown (install-unclipped-tooltip-positioning! editor)})
         (setup-mobile-keyboard-suppression! editor)
         (table-ui/init! editor)
         ;; Bubble's toolbar lives in the floating .ql-tooltip inside the
         ;; container — label its pickers/buttons.
         (a11y/label-quill-toolbar! container)
         ;; Tab moves focus out (Quill's indent bindings removed); Escape
         ;; also blurs, as belt-and-suspenders for nested contexts.
         (a11y/free-quill-tab! editor)
         (a11y/install-quill-tab-escape! editor)
         editor))))

(defn set-content!
  "Update Quill content without destroying the editor.
   Uses source 'api' so text-change listener does NOT fire.
   Does NOT set !dirty-html — caller decides persistence."
  [html]
  #?(:clj nil
     :cljs
     (when-let [{:keys [editor]} @!editor-state]
       (let [^js ed editor
             ^js clipboard (.-clipboard ed)
             cleaned (-> (or html "")
                       (str/replace #"^```html\s*\n?" "")
                       (str/replace #"^```\s*\n?" "")
                       (str/replace #"\n?```\s*$" "")
                       str/trim)
             current-html (.-innerHTML (.-root ed))]
         ;; Skip if content hasn't changed (preserves scroll + highlights)
         (when (not= (count cleaned) (count current-html))
           (let [delta (.convert clipboard #js {:html cleaned})]
             (.setText ed "" "api")
             (.setContents ed delta "api")
             (.clear (.-history ed))))
         true))))

(defn update-topic-id!
  "Update the topic-id in editor state without reinitializing Quill.
   The text-change listener reads from !editor-state, so this ensures
   dirty-html is tagged with the correct topic-id after page navigation."
  [new-topic-id]
  #?(:cljs (when @!editor-state
             (swap! !editor-state assoc :topic-id new-topic-id))
     :clj nil))

(defn get-current-html!
  "Read innerHTML from the current editor. Client-side only, never enters Electric reactive graph."
  []
  #?(:clj nil
     :cljs
     (when-let [{:keys [editor]} @!editor-state]
       (let [^js ed editor
             ^js root (.-root ed)]
         (.-innerHTML root)))))

(defn- effective-range
  "Live Quill range if non-empty; otherwise the cached last in-editor range.
   Cache survives blur (Chrome focuses buttons on mousedown, blurring Quill
   before the click handler runs). Bounds-checked against current editor
   length so post-edit stale ranges are skipped. Returns {:index :length}
   or nil."
  [editor]
  #?(:clj nil
     :cljs
     (let [^js ed editor
           ^js sel (.getSelection ed)]
       (if (and sel (> (.-length sel) 0))
         {:index (.-index sel) :length (.-length sel)}
         (when-let [{:keys [index length]} @!last-selection]
           (when (and (> length 0)
                   (<= (+ index length) (.getLength ed)))
             {:index index :length length}))))))

(defn get-selected-text!
  "Get selected text from Quill editor. Returns nil if no selection.
   Client-side only, never enters Electric reactive graph."
  []
  #?(:clj nil
     :cljs
     (when-let [{:keys [editor]} @!editor-state]
       (when-let [{:keys [index length]} (effective-range editor)]
         (let [^js ed editor]
           (.getText ed index length))))))

(defn get-selection!
  "Get selected text and its range from Quill. Returns nil if no selection."
  []
  #?(:clj nil
     :cljs
     (when-let [{:keys [editor]} @!editor-state]
       (when-let [{:keys [index length]} (effective-range editor)]
         (let [^js ed editor]
           {:text (.getText ed index length)
            :index index
            :length length})))))

(defn get-selection-html!
  "Get selected HTML and range from Quill. Returns nil if no selection.
   Creates a temporary Quill instance to convert Delta -> HTML."
  []
  #?(:clj nil
     :cljs
     (when-let [{:keys [editor]} @!editor-state]
       (when-let [{:keys [index length]} (effective-range editor)]
         (let [^js ed editor
               ^js delta (.getContents ed index length)
               ^js temp-div (js/document.createElement "div")
               ^js temp-quill (new (.-Quill js/window) temp-div)
               _ (.setContents temp-quill delta)
               html (.-innerHTML (.-root temp-quill))]
           {:html html
            :text (.getText ed index length)
            :index index
            :length length})))))

(defn highlight-range!
  "Apply highlight formatting to a range in the Quill editor.
   Default color is extract blue (#44C2FF). Pass :color for card-generation gold (#FBBF24) etc.
   Also pushes the updated HTML to !dirty-html so the highlight is persisted."
  [index length & {:keys [color] :or {color "var(--color-highlight-extract)"}}]
  #?(:clj nil
     :cljs
     (when-let [{:keys [editor topic-id]} @!editor-state]
       (.formatText ^js editor index length
         (clj->js {:background color})
         "api")
       ;; Push updated HTML so the auto-save pipeline persists the highlight
       (let [^js root (.-root editor)]
         (reset! !dirty-html {:html (.-innerHTML root)
                              :topic-id topic-id})))))
