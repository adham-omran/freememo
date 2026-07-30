(ns freememo.rich-text-editor
  "Quill rich text editor integration — global singleton."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.logging :as log]
   [freememo.quill-table-ui :as table-ui]
   [freememo.quill-field :as quill-field]
   [freememo.editor-actions :as editor-actions]
   [freememo.math :as math]
   [freememo.a11y :as a11y]
   #?(:cljs [freememo.format-menu :as format-menu])
   #?(:cljs [freememo.code-lang-picker :as code-lang-picker])
   #?(:cljs [freememo.formula-ui :as formula-ui])
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

;; Set to the current HTML on every user edit (immediate, no debounce — other
;; consumers need the live value: bottom_panel.cljc's ToolbarBar derives
;; card-gen context from it, rich_text_editor_component.cljc gates its
;; in-place content refresh on it). editor_pane.cljc's auto-save watches this
;; but debounces the actual DB write to an idle pause; it does not save on
;; every change here.
;; nil means "no pending user edits" — the auto-save guard checks for non-nil.
;; Reset to nil on init/page-switch so navigating doesn't trigger a save.
;; Shape: {:html "..." :topic-id N}
(defonce !dirty-html (atom nil))

;; Most recent non-empty in-editor selection range. Used as fallback when
;; (.getSelection ed) returns null/empty — happens in Chrome when clicking
;; the toolbar Extract button focuses the button and blurs Quill before the
;; click handler reads selection. Shape: {:index N :length N} or nil.
(defonce !last-selection (atom nil))

;; Code-block toolbar icon override lives in freememo.quill-field (single
;; source of truth, required above) — loading that ns installs it on both
;; editors' shared `Quill.import("ui/icons")` registry.

;; Generation counter for init-editor!'s KaTeX wait. That wait is async, so an
;; init can still be in flight when another init OR a teardown happens; only the
;; newest generation may build. Both init-editor! and destroy-editor! bump it,
;; which is what makes teardown cancel a pending build. Lives in a plain defn
;; per CLAUDE.md's "JS Library Init Side Effects" rule — a counter incremented
;; inside an e/defn body would not increment reliably.
(defonce ^:private !init-gen (atom 0))

(defn destroy-editor!
  "Destroy the current global editor instance (if any).

   Post: no editor is live, !editor-state is nil, AND no init still waiting on
         KaTeX may build. That last clause is why this bumps !init-gen: without
         it, an unmount during the wait let build-editor! construct Quill into a
         detached container and repopulate !editor-state — after which the
         component's `(when-not (some? (:editor @!editor-state)) …)` mount guard
         saw a live-looking editor and never initialised again."
  []
  #?(:clj nil
     :cljs
     (do
       (swap! !init-gen inc)
       (when-let [{:keys [editor container toolbar import-popover-teardown format-menu-teardown code-lang-teardown formula-teardown a11y-observer]} @!editor-state]
         (let [^js ed editor
               ^js ct container
               ^js tb toolbar]
           ;; Stop the content-a11y MutationObserver before the DOM is torn down.
           (when a11y-observer (.disconnect ^js a11y-observer))
           ;; Tear down the wiki-link import popover (element + document/window
           ;; listeners + status watch). Must run before innerHTML clear below.
           (when import-popover-teardown (import-popover-teardown))
           ;; Remove the custom format-menu (card element + its listeners).
           (when format-menu-teardown (format-menu-teardown))
           ;; Remove the custom code-language picker (observer + popup + triggers).
           (when code-lang-teardown (code-lang-teardown))
           ;; Remove the formula click/blur/Escape listeners on the editor root.
           (when formula-teardown (formula-teardown))
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

;; The bubble editor's formatting UI is a custom floating card
;; (freememo.format-menu), not Quill's native `.ql-tooltip` — the native
;; tooltip's placement collided with the app command bar and covered content
;; (see git history). init-editor! hides the native tooltip via CSS and calls
;; format-menu/install!; the returned teardown runs in destroy-editor!.

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

(defn- build-editor!
  "Construct the singleton Quill editor. Called by init-editor! once the KaTeX
   wait has settled; `math?` is that outcome.

   Pre  : `container` is mounted, `window.Quill` exists, no editor is live
          (init-editor! destroyed any predecessor).
   Post : !editor-state holds the new instance and its teardowns; returns it."
  [container initial-html topic-id math?]
  #?(:clj nil
     :cljs
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
                      ;; (Notion-style). Modules (toolbar/syntax/table) come
                      ;; from quill-field/quill-config — the SAME config
                      ;; QuillField uses (quill_field.cljc), so the two
                      ;; editors' formatting surfaces stay identical. Extra
                      ;; :bounds clamps the floating tooltip to this
                      ;; container (the narrow pane), not the default wide
                      ;; ancestor — otherwise it is positioned past the
                      ;; pane edge and clipped by the PDF pane. QuillField
                      ;; has no such ancestor and omits :bounds.
                      (clj->js (quill-field/quill-config "Enter text..." {:bounds container} math?)))
           ^js clipboard (.-clipboard editor)
           ;; See quill-field/install-clipboard-matchers! for what each
           ;; matcher does and why (shared with QuillField's own editor).
           _ (quill-field/install-clipboard-matchers! clipboard)
           ;; Pasted/dropped images → /api/media instead of inline base64.
           ;; This editor never had any image-paste handling: only QuillField
           ;; carried the (inert) matcher, so a screenshot pasted here was
           ;; stored as base64 in topics.content, which has no length cap.
           _ (editor-actions/install-image-rehost! editor)
           ;; Stored `\(TeX\)` → span.ql-formula[data-value] for Parchment.
           ;; Skipped without KaTeX so an unresolvable span cannot lose its
           ;; data-value on the next save.
           for-editor (if math? (math/stored->editor-html cleaned-html) cleaned-html)
           delta (.convert clipboard #js {:html for-editor})]
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
       ;; Immediate text-change: sets !dirty-html on every user edit. Other
       ;; consumers (ToolbarBar's live-content, the unsaved-edits guard)
       ;; need this un-debounced; editor_pane.cljc's auto-save is the one
       ;; consumer that debounces its reaction to an idle pause before it
       ;; writes to the DB — see that ns.
       ;; Text-change listener reads topic-id from !editor-state (mutable ref)
       ;; so it stays correct across page navigations without reinit
       (.on editor "text-change"
         (fn [_delta _oldDelta source]
           (when (= source "user")
             (let [current-topic-id (:topic-id @!editor-state)]
               (reset! !dirty-html {:html (quill-field/editor-html editor)
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
                              ;; The KaTeX-wait outcome, recorded rather than
                              ;; re-derived: set-content! needs it to decide
                              ;; whether to promote stored math to blots, and
                              ;; probing `(.getModule ed "formula")` for it
                              ;; depends on Quill registering the formula module
                              ;; under that name — an assumption this does not
                              ;; need to make.
                              :math? (boolean math?)
                              :a11y-observer (install-content-a11y! editor)
                              :import-popover-teardown (setup-link-import-popover! editor)
                              :format-menu-teardown (format-menu/install! editor {:card-gen? true})
                              :code-lang-teardown (code-lang-picker/install! editor)
                              ;; Formula popover + typed-source conversion. Only
                              ;; when KaTeX arrived — Formula.create throws without
                              ;; it, and there is no insert button to gate instead.
                              :formula-teardown (when math?
                                                  (formula-ui/install! editor))})
       (setup-mobile-keyboard-suppression! editor)
       (table-ui/init! editor)
       ;; Bubble's toolbar lives in the floating .ql-tooltip inside the
       ;; container — label its pickers/buttons.
       (a11y/label-quill-toolbar! container)
       ;; Tab moves focus out (Quill's indent bindings removed); Escape
       ;; also blurs, as belt-and-suspenders for nested contexts.
       (a11y/free-quill-tab! editor)
       (a11y/install-quill-tab-escape! editor)
       editor)))

(defn init-editor!
  "Initialize Quill editor in the given container with initial HTML.
   Destroys any existing editor first (singleton).
   Wires an immediate text-change listener that sets !dirty-html on user edits.
   topic-id tags dirty-html so auto-save targets the correct topic.

   Construction is deferred until the KaTeX wait settles (see
   freememo.math/on-katex-ready!) because Quill's formula module throws if KaTeX
   is absent. The wait is capped, so a blocked CDN costs `katex-wait-ms` and then
   yields a working editor without math rather than no editor at all.

   Pre  : `container` is a mounted node; `window.Quill` exists.
   Post : the previous editor is destroyed and !dirty-html cleared SYNCHRONOUSLY;
          the new editor appears in !editor-state asynchronously. Returns nil —
          callers must read !editor-state, not this value.
   Inv  : at most one generation may build. `destroy-editor!` bumps !init-gen, so
          this claims the generation AFTER that call — claiming it before would
          have this call invalidate itself and never build. Any later init or
          teardown bumps again and drops this pending wait."
  [container initial-html topic-id]
  #?(:clj nil
     :cljs
     (when (and container (.-Quill js/window))
       (log/log-debug (str "init-editor! topic-id=" topic-id " html-len=" (count initial-html)))
       ;; Destroy previous editor if any — this also invalidates any init still
       ;; waiting on KaTeX.
       (destroy-editor!)
       ;; Clear dirty flag — fresh content, no pending saves
       (reset! !dirty-html nil)
       (let [my-gen @!init-gen]
         (math/on-katex-ready!
           (fn [math?]
             (when (= my-gen @!init-gen)
               (build-editor! container initial-html topic-id math?)))))
       nil)))

(defn set-content!
  "Update Quill content without destroying the editor.
   Uses source 'api' so text-change listener does NOT fire.
   Does NOT set !dirty-html — caller decides persistence.

   The unchanged-content check compares stored form against stored form. Reading
   the root's raw innerHTML would compare against Quill's RENDERED form, whose
   KaTeX subtrees make the lengths differ on every call once a document contains
   math — replacing the content, and losing scroll position and highlights, on
   every reactive `initial-html` change."
  [html]
  #?(:clj nil
     :cljs
     (when-let [{:keys [editor math?]} @!editor-state]
       (let [^js ed editor
             ^js clipboard (.-clipboard ed)
             cleaned (-> (or html "")
                       (str/replace #"^```html\s*\n?" "")
                       (str/replace #"^```\s*\n?" "")
                       (str/replace #"\n?```\s*$" "")
                       str/trim)
             current-stored (quill-field/editor-html ed)]
         ;; Skip if content hasn't changed (preserves scroll + highlights)
         (when (not= (count cleaned) (count current-stored))
           (let [for-editor (if math? (math/stored->editor-html cleaned) cleaned)
                 delta (.convert clipboard #js {:html for-editor})]
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
  "Read the current editor's content in STORED form (see quill-field/editor-html).
   Client-side only, never enters Electric reactive graph."
  []
  #?(:clj nil
     :cljs
     (when-let [{:keys [editor]} @!editor-state]
       (quill-field/editor-html editor))))

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
   Creates a temporary Quill instance to convert Delta -> HTML.

   The returned :html is in STORED form. Extract creation persists it
   (content_toolbar_helpers → clean-html), so a formula left as
   `span.ql-formula` would lose both its class and its data-value there."
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
               html (math/editor->stored-html (.-innerHTML (.-root temp-quill)))]
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
       ;; Push updated HTML so the auto-save pipeline persists the highlight.
       ;; Stored form, like every other !dirty-html writer — a raw read here
       ;; would persist Quill's rendered KaTeX subtree instead of the TeX.
       (reset! !dirty-html {:html (quill-field/editor-html editor)
                            :topic-id topic-id}))))
