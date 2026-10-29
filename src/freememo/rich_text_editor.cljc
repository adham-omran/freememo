(ns freememo.rich-text-editor
  "Quill rich text editor integration — global singleton."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.logging :as log]
   [freememo.quill-table-ui :as table-ui]
   [freememo.quill-field :as quill-field]
   [freememo.merged-window :as merged-window]
   [freememo.quill-markdown :as quill-markdown]
   [freememo.editor-actions :as editor-actions]
   [freememo.math :as math]
   [freememo.motion :as motion]
   [freememo.a11y :as a11y]
   #?(:cljs [freememo.command-bus :as bus])
   #?(:cljs [freememo.editor-links :as editor-links])
   #?(:cljs [freememo.reading-position :as rp])
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

;; ── The editor's link channel ──────────────────────────────────────
;; `freememo.editor-links` owns the DOM; these five atoms are its only way out.
;; A click listener cannot reach the server, the router or the toast queue, so
;; every one of those is published here and answered by an e/Token in
;; editor_pane. All are defined on BOTH peers with no reader conditional —
;; they are named in e/defn bodies, and a CLJ-only def read from client scope
;; makes the two peers disagree on frame shape and kills the WebSocket.

;; Set by the card's Import button.
;; Shape: {:url "https://…" :ts <timestamp>} or nil. The url is already
;; fragment-stripped by link-target — it is the dupe key.
(defonce !import-url (atom nil))

;; Import status, written by the Electric wiring and read by the card's button.
;; :idle | :importing | :queued | :done | :already-exists | :error
(defonce !import-status (atom :idle))

;; Set by a click on a link to another topic. An imported documentation tree's
;; cross-references are why this exists: the href cannot navigate on its own,
;; because Chrome does not follow an anchor click inside a contenteditable and
;; router5 binds its handler per node rather than at the document.
;; Shape: {:topic-id N :fragment "sec" :ts <timestamp>} or nil.
(defonce !navigate-topic (atom nil))

;; Set when a click can only be explained, not acted on — a fragment with no
;; matching heading, or a relative href in a document with no source address.
;; Shape: {:level :info :message "…" :ts <timestamp>} or nil.
(defonce !link-toast (atom nil))

;; The document's own address, for resolving relative hrefs. Written by
;; editor_pane from the topic's effective source; nil when it has none, which
;; is what makes a relative link :no-base rather than a broken guess.
(defonce !document-base-url (atom nil))

;; An anchor to land on once the NEXT document has loaded. A click on
;; `/viewer/topic/<id>#sec` cannot scroll where it was clicked — that document
;; is about to be replaced — so the fragment waits here and init-editor! spends
;; it after the new content is in the DOM.
;; Shape: "section-id" or nil.
(defonce !pending-anchor (atom nil))

;; Each user edit publishes stored HTML and its immutable save scope.
;; Toolbar consumers read the live value. EditorPane saves each edit off the reactor.
;; A saved snapshot remains until another body installs. Its acknowledgement
;; prevents repeated saves after navigation or a component remount.
;; Shape: {:html string :topic-id id :save-scope map :edit-id string}.
(defonce !dirty-html (atom nil))
(defonce !saved-edit-id (atom nil))

(defn publish-dirty! [html]
  (reset! !dirty-html
    (merged-window/edit-entry @!editor-state html (str (random-uuid)))))


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

;; Whether a Quill instance is built and usable.
;;
;; Its own atom, deliberately NOT a read of `!editor-state`. Two components need
;; this flag, and `update-topic-id!` swaps `!editor-state` on every evaluation of
;; one of them — so watching that map means watching an atom the same component
;; writes. Work-skipping damps the loop only while every value in the map stays
;; `=` across the swap, which is a property no future field is obliged to keep.
;;
;; A boolean changes twice per editor lifetime, so a watch on it fires twice.
(defonce !editor-ready? (atom false))

;; How many times a document body has been installed into the editor.
;;
;; Bumped by the two sites that install one: `build-editor!` on a cold build,
;; and `set-content!` on a real replacement. NEVER on `set-content!`'s
;; length-equality skip — that skip exists to preserve scroll, so a bump there
;; would report an install that did not happen.
;;
;; NOT `!init-gen`, which counts editor BUILDS so a build waiting on KaTeX can
;; tell it has been superseded. This counts installs so a reader can tell the
;; body under it was replaced. One build can install twice: the component builds
;; empty when the body has not arrived, then installs it through `set-content!`.
;;
;; WHY ANYONE NEEDS IT. Every install puts the editor back at scrollTop 0, and
;; in merged reading that is page 1 whatever page the reader asked for. Four
;; entrances reach it — a deep link, turning the mode on, reopening the document
;; in-app, and an OCR scan finishing mid-read — so a mount hook would fix one of
;; the four. `merged-page-bridge` watches this and restores the page.
(defonce !content-generation (atom 0))

(defn current-editor
  "The live document-editor Quill instance, or nil.

   Pre : none.
   Post: nil when no editor is mounted, so every caller must handle nil — the
         document editor is torn down on navigation away from a topic."
  []
  #?(:clj nil
     :cljs (:editor @!editor-state)))

(defn release-selection!
  "Drop the document editor's selection, for when its pane is hidden
   (plans/editor-pane-toggle.md §3.4).

   Pre : none.
   Post: the editor holds no live range and no cached `!last-selection`, and
         focus is not inside it — so a card action's Quill fallback reads
         nothing. A selection elsewhere on the page is left alone: Quill's
         `setSelection nil` clears the document selection, so it runs only when
         that selection is the editor's."
  []
  #?(:clj nil
     :cljs (do (when-let [^js ed (current-editor)]
                 (when (.getSelection ed)
                   (.setSelection ed nil "silent")))
               (reset! !last-selection nil))))

;; The palette's :insert-audio (freememo.commands). It is registered here
;; because this namespace owns the one editor the palette can name.
;;
;; Pre : the palette dispatches from a keypress or a click, so the file picker
;;       opens under that user activation.
;; Post: the preparer is nil while no document editor is mounted, which keeps
;;       the entry out of the palette. The handler opens a file picker, and a
;;       picked MP3 lands at the caret the editor last held: `getSelection`
;;       with `true` focuses the editor, and Quill's focus restores its saved
;;       range. An editor that never held a caret inserts at index 0, because
;;       that is where Quill's saved range starts.
;; Inv : the caret survives whichever runs first, this handler or the
;;       palette's focus return, because `modal-shell/restore-focus!` also
;;       focuses a Quill root through Quill. A plain `.focus()` there moved the
;;       caret to index 0 first, and every clip landed at the top.
#?(:cljs
   (do (bus/register-preparer! :insert-audio
         ;; A hidden editor pane is still mounted, and a clip inserted into it
         ;; would land where nobody sees it (plans/editor-pane-toggle.md §4.3).
         (fn [ctx] (when (and (current-editor) (not (:editor-hidden? ctx))) {})))
       (bus/register-handler! :insert-audio
         (fn [_ctx _payload]
           (when-let [^js ed (current-editor)]
             (editor-actions/insert-audio! ed
               (some-> (.getSelection ed true) .-index)))))))

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
       (reset! !editor-ready? false)
       (swap! !init-gen inc)
       (when-let [{:keys [editor container toolbar link-actions-teardown format-menu-teardown code-lang-teardown context-menu-teardown clip-drag-teardown formula-teardown markdown-teardown resize-focus-teardown page-marker-teardown a11y-observer]} @!editor-state]
         (let [^js ed editor
               ^js ct container
               ^js tb toolbar]
           ;; Stop the content-a11y MutationObserver before the DOM is torn down.
           (when a11y-observer (.disconnect ^js a11y-observer))
           ;; Tear down the anchor click listener, its card and its status
           ;; watch. Must run before the innerHTML clear below.
           (when link-actions-teardown (link-actions-teardown))
           ;; Remove the custom format-menu (card element + its listeners).
           (when format-menu-teardown (format-menu-teardown))
           ;; Remove the custom code-language picker (observer + popup + triggers).
           (when code-lang-teardown (code-lang-teardown))
           (when context-menu-teardown (context-menu-teardown))
           (when clip-drag-teardown (clip-drag-teardown))
           ;; Remove the formula click/blur/Escape listeners on the editor root.
           (when formula-teardown (formula-teardown))
           ;; Remove the delayed Markdown listener before the editor DOM.
           (when markdown-teardown (markdown-teardown))
           ;; Remove the resize focus-out listener on the editor container.
           (when resize-focus-teardown (resize-focus-teardown))
           ;; Remove the scroll listener that reports the merged document's
           ;; current page.
           (when page-marker-teardown (page-marker-teardown))
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
  [container initial-html topic-id math? save-scope]
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
           ^js editor (doto (new Quill container
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
                      (clj->js (quill-field/quill-config "Enter text..." {:bounds container})))
                        ;; Cmd/Ctrl+\\ clears selected formatting in this editor.
                        (editor-actions/install-clear-format-binding!))
           _redo (quill-field/install-cmd-y-redo! editor)
           ^js clipboard (.-clipboard editor)
           ;; See quill-field/install-clipboard-matchers! for what each
           ;; matcher does and why (shared with QuillField's own editor).
           _ (quill-field/install-clipboard-matchers! clipboard)
           ;; Pasted/dropped images → /api/media instead of inline base64.
           ;; This editor never had any image-paste handling: only QuillField
           ;; carried the (inert) matcher, so a screenshot pasted here was
           ;; stored as base64 in topics.content, which has no length cap.
           _ (editor-actions/install-attachment-upload! editor)
           ;; Stored `\(TeX\)` → span.ql-formula[data-value] for Parchment.
           ;; Skipped without KaTeX so an unresolvable span cannot lose its
           ;; data-value on the next save.
           for-editor (if math? (math/stored->editor-html cleaned-html) cleaned-html)
           delta (.convert clipboard #js {:html for-editor})
           ;; Read before the flush below spends it: a link fragment owns this
           ;; build's scroll, so the reading position must not restore over it
           ;; (plans/reading-position-memory.md §4 5.3).
           fragment? (boolean (not-empty @!pending-anchor))]
       (when (seq cleaned-html)
         (.setContents editor delta))
       ;; A `/viewer/topic/<id>#sec` click left its anchor behind, because the
       ;; document it was clicked in was about to be replaced. This is the
       ;; first moment the target exists.
       (editor-links/flush-pending-anchor! (.-root editor) !pending-anchor)
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
               (publish-dirty! (quill-field/editor-html editor))
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
                              :topic-id topic-id :save-scope save-scope :toolbar nil
                              ;; The KaTeX-wait outcome, recorded rather than
                              ;; re-derived: set-content! needs it to decide
                              ;; whether to promote stored math to blots, and
                              ;; probing `(.getModule ed "formula")` for it would
                              ;; return nil forever. Quill 2 has no formula
                              ;; module — see the note in quill-config's :modules
                              ;; map. Not making that assumption is what kept this
                              ;; line correct.
                              :math? (boolean math?)
                              :a11y-observer (install-content-a11y! editor)
                              :link-actions-teardown (editor-links/install! editor
                                                       {:!import !import-url
                                                        :!navigate !navigate-topic
                                                        :!toast !link-toast
                                                        :!base-url !document-base-url
                                                        :!import-status !import-status})
                              :format-menu-teardown (format-menu/install! editor {:card-gen? true})
                              :code-lang-teardown (code-lang-picker/install! editor)
                              ;; The shared context menu: image, clip and caret
                              ;; rows (plans/custom-context-menu.md 4.1.1).
                              ;; Card fields install the same one.
                              :context-menu-teardown (quill-field/install-context-menu! editor {:document-editor? true})
                              ;; Drag a clip by its grip to another line.
                              :clip-drag-teardown (editor-actions/install-clip-drag! editor)
                              ;; Formula popover + typed-source conversion. Only
                              ;; when KaTeX arrived — Formula.create throws without
                              ;; it, and there is no insert button to gate instead.
                              :formula-teardown (when math?
                                                  (formula-ui/install! editor))
                              :markdown-teardown (quill-markdown/install! editor)
                              ;; Bound the resize module's document keydown
                              ;; listener to this editor — see
                              ;; quill-field/install-resize-release-on-focus-out!.
                              ;; Without it, an image selected here steals the
                              ;; first keystroke typed into any modal, palette or
                              ;; canvas label editor on the page.
                              :resize-focus-teardown
                              (quill-field/install-resize-release-on-focus-out! editor)
                              ;; Report the page the reader scrolls to in a
                              ;; merged document (plans/merged-page-reading.md
                              ;; §5.1). Installed unconditionally: the handler
                              ;; finds no marker outside merged mode and reports
                              ;; nothing, which costs one querySelectorAll per
                              ;; scroll event and needs no mode flag here.
                              ;; The document is read LIVE from !editor-state,
                              ;; never closed over: `update-topic-id!` changes it
                              ;; without rebuilding, which is exactly what leaving
                              ;; merged mode does.
                              :page-marker-teardown
                              (editor-actions/install-page-marker-tracking! editor
                                (fn [page]
                                  (reset! editor-actions/!scrolled-page
                                    {:doc-id (:topic-id @!editor-state)
                                     :body-key (get-in @!editor-state [:save-scope :body-key])
                                     :page page})))})
       (reset! !editor-ready? true)
       ;; Content is in the DOM (setContents above) and !editor-state is set, so
       ;; a reader woken by this bump can find the editor and its markers.
       (swap! !content-generation inc)
       ;; Same moment, for the reading position: this topic's body is in the
       ;; DOM (plans/reading-position-memory.md §4 3.6.1).
       (rp/installed! "editor" topic-id (.-root editor) fragment?)
       (setup-mobile-keyboard-suppression! editor)
       (table-ui/init! editor)
       ;; Bubble's toolbar lives in the floating .ql-tooltip inside the
       ;; container — label its pickers/buttons.
       (a11y/label-quill-toolbar! container)
       ;; Tab indents the line; Escape leaves the editor.
       (a11y/install-tab-indent! editor)
       (a11y/install-editor-escape! editor)
       editor)))

(defn init-editor-with-scope!
  "Initialize Quill editor in the given container with initial HTML.
   Destroys any existing editor first (singleton).
   Wires an immediate text-change listener that sets !dirty-html on user edits.
   topic-id tags dirty-html so auto-save targets the correct topic.

   Construction is deferred until the KaTeX wait settles (see
   freememo.math/on-katex-ready!) because Quill's Formula blot throws if KaTeX
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
  [container initial-html topic-id save-scope]
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
               (build-editor! container initial-html topic-id math? save-scope)))))
       nil)))

(defn init-editor!
  "Create an editor without an application save scope."
  [container initial-html topic-id]
  (init-editor-with-scope! container initial-html topic-id nil))

(defn set-content!
  "Update Quill content without destroying the editor.
   Uses source 'api' so text-change listener does NOT fire.
   Does NOT set !dirty-html — caller decides persistence.

   The unchanged-content check compares stored form against stored form. Reading
   the root's raw innerHTML would compare against Quill's RENDERED form, whose
   KaTeX subtrees make the lengths differ on every call once a document contains
   math — replacing the content, and losing scroll position and highlights, on
   every reactive `initial-html` change.

   `content-topic-id` is the topic `html` was fetched for. A real replacement
   reports it to the reading position, which restores only against its own
   topic's content (plans/reading-position-memory.md §4 3.6.1, 3.7)."
  ([html content-topic-id] (set-content! html content-topic-id false))
  ([html content-topic-id force?]
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
         (when (or force? (not= (count cleaned) (count current-stored)))
           (let [for-editor (if math? (math/stored->editor-html cleaned) cleaned)
                 delta (.convert clipboard #js {:html for-editor})]
             (.setText ed "" "api")
             (.setContents ed delta "api")
             (.clear (.-history ed))
             ;; A real replacement, so the reader's scroll position is gone.
             ;; Inside the `when`, never beside it: the skip above is what keeps
             ;; scroll, and a bump there would report an install that never
             ;; happened. See !content-generation.
             (swap! !content-generation inc)
             (rp/installed! "editor" content-topic-id (.-root ed))))
         true)))))

(defn install-body!
  "Replace a loaded page range only after its latest edit has saved."
  [html content-topic-id scope saved-edit-id]
  #?(:clj nil
     :cljs
     (when-let [state @!editor-state]
       (let [dirty @!dirty-html
             old-key (get-in state [:save-scope :body-key])
             new-key (:body-key scope)
             changed? (not= old-key new-key)]
         (when (merged-window/install-allowed? dirty saved-edit-id)
           (when (or changed? (nil? dirty))
             (swap! !editor-state assoc :save-scope scope)
             (set-content! html content-topic-id changed?)
             (when changed?
               (reset! !dirty-html nil)
               (reset! !last-selection nil))))))))

(defn install-saved-content!
  "Pre: before-html and after-html describe one committed change to topic-id.
   Post: install after-html only when the active editor still represents before-html.
   Invariant: preserve local edits and the editor's dirty state."
  [topic-id before-html after-html]
  #?(:clj false
     :cljs
     (if-let [{:keys [editor math?] active-topic-id :topic-id} @!editor-state]
       (if (and (= topic-id active-topic-id) (string? before-html) (string? after-html))
         (try
           (let [^js ed editor
                 ^js clipboard (.-clipboard ed)
                 ;; Convert both stored HTML values. Quill applies heading formats
                 ;; only to newlines, but its clipboard applies them to whole lines.
                 stored-delta (fn [html]
                                (.convert clipboard
                                  #js {:html (if math? (math/stored->editor-html html) html)}))
                 current (stored-delta (quill-field/editor-html ed))
                 expected (stored-delta before-html)]
             (if (zero? (alength (.-ops (.diff current expected))))
               (boolean (set-content! after-html topic-id))
               false))
           (catch :default _ false))
         false)
       false)))

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

(defn flash-range!
  "Flash a range of the Quill editor once, over the text.
   Pre:  index and length name a range of the open editor.
   Post: the document is not changed. The flash is an overlay on
         document.body, so Quill and auto-save see nothing. Returns nil."
  [index length]
  #?(:clj nil
     :cljs
     (when-let [{:keys [editor]} @!editor-state]
       (let [^js bounds (.getBounds ^js editor index length)
             ^js container (.getBoundingClientRect (.-container ^js editor))]
         (when (and bounds (pos? (.-width bounds)))
           (motion/flash-box! {:left (+ (.-left container) (.-left bounds))
                               :top (+ (.-top container) (.-top bounds))
                               :width (.-width bounds)
                               :height (.-height bounds)}))
         nil))))

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
       (publish-dirty! (quill-field/editor-html editor)))))
