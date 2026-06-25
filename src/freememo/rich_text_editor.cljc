(ns freememo.rich-text-editor
  "Quill rich text editor integration — global singleton."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.logging :as log]
   [freememo.quill-table-ui :as table-ui]
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
       (when-let [{:keys [editor container toolbar]} @!editor-state]
         (let [^js ed editor
               ^js ct container
               ^js tb toolbar]
           ;; Tear down the table action bar (appended to document.body)
           ;; and its window scroll/resize listeners. Idempotent; safe
           ;; even if table-ui/init! never ran.
           (table-ui/teardown! ed)
           (.off ed "text-change")
           (.off ed "selection-change")
           (remove-watch !import-status ::tooltip-btn)
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

(defn- setup-link-import-button!
  "Inject an 'Import' button into Quill Snow tooltip for Wikipedia links.
   On selection-change, show/hide the button based on whether the link is Wikipedia.
   Watches !import-status to update button text during import."
  [editor]
  #?(:cljs
     (let [^js ed editor
           ^js theme (.-theme ed)]
       (when-let [^js tooltip (.-tooltip theme)]
         (let [^js root (.-root tooltip)
               ^js import-btn (js/document.createElement "a")
               ;; Bubble has no .ql-preview (snow's link editor); the link href
               ;; is captured from the selection's DOM node in selection-change
               ;; and stashed here for the click handler. Theme-agnostic.
               !link-href (atom nil)]
           (set! (.-className import-btn) "ql-import")
           (set! (.-textContent import-btn) "Import")
           (.setAttribute import-btn "style" "display:none")
           (.appendChild root import-btn)
           ;; Click handler: import the captured Wikipedia URL.
           (.addEventListener import-btn "click"
             (fn [e]
               (.preventDefault e)
               (when-not (= @!import-status :importing)
                 (let [href @!link-href]
                   (when (wikipedia-url? href)
                     (reset! !import-status :importing)
                     (set! (.-textContent import-btn) "Importing...")
                     (.add (.-classList import-btn) "importing")
                     (reset! !import-url {:url href :ts (js/Date.now)}))))))
           ;; Watch import status to update button text
           (add-watch !import-status ::tooltip-btn
             (fn [_ _ _ new-status]
               (case new-status
                 :importing (do (set! (.-textContent import-btn) "Importing...")
                              (.add (.-classList import-btn) "importing"))
                 :done (do (set! (.-textContent import-btn) "Imported!")
                         (.remove (.-classList import-btn) "importing")
                         (js/setTimeout #(do (set! (.-textContent import-btn) "Import")
                                           (reset! !import-status :idle)) 2000))
                 :already-exists (do (set! (.-textContent import-btn) "Already imported")
                                   (.remove (.-classList import-btn) "importing")
                                   (js/setTimeout #(do (set! (.-textContent import-btn) "Import")
                                                     (reset! !import-status :idle)) 2000))
                 :error (do (set! (.-textContent import-btn) "Error")
                          (.remove (.-classList import-btn) "importing")
                          (js/setTimeout #(do (set! (.-textContent import-btn) "Import")
                                            (reset! !import-status :idle)) 2000))
                 ;; :idle — reset
                 (do (set! (.-textContent import-btn) "Import")
                   (.remove (.-classList import-btn) "importing")))))
           ;; On selection change, detect a Wikipedia link at the selection,
           ;; stash its href, and show the Import button in the bubble tooltip.
           (.on ed "selection-change"
             (fn [^js range _old-range _source]
               (let [href (when range
                            (let [idx (.-index range)
                                  ^js leaf-arr (.getLeaf ed idx)
                                  ^js leaf (when leaf-arr (aget leaf-arr 0))
                                  ^js parent (when leaf (.-parent leaf))
                                  ^js dom-node (when parent (.-domNode parent))
                                  tag (when dom-node (.-tagName dom-node))]
                              (when (= "A" tag)
                                (.getAttribute dom-node "href"))))]
                 (reset! !link-href (when (wikipedia-url? href) href))
                 (.setAttribute import-btn "style"
                   (if (wikipedia-url? href) "" "display:none"))))))))
     :clj nil))

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
                                :topic-id topic-id :toolbar nil})
         (setup-link-import-button! editor)
         (setup-mobile-keyboard-suppression! editor)
         (table-ui/init! editor)
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
