(ns freememo.code-pane
  "The read-only source pane of a file imported from a git forge
   (plans/git-forge-integration.md §4.4).

   Its own namespace, because Electric inlines same-namespace e/defn calls into
   the caller's method, and the document view is already split across
   namespaces for the 64KB method cap (§4.4 4.3).

   The source is REFERENCE, like a video transcript: it never enters
   `topics.content`, which holds the user's notes. Two paths carry it out.
   Copy to notes inserts the pane selection into the editor as a code block
   (§4.4 4.5). Generate reads the pane selection before the editor's, and the
   source itself when neither holds one (§4.4 4.6,
   `content-toolbar-helpers/build-gen-context*`).

   Highlighting runs in the browser through the eagerly loaded highlight.js. A
   language the bundle does not carry renders as plain text, and so does a file
   past `highlight-max-chars`, because highlight.js runs on the main thread.

   A Markdown file has a second view, rendered, which is its default
   (plans/markdown-rendered-code-pane.md). The server renders it
   (`freememo.repo-markdown`), each file remembers its view in the setting
   `code_view_<id>`, and a selection in it feeds Extract and Generate."
  (:require
   [clojure.string :as str]
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.code-languages :as code-languages]
   [freememo.doc-context :as dctx]
   [freememo.icons :as icons]
   [freememo.math :as math]
   [freememo.question-pin-pane :as pin-pane]
   [freememo.reading-position-view :refer [ReadingPosition]]
   [freememo.rich-text-editor :as editor]
   [freememo.video-format :refer [format-bytes]]
   #?(:cljs [goog.object :as gobj])
   #?(:cljs [freememo.editor-links :as editor-links])
   #?(:cljs [freememo.reading-position :as rp])
   #?(:clj [freememo.db :as db])
   #?(:clj [freememo.repo-markdown :as repo-md])
   #?(:clj [freememo.settings :as settings])))

(def ^:private highlight-max-chars
  "Largest source highlighted on the main thread. Past it the pane shows plain
   text, which still keeps every character and all indentation."
  200000)

(defn get-topic-code*
  "The pane's content for `topic-id`.
   Pre : `topic-id` is a code topic.
   Post: {:topic-id :path :source :lang :byte-size} when `user-id` owns the
         topic, else nil. `:source` is nil for an empty leaf. `:topic-id` is
         `topic-id`: the read latches across a file switch, and the reading
         position must tell the old file's source from the new one's
         (plans/reading-position-memory.md §4 3.7)."
  [_refresh user-id topic-id]
  #?(:clj (when-let [topic (db/get-topic topic-id)]
            (when (= user-id (:topics/user_id topic))
              (let [code (db/get-topic-code user-id topic-id)]
                {:topic-id topic-id
                 :path (:topics/repo_path topic)
                 :source (:source code)
                 :lang (:lang code)
                 :byte-size (:byte_size code)})))
     :cljs nil))

(defn rendered-markdown*
  "The rendered view of Markdown code topic `topic-id`.
   Post: sanitized HTML with every link resolved (`repo-md/render`) when
         `user-id` owns the topic and it has source, else nil. Two queries:
         the file's commit and forge, then every topic its links name."
  [user-id topic-id]
  #?(:clj (when-let [code (db/get-topic-code user-id topic-id)]
            (let [origin (db/get-repo-file-origin user-id topic-id)]
              (repo-md/render (:source code) (:repo_path code)
                (fn [paths]
                  (if origin (db/repo-topic-ids-by-path (:commit_id origin) paths) {}))
                (when origin
                  {:forge (:forge origin) :repo-url (:repo_url origin)
                   :commit-sha (:commit_sha origin)}))))
     :cljs nil))

(defn- selection-root
  "The element matching `selector` that holds both ends of the live selection.
   Post: [selection element], or nil when nothing is selected or the two ends
         sit in different elements, or outside any."
  [selector]
  #?(:cljs
     (when-let [sel (.getSelection js/window)]
       (when (and (pos? (.-rangeCount sel)) (not (.-isCollapsed sel)))
         (let [element-of (fn [n] (when n (if (= 1 (.-nodeType n)) n (.-parentElement n))))
               anchor (some-> (element-of (.-anchorNode sel)) (.closest selector))
               focus (some-> (element-of (.-focusNode sel)) (.closest selector))]
           (when (and anchor (identical? anchor focus))
             [sel anchor]))))
     :clj nil))

(defn pane-selection-text
  "The text the user selected inside a code pane, or nil.
   Post: nil unless the live selection is not collapsed and both of its ends
         sit inside the same `.code-pane-source` element."
  []
  #?(:cljs
     (when-let [[sel] (selection-root ".code-pane-source")]
       (not-empty (str (.toString sel))))
     :clj nil))

(defn rendered-selection-html
  "The HTML the user selected in the rendered Markdown view, or nil.
   Post: nil unless both ends of the selection sit in the same
         `.code-pane-rendered`. Formulas are TeX text again. A selection that
         lies inside one fence keeps that fence, language included, because
         a range inside a `pre` clones its text only."
  []
  #?(:cljs
     (when-let [[sel root] (selection-root ".code-pane-rendered")]
       (let [rng (.getRangeAt sel 0)
             holder (.createElement js/document "div")
             common (.-commonAncestorContainer rng)
             pre (some-> (if (= 1 (.-nodeType common)) common (.-parentElement common))
                   (.closest "pre"))]
         (.appendChild holder (.cloneContents rng))
         (when (and pre (.contains root pre))
           (let [wrapper (.cloneNode pre false)]
             (set! (.-textContent wrapper) (.-textContent holder))
             (set! (.-innerHTML holder) "")
             (.appendChild holder wrapper)))
         (math/katex->tex! holder)
         (not-empty (.-innerHTML holder))))
     :clj nil))

(defn source-block-html
  "Post: `text`, a selection from the source view, as an escaped code block —
   the form it reaches the context builder in (§4.4 4.6.1) and the form an
   extract's `repo-md/selection->extract-html` turns into a Quill code block.
   `freememo.selected-passage` decides when the source view is the surface."
  [text]
  (str "<pre><code>" (str/escape text {\& "&amp;" \< "&lt;" \> "&gt;"}) "</code></pre>"))

(defn render-source!
  "Write the source of `body` into the `<code>` element `node`.
   Pre : `body` is {:topic-id :source}, both from one `get-topic-code*` read.
   Post: the node's text equals the source. It is highlighted when highlight.js
         carries `lang` and the source is within `highlight-max-chars`;
         highlight.js escapes what it emits. The reading position learns that
         this node now holds that topic's source. Returns nil."
  [node {:keys [topic-id source]} lang]
  #?(:cljs
     (let [source (or source "")
           hljs (gobj/get js/window "hljs")]
       (if (and hljs lang (<= (count source) highlight-max-chars) (.getLanguage hljs lang))
         (set! (.-innerHTML node)
           (gobj/get (.highlight hljs source #js {:language lang :ignoreIllegals true}) "value"))
         (set! (.-textContent node) source))
       (rp/installed! "code-source" topic-id node))
     :clj nil)
  nil)

(defonce ^:private !pending-anchor
  ;; The fragment a click on a link to another repo file carried. It outlives
  ;; the navigation, because the target's HTML has not arrived yet, and
  ;; `show-rendered!` spends it. The notes editor keeps its own, and would
  ;; spend this one first against the wrong root if the two were shared.
  (atom nil))

(defonce ^:private !spent-hash
  ;; [topic-id hash] of the address-bar fragment `show-rendered!` last landed
  ;; on. A fragment wins once (plans/reading-position-memory.md §4 5.3): every
  ;; later render of that topic, such as a switch back from the source view,
  ;; opens at the reader's saved place instead of jumping to the fragment again.
  (atom nil))

(defn- highlight-fences!
  "Highlight each `pre[data-language]` under `root` that highlight.js carries,
   within `highlight-max-chars` per block, as `render-source!` does for a file."
  [root]
  #?(:cljs
     (when-let [hljs (gobj/get js/window "hljs")]
       (doseq [code (js/Array.from (.querySelectorAll root "pre[data-language] > code"))]
         (let [lang (.getAttribute (.-parentElement code) "data-language")
               source (.-textContent code)]
           (when (and (.getLanguage hljs lang) (<= (count source) highlight-max-chars))
             (set! (.-innerHTML code)
               (gobj/get (.highlight hljs source #js {:language lang :ignoreIllegals true}) "value"))
             (.add (.-classList code) "hljs")))))
     :clj nil)
  nil)

(defn show-rendered!
  "Write the rendered Markdown of `rendered` into `node`.
   Pre : `rendered` is {:topic-id :html}, both from one read.
   Post: math is typeset (once KaTeX is available), fences are highlighted, and
         a fragment left by a cross-file link — or the address bar's, on a
         direct load — is scrolled to once. The address bar's fragment is spent
         for that topic. The reading position learns that `node` now holds the
         topic's document, and that a fragment owns this render's scroll when
         one was scrolled to. Returns nil."
  [node {:keys [topic-id html]}]
  #?(:cljs
     (do (math/set-html! node html)
         (highlight-fences! node)
         (when (seq html)
           (let [hash (some-> (.. js/window -location -hash) (subs 1) js/decodeURIComponent
                        not-empty)
                 hash (when (and hash (not= [topic-id hash] @!spent-hash)) hash)
                 id (or (some-> @!pending-anchor not-empty) hash)]
             (when hash (reset! !spent-hash [topic-id hash]))
             (when id
               (reset! !pending-anchor nil)
               (js/requestAnimationFrame #(editor-links/scroll-to-anchor! node id)))
             (rp/installed! "code-rendered" topic-id node (some? id)))))
     :clj nil)
  nil)

(defn follow-rendered-link!
  "Act on a click inside the rendered view (§2.6). The browser would follow the
   href as a full page load, so every link is handled here instead.
   Post: `#x` scrolls within `root`; `/viewer/topic/<id>` navigates in the app,
         or opens a tab with a modifier held; any other link opens a tab."
  [e root]
  #?(:cljs
     (when-let [a (some-> (.-target e) (.closest "a[href]"))]
       (when (.contains root a)
         (let [href (.getAttribute a "href")
               modified? (or (.-metaKey e) (.-ctrlKey e) (.-shiftKey e))
               [_ topic-id fragment] (re-matches #"/viewer/topic/(\d+)(?:#(.*))?" href)]
           (.preventDefault e)
           (cond
             (str/starts-with? href "#")
             (editor-links/scroll-to-anchor! root (js/decodeURIComponent (subs href 1)))

             (and topic-id (not modified?))
             (do (reset! !pending-anchor (some-> fragment js/decodeURIComponent))
                 (reset! editor/!navigate-topic {:topic-id (js/parseInt topic-id)
                                                 :ts (js/Date.now)}))

             :else (js/window.open href "_blank" "noopener")))))
     :clj nil)
  nil)

(defn copy-selection-to-notes!
  "Insert the pane selection at the end of the editor as one code block
   (§4.4 4.5).

   Post: :copied when text was inserted, else :no-selection or :no-editor. The
         insert's source is \"user\", so auto-save persists it and undo reaches it.
   Invariant: the last line of the notes keeps its own format. A block format
         lives on the newline that ends a line, so the block goes on a new empty
         line; inserting into a line that holds text would turn it into code."
  [lang]
  #?(:cljs
     (let [text (pane-selection-text)
           ;; ^js, or :advanced may rename getLength/getText/insertText — Quill
           ;; ships no externs, and the :prod build reported all five calls.
           ^js quill (editor/current-editor)]
       (cond
         (nil? text) :no-selection
         (nil? quill) :no-editor
         :else
         (let [block-lang (if (contains? code-languages/code-language-keys lang) lang "plain")
               body (if (str/ends-with? text "\n") text (str text "\n"))
               len (.getLength quill)]
           (when (and (> len 1) (not= "\n" (.getText quill (- len 2) 1)))
             (.insertText quill (dec len) "\n" "user"))
           (.insertText quill (dec (.getLength quill)) body #js {"code-block" block-lang} "user")
           :copied)))
     :clj nil))

(e/defn ViewToggle
  "The rendered/source switch in the header of a Markdown file (§3.3).
   Pre : `view` is \"rendered\" or \"source\"; `set-view!` records a choice."
  [view set-view!]
  (e/client
    (dom/button
      (dom/props {:class "btn btn-sm btn-secondary"
                  :aria-label (if (= "rendered" view) "Show the Markdown source" "Show the rendered document")})
      (dom/span (dom/props {:class "icon-label"})
        (dom/text (if (= "rendered" view) "Source" "Rendered")))
      (dom/On "click" (fn [_] (set-view! (if (= "rendered" view) "source" "rendered"))) nil))))

(e/defn RenderedMarkdown
  "The rendered view of a Markdown file (§2), and its question pins
   (plans/anchored-questions.md §6.2).
   Pre : `rendered` is {:topic-id :html}, `:html` being `rendered-markdown*`'s
         result. A server value.
   Inv : the pane's div holds only the rendered blocks. A pin location names a
         block by its index among them, so `PinLayer` mounts beside the div."
  [rendered user-id pin-list-open? !pin-list-open?]
  (e/client
    (let [[node _] (dom/div
                     (dom/props {:class "code-pane-rendered" :tabindex "0"})
                     (let [node dom/node]
                       (dom/On "click" (fn [e] (follow-rendered-link! e node)) nil)
                       [node (show-rendered! node rendered)]))]
      (pin-pane/PinLayer user-id (:topic-id rendered) node pin-list-open?
        !pin-list-open?))))

(e/defn CodePane
  "Pre : dctx/page-topic-id names a code topic (dctx/is-code?).
   Post: the file's source renders read-only with its whitespace intact. An
         empty leaf renders a notice in place of the source. A Markdown file
         renders as a document instead, unless its saved view is \"source\"."
  []
  (e/client
    (let [user-id dctx/user-id
          topic-id dctx/page-topic-id
          ;; SERVER-FORM binding: the source crosses once, through the one read
          ;; below, and the metadata crosses as short strings.
          code (e/server (e/Offload #(get-topic-code* nil user-id topic-id)))
          lang (e/server (:lang code))
          source? (e/server (some? (:source code)))
          markdown? (= "markdown" lang)
          saved-view (e/server (when (= "markdown" (:lang code))
                                 (e/Offload #(settings/get-code-view user-id topic-id))))
          ;; This session's choices, per topic: the pane stays mounted when the
          ;; route moves to another file.
          !views (atom {})
          !view-save (atom nil)
          view-save (e/watch !view-save)
          [t-view _] (e/Token view-save)
          set-view! (fn [v]
                      (swap! !views assoc topic-id v)
                      (reset! !view-save {:topic-id topic-id :view v :id (random-uuid)}))
          view (or (get (e/watch !views) topic-id) saved-view)
          rendered? (and markdown? source? (= "rendered" view))
          ;; Nothing until a Markdown file's saved view is known, so a file
          ;; kept on "source" does not flash rendered first.
          show-source? (or (not markdown?) (= "source" view))
          ;; SERVER-FORM binding, gated inside: the HTML crosses once, and only
          ;; while the rendered view shows (§2.2).
          ;; The HTML travels with the topic it was rendered for, for the same
          ;; reason `get-topic-code*` returns `:topic-id`.
          rendered (e/server (when rendered?
                               (e/Offload #(when-let [h (rendered-markdown* user-id topic-id)]
                                             {:topic-id topic-id :html h}))))
          !notice (atom nil)
          notice (e/watch !notice)
          !pin-list-open? (atom false)
          pin-list-open? (e/watch !pin-list-open?)]
      (e/drain
        (when t-view
          (let [save-topic (:topic-id view-save)
                save-view (:view view-save)
                r (e/server (e/Offload #(settings/save-code-view user-id save-topic save-view)))]
            (when (some? r)
              (if (:success r) (case r (t-view)) (t-view (:error r)))))))
      (dom/div
        (dom/props {:class "code-pane"})
        (dom/div
          (dom/props {:class "code-pane__header"})
          (dom/span
            (dom/props {:class "code-pane__path" :dir "ltr"})
            (dom/text (e/server (or (:path code) ""))))
          (dom/span
            (dom/props {:class "code-pane__meta"})
            (dom/text (e/server (str/join " · " (remove nil? [(:lang code)
                                                             (some-> (:byte-size code) format-bytes)])))))
          (when (and markdown? source? view)
            (ViewToggle view set-view!))
          (when rendered?
            (pin-pane/PinHeaderControls user-id topic-id !pin-list-open?))
          ;; Copy to notes copies source lines, which the rendered view has
          ;; none of (§2.8). A hidden editor pane would take the lines out of
          ;; sight (plans/editor-pane-toggle.md §4.3).
          (when-not (or rendered? dctx/editor-hidden?)
            (dom/button
              (dom/props {:class "btn btn-sm btn-secondary"
                          :aria-label "Copy the selected lines to your notes"})
              (icons/Icon :clipboard :size 14)
              (dom/span (dom/props {:class "icon-label"}) (dom/text "Copy to notes"))
              (dom/On "click"
                (fn [_]
                  (reset! !notice
                    (case (copy-selection-to-notes! lang)
                      :copied nil
                      :no-selection "Select lines in the code first."
                      :no-editor "The editor is not ready. Try again."
                      nil)))
                nil))))
        (when notice
          (dom/div
            (dom/props {:class "code-pane__notice" :role "status"})
            (dom/text notice)))
        (cond
          (not source?)
          (dom/div
            (dom/props {:class "code-pane__empty"})
            (dom/text (str "No source is stored for this file. Its extension was not "
                        "selected at import, or the file is binary or larger than 1 MB.")))

          rendered? (RenderedMarkdown rendered user-id pin-list-open? !pin-list-open?)

          show-source?
          (dom/pre
            (dom/props {:class "code-pane-source" :dir "ltr" :tabindex "0"})
            (dom/code
              (dom/props {:class "hljs"})
              (render-source! dom/node (e/server (select-keys code [:topic-id :source])) lang)))))

      ;; One position per view (plans/reading-position-memory.md §4 3.5.4). A
      ;; view switch changes the key, so the old view's tracker flushes and the
      ;; new view's restores. No view shows while a Markdown file's saved view
      ;; is unknown, so no tracker mounts then.
      (let [surface (cond rendered? "code-rendered" show-source? "code-source")]
        (ReadingPosition surface (when (and source? surface) topic-id))))))
