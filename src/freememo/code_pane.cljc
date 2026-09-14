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
   past `highlight-max-chars`, because highlight.js runs on the main thread."
  (:require
   [clojure.string :as str]
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.code-languages :as code-languages]
   [freememo.doc-context :as dctx]
   [freememo.icons :as icons]
   [freememo.rich-text-editor :as editor]
   [freememo.video-format :refer [format-bytes]]
   #?(:cljs [goog.object :as gobj])
   #?(:clj [freememo.db :as db])))

(def ^:private highlight-max-chars
  "Largest source highlighted on the main thread. Past it the pane shows plain
   text, which still keeps every character and all indentation."
  200000)

(defn get-topic-code*
  "The pane's content for `topic-id`.
   Pre : `topic-id` is a code topic.
   Post: {:path :source :lang :byte-size} when `user-id` owns the topic, else
         nil. `:source` is nil for an empty leaf."
  [_refresh user-id topic-id]
  #?(:clj (when-let [topic (db/get-topic topic-id)]
            (when (= user-id (:topics/user_id topic))
              (let [code (db/get-topic-code user-id topic-id)]
                {:path (:topics/repo_path topic)
                 :source (:source code)
                 :lang (:lang code)
                 :byte-size (:byte_size code)})))
     :cljs nil))

(defn pane-selection-text
  "The text the user selected inside a code pane, or nil.
   Post: nil unless the live selection is not collapsed and both of its ends
         sit inside the same `.code-pane-source` element."
  []
  #?(:cljs
     (when-let [sel (.getSelection js/window)]
       (when (and (pos? (.-rangeCount sel)) (not (.-isCollapsed sel)))
         (let [element-of (fn [n] (when n (if (= 1 (.-nodeType n)) n (.-parentElement n))))
               anchor (some-> (element-of (.-anchorNode sel)) (.closest ".code-pane-source"))
               focus (some-> (element-of (.-focusNode sel)) (.closest ".code-pane-source"))]
           (when (and anchor (identical? anchor focus))
             (not-empty (str (.toString sel)))))))
     :clj nil))

(defn pane-selection-html
  "Post: the pane selection as an escaped code block — the form a pane selection
   reaches the context builder in (§4.4 4.6.1) — or nil when nothing is
   selected in the pane."
  []
  (when-let [text (pane-selection-text)]
    (str "<pre><code>" (str/escape text {\& "&amp;" \< "&lt;" \> "&gt;"}) "</code></pre>")))

(defn render-source!
  "Write `source` into the `<code>` element `node`.
   Post: the node's text equals `source`. It is highlighted when highlight.js
         carries `lang` and `source` is within `highlight-max-chars`; highlight.js
         escapes what it emits. Returns nil."
  [node source lang]
  #?(:cljs
     (let [source (or source "")
           hljs (gobj/get js/window "hljs")]
       (if (and hljs lang (<= (count source) highlight-max-chars) (.getLanguage hljs lang))
         (set! (.-innerHTML node)
           (gobj/get (.highlight hljs source #js {:language lang :ignoreIllegals true}) "value"))
         (set! (.-textContent node) source)))
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

(e/defn CodePane
  "Pre : dctx/page-topic-id names a code topic (dctx/is-code?).
   Post: the file's source renders read-only with its whitespace intact. An
         empty leaf renders a notice in place of the source."
  []
  (e/client
    (let [user-id dctx/user-id
          topic-id dctx/page-topic-id
          ;; SERVER-FORM binding: the source crosses once, through the one read
          ;; below, and the metadata crosses as short strings.
          code (e/server (e/Offload #(get-topic-code* nil user-id topic-id)))
          lang (e/server (:lang code))
          source? (e/server (some? (:source code)))
          !notice (atom nil)
          notice (e/watch !notice)]
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
              nil)))
        (when notice
          (dom/div
            (dom/props {:class "code-pane__notice" :role "status"})
            (dom/text notice)))
        (if source?
          (dom/pre
            (dom/props {:class "code-pane-source" :dir "ltr" :tabindex "0"})
            (dom/code
              (dom/props {:class "hljs"})
              (render-source! dom/node (e/server (:source code)) lang)))
          (dom/div
            (dom/props {:class "code-pane__empty"})
            (dom/text (str "No source is stored for this file. Its extension was not "
                        "selected at import, or the file is binary or larger than 1 MB."))))))))
