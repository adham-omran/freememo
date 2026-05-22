(ns freememo.quill-field
  "Parameterized Quill editor field — reusable across card-edit modal and any
   other site needing an inline HTML editor. NOT coupled to topic auto-save.
   Callers pass a string value and an on-change callback.

   API:
     (QuillField {:value-string \"<p>Hello</p>\"
                  :on-change    (fn [html] ...)
                  :placeholder  \"Enter text...\"
                  :field-key    :some-stable-key})"
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.quill-table-ui :as table-ui]
   [clojure.string :as str]))

;; ---------------------------------------------------------------------------
;; Quill config — shared with rich_text_editor.cljc via this defn.
;; ---------------------------------------------------------------------------

(defn quill-config
  "Returns the Quill constructor options map (passed via clj->js).
   Same toolbar, syntax module, and modules as the main editor so the card
   modals offer the identical formatting surface, including code-block with
   highlight.js syntax colouring."
  [placeholder]
  {:theme "snow"
   :modules {:toolbar [["bold" "italic" "underline" "strike"]
                       [{"header" 1} {"header" 2} {"header" 3}]
                       [{"size" ["small" false "large" "huge"]}]
                       [{"color" []} {"background" []}]
                       [{"list" "ordered"} {"list" "bullet"}]
                       [{"align" []}]
                       [{"direction" "rtl"}]
                       ["code-block"]
                       ["clean"]
                       ["image"]
                       ["table"]]
             ;; Syntax module — requires window.hljs + clojure pack (loaded in
             ;; index*.html). Same `:languages` list as the main editor for
             ;; consistent picker UX across modals and the page editor.
             :syntax {:languages [{:key "plain"      :label "Plain"}
                                  {:key "bash"       :label "Bash"}
                                  {:key "cpp"        :label "C++"}
                                  {:key "cs"         :label "C#"}
                                  {:key "clojure"    :label "Clojure"}
                                  {:key "css"        :label "CSS"}
                                  {:key "diff"       :label "Diff"}
                                  {:key "xml"        :label "HTML/XML"}
                                  {:key "java"       :label "Java"}
                                  {:key "javascript" :label "JavaScript"}
                                  {:key "markdown"   :label "Markdown"}
                                  {:key "php"        :label "PHP"}
                                  {:key "python"     :label "Python"}
                                  {:key "ruby"       :label "Ruby"}
                                  {:key "sql"        :label "SQL"}]}
             :table true}
   :placeholder (or placeholder "Enter text...")})

;; ---------------------------------------------------------------------------
;; Image-upload paste helper — CLJS only, defined at the module level.
;; ---------------------------------------------------------------------------

(defn upload-pasted-image!
  "Upload a data-URI blob to /api/upload-media.
   Calls on-uploaded with the /api/media/<id> URL on success, nil on error.
   CLJS only."
  [data-uri on-uploaded]
  #?(:cljs
     (let [parts (str/split data-uri #",")
           header (first parts)
           b64 (second parts)
           mime (-> header
                  (str/replace "data:" "")
                  (str/replace ";base64" ""))
           bin-str (js/atob b64)
           n (count bin-str)
           buf (js/Uint8Array. n)
           _ (dotimes [i n]
               (aset buf i (.charCodeAt bin-str i)))
           blob (js/Blob. (clj->js [buf]) (clj->js {:type mime}))
           ext (last (str/split mime #"/"))
           form (js/FormData.)]
       (.append form "file" blob (str "image." ext))
       (-> (js/fetch "/api/upload-media"
             (clj->js {:method "POST"
                       :credentials "same-origin"
                       :body form}))
         (.then (fn [r]
                  (if (.-ok r)
                    (.json r)
                    (throw (js/Error. (str "Upload failed: " (.-status r)))))))
         (.then (fn [json]
                  (let [id (.-id json)]
                    (when id
                      (on-uploaded (str "/api/media/" id))))))
         (.catch (fn [err]
                   (js/console.warn "[QuillField] image upload failed:" (str err))
                   (on-uploaded nil)))))
     :clj nil))

;; ---------------------------------------------------------------------------
;; Quill instance lifecycle — plain defns so side effects are stable.
;; ---------------------------------------------------------------------------

(defn init-quill-field!
  "Initialize a standalone Quill editor in container with initial-html.
   Wires text-change to call on-change with updated innerHTML on each user edit.
   Wires a clipboard matcher that uploads data-URI images before inserting them.
   Returns the Quill instance (CLJS) or nil (CLJ).
   No ^js on parameters — CLJ compiler sees the parameter list."
  [container initial-html placeholder on-change]
  #?(:cljs
     (when (and container (.-Quill js/window))
       (let [Quill (.-Quill js/window)
             cfg (clj->js (quill-config placeholder))
             ed (new Quill container cfg)
             cb (.-clipboard ed)
             ;; Tell Quill's clipboard pipeline to ignore the syntax module's
             ;; language-picker <select>. Without this, Quill's default
             ;; handling extracts <option> labels as text and emits a <p> with
             ;; the concatenated picker labels on every reload. Returning an
             ;; empty Delta makes the converter treat the <select> subtree as
             ;; if it didn't exist; container + child line divs are processed
             ;; by their own matchers and continue to render as multi-line
             ;; code blocks.
             Delta (.import (.-Quill js/window) "delta")
             _ (.addMatcher cb "select.ql-ui"
                 (fn [_node _delta] (new Delta)))
             ;; Strip Quill syntax-module token wrappers. CodeToken.formats
             ;; in Quill 2.0.3 returns boolean `true` instead of the actual
             ;; hljs-X value (rendering as `class="hljs-true"` and merging
             ;; adjacent same-value inlines). Returning a Delta with just
             ;; the textContent hands Quill clean text; the syntax module
             ;; re-applies fresh tokens on its 1 s timer.
             _ (.addMatcher cb "span.ql-token"
                 (fn [^js node _delta]
                   (-> (new Delta) (.insert (.-textContent node)))))
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
         ;; text-change: call on-change whenever user edits
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
                       (upload-pasted-image!
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
         ed))
     :clj nil))

(defn destroy-quill-field!
  "Destroy a QuillField Quill instance and clean up its DOM. No ^js params —
   ^js hints live on let-bindings inside the :cljs branch."
  [ed]
  #?(:cljs
     (when ed
       (let [^js ed ed]
         ;; Remove table action bar from document.body and unregister
         ;; window scroll/resize listeners. Idempotent.
         (table-ui/teardown! ed)
         (.off ed "text-change")
         (let [^js container (.-container ed)]
           (when container
             (let [^js parent (.-parentNode container)]
               (when parent
                 (let [^js toolbar (.querySelector parent ".ql-toolbar")]
                   (when toolbar
                     (.remove toolbar)))))
             (set! (.-innerHTML container) "")))))
     :clj nil))

(defn schedule-quill-init!
  "Schedule Quill init on the next tick (CLJS-only side effect).
   Wrapper exists so the reader conditional lives in a plain defn — keeps
   CLJ/CLJS signal counts identical inside the e/defn reactive body
   (CLAUDE.md frame-mismatch rule)."
  [!ed-state container value-string placeholder on-change]
  #?(:cljs (js/setTimeout
             (fn []
               (reset! !ed-state
                 (init-quill-field! container value-string placeholder on-change)))
             0)
     :clj nil))

;; ---------------------------------------------------------------------------
;; Electric component
;; ---------------------------------------------------------------------------

(e/defn QuillField
  "Standalone Quill editor field.

   Parameters: single opts map.
     :value-string  string  Initial HTML content (mount-time only).
     :on-change     fn      Called with updated HTML string on every user edit.
     :placeholder   string  Editor placeholder text.
     :field-key     any     Stable key for e/for-by frame isolation.
                             When this changes the Quill instance remounts.
                             Use card-id + field-name e.g. [:question card-id].

   Usage:
     (QuillField {:value-string (or (:question card) \"\")
                  :on-change    (fn [html] (reset! !question html))
                  :placeholder  \"Question...\"
                  :field-key    [:question card-id]})"
  [{:keys [value-string on-change placeholder field-key]}]
  (e/client
    ;; e/for-by provides frame isolation: Quill remounts when field-key changes.
    (e/for-by identity [_k [(or field-key :quill-field)]]
      (dom/div
        (dom/props {:class "quill-editor-wrapper quill-field-wrapper"})
        (let [!ed-state (atom nil)]
          ;; Schedule init via plain defn — reader conditional lives there,
          ;; not in this reactive body (avoids CLJ/CLJS signal-count mismatch).
          (schedule-quill-init! !ed-state dom/node value-string placeholder on-change)
          (e/on-unmount
            (fn []
              (destroy-quill-field! @!ed-state)
              (reset! !ed-state nil))))))))
