(ns freememo.editor-pin-layer
  "Question pins on the document editor (plans/document-question-pins.md §3.4):
   the pin layer of the Quill editor that `EditorPane` mounts, in the per-page
   view and the merged view alike, and the Pins and Ask N pins controls the
   content toolbar mounts.

   Both read the document through `DocumentPinSpec`, so the toolbar counts the
   pins the editor shows. They share the list state through
   `question-pin-state/!document-pin-list-open?`.

   The editor is plain JS and its root, `.ql-editor`, is not an Electric node.
   So the layer finds the root through the editor's install generation, and
   mounts BESIDE the editor, never inside it, as on the code pane.

   Its own namespace for the 64KB method cap: `EditorPane` is close to it
   (see `freememo.merged-page-bridge`)."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.doc-context :as dctx]
   [freememo.question-pin-state :as pin-state]
   [freememo.question-pin-pane :as pin-pane]
   [freememo.rich-text-editor :as editor]
   #?(:clj [freememo.document-ref :as docref])))

(defn- editor-root
  "The live editor's `.ql-editor`, or nil. `_generation` is the editor's
   install generation: a call with no input is evaluated once and held, and
   that once is before the editor exists (`merged-page-bridge`, ARRIVE)."
  [_generation]
  #?(:cljs (some-> (editor/current-editor) .-root) :clj nil))

(defn pin-spec*
  "The answer {:for topic-id :spec s}. `s` is the ref spec of the topic when
   the document editor can hold its pins, else nil. Only a document ref
   qualifies: a repository file opens the code pane. `:for` names the topic
   the answer is for, so the client can tell an answer for the previous topic
   from one for the topic it shows."
  [user-id topic-id]
  #?(:clj {:for topic-id
           :spec (when (and topic-id (docref/document? (docref/for-topic user-id topic-id)))
                   {:topic-id topic-id})}
     :cljs nil))

(defn pin-topic-id
  "The topic a document's pins are read through: the PDF root in the merged
   view, which `dctx/editor-doc-id` holds there, so a scroll from page to page
   keeps one spec. Else the topic whose body the editor shows
   (`dctx/effective-content-topic-id`). After a Learn Next, that id stays on
   the previous topic until the next body arrives, so the pins never show
   over the text of another topic (plans/learn-next-speed/issues/04-pin-layers-keep-frames.md)."
  [merged? editor-doc-id content-topic-id]
  (if merged? editor-doc-id content-topic-id))

(defn pin-scope
  "The view scope of the editor's pin layer (plans/document-question-pins.md
   §3.3.2): a page view's scope says which page the stale rule tests."
  [merged? topic-id]
  (if merged? {:merged? true} {:merged? false :topic-id topic-id}))

(defn held-spec
  "The spec that the editor and the toolbar show for `topic-id`. `answer` is
   the latest {:for :spec} answer, or nil.
   Post: the answer's spec when it is for `topic-id`. Else, while the read for
         `topic-id` runs: {:topic-id topic-id} when the previous spec was a
         document's, nil when it was nil. A document's spec is always
         {:topic-id id}, so this guess is the answer to come whenever the next
         topic is a document too. The spec then moves in the same frame as
         the body, a round trip before the answer, and the toolbar controls
         and the layer keep their frames. A wrong guess (a repository file
         after a document) lasts one round trip; the server checks every spec.
   Inv : a plain fn, for `e/with-cycle`."
  [held topic-id answer]
  (cond
    (and (some? answer) (= topic-id (:for answer))) (:spec answer)
    (and (some? held) (some? topic-id)) {:topic-id topic-id}
    :else nil))

(defn controls-key
  "The key list of the toolbar pin controls: one constant key while the
   document can have pins, else none.
   Inv : a plain fn, so a new spec emits no diff and the controls keep their
         frames."
  [spec]
  (if spec [::controls] []))

(defn close-list-on-change!
  "Close the pin list when the editor shows the pins of another document.
   `!last` holds the spec of the last call. Post: nil."
  [!last !list-open? spec]
  (when (not= spec @!last)
    (when (some? @!last) (reset! !list-open? false))
    (reset! !last spec))
  nil)

(e/defn DocumentPinSpec
  "The ref spec of the document open in the editor.
   Pre : dctx bound, as for the editor and the content toolbar.
   Post: {:topic-id id} when that document can hold pins, else nil. While the
         answer for a new topic is on its way, see `held-spec`.

   **Read the Offload through a client `e/as-vec`.** A Learn step changes
   `topic-id`, so the Offload runs a new thunk and is empty until it answers.
   Returned bare, that empty value unmounted every consumer and mounted it
   again within one round trip. On a slow tablet that churn made Electric's
   runtime read one remote signal twice (`Concurrent store reader.`), and the
   page went blank (plans/low-end-tablet-performance/issues/13-learn-session-blank-page.md).

   **Do not wait for the read.** The `e/as-vec` reads the run as nil, a round
   trip after the topic changed. Returned as is, that nil unmounted the
   toolbar pin controls on each Learn Next, and the old spec showed the
   previous topic's pins over the next body for a round trip. `held-spec`
   gives the next spec at once (plans/learn-next-speed/issues/04-pin-layers-keep-frames.md)."
  []
  (e/client
    (let [user-id dctx/user-id
          topic-id (pin-topic-id (boolean dctx/merged?) dctx/editor-doc-id
                     dctx/effective-content-topic-id)
          answer (peek (e/as-vec (e/server (e/Offload #(pin-spec* user-id topic-id)))))]
      (e/with-cycle [held nil]
        (held-spec held topic-id answer)))))

(e/defn EditorPinLayer
  "The pin layer of the document editor, or nothing when the open topic
   cannot have pins. A page view's scope says which page the stale rule tests
   (§3.3.2).
   Pre : mounted beside the editor, with dctx bound.
   Post: the pin list is closed whenever the document changes. A Learn Next
         between two documents keeps the layer's frames: `PinLayer` is keyed
         by the editor root, and the spec and the scope are values."
  []
  (e/client
    (let [user-id dctx/user-id
          merged? (boolean dctx/merged?)
          scope (pin-scope merged?
                  (pin-topic-id merged? dctx/editor-doc-id dctx/effective-content-topic-id))
          generation (e/watch editor/!content-generation)
          ready? (e/watch editor/!editor-ready?)
          node (when ready? (editor-root generation))
          spec (DocumentPinSpec)
          list-open? (e/watch pin-state/!document-pin-list-open?)
          !last-spec (atom nil)]
      ;; An element of its own for the layer's keyed loop, which mounts beside
      ;; the editor in the editor host (docs/electric-patterns.md, "An `e/for`
      ;; Gets an Element of Its Own"). `display: contents` (index.css).
      (dom/div
        (dom/props {:class "editor-pin-layer"})
        (e/on-unmount #(reset! pin-state/!document-pin-list-open? false))
        (pin-pane/PinLayer user-id spec node :quill scope list-open?
          pin-state/!document-pin-list-open?)
        (close-list-on-change! !last-spec pin-state/!document-pin-list-open? spec)))))

(e/defn EditorPinControls
  "The content toolbar's Pins toggle and Ask N pins button for the document
   in the editor, after a divider, or nothing when it cannot have pins. A
   repository file has its own controls in the code pane header.
   Pre : dctx bound, as for the content toolbar.
   Post: a Learn Next between two documents keeps the controls' frames. The
         key is constant while the spec is present, so the new spec arrives
         as a value."
  []
  (e/client
    (let [user-id dctx/user-id
          spec (DocumentPinSpec)]
      ;; `display: contents` (index.css): the divider and the group stay
      ;; layout children of the toolbar.
      (dom/div
        (dom/props {:class "editor-pin-controls"})
        (e/for-by identity [_k (controls-key spec)]
          (dom/div (dom/props {:class "toolbar-group-divider"}))
          (dom/div
            (dom/props {:class "toolbar-group"})
            (pin-pane/PinHeaderControls user-id spec pin-state/!document-pin-list-open?)))))))
