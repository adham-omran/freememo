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
  "The ref spec of `topic-id` when the document editor can hold its pins, else
   nil. Only a document ref qualifies: a repository file opens the code pane."
  [user-id topic-id]
  #?(:clj (when (docref/document? (docref/for-topic user-id topic-id)) {:topic-id topic-id})
     :cljs nil))

(defn- pin-topic-id
  "The topic a document's pins are read through: the PDF root in the merged
   view, which `dctx/editor-doc-id` holds there, so a scroll from page to page
   keeps one spec; else the open topic."
  [merged? editor-doc-id topic-id]
  (if merged? editor-doc-id topic-id))

(e/defn DocumentPinSpec
  "The ref spec of the document open in the editor.
   Pre : dctx bound, as for the editor and the content toolbar.
   Post: {:topic-id id} when that document can hold pins, else nil. Nil also
         while the spec of a new topic is on its way.

   **Read the Offload through a client `e/as-vec`.** A Learn step changes
   `topic-id`, so the Offload runs a new thunk and is empty until it answers.
   Returned bare, that empty value unmounted every consumer and mounted it
   again within one round trip. On a slow tablet that churn made Electric's
   runtime read one remote signal twice (`Concurrent store reader.`), and the
   page went blank (plans/low-end-tablet-performance/issues/13-learn-session-blank-page.md)."
  []
  (e/client
    (let [user-id dctx/user-id
          topic-id (pin-topic-id (boolean dctx/merged?) dctx/editor-doc-id dctx/topic-id)]
      (peek (e/as-vec (e/server (e/Offload #(pin-spec* user-id topic-id))))))))

(e/defn EditorPinLayer
  "The pin layer of the document editor, or nothing when the open topic
   cannot have pins. A page view's scope says which page the stale rule tests
   (§3.3.2).
   Pre : mounted beside the editor, with dctx bound.
   Post: the pin list is closed whenever the document changes."
  []
  (e/client
    (let [user-id dctx/user-id
          merged? (boolean dctx/merged?)
          scope (if merged? {:merged? true} {:merged? false :topic-id dctx/topic-id})
          generation (e/watch editor/!content-generation)
          ready? (e/watch editor/!editor-ready?)
          node (when ready? (editor-root generation))
          spec (DocumentPinSpec)
          list-open? (e/watch pin-state/!document-pin-list-open?)]
      (e/for-by identity [_k (if spec [spec] [])]
        (e/on-unmount #(reset! pin-state/!document-pin-list-open? false))
        (when node
          (pin-pane/PinLayer user-id spec node :quill scope list-open?
            pin-state/!document-pin-list-open?))))))

(e/defn EditorPinControls
  "The content toolbar's Pins toggle and Ask N pins button for the document
   in the editor, after a divider, or nothing when it cannot have pins. A
   repository file has its own controls in the code pane header.
   Pre : dctx bound, as for the content toolbar."
  []
  (e/client
    (let [user-id dctx/user-id
          spec (DocumentPinSpec)]
      (when spec
        (dom/div (dom/props {:class "toolbar-group-divider"}))
        (dom/div
          (dom/props {:class "toolbar-group"})
          (pin-pane/PinHeaderControls user-id spec pin-state/!document-pin-list-open?))))))
