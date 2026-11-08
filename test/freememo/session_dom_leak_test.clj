(ns freememo.session-dom-leak-test
  "Source-shape contract for the Learn session's dead editors
   (plans/low-end-tablet-performance/issues/20-session-dom-growth.md).

   Electric v3 keeps each frame that crossed the wire in its socket for the
   whole session, with the last value of each signal. Old frames of
   `RichTextEditorComponent` and `PinLayer` therefore keep the container and the
   `.ql-editor` root of each dead editor, and Quill reaches its whole instance
   from both. App code cannot drop those references, so:

   1. `destroy-editor!` empties what Quill built, after the DOM left the page.
   2. Every listener whose closure holds the root is removed.
   3. `PinLayer` reads its values from its key, so an old frame never installs
      a second layer on the next editor.

   The namespaces are `.cljs` or browser-only code, so the test reads their text."
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]))

(defn- form-body
  "The text of the top-level form in `src` that starts with `head`, up to the
   next top-level form."
  [src head]
  (let [start (str/index-of src head)
        _ (assert start (str head " not found"))
        end (or (str/index-of src "\n(" (inc start)) (count src))]
    (subs src start end)))

(def ^:private editor-src (slurp "src/freememo/rich_text_editor.cljc"))

(deftest destroy-editor-empties-the-dead-quill
  (let [destroy (form-body editor-src "(defn destroy-editor!")
        empty-fn (form-body editor-src "(defn- empty-dead-quill!")]
    (testing "the document, the undo stack, the tooltip and the toolbar are emptied"
      (is (str/includes? empty-fn "(.setContents ed #js [] \"silent\")"))
      (is (str/includes? empty-fn "(some-> (.-history ed) .clear)"))
      (is (str/includes? empty-fn "(some-> (.-tooltip theme) .-root)"))
      (is (str/includes? empty-fn "(.-container toolbar)")))
    (testing "the toolbar's controls and the theme's pickers are dropped"
      ;; They hold every button and select, so an empty toolbar node alone kept
      ;; 343 nodes a review.
      (is (str/includes? empty-fn "(set! (.-controls toolbar) #js [])"))
      (is (str/includes? empty-fn "(set! (.-pickers theme) #js [])")))
    (testing "a failure cannot stop destroy-editor! from clearing its state"
      (is (str/includes? empty-fn "(catch :default")))
    (testing "the emptying runs after the editor DOM left the page"
      ;; Before that, the scroll to the top would reach the reading-position
      ;; tracker while the old topic is still active.
      (is (< (str/index-of destroy "(set! (.-innerHTML ct) \"\")")
             (str/index-of destroy "(empty-dead-quill! ed)"))))
    (testing "no Quill field is set to nil: a late timer would throw"
      (is (not (str/includes? editor-src "release-quill-instance!"))))))

(deftest the-escape-listeners-on-the-container-are-removed
  (let [a11y-src (slurp "src/freememo/a11y.cljc")
        install (form-body a11y-src "(defn install-editor-escape!")
        destroy (form-body editor-src "(defn destroy-editor!")]
    (testing "install-editor-escape! returns a teardown for both listeners"
      (is (str/includes? install "(.removeEventListener container \"keydown\" on-key)"))
      (is (str/includes? install "(.removeEventListener container \"blur\" on-blur)")))
    (testing "the document editor keeps the teardown and destroy-editor! calls it"
      (is (str/includes? editor-src
            "(swap! !editor-state assoc :escape-teardown (a11y/install-editor-escape! editor))"))
      (is (str/includes? destroy "(when escape-teardown (escape-teardown))")))
    (testing "a card field keeps it with its other teardowns"
      (let [field-src (slurp "src/freememo/quill_field.cljc")
            teardowns (subs field-src (str/index-of field-src ":teardowns [markdown-teardown"))]
        (is (< (str/index-of teardowns "(a11y/install-editor-escape! ed)")
               (str/index-of teardowns "(format-menu/install! ed")))))))

(deftest pin-layer-frames-read-their-own-key
  (testing "PinLayer binds node, spec, model and scope from its key"
    (let [layer (form-body (slurp "src/freememo/question_pin_pane.cljc") "(e/defn PinLayer")]
      (is (str/includes? layer "(e/for-by identity [k [[node spec model scope]]]"))
      (is (str/includes? layer "(let [[node spec model scope] k"))))
  (testing "EditorPinLayer's frame binds spec from its key"
    (let [layer (form-body (slurp "src/freememo/editor_pin_layer.cljc") "(e/defn EditorPinLayer")]
      (is (str/includes? layer "(e/for-by identity [spec (if spec [spec] [])]")))))
