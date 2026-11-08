(ns freememo.question-pin-layer-leak-test
  "Source-shape contract for the pin layer's lifetime
   (plans/low-end-tablet-performance/issues/20-session-dom-growth.md).

   Electric can run `question-pin-dom/install!` for a branch that it drops before
   it registers the branch's unmount, so a Learn step kept one layer, and the
   layer's listeners kept the old editor's DOM alive (about 850 nodes a review).
   `install!` must sweep dead layers, and a teardown must be safe to run twice.
   The namespace is `.cljs`, so the test reads its text."
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]))

(def ^:private src (slurp "src/freememo/question_pin_dom.cljs"))

(defn- defn-body [name]
  (let [start (str/index-of src (str "(defn" name))
        _ (assert start (str name " not found"))
        end (or (str/index-of src "\n(defn" (inc start)) (count src))]
    (subs src start end)))

(deftest install-sweeps-layers-whose-container-left-the-document
  (testing "a registry of live layers exists"
    (is (str/includes? src "(defonce ^:private !live (js/Set.))")))
  (testing "sweep-stale! tears down a layer whose container is not connected"
    (let [body (defn-body "- sweep-stale!")]
      (is (str/includes? body ".-isConnected"))
      (is (str/includes? body ".-teardown"))
      (is (str/includes? body "mount-grace-ms"))))
  (testing "a layer installed on a container that is already gone is swept later"
    (is (str/includes? (defn-body " install!") "(js/setTimeout sweep-stale!")))
  (testing "install! sweeps before it builds a layer, and registers the new one"
    (let [body (defn-body " install!")]
      (is (< (str/index-of body "(sweep-stale!)") (str/index-of body "(el \"div\" \"question-pin-layer\")")))
      (is (str/includes? body "(.add !live rec)"))))
  (testing "a teardown runs once, so a sweep and Electric's unmount may both call it"
    (let [body (defn-body " install!")]
      (is (str/includes? body "(when (.has !live rec)"))
      (is (str/includes? body "(.delete !live rec)")))))

(deftest destroy-editor-releases-the-quill-instance
  ;; Quill 2 keeps each instance in a WeakMap keyed by its container, and an
  ;; Electric frame holds the container, so the instance must drop its DOM.
  (let [editor-src (slurp "src/freememo/rich_text_editor.cljc")
        start (str/index-of editor-src "(defn destroy-editor!")
        body (subs editor-src start (str/index-of editor-src "\n(defn" (inc start)))]
    (testing "the release fn nils the fields that hold DOM"
      (is (str/includes? editor-src "(defn- release-quill-instance!"))
      (doseq [k ["\"theme\"" "\"scroll\"" "\"root\""]]
        (is (str/includes? editor-src k))))
    (testing "destroy-editor! calls it after the editor DOM is cleared"
      (is (< (str/index-of body "(set! (.-innerHTML ct) \"\")")
             (str/index-of body "(release-quill-instance! ed)"))))))
