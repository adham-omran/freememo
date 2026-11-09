(ns freememo.pin-layers-keep-frames-test
  "The pin layers and the toolbar pin controls keep their frames across a
   Learn Next (plans/learn-next-speed/issues/04-pin-layers-keep-frames.md).
   The keys and the held values are plain fns, so this suite tests them
   without a browser. The desktop run `playwright/learn-session-run.mjs`
   (check `pins-stay-mounted`) tests the frames."
  (:require
   [clojure.test :refer [deftest is testing]]
   [freememo.editor-pin-layer :as epl]
   [freememo.question-pin-pane :as pane]))

(deftest the-layer-key-holds-no-spec
  (testing "two specs on one node give one key, so the layer stays"
    (is (= (pane/layer-key :node {:topic-id 1} :quill)
           (pane/layer-key :node {:topic-id 2} :quill)
           [[:node :quill]])))
  (testing "no node or no spec gives no layer"
    (is (= [] (pane/layer-key nil {:topic-id 1} :quill)))
    (is (= [] (pane/layer-key :node nil :quill))))
  (testing "a new editor root is a new key"
    (is (not= (pane/layer-key :a {:topic-id 1} :quill)
              (pane/layer-key :b {:topic-id 1} :quill)))))

(deftest the-held-view-never-shows-another-files-pins
  (let [a [{:topic-id 1} {:pins [:a]}]
        b [{:topic-id 2} {:pins [:b]}]]
    (testing "an answer replaces what the layer holds"
      (is (= b (pane/held-view a {:topic-id 2} b))))
    (testing "the same spec read again keeps the held view"
      (is (= a (pane/held-view a {:topic-id 1} nil))))
    (testing "a new spec read shows nothing until its answer"
      (is (nil? (pane/held-view a {:topic-id 2} nil))))))

(deftest the-held-answer-keeps-the-last-answer
  (is (= {:open 1} (pane/held-answer {:open 1} nil)))
  (is (= {:open 2} (pane/held-answer {:open 1} {:open 2})))
  (is (nil? (pane/held-answer nil nil))))

(deftest the-document-spec-is-held-while-it-is-read
  (testing "a read that runs keeps the previous spec"
    (is (= {:topic-id 1} (epl/held-spec {:topic-id 1} nil))))
  (testing "an answer gives its spec, nil included"
    (is (= {:topic-id 2} (epl/held-spec {:topic-id 1} {:spec {:topic-id 2}})))
    (is (nil? (epl/held-spec {:topic-id 1} {:spec nil})))))

(deftest the-controls-key-holds-no-spec
  (is (= (epl/controls-key {:topic-id 1}) (epl/controls-key {:topic-id 2})))
  (is (= [] (epl/controls-key nil))))

(deftest the-pins-follow-the-body-in-the-editor
  (testing "a page view reads the topic whose body is shown"
    (is (= 7 (epl/pin-topic-id false 99 7)))
    (is (= {:merged? false :topic-id 7} (epl/pin-scope false 7))))
  (testing "the merged view reads the PDF root"
    (is (= 99 (epl/pin-topic-id true 99 7)))
    (is (= {:merged? true} (epl/pin-scope true 99)))))

(deftest the-pin-list-closes-on-another-document
  (let [!last (atom nil)
        !open? (atom true)]
    (testing "the first spec closes nothing"
      (epl/close-list-on-change! !last !open? {:topic-id 1})
      (is (true? @!open?)))
    (testing "the same spec again closes nothing"
      (epl/close-list-on-change! !last !open? {:topic-id 1})
      (is (true? @!open?)))
    (testing "another spec closes the list"
      (epl/close-list-on-change! !last !open? {:topic-id 2})
      (is (false? @!open?)))))
