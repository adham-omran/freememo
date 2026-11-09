(ns freememo.editor-pane-load-window-test
  "The editor pane's choice between the spinner and the editor host, and its
   load window after a topic change
   (plans/learn-next-speed/issues/02-editor-stays-mounted-across-next.md)."
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.editor-pane :as pane]))

(deftest a-next-between-two-topic-bodies-keeps-the-editor-host
  (testing "the content read still holds topic 10's tuple while 11 is in view"
    (is (false? (pane/editor-spinner? "<p>old</p>" true 10)))
    (is (true? (pane/editor-loading? 10 11)))
    (is (= "true" (pane/loading-attr (pane/editor-loading? 10 11)))))
  (testing "the new tuple arrived"
    (is (false? (pane/editor-loading? 11 11)))
    (is (= "false" (pane/loading-attr (pane/editor-loading? 11 11))))))

(deftest the-first-fetch-and-a-switch-from-a-pdf-page-show-the-spinner
  (testing "no body and no editor yet"
    (is (true? (pane/editor-spinner? nil false 10))))
  (testing "a nil body with a live editor holds the editor (focus-reader toggle)"
    (is (false? (pane/editor-spinner? nil true 10))))
  (testing "the content read held a PDF page, so it has no topic body to keep"
    (is (true? (pane/editor-spinner? nil true nil)))))
