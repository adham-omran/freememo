(ns freememo.document-options-targets-test
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.document-options-targets :as t]))

(deftest targets-by-topic-shape
  (testing "a PDF page acts on its root for everything but the prompt"
    (is (= {:root-id 1 :bib-id 1 :review-id 1 :item-id 7}
           (t/targets {:context-mode :page :extract-status nil
                       :root-topic-id 1 :topic-id 7}))))
  (testing "a PDF root is every target"
    (is (= {:root-id 1 :bib-id 1 :review-id 1 :item-id 1}
           (t/targets {:context-mode :page :extract-status nil
                       :root-topic-id nil :topic-id 1}))))
  (testing "an extract owns its bibliography and is its own review unit"
    (is (= {:root-id 1 :bib-id 9 :review-id 9 :item-id 9}
           (t/targets {:context-mode :extract :extract-status "active"
                       :root-topic-id 1 :topic-id 9}))))
  (testing "a web topic at its root is every target"
    (is (= {:root-id 4 :bib-id 4 :review-id 4 :item-id 4}
           (t/targets {:context-mode :extract :extract-status nil
                       :root-topic-id 4 :topic-id 4})))))
