(ns freememo.focus-reader-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [freememo.focus-reader :as focus-reader]))

(deftest standard-text-topics-support-focus-reader
  (testing "supported text topics"
    (doseq [kind ["basic" "epub" "web" "wikipedia" "markdown"]]
      (is (focus-reader/available? kind false) kind))))

(deftest special-topics-do-not-support-focus-reader
  (testing "special topic views"
    (doseq [kind ["audio" "video" "score" "poem" "code" "page"]]
      (is (not (focus-reader/available? kind false)) kind))))

(deftest real-pdfs-keep-the-existing-focus-reader
  (is (focus-reader/available? "pdf" true))
  (is (focus-reader/available? "page" true))
  (is (not (focus-reader/available? "score" true))))
