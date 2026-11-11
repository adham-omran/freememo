(ns freememo.source-line-test
  "The Source Document line above a topic and in a Learn queue row
   (plans/topic-view-source-line.md, plans/learn-queue-source-lines.md)."
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.source-line :as sl]))

(deftest a-source-document-shows-only-its-own-title
  (is (= {:source-document "The Selfish Gene" :title nil}
        (sl/topic-line {:root? true :source-document "The Selfish Gene"
                        :title "The Selfish Gene" :body-text "Chapter 1. Why are people?"}))))

(deftest an-extract-whose-title-is-its-first-words-shows-only-the-source-document
  (testing "the title is the first 80 characters of the selection"
    (is (= {:source-document "The Selfish Gene" :title nil}
          (sl/topic-line {:root? false :source-document "The Selfish Gene"
                          :title "Intelligent life on a planet comes of age when it"
                          :body-text "Intelligent life on a planet comes of age when it first works out the reason for its own existence."}))))
  (testing "the selection had paragraphs, so the title has no space where the body has one"
    (is (nil? (:title (sl/topic-line {:root? false :source-document "Book"
                                      :title "First paragraph.Second"
                                      :body-text "First paragraph. Second paragraph."})))))
  (testing "case and spacing do not count"
    (is (nil? (:title (sl/topic-line {:root? false :source-document "Book"
                                      :title "  the   START of it"
                                      :body-text "The start of it all"}))))))

(deftest a-renamed-topic-shows-the-source-document-and-its-title
  (is (= {:source-document "The Selfish Gene" :title "Replicators"}
        (sl/topic-line {:root? false :source-document "The Selfish Gene"
                        :title "Replicators"
                        :body-text "At some point a particularly remarkable molecule was formed."}))))

(deftest a-page-topic-shows-its-page-title
  (is (= {:source-document "Calculus" :title "Page 12"}
        (sl/topic-line {:root? false :source-document "Calculus"
                        :title "Page 12" :body-text "Theorem 3. Let f be continuous."}))))

(deftest a-topic-with-no-title-shows-only-the-source-document
  (is (= {:source-document "Book" :title nil}
        (sl/topic-line {:root? false :source-document "Book" :title "" :body-text "Some text"})))
  (is (= {:source-document "Book" :title nil}
        (sl/topic-line {:root? false :source-document "Book" :title nil :body-text nil}))))

(deftest a-missing-source-document-title-falls-back-to-the-topic-title
  (is (= {:source-document "Orphan" :title nil}
        (sl/topic-line {:root? true :source-document nil :title "Orphan" :body-text ""}))))

(deftest the-full-text-joins-the-parts-with-the-separator
  (is (= "The Selfish Gene › Replicators"
        (sl/full-text {:source-document "The Selfish Gene" :title "Replicators"})))
  (is (= "The Selfish Gene"
        (sl/full-text {:source-document "The Selfish Gene" :title nil}))))
