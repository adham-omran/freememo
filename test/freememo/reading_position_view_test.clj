(ns freememo.reading-position-view-test
  "ReadingPosition keeps one frame across a Learn Session Next
   (plans/learn-next-speed/issues/05-reading-position-filter-bar-keep-frames.md).
   The server row carries the key it was read for, so a row read for topic N
   never reaches topic N+1's tracker while the read for N+1 runs."
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.reading-position-view :as rpv]))

(deftest a-row-belongs-to-the-key-it-was-read-for
  (let [row {:key [7 "editor"] :pos {:fraction 0.5}}]
    (is (rpv/own-row? [7 "editor"] row))
    (testing "another topic's row, or another surface's, is not the tracker's"
      (is (not (rpv/own-row? [8 "editor"] row)))
      (is (not (rpv/own-row? [7 "poem"] row))))
    (testing "no answer yet is not an answer of nil"
      (is (not (rpv/own-row? [7 "editor"] nil))))
    (testing "a tagged answer of no position is the tracker's"
      (is (rpv/own-row? [7 "editor"] {:key [7 "editor"] :pos nil})))))

(deftest a-nil-topic-reads-nothing
  (is (= {:key [nil "editor"] :pos nil}
        (rpv/get-reading-position* 1 nil "editor"))))
