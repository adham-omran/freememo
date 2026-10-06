(ns freememo.line-bands-test
  "The question finger outlines one band per line, never one box per inline
   fragment.

   Needs no database: freememo.line-bands has no requires."
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.line-bands :as lb]))

(defn- r [left top right bottom] {:left left :top top :right right :bottom bottom})

(deftest fragments-on-one-line-become-one-band
  (testing "text, a padded <code> box and its text box share a line"
    (is (= [(r 10 98 300 126)]
           (lb/bands [(r 10 100 120 124)
                      (r 120 98 200 126)    ; the <code> element box
                      (r 124 101 196 123)   ; its text
                      (r 200 100 300 124)])))))

(deftest lines-stay-apart
  (testing "a three-line range gives three bands, top to bottom"
    (is (= [(r 600 100 1300 124) (r 10 134 1300 160) (r 10 168 400 192)]
           (lb/bands [(r 10 168 400 192)
                      (r 600 100 1300 124)
                      (r 10 136 500 158)
                      (r 500 134 560 160)
                      (r 560 136 1300 158)])))))

(deftest a-tall-box-does-not-swallow-the-next-line
  (is (= 2 (count (lb/bands [(r 0 100 50 130) (r 0 134 50 158)])))))

(deftest no-rects-no-bands
  (is (= [] (lb/bands []))))
