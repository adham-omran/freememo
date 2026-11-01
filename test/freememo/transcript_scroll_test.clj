(ns freememo.transcript-scroll-test
  (:require [clojure.test :refer [deftest is]]
            [freememo.transcript-scroll :as scroll]))

(deftest layout-scroll-cannot-pause-following
  (is (true? (scroll/following-after-scroll? true false 125 533 2660)))
  (is (false? (scroll/following-after-scroll? false false 125 533 2660)))
  (is (false? (scroll/following-after-scroll? true true 125 533 2660)))
  (is (true? (scroll/following-after-scroll? false true 2127 533 2660))))
