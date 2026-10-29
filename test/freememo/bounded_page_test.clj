(ns freememo.bounded-page-test
  (:require [clojure.test :refer [deftest is]]
            [freememo.bounded-page :as page]))

(deftest pages-stay-bounded-through-deletions-and-stale-navigation
  (is (= (vec (range 20)) (page/page-items (range 100000) 0 20)))
  (is (= (vec (range 99980 100000)) (page/page-items (range 100000) 99999 20)))
  (is (= [0 1 2] (page/page-items (range 3) 99999 20)))
  (is (= [] (page/page-items [] 5 20)))
  (is (= (vec (range 20 40)) (page/page-items (range 100) 1 20))))
