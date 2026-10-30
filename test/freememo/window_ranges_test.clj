(ns freememo.window-ranges-test
  (:require [clojure.test :refer [deftest is]]
            [freememo.window-ranges :as ranges]))

(deftest mounted-indices-keep-gaps-and-query-limits
  (is (= [] (ranges/index-ranges [])))
  (is (= [[3 3] [50 2] [900 1]] (ranges/index-ranges [5 3 4 50 51 900 4 -1 nil])))
  (is (= [[0 100] [100 100] [200 1]] (ranges/index-ranges (range 201)))))
