(ns freememo.activity-panel-test
  "What a finished Activity row says under its label. Needs no database."
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.activity-panel :as panel]))

(deftest a-finished-row-says-why-then-what-then-how-far
  (testing "an error wins over everything"
    (is (= "Sync failed: boom"
          (panel/finished-line {:error "Sync failed: boom" :summary "12 sent to Anki"
                                :done 3 :total 9}))))
  (testing "a summary wins over progress"
    (is (= "12 sent to Anki"
          (panel/finished-line {:summary "12 sent to Anki" :done 12 :total 12}))))
  (testing "progress when no words were recorded"
    (is (= "12 / 40" (panel/finished-line {:done 12 :total 40}))))
  (testing "nothing at all"
    (is (= "" (panel/finished-line {})))
    (is (= "" (panel/finished-line {:error "" :summary "" :phase "writing"}))
      "never the phase of a run that has ended")))
