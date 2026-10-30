(ns freememo.measured-list-test
  (:require [clojure.test :refer [deftest is]]
            [freememo.measured-list :as measured]))

(deftest measured-heights-preserve-indexed-positions
  (let [model (-> (measured/height-model 30000 100)
                (measured/measure-height 0 240)
                (measured/measure-height 1 40)
                (measured/measure-height 15000 360))]
    (is (= 0 (measured/prefix-height model 0)))
    (is (= 240 (measured/prefix-height model 1)))
    (is (= 280 (measured/prefix-height model 2)))
    (is (= 3000340 (measured/prefix-height model 30000)))
    (is (= 0 (measured/index-at model 239.5)))
    (is (= 1 (measured/index-at model 240)))
    (is (= 2 (measured/index-at model 280)))
    (is (= 29999 (measured/index-at model 9999999)))
    (is (= 1500080 (measured/prefix-height (assoc model :count 15000) 30000)))
    (is (= 3000340 (measured/prefix-height (assoc model :count 30001) 30000)))))

(deftest full-traversal-bounds-the-window
  (let [model (measured/height-model 30000 100)]
    (doseq [position (range 0 3000000 50000)]
      (let [view (measured/visible-window model position 800 #{} 150)]
        (is (<= (count (:rows view)) 25))
        (is (= 3000000 (:height view)))
        (is (every? #(<= 0 (:index %) 29999) (:rows view)))))
    (let [view (measured/visible-window model 10000 800 #{0 29999} 150)]
      (is (= 0 (:index (first (:rows view)))))
      (is (= 29999 (:index (last (:rows view))))))))

(deftest empty-and-short-rows-remain-bounded
  (is (empty? (:rows (measured/visible-window (measured/height-model 0 100) 0 800 #{0} 150))))
  (let [view (measured/visible-window (measured/height-model 10000 1) 0 800 #{} 150)]
    (is (= 801 (count (:rows view))))))

(deftest following-includes-content-outside-the-measured-spacer
  (let [before (measured/height-model 10 100)
        after (measured/measure-height before 9 200)]
    (is (= 716 (measured/follow-target before after 1116 500)))
    (is (= 616 (measured/follow-target after after 1116 500)))
    (is (= 516 (measured/follow-target after before 1116 500)))))

(deftest sparse-height-tree-matches-naive-prefixes
  (let [heights (mapv #(+ 1 (mod (* % 53) 719)) (range 500))
        model (reduce-kv measured/measure-height (measured/height-model 500 100) heights)
        prefixes (vec (reductions + 0 heights))]
    (doseq [i (range 501)]
      (is (= (nth prefixes i) (measured/prefix-height model i))))
    (doseq [i (range 500)]
      (is (= i (measured/index-at model (nth prefixes i)))))))
