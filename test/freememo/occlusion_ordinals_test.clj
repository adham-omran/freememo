(ns freememo.occlusion-ordinals-test
  "Covers freememo.occlusion-ordinals: the mask-group → ordinal rule that
   decides how many cards an occlusion group's geometry produces. Pure, so it
   runs without a database — that is why the algebra lives outside db.clj."
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.occlusion-ordinals :as ord]))

(defn- rect [m] (merge {:x 0 :y 0 :w 10 :h 10} m))

(deftest o1-create-one-ordinal-per-rect
  (testing "no membership keys: every rect is its own mask group"
    (let [[rects next'] (ord/assign-ordinals [(rect {}) (rect {}) (rect {})] 1)]
      (is (= [1 2 3] (mapv :ordinal rects)))
      (is (= 4 next'))
      (is (= [1 2 3] (ord/ordinals-in-order rects))))))

(deftest o2-create-shared-gid-is-one-ordinal
  (testing "rects sharing a :gid become one mask group, hence one card"
    (let [[rects next'] (ord/assign-ordinals
                          [(rect {:gid "u1"}) (rect {:gid "u1"}) (rect {})] 1)]
      (is (= [1 1 2] (mapv :ordinal rects)))
      (is (= 3 next'))
      (is (= [1 2] (ord/ordinals-in-order rects)))
      (testing ":gid is dropped — ordinals carry membership from here on"
        (is (every? #(not (contains? % :gid)) rects))))))

(deftest o3-two-groups-and-a-solo
  (let [[rects next'] (ord/assign-ordinals
                        [(rect {:gid "a"}) (rect {:gid "b"}) (rect {:gid "a"})
                         (rect {}) (rect {:gid "b"})]
                        1)]
    (is (= [1 2 1 3 2] (mapv :ordinal rects)))
    (is (= [1 2 3] (ord/ordinals-in-order rects)))
    (is (= 4 next'))))

(deftest o4-existing-ordinals-are-kept
  (testing "an edit keeps saved ordinals and mints only for new mask groups"
    (let [[rects next'] (ord/assign-ordinals
                          [(rect {:ordinal 3}) (rect {:ordinal 3}) (rect {:gid "new"})
                           (rect {:gid "new"})]
                          7)]
      (is (= [3 3 7 7] (mapv :ordinal rects)))
      (is (= 8 next'))
      (testing "only ordinals >= the watermark need a new row"
        (is (= [7] (filterv #(>= % 7) (ord/ordinals-in-order rects))))))))

(deftest o5-absorbed-rect-adds-no-ordinal
  (testing "a rect grouped into a saved mask group arrives with its ordinal"
    (let [[rects next'] (ord/assign-ordinals
                          [(rect {:ordinal 2}) (rect {:ordinal 2}) (rect {:ordinal 5})]
                          9)]
      (is (= [2 2 5] (mapv :ordinal rects)))
      (is (= 9 next') "nothing minted")
      (is (empty? (filterv #(>= % 9) (ord/ordinals-in-order rects)))))))

(deftest o6-ordinal-wins-over-gid
  (testing "both keys is a caller bug; :ordinal wins and :gid is dropped"
    (let [[rects _] (ord/assign-ordinals [(rect {:ordinal 4 :gid "x"})] 9)]
      (is (= [4] (mapv :ordinal rects)))
      (is (every? #(not (contains? % :gid)) rects)))))

(deftest o7-card-count-before-assignment
  (testing "card-count anticipates the row count the server will create"
    (is (= 0 (ord/card-count [])))
    (is (= 3 (ord/card-count [(rect {}) (rect {}) (rect {})])))
    (is (= 2 (ord/card-count [(rect {:gid "u1"}) (rect {:gid "u1"}) (rect {})])))
    (is (= 1 (ord/card-count [(rect {:ordinal 3}) (rect {:ordinal 3})])))
    (is (= 2 (ord/card-count [(rect {:ordinal 3}) (rect {:ordinal 3}) (rect {:gid "u2"})])))
    (testing "an :ordinal and a :gid of the same value do not collide"
      (is (= 2 (ord/card-count [(rect {:ordinal 1}) (rect {:gid "1"})]))))))
