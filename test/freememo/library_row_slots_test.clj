(ns freememo.library-row-slots-test
  (:require [clojure.test :refer [deftest is]]
            [freememo.library-cards :as library]))

(deftest empty-and-small-results-retain-bounded-slot-identities
  (let [large (mapv #(hash-map :row-key [:card %]) (range 100000))
        broad (library/window-slots (subvec large 0 15) 15)
        empty (library/window-slots [] 15)
        small (library/window-slots [{:row-key [:card 99999]}] 15)]
    (is (= (range 15) (map :slot broad) (map :slot empty) (map :slot small)))
    (is (= 15 (count broad) (count empty) (count small)))
    (is (every? nil? (map :card empty)))
    (is (= 1 (count (keep :card small))))
    (is (= 9 (count (library/window-slots [] 9))))))

(deftest hidden-slots-retain-content-and-recycled-slots-use-new-actions
  (let [a {:row-key [:card 1] :member-ids [1] :pos 100 :front-html "First"}
        b {:row-key [:occlusion 2] :member-ids [2 3] :pos 0 :front-html "Second"}
        initial (library/card-slot-state nil 10 nil)
        visible (library/card-slot-state initial 10 a)
        hidden (library/card-slot-state visible 10 nil)
        recycled (library/card-slot-state hidden 10 b)]
    (is (= {:scope 10 :visible? false :card nil} initial))
    (is (:visible? visible))
    (is (= a (:card hidden)))
    (is (false? (:visible? hidden)))
    (is (= b (:card recycled)))
    (is (:visible? recycled))
    (is (= [2 3] (get-in recycled [:card :member-ids])))
    (is (= 0 (get-in recycled [:card :pos])))))

(deftest changing-account-discards-hidden-content
  (let [owned (library/card-slot-state nil 10 {:row-key [:card 1]})
        changed (library/card-slot-state owned 11 nil)]
    (is (= {:scope 11 :visible? false :card nil} changed))))
