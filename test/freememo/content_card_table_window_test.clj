(ns freememo.content-card-table-window-test
  "The card table's scroll window and its arrivals: which saved rows the
   window shows, where each one sits beside the pending rows, and which rows
   slide in. Needs no database."
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.content-card-table :as table]))

(defn- row [id]
  {:flashcards/id id :flashcards/kind "basic" :flashcards/question (str "q" id)
   :row-key [:card id] :member-ids [id] :members []})

(deftest window-views-places-saved-rows-below-the-pending-rows
  (let [rows (mapv row (range 10))
        visible (vec (range 10))]
    (testing "no pending row: the window is positions [offset, offset + limit)"
      (is (= [2 3 4] (mapv :pos (table/window-views rows visible 2 3 0 #{})))))
    (testing "two pending rows take display orders 0 and 1"
      (is (= [0 1 2] (mapv :pos (table/window-views rows visible 0 5 2 #{})))))
    (testing "a window that lies inside the pending rows shows no saved row"
      (is (= [] (table/window-views rows visible 0 2 3 #{}))))
    (testing "the window stops at the last visible row"
      (is (= [8 9] (mapv :pos (table/window-views rows visible 8 10 0 #{})))))
    (testing "a hidden row is skipped, and positions count visible rows only"
      (is (= [[0 [:card 0]] [1 [:card 2]]]
             (mapv (juxt :pos :row-key) (table/window-views rows [0 2 3] 0 2 0 #{})))))
    (testing "a row whose key arrived is fresh, and no other row is"
      (is (= [false true false]
             (mapv :fresh? (table/window-views rows visible 0 3 0 #{[:card 1]})))))))

(deftest arrivals-names-only-rows-new-since-the-last-call
  (let [!seen (atom {})]
    (testing "the first call for a topic animates nothing"
      (is (= #{} (table/arrivals! !seen 1 #{[:card 1] [:card 2]}))))
    (testing "a row new since the last call has arrived"
      (is (= #{[:card 3]} (table/arrivals! !seen 1 #{[:card 1] [:card 2] [:card 3]}))))
    (testing "a deleted row is not an arrival"
      (is (= #{} (table/arrivals! !seen 1 #{[:card 1] [:card 3]}))))
    (testing "another topic starts again, so opening it animates nothing"
      (is (= #{} (table/arrivals! !seen 2 #{[:card 9]}))))))
