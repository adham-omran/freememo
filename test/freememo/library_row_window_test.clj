(ns freememo.library-row-window-test
  "The Library tables' and the hierarchy panel's scroll windows: which rows each window shows, the
   display order each row carries, and which tree rows slide in. Needs no
   database."
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.hierarchy-side-panel :as panel]
            [freememo.knowledge-tree :as tree]
            [freememo.library-cards :as library-cards]
            [freememo.row-motion :as row-motion]))

(defn- card [id]
  {:flashcards/id id :row-key [:card id] :member-ids [id]})

(deftest window-cards-shows-visible-cards-by-display-order
  (let [cards (mapv card (range 10))]
    (testing "the window is display orders [offset, offset + limit)"
      (is (= [2 3 4] (mapv :pos (library-cards/window-cards cards (vec (range 10)) 2 3 #{})))))
    (testing "the window stops at the last visible card"
      (is (= [8 9] (mapv :pos (library-cards/window-cards cards (vec (range 10)) 8 10 #{})))))
    (testing "a hidden card is skipped, and display orders count visible cards only"
      (is (= [[0 [:card 0]] [1 [:card 2]] [2 [:card 3]]]
             (mapv (juxt :pos :row-key) (library-cards/window-cards cards [0 2 3] 0 5 #{})))))
    (testing "an offset past the end shows nothing"
      (is (= [] (library-cards/window-cards cards [0 1] 5 3 #{}))))
    (testing "a card whose row key arrived is fresh, and a hidden card is not a key"
      (is (= [false true] (mapv :fresh? (library-cards/window-cards cards [0 2] 0 2 #{[:card 2]}))))
      (is (= #{[:card 0] [:card 2]} (library-cards/visible-row-keys cards [0 2]))))))

(defn- flat-row [id]
  {:depth 0 :has-children false :is-root true :expanded? false
   :topic {:topics/id id :topics/title (str "t" id)}})

(deftest tree-window-carries-position-and-freshness
  (let [rows (mapv flat-row [10 11 12 13 14])]
    (testing "the window is display orders [offset, offset + limit)"
      (is (= [[1 11] [2 12]] (mapv (juxt :pos :id) (tree/tree-window rows 1 2 #{})))))
    (testing "a row whose id arrived is fresh, and no other row is"
      (is (= [false true false] (mapv :fresh? (tree/tree-window rows 0 3 #{11})))))
    (testing "the ids are the topic ids"
      (is (= #{10 11 12 13 14} (tree/tree-row-ids rows))))))

(deftest a-scroll-offset-in-the-scope-makes-no-arrival
  (let [!seen (atom {})
        view [nil nil nil "added" "desc"]]
    (row-motion/arrivals! !seen [view 0] #{1 2})
    (testing "an expand at the same offset adds its children"
      (is (= #{3 4} (row-motion/arrivals! !seen [view 0] #{1 2 3 4}))))
    (testing "a scroll is a new scope, so the rows it shows are not new"
      (is (= #{} (row-motion/arrivals! !seen [view 4] #{1 2 3 4}))))
    (testing "an expand after the scroll still adds its children"
      (is (= #{5} (row-motion/arrivals! !seen [view 4] #{1 2 3 4 5}))))))

(deftest panel-window-carries-position-and-freshness
  (let [rows (mapv (fn [id] {:depth 0 :topic {:topics/id id}}) [5 6 7 8])]
    (testing "the window is display orders [offset, offset + limit)"
      (is (= [[1 6] [2 7]] (mapv (juxt :pos (comp :topics/id :topic))
                                 (panel/panel-window rows 1 2 #{})))))
    (testing "a row whose topic arrived is fresh, and no other row is"
      (is (= [false true] (mapv :fresh? (panel/panel-window rows 2 2 #{8})))))
    (testing "the ids are the topic ids"
      (is (= #{5 6 7 8} (panel/panel-row-ids rows))))))
