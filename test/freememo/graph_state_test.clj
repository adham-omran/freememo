(ns freememo.graph-state-test
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.graph-state :as graph-state]))

(def edges
  [{:id "0" :source "a" :target "b" :pred 0 :topic 10}
   {:id "1" :source "b" :target "c" :pred 0 :topic 10}
   {:id "2" :source "c" :target "a" :pred 1 :topic 10}
   {:id "3" :source "c" :target "d" :pred 0 :topic 20}
   {:id "4" :source "d" :target "e" :pred 0 :topic 20}])

(deftest filters-before-computing-the-core
  (testing "A source filter removes edges before the k-core pass"
    (is (= {:node-ids #{"a" "b" "c"}
            :edge-ids #{"0" "1" "2"}}
          (graph-state/visible-subgraph
            edges {:source 10 :pred-set nil :min-degree 2 :view-mode :overview}))))
  (testing "A relation filter can remove the full two-core"
    (is (= {:node-ids #{} :edge-ids #{}}
          (graph-state/visible-subgraph
            edges {:source :all :pred-set #{0} :min-degree 2 :view-mode :overview})))))

(deftest limits-a-selected-neighborhood
  (is (= {:node-ids #{"a" "b" "c"}
          :edge-ids #{"0" "1" "2"}}
        (graph-state/visible-subgraph
          edges {:source :all :pred-set nil :min-degree 1
                 :view-mode :neighborhood :selected "a" :depth 1})))
  (is (= {:node-ids #{"a" "b" "c" "d"}
          :edge-ids #{"0" "1" "2" "3"}}
        (graph-state/visible-subgraph
          edges {:source :all :pred-set nil :min-degree 1
                 :view-mode :neighborhood :selected "a" :depth 2}))))

(deftest hides-a-selected-node-that-the-filters-remove
  (is (= {:node-ids #{} :edge-ids #{}}
        (graph-state/visible-subgraph
          edges {:source 20 :pred-set nil :min-degree 1
                 :view-mode :neighborhood :selected "a" :depth 1}))))

(deftest parallel-relations-do-not-inflate-connectedness
  (let [parallel-edges [{:id "0" :source "a" :target "b" :pred 0 :topic 10}
                        {:id "1" :source "a" :target "b" :pred 1 :topic 10}
                        {:id "2" :source "a" :target "b" :pred 2 :topic 20}]]
    (is (= {:node-ids #{} :edge-ids #{}}
          (graph-state/visible-subgraph
            parallel-edges
            {:source :all :pred-set nil :min-degree 2 :view-mode :overview})))))

(def radial-edges
  [{:id "0" :source "a" :target "b"}
   {:id "1" :source "a" :target "c"}
   {:id "2" :source "b" :target "d"}
   {:id "3" :source "b" :target "e"}
   {:id "4" :source "c" :target "f"}
   {:id "5" :source "d" :target "g"}])

(defn- distance-from-center
  [{:keys [x y]}]
  (Math/sqrt (+ (* (- x 0.5) (- x 0.5))
                (* (- y 0.5) (- y 0.5)))))

(deftest places-neighborhood-nodes-on-hop-rings
  (let [{:keys [positions hop-by-id node-ids-by-hop radii]}
        (graph-state/neighborhood-layout radial-edges "a")]
    (is (= {:x 0.5 :y 0.5} (get positions "a")))
    (is (= {0 #{"a"} 1 #{"b" "c"} 2 #{"d" "e" "f"}}
          node-ids-by-hop))
    (is (= {"a" 0 "b" 1 "c" 1 "d" 2 "e" 2 "f" 2}
          hop-by-id))
    (doseq [node-id #{"b" "c"}]
      (is (< (Math/abs (- (get radii 1)
                           (distance-from-center (get positions node-id))))
            1.0e-12)))
    (doseq [node-id #{"d" "e" "f"}]
      (is (< (Math/abs (- (get radii 2)
                           (distance-from-center (get positions node-id))))
            1.0e-12)))
    (is (nil? (get positions "g")))))

(deftest radial-layout-is-stable-and-keeps-all-nodes
  (let [forward (graph-state/neighborhood-layout radial-edges "a")
        reverse-order (graph-state/neighborhood-layout (reverse radial-edges) "a")
        second-hop (get-in forward [:node-ids-by-hop 2])
        second-points (map (:positions forward) second-hop)]
    (is (= forward reverse-order))
    (is (= (count second-hop) (count (set second-points))))
    (is (= #{"b" "c"}
          (set (vals (:parent-by-id forward)))))))

(deftest radial-layout-balances-shared-second-hop-nodes
  (let [shared-edges [{:source "a" :target "b"}
                      {:source "a" :target "c"}
                      {:source "b" :target "d"}
                      {:source "c" :target "d"}
                      {:source "b" :target "e"}
                      {:source "c" :target "e"}]
        parents (:parent-by-id
                  (graph-state/neighborhood-layout shared-edges "a"))]
    (is (= #{"b" "c"} (set (vals parents))))))

(deftest radial-layout-spreads-a-large-neighborhood
  (let [first-hop (mapv #(str "first-" %) (range 60))
        second-hop (mapv #(str "second-" %) (range 90))
        edges (concat
                (map (fn [node] {:source "selected" :target node}) first-hop)
                (map-indexed
                  (fn [index node]
                    {:source (nth first-hop (mod index (count first-hop)))
                     :target node})
                  second-hop))
        {:keys [positions node-ids-by-hop]}
        (graph-state/neighborhood-layout edges "selected")]
    (is (= 60 (count (get node-ids-by-hop 1))))
    (is (= 90 (count (get node-ids-by-hop 2))))
    (is (= 151 (count positions)))
    (is (= 151 (count (set (vals positions)))))))
