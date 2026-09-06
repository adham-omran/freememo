(ns freememo.graph-virtual-scroll-source-test
  (:require [clojure.test :refer [deftest is]]))

(deftest graph-long-lists-use-virtual-scroll
  (let [source (slurp "src/freememo/graph_page.cljc")]
    (is (= 2 (count (re-seq #"\(Scroll-window" source)))
      "Relations and connected facts each own one scroll window.")
    (is (re-find #"graph-relation-list tape-scroll" source))
    (is (re-find #"graph-inspector-facts tape-scroll" source))
    (is (re-find #":reset-key \[source query\]" source)
      "A relation filter change resets the relation window.")
    (is (re-find #":reset-key selected-id" source)
      "A concept change resets the connected-facts window.")))
