(ns freememo.kg-graph-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [freememo.kg-graph :as graph]))

(defn- private-value [sym]
  (var-get (ns-resolve 'freememo.kg-graph sym)))

(defn- private-fn [sym]
  (private-value sym))

(defn- node-width [dot id]
  (some->> (str/split-lines dot)
    (some #(when-let [[_ width]
                      (re-matches (re-pattern (str id "\\[width=([^,]+),height=[^]]+\\];")) %)]
             (parse-double width)))))

(deftest dot-reserves-the-rendered-node-size
  (let [dot (graph/->dot [1 2 3 4 5 6] [[1 2] [1 3] [1 4] [1 5]])
        hub-width (node-width dot 1)
        leaf-width (node-width dot 2)
        isolated-width (node-width dot 6)]
    (testing "node geometry"
      (is (some? hub-width))
      (is (some? leaf-width))
      (is (> hub-width leaf-width))
      (is (= leaf-width isolated-width))
      (is (< (Math/abs (- leaf-width (/ 8.8 72.0))) 1.0e-12)))
    (testing "fixed collision circles"
      (is (str/includes? dot
            "node [shape=circle,fixedsize=true,label=\"\",margin=0];"))
      (is (str/includes? dot "1--2;")))))

(deftest layout-uses-the-bounded-overlap-pass
  (let [args (private-value 'sfdp-args)]
    (is (= ["-Goverlap=prism0"
            "-Gsep=+24"
            "-Gpack=24"
            "-Gpackmode=node"
            "-Gbeautify=true"
            "-Gstart=42"
            "-Tplain"]
          args))
    (is (not-any? #{"-Goverlap=prism"} args))))

(deftest plain-output-parser-ignores-non-node-rows
  (let [parse-positions (private-fn 'parse-sfdp-positions)]
    (is (= {7 [1.25 2.5]
            9 [-3.0 4.75]}
          (parse-positions
            (str "graph 1 10 10\n"
                 "node 7 1.25 2.5 0.2 0.2 \"\" solid circle black lightgrey\n"
                 "edge 7 9 2 1.0 2.0 3.0 4.0 solid black\n"
                 "node 9 -3.0 4.75 0.2 0.2 \"\" solid circle black lightgrey\n"
                 "stop\n"))))))

(deftest cache-revision-invalidates-old-layouts
  (let [current-cache? (private-fn 'current-cache?)]
    (is (true? (current-cache? {:version 7
                                :payload {:layout-revision 2}}
                 7)))
    (is (false? (current-cache? {:version 7
                                 :payload {}}
                  7)))
    (is (false? (current-cache? {:version 6
                                 :payload {:layout-revision 2}}
                  7)))
    (is (false? (current-cache? nil 7)))))
