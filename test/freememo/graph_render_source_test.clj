(ns freememo.graph-render-source-test
  (:require [clojure.test :refer [deftest is]]))

(deftest hop-ring-draws-with-the-canvas-context
  (let [source (slurp "src/freememo/graph_render.cljs")]
    (is (re-find #"_ \(\.createCanvasContext renderer \"hopRings\"" source)
      "Sigma returns the renderer from createCanvasContext.")
    (is (re-find #"\^js ctx2d \(\.getContext canvas \"2d\"\)" source)
      "Get the drawing context from the created canvas.")))
