(ns freememo.command-palette-test
  (:require [clojure.test :refer [deftest is]]
            [freememo.command-bus :as bus]
            [freememo.command-palette :as palette]
            [freememo.commands :as commands]))

(deftest empty-query-shows-every-available-command
  (let [expected (->> (commands/palette-listed :viewer)
                   (remove #(= :search (:id %)))
                   (map :id)
                   set)
        rows (with-redefs [bus/available? (fn [_ _] true)]
               (palette/palette-rows :viewer {} ""))]
    (is (> (count expected) 12))
    (is (= expected (set (map :id (butlast rows)))))
    (is (= :search (:id (last rows))))))
