(ns freememo.walkthrough-text-test
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.walkthrough-text :as wtext]))

(deftest code-spans-reads-inline-code
  (is (= [[:text "Enables "] [:code "SpliceDef.spawnDef"] [:text " to accept "] [:code ".konst"]]
        (wtext/code-spans "Enables `SpliceDef.spawnDef` to accept `.konst`")))
  (testing "a backtick with no partner stays text"
    (is (= "a ` b" (apply str (map second (wtext/code-spans "a ` b"))))))
  (testing "a pair does not span a line break"
    (is (every? #(= :text (first %)) (wtext/code-spans "a `b\nc` d"))))
  (is (= [] (wtext/code-spans nil)))
  (is (= "Updates keyedResultType fallback" (wtext/plain "Updates `keyedResultType` fallback"))))
