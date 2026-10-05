(ns freememo.question-map-block-test
  (:require [clojure.test :refer [are deftest is testing]]
            [freememo.question-map-block :as block]))

(def reply
  "```freememo-map\n{\"next\": \"A\"}\n```\n\nHow many axes does §4.7 list?")

(deftest split-reply-separates-block-and-prose
  (is (= {:block "{\"next\": \"A\"}\n" :prose "How many axes does §4.7 list?"
          :pending? false}
        (block/split-reply reply)))
  (testing "leading whitespace before the fence"
    (is (= "{\"next\": \"A\"}\n" (:block (block/split-reply (str "\n  " reply)))))))

(deftest a-streaming-block-shows-no-raw-data
  (testing "§6.3.2.1: every prefix of the reply up to the closing fence shows nothing"
    (let [close (+ (.indexOf ^String reply "```" 5) 3)]
      (doseq [n (range 1 close)]
        (is (= "" (block/prose (subs reply 0 n))) (str "prefix " n))))))

(deftest a-reply-without-a-block-is-all-prose
  (is (= "Plain answer." (block/prose "Plain answer.")))
  (is (= "``` code\nx\n```" (block/prose "``` code\nx\n```")))
  (is (= "Use ```freememo-map later." (block/prose "Use ```freememo-map later."))))

(def roots [{:label "Signature axes" :status "next"}
            {:label "Subtyping" :status "open" :known? true}])

(deftest problem-accepts-a-valid-block
  (is (nil? (block/problem {:roots [{:label "Ports" :in_document true :citations ["E2"]}]
                            :pins {:1 "ports" :2 "Subtyping"}
                            :next "Ports" :learned ["signature axes"]}
              (map :label roots) [1 2]))))

(deftest problem-names-each-defect
  (are [data] (string? (block/problem data (map :label roots) [1 2]))
    []
    {:roots {}}
    {:roots [{:label "" :in_document true}]}
    {:roots [{:label "X"}]}
    {:roots [{:label "X" :in_document true :citations ["3"]}]}
    {:pins {:9 "Subtyping"}}
    {:pins {:1 "Nowhere"}}
    {:learned "Subtyping"}
    {:learned ["Nowhere"]}
    {:next "Nowhere"}
    {:roots (vec (for [i (range 13)] {:label (str "R" i) :in_document true}))}))

(deftest plan-places-every-open-pin-on-a-diagnosis
  (testing "§6.3.3: a pin the block leaves out gets a root of its own"
    (let [p (block/plan {:roots [{:label "Ports" :in_document true :citations ["E2"]}]
                         :pins {:1 "Ports"}}
              {:diagnosis? true :roots []
               :open-pins [{:label 1 :question "Q1" :quote "q1"}
                           {:label 2 :question "" :quote "closure signature"}]})]
      (is (= {1 "Ports" 2 "Pin 2: closure signature"} (:pin-roots p)))
      (is (= ["Ports" "Pin 2: closure signature"] (mapv :label (:new-roots p))))
      (is (= "Ports" (:next p)) "a map with no next root gets one")))
  (testing "an update run leaves unnamed pins alone"
    (is (= {} (:pin-roots (block/plan {} {:diagnosis? false :roots []
                                          :open-pins [{:label 1 :quote "q"}]}))))))

(deftest plan-keeps-existing-spelling-and-status
  (let [p (block/plan {:roots [{:label "subtyping" :in_document false}]
                       :pins {:1 "SUBTYPING"} :learned ["signature axes"]}
            {:diagnosis? true :roots roots :open-pins [{:label 1 :quote "q"}]})]
    (is (empty? (:new-roots p)) "an existing root is not added again")
    (is (= {1 "Subtyping"} (:pin-roots p)))
    (is (= #{"Signature axes"} (:learned p)))
    (is (nil? (:next p)) "a known root is not chosen when the block names no next"))
  (testing "a learned root is never made next"
    (is (nil? (:next (block/plan {:next "Subtyping" :learned ["Subtyping"]}
                       {:roots roots :open-pins []}))))))
