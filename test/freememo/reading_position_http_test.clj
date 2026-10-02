(ns freememo.reading-position-http-test
  "plans/reading-position-memory.md §4 1.6, 4.1: what the beacon route accepts."
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.reading-position-http :as h]))

(def ^:private good
  {"topic_id" "42" "surface" "editor" "i" "7" "fp" "Chapter two" "frac" "0.25"
   "fraction" "0.5"})

(deftest a-well-formed-beacon-parses
  (is (= {:topic-id 42 :surface "editor"
          :anchor {:i 7 :fp "Chapter two" :frac 0.25} :fraction 0.5}
        (h/position-body good)))
  (testing "an empty fingerprint is a real one: an empty paragraph"
    (is (some? (h/position-body (assoc good "fp" ""))))))

(deftest a-malformed-beacon-is-nil
  (doseq [[k v] [["topic_id" "x"] ["surface" "pdf"] ["i" "-1"] ["i" "1.5"]
                 ["fp" (apply str (repeat 65 "a"))] ["frac" "2"] ["frac" "NaN"]
                 ["fraction" "-0.1"] ["fraction" "Infinity"]]]
    (is (nil? (h/position-body (assoc good k v))) (str k " = " v)))
  (doseq [k (keys good)]
    (is (nil? (h/position-body (dissoc good k))) (str "missing " k))))
