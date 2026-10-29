(ns freememo.icon-dom-test
  (:require [clojure.test :refer [deftest is]]
            [freememo.icon-dom :as icon-dom]))

(deftest size-and-optional-attributes-preserve-the-icon-contract
  (doseq [size [16 0 "1.25em" "100%" nil]]
    (let [attrs (icon-dom/root-attributes size "example" "A title")]
      (is (= (str size) (:width attrs) (:height attrs)))
      (is (= "example" (:class attrs)))
      (is (= "false" (:aria-hidden attrs)))))
  (let [attrs (icon-dom/root-attributes 16 nil nil)]
    (is (nil? (:class attrs)))
    (is (= "true" (:aria-hidden attrs)))
    (is (= "0 0 24 24" (:viewBox attrs)))
    (is (= "none" (:fill attrs)))
    (is (= "currentColor" (:stroke attrs)))
    (is (= "2" (:stroke-width attrs)))))
