(ns freememo.curriculum-exam-render-test
  "The two pure helpers that turn stored exam JSON into reader-facing prose.

   They live in their own namespace rather than beside the source contracts in
   `freememo.curriculum-page-test`, because reaching them means LOADING the
   Electric page namespace. A load failure there would take the source
   contracts down with it, and those must keep running."
  (:require
   [clojure.test :refer [deftest is testing]]
   [freememo.curriculum-exam-page :as page]))

(deftest an-outcome-renders-as-a-sentence
  ;; The generated blueprint holds maps. `(str outcome)` printed the whole map,
  ;; so the Blueprint card read as EDN on every generated form.
  (testing "a generated outcome shows its statement"
    (is (= "Explain the four design goals."
          (page/outcome-text {:id "F1-goals" :statement "Explain the four design goals."}))))
  (testing "an older string outcome is unchanged"
    (is (= "Explain the goals." (page/outcome-text "Explain the goals."))))
  (testing "a blank statement falls through to the next readable field"
    (is (= "Fallback." (page/outcome-text {:statement "   " :text "Fallback."}))))
  (testing "a map with nothing readable shows itself rather than nothing"
    (is (= "{:id \"F1\"}" (page/outcome-text {:id "F1"})))))

(deftest a-packet-renders-as-the-passage-it-froze
  (testing "the section texts join in order"
    (is (= "One.\n\nTwo."
          (page/packet-text {:digest "d" :sections [{:text "One."} {:text "Two."}]}))))
  (testing "a blank section is left out"
    (is (= "One." (page/packet-text {:sections [{:text "One."} {:text "  "}]}))))
  (testing "an older top-level shape still reads"
    (is (= "Quoted." (page/packet-text {:quote "Quoted."}))))
  (testing "a packet with nothing readable shows itself rather than nothing"
    (is (= "{:digest \"d\"}" (page/packet-text {:digest "d"})))))
