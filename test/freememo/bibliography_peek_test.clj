(ns freememo.bibliography-peek-test
  "What the bibliography card shows for a source. Needs no database."
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.bibliography-peek :as peek]))

(def ^:private full-form
  {:title "  On Computable Numbers " :url "https://example.org/turing"
   :container-title "Proceedings of the London Mathematical Society"
   :issued "1936" :page "230-265" :accessed "2026-10-30"
   :authors [{:given "Alan" :family "Turing"} {:given "" :family "Church"}
             {:given " " :family ""}]})

(deftest every-filled-field-in-reading-order
  (is (= {:title "On Computable Numbers"
          :url "https://example.org/turing"
          :lines ["Alan Turing, Church"
                  "Proceedings of the London Mathematical Society"
                  "Published 1936"
                  "Page 230-265"]
          :inherited? false}
        (peek/peek-lines {:form full-form :inherited? false}))))

(deftest blank-fields-leave-no-row
  (is (= {:title nil :url nil :lines [] :inherited? true}
        (peek/peek-lines {:form {:title "" :url " " :container-title ""
                                 :issued "" :page "" :authors []}
                          :inherited? true}))))

(deftest no-source-no-card
  (is (nil? (peek/peek-lines nil))))

(deftest author-names
  (testing "blank names are dropped, and none left is nil"
    (is (nil? (peek/author-names [{:given "" :family ""}])))
    (is (nil? (peek/author-names nil)))
    (is (= "Ada Lovelace" (peek/author-names [{:given "Ada" :family "Lovelace"}])))))
