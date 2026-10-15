(ns freememo.biblio-merge-test
  "The merged CSL a bibliography import stores. Pure, so no database."
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.biblio-merge :as bm]))

(deftest the-resolver-wins-only-with-a-value
  (is (= {:title "Resolved" :author "Local"}
        (bm/merge-biblio {:local {:title "Local" :author "Local"}
                          :resolved {:title "Resolved" :author ""}}))))

(deftest a-merged-csl-holds-no-control-characters
  (testing "a NUL-padded PDF title, as production logged on 2026-10-14"
    (is (= "法语现代语法"
          (:title (bm/merge-biblio {:local {:title "法语现代语法\u0000\u0000"}})))))
  (testing "nested strings are cleaned, and tab, newline and return stay"
    (is (= {:title "a\tb\nc\rd" :author [{:family "Smith"}]}
          (bm/merge-biblio {:local {:title "a\tb\nc\rd\u0007"}
                            :resolved {:author [{:family "Sm\u0000ith"}]}})))))
