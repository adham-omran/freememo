(ns freememo.bibliography-form-test
  "CSL to form translation for the bibliography editor and the peek card.
   Needs no database."
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.bibliography-form]
            [freememo.bibliography-peek :as peek]))

(def ^:private csl->form @#'freememo.bibliography-form/csl->form)

(deftest zotero-string-date-parts
  ;; Zotero's /csljson export writes date parts as strings, and the Zotero
  ;; stage stores that CSL unchanged. `(int "2002")` threw a
  ;; ClassCastException, which failed the Electric server process and closed
  ;; the socket when the peek card opened (prod topic 88818, 2026-11-11).
  (testing "a year-only string"
    (is (= "2002" (:issued (csl->form {:issued {:date-parts [["2002"]]}}))))
    (is (= "2002-01-01" (:issued (csl->form {:issued {:date-parts [["2002" 1 1]]}})))
        "a padded Zotero year reads as a padded integer year does"))
  (testing "strings and numbers mixed in one date"
    (is (= "2024-03-05" (:accessed (csl->form {:accessed {:date-parts [["2024" 3 "5"]]}}))))
    (is (= "2002-09" (:issued (csl->form {:issued {:date-parts [["2002" "9"]]}})))))
  (testing "integer date parts are unchanged"
    (is (= "2002-09-17" (:issued (csl->form {:issued {:date-parts [[2002 9 17]]}}))))
    (is (= "1936" (:issued (csl->form {:issued {:date-parts [[1936]]}})))))
  (testing "a part that is not a number gives no date, not an exception"
    (is (= "" (:issued (csl->form {:issued {:date-parts [["spring"]]}}))))
    (is (= "" (:issued (csl->form {:issued {:raw "Spring 2002"}}))))))

(deftest zotero-item-reaches-the-peek-card
  ;; The whole path the peek card runs: a Zotero item as /csljson exports it
  ;; and the Zotero stage stores it, then the card's content.
  (let [zotero {:id "http://zotero.org/users/1/items/ABCD1234"
                :type "paper-conference"
                :title "Two Simplified Algorithms for Maintaining Order in a List"
                :container-title "Algorithms — ESA 2002"
                :page "152-164"
                :author [{:family "Bender" :given "Michael A."}
                         {:family "Cole" :given "Richard"}]
                :issued {:date-parts [["2002" "9" "17"]]}
                :accessed {:date-parts [["2026" 8 "30"]]}}
        form (csl->form zotero)]
    (is (= "2002-09-17" (:issued form)))
    (is (= "2026-08-30" (:accessed form)))
    (is (= ["Michael A. Bender, Richard Cole" "Algorithms — ESA 2002"
            "Published 2002-09-17" "Page 152-164"]
           (:lines (peek/peek-lines {:form form :inherited? false}))))))
