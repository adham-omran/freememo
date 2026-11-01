(ns freememo.focus-window-model-test
  (:require [clojure.test :refer [deftest is]]
            [freememo.focus-window-model :as model]))

(defn page-ops [page text]
  [{:insert (str "Page " page)}
   {:insert "\n" :attributes {:id (str "fm-page-" page) :header 6}}
   {:insert (str text "\n")}])

(deftest repeated-text-retains-the-same-pages
  (let [ops (vec (mapcat #(page-ops % "identical repeated body") (range 1 21)))
        spans (model/page-spans ops 1)
        length (second (get spans 20))
        plan (model/patch-plan spans (range 1 21) (range 11 31) length)]
    (is (= (first (get spans 11)) (:keep-start plan)))
    (is (= length (:keep-end plan)))
    (is (= [] (:prefix-pages plan)))
    (is (= (vec (range 21 31)) (:suffix-pages plan)))
    (is (> (:keep-start plan) (first (get spans 1))))))

(deftest leading-text-stays-with-the-lowest-owned-page
  (let [ops (into [{:insert "Leading local edit\n"}]
              (concat (page-ops 11 "Body11") (page-ops 12 "Body12")))
        spans (model/page-spans ops 11)
        plan (model/patch-plan spans [11 12] (range 1 21) (second (get spans 12)))]
    (is (= 0 (first (get spans 11)) (:keep-start plan)))
    (is (= (vec (range 1 11)) (:prefix-pages plan)))))

(deftest a-deleted-first-marker-keeps-its-prefix-owner
  (let [spans (model/page-spans
                (into [{:insert "Unmarked page11 text\n"}] (page-ops 12 "Body12")) 11)]
    (is (= [0 (count "Unmarked page11 text\n")] (get spans 11)))
    (is (= (count "Unmarked page11 text\n") (first (get spans 12))))))

(deftest all-unmarked-text-belongs-to-the-lowest-owned-page
  (is (= {11 [0 14]} (model/page-spans [{:insert "Unmarked text\n"}] 11))))

(deftest a-leading-prefix-needs-an-explicit-reload-before-prepending-pages
  (is (model/prefix-reload? (range 11 31) (range 1 21) 10 [0 100]))
  (is (model/prefix-reload? (range 11 31) (range 1 21) nil [0 100]))
  (is (not (model/prefix-reload? (range 11 31) (range 1 21) 0 [0 100])))
  (is (not (model/prefix-reload? (range 11 31) (range 21 41) 10 [0 100]))))
