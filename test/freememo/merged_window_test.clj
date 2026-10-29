(ns freememo.merged-window-test
  (:require [clojure.test :refer [deftest is]]
            [freememo.merged-window :as window]))

(deftest page-ranges-cover-the-document-without-gaps
  (is (= [1 20] (window/page-range 1 883)))
  (is (= [1 20] (window/page-range 20 883)))
  (is (= [21 40] (window/page-range 21 883)))
  (is (= [881 883] (window/page-range 883 883)))
  (is (= [881 883] (window/page-range 100000 883)))
  (is (= [1 1] (window/page-range nil nil)))
  (let [ranges (distinct (map #(window/page-range % 883) (range 1 884)))]
    (is (= (vec (range 1 884)) (vec (mapcat (fn [[start end]] (range start (inc end))) ranges))))))

(deftest a-new-body-waits-for-its-current-edit
  (is (window/install-allowed? nil nil))
  (is (not (window/install-allowed? {:edit-id "second"} "first")))
  (is (not (window/install-allowed? {:edit-id "second"} nil)))
  (is (window/install-allowed? {:edit-id "second"} "second")))

(deftest each-edit-retains-the-installed-body-owner
  (let [scope {:user-id 1 :topic-id 10 :editor-doc-id 5 :merged? true
               :pdf-root-id 5 :page-numbers (vec (range 1 21))
               :body-key (window/body-key 5 true (range 1 21))}
        state {:topic-id 999 :save-scope scope}
        dirty (window/edit-entry state "<p>First range edit</p>" "edit-1")
        requested (assoc-in state [:save-scope :page-numbers] (vec (range 21 41)))]
    (is (= 5 (:topic-id dirty)))
    (is (= scope (:save-scope dirty)))
    (is (not= (:save-scope requested) (:save-scope dirty)))
    (is (= (vec (range 1 21)) (get-in dirty [:save-scope :page-numbers])))))

(deftest a-remounted-editor-does-not-replay-an-acknowledged-edit
  (let [dirty {:edit-id "saved" :save-scope {:user-id 7}}]
    (is (not (window/save-needed? dirty 7 "saved" nil)))
    (is (window/save-needed? dirty 7 "previous" nil))
    (is (not (window/save-needed? dirty 8 "previous" nil)))
    (is (not (window/save-needed? nil 7 "saved" nil)))))
