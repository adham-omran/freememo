(ns freememo.merged-window-test
  (:require [clojure.set]
            [clojure.test :refer [deftest is]]
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

(deftest continuous-ranges-retain-overlap-in-both-directions
  (doseq [pages [(range 1 884) (range 883 0 -1)]]
    (reduce (fn [previous page]
              (let [[start end :as next] (window/continuous-range previous page 883)]
                (is (<= start page end))
                (is (<= (inc (- end start)) window/page-limit))
                (when (and previous (not= previous next))
                  (is (seq (clojure.set/intersection
                             (set (range (first previous) (inc (second previous))))
                             (set (range start (inc end)))))))
                next)) nil pages))
  (is (= [1 20] (window/continuous-range [1 20] 16 883)))
  (is (= [11 30] (window/continuous-range [1 20] 17 883)))
  (is (= [11 30] (window/continuous-range [11 30] 15 883)))
  (is (= [11 30] (window/continuous-range [11 30] 14 883))))

(deftest a-late-page-count-does-not-advance-the-reader
  (is (= [1 20] (window/continuous-range [1 1] 1 883)))
  (is (= [1 20] (window/continuous-range [1 1] 8 883)))
  (let [state (atom nil)]
    (is (= [1 1] (window/continuous-range! state 10 nil 0)))
    (is (= [1 20] (window/continuous-range! state 10 1 883)))
    (is (= [1 20] (window/continuous-range! state 10 8 883)))))

(deftest viewport-edges-retain-the-visible-page
  (is (= [11 30] (window/edge-range [1 20] 13 60 {:range [1 20] :page 13 :direction :forward})))
  (is (= [5 24] (window/edge-range [1 20] 5 60 {:range [1 20] :page 5 :direction :forward})))
  (is (= [1 20] (window/edge-range [1 20] 13 60 {:range [11 30] :page 13 :direction :forward})))
  (is (= [1 28] (window/edge-range [1 20] 1 60 {:range [1 20] :page 1 :direction :forward :capacity 28})))
  (is (= [1 40] (window/edge-range [1 20] 1 60 {:range [1 20] :page 1 :direction :forward :capacity 100})))
  (is (= [1 28] (window/continuous-range [1 28] 1 60))))

(deftest a-forward-edge-does-not-reverse-without-reader-input
  (let [range (window/edge-range [1 20] 4 60 {:range [1 20] :page 4 :direction :forward})]
    (is (= [4 23] range))
    (is (= range (window/continuous-range range 4 60)))))

(deftest changed-window-reads-wait-for-the-latest-save-and-freeze-after-install
  (let [state (atom nil)
        installed {:user-id 7 :pdf-root-id 9 :page-numbers (vec (range 1 21))}
        dirty (fn [id] {:edit-id id :save-scope {:user-id 7 :pdf-root-id 9}})
        request (fn [owner edit saved] (window/read-request! state 7 9 [11 30] owner edit saved))
        pending-a (request installed (dirty "A") nil)
        ready-a (request installed (dirty "A") "A")
        pending-b (request installed (dirty "B") "A")
        ready-b (request installed (dirty "B") "B")
        installed (assoc installed :page-numbers (vec (range 11 31)))]
    (is (false? (:ready? pending-a)))
    (is (:ready? ready-a))
    (is (= "A" (:fence ready-a)))
    (is (false? (:ready? pending-b)))
    (is (:ready? ready-b))
    (is (not (window/snapshot-current? (window/snapshot-tag ready-a) (window/snapshot-tag ready-b))))
    (is (= ready-b (request installed nil "B")))
    (is (= ready-b (request installed (dirty "C") "B")))))

(deftest a-foreign-document-edit-does-not-delay-the-new-read
  (let [state (atom nil)
        dirty {:edit-id "old" :save-scope {:user-id 7 :pdf-root-id 8}}
        request (window/read-request! state 7 9 [1 20] nil dirty nil)]
    (is (:ready? request))
    (is (nil? (:fence request)))))
