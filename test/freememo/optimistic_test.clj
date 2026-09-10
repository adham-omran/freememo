(ns freememo.optimistic-test
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.optimistic :as optimistic]
            [freememo.home-page :as home]))

(deftest queued-page-done-prefers-the-last-explicit-request
  (let [root-id 42
        page 7]
    (testing "no queued request preserves the persisted state"
      (is (false? (optimistic/queued-page-done? [] root-id page false))))
    (testing "a request changes its matching page immediately"
      (is (true? (optimistic/queued-page-done?
                  [{:type :set-page-done
                    :payload {:pdf-root-id root-id :page-number page :done? true}}]
                  root-id page false))))
    (testing "later requests converge to the last click"
      (is (false? (optimistic/queued-page-done?
                   [{:type :set-page-done
                     :payload {:pdf-root-id root-id :page-number page :done? true}}
                    {:type :set-page-done
                     :payload {:pdf-root-id root-id :page-number page :done? false}}]
                   root-id page false))))
    (testing "other pages cannot change this page's optimistic state"
      (is (true? (optimistic/queued-page-done?
                  [{:type :set-page-done
                    :payload {:pdf-root-id root-id :page-number (inc page) :done? false}}]
                  root-id page true))))))

(deftest scheduled-page-nudges-require-a-positive-quota-and-start-page
  (is (= [{:id 1 :pages-today 4 :next-page-number 3}]
        (home/scheduled-page-nudges
          [{:id 1 :pages-today 4 :next-page-number 3}
           {:id 2 :pages-today 0 :next-page-number 4}
           {:id 3 :pages-today -2 :next-page-number 5}
           {:id 4 :pages-today 1 :next-page-number nil}]))))
