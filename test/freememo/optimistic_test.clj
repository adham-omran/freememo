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

(deftest queued-pdf-done-prefers-the-last-explicit-request
  (let [root-id 42]
    (testing "no queued request preserves the persisted state"
      (is (false? (optimistic/queued-pdf-done? [] root-id false))))
    (testing "a request changes its matching PDF immediately"
      (is (true? (optimistic/queued-pdf-done?
                  [{:type :set-pdf-done
                    :payload {:pdf-root-id root-id :done? true}}]
                  root-id false))))
    (testing "later requests converge to the last click"
      (is (false? (optimistic/queued-pdf-done?
                   [{:type :set-pdf-done
                     :payload {:pdf-root-id root-id :done? true}}
                    {:type :set-pdf-done
                     :payload {:pdf-root-id root-id :done? false}}]
                   root-id false))))
    (testing "other PDFs cannot change this PDF's optimistic state"
      (is (true? (optimistic/queued-pdf-done?
                  [{:type :set-pdf-done
                    :payload {:pdf-root-id (inc root-id) :done? false}}]
                  root-id true))))))

(deftest scheduled-page-nudges-require-a-positive-quota-and-start-page
  (is (= [{:id 1 :pages-today 4 :next-page-number 3}]
        (home/scheduled-page-nudges
          [{:id 1 :pages-today 4 :next-page-number 3}
           {:id 2 :pages-today 0 :next-page-number 4}
           {:id 3 :pages-today -2 :next-page-number 5}
           {:id 4 :pages-today 1 :next-page-number nil}]))))
