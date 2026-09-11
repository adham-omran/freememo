(ns freememo.optimistic-test
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.optimistic :as optimistic]
            [freememo.client-state :as cs]
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

(deftest deleting-a-row-hides-it-before-server-completion
  (let [queue (cs/get-atom :pending-commands)
        deletions (cs/get-atom :pending-deletions)
        old-queue @queue
        old-deletions @deletions
        rows [[11] [21 22] [31]]]
    (try
      (reset! queue [])
      (reset! deletions {})
      (optimistic/enqueue-delete-card! [21 22])
      (let [command (first @queue)]
        (is (= :delete-card (:type command)))
        (is (= [21 22] (get-in command [:payload :member-ids])))
        (is (= [0 2] (optimistic/visible-card-indexes rows @deletions)))
        (testing "failure restores the row and removes the command"
          (optimistic/ack-command! command {:ok? false :error "Delete failed"})
          (is (= [0 1 2] (optimistic/visible-card-indexes rows @deletions)))
          (is (empty? @queue))))
      (testing "confirmation keeps the row hidden while the old query remains"
        (optimistic/enqueue-delete-card! [11])
        (optimistic/ack-command! (first @queue) {:ok? true :anki-note-ids []})
        (is (= [1 2] (optimistic/visible-card-indexes rows @deletions)))
        (is (empty? @queue)))
      (finally
        (reset! queue old-queue)
        (reset! deletions old-deletions)))))

(deftest confirmed-deletions-follow-each-query-revision
  (let [deletions {:delete-1 {:member-ids [21 22] :status :confirmed :card-revision 5}}
        old-rows [[11] [21 22] [31]]]
    (testing "an older query hides the deleted group"
      (is (= [0 2] (optimistic/visible-card-indexes old-rows deletions 4))))
    (testing "an unrelated filtered view cannot clear the deletion"
      (is (= [0] (optimistic/visible-card-indexes [[11]] deletions 5)))
      (is (= [0 2] (optimistic/visible-card-indexes old-rows deletions 4))))
    (testing "a query after Undo can show the restored group"
      (is (= [0 1 2] (optimistic/visible-card-indexes old-rows deletions 6))))
    (testing "pending deletes hide rows at every query revision"
      (is (= [0 2] (optimistic/visible-card-indexes old-rows
                       (assoc-in deletions [:delete-1 :status] :pending) 6))))))
