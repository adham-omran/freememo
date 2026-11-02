(ns freememo.anki-reconcile-panels-test
  "What the reconcile run says when it ends (plans/remove-toasts.md 2.4.2).

   The summary goes to the Activity row, the cards Anki refused go to the banner
   strip, and an aborted plan is the run's error. Needs no database."
  (:require
   [clojure.test :refer [deftest is testing]]
   [freememo.activity :as activity]
   [freememo.anki-reconcile-panels :as panels]))

(def ^:private result
  {:success true :counts {:pulls 0 :conflicts 0}})

(deftest a-failed-run-has-no-summary
  (is (nil? (panels/outcome-summary :error {:result nil :plan nil :tally {}}))
    "its error reaches the row through the lease release"))

(deftest a-cancelled-run-reports-how-far-it-got
  (is (= "Sync cancelled. 3 of 10 cards sent to Anki."
        (panels/outcome-summary :cancelled
          {:plan {:write-card-ids (range 10)}
           :tally {:written 3 :errors []}}))))

(deftest a-finished-run-states-conflicts-and-leaves-refusals-out
  (let [summary (panels/outcome-summary :done
                  {:result (assoc-in result [:counts :conflicts] 2)
                   :plan {}
                   :tally {:written 0 :errors [{:card 1} {:card 2} {:card 3}]}})]
    (is (re-find #"2 cards need a decision in Library" summary))
    (is (not (re-find #"refused" summary))
      "a refusal is a failure, so it goes to the banner"))
  (testing "a branch run says so"
    (is (re-find #"^This branch: "
          (panels/outcome-summary :done
            {:result result :plan {} :tally {:written 0 :errors []}
             :branch? true})))))

(deftest a-dry-run-says-nothing-was-applied
  (is (re-find #"^Dry run, nothing applied\."
        (panels/outcome-summary :done
          {:result nil :plan {:report "5 would be sent"} :tally {}}))))

(deftest refused-cards-are-counted-from-the-tally
  (is (= 2 (panels/refused-count {:tally {:errors [{} {}]}})))
  (is (= 0 (panels/refused-count {:tally {:errors []}}))))

(deftest refused-cards-record-one-failure-and-none-when-zero
  (let [calls (atom [])]
    (with-redefs [activity/record-failure! (fn [uid m] (swap! calls conj [uid m]) 1)]
      (is (nil? (panels/record-refused!* 7 0)))
      (is (empty? @calls))
      (is (nil? (panels/record-refused!* 7 3)))
      (is (= 1 (count @calls)))
      (let [[uid m] (first @calls)]
        (is (= 7 uid))
        (is (= "Anki sync" (:what m)))
        (is (re-find #"^3 cards Anki refused" (:error m)))))))
