(ns freememo.parse-questions-invoker-test
  (:require [clojure.test :refer [deftest is]]
            [freememo.cards :as cards]
            [freememo.commands :as commands]
            [freememo.parse-questions-invoker :as invoker]))

(deftest validation-messages-name-the-invalid-physical-lines
  (is (= "Could not parse line 2. Each nonblank line must contain one ? with text on both sides."
        (invoker/invalid-question-lines-message [2])))
  (is (= "Could not parse lines 2 and 4. Each nonblank line must contain one ? with text on both sides."
        (invoker/invalid-question-lines-message [2 4])))
  (is (= "Could not parse lines 1, 3, and 5. Each nonblank line must contain one ? with text on both sides."
        (invoker/invalid-question-lines-message [1 3 5]))))

(deftest result-line-reports-created-and-duplicate-counts
  (is (= "4 created" (invoker/result-line-text 4 0)))
  (is (= "3 created, 2 skipped as duplicates" (invoker/result-line-text 3 2)))
  (is (= "3 created, 1 skipped as duplicate" (invoker/result-line-text 3 1)))
  (is (= "0 created, 4 skipped as duplicates" (invoker/result-line-text 0 4))))

(deftest only-created-card-batches-highlight-the-selection
  (is (true? (invoker/created-cards-highlight-selection?
               {:success true :created 4})))
  (is (false? (invoker/created-cards-highlight-selection?
                {:success true :created 0})))
  (is (false? (invoker/created-cards-highlight-selection?
                {:success false :created 4}))))

(deftest valid-selection-saves-one-basic-card-batch
  (let [save-calls (atom [])
        bump-calls (atom [])]
    (with-redefs [cards/save-cards
                  (fn [& save-arguments]
                    (swap! save-calls conj save-arguments)
                    {:success true :ids [101 102 103 104] :skipped 0})
                  commands/bump! (fn [& bump-arguments]
                                   (swap! bump-calls conj bump-arguments) :done)]
      (is (= {:success true :ids [101 102 103 104] :skipped 0 :created 4}
            (invoker/create-selected-question-cards!*
              7 11 10
              "What is <x>? A & B\nWhy? Because\nWho? Someone\nWhere? Here")))
      (is (= [[7 11 10 "basic"
               [{:q "What is &lt;x&gt;?" :a "A &amp; B"}
                {:q "Why?" :a "Because"}
                {:q "Who?" :a "Someone"}
                {:q "Where?" :a "Here"}]
               false]]
            @save-calls))
      (is (= [[7 :parse-questions]] @bump-calls)))))

(deftest invalid-selection-writes-nothing-and-returns-the-error
  (let [save-calls (atom [])
        bump-calls (atom [])]
    (with-redefs [cards/save-cards (fn [& save-arguments]
                                     (swap! save-calls conj save-arguments))
                  commands/bump! (fn [& bump-arguments]
                                   (swap! bump-calls conj bump-arguments))]
      (let [creation-result (invoker/create-selected-question-cards!*
                              7 11 10 "Valid? Answer\nQuestion? Answer?")]
        (is (false? (:success creation-result)))
        (is (= [2] (:invalid-line-numbers creation-result)))
        (is (= (invoker/invalid-question-lines-message [2])
              (:error creation-result))))
      (is (empty? @save-calls))
      (is (empty? @bump-calls)))))

(deftest duplicates-do-not-bump-the-card-view
  (let [bump-calls (atom [])]
    (with-redefs [cards/save-cards
                  (fn [& _] {:success true :ids [] :skipped 2})
                  commands/bump! (fn [& bump-arguments]
                                   (swap! bump-calls conj bump-arguments))]
      (let [r (invoker/create-selected-question-cards!*
                7 11 10 "One? A\nTwo? B")]
        (is (:success r))
        (is (= 0 (:created r)))
        (is (= 2 (:skipped r))))
      (is (empty? @bump-calls)))))

(deftest save-failure-returns-the-error
  (with-redefs [cards/save-cards
                (fn [& _] {:success false :error "Database unavailable"})]
    (let [creation-result (invoker/create-selected-question-cards!*
                            7 11 10 "Question? Answer")]
      (is (false? (:success creation-result)))
      (is (= "Could not create cards: Database unavailable" (:error creation-result))))))
