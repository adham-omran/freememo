(ns freememo.getting-started-steps-test
  "The checklist's states and hiding rules (plans/getting-started-checklist.md
   6.1). Pure, so it runs with no database."
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.getting-started-steps :as steps]))

(def nothing-done
  {:imported? false :read? false :extracted? false
   :carded? false :reviewed? false :anki-pushed? false})

(defn- states [input]
  (mapv (juxt :id :state) (steps/checklist input)))

(deftest a-new-account-sees-six-steps-with-import-next
  (is (= [[:import :next] [:learn :pending] [:extract :pending]
          [:cards :pending] [:quiz :pending] [:anki :pending]]
         (states {:progress nothing-done :ai-configured? true}))))

(deftest the-first-open-step-is-next
  (is (= [[:import :done] [:learn :done] [:extract :next]
          [:cards :pending] [:quiz :pending] [:anki :pending]]
         (states {:progress (assoc nothing-done :imported? true :read? true)
                  :ai-configured? true}))))

(deftest a-dismissed-checklist-hides
  (is (nil? (steps/checklist {:progress nothing-done :dismissed? true
                              :ai-configured? true}))))

(deftest the-optional-anki-step-does-not-keep-it-open
  (testing "Q5: every required step done, Anki not pushed"
    (is (nil? (steps/checklist
                {:progress (assoc nothing-done :imported? true :read? true
                             :extracted? true :carded? true :reviewed? true)
                 :ai-configured? true}))))
  (testing "Anki alone done does not hide it"
    (is (some? (steps/checklist {:progress (assoc nothing-done :anki-pushed? true)
                                 :ai-configured? true})))))

(deftest no-operator-key-makes-cards-unavailable
  (let [items (steps/checklist {:progress nothing-done :ai-configured? false})
        cards (first (filter #(= :cards (:id %)) items))]
    (is (= :unavailable (:state cards)))
    (is (= steps/ai-unavailable-text (:text cards))))
  (testing "an unavailable step never counts as open, so the rest can finish it"
    (is (nil? (steps/checklist
                {:progress (assoc nothing-done :imported? true :read? true
                             :extracted? true :reviewed? true)
                 :ai-configured? false}))))
  (testing "a card added by hand completes the step with no AI"
    (is (= :done (->> (steps/checklist {:progress (assoc nothing-done :carded? true)
                                        :ai-configured? false})
                   (filter #(= :cards (:id %)))
                   first
                   :state)))))

(deftest exactly-one-step-is-next
  (doseq [done-keys (reductions conj [] (map :done-key steps/steps))
          :let [items (steps/checklist {:progress (merge nothing-done
                                                     (zipmap done-keys (repeat true)))
                                        :ai-configured? true})]
          :when items]
    (is (= 1 (count (filter #(= :next (:state %)) items)))
        (str "done: " done-keys))))
