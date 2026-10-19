(ns freememo.command-bus-target-test
  "plans/custom-context-menu.md 1.3 and 1.4: the bus checks a target at the
   boundary, and the palette lists a target command only for a focus target."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [freememo.command-bus :as bus]
            [freememo.command-palette :as palette]
            [freememo.interaction-trail :as trail]))

(use-fixtures :each
  (fn [t]
    (let [ctx @bus/!ctx inv @bus/!invocations req @bus/!input-requester]
      (try (reset! bus/!ctx {:active-tab :library})
           (reset! bus/!invocations [])
           (with-redefs [trail/record! (fn [& _] nil)] (t))
           (finally (reset! bus/!ctx ctx)
                    (reset! bus/!invocations inv)
                    (reset! bus/!input-requester req))))))

(def topic {:kind :topic :subjects [{:topic-id 7 :title "A" :root? true :status "active"}]})
(def card {:kind :card :subjects [{:card-id 3}]})

(deftest a-payload-of-the-wrong-kind-is-unavailable
  (is (= :unavailable (bus/dispatch! :rename-topic card :menu)))
  (is (= [] @bus/!invocations)))

(deftest a-menu-input-command-asks-for-its-value-first
  (let [asked (atom nil)]
    (bus/register-input-requester! (fn [id target] (reset! asked [id target])))
    (is (= :awaiting-input (bus/dispatch! :rename-topic topic :menu)))
    (is (= [:rename-topic topic] @asked))
    (is (= [] @bus/!invocations) "nothing queued before the value exists")))

(deftest a-queued-target-drops-its-surface
  (is (= :dispatched
        (bus/dispatch! :rename-topic (assoc topic :input "B" :surface {:anchor :node}) :menu)))
  (is (= [(assoc topic :input "B")] (map :payload @bus/!invocations))))

(deftest with-no-payload-the-focus-target-is-the-target
  (bus/publish-focus-target! ::row topic)
  (is (bus/available? :mark-topic-done))
  (is (= :dispatched (bus/dispatch! :mark-topic-done nil :palette)))
  (is (= [topic] (map :payload @bus/!invocations))))

(deftest a-late-retract-keeps-a-newer-focus-target
  (bus/publish-focus-target! ::old topic)
  (bus/publish-focus-target! ::new card)
  (bus/retract-focus-target! ::old)
  (is (= card (:focus-target @bus/!ctx)))
  (bus/retract-focus-target! ::new)
  (is (nil? (:focus-target @bus/!ctx))))

(deftest the-palette-lists-a-target-command-only-for-a-focus-target
  (testing "no focus target: Rename is not listed"
    (is (not-any? #(= :rename-topic (:id %))
          (palette/palette-rows :library @bus/!ctx "Rename"))))
  (testing "a topic focus target: Rename is listed"
    (bus/publish-focus-target! ::row topic)
    (is (some #(= :rename-topic (:id %))
          (palette/palette-rows :library @bus/!ctx "Rename"))))
  (testing "a card focus target: Rename is not listed"
    (bus/publish-focus-target! ::row card)
    (is (not-any? #(= :rename-topic (:id %))
          (palette/palette-rows :library @bus/!ctx "Rename")))))
