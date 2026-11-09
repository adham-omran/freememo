(ns freememo.reset-on-key-test
  "util/reset-on-key! is the one reset path for view state that outlives a
   topic. The Topic View stays mounted across a Learn Session Next, so its
   atoms see the new topic as a new key, not as a new mount
   (plans/learn-next-speed/issues/06-reset-per-topic-state.md)."
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.util :as util]))

(deftest first-key-resets-every-atom
  (let [!seen (atom {}) !a (atom :stale) !b (atom :stale)]
    (is (nil? (util/reset-on-key! !seen :topic 1 [[!a nil] [!b false]])))
    (is (= [nil false] [@!a @!b]))
    (is (= {:topic 1} @!seen))))

(deftest same-key-keeps-what-the-user-changed
  (let [!seen (atom {}) !a (atom nil)]
    (util/reset-on-key! !seen :topic 1 [[!a nil]])
    (reset! !a {:card 7})
    (testing "an equal key re-emits on a refresh and must not reset"
      (util/reset-on-key! !seen :topic 1 [[!a nil]])
      (is (= {:card 7} @!a)))))

(deftest new-key-resets-again
  (let [!seen (atom {}) !a (atom nil)]
    (util/reset-on-key! !seen :topic 1 [[!a nil]])
    (reset! !a {:card 7})
    (util/reset-on-key! !seen :topic 2 [[!a nil]])
    (is (nil? @!a))
    (is (= {:topic 2} @!seen))))

(deftest slots-are-independent
  (let [!seen (atom {}) !a (atom nil) !b (atom nil)]
    (util/reset-on-key! !seen :topic 1 [[!a nil]])
    (util/reset-on-key! !seen :scope 10 [[!b nil]])
    (reset! !a :kept) (reset! !b :kept)
    (util/reset-on-key! !seen :scope 11 [[!b nil]])
    (util/reset-on-key! !seen :topic 1 [[!a nil]])
    (is (= [:kept nil] [@!a @!b]))))

(deftest nil-is-a-key
  (testing "an absent slot always seeds, even when the first key is nil"
    (let [!seen (atom {}) !a (atom :stale)]
      (util/reset-on-key! !seen :topic nil [[!a nil]])
      (is (nil? @!a)))))

(deftest on-reset-runs-once-per-new-key
  (testing "the thunk resets what no atom holds (an uncontrolled input box), before the atoms"
    (let [!seen (atom {}) !a (atom :stale) !log (atom [])
          on-reset! #(swap! !log conj [:thunk @!a])]
      (is (nil? (util/reset-on-key! !seen :topic 1 [[!a nil]] on-reset!)))
      (is (= [[:thunk :stale]] @!log))
      (reset! !a :typed)
      (util/reset-on-key! !seen :topic 1 [[!a nil]] on-reset!)
      (is (= :typed @!a) "an equal key runs nothing")
      (is (= 1 (count @!log)))
      (util/reset-on-key! !seen :topic 2 [[!a nil]] on-reset!)
      (is (= [[:thunk :stale] [:thunk :typed]] @!log))
      (is (nil? @!a)))))
