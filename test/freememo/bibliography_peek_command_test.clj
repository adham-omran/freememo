(ns freememo.bibliography-peek-command-test
  "⌘⇧B (`:peek-bibliography`, plans/feedback-activity-biblio-youtube.md 3.2.4)
   and the context key that serves it. Needs no database."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [freememo.command-bus :as bus]
            [freememo.interaction-trail :as trail]))

(use-fixtures :each
  (fn [t]
    (let [ctx @bus/!ctx]
      (try (reset! bus/!ctx {:active-tab :viewer})
           (with-redefs [trail/record! (fn [& _] nil)] (t))
           (finally (reset! bus/!ctx ctx))))))

(deftest the-command-is-hidden-without-a-citation
  (is (not (bus/available? :peek-bibliography)))
  (is (= :unavailable (bus/dispatch! :peek-bibliography nil :keyboard))))

(deftest the-command-toggles-the-published-peek
  (let [calls (atom 0)]
    (bus/publish-ctx! {:open-bibliography-peek! #(swap! calls inc)})
    (is (bus/available? :peek-bibliography))
    (is (= :dispatched (bus/dispatch! :peek-bibliography nil :keyboard)))
    (is (= 1 @calls))
    (testing "outside the viewer the chord is not the app's"
      (swap! bus/!ctx assoc :active-tab :library)
      (is (not (bus/available? :peek-bibliography))))))

(deftest an-old-unmount-cannot-withdraw-a-new-publisher
  (let [old-toggle (fn [] :old)
        new-toggle (fn [] :new)]
    (bus/publish-ctx! {:open-bibliography-peek! old-toggle})
    (bus/publish-ctx! {:open-bibliography-peek! new-toggle})
    (bus/retract-ctx-value! :open-bibliography-peek! old-toggle)
    (is (identical? new-toggle (:open-bibliography-peek! @bus/!ctx)))
    (bus/retract-ctx-value! :open-bibliography-peek! new-toggle)
    (is (not (contains? @bus/!ctx :open-bibliography-peek!)))))
