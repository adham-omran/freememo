(ns freememo.content-toolbar-helpers-test
  (:require
   [clojure.test :refer [deftest is]]
   [freememo.content-toolbar-helpers :as helpers]
   [missionary.core :as m]))

(defn- await-task
  "Run `task` and return its callback type and value.
   Pre:  `task` is a Missionary task that completes within one second.
   Post: returns `[:success value]`, `[:failure error]`, or `::timed-out`.
  Inv:  the test wait does not change task cancellation behavior."
  [task]
  (let [result (promise)]
    (let [cancel! (task #(deliver result [:success %])
                    #(deliver result [:failure %]))
          outcome (deref result 1000 ::timed-out)]
      (when (= ::timed-out outcome)
        (cancel!))
      outcome)))

(deftest saved-card-generation-cannot-report-an-outer-timeout
  (let [saved? (atom false)
        card-generation-task
        (ns-resolve 'freememo.content-toolbar-helpers 'card-generation-task)]
    (with-redefs [helpers/generate-and-save!
                  (fn [_]
                    (Thread/sleep 20)
                    (reset! saved? true)
                    {:success true})
                  m/timeout
                  (fn [& _]
                    (throw (ex-info "Card generation must not use an outer timeout" {})))]
      (is (= [:success {:success true}]
            (await-task (card-generation-task {}))))
      (is @saved?))))
