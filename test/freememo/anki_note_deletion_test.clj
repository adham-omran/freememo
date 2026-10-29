(ns freememo.anki-note-deletion-test
  (:require [clojure.test :refer [deftest is]]
            [missionary.core :as m]
            [freememo.anki-note-deletion :as deletion])
  (:import [missionary Cancelled]))

(defn- await! [task]
  (let [result (promise)]
    (task #(deliver result [:ok %]) #(deliver result [:error %]))
    (deref result 10000 [:timeout])))

(deftest one-hundred-thousand-notes-use-sequential-bounded-attempts
  (let [calls (atom [])
        ids (range 100000)
        result (await! (deletion/delete-notes!
                         (fn [action data]
                           (m/sp (swap! calls conj [action (:notes data)]) nil)) ids))]
    (is (= [:ok nil] result))
    (is (= 200 (count @calls)))
    (is (every? #(and (= "deleteNotes" (first %)) (<= (count (second %)) 500)) @calls))
    (is (= (vec ids) (into [] (mapcat second) @calls)))))

(deftest a-failed-attempt-does-not-lose-later-batches
  (let [calls (atom [])
        result (await! (deletion/delete-notes!
                         (fn [_ {:keys [notes]}]
                           (m/sp
                             (swap! calls conj notes)
                             (when (= 500 (first notes))
                               (throw (ex-info "Controlled deletion failure." {})))))
                         (range 1001)))]
    (is (= [:ok nil] result))
    (is (= [500 500 1] (mapv count @calls)))
    (is (= 1000 (first (last @calls))))))

(deftest cancellation-stops-the-next-attempt
  (let [calls (atom 0) result (promise)
        task (deletion/delete-notes!
               (fn [_ _] (m/sp (swap! calls inc) (m/? (m/sleep 10000))))
               (range 100000))
        cancel! (task #(deliver result [:ok %]) #(deliver result [:error %]))]
    (cancel!)
    (let [[status error] (deref result 1000 [:timeout])]
      (is (= :error status))
      (is (instance? Cancelled error))
      (is (= 1 @calls)))))

(deftest empty-and-nil-inputs-make-no-request
  (let [calls (atom 0)
        call! (fn [_ _] (m/sp (swap! calls inc)))]
    (is (= [:ok nil] (await! (deletion/delete-notes! call! []))))
    (is (= [:ok nil] (await! (deletion/delete-notes! call! nil))))
    (is (zero? @calls))))
