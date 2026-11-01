(ns freememo.generation-commit-test
  (:require [clojure.test :refer [deftest is]]
            [freememo.cards :as cards]
            [freememo.db :as db]
            [freememo.work-run :as run]))

(deftest cancellation-during-image-preparation-rejects-the-actual-insert
  (let [attempt (run/start) writes (atom [])]
    (with-redefs-fn
      {#'cards/bake-card-html (fn [_ _ card] card)
       (ns-resolve 'freememo.cards 'rehost-row-data-uris!)
       (fn [_ row] (run/cancel! attempt) {:row row :remaining 0})
       #'db/insert-flashcards! (fn [rows] (swap! writes into rows) [1])}
      (fn []
        (is (:cancelled?
              (cards/save-cards 1 2 2 "basic" [{:q "q" :a "a"}] true nil
                #(run/commit! attempt %))))
        (is (empty? @writes))))))
