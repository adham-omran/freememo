(ns freememo.command-palette-test
  (:require [clojure.test :refer [deftest is]]
            [freememo.command-bus :as bus]
            [freememo.command-palette :as palette]
            [freememo.commands :as commands]))

(deftest empty-query-shows-every-available-command
  (let [expected (->> (commands/palette-listed :viewer)
                   (remove #(= :search (:id %)))
                   (map :id)
                   set)
        rows (with-redefs [bus/available? (fn [_ _] true)]
               (palette/palette-rows :viewer {} ""))]
    (is (> (count expected) 12))
    (is (= expected (set (map :id (butlast rows)))))
    (is (= :search (:id (last rows))))))

(defn- rows-for [query]
  (with-redefs [bus/available? (fn [_ _] true)]
    (palette/palette-rows :viewer {} query)))

(deftest help-rows-follow-commands-and-precede-search
  ;; plans/searchable-help.md 4.1
  (let [rows (rows-for "anki")
        kinds (map #(cond (:search-row %) :search (:help-slug %) :help :else :command) rows)]
    (is (some #{:help} kinds))
    (is (= kinds (sort-by {:command 0 :help 1 :search 2} kinds))
      "commands, then help rows, then the search row")
    (is (some #(= "Syncing to Anki" (:label %)) (filter :help-slug rows)))))

(deftest a-help-row-matches-on-a-step-label
  ;; No title holds "transcribe"; the Audio and Video sections each have a
  ;; step label that does.
  (is (= ["Import audio" "Import video"] (map :label (filter :help-slug (rows-for "transcribe")))))
  (is (empty? (filter :help-slug (rows-for "xyzzy")))))

(deftest a-blank-query-lists-no-help-rows
  ;; plans/help-import-sources.md 3.1
  (is (empty? (filter :help-slug (rows-for ""))))
  (is (empty? (filter :help-slug (rows-for "  ")))))

(deftest a-title-match-ranks-before-a-step-label-match
  (let [help-labels (map :label (filter :help-slug (rows-for "quiz")))]
    (is (= "Testing yourself — Quiz & Timed Quiz" (first help-labels)))))

(deftest shift-enter-target
  (let [scan (some #(when (= :scan (:id %)) %) (rows-for "scan"))
        search-row (last (rows-for "scan"))]
    (is (= "studying-a-pdf" (palette/row-help-slug scan)))
    (is (nil? (palette/row-help-slug search-row)))))
