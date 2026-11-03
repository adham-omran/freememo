(ns freememo.exam-grid-test
  "The grids of plans/blueprint-first-exams.md §4, row by row."
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.exam-grid :as grid]))

(deftest each-format-has-its-minutes-and-points
  (is (= {"multiple-choice" 1.25 "short-essay" 5 "long-essay" 12} grid/format-minutes))
  (is (= {"multiple-choice" 1 "short-essay" 4 "long-essay" 10} grid/format-points)))

(deftest the-target-runs-from-5-to-90-minutes
  (is (grid/minutes-in-range? 5))
  (is (grid/minutes-in-range? 90))
  (is (not (grid/minutes-in-range? 4)))
  (is (not (grid/minutes-in-range? 91)))
  (is (not (grid/minutes-in-range? 30.5))))

(deftest the-mixed-grid-matches-the-plan-table
  ;; T long short mcq questions points minutes (plan 4.3)
  (doseq [[t long-n short-n mcq questions points minutes]
          [[5 0 0 4 4 4 5.0]
           [20 0 1 12 13 16 20.0]
           [30 1 1 10 12 24 29.5]
           [45 1 2 18 21 36 44.5]
           [60 1 3 26 30 48 59.5]
           [90 2 4 34 40 70 86.5]]]
    (testing t
      (is (= {:long long-n :short short-n :mcq mcq :questions questions
              :points points :minutes minutes}
             (grid/summary :mixed t))))))

(deftest the-essay-grid-matches-the-plan-table
  ;; T long short essays points minutes (plan 4.4)
  (doseq [[t long-n short-n essays points minutes]
          [[5 0 1 1 4 5]
           [20 0 4 4 16 20]
           [30 1 4 5 26 32]
           [45 1 7 8 38 47]
           [60 1 8 9 42 52]
           [75 2 8 10 52 64]
           [90 4 8 12 72 88]]]
    (testing t
      (is (= {:long long-n :short short-n :mcq 0 :questions essays
              :points points :minutes minutes}
             (update (grid/summary :essay t) :minutes long))))))

(deftest no-grid-passes-its-limits-at-any-target
  (doseq [t (range grid/min-minutes (inc grid/max-minutes))
          kind grid/grid-kinds]
    (let [{:keys [questions] short-n :short long-n :long} (grid/summary kind t)]
      (is (<= 1 questions grid/max-questions) [kind t])
      (is (<= short-n grid/max-short-essays) [kind t])
      (is (<= long-n grid/max-long-essays) [kind t]))))

(deftest essays-take-the-roles-in-turn
  (is (= [["short-essay" "explain" "understand"] ["short-essay" "compare" "analyze"]
          ["short-essay" "apply" "apply"] ["short-essay" "evaluate" "evaluate"]
          ["long-essay" "synthesize" "create"]]
         (mapv (juxt :format :role :level) (grid/slots :essay 30))))
  (is (= ["synthesize" "evaluate" "synthesize" "evaluate"]
         (mapv :role (filter #(= "long-essay" (:format %)) (grid/slots :essay 90)))))
  (testing "the mixed grid leaves roles and levels to the blueprint"
    (is (every? #(and (nil? (:role %)) (nil? (:level %))) (grid/slots :mixed 45)))))

(deftest slots-are-numbered-in-order
  (is (= ["s1" "s2" "s3" "s4" "s5"] (mapv :id (grid/slots :essay 30)))))

(deftest the-slider-line-names-each-count
  (is (= "45 minutes: 18 multiple-choice questions, 2 short essays, 1 long essay (36 points)"
         (grid/summary-text :mixed 45)))
  (is (= "5 minutes: 1 short essay (4 points)" (grid/summary-text :essay 5))))

(deftest the-estimate-scales-with-the-question-count
  (let [sheet {:input-usd 0.05 :usd-per-output-token 4.125e-6 :credits-per-usd 2660.0}
        short-exam (grid/estimate sheet :essay 30)
        long-exam (grid/estimate sheet :mixed 45)]
    (is (< (:usd short-exam) (:usd long-exam)))
    (is (= (+ 4000 (* 900 21)) (grid/output-tokens :mixed 45)))
    (is (= (long (Math/ceil (* (:usd long-exam) 2660.0))) (:credits long-exam)))
    (is (nil? (:credits (grid/estimate (dissoc sheet :credits-per-usd) :mixed 45))))))
