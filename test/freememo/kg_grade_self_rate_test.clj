(ns freememo.kg-grade-self-rate-test
  "The Review flow's answer to a credit refusal: the grade returns the reference
   answer, and the learner's own 1-4 rating schedules the item. A grade failure
   returns its error to the caller. Every database and credit
   seam is stubbed, so this runs with no PostgreSQL."
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.credits :as credits]
            [freememo.db :as db]
            [freememo.kg-grade :as grade]
            [freememo.kg-llm :as llm]
            [freememo.settings :as settings]))

(defn- refuse! [& _]
  (throw (ex-info "Out of credits. Top up in Settings to keep using AI features."
           {:type :freememo.kg-llm/insufficient-credits})))

(defmacro ^:private with-seams
  "Run body with the grader refused for credits. Binds `reviews` to an atom
   holding every apply-fsrs-review! call."
  [[reviews] & body]
  `(let [~reviews (atom [])]
     (with-redefs [llm/resolve-model+gate! refuse!
                   settings/fsrs-config (fn [_#] {:scheduler {} :enable-fuzzing false})
                   db/apply-fsrs-review!
                   (fn [user-id# item-ref# rating# record# _# _#]
                     (swap! ~reviews conj [user-id# item-ref# rating# record#])
                     {:due-today? false})
                   db/get-kg-question-for-session
                   (fn [_# qid#] {:id qid# :reference-answer "Staphylococcus aureus"})
                   db/get-card-item
                   (fn [_# card-id# ord#] {:card-id card-id# :ord ord#
                                           :prompt "<p>Q</p>" :answer "<p>A</p>"})]
       ~@body)))

(deftest a-refused-question-grade-carries-its-reference-and-writes-nothing
  (with-seams [reviews]
    (let [{:keys [result schedule]} (grade/review-question! 50 7 "cocci" 1200)]
      (is (false? (:success result)))
      (is (= :insufficient-credits (:error-type result)))
      (is (= "Staphylococcus aureus" (:reference-answer result)))
      (is (nil? schedule))
      (is (empty? @reviews) "a refused grade schedules nothing"))))

(deftest a-refused-card-grade-carries-the-card-answer
  (with-seams [reviews]
    (let [{:keys [result]} (grade/review-card! 50 11 0 "my answer" 900)]
      (is (= :insufficient-credits (:error-type result)))
      (is (= "<p>A</p>" (:reference-answer result)) "the HTML is kept for CardHtml")
      (is (empty? @reviews)))))

(deftest a-session-refusal-returns-an-error-without-a-retry-hint
  (with-seams [_reviews]
    (let [res (grade/grade-question 50 7 "cocci")]
      (is (= :insufficient-credits (:error-type res)))
      (is (nil? (:reference-answer res)) "only the Review flow reveals the reference")
      (is (re-find #"needs credits" (:error res)))
      (is (not (re-find #"(?i)try Submit again" (:error res)))
        "Submit cannot succeed until the balance is topped up"))))

(deftest a-session-grade-failure-says-the-answer-was-kept
  (with-seams [_reviews]
    (with-redefs [db/record-kg-answer! (constantly 9)]
      (let [res (grade/grade-answer! 50 3 7 0 "cocci")]
        (is (false? (:success res)))
        (is (re-find #"Your answer was kept" (:error res)))))))

(deftest a-review-failure-never-claims-the-answer-was-kept
  (with-seams [_reviews]
    (with-redefs [llm/resolve-model+gate! (fn [& _] (throw (ex-info "Provider down" {})))]
      (let [{:keys [result]} (grade/review-question! 50 7 "cocci" 1200)]
        (is (nil? (:error-type result)))
        (is (nil? (:reference-answer result)) "Submit may work again, so no reveal")
        (is (re-find #"Provider down" (:error result)))
        (is (not (re-find #"(?i)kept" (:error result)))
          "Review saves nothing, so it must not say the answer was kept")))))

(deftest a-schedule-write-failure-is-returned-with-the-grade
  (with-seams [_reviews]
    (with-redefs [db/apply-fsrs-review! (fn [& _] (throw (ex-info "db down" {})))]
      (let [out (grade/rate-question! 50 7 3 100 "x")]
        (is (true? (get-in out [:result :success])))
        (is (nil? (:schedule out)))
        (is (re-find #"schedule could not be saved" (:schedule-error out)))))))

(deftest a-self-rated-question-schedules-with-its-typed-answer
  (with-seams [reviews]
    (let [{:keys [result schedule]} (grade/rate-question! 50 7 2 3400 "cocci")]
      (is (= {:success true :rating 2} result))
      (is (= {:due-today? false} schedule))
      (is (= [[50 [:question 7] 2 {:grade-source "self" :duration-ms 3400
                                   :user-answer "cocci"}]]
            @reviews)))))

(deftest a-self-rated-card-records-an-answer-only-when-one-was-typed
  (with-seams [reviews]
    (grade/rate-card! 50 11 0 3 800)
    (grade/rate-card! 50 11 0 1 900 "typed")
    (is (= [[50 [:card 11 0] 3 {:grade-source "self" :duration-ms 800}]
            [50 [:card 11 0] 1 {:grade-source "self" :duration-ms 900
                                :user-answer "typed"}]]
          @reviews))))

(deftest a-rating-outside-one-to-four-is-refused
  (with-seams [reviews]
    (doseq [bad [0 5 nil "3"]]
      (is (false? (:success (:result (grade/rate-question! 50 7 bad 100 "x"))))))
    (is (empty? @reviews))))

(deftest the-card-arm-self-rates-when-credits-refuse-ai-grading
  (testing "setting on, credits present: AI grades"
    (with-redefs [settings/card-quiz-llm-grading? (constantly true)
                  credits/check-cost-billed-balance! (constantly {:ok true})]
      (is (= {:llm-grading? true :credits-out? false} (grade/card-grading-arm 50)))))
  (testing "setting on, credits empty: self-rate, and say why"
    (with-redefs [settings/card-quiz-llm-grading? (constantly true)
                  credits/check-cost-billed-balance! (constantly {:ok false :error "Out"})]
      (is (= {:llm-grading? false :credits-out? true} (grade/card-grading-arm 50)))))
  (testing "setting off: self-rate, and the credits are not the reason"
    (with-redefs [settings/card-quiz-llm-grading? (constantly false)
                  credits/check-cost-billed-balance!
                  (fn [_] (throw (ex-info "the gate must not be read" {})))]
      (is (= {:llm-grading? false :credits-out? false} (grade/card-grading-arm 50))))))
