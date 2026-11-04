(ns freememo.exam-minutes
  "The minutes slider that both exam generators show: a range from 5 to 90
   minutes, the counts the grid asks for at that value, the estimated cost,
   and a warning when the source is longer than an exam reads.
   The counts come from freememo.exam-grid, which the server reads too, so the
   line beside the slider is the grid the generator builds.
   See plans/blueprint-first-exams.md §3.3."
  (:require [clojure.string :as str]
            [hyperfiddle.electric3 :as e]
            [hyperfiddle.electric-dom3 :as dom]
            [freememo.exam-grid :as grid]))

(def slider-step 5)

(defn- cost-line [sheet estimate]
  (cond
    (nil? sheet) "Estimating the cost…"
    (:none sheet) "The cost of this model cannot be estimated."
    (some? (:credits estimate)) (str "Estimated cost: about " (:credits estimate) " credits, for two model calls.")
    :else (str "Estimated cost: about US$" (/ (Math/round (* 1000.0 (:usd estimate))) 1000.0)
            ", for two model calls.")))

(defn cost-text
  "The estimate line. Pre: sheet is assessment/estimated-cost's price sheet,
   {:none true} when the model has no price, or nil while it loads."
  [sheet kind t]
  (let [estimate (when (and sheet (not (:none sheet))) (grid/estimate sheet kind t))]
    (cost-line sheet estimate)))

(defn group-digits
  "A whole number with comma thousands separators: 1200000 → \"1,200,000\".
   Plain string work, so both peers give the same text."
  [n]
  (->> (reverse (str n)) (partition-all 3) (map #(apply str (reverse %)))
       reverse (str/join ",")))

(defn omission-text
  "The warning line. Pre: sheet is the price sheet, or nil while it loads.
   Post: a sentence when the exam would leave source text out, else \"\"."
  [{:keys [source-characters omitted-characters]}]
  (if (pos? (or omitted-characters 0))
    (str "This source has " (group-digits source-characters)
      " characters, and an exam reads at most "
      (group-digits (- source-characters omitted-characters))
      ". The last " (group-digits omitted-characters)
      " characters will not be assessed.")
    ""))

(defn read-minutes
  "Pre: text is the value of the range input. Post: an integer in range, or nil."
  [text]
  (let [n (parse-long (str text))]
    (when (grid/minutes-in-range? n) n)))

(e/defn MinutesSlider
  "A range input bound to !minutes, with the grid's counts and the estimate
   beside it. kind is :mixed or :essay. sheet is the price sheet, or nil.
   Post: returns no value."
  [kind !minutes sheet]
  (e/client
    (let [minutes (e/watch !minutes)]
      (dom/div
        (dom/props {:class "exam-minutes"})
        (dom/label
          (dom/props {:class "exam-minutes__control"})
          (dom/span (dom/props {:class "exam-minutes__label"}) (dom/text "Exam length"))
          (dom/input
            (dom/props {:type "range" :class "exam-minutes__range"
                        :min (str grid/min-minutes) :max (str grid/max-minutes)
                        :step (str slider-step) :aria-label "Exam length in minutes"})
            (set! (.-value dom/node) (str minutes))
            (dom/On "input" (fn [event]
                              (when-some [n (read-minutes (.. event -target -value))]
                                (reset! !minutes n))) nil)))
        (dom/span (dom/props {:class "exam-minutes__summary" :aria-live "polite"})
          (dom/text (grid/summary-text kind minutes)))
        (dom/span (dom/props {:class "exam-minutes__cost"})
          (dom/text (cost-text sheet kind minutes)))
        ;; Always mounted, and empty when the source fits (hidden by :empty).
        (dom/span (dom/props {:class "exam-minutes__omission" :role "note"})
          (dom/text (omission-text sheet))))
      ;; No value: inside a Form!'s Fields, a value would read as a field edit.
      (e/amb))))
