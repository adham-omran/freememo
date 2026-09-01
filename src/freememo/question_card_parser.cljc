(ns freememo.question-card-parser
  "Parse selected plain-text lines into basic question-and-answer cards."
  (:require [clojure.string :as str]))

(defn- parse-question-line
  "Parse one nonblank source line.

   Pre : `line-number` is its one-based physical line number and `line` is text.
   Post: returns {:card {:question :answer}} when the line contains exactly one
         ASCII question mark with text on both sides. Otherwise returns
         {:invalid-line-number line-number}.
   Inv : the question keeps its boundary question mark."
  [line-number line]
  (let [trimmed (str/trim line)
        question-mark-count (count (filter #(= \? %) trimmed))]
    (if (not= 1 question-mark-count)
      {:invalid-line-number line-number}
      (let [question-mark-index (str/index-of trimmed "?")
            question-text (str/trim (subs trimmed 0 question-mark-index))
            answer (str/trim (subs trimmed (inc question-mark-index)))]
        (if (or (str/blank? question-text) (str/blank? answer))
          {:invalid-line-number line-number}
          {:card {:question (str question-text "?")
                  :answer answer}})))))

(defn parse-question-selection
  "Parse a selected plain-text block as an all-or-nothing card batch.

   Pre : `selected-text` is a string or nil.
   Post: returns {:cards [...], :invalid-line-numbers [...], :record-count n}.
         Blank physical lines do not count as records. If any record is invalid,
         :cards is empty and every invalid physical line number is reported.
   Inv : a caller can save :cards without repeating partial-batch validation."
  [selected-text]
  (let [numbered-lines (->> (str/split-lines (or selected-text ""))
                         (map-indexed (fn [index line] [(inc index) line]))
                         (remove (fn [[_ line]] (str/blank? line))))
        parsed-lines (mapv (fn [[line-number line]]
                             (parse-question-line line-number line))
                       numbered-lines)
        invalid-line-numbers (into [] (keep :invalid-line-number) parsed-lines)
        parsed-cards (into [] (keep :card) parsed-lines)]
    {:cards (if (seq invalid-line-numbers) [] parsed-cards)
     :invalid-line-numbers invalid-line-numbers
     :record-count (count numbered-lines)}))
