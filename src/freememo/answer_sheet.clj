(ns freememo.answer-sheet
  "The printable answer sheet of a comprehension cycle, as a PDF made by Typst.

   `freememo.typst` compiles it. A missing binary or font is a typed failure, so
   the print endpoint can fall back to its HTML sheet."
  (:require [freememo.typst :as typst]
            [taoensso.telemere :as tel]))

(def ^:private template "typst/answer-sheet.typ")

(defn- rtl-text?
  "Same ranges as `freememo.card-components/rtl-text?`, which requires Electric."
  [text]
  (boolean (and text (re-find #"[؀-ۿݐ-ݿࢠ-ࣿﭐ-﷿ﹰ-﻿֐-׿]" text))))

(defn sheet-data
  "What the sheet shows for one cycle detail.
   Post: only a title, the cycle id, and per question its number, format, points,
   prompt, options and direction. No answer key, rubric or evidence leaves here."
  [detail]
  {:title (str (or (:title detail) "Study cycle"))
   :cycle (:id detail)
   :questions (vec (map-indexed
                     (fn [i question]
                       (let [prompt (str (:prompt question))]
                         {:number (inc i)
                          :format (str (or (:format question) "long-essay"))
                          :points (or (:points question) 0)
                          :prompt prompt
                          :rtl (rtl-text? prompt)
                          :options (vec (for [option (:options question)]
                                          (if (map? option)
                                            {:id (str (:id option)) :text (str (:text option))}
                                            {:id "" :text (str option)})))}))
                     (get-in detail [:form :questions])))})

(defn render
  "The answer sheet of a cycle detail as PDF bytes.
   Post: {:ok true :pdf bytes}, or {:ok false :reason kw :error S} when Typst or
   the font is missing or the compile fails. A failure is logged here."
  [detail]
  (let [result (typst/render-pdf template (sheet-data detail))]
    (when-not (:ok result)
      (tel/log! {:level :warn :id ::render-failed
                 :data {:cycle-id (:id detail) :reason (:reason result) :error (:error result)}}
        "Answer sheet PDF failed; serving the HTML sheet"))
    result))
