(ns freememo.occlusion-svg-test
  "Mask-svg generation, labels included.

   Needs NO database: freememo.occlusion-svg requires only clojure.string, so
   this namespace runs without PostgreSQL up."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [freememo.occlusion-svg :as osvg]))

(def ^:private empty-labels-group
  "The Labels group exactly as every pre-label push emitted it. A group with no
   labels must still produce this, or existing cards re-push different bytes."
  " <g>\n  <title>Labels</title>\n </g>\n")

(def ^:private geo
  {:width 800 :height 600
   :rects [{:x 10 :y 20 :w 30 :h 40 :ordinal 1}
           {:x 50 :y 60 :w 70 :h 80 :ordinal 2}]})

(defn- labelled [& elements]
  (assoc geo :elements (vec elements)))

(def ^:private one-line
  {:type "text" :x 104 :y 39 :size 25 :text "Aortic arch"})

(deftest a-group-with-no-labels-is-unchanged
  (testing "every svg keeps the empty Labels group, byte for byte"
    (doseq [[what svg] {"hide-all Q" (osvg/question-mask-svg "K" geo "hide-all" 1)
                        "hide-all A" (osvg/answer-mask-svg "K" geo "hide-all" 1)
                        "hide-one Q" (osvg/question-mask-svg "K" geo "hide-one" 1)
                        "hide-one A" (osvg/answer-mask-svg "K" geo "hide-one" 1)
                        "original"   (osvg/original-mask-svg "K" geo)}]
      (is (str/includes? svg empty-labels-group) what)
      (is (not (str/includes? svg "<text")) what))))

(deftest a-label-reaches-every-svg
  (testing "a label is content the image lacks, so it is in Q, A and O alike"
    (doseq [[what svg] {"hide-all Q" (osvg/question-mask-svg "K" (labelled one-line) "hide-all" 1)
                        "hide-all A" (osvg/answer-mask-svg "K" (labelled one-line) "hide-all" 1)
                        "hide-one Q" (osvg/question-mask-svg "K" (labelled one-line) "hide-one" 1)
                        "hide-one A" (osvg/answer-mask-svg "K" (labelled one-line) "hide-one" 1)
                        "original"   (osvg/original-mask-svg "K" (labelled one-line))}]
      (is (str/includes? svg ">Aortic arch</tspan>") what))))

(deftest the-hide-one-answer-carries-labels-with-no-rects
  (let [svg (osvg/answer-mask-svg "K" (labelled one-line) "hide-one" 1)]
    (is (str/includes? svg ">Aortic arch</tspan>"))
    (is (not (str/includes? svg "<rect"))
      "hide-one reveals everything, so the answer holds no mask")))

(deftest labels-paint-before-masks
  (testing "paint order IS the occlusion mechanism: a mask must cover a label"
    (let [svg (osvg/question-mask-svg "K" (labelled one-line) "hide-all" 1)]
      (is (< (str/index-of svg "<title>Labels</title>")
            (str/index-of svg "<title>Masks</title>")))
      (is (< (str/index-of svg "<text") (str/index-of svg "<rect"))))))

(deftest label-text-is-xml-escaped
  (let [svg (osvg/original-mask-svg "K" (labelled (assoc one-line :text "a < b & c > d")))]
    (is (str/includes? svg ">a &lt; b &amp; c &gt; d</tspan>"))
    (is (not (str/includes? svg "a < b &")))))

(deftest each-line-becomes-a-tspan
  (testing "only the user's own newlines break a line — nothing wraps"
    (let [svg (osvg/original-mask-svg "K" (labelled (assoc one-line :text "one\ntwo\nthree")))]
      (is (str/includes? svg "<tspan x=\"104\">one</tspan>"))
      (is (str/includes? svg (str "<tspan x=\"104\" dy=\"" osvg/label-line-height "em\">two</tspan>")))
      (is (str/includes? svg (str "<tspan x=\"104\" dy=\"" osvg/label-line-height "em\">three</tspan>")))
      (is (= 1 (count (re-seq #"<text " svg))) "one text element, three tspans"))))

(deftest a-blank-line-holds-its-height
  (testing "dy needs a character to apply to, so an empty line emits a space"
    (let [svg (osvg/original-mask-svg "K" (labelled (assoc one-line :text "top\n\nbottom")))]
      (is (str/includes? svg (str "<tspan x=\"104\" dy=\"" osvg/label-line-height "em\"> </tspan>"))))))

(deftest the-anchor-makes-x-y-the-top-left-corner
  (testing "the editor and the card place a label from the same two numbers"
    (let [svg (osvg/original-mask-svg "K" (labelled one-line))]
      (is (str/includes? svg "x=\"104\" y=\"39\""))
      (is (str/includes? svg "text-anchor=\"start\""))
      (is (str/includes? svg (str "dominant-baseline=\"" osvg/label-baseline "\""))))))

(deftest a-label-carries-no-direction-attribute
  (testing "unstyled text is what keeps Konva and the svg agreeing on bidi"
    (let [svg (osvg/original-mask-svg "K" (labelled (assoc one-line :text "تجربة نص")))]
      (is (str/includes? svg ">تجربة نص</tspan>"))
      (is (not (str/includes? svg "direction=")))
      (is (not (str/includes? svg "unicode-bidi")))
      (is (not (str/includes? svg " dir="))))))

(deftest the-halo-scales-with-the-font-size
  (is (= 1 (osvg/halo-width 4)) "never below 1")
  (is (= 1 (osvg/halo-width 8)))
  (is (= 3 (osvg/halo-width 25)))
  (is (= 13 (osvg/halo-width 100)))
  (testing "the emitted stroke-width is that same number"
    (is (str/includes? (osvg/original-mask-svg "K" (labelled (assoc one-line :size 100)))
          "stroke-width=\"13\""))))

(deftest every-label-is-emitted-in-order
  (let [svg (osvg/original-mask-svg "K"
              (labelled (assoc one-line :text "first")
                (assoc one-line :y 200 :text "second")))]
    (is (< (str/index-of svg ">first<") (str/index-of svg ">second<")))
    (is (= 2 (count (re-seq #"<text " svg))))))
