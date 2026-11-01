(ns freememo.card-export-test
  "plans/card-export-csv-tsv.md, sections 2 to 4. Needs no database."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [freememo.card-export :as export]
            [freememo.card-export-http :as http]
            [freememo.overlapping :as overlapping])
  (:import [java.io StringWriter]))

(defn- preset [id & [images?]] (export/resolve-preset id images?))

(defn- card [kind & {:as fields}]
  (merge {:flashcards/kind kind :root_title "Biology"}
    (update-keys fields #(keyword "flashcards" (name %)))))

(defn- export-string [p cards & [names]]
  (let [w (StringWriter.)]
    (export/write-cards! w p cards names)
    (str w)))

;; ── 2. Kind expansion ──────────────────────────────────────────────

(deftest basic-is-one-pair
  (is (= [["Q" "A"]] (export/card-sides (card "basic" :question "Q" :answer "A") identity))))

(deftest cloze-is-one-pair-per-deletion-number
  (let [sides (export/card-sides
                (card "cloze" :cloze "{{c1::a}} and {{c1::b}} then {{c2::c::hint}}")
                identity)]
    (is (= 2 (count sides)))
    (is (= ["[...] and [...] then c" "a and b then c"] (first sides)))
    (testing "a hint shows on the front as [hint]"
      (is (= "a and b then [hint]" (first (second sides)))))))

(deftest cloze-tex-braces-do-not-end-a-deletion
  (let [sides (export/card-sides (card "cloze" :cloze "{{c1::\\(x^{a^{b}}\\)}}") identity)]
    (is (= [["[...]" "\\(x^{a^{b}}\\)"]] sides))))

(deftest mnemosyne-back-bolds-the-revealed-deletion
  (let [out (export-string (preset "mnemosyne") [(card "cloze" :cloze "{{c1::a}} b")])]
    (is (= "[...] b\t<b>a</b> b\n" out)))
  (testing "text presets add no mark"
    (is (= "[...] b,a b\r\n"
          (export-string (preset "noji") [(card "cloze" :cloze "{{c1::a}} b")])))))

(deftest overlapping-is-one-pair-per-item-plus-reveal-all
  (let [items ["one" "two" "three" "four"]
        ol {:question "Steps" :items items :settings {:reveal-all? true}
            :fields (overlapping/expand items {:reveal-all? true})}
        sides (export/card-sides (card "overlapping" :overlapping ol) identity)]
    (is (= 5 (count sides)))
    (is (every? #(str/starts-with? (first %) "Steps") sides))
    (testing "the reveal-all card hides every item"
      (is (= 4 (count (re-seq #"\[\.\.\.\]" (first (peek sides))))))))
  (testing "no reveal-all row when the settings turn it off"
    (let [items ["one" "two"]
          ol {:items items :fields (overlapping/expand items {:reveal-all? false})}]
      (is (= 2 (count (export/card-sides (card "overlapping" :overlapping ol) identity)))))))

(deftest occlusion-and-score-are-skipped-and-counted
  (let [summary (export/summarize (preset "noji")
                  [(card "basic" :question "Q" :answer "A")
                   (card "occlusion") (card "occlusion") (card "score")])]
    (is (= {:cards 1 :rows 1 :dropped 0 :skipped {"occlusion" 2 "score" 1}} summary))))

(deftest an-image-only-front-is-dropped-in-a-text-preset
  (let [c (card "basic" :question "<img src=\"/api/media/4\">" :answer "A")]
    (is (= {:cards 1 :rows 0 :dropped 1 :skipped {}}
          (export/summarize (preset "noji") [c])))
    (testing "the zip preset keeps it, because it carries the image"
      (is (= 1 (:rows (export/write-cards! (StringWriter.) (preset "mnemosyne" true) [c]
                         {4 "4.png"})))))))

;; ── 3. Presets ─────────────────────────────────────────────────────

(deftest noji-writes-math-as-double-dollars-and-plain-text
  (is (= "\"$$x<1$$ and a, b\",\"line one\nline two\"\r\n"
        (export-string (preset "noji")
          [(card "basic" :question "<p>\\(x&lt;1\\) and <b>a</b>, b</p>"
             :answer "<p>line one</p><p>line two</p>")]))))

(deftest html->text-gives-one-line-per-block-however-deep
  (testing "a block closing inside a block is one line break, not two"
    (is (= "a\nb\nc"
          (export/html->text "<div><p>a</p></div><div>b</div><div><p>c</p></div>"))))
  (testing "a <br> after a block keeps its own break"
    (is (= "Q\n\na" (export/html->text "<p>Q</p><br><div>a</div>"))))
  (testing "overlapping items are spaced alike, revealed or hidden"
    (let [items ["<p>one</p>" "<p>two</p>" "<p>three</p>"]
          ol {:question "<p>Steps</p>" :items items
              :fields (overlapping/expand items {:reveal-all? false})}
          [[_ back] & _] (export/card-sides (card "overlapping" :overlapping ol) identity)]
      (is (= "Steps\n\none\n...\n..." (export/html->text back))))))

(deftest mnemosyne-writes-math-in-its-own-tags-with-decoded-tex
  (is (= "<p><$>x<1</$></p>\tA\n"
        (export-string (preset "mnemosyne")
          [(card "basic" :question "<p>\\(x&lt;1\\)</p>" :answer "A")]))))

(deftest mnemosyne-images
  (let [c (card "basic" :question "Q <img src=\"/api/media/7\"> <img src=\"https://x/y.png\">"
            :answer "A")]
    (testing "the text preset removes every image"
      (is (= "Q\tA\n" (export-string (preset "mnemosyne") [c]))))
    (testing "the zip preset points app media at media/ and drops the rest"
      (is (= "Q <img src=\"media/7.png\">\tA\n"
            (export-string (preset "mnemosyne" true) [c] {7 "7.png"}))))
    (is (= [7] (export/cards-media-ids [c])))))

(deftest plain-presets-have-a-header-and-document-and-kind
  (is (= "Front,Back,Document,Kind\r\nQ,\\(x\\),Biology,basic\r\n"
        (export-string (preset "plain-csv")
          [(card "basic" :question "Q" :answer "\\(x\\)")])))
  (is (= "Front\tBack\tDocument\tKind\nQ a\tA\tBiology\tbasic\n"
        (export-string (preset "plain-tsv")
          [(card "basic" :question "Q<br>a" :answer "A")]))))

(deftest images-only-on-mnemosyne
  (is (true? (:images? (preset "mnemosyne" true))))
  (is (false? (:images? (preset "noji" true)))))

;; ── 4. Encoders ────────────────────────────────────────────────────

(defn- parse-csv
  "A strict RFC 4180 reader, enough for a round trip."
  [s]
  (loop [i 0 field "" row [] rows [] quoted? false]
    (if (>= i (count s))
      (cond-> rows (or (seq row) (seq field)) (conj (conj row field)))
      (let [c (.charAt ^String s i)]
        (cond
          (and quoted? (= c \") (< (inc i) (count s)) (= \" (.charAt ^String s (inc i))))
          (recur (+ i 2) (str field \") row rows true)
          (= c \") (recur (inc i) field row rows (not quoted?))
          (and (not quoted?) (= c \,)) (recur (inc i) "" (conj row field) rows false)
          (and (not quoted?) (= c \return)) (recur (inc i) field row rows false)
          (and (not quoted?) (= c \newline)) (recur (inc i) "" [] (conj rows (conj row field)) false)
          :else (recur (inc i) (str field c) row rows quoted?))))))

(deftest csv-round-trips
  (let [cells ["a,b" "say \"hi\"" "two\nlines" "plain"]]
    (is (= [cells] (parse-csv (export/encode-line (preset "plain-csv") cells))))))

(deftest noji-has-no-header
  (is (str/starts-with? (export-string (preset "noji") [(card "basic" :question "Q" :answer "A")]) "Q,")))

(def ^:private hostile-fields
  "Field html that each break one Mnemosyne rule if written as it stands."
  ["" " " "\t" "a\tb" "a\nb" "a\r\nb" "&#9;" "&#x9;" "&#10;" "&#13;" "&#xD;"
   "x&#9;y" "\"quoted\"" "<p></p>" "<br>" "&nbsp;" "trailing \t" " "])

(defn- mnemosyne-split
  "What `tsv.py` does to one line: rstrip, decode numeric references, split."
  [line]
  (-> (str/trimr line)
    (str/replace #"&#(?:[xX]([0-9a-fA-F]+)|([0-9]+));"
      (fn [[_ hex dec]] (str (char (if hex (Long/parseLong hex 16) (Long/parseLong dec))))))
    (str/split #"\t" -1)))

(deftest mnemosyne-tsv-rules-hold-for-hostile-fields
  (doseq [front (cons "F" hostile-fields)
          back hostile-fields
          :let [c (card "basic" :question front :answer back)
                out (export-string (preset "mnemosyne") [c])]]
    (testing (pr-str [front back])
      (is (not (str/starts-with? out "﻿")) "no BOM")
      (doseq [line (remove str/blank? (str/split-lines out))
              :let [fields (mnemosyne-split line)]]
        (is (= 2 (count fields)) "exactly two fields")
        (is (not (str/blank? (first fields))) "a non-empty front")
        (is (not (str/blank? (second fields))) "a back that survives rstrip"))
      (is (= (count (re-seq #"\t" out)) (count (re-seq #"\n" out)))
        "one tab and one line break per record"))))

(deftest mnemosyne-keeps-quotes
  (is (= "\"q\"\tA\n" (export-string (preset "mnemosyne") [(card "basic" :question "\"q\"" :answer "A")]))))

;; ── 5. Route helpers ───────────────────────────────────────────────

(deftest download-filename-is-safe
  (is (= "Cell-Biology-noji.csv" (export/download-filename "Cell / \"Biology\"" (preset "noji"))))
  (is (= "freememo-cards-mnemosyne.zip" (export/download-filename "خلية" (preset "mnemosyne" true)))))

(deftest parse-request-rejects-bad-input
  (is (= "Unknown preset" (:error (http/parse-request :get {"scope" "all" "preset" "x"}))))
  (is (= "The filter scope needs POST"
        (:error (http/parse-request :get {"scope" "filter" "preset" "noji" "opts" "{}"}))))
  (is (= "Bad filter options"
        (:error (http/parse-request :post {"scope" "filter" "preset" "noji" "opts" "#=(eval 1)"}))))
  (is (= {:kinds #{"cloze"} :sort-col :added}
        (:opts (http/parse-request :post {"scope" "filter" "preset" "noji"
                                          "opts" "{:kinds #{\"cloze\"} :sort-col :added :evil 1}"})))))
