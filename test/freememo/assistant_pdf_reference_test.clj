(ns freememo.assistant-pdf-reference-test
  "An @ reference to a PDF sends a window of its pages. The PDF root holds no
   text, so a reference to it gave the model nothing, with no error and no log
   line. Runs with the database down: every read is stubbed."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [freememo.assistant :as assistant]
            [freememo.db :as db]
            [freememo.settings :as settings]))

(def ^:private reference-context-packet #'assistant/reference-context-packet)
(def ^:private max-window-chars @#'assistant/max-window-chars)

(def ^:private pdf
  {:topics/id 30 :topics/kind "pdf" :topics/title "Paper" :topics/content nil})

(defn- page [n content]
  {:topics/id (+ 300 n) :topics/page_number n :topics/content content})

(defn- packet
  "Reference `pdf` with these pages stored, the learner's last-read page at
   `last-page` and their window setting at `window`.
   Post: [packet the page range the window asked for]."
  [pages last-page window]
  (let [!asked (atom nil)]
    (with-redefs [db/get-topic-for-user (fn [_ id] (when (= id 30) pdf))
                  settings/get-last-page (fn [_ id] (when (= id 30) last-page))
                  settings/get-assistant-pdf-window (constantly window)
                  db/get-context-pages
                  (fn [parent-id start end]
                    (reset! !asked [parent-id start end])
                    (filterv #(<= start (:topics/page_number %) end) pages))]
      [(reference-context-packet 5 [30] 4) @!asked])))

(deftest a-referenced-pdf-reads-the-window-around-the-last-read-page
  (let [pages (mapv #(page % (str "<p>Text of page " % ".</p>")) (range 1 21))
        [{:keys [messages evidence next-index]} asked] (packet pages 10 2)]
    (is (= [30 8 12] asked) "the learner's own window, around the page they last read")
    (is (= 1 (count messages)) "one source turn for the whole window")
    (is (= [8 9 10 11 12] (map :page-number evidence)) "pages in reading order")
    (is (= (map #(+ 300 %) [8 9 10 11 12]) (map :page-topic-id evidence)))
    (is (every? #(= 30 (:document-topic-id %)) evidence) "a citation opens the PDF")
    (is (every? #(= "Paper" (:document-title %)) evidence))
    (is (= ["E4" "E5" "E6" "E7" "E8"] (map :citation-key evidence))
      "keys run on from the current topic")
    (is (= 9 next-index))
    (is (str/includes? (:content (first messages)) "Paper, page 10 — Page 10 · excerpt 1"))))

(deftest a-pdf-never-opened-starts-at-page-one
  (let [[_ asked] (packet [(page 1 "<p>Abstract.</p>")] 1 20)]
    (is (= [30 1 21] asked))))

(deftest the-window-keeps-the-pages-nearest-the-last-read-page
  (testing "a far page is dropped when the window passes the character budget"
    (let [long-text (fn [n] (str "<p>" (str/join (repeat (quot max-window-chars 4) (str n))) "</p>"))
          pages [(page 4 (long-text 4)) (page 5 (long-text 5))
                 (page 6 (long-text 6)) (page 9 (long-text 9))]
          [{:keys [evidence]} _] (packet pages 5 4)]
      (is (= #{4 5 6} (set (map :page-number evidence)))))))

(deftest a-pdf-with-no-scanned-page-gives-nothing
  (let [[result _] (packet [(page 1 nil) (page 2 "   ")] 1 20)]
    (is (= {:messages [] :evidence [] :next-index 4} result))))
