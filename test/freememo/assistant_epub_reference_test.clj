(ns freememo.assistant-epub-reference-test
  "An @ reference to an EPUB sends a window of its chapters. The EPUB root holds
   no text, so a reference to it gave the model nothing, with no error and no
   log line. Runs with the database down: every read is stubbed."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [freememo.assistant :as assistant]
            [freememo.db :as db]
            [freememo.settings :as settings]))

(def ^:private reference-context-packet #'assistant/reference-context-packet)
(def ^:private max-window-chars @#'assistant/max-window-chars)

(def ^:private book
  {:topics/id 50 :topics/kind "epub" :topics/title "Book" :topics/content nil})

(defn- chapter [n title content]
  {:topics/id (+ 500 n) :topics/page_number n :topics/title title
   :topics/content content})

(defn- packet
  "Reference `book` with these chapters stored, the first unfinished chapter at
   `reading` and the learner's window setting at `window`.
   Post: [packet the chapter range the window asked for]."
  [chapters reading window]
  (let [!asked (atom nil)]
    (with-redefs [db/get-topic-for-user (fn [_ id] (when (= id 50) book))
                  db/get-epub-reading-chapter (fn [id] (when (= id 50) reading))
                  settings/get-assistant-pdf-window (constantly window)
                  db/get-epub-chapters
                  (fn [epub-id start end]
                    (reset! !asked [epub-id start end])
                    (filterv #(<= start (:topics/page_number %) end) chapters))]
      [(reference-context-packet 5 [50] 4) @!asked])))

(def ^:private five-chapters
  (mapv #(chapter % (str "Chapter " %) (str "<p>Text of chapter " % ".</p>"))
    (range 1 6)))

(deftest a-referenced-epub-reads-the-chapters-around-the-first-unfinished-one
  (let [[{:keys [messages evidence next-index]} asked] (packet five-chapters 3 1)]
    (is (= [50 2 4] asked) "the learner's window, counted in chapters")
    (is (= 1 (count messages)) "one source turn for the whole window")
    (is (= ["Text of chapter 2." "Text of chapter 3." "Text of chapter 4."]
          (map :quoted-text evidence))
      "chapters in reading order")
    (is (= [502 503 504] (map :document-topic-id evidence))
      "a citation opens the chapter, not the book")
    (is (= ["Book — Chapter 2" "Book — Chapter 3" "Book — Chapter 4"]
          (map :document-title evidence)))
    (is (every? #(nil? (:page-number %)) evidence) "a chapter index is not a page")
    (is (= ["E4" "E5" "E6"] (map :citation-key evidence))
      "keys run on from the current topic")
    (is (= 7 next-index))
    (is (not (str/includes? (:content (first messages)) ", page "))
      "no page label reaches the model")))

(deftest a-finished-book-centers-on-chapter-one
  (let [[{:keys [evidence]} asked] (packet five-chapters nil 20)]
    (is (= [50 1 21] asked))
    (is (= 5 (count evidence)) "every short chapter fits in the budget")))

(deftest a-chapter-past-the-budget-is-cut-and-says-so
  (let [paragraph (str "<p>" (str/join (repeat 300 "x")) "</p>")
        long-chapter (chapter 1 "Everything" (str/join (repeat 200 paragraph)))
        [{:keys [messages evidence]} _]
        (packet [long-chapter (chapter 2 "Next" "<p>Next chapter.</p>")] 1 20)
        sent (reduce + (map (comp count :snapshot-content) evidence))]
    (testing "only the leading blocks that fit go"
      (is (< 100 (count evidence) 200))
      (is (<= sent max-window-chars))
      (is (every? #(= 501 (:document-topic-id %)) evidence)
        "the neighbor does not fit beside the cut chapter"))
    (testing "the model learns that the text goes on"
      (is (str/includes? (:content (first messages))
            "Note: Book — Everything is too long to read in one turn."))
      (is (str/includes? (:content (first messages)) (str "its first " sent " of "))))))

(deftest an-epub-with-no-chapter-text-gives-nothing
  (let [[result _] (packet [(chapter 1 "Cover" nil) (chapter 2 "Blank" " ")] 1 20)]
    (is (= {:messages [] :evidence [] :next-index 4} result))))
