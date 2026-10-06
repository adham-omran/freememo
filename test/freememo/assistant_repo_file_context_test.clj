(ns freememo.assistant-repo-file-context-test
  "The assistant reads a repo file's source, which lives in `topic_code` and
   not in `topics.content`. Runs with the database down: the two reads are
   stubbed."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [freememo.assistant :as assistant]
            [freememo.db :as db]))

(def ^:private topic-context-packet #'assistant/topic-context-packet)

(def ^:private spec-md
  (str "# IR spec\n\nIntro.\n\n"
    "## 6. Type system\n\n### 6.1 Axes\n\n"
    "Every slot has a signature.\n\n"
    "```clojure\n(defn f [x]\n  (inc x))\n```\n"))

(defn- packet [topic code]
  (with-redefs [db/get-topic (fn [id] (when (= id (:topics/id topic)) topic))
                db/get-topic-code (fn [_user-id id]
                                    (when (= id (:topics/id topic)) code))]
    (topic-context-packet 7 {:root-id 1 :page-topic-id (:topics/id topic)}
      "grid" 1)))

(defn- repo-file [content]
  {:topics/id 42 :topics/kind "code" :topics/repo_path "grid/docs/ir-spec.md"
   :topics/page_number 37 :topics/content content})

(deftest a-markdown-repo-file-is-read-as-rendered-sections
  (let [{:keys [messages evidence next-index]}
        (packet (repo-file "") {:source spec-md :lang "markdown"})
        text (:content (first messages))]
    (is (= 1 (count messages)) "empty notes add no second source")
    (is (not (str/includes? text "Note:")) "a file under the budget has no cut note")
    (is (str/includes? text "Every slot has a signature."))
    (is (str/includes? text "(defn f [x]\n  (inc x))") "a fence keeps its indentation")
    (is (not (str/includes? text "```")) "the model reads rendered text, not fences")
    (is (= (inc (count evidence)) next-index))
    (let [block (some #(when (str/includes? (:quoted-text %) "Every slot") %) evidence)]
      (is (str/starts-with? (:display-label block) "6.1 Axes, excerpt "))
      (is (some? (:anchor block)) "a citation can land on the heading")
      (is (= "grid/docs/ir-spec.md" (:document-title block)))
      (is (nil? (:page-number block)) "a tree ordinal is not a page")
      (is (= 1 (:document-topic-id block)) "evidence stays in the chat's document"))))

(deftest any-other-repo-file-is-one-preformatted-block
  (let [{:keys [messages evidence]}
        (packet (assoc (repo-file nil) :topics/repo_path "grid/src/a.clj")
          {:source "(ns a)\n\n(defn g [x]\n  (< x 1))" :lang "clojure"})]
    (is (= 1 (count evidence)))
    (is (= "(ns a)\n\n(defn g [x]\n  (< x 1))" (:quoted-text (first evidence)))
      "no entity reaches the model")
    (is (str/includes? (:content (first messages)) "grid/src/a.clj"))))

(deftest notes-follow-the-source-under-their-own-title
  (let [{:keys [messages evidence]}
        (packet (repo-file "<p>My note about axes.</p>")
          {:source spec-md :lang "markdown"})]
    (is (= 2 (count messages)))
    (is (str/includes? (:content (second messages)) "My note about axes."))
    (is (= "grid/docs/ir-spec.md — learner's notes" (:document-title (last evidence))))
    (is (= (map :citation-key evidence)
          (map #(str "E" %) (range 1 (inc (count evidence)))))
      "keys run on across the two sources")))

(deftest an-empty-leaf-gives-only-its-notes
  (let [{:keys [messages evidence]}
        (packet (repo-file "<p>Note.</p>") nil)]
    (is (= 1 (count messages)))
    (is (= ["Note."] (map :quoted-text evidence)))))

(def ^:private budget @#'assistant/max-repo-file-chars)

(defn- sent-chars [evidence]
  (reduce + (map (comp count :snapshot-content) evidence)))

(def ^:private paragraph (str/join " " (repeat 150 "word")))

(deftest a-long-markdown-file-is-cut-and-the-model-is-told
  (let [n 400
        md (str/join "\n\n" (for [i (range 1 (inc n))]
                              (str "## Section " i "\n\n" paragraph)))
        {:keys [messages evidence next-index]}
        (packet (repo-file "<p>My note.</p>") {:source md :lang "markdown"})
        source-turn (:content (first messages))]
    (is (> (* n (count paragraph)) budget) "the fixture is over the budget")
    (is (<= (sent-chars (butlast evidence)) budget))
    (is (str/includes? source-turn "Note: grid/docs/ir-spec.md is too long"))
    (is (str/includes? source-turn (str "its first " (sent-chars (butlast evidence)) " of ")))
    (testing "the note names sections that were not sent, and stops at a cap"
      (is (str/includes? source-turn "Sections not sent, wholly or in part: Section "))
      (is (re-find #"; and \d+ more\." source-turn))
      (is (not (str/includes? source-turn "Section 400;")) "the list is capped"))
    (testing "the notes are not cut and keep the next key"
      (is (= 2 (count messages)))
      (is (= "My note." (:snapshot-content (last evidence))))
      (is (= next-index (inc (count evidence)))))))

(deftest a-long-code-file-is-cut-with-no-section-list
  (let [src (str/join "\n" (repeat 30000 "(defn f [x] (inc x))"))
        {:keys [messages evidence]}
        (packet (assoc (repo-file nil) :topics/repo_path "grid/src/big.clj")
          {:source src :lang "clojure"})
        source-turn (:content (first messages))]
    (is (<= (sent-chars evidence) budget))
    (is (> (sent-chars evidence) (- budget 1000)) "the budget is used, not wasted")
    (is (str/includes? source-turn "Note: grid/src/big.clj is too long"))
    (is (not (str/includes? source-turn "Sections not sent")))))

(deftest the-first-block-is-kept-whatever-the-budget
  (let [within-budget #'assistant/within-budget]
    (is (= [[{:text "abc"}] [{:text "de"}]]
          (within-budget [{:text "abc"} {:text "de"}] 1)))
    (is (= [[] []] (within-budget nil 10)))
    (is (= [[{:text "ab"} {:text "cd"}] []]
          (within-budget [{:text "ab"} {:text "cd"}] 4)))))

(deftest a-topic-that-is-not-a-repo-file-is-unchanged
  (testing "kg-code writes kind code with no repo_path and its source in content"
    (let [{:keys [evidence]}
          (packet {:topics/id 5 :topics/kind "code" :topics/page_number 3
                   :topics/content "<p>Body.</p>"}
            {:source "never read" :lang "clojure"})]
      (is (= ["Body."] (map :quoted-text evidence)))
      (is (= "grid" (:document-title (first evidence))))
      (is (= 3 (:page-number (first evidence)))))))
