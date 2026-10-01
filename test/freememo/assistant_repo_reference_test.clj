(ns freememo.assistant-repo-reference-test
  "An @ reference names repo FILES, never a repository root. The root holds no
   text, so a reference to it gave the model nothing, with no error and no log
   line. A referenced file reads its source from `topic_code`, as the current
   topic does. Runs with the database down: every read is stubbed."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [freememo.assistant :as assistant]
            [freememo.db :as db]))

(def ^:private reference-context-packet #'assistant/reference-context-packet)

(def ^:private spec-md
  (str "# IR spec\n\n## 6. Type system\n\n### 6.1 Axes\n\n"
    "Every slot has a signature.\n\n"
    "```clojure\n(defn f [x]\n  (inc x))\n```\n"))

(def ^:private topics
  {1 {:topics/id 1 :topics/kind "repo" :topics/title "adham/grid"
      :topics/content nil}
   42 {:topics/id 42 :topics/kind "code" :topics/title "ir-spec.md"
       :topics/repo_path "docs/ir-spec.md" :topics/page_number 37
       :topics/content ""}
   43 {:topics/id 43 :topics/kind "code" :topics/title "a.clj"
       :topics/repo_path "src/a.clj" :topics/page_number 4
       :topics/content "<p>My note on g.</p>"}
   7 {:topics/id 7 :topics/kind "web" :topics/title "Typed IRs"
      :topics/page_number 2 :topics/content "<p>A web article.</p>"}})

(def ^:private code
  {42 {:source spec-md :lang "markdown"}
   43 {:source "(defn g [x]\n  (< x 1))" :lang "clojure"}})

(defn- packet [ref-ids first-index]
  (with-redefs [db/get-topic-for-user (fn [_user-id id] (get topics id))
                db/get-topic-code (fn [_user-id id] (get code id))]
    (reference-context-packet 5 ref-ids first-index)))

(deftest a-referenced-markdown-file-reads-its-rendered-source
  (let [{:keys [messages evidence next-index]} (packet [42] 4)
        text (:content (first messages))]
    (is (= 1 (count messages)) "empty notes add no second source")
    (is (str/includes? text "Every slot has a signature."))
    (is (str/includes? text "(defn f [x]\n  (inc x))") "a fence keeps its indentation")
    (is (not (str/includes? text "```")) "the model reads rendered text, not fences")
    (is (= "E4" (:citation-key (first evidence))) "keys run on from the current topic")
    (is (= (+ 4 (count evidence)) next-index))
    (let [block (some #(when (str/includes? (:quoted-text %) "Every slot") %) evidence)]
      (is (str/starts-with? (:display-label block) "6.1 Axes · excerpt "))
      (is (= "docs/ir-spec.md" (:document-title block)))
      (is (nil? (:page-number block)) "a tree ordinal is not a page")
      (is (= 42 (:document-topic-id block)) "a citation opens the referenced file")
      (is (= 42 (:page-topic-id block))))))

(deftest a-referenced-file-gives-its-source-then-its-notes
  (let [{:keys [messages evidence]} (packet [43] 1)]
    (is (= 2 (count messages)))
    (is (= "(defn g [x]\n  (< x 1))" (:quoted-text (first evidence)))
      "no entity reaches the model")
    (is (= ["src/a.clj" "src/a.clj — learner's notes"] (map :document-title evidence)))
    (is (= ["E1" "E2"] (map :citation-key evidence)))))

(deftest a-referenced-document-reads-as-before
  (let [{:keys [evidence]} (packet [7] 1)]
    (is (= ["A web article."] (map :quoted-text evidence)))
    (is (= "Typed IRs" (:document-title (first evidence))))
    (is (= 2 (:page-number (first evidence))))
    (is (= 7 (:document-topic-id (first evidence))))))

(deftest a-reference-with-no-text-adds-nothing
  (testing "a repository root, a missing topic and an empty leaf give no source"
    (with-redefs [db/get-topic-for-user (fn [_ id] (get topics id))
                  db/get-topic-code (constantly nil)]
      (is (= {:messages [] :evidence [] :next-index 3}
            (reference-context-packet 5 [1 99 42] 3)))))
  (testing "keys run on across the references that do give text"
    (let [{:keys [evidence]} (packet [1 7 43] 1)]
      (is (= ["E1" "E2" "E3"] (map :citation-key evidence)))
      (is (= [7 43 43] (map :document-topic-id evidence))))))

(deftest a-long-referenced-file-gets-the-smaller-reference-budget
  (let [budget @#'assistant/max-reference-file-chars
        sent #(reduce + (map (comp count :snapshot-content) %))
        big {:topics/id 44 :topics/kind "code" :topics/title "big.clj"
             :topics/repo_path "src/big.clj" :topics/content nil}
        src (str/join "\n" (repeat 30000 "(defn f [x] (inc x))"))
        {:keys [messages evidence]}
        (with-redefs [db/get-topic-for-user (fn [_ id] (when (= id 44) big))
                      db/get-topic-code (fn [_ id] (when (= id 44)
                                                     {:source src :lang "clojure"}))]
          (reference-context-packet 5 [44] 1))]
    (is (< budget @#'assistant/max-repo-file-chars)
      "a reference shares the run with the current topic and other references")
    (is (<= (sent evidence) budget))
    (is (> (sent evidence) (- budget 1000)) "the budget is used, not wasted")
    (is (str/includes? (:content (first messages)) "Note: src/big.clj is too long"))))

(defn- picker [query]
  (let [!file-calls (atom [])]
    (with-redefs [db/get-root-topics
                  (fn [_]
                    [{:topics/id 9 :topics/kind "pdf" :topics/title "Open document"}
                     {:topics/id 1 :topics/kind "repo" :topics/title "adham/grid"}
                     {:topics/id 11 :topics/kind "score" :topics/title "Grid sonata"}
                     {:topics/id 7 :topics/kind "web" :topics/title "Grid layout notes"}])
                  db/get-referenceable-repo-files
                  (fn [user-id needle limit]
                    (swap! !file-calls conj [user-id needle limit])
                    [{:id 42 :label "adham/grid/docs/ir-spec.md"}])]
      {:docs (assistant/referenceable-docs 5 9 query)
       :file-calls @!file-calls})))

(deftest the-picker-offers-repo-files-and-no-root-without-text
  (let [{:keys [docs file-calls]} (picker "GRID")]
    (is (= [{:id 7 :title "Grid layout notes"}
            {:id 42 :title "adham/grid/docs/ir-spec.md"}]
          docs)
      (str "documents first, then files. The repository root and the score both "
        "match the query, and neither is offered."))
    (is (= [[5 "grid" 50]] file-calls) "the needle reaches the query in lower case")))

(deftest a-blank-query-lists-documents-only
  (doseq [query ["" nil]]
    (let [{:keys [docs file-calls]} (picker query)]
      (is (= [{:id 7 :title "Grid layout notes"}] docs))
      (is (empty? file-calls) "the closed menu runs no repository walk"))))
