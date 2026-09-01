(ns freememo.curriculum-page-test
  "Source contracts for Curriculum's Electric collection and Forms5 boundaries."
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]
   [clojure.walk :as walk]))

(defn- read-source-forms [path]
  (with-open [reader (java.io.PushbackReader. (io/reader path))]
    (loop [forms []]
      (let [form (read {:eof ::eof :read-cond :allow :features #{:clj}} reader)]
        (if (= ::eof form) forms (recur (conj forms form)))))))

(defn- forms-matching [predicate forms]
  (let [matches (atom [])]
    (doseq [form forms]
      (walk/prewalk
        (fn [node]
          (when (predicate node) (swap! matches conj node))
          node)
        form))
    @matches))

(defn- calls? [symbol form]
  (boolean
    (some #(and (seq? %) (= symbol (first %)))
      (tree-seq coll? seq form))))

(def source-forms
  (read-source-forms "src/freememo/curriculum_page.cljc"))

(deftest every-curriculum-collection-loop-has-a-differential-source
  (let [loops (forms-matching
                #(and (seq? %) (= 'e/for (first %)))
                source-forms)
        invalid-sources
        (->> loops
          (map #(-> % second second))
          (remove #{'commits})
          (remove #(calls? 'e/diff-by %)))]
    (is (empty? invalid-sources)
      (str "e/for requires e/diff-by for plain Curriculum collections: "
        (pr-str invalid-sources)))))

(deftest tracked-inputs-with-dom-siblings-use-e-amb
  (let [invalid-containers
        (forms-matching
          (fn [form]
            (when (and (seq? form)
                    (symbol? (first form))
                    (= "dom" (namespace (first form))))
              (let [children (vec (rest form))]
                (some (fn [[index child]]
                        (and (seq? child)
                          (= 'forms/Input! (first child))
                          (< index (dec (count children)))))
                  (map-indexed vector children)))))
          source-forms)]
    (testing "a DOM body's implicit do must not discard tracked input edits"
      (is (empty? invalid-containers)
        (str "Wrap tracked input siblings in e/amb: "
          (pr-str (map first invalid-containers)))))))

(deftest form-command-streams-render-inside-their-visual-containers
  (let [detached-streams
        (forms-matching
          (fn [form]
            (and (seq? form)
              (symbol? (first form))
              (= "dom" (namespace (first form)))
              (some #{'commits} (rest form))))
          source-forms)]
    (is (empty? detached-streams)
      (str "Create Forms5 inside its visual container, not before it: "
        (pr-str (map first detached-streams))))))
