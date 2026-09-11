(ns freememo.curriculum-page-test
  "Source contracts for Curriculum UI collection and Forms5 boundaries."
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

(def page-source-forms
  (read-source-forms "src/freememo/curriculum_page.cljc"))

(def exam-source-forms
  (read-source-forms "src/freememo/curriculum_exam_page.cljc"))

(def schedule-source-forms
  (read-source-forms "src/freememo/schedule_page.cljc"))

;; These files render Curriculum collections through the same Electric rules.
;; Keep one contract over every UI that owns those collections.
(def source-forms
  (into [] cat [page-source-forms exam-source-forms schedule-source-forms]))

(defn- definition
  ([name] (definition source-forms name))
  ([forms name]
   (some #(when (and (seq? %)
                  (= 'e/defn (first %))
                  (= name (second %)))
            %)
     forms)))

(deftest every-curriculum-collection-loop-has-a-differential-source
  (let [loops (forms-matching
                #(and (seq? %) (= 'e/for (first %)))
                source-forms)
        invalid-sources
        (->> loops
          (map #(-> % second second))
          (remove #{'commits})
          ;; `dom/On-all` already yields a differential collection of
          ;; [token value] branches, so it needs no `e/diff-by`.
          (remove #(calls? 'dom/On-all %))
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

(deftest exam-generation-control-shows-its-lifecycle
  (let [trigger (definition exam-source-forms 'GenerateExamTrigger)
        control (definition exam-source-forms 'GenerateExamButton)
        trigger-nodes (tree-seq coll? seq trigger)
        control-nodes (tree-seq coll? seq control)]
    (testing "a model request cannot look like an ignored click"
      (is (some #(and (map? %)
                   (contains? % :disabled)
                   (contains? % :aria-busy))
            trigger-nodes)
        "the control must expose its pending state")
      (is (some #{"Generating exam…"} trigger-nodes)
        "the control must replace its idle label while the request runs"))
    (testing "a failed request is announced outside the trigger"
      (is (some #(and (map? %) (= "alert" (:role %))) control-nodes)
        "the control must render a visible error alert")
      (is (not-any? #(and (map? %) (= "alert" (:role %))) trigger-nodes)
        "the error must not become part of the button's accessible name"))))

(defn- e-fn? [x]
  (and (seq? x) (= 'e/fn (first x))))

(defn- local-definitions [forms]
  (into {}
    (keep (fn [form]
            (when (and (seq? form) (= 'e/defn (first form)))
              (when-some [params (first (filter vector? (drop 2 form)))]
                [(second form) params]))))
    forms))

(deftest an-electric-fn-argument-lands-on-a-capitalized-parameter
  ;; `lang3/electric-sym?` reads the first character of a call's head symbol.
  ;; A lowercase parameter therefore compiles to a plain CLJS call on the arity
  ;; map an `e/fn` value is, and the call throws `Invalid arity: 0` at run time
  ;; with no compile warning. MoveButton shipped that way.
  (let [params-by-name (local-definitions source-forms)
        bad (atom [])]
    (doseq [form source-forms
            node (tree-seq coll? seq form)]
      (when (and (seq? node) (symbol? (first node))
              (contains? params-by-name (first node)))
        (let [params (get params-by-name (first node))]
          (doseq [[index argument] (map-indexed vector (rest node))]
            (when (e-fn? argument)
              (let [parameter (get params index)]
                (when (and (symbol? parameter)
                        (Character/isLowerCase (.charAt (name parameter) 0)))
                  (swap! bad conj [(first node) parameter]))))))))
    (is (empty? @bad)
      (str "An e/fn argument needs a capitalized parameter: " (pr-str @bad)))))

(deftest curriculum-delete-dialog-states-the-recovery-boundary
  (let [source (slurp "src/freememo/curriculum_page.cljc")]
    (is (.contains source "stage-curriculum-for-deletion!")
      "the dialog must stage the root instead of physically deleting it")
    (is (.contains source "You can undo this for 12 hours.")
      "the dialog must state the recovery deadline")
    (is (.contains source "Imported documents stay.")
      "the dialog must state that imported sources remain")))
