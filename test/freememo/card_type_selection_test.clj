(ns freememo.card-type-selection-test
  "Exercise the card-type change handler with a controlled select value. No database is required."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [clojure.walk :as walk]))

(defn- read-forms []
  (with-open [reader (java.io.PushbackReader. (io/reader "src/freememo/card_modals.cljc"))]
    (binding [*read-eval* false
              *data-readers* (assoc *data-readers* 'js identity)]
      (loop [forms []]
        (let [form (read {:read-cond :allow :features #{:clj} :eof ::eof} reader)]
          (if (= ::eof form) forms (recur (conj forms form))))))))

(defn- kind-change-handler []
  (let [handlers (->> (tree-seq coll? seq (read-forms))
                     (filter #(and (seq? %) (= 'dom/On (first %)) (= "change" (second %))))
                     (filter #(some (fn [node]
                                      (and (seq? node) (= 'switch-add-card-kind! (first node))))
                                (tree-seq coll? seq %))))]
    (assert (= 1 (count handlers)) "Expected one card-type change handler")
    (nth (first handlers) 2)))

(deftest select-choice-survives-editor-flush
  (let [handler (walk/postwalk
                  (fn [node]
                    (cond
                      ;; The browser's current select value can change during a reactive flush.
                      (= '(.. event -target -value) node) '(deref event)
                      (and (seq? node) (= 'switch-add-card-kind! (first node)))
                      (list 'record-kind! (last node))
                      :else node))
                  (kind-change-handler))
        make-handler (eval (list 'fn '[flush! record-kind!] handler))
        select-value (atom "overlapping")
        chosen-kind (atom nil)
        on-change (make-handler #(reset! select-value "basic") #(reset! chosen-kind %))]
    (on-change select-value)
    (is (= "overlapping" @chosen-kind)
      "Read the user's selection before editor flush restores the previous DOM value")))
