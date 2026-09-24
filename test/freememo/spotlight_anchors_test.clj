(ns freememo.spotlight-anchors-test
  "Every spotlight anchor has a control that carries it
   (plans/getting-started-checklist.md 6.2). A value with no site would give a
   \"Show me\" that waits five seconds and then points at nothing.

   The sites write the literal value as `:data-tour \"…\"`, or as `:tour \"…\"`
   on a nav-rail item, so a text search over `src/` finds each one."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [freememo.spotlight-anchors :as catalogue]))

(defn- source-text []
  (->> (file-seq (io/file "src"))
    (filter #(re-find #"\.clj[cs]?$" (.getName %)))
    (map slurp)
    (apply str)))

(deftest every-anchor-has-a-site
  (let [src (source-text)]
    (doseq [[k value] catalogue/anchors]
      (is (re-find (re-pattern (str ":(data-)?tour \"" value "\"")) src)
          (str k " has no :data-tour \"" value "\" in src/")))))
