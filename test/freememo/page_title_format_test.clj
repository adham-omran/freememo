(ns freememo.page-title-format-test
  "Tab title format and vocabulary (plans/reactive-page-title.md §1).

   Needs no database: freememo.page-title-format has no requires."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [freememo.page-title-format :as fmt]))

(deftest a-name-is-followed-by-the-brand
  (is (= "Thermodynamics, ch. 3 | FreeMemo" (fmt/page-title "Thermodynamics, ch. 3"))))

(deftest a-name-with-no-visible-character-reads-untitled
  (doseq [blank ["" " " "\t\n "]]
    (is (= "Untitled | FreeMemo" (fmt/page-title blank)) (pr-str blank))))

(deftest a-missing-item-reads-not-found
  (is (= "Not found | FreeMemo" (fmt/page-title nil))))

(deftest every-tab-has-its-label
  (is (= {:home "Home" :viewer "Viewer" :library "Library" :learn "Learn"
          :schedule "Schedule" :curriculum "Curriculum" :quiz "Quiz"
          :search "Search" :knowledge "Knowledge" :graph "Graph"
          :import "Import" :help "Help" :settings "Settings" :credits "Credits"}
        fmt/tab-labels)))

(deftest an-unknown-tab-reads-not-found
  (is (= "Not found | FreeMemo" (fmt/page-title (get fmt/tab-labels :foo)))))

(deftest a-self-titled-tab-has-a-label-for-its-list-view
  (is (every? fmt/tab-labels fmt/self-titled-tabs)))

(deftest every-tab-main-renders-has-a-label
  ;; The same branch shape playwright/support/counts-from-source.mjs counts as
  ;; Routes. A tab added there without a label reads "Not found" in its tab.
  (let [source (slurp (io/file "src/freememo/main.cljc"))
        tabs (set (map (comp keyword second)
                    (re-seq #"\(when \(= active-tab :([a-z-]+)\)" source)))]
    (is (seq tabs) "no (when (= active-tab :kw) …) branch found in main.cljc")
    (is (empty? (remove fmt/tab-labels tabs))
      (str "tabs with no label: " (vec (remove fmt/tab-labels tabs))))))

(deftest a-walkthrough-is-titled-by-its-name-then-its-repository
  (is (= "Split hold · owner/grid | FreeMemo"
        (fmt/page-title (fmt/walkthrough-name "Split hold" "owner/grid"))))
  (is (= "Not found | FreeMemo" (fmt/page-title (fmt/walkthrough-name nil nil)))))
