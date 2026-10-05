(ns freememo.quote-anchor-locate-test
  "`quote-anchor/locate` over rendered Markdown (plans/anchored-questions.md
   §6.1.4)."
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.quote-anchor :as qa]
            [freememo.repo-markdown :as repo-md]))

(defn- render [md] (repo-md/render md "docs/spec.md" (constantly {}) nil))

(def doc
  (render (str "# Ports\n\n"
            "A port declares shape, degree, Type and timing.\n\n"
            "## Signatures\n\n"
            "Every slot has a signature along shape, degree, Type and timing.\n\n"
            "Callable slots also carry a *closure signature*.\n")))

(defn- block-text
  "The raw text of top-level block `i`, as a browser's TreeWalker reads it."
  [html i]
  (.wholeText (nth (.children (.body (org.jsoup.Jsoup/parseBodyFragment html))) i)))

(deftest locate-reports-block-offsets-a-browser-can-map
  (let [{[sb so] :start [eb eo] :end} (qa/locate doc "closure signature" nil)]
    (is (= sb eb))
    (is (= "closure signature" (subs (block-text doc sb) so eo))))
  (testing "a quote across an inline element boundary"
    (let [{[b s] :start [_ e] :end} (qa/locate doc "carry a closure" nil)]
      (is (= "carry a closure" (subs (block-text doc b) s e))))))

(deftest whitespace-and-curly-quotes-do-not-defeat-a-match
  (is (some? (qa/locate doc "shape,\n  degree, Type" nil))))

(deftest a-changed-passage-is-unmatched
  (is (nil? (qa/locate doc "closure type" nil)))
  (is (nil? (qa/locate doc "   " nil))))

(deftest the-heading-anchor-chooses-between-duplicates
  (let [first-hit (qa/locate doc "shape, degree, Type and timing" nil)
        under-signatures (qa/locate doc "shape, degree, Type and timing" "signatures")]
    (is (not= (:start first-hit) (:start under-signatures)))
    (is (< (first (:start first-hit)) (first (:start under-signatures))))
    (testing "an unknown anchor falls back to the first occurrence"
      (is (= first-hit (qa/locate doc "shape, degree, Type and timing" "nowhere"))))))
