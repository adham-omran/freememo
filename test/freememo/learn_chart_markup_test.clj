(ns freememo.learn-chart-markup-test
  "The Learn charts are one markup string each, not one Electric frame tree per
   bar (plans/low-end-tablet-performance/issues/19-first-learn-visit.md)."
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [freememo.learn-page :as learn-page]))

(defn- n-of [re s] (count (re-seq re s)))

(def studied
  [{:date "2026-11-01" :label "M" :count 0}
   {:date "2026-11-02" :label "T" :count 3}
   {:date "2026-11-03" :label "W" :count 1 :today true}])

(deftest bar-chart-has-one-group-per-bar
  (let [{:keys [view-box markup]} (learn-page/bar-chart-markup studied)]
    (is (= "0 0 300 132" view-box))
    (is (= 3 (n-of #"<g>" markup)))
    (is (= 3 (n-of #"<rect " markup)))
    (testing "a zero bar has no count label, the others do; every bar has a day label"
      (is (= 5 (n-of #"<text " markup))))
    (testing "titles keep the singular and plural of the old tooltip text"
      (is (str/includes? markup "<title>3 topics on 2026-11-02</title>"))
      (is (str/includes? markup "<title>1 topic on 2026-11-03</title>")))
    (testing "today is green"
      (is (str/includes? markup "fill:var(--color-success)")))))

(deftest bar-chart-escapes-text
  (let [{:keys [markup]} (learn-page/bar-chart-markup
                           [{:date "<b>&" :label "\"x\"" :count 1}])]
    (is (not (str/includes? markup "<b>")))
    (is (str/includes? markup "&lt;b&gt;&amp;"))
    (is (str/includes? markup "&quot;x&quot;"))))

(deftest bar-chart-of-no-data-is-valid
  (let [{:keys [markup]} (learn-page/bar-chart-markup [])]
    (is (= "" markup))))

(deftest future-due-chart-adds-backlog-line-and-total
  (let [{:keys [view-box markup]}
        (learn-page/future-due-chart-markup
          {:backlog 4
           :days [{:date "2026-11-08" :count 2 :today true}
                  {:date "2026-11-09" :count 0}]})]
    (is (= "0 0 340 132" view-box))
    (is (= 3 (n-of #"<rect " markup)) "backlog bar plus two days")
    (is (str/includes? markup "<title>4 overdue (backlog)</title>"))
    (is (= 1 (n-of #"<polyline " markup)))
    (is (str/includes? markup "Σ 6"))
    (testing "the backlog bar is orange and labelled with a chevron"
      (is (str/includes? markup "fill:var(--color-warning, #d97706)"))
      (is (str/includes? markup "‹")))))

(deftest charts-are-not-electric-trees
  (let [source (slurp "src/freememo/learn_page.cljc")
        start (str/index-of source "(e/defn Bar-chart")
        end (str/index-of source "(e/defn Kpi-tile")
        charts (subs source start end)]
    (is (not (str/includes? charts "e/for")) "no per-bar frames")
    (is (not (str/includes? charts "svg/rect")))))

(deftest the-dashboard-mounts-after-the-queue
  (let [source (slurp "src/freememo/learn_page.cljc")]
    (is (str/includes? source "(when (e/watch !queue-ready)\n            (LearnDashboard revision refresh user-id))"))
    (is (= 2 (n-of #"\(ready-after-paint! !queue-ready\)" source))
        "the queue marks ready when it loads and when it fails")))
