(ns freememo.help-content-test
  "Guards for plans/searchable-help.md: slugs (1.2), resolved text (1.4), page
   search (3.1, 3.2), and the coverage guards that keep Help from silently
   falling behind the app (6.1-6.4)."
  (:require
   [clojure.set :as set]
   [clojure.test :refer [deftest is testing]]
   [freememo.commands :as commands]
   [freememo.help-content :as help]
   [freememo.page-title-format :as page-title-format]))

(def ^:private slugs (map :slug help/sections))

(def ^:private documented-ids
  (set (mapcat :commands (mapcat :steps help/sections))))

(deftest slugs-are-unique-and-url-safe
  (is (= (count slugs) (count (set slugs))) "duplicate section slug")
  (doseq [slug slugs]
    (is (re-matches #"[a-z0-9-]+" slug) (str "slug is not url-safe: " (pr-str slug)))))

(deftest step-labels-are-unique-within-a-section
  ;; HelpSection keys its steps with e/for-by :label.
  (doseq [{:keys [slug steps]} help/sections
          :let [labels (map :label steps)]]
    (is (= (count labels) (count (set labels))) (str slug " repeats a step label"))))

(deftest resolved-text-is-what-search-matches
  (let [chord (commands/display-chord :scan)
        hit-labels (fn [query] (set (map :label (mapcat :steps (help/filter-sections query)))))]
    (is (some? chord))
    (is (contains? (hit-labels chord) "Extract the text ({scan})")
      "the chord a user sees finds the step")
    (is (empty? (help/filter-sections "{scan}"))
      "the raw token is not text a user sees, so it finds nothing")))

(deftest page-search-filters-sections-and-steps
  (testing "a blank query returns every section unchanged"
    (is (= help/sections (help/filter-sections "")))
    (is (= help/sections (help/filter-sections "   "))))
  (testing "a step query leaves exactly the steps that mention it"
    (let [shown (help/filter-sections "zotero")
          expected (for [section help/sections
                         step (:steps section)
                         :when (re-find #"(?i)zotero" (help/resolved-text step))]
                     (:label step))]
      (is (seq expected))
      (is (= expected (map :label (mapcat :steps shown))))))
  (testing "a title match keeps every step of its section"
    (let [anki (help/section-by-slug "syncing-to-anki")
          shown (some #(when (= "syncing-to-anki" (:slug %)) %)
                  (help/filter-sections "Syncing to Anki"))]
      (is (= (:steps anki) (:steps shown)))))
  (testing "no match gives no sections"
    (is (empty? (help/filter-sections "xyzzy")))))

(deftest every-routed-tab-maps-to-help
  ;; 6.1 (G1). tab-labels has a key per routed tab, so a new tab lands here.
  ;; The rail is only one view of the tabs: it misses :import and :credits.
  (doseq [tab (keys page-title-format/tab-labels)]
    (let [v (get help/tab->slug tab ::missing)]
      (is (not= ::missing v) (str tab " has no tab->slug entry"))
      (cond
        (string? v) (is (help/section-by-slug v) (str tab " maps to unknown slug " v))
        (vector? v) (is (and (= :none (first v)) (string? (second v)))
                      (str tab " :none entry needs a reason"))
        :else nil))))

(deftest every-command-is-documented-or-excused
  ;; 6.2 (G2). A new registry entry fails here until a step documents it or
  ;; `undocumented` says why not.
  (let [excused (set (keys help/undocumented))
        registry-ids (set (keys commands/registry))]
    (is (empty? (set/difference registry-ids documented-ids excused))
      "registry ids with no Help step and no undocumented entry")
    (is (empty? (set/intersection documented-ids excused))
      "an id is both documented and listed as undocumented")
    ;; 6.3: neither list may name an id the registry no longer has.
    (is (empty? (set/difference documented-ids registry-ids)) "stale ids in :commands")
    (is (empty? (set/difference excused registry-ids)) "stale ids in undocumented")
    (doseq [[id reason] help/undocumented]
      (is (or (= :pending reason) (string? reason)) (str id " needs :pending or a reason")))))

(def ^:private pending-ceiling
  "6.4. The number of commands Help still owes a step. Lower it when a step
   lands. Never raise it: a new command owes Help a step, not a :pending."
  38)

(deftest pending-help-only-shrinks
  (is (<= (count (filter #(= :pending (val %)) help/undocumented)) pending-ceiling)))

(deftest a-command-opens-the-section-that-documents-it
  (is (= "studying-a-pdf" (help/slug-for-command :scan)))
  (is (= "syncing-to-anki" (help/slug-for-command :anki-sync)))
  (is (nil? (help/slug-for-command :debit-credits))))
