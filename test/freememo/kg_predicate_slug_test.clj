(ns freememo.kg-predicate-slug-test
  "Predicate identity checks that require no database."
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.kg-predicate :as predicate])
  (:import [java.util Locale]))

(deftest ordinary-ascii-slugs-stay-readable
  (doseq [[label expected] [["Drains into" "drains-into"]
                            [" --was signed in! " "was-signed-in"]
                            ["has\n duration" "has-duration"]
                            ["defined-in" "defined-in"]
                            ["u-shaped" "u-shaped"]
                            ["123" "123"]]]
    (is (= expected (predicate/slugify label)))))

(deftest unicode-labels-have-distinct-stable-ascii-identifiers
  (let [labels ["يسبب" "يُسَبِّبُ" "علاقة 1" "سبب 1" "علاقة ٢" "علاقة ٣"
                "relates إلى 1" "définit" "определяет" "定义"]
        slugs (mapv predicate/slugify labels)]
    (is (= (count labels) (count (set slugs))))
    (doseq [[label slug] (map vector labels slugs)]
      (is (re-matches #"[a-z0-9]+(?:-[a-z0-9]+)*" slug) label)
      (is (= slug (predicate/slugify label)))
      (is (<= (count slug) 72)))))

(deftest normalized-labels-reuse-identifiers
  (is (= (predicate/slugify "أثر") (predicate/slugify "أثر")))
  (is (= (predicate/slugify "يؤدي إلى") (predicate/slugify " --يؤدي\nإلى! ")))
  (is (= (predicate/slugify "DÉFINIT") (predicate/slugify "définit"))))

(deftest generated-identifiers-cannot-impersonate-labels
  (let [unicode-slug (predicate/slugify "يسبب")
        ascii-slug (predicate/slugify unicode-slug)]
    (is (not= unicode-slug ascii-slug))
    (is (not= ascii-slug (predicate/slugify ascii-slug)))
    (is (= ascii-slug (predicate/slugify unicode-slug))))
  (testing "collision candidates are stable and separate from literal ASCII labels"
    (let [first-candidate (predicate/collision-slug "يسبب" 0)
          second-candidate (predicate/collision-slug "يسبب" 1)]
      (is (not= first-candidate second-candidate))
      (is (= first-candidate (predicate/collision-slug "يسبب" 0)))
      (is (not= first-candidate (predicate/slugify first-candidate))))))

(deftest labels-without-letters-or-numbers-have-no-identifier
  (doseq [label [nil "" " \n" "---!!!" "🙂" "َّ"]]
    (is (nil? (predicate/canonical-label label)))
    (is (nil? (predicate/slugify label)))))

(deftest identity-does-not-depend-on-the-server-locale
  (let [previous (Locale/getDefault)]
    (try
      (Locale/setDefault (Locale/forLanguageTag "tr-TR"))
      (is (= "is-defined-in" (predicate/slugify "IS DEFINED IN")))
      (is (= (predicate/slugify "İLİŞKİ") (predicate/slugify "i̇li̇şki̇")))
      (finally
        (Locale/setDefault previous)))))
