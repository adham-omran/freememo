(ns freememo.gen-settings-test
  "plans/toolbar-card-actions-fold.md §1: the one settings value the popover,
   the invokers and the whole-document form share."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [freememo.gen-settings :as gs]))

(use-fixtures :each (fn [t] (reset! gs/!gen-settings nil) (t) (reset! gs/!gen-settings nil)))

(deftest defaults-match-the-saved-setting-defaults
  ;; settings/get-* answer these for a user who never saved a value.
  (is (= {:count 2 :kind "basic" :context? true :context-pages 1} gs/defaults)))

(deftest normalize-keeps-each-setting-in-range
  (is (= gs/count-min (gs/normalize :count 0)))
  (is (= gs/count-max (gs/normalize :count 99)))
  (is (= gs/context-pages-min (gs/normalize :context-pages -3)))
  (is (= gs/context-pages-max (gs/normalize :context-pages 11)))
  (is (= "cloze" (gs/normalize :kind "cloze")))
  (is (= "basic" (gs/normalize :kind "essay")) "an unknown kind becomes the default")
  (is (false? (gs/normalize :context? nil))))

(deftest an-unseeded-atom-reads-as-the-defaults
  (is (= gs/defaults (gs/current)))
  (is (= gs/defaults (gs/effective nil))))

(deftest assoc-setting-fills-every-key
  (testing "a write before the seed makes a full map"
    (gs/assoc-setting! :kind "overlapping")
    (is (= (assoc gs/defaults :kind "overlapping") @gs/!gen-settings)))
  (testing "a write is normalized"
    (gs/assoc-setting! :count 50)
    (is (= gs/count-max (:count @gs/!gen-settings)))))

(deftest from-form-parses-strings-and-falls-back
  (let [fallback {:count 4 :kind "cloze" :context? true :context-pages 3}]
    (is (= {:count 7 :kind "basic" :context? false :context-pages 2}
           (gs/from-form {:count "7" :kind "basic" :context? false :context-pages "2"} fallback)))
    (is (= fallback (gs/from-form {} fallback)) "untouched fields keep the fallback")
    (is (= 4 (:count (gs/from-form {:count "x"} fallback))) "an unparseable number keeps the fallback")
    (is (= gs/count-max (:count (gs/from-form {:count "80"} fallback))))))
