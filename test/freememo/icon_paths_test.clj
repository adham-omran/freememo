(ns freememo.icon-paths-test
  "svg-markup's postcondition, and the format menu's use of the shared map.

   Needs no database: freememo.icon-paths has no requires."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [freememo.icon-paths :as icon-paths]))

(def ^:private format-menu-icons
  "Every icon freememo.format-menu names. A missing one renders blank."
  [:baseline :bold :italic :underline :strikethrough :remove-formatting
   :list :list-ordered :ellipsis :list-indent-decrease :list-indent-increase
   :text-align-start :text-align-center :text-align-end :text-align-justify
   :pilcrow-left :pilcrow :heading-1 :heading-2 :heading-3 :code-xml
   :table :image :volume-2 :a-large-small :chevron-down :sparkles
   :circle-help :cloze-new :cloze-same :page-boundary])

(deftest a-known-icon-renders-lucides-stroke-contract
  (let [m (icon-paths/svg-markup :bold 18)]
    (is (str/starts-with? m "<svg"))
    (is (str/ends-with? m "</svg>"))
    (doseq [attr ["viewBox=\"0 0 24 24\"" "width=\"18\"" "height=\"18\""
                  "fill=\"none\"" "stroke=\"currentColor\"" "stroke-width=\"2\""
                  "stroke-linecap=\"round\"" "stroke-linejoin=\"round\""
                  "aria-hidden=\"true\""]]
      (is (str/includes? m attr) attr))
    (testing "every child of the entry appears, with its attributes"
      (is (str/includes? m "<path d=\"M6 12h9a4 4 0 0 1 0 8H7a1 1 0 0 1-1-1V5a1 1 0 0 1 1-1h7a4 4 0 0 1 0 8\"/>")))
    (testing "non-path children keep their tag"
      (is (str/includes? (icon-paths/svg-markup :italic 18)
            "<line x1=\"19\" x2=\"10\" y1=\"4\" y2=\"4\"/>")))))

(deftest an-unknown-icon-renders-an-empty-svg
  (let [m (icon-paths/svg-markup :no-such-icon 18)]
    (is (str/starts-with? m "<svg"))
    (is (str/ends-with? m "\"true\"></svg>"))))

(deftest every-format-menu-icon-has-paths
  (doseq [k format-menu-icons]
    (is (seq (get icon-paths/paths k)) (str k))))
