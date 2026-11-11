(ns freememo.nav-rail-label-css-test
  "No rule in index.css may clip the text of a span inside a drawer label.

   On iOS Safari, a span inside `.nav-rail__label` with `overflow: hidden`
   (or `clip`) paints no text when the phone drawer goes from
   `visibility: hidden` to visible. The middle rows of the drawer then show
   their icons and no labels. A scroll or a rotation paints them again.
   Chromium and Playwright's Linux WebKit paint them, so no browser test here
   can go red on the bug. This test keeps the clip from coming back. The label
   itself may clip. On the device, only the clip on the inner span stopped the
   paint. See plans/phone-drawer-labels.md."
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]))

(def ^:private css-path "resources/public/freememo/index.css")

(def ^:private inner-span-classes
  "The classes of the spans that sit inside a `.nav-rail__label`."
  #{"nav-rail__sub" "nav-rail__keep-row"})

(def ^:private clip-properties #{"overflow" "overflow-x" "overflow-y" "text-overflow"})

(defn- rules
  "Each innermost `selector { body }` in `css`, as [selector body]. Comments
   are removed first. A rule inside `@media` comes out with its own selector."
  [css]
  (let [css (str/replace css #"(?s)/\*.*?\*/" "")]
    (for [[_ selector body] (re-seq #"([^{}]+)\{([^{}]*)\}" css)]
      [(str/trim selector) body])))

(defn- properties [body]
  (set (for [[_ p] (re-seq #"(?:^|;)\s*([a-z-]+)\s*:" body)] p)))

(defn- reaches-inner-span?
  "True when one selector in the list `selector` targets an element inside a
   `.nav-rail__label`, or one of the inner span classes."
  [selector]
  (some (fn [s]
          (or (re-find #"\.nav-rail__label(?![\w-])(?:\s*>\s*|\s+)[^\s,)]" s)
              (some #(re-find (re-pattern (str "\\." % "(?![\\w-])")) s) inner-span-classes)))
        (str/split selector #",(?![^(]*\))")))

(defn- clipping-rules [css]
  (for [[selector body] (rules css)
        :when (reaches-inner-span? selector)
        :let [found (filter clip-properties (properties body))]
        :when (seq found)]
    {:selector selector :properties (set found)}))

(deftest no-rule-clips-the-text-inside-a-drawer-label
  (let [css (slurp (io/file css-path))]
    (testing "the parser finds the label rule, so an empty result means something"
      (is (some (fn [[selector _]] (= ".nav-rail__label" selector)) (rules css))))
    (testing "no rule gives a span inside a drawer label a clip or an ellipsis"
      (is (= [] (vec (clipping-rules css)))))))

(deftest the-check-catches-each-form-of-the-clip
  (testing "the rule that caused the bug"
    (is (seq (clipping-rules ".nav-rail__label > span { overflow: hidden; text-overflow: ellipsis; }"))))
  (testing "a descendant selector inside a media query"
    (is (seq (clipping-rules "@media (max-width: 600px) { .nav-rail .nav-rail__label span { overflow-x: clip; } }"))))
  (testing "an inner span class"
    (is (seq (clipping-rules ".nav-rail__sub { text-overflow: ellipsis; }"))))
  (testing "a list where only one selector reaches the span"
    (is (seq (clipping-rules ".x, .nav-rail__label > span { overflow: clip; }")))))

(deftest the-check-allows-what-the-device-proved-safe
  (testing "the label may clip its own box"
    (is (empty? (clipping-rules ".nav-rail__label { overflow: hidden; white-space: nowrap; }"))))
  (testing "a list of subjects inside :is() is not a descendant selector"
    (is (empty? (clipping-rules ".nav-rail[data-motion] :is(.nav-rail__label, .nav-rail__count) { overflow: hidden; }"))))
  (testing "a span rule with no clip"
    (is (empty? (clipping-rules ".nav-rail__label > span { color: red; }")))))
