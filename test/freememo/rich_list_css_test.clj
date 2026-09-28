(ns freememo.rich-list-css-test
  "The list geometry outside an editor exists twice: under `.fm-rich` in
   index.css (read-only card views in the app) and unscoped in
   freememo-anki.css (pushed to Anki as note-model styling). Nothing but this
   test keeps the two copies equal. See plans/bullet-indent-tab-behavior.md."
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]))

(def ^:private start-marker "/* fm-rich-lists:start */")
(def ^:private end-marker "/* fm-rich-lists:end */")

(defn- marked-block
  "The text from the start marker to the end marker, both included, or nil
   when the file does not hold exactly one of each."
  [css]
  (let [start (str/index-of css start-marker)
        end (str/index-of css end-marker)]
    (when (and start end (< start end)
               (= start (str/last-index-of css start-marker))
               (= end (str/last-index-of css end-marker)))
      (subs css start (+ end (count end-marker))))))

(defn- read-block [path]
  (marked-block (slurp (io/file path))))

(deftest anki-block-is-the-app-block-without-its-scope
  (let [app (read-block "resources/public/freememo/index.css")
        anki (read-block "resources/public/freememo/freememo-anki.css")]
    (testing "each file holds exactly one marked block"
      (is (some? app))
      (is (some? anki)))
    (testing "removing the `.fm-rich ` prefix from the app block gives the Anki block"
      (is (= anki (str/replace app ".fm-rich " ""))))
    (testing "the Anki block carries no app scope"
      (is (not (str/includes? anki ".fm-rich"))))))

(deftest a-one-character-drift-fails
  (let [app (read-block "resources/public/freememo/index.css")
        drifted (str/replace-first app "1.25em" "1.26em")]
    (is (not= app drifted))
    (is (not= (read-block "resources/public/freememo/freememo-anki.css")
              (str/replace drifted ".fm-rich " "")))))
