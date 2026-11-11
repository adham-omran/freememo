(ns freememo.release-assets-test
  "Review fixes 4, 9 and 10: the one asset-name manifest has three readers that
   agree, the workflow fails clearly without a draft, the deploy makes the
   draft before it pushes the tag, and the public workflow pins its actions and
   keeps `${{ }}` out of `run:` blocks."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [freememo.update-feed :as uf]))

(def ^:private rv "26.11.13832")

(defn- lines [& cmd]
  (let [p (.start (doto (ProcessBuilder. ^java.util.List (vec cmd)) (.redirectErrorStream true)))
        out (slurp (.getInputStream p))]
    (is (zero? (.waitFor p)) out)
    (str/split-lines (str/trim out))))

(deftest the-three-readers-agree
  (testing "required names"
    (let [clj (uf/required-asset-names rv)]
      (is (= 5 (count clj)))
      (is (= clj (lines "node" "tools/installer/asset-names.mjs" rv)))
      (is (= clj (lines "sh" "tools/release/asset-names.sh" rv)))))
  (testing "one kind each, the optional .sig as a field of the manifest"
    (doseq [kind ["macos" "windows" "linux" "app" "release" "sig"]]
      (let [expected (uf/asset-name rv (keyword kind))]
        (is (= [expected] (lines "node" "tools/installer/asset-names.mjs" rv kind)) kind)
        (is (= [expected] (lines "sh" "tools/release/asset-names.sh" rv kind)) kind))))
  (is (= [(uf/asset-name rv :sig)] (lines "sh" "tools/release/asset-names.sh" rv "--optional")))
  (is (not-any? #(str/ends-with? % ".sig") (uf/required-asset-names rv))))

(defn- workflow [] (slurp (io/file (str "." "github") "workflows" "installers.yml")))

(deftest the-workflow-fails-clearly-without-a-draft
  (let [text (workflow)]
    (is (str/includes? text "Check that the draft release exists"))
    (is (str/includes? text "There is no draft release for the tag"))
    (is (< (str/index-of text "Check that the draft release exists")
          (str/index-of text "  jar:")) "before any job builds")))

(deftest actions-are-pinned-and-run-blocks-take-values-from-env
  (let [text (workflow)]
    (doseq [[_ ref] (re-seq #"uses:\s+(\S+)" text)]
      (is (re-find #"@[0-9a-f]{40}$" ref) (str ref " is pinned to a full commit SHA")))
    (is (every? #(re-find #"@[0-9a-f]{40} # \S+" %)
          (filter #(str/includes? % "uses:") (str/split-lines text)))
      "the tag stays in a comment")
    (let [in-run (->> (str/split text #"(?m)^      - ")
                   (filter #(re-find #"(?m)^\s+run:" %))
                   (map #(->> (rest (str/split-lines (second (str/split % #"(?m)^\s+run:" 2))))
                           (take-while (fn [l] (or (str/blank? l) (str/starts-with? l "          "))))
                           (str/join "\n"))))]
      (is (seq in-run))
      (doseq [r in-run]
        (is (not (str/includes? r "${{")) (str "inline expression in a run block: " (subs r 0 (min 80 (count r)))))))))

(deftest the-deploy-makes-the-draft-before-it-pushes-the-tag
  (let [text (slurp (io/file ".forgejo" "workflows" "deploy-truenas.yml"))
        draft (str/last-index-of text "tools/release/create-draft.sh")
        push (str/index-of text "push public \"refs/tags/")]
    (is (and draft push))
    (is (< draft push))))
