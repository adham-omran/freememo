(ns freememo.separator-test
  "Pins the rule that `src/` carries no middle dot.

   The dot reached 170 sites with no rule asking for it. Each new string copied
   the one beside it, and a docstring even named it \"the same idiom\". So the
   rule lives here as well as in CLAUDE.md: prose loses to copying, and only a
   failing test stops the next copy. Comments and docstrings count, because they
   are what gets copied.

   The replacements and the reason for each exception are in
   plans/middot-separator-replacement.md."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(def ^:private middot-pattern
  "Every spelling of U+00B7 a source file can carry: the literal character, the
   Clojure/JS escape, and the two HTML entities."
  #"·|\\u00[bB]7|&middot;|&#183;|&#[xX]0*[bB]7;")

(def ^:private allowed
  "Files that keep a dot, each for a reason that is not prose."
  {"src/freememo/ocr_math.clj"
   "input, not output: the map turns the dot OCR emits into \\cdot"
   "src/freememo/walkthrough_grouping.clj"
   "LLM prompt heading that no user reads; a test stub and a probe parse it"})

(defn- source-files []
  (->> (file-seq (io/file "src"))
       (filter #(re-find #"\.clj[cs]?$" (.getName ^java.io.File %)))
       (map #(str/replace (.getPath ^java.io.File %) "\\" "/"))
       sort))

(defn- dot-lines
  "Post: every `path:line: text` in `path` that matches `middot-pattern`."
  [path]
  (->> (str/split-lines (slurp path))
       (keep-indexed (fn [i line]
                       (when (re-find middot-pattern line)
                         (str path ":" (inc i) ": " (str/trim line)))))))

(deftest src-carries-no-middle-dot
  (let [offending (mapcat dot-lines (remove allowed (source-files)))]
    (is (empty? offending)
      (str "Use words, parentheses, a colon or commas instead (CLAUDE.md, "
           "\"Separators in user-visible text\"):\n"
           (str/join "\n" offending)))))

(deftest every-allowlisted-file-still-needs-its-entry
  (testing "an entry whose file no longer holds a dot hides the next one added there"
    (doseq [path (keys allowed)]
      (is (seq (dot-lines path)) (str path " holds no dot; remove its entry")))))
