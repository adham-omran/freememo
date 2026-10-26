(ns freememo.tooltip-construct-test
  "Pins the rule that every hover tooltip goes through `freememo.tooltip`.

   The app once set a tooltip three ways: `Tooltip!`, an inline
   `:data-tooltip`, and a native `title`. CLAUDE.md asked for the first one,
   and the other two still grew to about 80 sites, because each new element
   copied the one beside it. Only a failing test stops the next copy.

   See plans/tooltip-top-layer.md."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(def ^:private owner
  "The one file that may write the attribute."
  "src/freememo/tooltip.cljc")

(defn- source-files []
  (->> (file-seq (io/file "src"))
       (filter #(re-find #"\.clj[cs]?$" (.getName ^java.io.File %)))
       (map #(str/replace (.getPath ^java.io.File %) "\\" "/"))
       sort))

(defn- re-seq-pos
  "Post: {:start i} for each match of `re` in `s`, in order."
  [re s]
  (let [m (re-matcher re s)]
    (loop [acc []]
      (if (.find m)
        (recur (conj acc {:start (.start m)}))
        acc))))

(defn- line-of
  "Post: the 1-based line of character `index` in `text`."
  [text index]
  (inc (count (re-seq #"\n" (subs text 0 index)))))

(defn- hit
  "Post: `path:line: text` for the line that holds character `index`."
  [path text index]
  (let [n (line-of text index)]
    (str path ":" n ": " (str/trim (nth (str/split-lines text) (dec n))))))

(defn- form-end
  "Post: the index just past the form that opens at `start` (an open paren),
   or the text length when it never closes. Skips strings, character
   literals and comments, so a paren inside one does not count."
  [text start]
  (let [n (count text)]
    (loop [i start depth 0 in-string? false]
      (if (>= i n)
        n
        (let [c (.charAt ^String text i)]
          (cond
            in-string? (case c
                         \\ (recur (+ i 2) depth true)
                         \" (recur (inc i) depth false)
                         (recur (inc i) depth true))
            (= c \\) (recur (+ i 2) depth false)
            (= c \") (recur (inc i) depth true)
            (= c \;) (recur (or (str/index-of text "\n" i) n) depth false)
            (#{\( \[ \{} c) (recur (inc i) (inc depth) false)
            (#{\) \] \}} c) (if (= depth 1) (inc i) (recur (inc i) (dec depth) false))
            :else (recur (inc i) depth false)))))))

(def ^:private title-key
  "`:title` as a map key. Not `(:title x)`, which is a lookup."
  #"(?<![(\w\-:]):title(?![\w\-])")

(defn- props-title-hits
  "Post: every `:title` key inside a `(dom/props …)` form in `path`."
  [path text]
  (for [m (re-seq-pos #"\(dom/props\b" text)
        :let [start (:start m)
              body (subs text start (form-end text start))]
        k (re-seq-pos title-key body)]
    (hit path text (+ start (:start k)))))

(def ^:private direct-writes
  "Ways to set a tooltip that bypass `freememo.tooltip`, outside a props map."
  [#":data-tooltip\b"
   #"\.setAttribute\s+\S+\s+\"data-tooltip\""
   #"\.setAttribute\s+\S+\s+\"title\""
   #"\(set!\s+\(\.-title\s+(?!js/document\b)"])

(def ^:private sealed-element-calls
  "Forms5 calls that build their own element and take its props as keyword
   arguments. The caller never reaches the element body, so `Tooltip!` cannot
   go there, and `:data-tooltip` as a keyword argument is the only way in."
  #"\((?:forms/Button!|forms/SubmitButton!)(?=[\s)])")

(defn- sealed-ranges
  "Post: [start end) of every sealed element call in `text`."
  [text]
  (for [m (re-seq-pos sealed-element-calls text)
        :let [start (:start m)]]
    [start (form-end text start)]))

(defn- direct-write-hits [path text]
  (let [ranges (sealed-ranges text)
        sealed? (fn [i] (some (fn [[a b]] (and (<= a i) (< i b))) ranges))]
    (for [re direct-writes
          m (re-seq-pos re text)
          :when (not (and (= ":data-tooltip" (subs text (:start m) (+ (:start m) 13)))
                          (sealed? (:start m))))]
      (hit path text (:start m)))))

(deftest every-tooltip-goes-through-freememo-tooltip
  (testing "no file but tooltip.cljc writes data-tooltip or a native title"
    (let [hits (->> (source-files)
                    (remove #{owner})
                    (mapcat (fn [path]
                              (let [text (slurp path)]
                                (concat (direct-write-hits path text)
                                        (props-title-hits path text))))))]
      (is (empty? hits)
          (str "Set a tooltip with tooltip/Tooltip! (Electric) or "
               "tooltip/set-tooltip! (plain CLJS), never directly:\n"
               (str/join "\n" hits))))))

(deftest the-scanner-finds-what-it-guards
  (testing "a multi-line props map with a :title key is a hit"
    (is (= 1 (count (props-title-hits
                      "x.cljc"
                      "(dom/button\n  (dom/props {:class \"b\"\n              :title \"Zoom\"}))")))))
  (testing "a :title lookup inside a props map is not a hit"
    (is (empty? (props-title-hits "x.cljc" "(dom/props {:data-x (:title s)})"))))
  (testing "a :data-tooltip keyword argument to a Forms5 button is allowed"
    (is (empty? (direct-write-hits
                  "x.cljc"
                  "(forms/Button! [::k]\n  :label \"Add\" :data-tooltip \"t\")"))))
  (testing "a :data-tooltip key in a props map is a hit"
    (is (= 1 (count (direct-write-hits "x.cljc" "(dom/props {:data-tooltip \"t\"})")))))
  (testing "a paren inside a string does not end the props form early"
    (is (= 1 (count (props-title-hits
                      "x.cljc"
                      "(dom/props {:class \")\" :title \"t\"})"))))))
