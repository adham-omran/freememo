(ns freememo.repo-tree-test
  "The selection matcher behind the import picker and every repository update
   (plans/git-forge-integration.md §4.7 7.4).

   Pure: no database and no network. The picker computes checkbox state and the
   importer computes rows from the same functions, so a defect here is a picker
   that shows one tree and imports another."
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.repo-tree :as tree]))

(def ^:private files
  ["README.md" "Makefile" "src/core.clj" "src/util.clj" "src/vendor/lib.js"
   "src/vendor/deep/x.js" "src-old/legacy.clj" "docs/guide.md" "docs/img/logo.png"])

(def ^:private commit-tree (tree/tree files))

(deftest paths-split-into-directory-name-and-extension
  (is (= "" (tree/parent-dir "README.md")))
  (is (= "src/vendor" (tree/parent-dir "src/vendor/lib.js")))
  (is (= "lib.js" (tree/base-name "src/vendor/lib.js")))
  (is (= ".clj" (tree/extension "src/core.clj")))
  (is (= "" (tree/extension "Makefile")))
  (is (= "" (tree/extension ".gitignore")) "a leading dot is not an extension")
  (is (= ".gz" (tree/extension "dist/a.tar.GZ")) "the last dot wins, lower-cased"))

(deftest the-tree-lists-every-directory-depth-first
  ;; `src/vendor` precedes `src-old`: a directory's children come before its
  ;; next sibling, although `-` sorts below `/` in a plain string compare.
  (is (= ["" "docs" "docs/img" "src" "src/vendor" "src/vendor/deep" "src-old"]
        (:dirs commit-tree))))

(deftest nothing-is-selected-until-the-user-selects-it
  (is (false? (tree/dir-selected? {} "src")))
  (is (= {:dirs [] :files []} (tree/import-plan commit-tree {})))
  (is (= 1 (tree/node-count (tree/import-plan commit-tree {})))
    "the commit node alone"))

(deftest a-selected-directory-can-hold-an-unselected-subdirectory
  (let [decisions (-> {} (tree/toggle-dir "src") (tree/toggle-dir "src/vendor"))
        plan (tree/import-plan commit-tree decisions)]
    (is (= {"src" true "src/vendor" false} decisions))
    (is (= ["src"] (:dirs plan)))
    (is (= ["src/core.clj" "src/util.clj"] (:files plan)))
    (testing "nothing below the unselected subdirectory is imported"
      (is (not-any? #(re-find #"vendor" %) (concat (:dirs plan) (:files plan)))))))

(deftest unselecting-a-parent-takes-its-subtree-with-it
  (is (= {} (-> {}
              (tree/toggle-dir "src")
              (tree/toggle-dir "src/vendor")
              (tree/toggle-dir "src")))
    "no decision below `src` survives, so no stale choice returns on a later select"))

(deftest a-directory-the-first-import-never-saw-follows-its-parent
  ;; The update rule (D6): a new file or directory inside a selected directory
  ;; appears; inside an unselected one it does not.
  (let [decisions {"src" true "src/vendor" false}]
    (is (true? (tree/dir-selected? decisions "src/new")))
    (is (false? (tree/dir-selected? decisions "src/vendor/new")))
    (is (false? (tree/dir-selected? decisions "lib")))))

(deftest selecting-the-root-selects-the-whole-repository
  (let [decisions (tree/toggle-dir {} "")
        plan (tree/import-plan commit-tree decisions)]
    (is (= {"" true} decisions))
    (is (= (count files) (count (:files plan))))
    (is (= 6 (count (:dirs plan))) "every directory but the root, which the commit stands for")
    (is (= (+ 1 6 (count files)) (tree/node-count plan)))))

(deftest subtree-nodes-name-parents-titles-and-sibling-order
  (let [decisions {"src/vendor" true}
        plan (tree/import-plan commit-tree decisions)
        nodes (tree/subtree-nodes plan decisions)
        by-path (into {} (map (juxt :path identity)) nodes)]
    (testing "an unselected ancestor stays readable in the title"
      (is (= {:kind "dir" :path "src/vendor" :parent nil :title "src/vendor" :ordinal 1}
            (by-path "src/vendor"))))
    (testing "a directory below an imported one is titled relative to it"
      (is (= "deep" (:title (by-path "src/vendor/deep"))))
      (is (= "src/vendor" (:parent (by-path "src/vendor/deep")))))
    (testing "directories sort before files among siblings"
      (is (= 1 (:ordinal (by-path "src/vendor/deep"))))
      (is (= 2 (:ordinal (by-path "src/vendor/lib.js")))))
    (testing "every parent precedes its children, which a batched insert needs"
      (let [index (into {} (map-indexed (fn [i n] [(:path n) i])) nodes)]
        (doseq [n nodes :when (:parent n)]
          (is (< (index (:parent n)) (index (:path n))) (:path n)))))))

(deftest sibling-order-ignores-case
  (let [decisions {"" true}
        nodes (tree/subtree-nodes (tree/import-plan (tree/tree ["b.clj" "A.clj" "c.clj"]) decisions)
                decisions)]
    (is (= {"A.clj" 1 "b.clj" 2 "c.clj" 3} (into {} (map (juxt :path :ordinal)) nodes)))))

(deftest picker-rows-count-the-files-directly-inside
  (let [rows (tree/picker-rows commit-tree)]
    (is (= {:path "" :name "" :depth 0 :file-count 2} (first rows)))
    (is (= {:path "src/vendor" :name "vendor" :depth 2 :file-count 1} (nth rows 4)))))

(deftest extension-counts-put-the-most-common-first
  ;; Equal counts sort by extension, so `.js` precedes `.md` and `""` precedes
  ;; `.png`.
  (is (= [[".clj" 3] [".js" 2] [".md" 2] ["" 1] [".png" 1]]
        (tree/extension-counts commit-tree))))

(deftest languages-come-from-the-extension
  (is (= "clojure" (tree/language-for "src/core.cljc")))
  (is (= "zig" (tree/language-for "build.zig")))
  (is (nil? (tree/language-for "Makefile"))))

(deftest the-caps-sit-below-the-parameter-ceiling
  (is (< tree/max-selection-nodes tree/max-repository-nodes 65535)))
