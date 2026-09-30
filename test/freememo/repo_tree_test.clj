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

(deftest set-dir-agrees-with-toggle-dir
  (doseq [decisions [{} {"src" true} {"" true "src/vendor" false}]
          dir (:dirs commit-tree)]
    (is (= (tree/toggle-dir decisions dir)
          (tree/set-dir decisions dir (not (tree/dir-selected? decisions dir))))
      (str decisions " " dir))))

(deftest set-dirs-ignores-order
  (let [dirs ["src" "src/vendor/deep" "docs/img"]]
    (doseq [decisions [{} {"" true} {"src/vendor" false}]
            target [true false]]
      (is (= (tree/set-dirs decisions dirs target)
            (tree/set-dirs decisions (reverse dirs) target))
        (str decisions " " target)))))

(deftest a-directory-search-keeps-the-ancestors-of-a-match
  (let [rows (tree/picker-rows commit-tree)]
    (testing "a match deep in the tree brings its ancestors, marked as context"
      (is (= [["" false] ["src" false] ["src/vendor" false] ["src/vendor/deep" true]]
            (mapv (juxt :path :match?) (tree/filter-picker-rows rows "DEEP")))))
    (testing "the name matches, not the path"
      (is (= ["" "src" "src/vendor"]
            (mapv :path (tree/filter-picker-rows rows "vendor"))))
      (is (= ["" "src" "src-old"]
            (mapv :path (tree/filter-picker-rows rows "src")))
        "src/vendor holds \"src\" in its path, not in its name"))
    (testing "a blank query shows every row as a match"
      (is (= (mapv #(assoc % :match? true) rows) (tree/filter-picker-rows rows "  "))))
    (testing "no match shows nothing"
      (is (= [] (tree/filter-picker-rows rows "nothing-here"))))))

(deftest all-writes-only-the-outermost-matches
  (let [rows (tree/picker-rows commit-tree)
        outermost #(tree/outermost-matches (tree/filter-picker-rows rows %))]
    (testing "a blank query writes the root, which is today's All and None"
      (is (= [""] (outermost "")))
      (is (= {"" true} (tree/set-dirs {} (outermost "") true)))
      (is (= {} (tree/set-dirs {"src" true} (outermost "") false))))
    (testing "a match inside another match is covered by it"
      ;; "vendor" and "deep" both match "e"; deep sits under vendor.
      (is (= ["src/vendor"] (outermost "e"))))
    (testing "All selects the matches and their subtrees, and nothing else"
      (let [plan (tree/import-plan commit-tree (tree/set-dirs {} (outermost "vendor") true))]
        (is (= ["src/vendor" "src/vendor/deep"] (:dirs plan)))))))

(deftest an-extension-search-matches-the-label-shown
  (let [counts (tree/extension-counts commit-tree)]
    (is (= [[".js" 2]] (tree/filter-extension-counts counts ".J")))
    (is (= [["" 1]] (tree/filter-extension-counts counts "no ext"))
      "the empty extension is found by its label")
    (is (= [[".clj" 3] [".png" 1]]
          (filterv (fn [[e _]] (#{".clj" ".png"} e)) (tree/filter-extension-counts counts "")))
      "a blank query keeps every pair")
    (is (= [] (tree/filter-extension-counts counts ".rs")))))

(deftest languages-come-from-the-extension
  (is (= "clojure" (tree/language-for "src/core.cljc")))
  (is (= "zig" (tree/language-for "build.zig")))
  (is (nil? (tree/language-for "Makefile"))))

(deftest the-caps-sit-below-the-parameter-ceiling
  (is (< tree/max-selection-nodes tree/max-repository-nodes 65535)))
