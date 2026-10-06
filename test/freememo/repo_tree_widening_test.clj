(ns freememo.repo-tree-widening-test
  "Adding to an imported repository's selection, the pure half
   (plans/repo-selection-widening.md §3). No database and no git."
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.repo-tree :as tree]))

(def ^:private tree
  (tree/tree ["README.md"
              "src/main/core.clj" "src/main/notes.md"
              "src/test/core_test.clj"
              "src/util.clj"
              "docs/guide.md"
              "a/b/c.clj"]))

(defn- sel [dirs exts] {:dirs dirs :extensions (set exts)})

;; ── §3.1 widens? ───────────────────────────────────────────────────

(deftest widens-holds-only-for-a-superset
  (let [old (sel {"src" true "src/test" false} [".clj"])]
    (testing "the same selection"
      (is (tree/widens? old old)))
    (testing "unchecking a selected directory"
      (is (not (tree/widens? old (sel {"src" true "src/test" false "src/main" false} [".clj"])))))
    (testing "unselecting through the root"
      (is (not (tree/widens? old (sel {} [".clj"])))))
    (testing "selecting the unselected subdirectory"
      (is (tree/widens? old (sel {"src" true} [".clj"]))))
    (testing "selecting an ancestor of a selected directory"
      (is (tree/widens? (sel {"src/main" true} [".clj"]) (sel {"src" true} [".clj"]))))
    (testing "an explicit false under a newly selected directory"
      (is (tree/widens? (sel {"src/main" true} [])
            (sel {"src" true "src/test" false} []))))
    (testing "removing an extension"
      (is (not (tree/widens? old (sel {"src" true "src/test" false} [])))))
    (testing "adding an extension"
      (is (tree/widens? old (sel {"src" true "src/test" false} [".clj" ".md"]))))))

(deftest a-locked-directory-is-one-whose-unchecking-shrinks-the-selection
  (let [old {"src/main" true}]
    (testing "a first import locks nothing"
      (is (not (tree/dir-locked? {} {"src" true} "src"))))
    (testing "the stored directory is locked"
      (is (tree/dir-locked? old old "src/main")))
    (testing "an added ancestor is locked, because unchecking it drops src/main"
      (is (tree/dir-locked? old {"src" true} "src")))
    (testing "an added sibling is not"
      (is (not (tree/dir-locked? old {"src/main" true "docs" true} "docs"))))
    (testing "an unselected directory is not"
      (is (not (tree/dir-locked? old old "docs"))))))

;; ── §3.2 widening-plan ─────────────────────────────────────────────

(defn- live-of
  "The `live` map of a fresh import of `tree` under `dirs`, with ids from 100."
  [dirs]
  (let [nodes (tree/subtree-nodes (tree/import-plan tree dirs) dirs)]
    (into {}
      (map-indexed (fn [i {:keys [path parent title ordinal]}]
                     [path {:id (+ 100 i) :parent parent :title title :ordinal ordinal}]))
      nodes)))

(deftest an-unchanged-selection-writes-nothing
  (let [old (sel {"src" true} [".clj"])
        p (tree/widening-plan tree old old (live-of (:dirs old)))]
    (is (= {:insert [] :move [] :fill [] :skipped []} p))))

(deftest adding-an-ancestor-moves-the-imported-directory-under-it
  (let [old (sel {"src/main" true} [".clj"])
        new (sel {"src" true} [".clj"])
        live (live-of (:dirs old))
        {:keys [insert move]} (tree/widening-plan tree old new live)]
    (testing "§3.2.1: only paths the old plan lacks are inserted"
      (is (= #{"src" "src/test" "src/util.clj" "src/test/core_test.clj"}
            (set (map :path insert))))
      (is (not-any? #(contains? live (:path %)) insert)))
    (testing "§3.2.3: src/main moves under src and is retitled main"
      (is (= [{:id (get-in live ["src/main" :id]) :parent "src" :title "main" :ordinal 1}]
            move)))))

(deftest a-removed-path-is-never-inserted-again
  (let [old (sel {"src" true} [".clj"])
        new (sel {"" true} [".clj"])
        live (dissoc (live-of (:dirs old)) "src/util.clj")
        {:keys [insert]} (tree/widening-plan tree old new live)]
    (is (not-any? #(= "src/util.clj" (:path %)) insert))
    (is (some #(= "README.md" (:path %)) insert))))

(deftest a-new-path-under-a-deleted-directory-is-skipped
  (testing "§3.2.2: a/ was imported and deleted; adding a/b/ inserts nothing under a/"
    (let [old (sel {"a" true "a/b" false} [])
          new (sel {"a" true} [])
          live (dissoc (live-of (:dirs old)) "a")
          {:keys [insert skipped]} (tree/widening-plan tree old new live)]
      (is (empty? insert))
      (is (= ["a/b" "a/b/c.clj"] skipped)))))

(deftest an-added-extension-fills-the-live-files-that-carry-it
  (let [old (sel {"" true} [".clj"])
        new (sel {"" true} [".clj" ".md"])
        live (dissoc (live-of (:dirs old)) "docs/guide.md")
        {:keys [insert fill]} (tree/widening-plan tree old new live)]
    (testing "§3.2.4: every live .md file and no other"
      (is (= #{"README.md" "src/main/notes.md"} (set fill))))
    (testing "the removed .md file is neither filled nor inserted"
      (is (not (some #{"docs/guide.md"} fill)))
      (is (empty? insert)))))

(deftest ordinals-number-the-rows-that-exist-after-the-run
  (let [old (sel {"src" true} [])
        new (sel {"" true} [])
        live (live-of (:dirs old))
        {:keys [insert move]} (tree/widening-plan tree old new live)
        top (filter #(nil? (:parent %)) insert)]
    (testing "directories first, then files, from 1"
      (is (= {"a" 1 "docs" 2 "README.md" 4} (into {} (map (juxt :path :ordinal)) top))))
    (testing "the live src keeps its title and moves to ordinal 3"
      (is (= [{:id (get-in live ["src" :id]) :parent nil :title "src" :ordinal 3}] move)))))

(deftest a-staged-row-holds-its-ordinal-and-parents-nothing-new
  (let [old (sel {"src" true "src/test" false} [".clj"])
        new (sel {"" true} [".clj" ".md"])
        live (update-vals (live-of (:dirs old))
               #(assoc % :staged? false))
        ;; The user deleted src/ inside the undo window: the whole subtree is staged.
        live (into {} (map (fn [[p row]] [p (cond-> row (= "src" (subs p 0 (min 3 (count p))))
                                              (assoc :staged? true))]))
               live)
        {:keys [insert move fill]} (tree/widening-plan tree old new live)]
    (testing "src/test is new, but its parent is staged, so it is skipped"
      (is (not-any? #(.startsWith ^String (:path %) "src/") insert)))
    (testing "the staged src still takes a top-level ordinal, so none collides with it"
      (is (= #{1 2 4} (set (map :ordinal (filter #(nil? (:parent %)) insert)))))
      (is (= [3] (map :ordinal (filter #(= (get-in live ["src" :id]) (:id %)) move)))))
    (testing "a staged file is not filled"
      (is (not (some #{"src/main/notes.md"} fill))))))
