(ns freememo.walkthrough-range-test
  "Diff reading and line attribution over a fixture repository
   (plans/commit-range-walkthrough.md §6.4, §6.6.3). No database."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [freememo.git-clone :as git]
            [freememo.git-clone-test :as fixture]
            [freememo.unified-diff :as diff]
            [freememo.walkthrough-range :as wr]))

(deftest lines-keep-their-terminators
  (is (= ["a\n" "b"] (diff/lines "a\nb")))
  (is (= ["a\n" "b\n"] (diff/lines "a\nb\n")))
  (is (= [] (diff/lines "")))
  (doseq [t ["" "x" "x\n" "\n\n" "a\nb\nc"]]
    (is (= t (apply str (diff/lines t))))))

(deftest a-patch-parses-into-files-and-hunks
  (let [patch (str "diff --git a/x.zig b/x.zig\n"
                   "index 1..2 100644\n--- a/x.zig\n+++ b/x.zig\n"
                   "@@ -1,2 +1,2 @@ fn top\n a\n-b\n+c\n"
                   "@@ -10 +10,2 @@\n z\n+y\n\\ No newline at end of file\n"
                   "diff --git a/old.md b/new.md\nsimilarity index 100%\n"
                   "rename from old.md\nrename to new.md\n"
                   "diff --git a/img.png b/img.png\nnew file mode 100644\nBinary files /dev/null and b/img.png differ\n")
        [x r img] (diff/parse patch)]
    (is (= 2 (count (:hunks x))))
    (is (= [0 1] (map :index (:hunks x))))
    (is (= [" a\n" "-b\n" "+c\n"] (:lines (first (:hunks x)))))
    (is (= "+y" (last (:lines (second (:hunks x))))) "no newline at end of file")
    (is (= [1 1] ((juxt :added :removed) (first (:hunks x)))))
    (is (= {:old-path "old.md" :new-path "new.md" :status :renamed}
          (select-keys r [:old-path :new-path :status])))
    (is (:pseudo? (first (:hunks r))) "a pure rename still gets a hunk")
    (is (= [nil "img.png" :added true] ((juxt :old-path :new-path :status :binary?) img)))))

(deftest every-line-names-the-hunk-that-wrote-it
  (let [{repo :dir [a b c d] :shas}
        (fixture/fixture-repo!
          [{"docs/spec.md" "# Spec\n\nA cycle op.\n\nEnd.\n" :message "start"}
           {"docs/spec.md" "# Spec\n\nA cycle op.\n\nA hold op splits.\n\nEnd.\n" :message "feat: split hold"}
           {"docs/ir.md" "# Spec\n\nA loop op.\n\nA hold op splits.\n\nEnd.\n"
            "docs/spec.md" nil :message "refactor: rename cycle to loop, move file"}
           {"src/x.zig" "const a = 1;\n" :message "feat: x"}])
        dir (io/file (fixture/temp-dir "clones") "1.git")]
    (fixture/with-file-protocol
      (git/clone! dir (str "file://" repo) {:what "the fixture"})
      (let [commits (wr/range-commits dir a d)]
        (is (= [b c d] (map :sha commits)))
        (testing "§6.4.2: the range is checked"
          (is (thrown-with-msg? Exception #"before the second" (wr/range-commits dir d a)))
          (with-redefs [wr/max-commits 2]
            (is (thrown-with-msg? Exception #"at most 2" (wr/range-commits dir a d)))))
        (testing "§6.4.3: hunks carry their commit and paths"
          (let [hunks (wr/range-hunks dir commits {})]
            (is (= #{b c d} (set (map :commit-sha hunks))))
            (is (some #(and (= c (:commit-sha %)) (= "docs/ir.md" (:new-path %))) hunks))))
        (testing "a renamed file is followed back to A"
          (let [{:keys [lines attributed?]} (wr/file-state dir a commits c "docs/ir.md" {})
                origin-of (fn [needle] (:origin (first (filter #(str/includes? (:text %) needle) lines))))]
            (is attributed?)
            (is (nil? (origin-of "# Spec")) "unchanged since A")
            (is (= b (first (origin-of "hold"))) "written by the split")
            (is (= c (first (origin-of "loop"))) "written by the rename")))
        (testing "a file the range adds starts empty"
          (let [{:keys [lines]} (wr/file-state dir a commits d "src/x.zig" {})]
            (is (= d (first (:origin (first lines)))))))))))

(deftest noise-is-vendored-or-huge
  (let [{repo :dir [a b] :shas}
        (fixture/fixture-repo!
          [{".gitattributes" "vendor/** linguist-vendored\n" "a.md" "x\n" :message "start"}
           {"vendor/lib.c" "int x;\n" "a.md" "y\n"
            "big.txt" (apply str (repeat 1600 "line\n")) :message "add"}])
        dir (io/file (fixture/temp-dir "clones") "2.git")]
    (fixture/with-file-protocol
      (git/clone! dir (str "file://" repo) {:what "the fixture"})
      (let [commits (wr/range-commits dir a b)
            hunks (wr/range-hunks dir commits {})
            noise (wr/noise-keys dir hunks {})
            noisy (set (keep (fn [h] (when (noise (wr/hunk-key (:commit-sha h) (:old-path h)
                                                     (:new-path h) (:index h)))
                                       (:new-path h)))
                         hunks))]
        (is (= #{"vendor/lib.c" "big.txt"} noisy))))))
