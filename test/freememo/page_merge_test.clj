(ns freememo.page-merge-test
  "Guards the boundary rules of focus reader's merged reading surface
   (plans/merged-page-reading.md).

   Needs no database — freememo.page-merge requires only clojure.string and
   Jsoup, so this suite runs with PostgreSQL down.

   Four invariants carry the design, and each has its own deftest:
     1. A marker never reaches a stored slice (item 3.1).
     2. A page row is never absent from a split (item 4.1 / 4.2).
     3. No page gains text that sat below its own marker (the absorb-upward
        rule).
     4. The restore control answers the LOWEST missing page (item 4.6)."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [freememo.page-merge :as pm]))

(def ^:private pages
  [{:page-number 1 :content "<p>alpha</p>"}
   {:page-number 2 :content "<p>beta</p>"}
   {:page-number 3 :content "<p>gamma</p>"}])

(def ^:private nums [1 2 3])

;; ── The marker vocabulary ───────────────────────────────────────────────────

(deftest marker-ids-round-trip
  (doseq [n [1 2 7 42 883]]
    (testing (str "page " n)
      (is (= n (pm/parse-marker-id (pm/marker-id n)))))))

(deftest a-corrupted-marker-id-is-absent-not-page-zero
  (testing "a marker that parsed to 0 would sort before every real page and
            collect the document's opening text"
    (doseq [bad ["fm-page-x" "fm-page-" "fm-page-0" "fm-page-01" "fm-page--1"
                 "fm-page-1x" "other-7" "" nil]]
      (testing (pr-str bad)
        (is (nil? (pm/parse-marker-id bad)))))))

(deftest the-marker-shows-the-page-number-and-nothing-else
  (let [html (pm/marker-html 7)]
    (is (str/includes? html "Page 7"))
    (is (str/includes? html (pm/marker-id 7)))
    (testing "no page title — a page's title is literally \"Page N\" today"
      (is (= ">Page 7</h6>" (subs html (- (count html) 12)))))))

;; ── Merge ───────────────────────────────────────────────────────────────────

(deftest merge-emits-one-marker-per-page-in-page-order
  (let [merged (pm/merge-pages (shuffle pages))]
    (is (= 3 (count (re-seq (re-pattern pm/marker-id-prefix) merged))))
    (testing "ascending page order regardless of input order"
      (is (< (str/index-of merged (pm/marker-id 1))
            (str/index-of merged (pm/marker-id 2))
            (str/index-of merged (pm/marker-id 3)))))))

(deftest merge-keeps-a-marker-for-an-empty-page
  (testing "an unscanned page keeps a boundary the reader can navigate to and
            the split can find"
    (let [merged (pm/merge-pages [{:page-number 1 :content "<p>a</p>"}
                                  {:page-number 2 :content ""}
                                  {:page-number 3 :content nil}])]
      (is (str/includes? merged (pm/marker-id 2)))
      (is (str/includes? merged (pm/marker-id 3))))))

(deftest merge-drops-a-row-with-no-page-number
  (testing "a row with no page number cannot be addressed by a marker"
    (let [merged (pm/merge-pages [{:page-number 1 :content "<p>a</p>"}
                                  {:content "<p>orphan</p>"}])]
      (is (= 1 (count (re-seq (re-pattern pm/marker-id-prefix) merged)))))))

(deftest merge-reads-the-namespaced-keys-the-database-returns
  (let [merged (pm/merge-pages [{:topics/page_number 4 :topics/content "<p>d</p>"}])]
    (is (str/includes? merged (pm/marker-id 4)))
    (is (str/includes? merged "<p>d</p>"))))

;; ── Split: the four invariants ──────────────────────────────────────────────

(deftest split-strips-every-marker-from-every-slice
  (testing "item 3.1 — a marker in a stored slice would reach content_text,
            the card-generation prompt, a search snippet and KG extraction"
    (let [slices (pm/split-merged (pm/merge-pages pages) nums)]
      (doseq [[n html] slices]
        (testing (str "page " n)
          (is (not (str/includes? html pm/marker-id-prefix)))
          (is (not (str/includes? html "<h6")))
          (is (not (str/includes? html "Page "))))))))

(deftest split-reports-every-page-even-when-its-marker-is-gone
  (testing "items 4.1 and 4.2 — a page absent from the split is a page a caller
            could be tempted to delete, which orphans its cards"
    (let [merged (str/replace (pm/merge-pages pages) (pm/marker-html 2) "")
          slices (pm/split-merged merged nums)]
      (is (= #{1 2 3} (set (keys slices))))
      (is (= "" (get slices 2))))))

(deftest split-absorbs-an-orphaned-slice-into-the-page-above
  (let [merged (str/replace (pm/merge-pages pages) (pm/marker-html 2) "")
        slices (pm/split-merged merged nums)]
    (testing "page 2's text joins page 1"
      (is (str/includes? (get slices 1) "alpha"))
      (is (str/includes? (get slices 1) "beta")))
    (testing "page 2 comes back empty, not missing"
      (is (= "" (get slices 2))))
    (testing "page 3 is untouched — the absorb is local"
      (is (str/includes? (get slices 3) "gamma"))
      (is (not (str/includes? (get slices 3) "beta"))))))

(deftest no-page-gains-text-that-sat-below-its-own-marker
  (testing "the absorb-upward invariant, swept over every single-marker loss"
    (doseq [lost nums]
      (testing (str "marker " lost " deleted")
        (let [merged (str/replace (pm/merge-pages pages) (pm/marker-html lost) "")
              slices (pm/split-merged merged nums)
              text-of {1 "alpha" 2 "beta" 3 "gamma"}]
          (doseq [[page html] slices
                  [below word] text-of
                  :when (> below page)]
            ;; `below` sits under `page`'s marker only when every marker
            ;; between them survived. The one exception is the absorbed page.
            (when-not (and (= below lost) (= page (dec lost)))
              (is (not (str/includes? html word))
                (str "page " page " gained page " below "'s text")))))))))

(deftest content-above-the-first-marker-joins-the-lowest-page
  (testing "there is no earlier page to absorb it, and dropping it would lose
            the reader's text"
    (let [merged (str "<p>preamble</p>" (pm/merge-pages pages))
          slices (pm/split-merged merged nums)]
      (is (str/includes? (get slices 1) "preamble")))))

(deftest split-drops-a-marker-for-a-page-the-caller-did-not-ask-for
  (testing "an unknown page can only come from a paste or a hand edit, and
            honouring it would write a page row that does not exist"
    (let [merged (str (pm/merge-pages pages) (pm/marker-html 99) "<p>pasted</p>")
          slices (pm/split-merged merged nums)]
      (is (= #{1 2 3} (set (keys slices))))
      (testing "its text stays with the page that was open"
        (is (str/includes? (get slices 3) "pasted"))))))

(deftest split-of-an-untouched-merge-preserves-every-page-body
  (let [slices (pm/split-merged (pm/merge-pages pages) nums)]
    (is (str/includes? (get slices 1) "alpha"))
    (is (str/includes? (get slices 2) "beta"))
    (is (str/includes? (get slices 3) "gamma"))))

(deftest split-handles-an-empty-and-a-nil-document
  (doseq [doc ["" nil]]
    (testing (pr-str doc)
      (let [slices (pm/split-merged doc nums)]
        (is (= #{1 2 3} (set (keys slices))))
        (is (every? #{""} (vals slices)))))))

(deftest split-inserts-no-whitespace-of-its-own
  (testing "pretty-printed output would add newlines Quill renders as text"
    (let [slices (pm/split-merged (pm/merge-pages pages) nums)]
      (is (= "<p>alpha</p>" (get slices 1))))))

;; ── The restore rule ────────────────────────────────────────────────────────

(deftest restore-answers-the-lowest-missing-page
  (let [all [1 2 3 4 5 6 7 8 9 10]]
    (testing "markers 7 and 9 present, a point inside 7 answers 8"
      (is (= 8 (pm/next-missing-page (disj (set all) 8) 7 all))))
    (testing "markers 7 and 10 present, it answers 8, then 9 after 8 returns"
      (let [present (disj (set all) 8 9)]
        (is (= 8 (pm/next-missing-page present 7 all)))
        (is (= 9 (pm/next-missing-page (conj present 8) 7 all)))))
    (testing "a point above every marker uses 0 as the floor"
      (is (= 1 (pm/next-missing-page (disj (set all) 1) nil all))))))

(deftest restore-refuses-when-nothing-is-missing
  (testing "nil is the refusal signal — this control never creates a page"
    (let [all [1 2 3]]
      (is (nil? (pm/next-missing-page (set all) 1 all)))
      (is (nil? (pm/next-missing-page (set all) nil all)))
      (testing "and refuses past the last page even with gaps below"
        (is (nil? (pm/next-missing-page #{1 3} 3 all)))))))

(deftest restore-never-answers-a-page-the-document-does-not-have
  (testing "the answer is bounded by `all`, so it cannot mint a page number"
    (is (nil? (pm/next-missing-page #{1 2} 2 [1 2])))
    (is (= 2 (pm/next-missing-page #{1 3} 1 [1 2 3])))))
