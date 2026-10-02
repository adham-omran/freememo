(ns freememo.reading-anchor-test
  "plans/reading-position-memory.md §4.2: the anchor model, with no DOM."
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.reading-anchor :as ra]))

(defn- blocks
  "Blocks of `heights` stacked from `pad`, with texts `texts`."
  [pad heights texts]
  (let [tops (reductions + pad heights)]
    (mapv (fn [top h t] {:top top :height h :text t}) tops heights texts)))

(def ^:private doc
  ;; Six blocks of 100 px from a 10 px pad: tops 10, 110, …, 510.
  (blocks 10 (repeat 6 100) ["Alpha" "Beta" "Gamma" "Delta" "Epsilon" "Zeta"]))

(deftest fingerprint-collapses-whitespace
  (is (= "a b c" (ra/fingerprint "  a \n\t b  c  ")))
  (is (= (ra/fingerprint "one  two") (ra/fingerprint "one\ntwo")))
  (is (= "" (ra/fingerprint nil)))
  (is (= ra/fingerprint-chars (count (ra/fingerprint (apply str (repeat 200 "x")))))))

(deftest capture-at-the-top-the-middle-and-the-end
  (testing "the top"
    (is (= {:anchor {:i 0 :fp "Alpha" :frac 0.0} :fraction 0.0}
          (ra/capture doc 0 300 610))))
  (testing "the middle: 260 is half way into Gamma"
    (let [{:keys [anchor fraction]} (ra/capture doc 260 300 610)]
      (is (= {:i 2 :fp "Gamma" :frac 0.5} anchor))
      (is (= (double (/ 260 310)) fraction))))
  (testing "a block bottom exactly at the top edge belongs above it"
    (is (= 1 (get-in (ra/capture doc 110 300 610) [:anchor :i]))))
  (testing "the end"
    (let [{:keys [anchor fraction]} (ra/capture doc 310 300 610)]
      (is (= 3 (:i anchor)))
      (is (= 1.0 fraction)))))

(deftest above-the-first-block-restores-to-the-top
  (let [{:keys [anchor]} (ra/capture doc 4 300 610)]
    (is (= {:i 0 :fp "Alpha" :frac 0.0} anchor))
    (is (= 0 (ra/target-scroll-top doc 0 (:frac anchor))))))

(deftest capture-of-no-blocks-is-nil
  (is (nil? (ra/capture [] 0 300 300))))

(deftest resolve-index-finds-the-block-after-an-edit-above
  (let [edited (blocks 10 (repeat 7 100)
                 ["New" "Alpha" "Beta" "Gamma" "Delta" "Epsilon" "Zeta"])]
    (is (= 3 (ra/resolve-index edited {:i 2 :fp "Gamma" :frac 0.5})))))

(deftest resolve-index-falls-back-to-the-index-when-the-block-is-gone
  (let [deleted (blocks 10 (repeat 5 100) ["Alpha" "Beta" "Delta" "Epsilon" "Zeta"])]
    (is (= 2 (ra/resolve-index deleted {:i 2 :fp "Gamma" :frac 0.5})))))

(deftest resolve-index-prefers-the-nearest-match-and-the-lower-on-a-tie
  (let [empties (blocks 0 (repeat 7 10) ["" "x" "" "y" "" "z" ""])]
    (is (= 2 (ra/resolve-index empties {:i 2 :fp ""})))
    (is (= 2 (ra/resolve-index empties {:i 3 :fp ""})) "2 and 4 tie, so 2")
    (is (= 6 (ra/resolve-index empties {:i 6 :fp ""})))))

(deftest resolve-index-clamps-an-index-past-the-end
  (is (= 5 (ra/resolve-index doc {:i 40 :fp "gone"})))
  (is (= 5 (ra/resolve-index doc {:i 40 :fp "Zeta"}))))

(deftest resolve-index-of-no-blocks-is-nil
  (is (nil? (ra/resolve-index [] {:i 0 :fp "Alpha"}))))

(deftest capture-then-restore-round-trips
  (doseq [scroll-top [0 11 37 110 260 333 509]]
    (let [{:keys [anchor]} (ra/capture doc scroll-top 100 610)
          i (ra/resolve-index doc anchor)]
      (is (< (abs (- scroll-top (ra/target-scroll-top doc i (:frac anchor)))) 1)
        (str "scroll-top " scroll-top)))))

(deftest first-visible-index-agrees-with-a-linear-scan
  (let [bottoms (mapv #(+ (:top %) (:height %)) doc)]
    (doseq [y (range -5 700 7)]
      (is (= (or (first (keep-indexed (fn [i b] (when (> b y) i)) bottoms)) 5)
            (ra/first-visible-index 6 bottoms y))
        (str "y " y)))))

(deftest valid-anchor-rejects-what-capture-cannot-make
  (is (ra/valid-anchor? {:i 0 :fp "" :frac 0} 0))
  (is (ra/valid-anchor? {:i 3 :fp "Gamma" :frac 0.5} 0.8))
  (is (not (ra/valid-anchor? {:i -1 :fp "" :frac 0} 0)))
  (is (not (ra/valid-anchor? {:i 0 :fp (apply str (repeat 65 "x")) :frac 0} 0)))
  (is (not (ra/valid-anchor? {:i 0 :fp "" :frac 1.5} 0)))
  (is (not (ra/valid-anchor? {:i 0 :fp "" :frac 0} 2)))
  (is (not (ra/valid-anchor? {:i 0 :fp nil :frac 0} 0))))
