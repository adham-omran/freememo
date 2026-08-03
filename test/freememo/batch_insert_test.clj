(ns freememo.batch-insert-test
  "Guards on the parameter ceiling for multi-row INSERTs.

   A 1.7 GB SuperMemo collection produced one `insert-topic-rows!` call whose
   rendered statement bound 148 542 parameters, and PostgreSQL refuses a
   prepared statement past 65 535 — so the whole import rolled back at the last
   step. These tests assert the property that failure violated: every statement
   `param-bounded-batches` hands to the driver stays under the ceiling, and
   partitioning changes neither the row order nor the emitted column list.

   Pure — no database. What is under test is the arithmetic and the HoneySQL
   render, which is exactly where the defect lived."
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.db :as db]
            [honey.sql :as sql]))

(def ^:private batches #'db/param-bounded-batches)
(def ^:private ceiling @#'db/max-statement-params)

;; ── Fixtures ───────────────────────────────────────────────────────

(defn- topic-row
  "A topic row at the width the SuperMemo importer actually builds: every
   column populated, because a nil renders as inline NULL and therefore costs
   no parameter — the all-non-nil row is the worst case."
  [i]
  {:user_id 17 :kind "supermemo" :title (str "Element " i) :status "active"
   :priority 50 :sm_rank i :sm_element_id i :interval_days 1.0 :a_factor 1.2
   :next_review_at "2026-08-03" :last_review_at "2026-08-03" :parent_id 1
   :content "<p>x</p>" :content_text "x" :source_id 1})

(defn- render
  "Format one batch the way the insert fns do; return the rendered vector."
  [batch]
  (sql/format {:insert-into :topics :values (vec batch) :returning [:id]}))

(defn- param-count [batch] (dec (count (render batch))))

(defn- column-clause [batch] (re-find #"\(.*?\)" (first (render batch))))

;; ── The regression ─────────────────────────────────────────────────

(deftest unpartitioned-render-exceeds-the-ceiling
  (testing "the failing shape really does breach the ceiling unbatched —
            otherwise the rest of this file guards nothing"
    (let [rows (mapv topic-row (range 10000))]
      (is (> (param-count rows) ceiling)
        "10k full-width topic rows must exceed 65 535 params in one statement"))))

(deftest every-batch-fits-under-the-ceiling
  (testing "each partition renders a statement the driver will accept"
    (doseq [n [1 2 4369 4370 10000 148542]]
      (let [rows (mapv topic-row (range n))]
        (doseq [b (batches rows)]
          (is (<= (param-count b) ceiling)
            (str "batch from " n " rows bound " (param-count b) " params")))))))

(deftest degenerate-width-still-yields-nonempty-batches
  (testing "a row wider than the ceiling gets its own batch, not a zero-sized
            partition — the driver must report the real problem, not this fn hang"
    (let [wide (zipmap (map #(keyword (str "c" %)) (range (+ ceiling 10))) (repeat 1))
          out (batches [wide wide])]
      (is (= 2 (count out)))
      (is (every? #(= 1 (count %)) out)))))

;; ── Properties partitioning must not change ────────────────────────

(deftest order-is-preserved
  (testing "callers align returned ids positionally against the rows passed"
    (let [rows (mapv topic-row (range 10000))
          flat (vec (apply concat (batches rows)))]
      (is (= (count rows) (count flat)))
      (is (= (mapv :sm_element_id rows) (mapv :sm_element_id flat))))))

(deftest small-lists-stay-a-single-statement
  (testing "the common path — card generation inserts ~10 rows — is unchanged"
    (is (= 1 (count (batches (mapv topic-row (range 10))))))))

(deftest column-list-is-identical-across-batches
  (testing "a key present in one batch and absent from another would be an
            explicit NULL in the first and a column DEFAULT in the second"
    (let [rows (into (mapv #(assoc (topic-row %) :sm_rank %) (range 5000))
                 ;; the tail omits two keys the head carries
                 (mapv #(dissoc (topic-row %) :content :source_id) (range 5000 10000)))
          clauses (distinct (map column-clause (batches rows)))]
      (is (= 1 (count clauses))
        "every batch must emit the same column list"))))

(deftest missing-keys-render-as-null-not-default
  (testing "normalization must reproduce what the unpartitioned statement did"
    (let [rows [{:a 1 :b 2} {:a 3}]
          only-batch (first (batches rows))
          [sql-str & params] (render only-batch)]
      (is (= 1 (count (batches rows))))
      (is (re-find #"NULL" sql-str)
        "the absent :b must be an inline NULL")
      (is (not (re-find #"DEFAULT" sql-str))
        "and must never become a column DEFAULT")
      (is (= [1 2 3] params)
        "only the three non-nil cells are bound as parameters"))))
