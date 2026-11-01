(ns freememo.hierarchy-side-panel-test
  "The viewer's hierarchy panel reads the whole root on every topic open and
   every expand click. With `t.*`, that read loaded every topic's `content` and
   `content_text`. On a 59,000-topic SuperMemo root, the container passed its
   3 GiB limit and the kernel killed the JVM (502s on 2026-10-31).

   These tests keep the read narrow, and keep the narrow list complete."
  (:require [clojure.set :as set]
            [clojure.test :refer [deftest is testing]]
            [next.jdbc :as jdbc]
            [freememo.db :as db]
            [freememo.hierarchy-side-panel :as hsp]))

(defn- db-up? []
  (try (jdbc/execute-one! db/ds ["SELECT 1"]) true (catch Exception _ false)))

(deftest the-subtree-read-names-its-columns
  (let [!columns (atom ::not-called)]
    (with-redefs [db/get-root-topic-id (constantly 1)
                  db/get-topic (constantly {:topics/id 1 :topics/kind "basic"
                                            :topics/content "<p>big</p>"})
                  db/get-subtree (fn
                                   ([_ _] (reset! !columns nil) [])
                                   ([_ _ columns] (reset! !columns columns) []))]
      (hsp/build-hierarchy-rows* 0 7 1 #{}))
    (testing "the panel passes a column list, so the query is not t.*"
      (is (seq @!columns)))
    (testing "the list holds no content column"
      (is (empty? (set/intersection (set @!columns) #{:content :content_text}))))))

(deftest every-topic-key-the-panel-reads-is-selected
  ;; A key read in the panel but missing from the list is nil on every row,
  ;; with no error. Content keys appear only in slim-topic, which drops them.
  (let [src (slurp "src/freememo/hierarchy_side_panel.cljc")
        read-keys (->> (re-seq #":topics/([a-z_]+)" src)
                       (map (comp keyword second))
                       set)]
    (is (= #{}
           (set/difference read-keys
                           (set hsp/panel-topic-columns)
                           #{:content :content_text})))))

(deftest the-columns-exist-in-the-schema
  (if-not (db-up?)
    (println "SKIP the-columns-exist-in-the-schema: no database")
    (is (= [] (db/get-subtree -1 -1 hsp/panel-topic-columns)))))
