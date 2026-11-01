(ns freememo.library-snapshot-test
  (:require [clojure.test :refer [deftest is]]
            [freememo.library-cards :as library]
            [freememo.db :as db]
            [freememo.user-state :as us]))

(def defaults {:text "" :kinds #{} :origins #{} :flags #{} :overlay-ids {}
               :sort-col :added :sort-dir :desc})

(defn card [id title text]
  {:flashcards/id id :flashcards/kind "basic" :flashcards/topic_id 10
   :flashcards/root_topic_id 10 :flashcards/question (str "<p>" text "</p>")
   :flashcards/answer "<p>Answer</p>" :flashcards/created_at (java.sql.Timestamp. (* id 1000))
   :root_title title :topic_title "Page"})

(def data [(card 1 "Atlas" "Alpha") (card 2 "Zebra" "Beta")
           (assoc (card 3 "Masks" "") :flashcards/kind "occlusion" :flashcards/occlusion_group_id 99)
           (assoc (card 4 "Masks" "") :flashcards/kind "occlusion" :flashcards/occlusion_group_id 99)])

(deftest repeated-filters-reuse-one-owned-snapshot
  (let [calls (atom [])]
    (with-redefs [db/get-user-flashcards (fn [uid] (swap! calls conj uid) data)]
      (let [snapshot (library/load-user-cards* [0 0 0 0] 7001)]
        (doseq [_ (range 10)
                [query count] [["Alpha" 1] ["Atlas" 1] ["unfindable" 0] ["" 3]]]
          (is (= count (:count (library/query-loaded-cards* snapshot 7001 (assoc defaults :text query))))))
        (is (= [7001] @calls))))))

(deftest overlay-facets-retain-every-member-of-a-matching-group
  (with-redefs [db/get-user-flashcards (constantly data)]
    (let [snapshot (library/load-user-cards* [0 0 0 0] 7002)
          result (library/query-loaded-cards* snapshot 7002
                   (assoc defaults :flags #{"marked"} :overlay-ids {:marked #{4}}))]
      (is (= 1 (:count result)))
      (is (= 2 (:card-count result)))
      (is (= [3 4] (:member-ids (first (:cards result)))))
      (is (= [3 4] (second (first (:filtered-rows result))))))))

(deftest each-existing-mutation-channel-can-replace-its-snapshot
  (let [rows (atom [(card 1 "Original title" "Original body")]) calls (atom 0)
        revision [0 0 0 0]]
    (with-redefs [db/get-user-flashcards (fn [_] (swap! calls inc) @rows)]
      (let [initial (library/load-user-cards* revision 7003)]
        (doseq [[slot replacement] [[0 [(card 2 "Refresh title" "Refresh body")]]
                                  [1 [(card 3 "Edited title" "Edited body")]]
                                  [2 [(assoc (card 4 "Synced title" "Synced body") :flashcards/anki_note_id 9004)]]
                                  [3 [(card 5 "Renamed title" "Renamed body")]]]]
          (reset! rows replacement)
          (let [next (library/load-user-cards* (assoc revision slot 1) 7003)]
            (is (= (:flashcards/id (first replacement))
                  (:flashcards/id (first (:cards (library/query-loaded-cards* next 7003 defaults))))))))
        (is (= 1 (:flashcards/id (first (:cards (library/query-loaded-cards* initial 7003 defaults))))))
        (is (= 5 @calls))))))

(deftest only-the-window-builds-display-html
  (with-redefs [db/get-user-flashcards (constantly (mapv #(card % "Document" (str "Question " %)) (range 100000)))]
    (let [snapshot (library/load-user-cards* [0 0 0 0] 7004)
          result (library/query-loaded-cards* snapshot 7004 defaults)
          window (library/window-card-summaries*
                   (library/window-cards (:cards result) (vec (range 100000)) 40000 15 #{}))]
      (is (= 100000 (:count result)))
      (is (= 15 (count window)))
      (is (every? :front-html window))
      (is (not-any? #(contains? % :members) window))
      (is (not-any? #(contains? % :search-text) window))
      (is (not-any? #(contains? % :front-html) (:cards result))))))

(deftest two-users-and-two-components-have-independent-snapshots
  (let [calls (atom [])]
    (with-redefs [db/get-user-flashcards (fn [uid]
                                        (swap! calls conj uid)
                                        [(card uid (str "User " uid) "Owned")])]
      (let [a (library/load-user-cards* [0 0 0 0] 7005)
            b (library/load-user-cards* [0 0 0 0] 7006)
            again (library/load-user-cards* [0 0 0 0] 7005)]
        (is (= [7005 7006 7005] @calls))
        (is (= 7005 (:flashcards/id (first (:cards (library/query-loaded-cards* a 7005 defaults))))))
        (is (= 7006 (:flashcards/id (first (:cards (library/query-loaded-cards* b 7006 defaults))))))
        (is (not (identical? a again)))))))
