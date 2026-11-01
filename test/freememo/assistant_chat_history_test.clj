(ns freememo.assistant-chat-history-test
  "The history view's pure helpers: date sections, search, relative time
   (plans/assistant-chat-history.md §3.3)."
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.assistant-chat-history :as history]))

(def ^:private zone (java.time.ZoneId/systemDefault))

(defn- local-ms
  "Epoch ms of a local wall-clock time `days-ago` days before today."
  [days-ago hh mm]
  (-> (java.time.LocalDate/now zone) (.minusDays days-ago)
      (.atTime hh mm) (.atZone zone) (.toInstant) (.toEpochMilli)))

(defn- ids [chats] (mapv :id chats))

(deftest chats-fall-into-local-calendar-sections
  (let [now (local-ms 0 12 0)
        chats [{:id 1 :label "Today early" :activity-ms (local-ms 0 0 30)}
               {:id 2 :label "Yesterday late" :activity-ms (local-ms 1 23 30)}
               {:id 3 :label "A week ago" :activity-ms (local-ms 7 0 30)}
               {:id 4 :label "Eight days ago" :activity-ms (local-ms 8 23 0)}
               {:id 5 :label "Pinned first" :activity-ms (local-ms 0 9 0) :pinned-ms 100}
               {:id 6 :label "Pinned later" :activity-ms (local-ms 30 9 0) :pinned-ms 200}]
        sections (history/history-sections chats "" now)]
    (testing "3.3.2: 00:30 today is Today; 23:30 yesterday is Previous 7 days"
      (is (= [1] (ids (:today sections))))
      (is (= [2 3] (ids (:week sections))))
      (is (= [4] (ids (:older sections)))))
    (testing "3.3.3: a pinned chat is in Pinned only, most recently pinned first"
      (is (= [6 5] (ids (:pinned sections)))))))

(deftest search-filters-within-sections
  (let [now (local-ms 0 12 0)
        chats [{:id 1 :label "Sorting arrays" :activity-ms (local-ms 0 10 0)}
               {:id 2 :label "Heaps" :activity-ms (local-ms 0 9 0)}
               {:id 3 :label "Quick SORT" :activity-ms (local-ms 3 9 0) :pinned-ms 1}]
        sections (history/history-sections chats " sort " now)]
    (is (= [1] (ids (:today sections))))
    (is (= [3] (ids (:pinned sections))))
    (is (empty? (:week sections)))))

(deftest relative-time-reads-compactly
  (let [now 1000000000000
        ago (fn [ms] (history/relative-time-label now (- now ms)))]
    (is (= "now" (ago 30000)))
    (is (= "5m" (ago (* 5 60000))))
    (is (= "3h" (ago (* 3 3600000))))
    (is (= "2d" (ago (* 2 86400000))))
    (is (= "2w" (ago (* 14 86400000))))
    (is (= "2mo" (ago (* 61 86400000))))
    (is (= "1y" (ago (* 400 86400000))))))

(deftest history-window-order-keeps-ancestors-outside-the-old-page
  (let [now (local-ms 0 12 0)
        root {:id 1 :label "Root" :activity-ms (local-ms 30 9 0)}
        children (mapv (fn [i] {:id (+ 2 i) :parent-id 1 :fork-message-id i
                                :label (str "Branch " i) :activity-ms now})
                   (range 200))
        chats (into [root] children)
        all (history/history-projection chats "" now)
        matched (history/history-projection chats "Branch 190" now)]
    (is (= 201 (count all)))
    (is (= (vec (range 201)) (mapv :history/index all)))
    (is (= [1 192] (ids matched)))
    (is (= [0 1] (mapv :depth matched)))
    (is (= [:older :older] (mapv :history/section matched)))
    (is (= "Older" (:history/section-label (first matched))))
    (is (nil? (:history/section-label (second matched))))
    (is (= [[:corner]] (mapv :guides (rest matched))))))

(deftest server-projection-uses-the-client-calendar-boundaries
  (let [chats [{:id 1 :label "Client today" :activity-ms 1200}
               {:id 2 :label "Client week" :activity-ms 800}
               {:id 3 :label "Client older" :activity-ms 200}]
        rows (history/history-projection chats "" 2000 [1000 500])]
    (is (= [:today :week :older] (mapv :history/section rows)))
    (is (= ["Today" "Previous 7 days" "Older"] (mapv :history/section-label rows)))))

(deftest browse-order-stays-stable-while-row-metadata-updates
  (let [initial [{:id 1 :label "First" :activity-ms 1500}
                 {:id 2 :label "Second" :activity-ms 1400}]
        order (history/history-browse-order initial)
        current [{:id 2 :label "Second renamed" :activity-ms 9000 :pinned-ms 9000}
                 {:id 1 :label "First" :activity-ms 1500}]
        rows (history/frozen-history-projection current "" 2000 [1000 500] order)]
    (is (= [1 2] (ids rows)))
    (is (= [:today :today] (mapv :history/section rows)))
    (is (= "Second renamed" (:label (second rows))))
    (is (= 9000 (:pinned-ms (second rows))))
    (is (= 9000 (:activity-ms (second rows))))
    (is (= [2 1] (ids (history/frozen-history-projection current "" 2000 [1000 500]
                       (history/history-browse-order current)))))))

(deftest browse-generation-includes-new-forks-and-removes-deleted-chats
  (let [initial [{:id 1 :label "Root" :activity-ms 1500}
                 {:id 2 :label "Deleted" :activity-ms 1400}]
        order (history/history-browse-order initial)
        current [{:id 1 :label "Root" :activity-ms 1500}
                 {:id 3 :label "New branch" :activity-ms 1900 :parent-id 1 :fork-message-id 5}
                 {:id 4 :label "New root" :activity-ms 1900}]
        rows (history/frozen-history-projection current "" 2000 [1000 500] order)
        search (history/frozen-history-projection current "New branch" 2000 [1000 500] order)]
    (is (= [1 3] (ids rows)))
    (is (= [1 3] (ids search)))
    (is (= [0 1] (mapv :depth search)))
    (is (= [:today :today] (mapv :history/section search)))
    (is (= [1 3 4] (ids (history/frozen-history-projection current "" 2000 [1000 500]
                         (history/history-browse-order current)))))))

(deftest history-window-reads-only-requested-owned-rows
  (let [rows (mapv #(hash-map :id % :history/index %) (range 1000))
        result {:scope [7 "needle"] :rows rows}]
    (is (= [0 543 999] (ids (history/history-window-rows result 7 "needle" [0 543 999]))))
    (is (= [] (history/history-window-rows result 7 "needle" [])))
    (is (= [] (history/history-window-rows result 8 "needle" [0 543 999])))
    (is (= [] (history/history-window-rows result 7 "other" [0 543 999])))
    (is (= [999] (ids (history/history-window-rows result 7 "needle" [999 1000]))))))
