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
