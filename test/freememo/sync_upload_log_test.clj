(ns freememo.sync-upload-log-test
  "A finished desktop upload writes one info line that places it in Loki:
   the user, the rows per table and the id range. It holds no row content."
  (:require [clojure.test :refer [deftest is]]
            [freememo.sync-server :as sync-server]
            [taoensso.telemere :as tel]))

(def ^:private rows
  [{"t" "assistant_chats" "k" {"id" 1099511627800} "r" {"title" "secret title"}}
   {"t" "assistant_messages" "k" {"id" 1099511627801} "r" {"content" "secret text"}}
   {"t" "assistant_messages" "k" {"id" 1099511627805} "r" {"content" "more"}}])

(deftest an-upload-writes-one-info-line-with-counts-and-id-range
  (let [{:keys [signals]} (tel/with-signals (sync-server/log-upload! 7 rows []))
        line (first (filter #(= ::sync-server/sync-upload (:id %)) signals))]
    (is (= 1 (count signals)))
    (is (= :info (:level line)))
    (is (= {:user-id 7
            :counts {"assistant_chats" 1 "assistant_messages" 2}
            :min-id 1099511627800
            :max-id 1099511627805
            :rejected 0}
           (:data line)))
    (is (not (re-find #"secret" (pr-str (:data line)))))))

(deftest the-line-counts-rejected-rows-and-says-so
  (let [{:keys [signals]} (tel/with-signals (sync-server/log-upload! 7 rows [{"t" "assistant_chats"}]))
        line (first (filter #(= ::sync-server/sync-upload (:id %)) signals))]
    (is (= 1 (get-in line [:data :rejected])))
    (is (re-find #"1 rejected" (str (:msg_ line))))
    (is (not (re-find #"applied" (str (:msg_ line)))))))

(deftest a-full-upload-writes-the-same-line
  (let [{:keys [signals]} (tel/with-signals (sync-server/log-upload! 7 [] []))
        line (first (filter #(= ::sync-server/sync-upload (:id %)) signals))]
    (is (= {:user-id 7 :counts {} :min-id nil :max-id nil :rejected 0} (:data line)))))
