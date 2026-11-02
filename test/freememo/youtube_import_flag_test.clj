(ns freememo.youtube-import-flag-test
  "A YouTube import whose oEmbed answered 401 stores the embedding caveat on
   the topic (plans/remove-toasts.md 2.11.1). It used to be a toast, so a
   reload lost it. Needs no database: the writers are stubbed."
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.commands :as commands]
            [freememo.db :as db]
            [freememo.youtube-import :as yt]))

(defn- import-with [oembed]
  (let [created (atom nil)]
    (with-redefs [db/find-youtube-root (fn [_ _] nil)
                  yt/fetch-oembed (fn [_] oembed)
                  db/create-youtube-topic! (fn [_ opts] (reset! created opts)
                                             {:topic-id 7 :title "T"})
                  commands/bump! (fn [& _] nil)]
      {:result (yt/import! 1 {:video-id "dQw4w9WgXcQ" :start-ms nil})
       :created @created})))

(deftest a-401-sets-the-flag-on-the-new-topic
  (let [{:keys [result created]} (import-with {:status :unauthorized})]
    (is (true? (:embed-warning? created)))
    (testing "the result carries no warning text"
      (is (not (contains? result :warning)))
      (is (= :imported (:flow result))))))

(deftest other-answers-leave-the-flag-clear
  (doseq [oembed [{:status :ok :title "A talk" :channel "C"} {:status :unavailable}]]
    (is (false? (:embed-warning? (:created (import-with oembed)))) (str oembed))))
