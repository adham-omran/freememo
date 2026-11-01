(ns freememo.schedule-history-test
  (:require [clojure.test :refer [deftest is]]
            [missionary.core :as m]
            [freememo.schedule-history :as history]
            [freememo.schedule-fsrs :as sf]
            [freememo.fsrs :as fsrs]
            [freememo.anki-bounded-pipeline-test :refer [await!]]))

(deftest whole-history-transport-is-bounded-at-a-million-reviews
  (let [read-sizes (atom []) upload-sizes (atom []) final-cards (atom #{})
        infos (mapv (fn [id] {:cardId id}) (range 1000))
        result (await!
                 (history/read-history!
                   (fn [_ {:keys [cards]}]
                     (m/sp
                       (swap! read-sizes conj (count cards))
                       (into {} (map (fn [id]
                                       [id (mapv (fn [n] {:id n :ease 3}) (range 1000))])) cards)))
                   (fn [{:keys [info reviews initial final?]}]
                     (m/sp
                       (swap! upload-sizes conj (count reviews))
                       (when final? (swap! final-cards conj (:cardId info)))
                       {:ok true :state {:reps (+ (or (:reps initial) 0) (count reviews))}}))
                   {} infos))]
    (is (= 1000000 (:review-count result)))
    (is (= 40 (count @read-sizes)))
    (is (= 1000 (count @upload-sizes)))
    (is (every? #(<= % 25) @read-sizes))
    (is (every? #(<= % 1000) @upload-sizes))
    (is (= 1000 (count @final-cards)))))

(deftest chunked-replay-matches-a-fresh-complete-replay
  (let [scheduler (sf/scheduler-from-deck-config {:fsrsParams6 fsrs/default-parameters
                                                :desiredRetention 0.9})
        rows (mapv (fn [n] {:id (+ 1700000000000 (* n sf/ms-per-day))
                            :ease (inc (mod n 4))}) (range 120))
        whole (sf/replay scheduler (sf/filtered-revlog rows 0))
        pieces (reduce (fn [state chunk] (sf/replay scheduler (sf/filtered-revlog chunk 0) state))
                 nil (partition-all 7 rows))]
    (is (= whole pieces))))

(deftest a-card-without-reviews-still-commits-a-final-empty-chunk
  (let [seen (atom [])]
    (await! (history/read-history! (fn [& _] (m/sp {}))
              (fn [data] (m/sp (swap! seen conj data) {:ok true :state nil}))
              {} [{:cardId 5}]))
    (is (= 1 (count @seen)))
    (is (true? (:final? (first @seen))))
    (is (= [] (:reviews (first @seen))))))
