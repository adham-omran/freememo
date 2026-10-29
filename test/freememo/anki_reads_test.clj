(ns freememo.anki-reads-test
  (:require [clojure.test :refer [deftest is]]
            [freememo.anki-reads :as reads]
            [missionary.core :as m]))

(defn- await-task [task]
  (let [result (promise)
        cancel (task #(deliver result [:ok %]) #(deliver result [:error %]))
        [status value] (deref result 5000 [:timeout nil])]
    (case status
      :ok value
      :error (throw value)
      (do (cancel) (throw (ex-info "The Anki read test timed out." {}))))))

(deftest collection-sized-read-keeps-missing-note-positions
  (let [calls (atom [])
        ids (vec (range 30000))
        result (await-task
                 (reads/read-notes!
                   (fn [action {:keys [notes]}]
                     (m/sp
                       (swap! calls conj [action notes])
                       (mapv #(if (= % 1234) {} {:noteId %}) notes)))
                   ids))]
    (is (= 60 (count @calls)))
    (is (every? #(<= (count (second %)) 500) @calls))
    (is (= ids (vec (mapcat second @calls))))
    (is (= 30000 (count result)))
    (is (= {} (nth result 1234)))
    (is (= {:noteId 29999} (peek result)))))

(deftest malformed-response-stops-the-next-request
  (let [calls (atom 0)]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"incomplete"
          (await-task
            (reads/read-notes! (fn [& _] (m/sp (swap! calls inc) [])) (range 1001)))))
    (is (= 1 @calls))))

(deftest empty-manifest-makes-no-request
  (is (= [] (await-task (reads/read-notes! (fn [& _] (throw (Exception. "Unexpected request"))) [])))))

(deftest cancellation-does-not-start-the-next-chunk
  (let [calls (atom 0)
        started (promise)
        done (promise)
        task (reads/read-notes!
               (fn [& _]
                 (m/sp
                   (swap! calls inc)
                   (deliver started true)
                   (m/? (m/sleep 60000))))
               (range 1001))
        cancel (task #(deliver done [:ok %]) #(deliver done [:error %]))]
    (is (= true (deref started 1000 false)))
    (cancel)
    (is (= :error (first (deref done 1000 [:timeout]))))
    (is (= 1 @calls))))
