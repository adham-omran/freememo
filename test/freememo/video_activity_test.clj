(ns freememo.video-activity-test
  "A failed `process-video!` run must close its Activity row as failed.

   The reporter reads `registry/work-error` when it sees a topic leave
   `:processing-videos`, so the error has to be recorded before membership
   drops. Each test stubs `process-video!`, drives the real `start-processing!`,
   and reads the registry at the first moment membership is gone.

   Needs no database."
  (:require
   [clojure.test :refer [deftest is testing]]
   [freememo.activity-registry :as registry]
   [freememo.user-state :as us]
   [freememo.video :as video]))

(defn- run-to-end!
  "Start processing with `process-video!` stubbed by `stub`, and wait for
   membership to drop. Returns what `work-error` read at that moment."
  [user-id topic-id stub]
  (let [!in-flight (us/get-atom user-id :processing-videos)
        seen (promise)]
    (add-watch !in-flight ::end
      (fn [_ _ old new]
        (when (and (contains? old topic-id) (not (contains? new topic-id)))
          (deliver seen (registry/work-error "video-process" user-id (str topic-id))))))
    (try
      (with-redefs [video/process-video! stub]
        (video/start-processing! user-id topic-id true)
        (deref seen 5000 ::timeout))
      (finally
        (remove-watch !in-flight ::end)))))

(deftest a-failed-run-closes-as-failed
  (testing "a {:success false} result"
    (is (= "ffmpeg: no audio stream"
           (run-to-end! 987650001 11
             (fn [_ _ _] {:success false :error "ffmpeg: no audio stream" :stage :extract})))))
  (testing "a thrown exception"
    (is (= "boom"
           (run-to-end! 987650002 12
             (fn [_ _ _] (throw (RuntimeException. "boom"))))))))

(deftest a-successful-run-closes-as-success
  (testing "a transcribed run"
    (is (nil? (run-to-end! 987650003 13
                (fn [_ _ _] {:success true :segments 3 :transcribed true})))))
  (testing "a skipped transcription"
    (is (nil? (run-to-end! 987650004 14
                (fn [_ _ _] {:success true :segments 0 :transcribed false}))))))

(deftest a-new-run-clears-the-previous-error
  (let [user-id 987650005 topic-id 15]
    (is (= "first"
           (run-to-end! user-id topic-id
             (fn [_ _ _] {:success false :error "first" :stage :extract}))))
    (is (nil? (run-to-end! user-id topic-id
                (fn [_ _ _] {:success true :segments 1 :transcribed true}))))))
