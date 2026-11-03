(ns freememo.comprehension-ocr-start-test
  "Checks the transcription that an answer upload starts. No database is used."
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.commands :as commands]
            [freememo.comprehension-ocr :as scan]))

(defn- bumps []
  (let [!bumps (promise)]
    [!bumps (fn [user-id channels] (deliver !bumps [user-id channels]) :done)]))

(deftest the-upload-response-waits-for-the-claim-and-not-for-the-ocr
  (let [!release (promise)
        [!bumped bump!] (bumps)]
    (with-redefs [scan/transcribe-scan! (fn [_ _ _ on-claimed]
                                          (on-claimed)
                                          @!release
                                          {:ok true})
                  commands/bump-channels! bump!]
      (let [started (System/nanoTime)]
        (is (nil? (scan/start-transcription! 7 42 91)))
        (testing "the call returns at the claim, while the OCR still runs"
          (is (< (- (System/nanoTime) started) (* 2000 1000000)))
          (is (not (realized? !bumped)))))
      (deliver !release true)
      (testing "the end of the run bumps the comprehension channel"
        (is (= [7 #{:comprehension-mutations}] (deref !bumped 2000 :timeout)))))))

(deftest a-run-that-fails-before-its-claim-releases-the-caller
  (let [[!bumped bump!] (bumps)]
    (with-redefs [scan/transcribe-scan! (fn [& _] (throw (ex-info "no key" {})))
                  commands/bump-channels! bump!]
      (let [started (System/nanoTime)]
        (is (nil? (scan/start-transcription! 7 42 91)))
        (is (< (- (System/nanoTime) started) (* 2000 1000000))))
      (is (= [7 #{:comprehension-mutations}] (deref !bumped 2000 :timeout))))))

(deftest a-refused-run-releases-the-caller
  (let [[!bumped bump!] (bumps)]
    (with-redefs [scan/transcribe-scan! (fn [& _] {:ok false :error "already running"})
                  commands/bump-channels! bump!]
      (is (nil? (scan/start-transcription! 7 42 91)))
      (is (= [7 #{:comprehension-mutations}] (deref !bumped 2000 :timeout))))))
