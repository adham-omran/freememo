(ns freememo.learn-next-gate-test
  "Next is enabled when the new body is shown
   (plans/learn-next-speed/issues/03-next-enabled-when-body-shown.md).

   These tests hold the gate rule of the Next button with no browser. The
   browser checks are in `playwright/learn-session-run.mjs`
   (`next-waits-for-body`, `double-tap-one-advance`, `video-next`)."
  (:require
   [clojure.test :refer [deftest is testing]]
   [freememo.learn-session :as learn-session]))

(defn- closed? [gate topic-id shown-id body-kind?]
  (learn-session/next-gate-closed? gate topic-id shown-id body-kind?))

(def ^:private tapped {:id "g1" :from 10})

(deftest no-gate-is-open
  (is (false? (closed? nil 10 10 true)))
  (is (false? (closed? nil 10 nil false))))

(deftest the-gate-holds-until-the-queue-advances
  (testing "the tapped topic is still in view: closed, even after every timer"
    (is (true? (closed? tapped 10 10 true)))
    (is (true? (closed? (assoc tapped :min? true :timer? true) 10 10 true)))
    (is (true? (closed? (assoc tapped :min? true :timer? true) 10 10 false)))))

(deftest a-text-topic-opens-when-its-body-is-in-the-editor
  (let [gate (assoc tapped :min? true)]
    (testing "the editor still shows the previous body: closed"
      (is (true? (closed? gate 11 10 true))))
    (testing "the timer of other kinds does not open a text topic"
      (is (true? (closed? (assoc gate :timer? true) 11 10 true))))
    (testing "the new body is in the editor: open"
      (is (false? (closed? gate 11 11 true))))))

(deftest the-minimum-holds-a-fast-body
  (testing "the body is in, but the minimum has not passed: closed"
    (is (true? (closed? tapped 11 11 true))))
  (testing "after the minimum: open"
    (is (false? (closed? (assoc tapped :min? true) 11 11 true)))))

(deftest another-kind-keeps-the-timer
  (let [gate (assoc tapped :min? true)]
    (testing "a PDF or video topic is closed until the timer"
      (is (true? (closed? gate 11 nil false)))
      (is (true? (closed? gate 11 11 false))))
    (testing "the timer opens it"
      (is (false? (closed? (assoc gate :timer? true) 11 nil false))))))

(deftest the-cap-always-opens-after-the-advance
  (testing "a body that never arrives cannot hold Next for ever"
    (is (false? (closed? (assoc tapped :cap? true) 11 10 true))))
  (testing "the cap opens even when the topic in view did not change, so Next
            never stays disabled"
    (is (false? (closed? (assoc tapped :cap? true) 10 10 true)))))

(deftest a-mark-applies-only-to-its-own-gate
  (testing "a timer of an earlier tap does not mark a later gate"
    (is (= {:id "g2" :from 11}
           (learn-session/mark-gate {:id "g2" :from 11} "g1" :min?))))
  (testing "a timer of this tap marks it"
    (is (= {:id "g2" :from 11 :min? true}
           (learn-session/mark-gate {:id "g2" :from 11} "g2" :min?))))
  (testing "no gate stays no gate"
    (is (nil? (learn-session/mark-gate nil "g1" :min?)))))

(deftest editor-body-kinds
  (is (true? (learn-session/editor-body-kind? "supermemo")))
  (is (true? (learn-session/editor-body-kind? "basic")))
  (is (false? (learn-session/editor-body-kind? "pdf")))
  (is (false? (learn-session/editor-body-kind? "video")))
  (is (false? (learn-session/editor-body-kind? nil))))
