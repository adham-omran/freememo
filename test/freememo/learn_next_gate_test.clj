(ns freememo.learn-next-gate-test
  "Next is enabled when the new body is shown
   (plans/learn-next-speed/issues/03-next-enabled-when-body-shown.md).

   Ticket 08 (plans/learn-next-speed/issues/08-gate-waits-after-early-body.md)
   moved the 150 ms minimum from the display to the tap: a long task after an
   early body delayed the minimum's timer by 300 to 400 ms.

   These tests hold the gate rule of the Next button with no browser. The
   browser checks are in `playwright/learn-session-run.mjs`
   (`next-waits-for-body`, `double-tap-one-advance`, `video-next`)."
  (:require
   [clojure.test :refer [deftest is testing]]
   [freememo.learn-session :as learn-session]))

(defn- closed? [gate topic-id shown-id body-kind?]
  (learn-session/next-gate-closed? gate topic-id shown-id body-kind?))

(def ^:private tapped {:id "g1" :from 10 :at 1000})

(deftest no-gate-is-open
  (is (false? (closed? nil 10 10 true)))
  (is (false? (closed? nil 10 nil false))))

(deftest the-gate-holds-until-the-queue-advances
  (testing "the tapped topic is still in view: closed, even after every timer"
    (is (true? (closed? tapped 10 10 true)))
    (is (true? (closed? (assoc tapped :timer? true) 10 10 true)))
    (is (true? (closed? (assoc tapped :timer? true) 10 10 false)))))

(deftest a-text-topic-opens-when-its-body-is-in-the-editor
  (let [gate tapped]
    (testing "the editor still shows the previous body: closed"
      (is (true? (closed? gate 11 10 true))))
    (testing "the timer of other kinds does not open a text topic"
      (is (true? (closed? (assoc gate :timer? true) 11 10 true))))
    (testing "the new body is in the editor: open"
      (is (false? (closed? gate 11 11 true))))))

(deftest a-fast-body-opens-the-display-at-once
  (testing "the body is in before the minimum: Next shows enabled. No timer
            has to run, so a long task after the body cannot hold it."
    (is (false? (closed? tapped 11 11 true)))))

(defn- allowed? [gate topic-id shown-id body-kind? t]
  (learn-session/tap-allowed? gate topic-id shown-id body-kind? t))

(deftest the-minimum-holds-a-second-tap
  (testing "no gate: any tap counts"
    (is (true? (allowed? nil 10 10 true 0))))
  (testing "the body is in, but the tap came less than 150 ms after the
            previous tap: it does not count"
    (is (false? (allowed? tapped 11 11 true 1001)))
    (is (false? (allowed? tapped 11 11 true 1149))))
  (testing "150 ms or more after the previous tap: it counts"
    (is (true? (allowed? tapped 11 11 true 1150)))
    (is (true? (allowed? tapped 11 11 true 1600))))
  (testing "a gate with no tap time (a timer-only gate) applies no minimum"
    (is (true? (allowed? (dissoc tapped :at) 11 11 true 0))))
  (testing "a closed gate refuses a late tap"
    (is (false? (allowed? tapped 11 10 true 5000)))
    (is (false? (allowed? tapped 10 10 true 5000)))))

(deftest another-kind-keeps-the-timer
  (let [gate tapped]
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
           (learn-session/mark-gate {:id "g2" :from 11} "g1" :timer?))))
  (testing "a timer of this tap marks it"
    (is (= {:id "g2" :from 11 :timer? true}
           (learn-session/mark-gate {:id "g2" :from 11} "g2" :timer?))))
  (testing "no gate stays no gate"
    (is (nil? (learn-session/mark-gate nil "g1" :timer?)))))

(deftest editor-body-kinds
  (is (true? (learn-session/editor-body-kind? "supermemo")))
  (is (true? (learn-session/editor-body-kind? "basic")))
  (is (false? (learn-session/editor-body-kind? "pdf")))
  (is (false? (learn-session/editor-body-kind? "video")))
  (is (false? (learn-session/editor-body-kind? nil))))
