(ns freememo.learn-session-blank-page-test
  "Source contracts for the Learn session blank page on a slow tablet
   (plans/low-end-tablet-performance/issues/13-learn-session-blank-page.md).

   The defect needs a slow device to show, so no suite here can reproduce it.
   The tablet probe is flow 3 of `playwright/tablet-rig.sh`. These tests keep
   the two code shapes that the fix depends on."
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]))

(deftest document-pin-spec-reads-its-offload-through-a-client-as-vec
  (testing "a bare Offload goes empty on each Learn step and remounts every consumer"
    (let [source (slurp "src/freememo/editor_pin_layer.cljc")]
      (is (re-find #"\(peek \(e/as-vec \(e/server \(e/Offload #\(pin-spec\* user-id topic-id\)\)\)\)\)"
            source)))))

(deftest boot-reports-a-heartbeat-that-nobody-reads
  (testing "a stalled client process leaves its socket open, so only the heartbeat shows it"
    (let [source (slurp "src/freememo/client_boot.cljs")]
      (is (str/includes? source "(= \"HEARTBEAT\" (.-data event))"))
      (is (str/includes? source "(nil? (.-onmessage socket))"))
      (is (str/includes? source "(.addEventListener socket \"message\""))
      (is (str/includes? source "(report-once! \"stalled\" error)")))))
