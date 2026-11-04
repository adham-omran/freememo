(ns freememo.memory-sampler-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [freememo.memory-sampler :as ms]))

;; Real output of `VM.native_memory summary scale=KB` on JDK 25.0.4.1, with
;; the per-category detail lines cut.
(def nmt-text
  "
Native Memory Tracking:

(Omitting categories weighting less than 1KB)

Total: reserved=4603043KB, committed=319623KB
       malloc: 52719KB #56089, peak=57670KB #50361
       mmap:   reserved=4550324KB, committed=266904KB

-                 Java Heap (reserved=3059712KB, committed=194560KB)
                            (mmap: reserved=3059712KB, committed=194560KB)

-                     Class (reserved=1048810KB, committed=1898KB)

-                    Thread (reserved=19520KB, committed=856KB)
                            (threads #19)

-                      Code (reserved=249248KB, committed=9424KB)

-                     Other (reserved=34KB, committed=34KB)

-    Native Memory Tracking (reserved=1002KB, committed=1002KB)

-        Shared class space (reserved=16384KB, committed=13876KB, readonly=0KB)

-                 Metaspace (reserved=65577KB, committed=11177KB)
")

(deftest parse-nmt-summary-reads-total-and-categories
  (let [r (ms/parse-nmt-summary nmt-text)]
    (is (= (* 1024 319623) (:total r)))
    (is (= (* 1024 194560) (:java-heap r)))
    (is (= (* 1024 9424) (:code r)))
    (is (= (* 1024 34) (:other r)))
    (is (= (* 1024 1002) (:native-memory-tracking r)))
    (testing "a category line with a third field still parses"
      (is (= (* 1024 13876) (:shared-class-space r))))
    (is (= (* 1024 11177) (:metaspace r)))))

(deftest parse-nmt-summary-is-nil-when-nmt-is-off
  (is (nil? (ms/parse-nmt-summary nil)))
  (is (nil? (ms/parse-nmt-summary "Native memory tracking is not enabled\n"))))

(deftest parse-memory-stat-reads-key-value-lines
  (is (= {"anon" 1048576 "file" 4096 "oom_kill" 2}
        (ms/parse-memory-stat "anon 1048576\nfile 4096\noom_kill 2\n")))
  (is (= {} (ms/parse-memory-stat nil))))

(deftest parse-proc-status-converts-kb-to-bytes
  (let [r (ms/parse-proc-status "Name:\tjava\nVmRSS:\t  2048 kB\nRssAnon:\t  1024 kB\nThreads:\t42\n")]
    (is (= (* 2048 1024) (get r "VmRSS")))
    (is (= (* 1024 1024) (get r "RssAnon")))
    (is (= 42 (get r "Threads")))))

(deftest parse-histogram-keeps-the-first-rows
  (let [text " num     #instances         #bytes  class name (module)
-------------------------------------------------------
   1:         19437      209715200  [B (java.base@25.0.4.1)
   2:          3970        1048576  java.lang.Class (java.base@25.0.4.1)
   3:         19247         461928  java.lang.String (java.base@25.0.4.1)
Total         91218      211225704
"]
    (is (= [{:class "[B" :instances 19437 :mib 200}
            {:class "java.lang.Class" :instances 3970 :mib 1}]
          (ms/parse-histogram text 2)))))

(deftest usage-is-anon-plus-kernel-and-leaves-out-page-cache
  (is (= 300 (ms/usage-bytes {:cg-anon 200 :cg-kernel 100 :cg-file 5000 :rss-anon 7})))
  (testing "no cgroup: falls back to RssAnon"
    (is (= 7 (ms/usage-bytes {:rss-anon 7})))))

(def gib (* 1024 1024 1024))
(def mib (* 1024 1024))

(deftest decide-logs-the-first-sample-then-periodically
  (is (= :periodic (:reason (ms/decide {} 0 gib (* 6 gib)))))
  (is (nil? (ms/decide {:last-at 0 :last-usage gib} 1000 gib (* 6 gib))))
  (is (= :periodic (:reason (ms/decide {:last-at 0 :last-usage gib} ms/log-every-ms gib (* 6 gib))))))

(deftest decide-logs-a-jump-at-once
  (is (= :jump (:reason (ms/decide {:last-at 0 :last-usage gib} 1000 (+ gib (* 256 mib)) (* 6 gib)))))
  (is (nil? (ms/decide {:last-at 0 :last-usage gib} 1000 (+ gib (* 255 mib)) (* 6 gib)))))

(deftest decide-logs-every-tick-near-the-limit-and-rate-limits-the-histogram
  (let [limit (* 6 gib)
        near (long (* 0.9 limit))]
    (is (= {:reason :high :histogram? true}
          (ms/decide {:last-at 0 :last-usage near} 1000 near limit)))
    (is (= {:reason :high :histogram? false}
          (ms/decide {:last-at 0 :last-usage near :last-histogram-at 0} 1000 near limit)))
    (is (true? (:histogram?
                 (ms/decide {:last-at 0 :last-usage near :last-histogram-at 0}
                   ms/histogram-gap-ms near limit))))))

(deftest decide-without-a-limit-never-reads-high
  (is (= :periodic (:reason (ms/decide {} 0 (* 100 gib) nil)))))

(deftest sample-reads-the-jvm-on-any-machine
  (let [s (ms/sample)]
    (is (pos? (:heap-used s)))
    (is (pos? (:heap-committed s)))
    (is (pos? (:threads s)))
    (is (pos? (:metaspace s)))))

(deftest sample-reads-proc-on-linux
  ;; `slurp` of /proc throws on JDK 25, and the sampler swallows read errors.
  ;; Without this test, the process size would go missing with no error.
  (when (.exists (java.io.File. "/proc/self/status"))
    (let [s (ms/sample)]
      (is (pos? (:rss s)))
      (is (pos? (:rss-anon s)))
      (is (some? (ms/usage-bytes s))))))

(deftest log-data-is-flat-mib-with-counts-kept
  (let [d (ms/log-data {:rss (* 3 gib) :heap-used gib :threads 40 :cg-oom-kills 1 :direct nil}
            {:total (* 2 gib) :java-heap gib :other 1024}
            :jump (* 2 gib))]
    (is (= 3072 (:rss-mib d)))
    (is (= 1024 (:heap-used-mib d)))
    (is (= 40 (:threads d)))
    (is (= 1 (:cg-oom-kills d)))
    (is (not (contains? d :direct-mib)) "an absent source gives no field")
    (is (= 1024 (:nmt-java-heap-mib d)))
    (is (not (contains? d :nmt-other-mib)) "a category under 1 MiB is left out")
    (is (= 1024 (:rss-minus-nmt-mib d)))
    (is (= :jump (:reason d)))
    (is (= 2048 (:usage-mib d)))))

(deftest nmt-reading-matches-the-jvm-flag
  ;; The :test alias runs without -XX:NativeMemoryTracking, so NMT is off.
  ;; With the flag on, the same call must return the java heap category.
  (let [on? (some #(re-find #"NativeMemoryTracking=(summary|detail)" %)
              (.getInputArguments (java.lang.management.ManagementFactory/getRuntimeMXBean)))
        r (ms/nmt)]
    (if on?
      (is (pos? (:java-heap r)))
      (is (nil? r)))))
