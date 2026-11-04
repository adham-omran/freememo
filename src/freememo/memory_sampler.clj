(ns freememo.memory-sampler
  "Records where the process's memory goes, so that a kernel kill can be
   explained after the fact (plans/jvm-cgroup-oom-kills.md).

   The kernel kills the JVM with SIGKILL when the container goes past its
   `mem_limit`. The JVM writes nothing at that moment: no OutOfMemoryError, no
   heap dump. So the only evidence is what this namespace wrote BEFORE the kill.

   One sample every `tick-ms` reads three sources:
     - the JVM's own view: heap, metaspace, code cache, direct and mapped
       buffers, thread count (java.lang.management);
     - the kernel's view of this process: /proc/self/status (VmRSS, RssAnon);
     - the kernel's view of the container: the cgroup v2 files memory.current,
       memory.max, memory.peak, memory.stat and memory.events. The kernel kills
       on these numbers, not on the JVM's.
   A logged sample adds Native Memory Tracking (NMT) per category, when the JVM
   runs with -XX:NativeMemoryTracking=summary. `rss-minus-nmt-mib` is memory
   that the JVM does not track, such as malloc arena overhead in native code.

   A sample is logged as `::sample` when one of these is true:
     - :periodic, `log-every-ms` passed since the last logged sample;
     - :jump, usage rose `jump-bytes` or more since the last logged sample;
     - :high, usage is at `high-fraction` of the limit or more. Then every tick
       logs, so the last line before a kill is at most `tick-ms` old.
   Usage is cgroup anon + kernel memory: the part that the kernel cannot drop
   as page cache when the container nears its limit.

   On :high, the sampler also logs `::histogram`, the classes that hold the
   most live heap, at most once per `histogram-gap-ms`. GC.class_histogram runs
   a full GC first, so it pauses the app for about a second on a 3 GiB heap.

   Read the records with `python3 scripts/prod_logs.py memory`."
  (:require
   [clojure.string :as str]
   [taoensso.telemere :as tel])
  (:import
   [java.lang.management BufferPoolMXBean ManagementFactory MemoryPoolMXBean]
   [java.nio.file Files Path]
   [java.util.concurrent Executors ScheduledExecutorService ThreadFactory TimeUnit]
   [javax.management ObjectName]))

(def tick-ms
  "How often a sample is taken. Short, so that a spike shows before the kill."
  5000)

(def log-every-ms
  "How often a sample is logged when nothing unusual happens."
  60000)

(def jump-bytes
  "A rise in usage this large since the last logged sample logs at once."
  (* 256 1024 1024))

(def high-fraction
  "Usage at this fraction of the container limit counts as near a kill."
  0.85)

(def histogram-gap-ms
  "The least time between two class histograms. Each one pauses the app."
  (* 10 60 1000))

(def histogram-rows
  "How many classes a histogram record lists."
  15)

(defn- mib
  "Bytes as whole MiB, or nil."
  [b]
  (when b (quot (long b) 1048576)))

;; ---------------------------------------------------------------------------
;; Pure parsers. Each takes the text that the kernel or the JVM writes.
;; ---------------------------------------------------------------------------

(defn- category-key
  "\"Java Heap\" -> :java-heap, \"Shared class space\" -> :shared-class-space."
  [s]
  (keyword (str/replace (str/lower-case (str/trim s)) #"[^a-z0-9]+" "-")))

(defn parse-nmt-summary
  "Parse the text of `VM.native_memory summary scale=KB`.
   Pre : `text` is that command's output, or nil.
   Post: {:total committed-bytes, category-key committed-bytes ...}, or nil
         when the text holds no NMT report (NMT is off, or the call failed)."
  [text]
  (when-let [[_ total-kb] (and text (re-find #"Total: reserved=\d+KB, committed=(\d+)KB" text))]
    (into {:total (* 1024 (parse-long total-kb))}
      (for [[_ category kb] (re-seq #"(?m)^-\s+(.+?)\s+\(reserved=\d+KB, committed=(\d+)KB" text)]
        [(category-key category) (* 1024 (parse-long kb))]))))

(defn parse-memory-stat
  "Parse cgroup v2 `memory.stat` or `memory.events` text (\"key value\" lines).
   Post: {\"anon\" bytes ...}. Keys stay strings, as the kernel names them."
  [text]
  (into {}
    (for [[_ k v] (re-seq #"(?m)^(\S+) (\d+)$" (or text ""))]
      [k (parse-long v)])))

(defn parse-proc-status
  "Parse /proc/self/status. A value in kB is converted to bytes.
   Post: {\"VmRSS\" bytes, \"Threads\" n ...}."
  [text]
  (into {}
    (for [[_ k v unit] (re-seq #"(?m)^(\w+):\s+(\d+)(?: (kB))?$" (or text ""))]
      [k (cond-> (parse-long v) unit (* 1024))])))

(defn parse-histogram
  "Parse the text of `GC.class_histogram`.
   Post: the first `n` rows, biggest first, as [{:class :instances :mib}]."
  [text n]
  (->> (re-seq #"(?m)^\s*\d+:\s+(\d+)\s+(\d+)\s+(\S+)" (or text ""))
    (take n)
    (mapv (fn [[_ instances bytes cls]]
            {:class cls :instances (parse-long instances) :mib (mib (parse-long bytes))}))))

(defn usage-bytes
  "The usage that the thresholds compare: cgroup anon + kernel memory. Page
   cache is left out, because the kernel drops it before it kills. Falls back
   to RssAnon when there is no cgroup v2 memory controller (a dev machine)."
  [{:keys [cg-anon cg-kernel rss-anon]}]
  (if cg-anon (+ cg-anon (or cg-kernel 0)) rss-anon))

(defn decide
  "Pure: what one sample earns.
   Pre : `state` is {:last-at :last-usage :last-histogram-at}, each nil until
         set. `usage` and `limit` are bytes or nil. `now` is epoch ms.
   Post: nil, or {:reason r :histogram? bool} where r is :high, :jump or
         :periodic, in that order of precedence."
  [{:keys [last-at last-usage last-histogram-at]} now usage limit]
  (let [high? (boolean (and usage limit (>= usage (* high-fraction limit))))
        jump? (boolean (and usage last-usage (>= (- usage last-usage) jump-bytes)))
        due? (or (nil? last-at) (>= (- now last-at) log-every-ms))
        reason (cond high? :high jump? :jump due? :periodic)]
    (when reason
      {:reason reason
       :histogram? (and high?
                     (or (nil? last-histogram-at)
                       (>= (- now last-histogram-at) histogram-gap-ms)))})))

;; ---------------------------------------------------------------------------
;; Readers
;; ---------------------------------------------------------------------------

(defn- slurp-or-nil
  "The file's text, or nil. Not `slurp`: on JDK 25 `slurp` of a /proc file
   throws IOException \"Invalid argument\" from FileInputStream.available."
  [^String path]
  (try (Files/readString (Path/of path (make-array String 0)))
       (catch Exception _ nil)))

(defn- read-long
  "A file that holds one number. nil when it is absent or holds \"max\"."
  [path]
  (some-> (slurp-or-nil path) str/trim parse-long))

(def ^:private cgroup-dir
  (delay
    ;; cgroup v2 names one path, on the line that starts with "0::". Inside a
    ;; container with its own cgroup namespace that path is "/".
    (when-let [line (some->> (slurp-or-nil "/proc/self/cgroup")
                      str/split-lines
                      (some #(when (str/starts-with? % "0::") %)))]
      (let [dir (str "/sys/fs/cgroup" (str/trimr (subs line 3)))]
        (str/replace dir #"/+$" "")))))

(defn- diagnostic-command
  "Run a HotSpot diagnostic command (what jcmd runs) in this JVM. The runtime
   image is a JRE with no jcmd, so the MBean is the only way in.
   Post: the command's text, or nil when it fails."
  [op & args]
  (try
    (.invoke (ManagementFactory/getPlatformMBeanServer)
      (ObjectName. "com.sun.management:type=DiagnosticCommand")
      ^String op
      (object-array [(into-array String args)])
      (into-array String ["[Ljava.lang.String;"]))
    (catch Exception _ nil)))

(defn nmt
  "Committed bytes per NMT category, or nil when NMT is off."
  []
  (parse-nmt-summary (diagnostic-command "vmNativeMemory" "summary" "scale=KB")))

(defn sample
  "One reading of every cheap source, in bytes. A source that is absent gives
   nil values, never an exception."
  []
  (let [heap (.getHeapMemoryUsage (ManagementFactory/getMemoryMXBean))
        pools (ManagementFactory/getMemoryPoolMXBeans)
        pool (fn [re]
               (reduce + 0 (for [^MemoryPoolMXBean p pools
                                 :when (re-find re (.getName p))]
                             (.getCommitted (.getUsage p)))))
        buffers (ManagementFactory/getPlatformMXBeans BufferPoolMXBean)
        buffer (fn [n]
                 (some (fn [^BufferPoolMXBean b] (when (= n (.getName b)) (.getMemoryUsed b)))
                   buffers))
        threads (ManagementFactory/getThreadMXBean)
        status (parse-proc-status (slurp-or-nil "/proc/self/status"))
        dir @cgroup-dir
        stat (when dir (parse-memory-stat (slurp-or-nil (str dir "/memory.stat"))))
        events (when dir (parse-memory-stat (slurp-or-nil (str dir "/memory.events"))))]
    {:rss (get status "VmRSS")
     :rss-anon (get status "RssAnon")
     :rss-file (get status "RssFile")
     :heap-used (.getUsed heap)
     :heap-committed (.getCommitted heap)
     :heap-max (.getMax heap)
     :metaspace (pool #"^Metaspace$")
     :class-space (pool #"Compressed Class Space")
     :code-cache (pool #"CodeHeap|Code Cache")
     :direct (buffer "direct")
     :mapped (buffer "mapped")
     :threads (.getThreadCount threads)
     :threads-peak (.getPeakThreadCount threads)
     :cg-current (when dir (read-long (str dir "/memory.current")))
     :cg-max (when dir (read-long (str dir "/memory.max")))
     :cg-peak (when dir (read-long (str dir "/memory.peak")))
     :cg-anon (get stat "anon")
     :cg-file (get stat "file")
     :cg-kernel (get stat "kernel")
     :cg-sock (get stat "sock")
     :cg-oom-kills (get events "oom_kill")}))

(def ^:private count-keys
  "Sample keys that hold a count, not bytes."
  #{:threads :threads-peak :cg-oom-kills})

(defn log-data
  "A sample and an NMT reading as one flat map for the log line. Bytes become
   MiB with a `-mib` suffix, so LogQL `unwrap` reads each field as a number."
  [s nmt-reading reason usage]
  (let [base (into {}
               (for [[k v] s :when (some? v)]
                 (if (count-keys k)
                   [k v]
                   [(keyword (str (name k) "-mib")) (mib v)])))
        nmt-part (into {}
                   (for [[k v] nmt-reading :when (pos? (mib v))]
                     [(keyword (str "nmt-" (name k) "-mib")) (mib v)]))]
    (cond-> (merge base nmt-part {:reason reason :usage-mib (mib usage)})
      (and (:rss s) (:total nmt-reading))
      (assoc :rss-minus-nmt-mib (- (mib (:rss s)) (mib (:total nmt-reading)))))))

;; ---------------------------------------------------------------------------
;; Schedule
;; ---------------------------------------------------------------------------

(defonce ^:private state (atom {}))

(defn tick-once!
  "Take one sample, and log it when `decide` says so."
  []
  (let [now (System/currentTimeMillis)
        s (sample)
        usage (usage-bytes s)
        {:keys [reason histogram?]} (decide @state now usage (:cg-max s))]
    (when reason
      (tel/log! {:level :info :id ::sample
                 :data (log-data s (nmt) reason usage)}
        (str "Memory sample (" (name reason) ")"))
      (swap! state assoc :last-at now :last-usage usage))
    (when histogram?
      (swap! state assoc :last-histogram-at now)
      (let [started (System/nanoTime)
            top (parse-histogram (diagnostic-command "gcClassHistogram") histogram-rows)]
        (tel/log! {:level :warn :id ::histogram
                   :data {:usage-mib (mib usage)
                          :limit-mib (mib (:cg-max s))
                          :pause-ms (quot (- (System/nanoTime) started) 1000000)
                          :top top}}
          "Memory near the container limit: classes holding the most live heap")))))

(defonce ^:private scheduler (atom nil))

(defn start!
  "Start the sampler. Idempotent: once per process.
   Post: `tick-once!` runs every `tick-ms` until `stop!`. Returns :started, or
         :already-running.
   Invariant: a throwing tick never stops the schedule. `scheduleAtFixedRate`
         cancels every later run when its task throws, and a sampler that
         stops silently is worse than none."
  []
  (if @scheduler
    :already-running
    (let [exec (Executors/newSingleThreadScheduledExecutor
                 (reify ThreadFactory
                   (newThread [_ r]
                     (doto (Thread. ^Runnable r "memory-sampler") (.setDaemon true)))))
          s (sample)
          nmt-reading (nmt)]
      (tel/log! {:level :info :id ::started
                 :data {:tick-ms tick-ms
                        :log-every-ms log-every-ms
                        :jump-mib (mib jump-bytes)
                        :high-fraction high-fraction
                        :heap-max-mib (mib (:heap-max s))
                        :limit-mib (mib (:cg-max s))
                        :cgroup-dir @cgroup-dir
                        :nmt? (some? nmt-reading)
                        :jvm-args (str/join " " (.getInputArguments (ManagementFactory/getRuntimeMXBean)))}}
        "Memory sampler started")
      (.scheduleAtFixedRate ^ScheduledExecutorService exec
        (fn [] (try (tick-once!)
                 (catch Throwable t (tel/error! {:id ::tick-failed} t))))
        0 tick-ms TimeUnit/MILLISECONDS)
      (reset! scheduler exec)
      :started)))

(defn stop!
  "Stop the sampler. Idempotent. Post: no further ticks; returns :stopped."
  []
  (when-some [^ScheduledExecutorService exec @scheduler]
    (.shutdownNow exec)
    (reset! scheduler nil))
  :stopped)
