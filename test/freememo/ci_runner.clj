(ns freememo.ci-runner
  "Runs every suite that needs no PostgreSQL. CI calls `-main`.

  The rule is behavior, not a list. Run each suite with the database port
  refused. A suite that passes is database-free. A suite whose every failure is
  a refused database connection (or a missing client build) cannot run in CI, so the run skips it. Any
  other failure is red. A new suite is classified with no edit here.

  Seam 1 suites (`local-install*`, `local-postgres*`) start child JVMs and have
  their own command in test/CLAUDE.md. `excluded-ns?` leaves them out."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :as t]))

(def unavailable-re
  "Text that a missing database or a missing client build leaves in a report.
  The job has no PostgreSQL and builds no client, so a suite that fails only on
  one of these cannot run there."
  #"(?s)Connection to \S+ refused|PSQLException|java\.net\.ConnectException|HikariPool\S* - .*(?:Exception|Failed)")

(def no-build-re
  "A missing client build breaks every assertion that reads the shell, and most
  of those failures do not name the cause. So one mention marks the whole suite."
  #"missing-shadow-build-manifest")

(defn excluded-ns?
  "True for a suite that has its own command (Seam 1)."
  [ns-sym]
  (boolean (re-find #"^freememo\.local-(install|postgres)" (name ns-sym))))

(defn test-namespaces
  "Every `*_test.clj` namespace under `dir`, sorted, as symbols."
  [dir]
  (->> (file-seq (io/file dir))
       (map #(.getPath ^java.io.File %))
       (filter #(str/ends-with? % "_test.clj"))
       (map #(-> % (subs (inc (count (str dir)))) (str/replace #"\.clj$" "")
                 (str/replace "/" ".") (str/replace "_" "-") symbol))
       (remove excluded-ns?)
       sort))

(defn- ex-text [^Throwable e]
  (str/join "\n" (map #(str (class %) " " (ex-message %)) (take-while some? (iterate ex-cause e)))))

(defn- failure-blocks
  "Split the text that `run-tests` printed into one string per FAIL or ERROR block."
  [out]
  (->> (str/split out #"(?m)^(?=(?:FAIL|ERROR) in )")
       (filter #(re-find #"^(?:FAIL|ERROR) in " %))))

(defn classify
  "Return :pass, :skipped or :fail from the text that `run-tests` printed and
  the exception that it threw, if any. It reads text, because a rebound
  `clojure.test/report` breaks the Hyperfiddle RCF reporter."
  [out thrown]
  (let [texts (concat (failure-blocks out) (when thrown [(ex-text thrown)]))]
    (cond (empty? texts) :pass
          (re-find no-build-re out) :skipped
          (every? #(re-find unavailable-re %) texts) :skipped
          :else :fail)))

(defn run-suite
  "Run one suite. Return {:ns :class :out :thrown}."
  [ns-sym]
  (let [out (java.io.StringWriter.)
        thrown (atom nil)]
    (binding [*out* out
              t/*test-out* out]
      (try (require ns-sym) (t/run-tests ns-sym)
           (catch Throwable e (reset! thrown e))))
    {:ns ns-sym :class (classify (str out) @thrown) :out (str out)
     :thrown @thrown}))

(defn -main [& _]
  (let [results (mapv run-suite (test-namespaces "test"))
        by (group-by :class results)]
    (doseq [{:keys [ns out thrown]} (:fail by)]
      (println "FAILED" ns)
      (println out)
      (when thrown (println (ex-text thrown))))
    (doseq [{:keys [ns]} (:skipped by)] (println "skipped (needs database or client build)" ns))
    (println (format "ci-runner: %d suites, %d passed, %d cannot run here, %d failed"
                     (count results) (count (:pass by)) (count (:skipped by)) (count (:fail by))))
    (shutdown-agents)
    (System/exit (if (seq (:fail by)) 1 0))))
