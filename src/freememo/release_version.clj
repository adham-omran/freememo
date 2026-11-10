(ns freememo.release-version
  "The Release Version of a commit (docs/adr/0002-release-version-is-a-commit-timestamp.md).

   It is the UTC committer time of the commit, written `YY.MM.<minute of the
   month>`. For example `26.11.13832` is 10 November 2026, 14:32 UTC. The
   largest part is 31 * 1440 - 1 = 44639, so every part fits the Windows MSI
   limit of 255.255.65535.

   A dev build has no Release Version and reports `dev`. NO requires: the
   build (src-build/build.clj) and the tests load this namespace alone."
  (:import [java.time Instant ZoneOffset]))

(def dev "What a build with no Release Version reports." "dev")

(defn from-epoch-seconds
  "Pre:  `seconds` is a Unix time (the committer time of a commit).
   Post: the Release Version string. It does not depend on the time zone of
         this computer."
  [seconds]
  (let [t (.atOffset (Instant/ofEpochSecond seconds) ZoneOffset/UTC)
        minute-of-month (+ (* (dec (.getDayOfMonth t)) 1440) (* (.getHour t) 60) (.getMinute t))]
    (format "%02d.%d.%d" (mod (.getYear t) 100) (.getMonthValue t) minute-of-month)))

(defn of-commit
  "Post: the Release Version of `commit` (default HEAD) in the git checkout at
         the working directory. Throws when git cannot read the commit."
  ([] (of-commit "HEAD"))
  ([commit]
   (let [pb (ProcessBuilder. ^java.util.List ["git" "log" "-1" "--format=%ct" (str commit)])
         p (.start (.redirectErrorStream pb true))
         out (slurp (.getInputStream p))]
     (when-not (zero? (.waitFor p))
       (throw (ex-info (str "git cannot read the commit time: " out) {:commit commit})))
     (from-epoch-seconds (parse-long (.trim ^String out))))))
