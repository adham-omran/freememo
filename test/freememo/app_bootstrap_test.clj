(ns freememo.app-bootstrap-test
  "Ticket 19: the bootstrap (tools/installer/bootstrap/FreememoBoot.java). The
   tests compile it with the JDK's compiler and run it for real, in a child
   JVM, against small fake apps. A fake app is a jar with `fake.Main` and a
   resource `mode.txt`: `ready`, `hang`, `crash` or `restart-once`. The
   jar's main prints what the real app prints, `FREEMEMO_READY port=… pg_port=…`."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [freememo.app-update :as au]
            [freememo.app-update-test :as t])
  (:import [java.io File]
           [java.nio.file Files Path]
           [java.util.jar JarEntry JarOutputStream]
           [javax.tools ToolProvider]))

(def ^:private work (delay (let [d (t/temp-dir)] (.deleteOnExit d) d)))

(def ^:private fake-source
  "package fake;
   import java.nio.file.*;
   public class Main {
     public static void main(String[] args) throws Exception {
       String[] w = new String(Main.class.getResourceAsStream(\"/mode.txt\").readAllBytes()).trim().split(\" \");
       System.out.println(\"who=\" + w[0] + \" args=\" + String.join(\",\", args)
           + \" tools=\" + System.getProperty(\"freememo.tools-dir\") + \" boot=\" + System.getProperty(\"freememo.bootstrap\")
           + \" ui-element=\" + System.getProperty(\"apple.awt.UIElement\"));
       switch (w[1]) {
         case \"crash\": System.exit(3);
         case \"hang\": Thread.sleep(600000); break;
         case \"restart-once\": {
           Path flag = Paths.get(System.getProperty(\"freememo.fake.flag\"));
           System.out.println(\"FREEMEMO_READY port=1 pg_port=2 who=\" + w[0]);
           System.out.flush();
           if (!Files.exists(flag)) { Files.writeString(flag, \"x\"); System.exit(75); }
           System.exit(0);
         }
         default: System.out.println(\"FREEMEMO_READY port=1 pg_port=2 who=\" + w[0]); System.out.flush(); System.exit(0);
       }
     }
   }")

(defn- compile! [^File src ^File out]
  (.mkdirs out)
  (let [rc (.run (ToolProvider/getSystemJavaCompiler) nil nil nil (into-array String ["-d" (.getPath out) (.getPath src)]))]
    (assert (zero? rc) "javac failed")))

(def ^:private boot-classes
  (delay (let [out (io/file @work "boot")]
           (compile! (io/file "tools/installer/bootstrap/FreememoBoot.java") out)
           out)))

(def ^:private fake-class
  (delay (let [src (io/file @work "fake-src" "fake" "Main.java") out (io/file @work "fake-classes")]
           (io/make-parents src)
           (spit src fake-source)
           (compile! src out)
           (Files/readAllBytes (.toPath (io/file out "fake" "Main.class"))))))

(defn- fake-jar!
  "Post: a jar at `file` for the fake app `who` that behaves as `mode`."
  [^File file who mode]
  (io/make-parents file)
  (with-open [o (JarOutputStream. (io/output-stream file))]
    (.putNextEntry o (JarEntry. "fake/Main.class")) (.write o ^bytes @fake-class) (.closeEntry o)
    (.putNextEntry o (JarEntry. "mode.txt")) (.write o (.getBytes (str who " " mode))) (.closeEntry o)))

(defn- signed-update!
  "Post: a verified-looking App Update jar `rv` in the data dir, signed by `kp`."
  [data rv mode kp]
  (let [dir (au/updates-dir data)]
    (fake-jar! (au/jar-file dir rv) (str "new-" rv) mode)
    (Files/write (.toPath (au/sig-file dir rv)) ^bytes (t/sign-bytes kp rv (au/jar-file dir rv)) (make-array java.nio.file.OpenOption 0))))

(defn- run-boot
  "Run the bootstrap in a JVM. Post: `{:exit :out}`."
  [{:keys [app data kp timeout extra-props]} & args]
  (let [key (when kp (.encodeToString (java.util.Base64/getEncoder) (.getEncoded (.getPublic kp))))
        cmd (concat ["java" "-cp" (.getPath @boot-classes)
                     (str "-Dfreememo.boot.appdir=" (.getPath app))
                     "-Dfreememo.boot.bundled-version=26.11.1"
                     "-Dfreememo.boot.main-class=fake.Main"
                     (str "-Dfreememo.boot.ready-timeout-s=" (or timeout 5))
                     "-Dfreememo.tools-dir=/the/tools"
                     (str "-Dfreememo.fake.flag=" (io/file data "flag"))]
                extra-props
                (when key [(str "-Dfreememo.update-public-key=" key)])
                ["FreememoBoot" "--data-dir" (.getPath data)] args)
        pb (doto (ProcessBuilder. ^java.util.List (vec cmd)) (.redirectErrorStream true))
        p (.start pb)
        out (slurp (.getInputStream p))]
    {:exit (.waitFor p) :out out}))

(defn- install!
  "Post: `{:app :data}`, an app dir with the bundled fake jar and an empty data dir."
  [bundled-mode]
  (let [app (t/temp-dir) data (t/temp-dir)]
    (fake-jar! (io/file app "freememo.jar") "bundled" bundled-mode)
    (spit (io/file app "freememo-boot.jar") "not a jar: the bootstrap must not put it on the class path")
    {:app app :data data}))

(defn- whos [out] (vec (map second (re-seq #"FREEMEMO_READY port=1 pg_port=2 who=(\S+)" out))))

(deftest a-newer-verified-jar-runs-with-the-same-options-and-arguments
  (let [{:keys [app data]} (install! "ready") kp (t/key-pair)]
    (try
      (signed-update! data "26.12.1" "ready" kp)
      (let [{:keys [exit out]} (run-boot {:app app :data data :kp kp
                                          :extra-props ["-Dapple.awt.UIElement=true"]}
                                         "--no-tab")]
        (is (zero? exit) out)
        (is (= ["new-26.12.1"] (whos out)))
        (is (str/includes? out "args=--data-dir," ) "the user's arguments reach the app")
        (is (str/includes? out "--no-tab"))
        (is (str/includes? out "tools=/the/tools") "freememo.* properties go on")
        (is (str/includes? out "boot=true"))
        (is (str/includes? out "ui-element=true")
            "macOS: the child stays out of the Dock and Cmd-Tab"))
      (finally (t/delete-tree! app) (t/delete-tree! data)))))

(deftest the-child-runs-the-java-exe-of-the-bundled-runtime
  ;; Windows: the installer sets freememo-app.exe, a renamed java.exe, so
  ;; Task Manager shows FreeMemo. A missing name shows where the bootstrap looked.
  (let [{:keys [app data]} (install! "ready")]
    (try
      (let [{:keys [exit out]} (run-boot {:app app :data data
                                          :extra-props ["-Dfreememo.boot.java-exe=no-such-java"]})]
        (is (not (zero? exit)) out)
        (is (str/includes? out (str File/separator "bin" File/separator "no-such-java")) out))
      (finally (t/delete-tree! app) (t/delete-tree! data)))))

(defn- starts-file [data rv]
  (io/file (au/updates-dir data) (str "freememo-" rv ".jar.starts")))

(deftest a-slow-start-is-forgiven-and-a-second-failed-start-marks-the-jar
  (let [{:keys [app data]} (install! "ready") kp (t/key-pair)]
    (try
      (signed-update! data "26.12.1" "ready" kp)
      (signed-update! data "26.12.2" "hang" kp)
      (testing "first failed start: the previous jar runs, no marker"
        (let [{:keys [exit out]} (run-boot {:app app :data data :kp kp :timeout 2})]
          (is (zero? exit) out)
          (is (= ["new-26.12.1"] (whos out)) "the previous jar runs")
          (is (re-find #"freememo-26.12.2.jar failed: no READY after 2 s; it gets one more start, after the previous jar" out))
          (is (not (.exists (au/failed-file (au/updates-dir data) "26.12.2"))))
          (is (.exists (starts-file data "26.12.2")))))
      (testing "second failed start in a row: marked"
        (let [{:keys [out]} (run-boot {:app app :data data :kp kp :timeout 2})]
          (is (= ["new-26.12.1"] (whos out)))
          (is (re-find #"failed 2 starts in a row, so it is marked" out))
          (is (.exists (au/failed-file (au/updates-dir data) "26.12.2")))))
      (testing "the next start does not try the marked jar"
        (let [{:keys [out]} (run-boot {:app app :data data :kp kp :timeout 2})]
          (is (= ["new-26.12.1"] (whos out)))
          (is (not (str/includes? out "26.12.2")))))
      (finally (t/delete-tree! app) (t/delete-tree! data)))))

(deftest a-ready-clears-the-failed-start-count
  (let [{:keys [app data]} (install! "ready") kp (t/key-pair)]
    (try
      (signed-update! data "26.12.2" "hang" kp)
      (run-boot {:app app :data data :kp kp :timeout 2})
      (is (.exists (starts-file data "26.12.2")))
      (signed-update! data "26.12.2" "ready" kp)
      (is (= ["new-26.12.2"] (whos (:out (run-boot {:app app :data data :kp kp :timeout 2})))))
      (is (not (.exists (starts-file data "26.12.2"))) "READY clears the count")
      (finally (t/delete-tree! app) (t/delete-tree! data)))))

(deftest a-crashing-jar-falls-back-to-the-bundled-jar
  (let [{:keys [app data]} (install! "ready") kp (t/key-pair)]
    (try
      (signed-update! data "26.12.1" "crash" kp)
      (let [{:keys [exit out]} (run-boot {:app app :data data :kp kp})]
        (is (zero? exit) out)
        (is (= ["bundled"] (whos out)))
        (is (re-find #"failed: the app exited with code 3 before READY; it gets one more start, after the previous jar" out)))
      (finally (t/delete-tree! app) (t/delete-tree! data)))))

(deftest when-every-jar-fails-no-marker-stays
  (let [{:keys [app data]} (install! "crash") kp (t/key-pair)]
    (try
      (signed-update! data "26.12.1" "crash" kp)
      (let [{:keys [exit out]} (run-boot {:app app :data data :kp kp})]
        (is (= 3 exit) out)
        (is (empty? (whos out)))
        (is (not (.exists (au/failed-file (au/updates-dir data) "26.12.1"))) "the cause was not the jar")
        (is (not (.exists (starts-file data "26.12.1"))) "and the count is cleared"))
      (finally (t/delete-tree! app) (t/delete-tree! data)))))

(deftest a-jar-with-a-bad-signature-is-deleted-and-never-runs
  (let [{:keys [app data]} (install! "ready") kp (t/key-pair) evil (t/key-pair)]
    (try
      (signed-update! data "26.12.1" "ready" evil)
      (let [{:keys [out]} (run-boot {:app app :data data :kp kp})
            dir (au/updates-dir data)]
        (is (= ["bundled"] (whos out)))
        (is (= 1 (count (re-seq #"deleting freememo-26.12.1.jar: the signature is not valid" out))) "one log line")
        (is (not (.exists (au/jar-file dir "26.12.1")))))
      (finally (t/delete-tree! app) (t/delete-tree! data)))))

(deftest without-a-public-key-or-for-an-old-version-the-bundled-jar-runs
  (let [{:keys [app data]} (install! "ready") kp (t/key-pair)]
    (try
      (signed-update! data "26.12.1" "ready" kp)
      (signed-update! data "26.10.1" "ready" kp)
      (is (= ["bundled"] (whos (:out (run-boot {:app app :data data})))) "no key: App Update is off")
      (is (= ["new-26.12.1"] (whos (:out (run-boot {:app app :data data :kp kp}))))
        "a jar older than the bundled one (26.10.1 < 26.11.1) is never picked")
      (finally (t/delete-tree! app) (t/delete-tree! data)))))

(deftest a-half-written-download-is-not-picked
  (let [{:keys [app data]} (install! "ready") kp (t/key-pair)]
    (try
      (let [dir (au/updates-dir data)]
        (.mkdirs dir)
        (spit (io/file dir ".download-26.12.1.jar.tmp") "half")
        (spit (au/jar-file dir "26.12.2") "no signature"))
      (let [{:keys [out]} (run-boot {:app app :data data :kp kp})]
        (is (= ["bundled"] (whos out))))
      (finally (t/delete-tree! app) (t/delete-tree! data)))))

(deftest exit-code-75-starts-the-newest-jar-again
  (let [{:keys [app data]} (install! "ready") kp (t/key-pair)]
    (try
      (fake-jar! (io/file app "freememo.jar") "bundled" "restart-once")
      (let [{:keys [exit out]} (run-boot {:app app :data data :kp kp})]
        (is (zero? exit) out)
        (is (= ["bundled" "bundled"] (whos out)) "the second run is the restart")
        (is (str/includes? out "restarting to apply an update")))
      (finally (t/delete-tree! app) (t/delete-tree! data)))))

(deftest the-log-file-holds-the-app-output-and-rolls-over-at-5-mb
  (let [{:keys [app data]} (install! "ready")]
    (try
      (testing "a Windows launcher has no console: the output also goes to freememo.log"
        (let [{:keys [exit out]} (run-boot {:app app :data data})
              log (slurp (io/file data "freememo.log"))]
          (is (zero? exit) out)
          (is (re-find #"\[boot\] \S+ FreeMemo 26.11.1 starting" log))
          (is (re-find #"FREEMEMO_READY port=1 pg_port=2 who=bundled" log))))
      (testing "a log over 5 MB moves to freememo.log.1 at the next start"
        (spit (io/file data "freememo.log") (apply str (repeat (* 6 1024 1024) "x")))
        (run-boot {:app app :data data})
        (is (= (* 6 1024 1024) (.length (io/file data "freememo.log.1"))))
        (is (< (.length (io/file data "freememo.log")) 10000)))
      (finally (t/delete-tree! app) (t/delete-tree! data)))))
