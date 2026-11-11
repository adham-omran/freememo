(ns freememo.bundled-tools-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [freememo.bundled-tools :as bt])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- tools-dir-with [& files]
  (let [dir (.toFile (Files/createTempDirectory "bt" (make-array FileAttribute 0)))]
    (doseq [f files]
      (let [file (io/file dir f)]
        (io/make-parents file)
        (spit file "x")))
    dir))

(deftest resolves-bundled-tool-or-keeps-the-name
  (let [dir (tools-dir-with "bin/ffmpeg" "imagemagick/magick.exe" "graphviz/config8")]
    (testing "a tool in bin/"
      (is (= (.getAbsolutePath (io/file dir "bin" "ffmpeg")) (bt/resolve-command dir "ffmpeg"))))
    (testing "a Windows tool gets its .exe"
      (is (= (.getAbsolutePath (io/file dir "imagemagick" "magick.exe")) (bt/resolve-command dir "magick"))))
    (testing "a tool that is not bundled keeps its name, so PATH finds it"
      (is (= "sfdp" (bt/resolve-command dir "sfdp")))
      (is (= "ffprobe" (bt/resolve-command nil "ffprobe"))))))

(deftest tool-env-points-at-bundled-directories
  (let [dir (tools-dir-with "imagemagick/magick.exe" "graphviz/config8")]
    (is (= {"GVBINDIR" (.getAbsolutePath (io/file dir "graphviz"))
            "MAGICK_HOME" (.getAbsolutePath (io/file dir "imagemagick"))}
           (bt/tool-env dir)))
    (is (= {} (bt/tool-env nil)))
    (is (= {} (bt/tool-env (tools-dir-with "bin/ffmpeg"))))))

(deftest process-builder-without-tools-dir-is-plain
  (is (= ["ffmpeg" "-version"] (vec (.command (bt/process-builder ["ffmpeg" "-version"]))))))
