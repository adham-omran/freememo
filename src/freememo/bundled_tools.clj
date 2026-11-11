(ns freememo.bundled-tools
  "The command line tools that an installer bundles: ffmpeg, ffprobe, ImageMagick
   (`magick`) and Graphviz (`sfdp`). The jpackage launcher cannot change PATH,
   and the JDK finds a command with the PATH that the JVM started with. So the
   code that starts one of these tools asks `process-builder`.

   The installer sets the system property `freememo.tools-dir`
   (tools/installer/build.mjs). Layout of that directory:

     bin/          ffmpeg, ffprobe (macOS also magick, sfdp and libs/)
     imagemagick/  ImageMagick; MAGICK_HOME points here (Windows: magick.exe)
     graphviz/     Graphviz plugins and config; GVBINDIR points here
                   (Windows: sfdp.exe and its DLLs)

   Without the property (dev, tests, a server, a Linux package that declares
   its tools as package dependencies), a command keeps its bare name and the
   PATH of the JVM finds it."
  (:require [clojure.java.io :as io])
  (:import [java.io File]))

(def ^:private search-dirs ["bin" "imagemagick" "imagemagick/bin" "graphviz"])

(defn tools-dir
  "Post: the bundled tools directory as a File, or nil when there is none."
  ^File []
  (let [p (System/getProperty "freememo.tools-dir")]
    (when (and p (seq p))
      (let [f (io/file p)] (when (.isDirectory f) f)))))

(defn- executable-in
  "Post: the first file called `command` (or `command.exe`) in `dirs`, or nil."
  [dirs ^String command]
  (first (for [d dirs, n [command (str command ".exe")]
               :let [f (io/file d n)]
               :when (.isFile f)]
           f)))

(defn resolve-command
  "Post: the absolute path of the bundled `command`, or `command` itself when
         `dir` is nil or holds no such tool."
  [^File dir ^String command]
  (if-let [f (and dir (executable-in (map #(io/file dir %) search-dirs) command))]
    (.getAbsolutePath ^File f)
    command))

(defn tool-env
  "Post: the environment variables that the bundled tools need, as a map.
         Empty without `dir`."
  [^File dir]
  (if-not dir
    {}
    (let [dir-of (fn [n] (let [f (io/file dir n)] (when (.isDirectory f) (.getAbsolutePath f))))]
      (cond-> {}
        (dir-of "graphviz") (assoc "GVBINDIR" (dir-of "graphviz"))
        (dir-of "imagemagick") (assoc "MAGICK_HOME" (dir-of "imagemagick"))))))

(defn process-builder
  "Post: a ProcessBuilder for `args` whose first element is the bundled tool when
         one is installed."
  ^ProcessBuilder [args]
  (let [dir (tools-dir)
        [command & more] (vec args)
        pb (ProcessBuilder. ^java.util.List (into [(resolve-command dir command)] more))]
    (doseq [[k v] (tool-env dir)] (.put (.environment pb) k v))
    pb))
