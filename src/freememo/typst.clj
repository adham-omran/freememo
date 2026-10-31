(ns freememo.typst
  "Compile a Typst template with JSON data to PDF bytes.

   `typst` is a binary on PATH, like `ffmpeg` and `sfdp`. The Dockerfile installs
   a pinned release. Absence is a typed failure, never a throw, so a caller can
   fall back or retry.

   Typst gets only the fonts this server ships. `--ignore-system-fonts` keeps
   the PDF the same on every host: Typst's built-in Libertinus Serif for Latin
   text, and the vendored Noto Naskh Arabic for Arabic. A host font cannot change
   the page, and a host without fonts (the runtime image) still renders Arabic."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [freememo.ffmpeg :as ffmpeg])
  (:import [java.util.concurrent TimeUnit]))

(def ^:private arabic-font
  "Vendored by src-build/vendor.clj. A version bump there changes this path."
  "public/freememo/vendor/noto-naskh-arabic@2.021/NotoNaskhArabic-wght.ttf")

(def ^:private compile-timeout-ms
  "A sheet of 40 questions compiles in well under a second."
  30000)

(defonce ^:private font-dir
  ;; The font lives in the jar in production, and Typst needs a directory on
  ;; disk. Copy it once per JVM. Nil when the vendor tree was never fetched.
  (delay
    (when-let [resource (io/resource arabic-font)]
      (let [dir (.toFile (java.nio.file.Files/createTempDirectory "freememo-typst-fonts"
                           (make-array java.nio.file.attribute.FileAttribute 0)))
            file (io/file dir "NotoNaskhArabic-wght.ttf")]
        (with-open [in (io/input-stream resource)] (io/copy in file))
        (.deleteOnExit file)
        (.deleteOnExit dir)
        dir))))

(defn- run-typst!
  "Compile dir/main.typ to PDF bytes on stdout.
   Post: {:ok true :pdf bytes} or {:ok false :reason :missing-binary|:timeout|:exit
   :error S}. Never throws."
  [^java.io.File dir ^java.io.File fonts]
  (try
    (let [proc (.start (ProcessBuilder.
                         ^java.util.List
                         ["typst" "compile" "--root" (str dir) "--ignore-system-fonts"
                          "--font-path" (str fonts) (str (io/file dir "main.typ")) "-"]))
          out (future (.readAllBytes (.getInputStream proc)))
          err (future (slurp (.getErrorStream proc)))]
      (if-not (.waitFor proc compile-timeout-ms TimeUnit/MILLISECONDS)
        (do (.destroyForcibly proc)
            {:ok false :reason :timeout :error "Typst timed out"})
        (if (zero? (.exitValue proc))
          {:ok true :pdf (deref out 5000 nil)}
          {:ok false :reason :exit :error (deref err 5000 "")})))
    (catch java.io.IOException _
      {:ok false :reason :missing-binary :error "typst is not installed on this server"})
    (catch Exception e
      {:ok false :reason :exit :error (ex-message e)})))

(defn render-pdf
  "Compile the template at classpath `template` with `data` as /data.json.
   Pre:  `template` names a .typ resource that reads json(\"/data.json\") and
         evaluates none of its values; `data` encodes as JSON.
   Post: {:ok true :pdf bytes}, or {:ok false :reason kw :error S} when Typst or
         the font is missing or the compile fails. Never throws. Logging the
         failure is the caller's job, because only the caller knows what it
         falls back to."
  [template data]
  (if-let [fonts @font-dir]
    (ffmpeg/with-temp-dir "freememo-typst"
      (fn [dir]
        (spit (io/file dir "main.typ") (slurp (io/resource template)))
        (spit (io/file dir "data.json") (json/generate-string data))
        (run-typst! dir fonts)))
    {:ok false :reason :missing-font
     :error (str arabic-font " is not on the classpath. Run clj -X:build vendor/fetch!")}))
