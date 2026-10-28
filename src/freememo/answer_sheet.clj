(ns freememo.answer-sheet
  "The printable answer sheet of a comprehension cycle, as a PDF made by Typst.

   `typst` is a binary on PATH, like `ffmpeg` and `sfdp`. The Dockerfile installs
   a pinned release. Absence is a typed failure, never a throw, so the print
   endpoint can fall back to its HTML sheet.

   Typst gets only the fonts this server ships. `--ignore-system-fonts` keeps
   the PDF the same on every host: Typst's built-in Libertinus Serif for Latin
   text, and the vendored Noto Naskh Arabic for Arabic. A host font cannot change
   the page, and a host without fonts (the runtime image) still renders Arabic."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [freememo.ffmpeg :as ffmpeg]
            [taoensso.telemere :as tel])
  (:import [java.util.concurrent TimeUnit]))

(def ^:private template "typst/answer-sheet.typ")

(def ^:private arabic-font
  "Vendored by src-build/vendor.clj. A version bump there changes this path."
  "public/freememo/vendor/noto-naskh-arabic@2.021/NotoNaskhArabic-wght.ttf")

(def ^:private compile-timeout-ms
  "A sheet of 40 questions compiles in well under a second."
  30000)

(defn- rtl-text?
  "Same ranges as `freememo.card-components/rtl-text?`, which requires Electric."
  [text]
  (boolean (and text (re-find #"[؀-ۿݐ-ݿࢠ-ࣿﭐ-﷿ﹰ-﻿֐-׿]" text))))

(defn sheet-data
  "What the sheet shows for one cycle detail.
   Post: only a title, the cycle id, and per question its number, format, points,
   prompt, options and direction. No answer key, rubric or evidence leaves here."
  [detail]
  {:title (str (or (:title detail) "Study cycle"))
   :cycle (:id detail)
   :questions (vec (map-indexed
                     (fn [i question]
                       (let [prompt (str (:prompt question))]
                         {:number (inc i)
                          :format (str (or (:format question) "long-essay"))
                          :points (or (:points question) 0)
                          :prompt prompt
                          :rtl (rtl-text? prompt)
                          :options (vec (for [option (:options question)]
                                          (if (map? option)
                                            {:id (str (:id option)) :text (str (:text option))}
                                            {:id "" :text (str option)})))}))
                     (get-in detail [:form :questions])))})

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
  "Compile dir/sheet.typ to PDF bytes on stdout.
   Post: {:ok true :pdf bytes} or {:ok false :reason :missing-binary|:timeout|:exit
   :error S}. Never throws."
  [^java.io.File dir ^java.io.File fonts]
  (try
    (let [proc (.start (ProcessBuilder.
                         ^java.util.List
                         ["typst" "compile" "--root" (str dir) "--ignore-system-fonts"
                          "--font-path" (str fonts) (str (io/file dir "sheet.typ")) "-"]))
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

(defn render
  "The answer sheet of a cycle detail as PDF bytes.
   Post: {:ok true :pdf bytes}, or {:ok false :reason kw :error S} when Typst or
   the font is missing or the compile fails. A failure is logged here."
  [detail]
  (let [result (if-let [fonts @font-dir]
                 (ffmpeg/with-temp-dir "freememo-answer-sheet"
                   (fn [dir]
                     (spit (io/file dir "sheet.typ") (slurp (io/resource template)))
                     (spit (io/file dir "data.json") (json/generate-string (sheet-data detail)))
                     (run-typst! dir fonts)))
                 {:ok false :reason :missing-font
                  :error (str arabic-font " is not on the classpath. Run clj -X:build vendor/fetch!")})]
    (when-not (:ok result)
      (tel/log! {:level :warn :id ::render-failed
                 :data {:cycle-id (:id detail) :reason (:reason result) :error (:error result)}}
        "Answer sheet PDF failed; serving the HTML sheet"))
    result))
