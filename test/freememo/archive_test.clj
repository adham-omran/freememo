(ns freememo.archive-test
  "Guards and source-agnosticism for `freememo.archive`.

   The File arities exist because a chunked upload lands archives too large for
   a byte[] (plans/supermemo-import-large-archives.md §6.3). Every test that
   matters therefore runs BOTH ways and asserts the same answer — a File path
   that silently diverges from the byte[] path is the failure this file is
   here to catch."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [freememo.archive :as archive]
            [freememo.content-type :as ct])
  (:import [java.io ByteArrayOutputStream File]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]
           [java.util.zip ZipEntry ZipOutputStream]
           [org.apache.commons.compress.archivers.sevenz
            SevenZArchiveEntry SevenZOutputFile]))

;; ── Fixtures ───────────────────────────────────────────────────────

(defn- zip-bytes
  "ZIP of `entries` — a seq of [name content-string-or-bytes]."
  ^bytes [entries]
  (let [baos (ByteArrayOutputStream.)]
    (with-open [zos (ZipOutputStream. baos)]
      (doseq [[nm content] entries]
        (.putNextEntry zos (ZipEntry. ^String nm))
        (let [^bytes b (if (bytes? content) content (.getBytes ^String content "UTF-8"))]
          (.write zos b 0 (alength b)))
        (.closeEntry zos)))
    (.toByteArray baos)))

(defn- temp-file
  "Write `b` to a fresh temp file and return it."
  ^File [^bytes b]
  (let [f (.toFile (Files/createTempFile "archive-test" ".bin"
                     (make-array FileAttribute 0)))]
    (io/copy b f)
    f))

(defn- sevenz-file
  "7z on disk holding `entries`. Returns the File."
  ^File [entries]
  (let [f (.toFile (Files/createTempFile "archive-test" ".7z"
                     (make-array FileAttribute 0)))]
    (.delete f)                       ; SevenZOutputFile wants to create it
    (with-open [out (SevenZOutputFile. f)]
      (doseq [[nm content] entries]
        (let [^bytes b (.getBytes ^String content "UTF-8")
              e (doto (SevenZArchiveEntry.)
                  (.setName nm)
                  (.setSize (alength b)))]
          (.putArchiveEntry out e)
          (.write out b 0 (alength b))
          (.closeArchiveEntry out))))
    f))

(def ^:private supermemo-entries
  "The two entries `classify-archive` requires, plus filler."
  [["coll/info/contents.dat" "x"]
   ["coll/info/ElementInfo.dat" "y"]
   ["coll/elements/1.htm" "<p>hi</p>"]])

(defn- tree
  "Relative paths of every file under `dir`, sorted."
  [^File dir]
  (let [base (inc (count (.getCanonicalPath dir)))]
    (->> (file-seq dir)
      (filter #(.isFile ^File %))
      (map #(subs (.getCanonicalPath ^File %) base))
      sort
      vec)))

;; ── Source-agnosticism: byte[] and File must agree ─────────────────

(deftest archive-kind-reads-both-sources
  (let [zb (zip-bytes [["a.txt" "a"]])]
    (is (= :zip (archive/archive-kind zb)))
    (is (= :zip (archive/archive-kind (temp-file zb))))
    (is (= :sevenz (archive/archive-kind (sevenz-file [["a.txt" "a"]]))))
    (testing "neither container"
      (is (nil? (archive/archive-kind (.getBytes "not an archive"))))
      (is (nil? (archive/archive-kind (temp-file (.getBytes "not an archive")))))
      (is (nil? (archive/archive-kind (temp-file (byte-array 0))))))))

(deftest entry-names-reads-both-sources
  (let [zb (zip-bytes supermemo-entries)
        expected ["coll/info/contents.dat" "coll/info/ElementInfo.dat" "coll/elements/1.htm"]]
    (is (= expected (archive/entry-names zb)))
    (is (= expected (archive/entry-names (temp-file zb))))
    (testing "unreadable source is empty, not an exception"
      (is (= [] (archive/entry-names (.getBytes "junk"))))
      (is (= [] (archive/entry-names (temp-file (.getBytes "junk"))))))))

(deftest extraction-reads-both-sources
  (let [zb (zip-bytes supermemo-entries)
        from-bytes (archive/extract-to-temp-dir! zb {})
        from-file (archive/extract-to-temp-dir! (temp-file zb) {})]
    (try
      (is (= (tree from-bytes) (tree from-file)))
      (is (= 3 (count (tree from-file))))
      (is (= "<p>hi</p>" (slurp (io/file from-file "coll/elements/1.htm"))))
      (finally
        (archive/delete-dir! from-bytes)
        (archive/delete-dir! from-file)))))

(deftest sevenz-extracts-from-a-file
  ;; §6.3 3.3 — `SevenZFile$Builder.setFile`, so the archive is never in heap.
  (let [f (sevenz-file [["x/a.txt" "alpha"] ["x/b.txt" "beta"]])
        dir (archive/extract-to-temp-dir! f {})]
    (try
      (is (= ["x/a.txt" "x/b.txt"] (tree dir)))
      (is (= "alpha" (slurp (io/file dir "x/a.txt"))))
      (finally (archive/delete-dir! dir)))))

;; ── Guards (§6.3 3.4) ──────────────────────────────────────────────

(deftest traversal-guard-writes-nothing-outside
  (let [zb (zip-bytes [["../escaped.txt" "nope"] ["ok.txt" "yes"]])
        dir (archive/extract-to-temp-dir! zb {})]
    (try
      (is (= ["ok.txt"] (tree dir)) "the ../ entry is skipped, not written")
      (is (not (.exists (io/file (.getParentFile dir) "escaped.txt"))))
      (finally (archive/delete-dir! dir)))))

(deftest entry-count-guard-throws
  (let [zb (zip-bytes (for [i (range 12)] [(str "f" i ".txt") "x"]))]
    (is (thrown? clojure.lang.ExceptionInfo
          (archive/extract-to-temp-dir! zb {:max-entries 5})))
    (testing "and leaves no temp dir behind"
      (is (= ::archive/archive-too-large
            (try (archive/extract-to-temp-dir! zb {:max-entries 5})
                 (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))))))

(deftest total-bytes-guard-throws
  (let [zb (zip-bytes [["a.txt" (apply str (repeat 500 "a"))]
                       ["b.txt" (apply str (repeat 500 "b"))]])]
    (is (= ::archive/archive-too-large
          (try (archive/extract-to-temp-dir! zb {:max-total-bytes 600})
               (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))))))

(deftest per-entry-guard-throws-and-names-the-entry
  (let [zb (zip-bytes [["big.bin" (apply str (repeat 2000 "z"))]])]
    (is (= ["big.bin" ::archive/archive-too-large]
          (try (archive/extract-to-temp-dir! zb {:max-entry-bytes 100})
               (catch clojure.lang.ExceptionInfo e
                 [(:entry (ex-data e)) (:type (ex-data e))]))))))

(deftest unsupported-container-throws
  (is (= ::archive/unsupported-archive
        (try (archive/extract-to-temp-dir! (.getBytes "plain text") {})
             (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))))

(deftest keep-predicate-filters-and-skips-the-entry-cap
  ;; §6.5 5.1 relies on this: a filtered entry goes through `skip-entry!`, which
  ;; does not apply `max-entry-bytes`. That is what would let a media filter drop
  ;; slots larger than the cap instead of failing the whole extraction.
  (let [zb (zip-bytes [["keep.txt" "small"]
                       ["drop.bin" (apply str (repeat 2000 "z"))]])
        dir (archive/extract-to-temp-dir! zb
              {:keep? #(clojure.string/ends-with? % ".txt")
               :max-entry-bytes 100})]
    (try
      (is (= ["keep.txt"] (tree dir)))
      (finally (archive/delete-dir! dir)))))

(deftest entry-larger-than-the-old-heap-cap-extracts
  ;; §6.3 3.2 — `copy-entry!` streams to disk now. While it buffered each entry
  ;; in a ByteArrayOutputStream, the per-entry cap was the only thing between a
  ;; large member and the heap, so raising that cap was unsafe. 24 MB is past the
  ;; old 20 MB default and stays small enough to keep the suite quick.
  (let [big (byte-array (* 24 1024 1024) (byte 7))
        zb (zip-bytes [["big.bin" big]])
        dir (archive/extract-to-temp-dir! zb
              {:max-entry-bytes (* 64 1024 1024)
               :max-total-bytes (* 64 1024 1024)})]
    (try
      (is (= (* 24 1024 1024) (.length (io/file dir "big.bin"))))
      (finally (archive/delete-dir! dir)))))

;; ── every-entry-name? (§6.4) ───────────────────────────────────────

(deftest every-entry-name-requires-all-predicates
  (let [sm (zip-bytes supermemo-entries)
        partial (zip-bytes [["coll/info/contents.dat" "x"]])
        preds [#(clojure.string/ends-with? % "info/contents.dat")
               #(clojure.string/ends-with? % "info/elementinfo.dat")]]
    (is (true? (archive/every-entry-name? sm preds)))
    (is (true? (archive/every-entry-name? (temp-file sm) preds)))
    (is (false? (archive/every-entry-name? partial preds))
      "a lone contents.dat must not match — that is why both names are required")
    (is (false? (archive/every-entry-name? (.getBytes "junk") preds))
      "unreadable source is false, not an exception")))

;; ── classify-archive (§6.4 4.1) ────────────────────────────────────

(deftest classify-archive-agrees-across-sources
  (let [sm (zip-bytes supermemo-entries)
        repo (zip-bytes [["src/core.clj" "(ns core)"]])]
    (testing "SuperMemo collection wins over repo"
      (is (= [:supermemo nil] (ct/classify-archive "c.zip" sm)))
      (is (= [:supermemo nil] (ct/classify-archive "c.zip" (temp-file sm)))))
    (testing "every other zip is a repo"
      (is (= [:repo nil] (ct/classify-archive "r.zip" repo)))
      (is (= [:repo nil] (ct/classify-archive "r.zip" (temp-file repo)))))
    (testing "a zip that is not a zip"
      (is (= :reject (first (ct/classify-archive "r.zip" (.getBytes "junk"))))))))

(deftest classify-archive-7z-is-supermemo-only
  (let [sm (sevenz-file [["coll/info/contents.dat" "x"]
                         ["coll/info/ElementInfo.dat" "y"]])
        other (sevenz-file [["notes.txt" "hello"]])]
    (is (= [:supermemo nil] (ct/classify-archive "c.7z" sm)))
    (is (= :reject (first (ct/classify-archive "n.7z" other))))
    (is (= :reject (first (ct/classify-archive "n.7z" (temp-file (.getBytes "junk"))))))))
