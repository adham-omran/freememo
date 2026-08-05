(ns freememo.archive-zip64-test
  "ZIP64 round trip: `ZipOutputStream` out, `freememo.archive` back in.

   plans/user-data-export-import.md §5.8.1. The user-data export includes video
   bytes, so a single entry passes 4 GiB and the archive leaves the 32-bit ZIP
   fields behind. `archive/read-source` reads ZIP with `ZipInputStream`, which
   consumes local headers only — a ZIP64 entry declares 0xFFFFFFFF there and
   puts the real sizes in a data descriptor after the compressed data. Whether
   the reader recovers its stream position from that is what decides the format,
   so it is verified before anything is built on top of it.

   The discriminator is `after-big.txt`, written AFTER the oversized entry. A
   reader that mishandles the ZIP64 descriptor loses the stream position and
   never reaches it, so its absence is the failure signal. Reading the oversized
   entry back without an exception proves much less.

   `oversized-round-trip-drains-and-finds-the-next-entry` needs no free disk: it
   filters the big entry out, so `archive/skip-entry!` drains it and only the
   sentinel lands. `oversized-entry-extracts-byte-identical` writes the whole
   entry and needs more than 4 GiB of temp space, so it runs only when
   ZIP64_EXTRACT_TEST is set. Set TMPDIR too when java.io.tmpdir is a tmpfs —
   4 GiB of extraction there is 4 GiB of RAM."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [freememo.archive :as archive])
  (:import [java.io BufferedOutputStream File FileInputStream FileOutputStream]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]
           [java.security DigestInputStream MessageDigest]
           [java.util.zip ZipEntry ZipOutputStream]))

;; ── Fixture ────────────────────────────────────────────────────────

(def ^:private oversized-entry-bytes
  "Uncompressed size of the big entry. One byte past 0xFFFFFFFF is the smallest
   value that forces ZIP64, and the smallest is what keeps the test runnable.
   Override with ZIP64_TEST_BYTES to shrink it while iterating — a smaller value
   tests nothing about ZIP64, so a green run under an override proves nothing."
  (or (some-> (System/getenv "ZIP64_TEST_BYTES") parse-long)
    (inc 0xFFFFFFFF)))

(def ^:private fill-byte
  "Every byte of the big entry. A constant deflates to almost nothing, so the
   archive on disk stays a few MB while the entry it declares passes 4 GiB."
  (byte 0x5A))

(def ^:private sentinel-name "after-big.txt")
(def ^:private sentinel-content "the reader kept its place")

(defn- write-oversized-zip!
  "ZIP holding `big.bin` of `n` bytes, then the sentinel entry. Returns the File.

   Pre : `n` > 0. The caller owns the returned File and deletes it.
   Post: a DEFLATED ZIP. `big.bin` precedes the sentinel in stream order, which
         is the whole point — see the namespace docstring.
   Invariant: heap high-water is one 1 MiB buffer, whatever `n` is."
  ^File [^long n]
  (let [f (.toFile (Files/createTempFile "zip64-test" ".zip"
                     (make-array FileAttribute 0)))
        chunk (byte-array (* 1024 1024) fill-byte)]
    (with-open [out (BufferedOutputStream. (FileOutputStream. f))
                zos (ZipOutputStream. out)]
      (.putNextEntry zos (ZipEntry. "big.bin"))
      (loop [written 0]
        (when (< written n)
          (let [len (int (min (alength chunk) (- n written)))]
            (.write zos chunk 0 len)
            (recur (+ written len)))))
      (.closeEntry zos)
      (.putNextEntry zos (ZipEntry. sentinel-name))
      (let [^bytes b (.getBytes ^String sentinel-content "UTF-8")]
        (.write zos b 0 (alength b)))
      (.closeEntry zos))
    f))

(defn- sha256-of
  "Hex SHA-256 of `f`, read in 1 MiB steps.
   Pre : `f` is a readable File. Post: a 64-character lower-case hex string."
  [^File f]
  (let [md (MessageDigest/getInstance "SHA-256")
        buf (byte-array (* 1024 1024))]
    (with-open [in (DigestInputStream. (FileInputStream. f) md)]
      (while (pos? (.read in buf))))
    (str/join (map #(format "%02x" %) (.digest md)))))

(defn- expected-sha256
  "Hex SHA-256 of `n` repetitions of `fill-byte`, computed without a file.
   Pre : n >= 0. Post: a 64-character lower-case hex string."
  [^long n]
  (let [md (MessageDigest/getInstance "SHA-256")
        chunk (byte-array (* 1024 1024) fill-byte)]
    (loop [done 0]
      (when (< done n)
        (let [len (int (min (alength chunk) (- n done)))]
          (.update md chunk 0 len)
          (recur (+ done len)))))
    (str/join (map #(format "%02x" %) (.digest md)))))

;; ── The go/no-go test ──────────────────────────────────────────────

(deftest oversized-round-trip-drains-and-finds-the-next-entry
  (let [zip (write-oversized-zip! oversized-entry-bytes)]
    (try
      (testing "the writer produced a ZIP the reader recognises"
        (is (= :zip (archive/archive-kind zip))))

      (testing "the header walk reaches the entry after the oversized one"
        ;; entry-names decompresses nothing, so this isolates header traversal
        ;; from entry payload handling.
        (is (= ["big.bin" sentinel-name] (archive/entry-names zip))))

      (testing "draining the oversized entry leaves the stream on the sentinel"
        (let [dir (archive/extract-to-temp-dir! zip
                    {:keep? #(= % sentinel-name)
                     :max-entries 10
                     :max-entry-bytes (* 1024 1024)
                     :max-total-bytes (* 1024 1024)})]
          (try
            (is (= [sentinel-name]
                  (->> (file-seq dir)
                    (filter #(.isFile ^File %))
                    (map #(.getName ^File %))
                    sort
                    vec)))
            (is (= sentinel-content (slurp (io/file dir sentinel-name)))
              "a truncated drain corrupts the entry that follows")
            (finally (archive/delete-dir! dir)))))
      (finally (.delete zip)))))

(deftest oversized-entry-extracts-byte-identical
  ;; Needs more than `oversized-entry-bytes` of free space under java.io.tmpdir,
  ;; so it is opt-in. The test above already proves the reader keeps its place.
  (if-not (System/getenv "ZIP64_EXTRACT_TEST")
    (is true "skipped — set ZIP64_EXTRACT_TEST to run the full extraction")
    (let [zip (write-oversized-zip! oversized-entry-bytes)
          cap (* 2 oversized-entry-bytes)]
      (try
        (let [dir (archive/extract-to-temp-dir! zip
                    {:max-entries 10
                     :max-entry-bytes cap
                     :max-total-bytes cap})]
          (try
            (let [big (io/file dir "big.bin")]
              (is (= oversized-entry-bytes (.length big))
                "length must survive the ZIP64 round trip")
              (is (= (expected-sha256 oversized-entry-bytes) (sha256-of big))
                "content must survive the ZIP64 round trip"))
            (finally (archive/delete-dir! dir))))
        (finally (.delete zip))))))
