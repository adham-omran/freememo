(ns freememo.content-type-test
  "Archive routing, with the zips built in the test.

   `freememo.content-type` requires only `freememo.archive`, so this suite
   touches no database and needs no PostgreSQL.

   The zips are built here rather than committed because the property under test
   is which ENTRY NAMES a flow claims, and a binary fixture hides that behind a
   checksum. A test that builds its own archive states the input in the same file
   as the expectation."
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.content-type :as ct])
  (:import [java.io ByteArrayOutputStream]
           [java.util.zip ZipEntry ZipOutputStream]))

(defn- zip-bytes
  "A ZIP holding `entries`, a map of entry name to its UTF-8 content."
  ^bytes [entries]
  (let [out (ByteArrayOutputStream.)]
    (with-open [zos (ZipOutputStream. out)]
      (doseq [[nm content] entries]
        (.putNextEntry zos (ZipEntry. ^String nm))
        (.write zos (.getBytes ^String content "UTF-8"))
        (.closeEntry zos)))
    (.toByteArray out)))

(def ^:private poem-entries
  {"manifest.json" "{\"format\":\"freememo-poem/1\"}"
   "verses.json" "[]"
   "notes.json" "[]"
   "sources.json" "[]"})

(deftest a-poem-package-routes-to-poem
  (is (= [:poem nil]
        (ct/classify-archive "poem.zip" (zip-bytes poem-entries)))))

(deftest the-same-package-routes-through-classify-multipart
  (testing "the Poem card posts multipart, so this is the path it actually takes"
    (is (= [:poem nil]
          (ct/classify-multipart "poem.zip" "application/zip"
            (zip-bytes poem-entries))))))

(deftest a-zip-with-only-a-manifest-routes-to-repo
  (testing "one name is not enough — :repo claims every unrecognised .zip"
    (is (= [:repo nil]
          (ct/classify-archive "thing.zip"
            (zip-bytes {"manifest.json" "{}"
                        "src/core.clj" "(ns core)"}))))))

(deftest three-of-the-four-names-route-to-repo
  (doseq [absent (keys poem-entries)]
    (is (= [:repo nil]
          (ct/classify-archive "thing.zip" (zip-bytes (dissoc poem-entries absent))))
      (str "a package missing " absent " must not claim :poem"))))

(deftest the-four-names-must-sit-at-the-zip-root
  (testing "a repository holding docs/notes.json is not a poem package"
    (is (= [:repo nil]
          (ct/classify-archive "repo.zip"
            (zip-bytes {"pkg/manifest.json" "{}"
                        "pkg/verses.json" "[]"
                        "pkg/notes.json" "[]"
                        "pkg/sources.json" "[]"}))))))

(deftest a-more-specific-flow-still-wins-over-poem
  (testing "an account export carrying the four names routes to :account"
    (is (= [:account nil]
          (ct/classify-archive "export.zip"
            (zip-bytes (merge poem-entries
                         {"manifest.edn" "{}"
                          "entities/topics.ndjson" "{}"})))))))

(deftest a-file-that-is-not-a-zip-is-refused
  (is (= :reject (first (ct/classify-archive "poem.zip" (.getBytes "not a zip" "UTF-8"))))))
