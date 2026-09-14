(ns freememo.poem-import-test
  "Import of a `freememo-poem/1` package, and the content-write refusal that
   keeps an imported poem read-only.

   The validation tests are pure and run with the database down. The import tests
   are marked `^:db` and skip with a printed line when no PostgreSQL is up, which
   is the shape `document_summary_test` already uses. Loading this namespace
   reaches `freememo.db` but does not connect, because the pool starts at the
   first query. So the skip sits inside each import test, and the pure tests
   still run.

   The package is built here rather than committed. Its rules are stated as
   verse and note shapes in `plans/poem-package-format.md`, and a binary fixture
   would hide which field a failing rule is reading."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [freememo.db :as db]
            [freememo.import-staging :as staging]
            [freememo.poem-import :as poem]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs])
  (:import [java.io ByteArrayOutputStream]
           [java.util.zip ZipEntry ZipOutputStream]))

;; ── A minimal valid package ────────────────────────────────────────

(def ^:private word
  "One vocalized word: three letters and two marks, a diacritic ratio of 0.667.
   V6's floor is 0.60, so a verse built from these clears it by a margin that no
   rounding can close."
  "قِفَا")

(def ^:private half (str word " " word))

(defn- verse [n]
  {:n n
   :sadr half
   :ajuz half
   :words [{:i 0 :half "sadr" :text word}
           {:i 1 :half "sadr" :text word}
           {:i 2 :half "ajuz" :text word}
           {:i 3 :half "ajuz" :text word}]
   :source {:source_id "s1" :page_id "3" :printed_page 4}})

(defn- note [id verse-n word-i type]
  {:id id :verse verse-n :word word-i :type type
   :html (str "<p dir=\"rtl\">" id "</p>")
   :text id
   :source_id "s1" :page_id "23" :printed_page 23 :part nil})

(def ^:private valid-package
  {:manifest {:format "freememo-poem/1"
              :title "معلقة"
              :poet "امرؤ القيس"
              :meter "الطويل"
              :verse_count 2
              :verse_source_id "37488"
              :unmatched_verses []}
   :verses [(verse 1) (verse 2)]
   :notes [(note "n0001" 1 0 "gloss")
           (note "n0002" 1 2 "iraab")
           (assoc (note "n0003" 2 nil "meaning") :word nil)]
   :sources [{:id "s1" :short "الدرة"
              :title "Book" :author "Author" :shamela_book_id "151017"}]})

(defn- package-zip
  "A .zip holding `pkg`'s four documents at its root, plus one entry the format
   permits and the importer must ignore (§1)."
  ^bytes [pkg]
  (let [out (ByteArrayOutputStream.)]
    (with-open [zos (ZipOutputStream. out)]
      (doseq [[nm v] [["manifest.json" (:manifest pkg)]
                      ["verses.json" (:verses pkg)]
                      ["notes.json" (:notes pkg)]
                      ["sources.json" (:sources pkg)]]]
        (.putNextEntry zos (ZipEntry. ^String nm))
        (.write zos (.getBytes ^String (json/generate-string v) "UTF-8"))
        (.closeEntry zos))
      (.putNextEntry zos (ZipEntry. "README.txt"))
      (.write zos (.getBytes "ignored" "UTF-8"))
      (.closeEntry zos))
    (.toByteArray out)))

;; ── Validation (format doc §6) — pure, no database ─────────────────

(deftest a-minimal-package-passes-every-rule
  (let [r (poem/read-package (package-zip valid-package))]
    (is (:ok r) (str "expected a clean read, got " (:error r)))
    (is (= 2 (count (:verses (:package r)))))
    (is (= 3 (count (:notes (:package r)))))))

(deftest an-extra-entry-is-ignored
  (testing "§1 permits other entries; only the four names are read"
    (is (:ok (poem/read-package (package-zip valid-package))))))

(defn- error-for [pkg] (:error (poem/read-package (package-zip pkg))))

(deftest v5-rejects-a-note-naming-a-verse-that-does-not-exist
  (let [err (error-for (assoc-in valid-package [:notes 0 :verse] 99))]
    (is (str/starts-with? err "V5:") (str "expected a V5 rejection, got " err))
    (is (str/includes? err "n0001") "the message must name the offending note")))

(deftest v5-rejects-a-word-index-that-is-not-a-word-of-its-verse
  (let [err (error-for (assoc-in valid-package [:notes 0 :word] 17))]
    (is (str/starts-with? err "V5:"))
    (is (str/includes? err "17"))))

(deftest v5-rejects-a-null-verse-on-a-note-that-is-not-a-meter-note
  (let [err (error-for (assoc-in valid-package [:notes 0 :verse] nil))]
    (is (str/starts-with? err "V5:"))))

(deftest v2-rejects-a-foreign-format
  (is (str/starts-with? (error-for (assoc-in valid-package [:manifest :format] "other/1"))
        "V2:")))

(deftest v3-rejects-a-gap-in-the-verse-numbers
  (is (str/starts-with? (error-for (assoc-in valid-package [:verses 1 :n] 3)) "V3:")))

(deftest v4-rejects-a-note-naming-an-unknown-source
  (is (str/starts-with? (error-for (assoc-in valid-package [:notes 1 :source_id] "s9"))
        "V4:")))

(deftest v6-rejects-an-undiacritized-verse
  (let [bare "قفا قفا"
        err (error-for (-> valid-package
                         (assoc-in [:verses 0 :sadr] bare)
                         (assoc-in [:verses 0 :ajuz] bare)))]
    (is (str/starts-with? err "V6:"))))

(deftest v7-rejects-a-note-carrying-an-image
  (is (str/starts-with?
        (error-for (assoc-in valid-package [:notes 0 :html]
                     "<p dir=\"rtl\"><img src=\"/api/media/7\"></p>"))
        "V7:")))

(deftest v8-rejects-a-duplicate-note-id
  (is (str/starts-with? (error-for (assoc-in valid-package [:notes 1 :id] "n0001")) "V8:")))

(deftest v1-rejects-a-zip-missing-a-document
  (let [out (ByteArrayOutputStream.)]
    (with-open [zos (ZipOutputStream. out)]
      (.putNextEntry zos (ZipEntry. "manifest.json"))
      (.write zos (.getBytes "{}" "UTF-8"))
      (.closeEntry zos))
    (let [err (:error (poem/read-package (.toByteArray out)))]
      (is (str/starts-with? err "V1:"))
      (is (str/includes? err "verses.json")))))

(deftest v1-rejects-a-document-that-is-not-json
  (let [out (ByteArrayOutputStream.)]
    (with-open [zos (ZipOutputStream. out)]
      (doseq [[nm body] [["manifest.json" "{"] ["verses.json" "[]"]
                         ["notes.json" "[]"] ["sources.json" "[]"]]]
        (.putNextEntry zos (ZipEntry. ^String nm))
        (.write zos (.getBytes ^String body "UTF-8"))
        (.closeEntry zos)))
    (is (str/starts-with? (:error (poem/read-package (.toByteArray out))) "V1:"))))

(deftest the-rendered-fallback-carries-every-verse
  (let [html (poem/render-content (:verses valid-package))]
    (is (= 2 (count (re-seq #"<p dir=\"rtl\">" html))))
    (is (str/includes? html (str half "   " half))
      "the two halves are separated by three spaces (format doc §7)")))

;; ── Import — needs PostgreSQL ──────────────────────────────────────

(defn- db-up? []
  (try (jdbc/execute-one! db/ds ["SELECT 1"]) true (catch Exception _ false)))

(defn- insert-user! []
  (let [tag (str "poem-import-" (System/nanoTime))]
    (:id (jdbc/execute-one! db/ds
           ["INSERT INTO users (username, email) VALUES (?, ?) RETURNING id"
            tag (str tag "@example.test")]
           {:builder-fn rs/as-unqualified-maps}))))

(defn- topic-row [id]
  (jdbc/execute-one! db/ds
    ["SELECT kind, title, content, content_text FROM topics WHERE id = ?" id]
    {:builder-fn rs/as-unqualified-maps}))

(defn- package-count [user-id]
  (:n (jdbc/execute-one! db/ds
        ["SELECT count(*)::int AS n FROM poem_packages WHERE user_id = ?" user-id]
        {:builder-fn rs/as-unqualified-maps})))

(deftest ^:db an-import-creates-one-topic-and-one-package-row
  (if-not (db-up?)
    (println "SKIP poem-import: no PostgreSQL database is available.")
    (do
      (db/setup-schema)
      (let [user-id (insert-user!)]
        (try
          (let [r (poem/import-package! user-id valid-package)]
            (is (:ok r) (str "import failed: " (:error r)))
            (is (= 1 (package-count user-id)) "exactly one package row")

            (testing "the topic is a poem carrying the rendered fallback"
              (let [row (topic-row (:topic-id r))]
                (is (= "poem" (:kind row)))
                (is (= "معلقة" (:title row)))
                (is (str/includes? (:content row) "<p dir=\"rtl\">"))
                (is (str/includes? (:content_text row) word)
                  "content_text is what search reads")))

            (testing "get-poem-package round-trips the four documents (§8)"
              (let [pkg (db/get-poem-package (:topic-id r))]
                (is (= "freememo-poem/1" (:format (:manifest pkg))))
                (is (= 2 (count (:verses pkg))))
                (is (= [0 1 2 3] (mapv :i (:words (first (:verses pkg))))))
                (is (= "sadr" (:half (first (:words (first (:verses pkg)))))))
                (is (= "gloss" (:type (first (:notes pkg))))
                  ":type stays a string")
                (is (= 1 (count (:sources pkg))))))

            (testing "update-topic-content! refuses a poem (spec row 3.2.2)"
              (let [before (:content (topic-row (:topic-id r)))]
                (is (nil? (db/update-topic-content! (:topic-id r) "<p>edited</p>"))
                  "it returns nil rather than throwing")
                (is (= before (:content (topic-row (:topic-id r))))
                  "the poem's content is unchanged")))

            (testing "a topic of another kind still takes a content write"
              (let [other (:id (jdbc/execute-one! db/ds
                                 ["INSERT INTO topics (user_id, kind, title, status)
                                   VALUES (?, 'basic', 'Not a poem', 'active')
                                   RETURNING id" user-id]
                                 {:builder-fn rs/as-unqualified-maps}))]
                (db/update-topic-content! other "<p>edited</p>")
                (is (str/includes? (:content (topic-row other)) "edited")))))

          (testing "a package that breaks V5 is rejected and writes nothing"
            (let [before (package-count user-id)
                  bad (package-zip (assoc-in valid-package [:notes 0 :verse] 99))
                  upload-id (staging/stage! user-id bad "bad.zip" :poem)
                  r (poem/confirm-poem-upload!* user-id upload-id)]
              (is (false? (:ok r)))
              (is (str/starts-with? (:error r) "V5:")
                (str "the rejection must name V5, got " (:error r)))
              (is (= before (package-count user-id))
                "a rejected package leaves no row behind")))

          (testing "confirm claims the staged upload exactly once"
            (let [upload-id (staging/stage! user-id (package-zip valid-package)
                              "poem.zip" :poem)
                  r1 (poem/confirm-poem-upload!* user-id upload-id)
                  r2 (poem/confirm-poem-upload!* user-id upload-id)]
              (is (:ok r1))
              (is (= 2 (package-count user-id)))
              (is (false? (:ok r2)) "a replay imports nothing")
              (is (= 2 (package-count user-id)))))

          (finally
            (jdbc/execute! db/ds ["DELETE FROM users WHERE id = ?" user-id])))))))
