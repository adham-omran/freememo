(ns freememo.supermemo-reference-test
  "Pins the 1-based keying of `reference.dat`.

   SuperMemo numbers references from 1 and stores reference R at byte index
   R-1, the same convention `read-mem` and `read-element-info` follow. Keying a
   record by its own index shifts every citation by one and drops the highest
   reference entirely — an element cites the NEXT article and the last
   reference resolves to nothing. Both halves are asserted here, because the
   shift alone is silent: every row is well-formed, the count is right, and the
   citation renders.

   Measured against a real 37-reference collection: used ids ran 1..37 with no
   gaps over 37 records, and the element titled `Navigate with the sidebar`
   cited `Intro to workspaces` under the old keying."
  (:require
   [clojure.test :refer [deftest is testing]]
   [freememo.supermemo-format])
  (:import
   [java.nio ByteBuffer ByteOrder]
   [java.nio.charset StandardCharsets]))

(def ^:private read-references #'freememo.supermemo-format/read-references)
(def ^:private reference-record-bytes 32)
(def ^:private rtx-trailer-bytes 6)

;; Field index -> byte offset inside a record is (* 4 field); the reader uses
;; 1 title, 2 date, 3 link, 4 source, 7 comment.
(def ^:private title-field 1)
(def ^:private url-field 3)

(defn- text-registry
  "A registry holding `values`, addressable as 1-based member ids.
   An .rtx record is <value> NUL <int32 member-id> <byte flag>, and the .mem
   row's length spans all six trailer bytes — `rtx-value` subtracts them back."
  [values]
  (let [buf (ByteBuffer/allocate (reduce + (map #(+ (count (.getBytes ^String % StandardCharsets/UTF_8))
                                                   rtx-trailer-bytes)
                                             values)))
        _ (.order buf ByteOrder/LITTLE_ENDIAN)
        members (vec (map-indexed
                       (fn [i ^String v]
                         (let [bs (.getBytes v StandardCharsets/UTF_8)
                               offset (.position buf)]
                           (.put buf bs)
                           (.put buf (byte 0))
                           (.putInt buf (inc i))
                           (.put buf (byte 0))
                           {:id (inc i)
                            :rtx-offset (inc offset)   ; .mem offsets are 1-based
                            :rtx-length (+ (alength bs) rtx-trailer-bytes)}))
                       values))]
    {:members members :rtx (.array buf)}))

(defn- reference-dat
  "One 32-byte record per entry. `entries` is a seq of [title-id url-id]."
  [entries]
  (let [buf (doto (ByteBuffer/allocate (* reference-record-bytes (count entries)))
              (.order ByteOrder/LITTLE_ENDIAN))]
    (doseq [[i [title-id url-id]] (map-indexed vector entries)]
      (let [o (* i reference-record-bytes)]
        (.putInt buf (+ o (* 4 title-field)) (int title-id))
        (.putInt buf (+ o (* 4 url-field)) (int url-id))))
    (.array buf)))

;; Three references, whose titles and urls occupy registry ids 1..6.
(def ^:private registry
  (text-registry ["First article"  "https://example.test/first"
                  "Second article" "https://example.test/second"
                  "Third article"  "https://example.test/third"]))

(def ^:private refdat (reference-dat [[1 2] [3 4] [5 6]]))

(deftest reference-one-is-the-record-at-index-zero
  (testing "the first record is reference 1, not reference 0"
    (is (= "First article" (:title (get (read-references refdat registry) 1))))
    (is (= "https://example.test/first" (:url (get (read-references refdat registry) 1))))))

(deftest every-reference-keeps-its-own-record
  (let [refs (read-references refdat registry)]
    (testing "no citation is shifted onto its neighbour"
      (is (= "Second article" (:title (get refs 2))))
      (is (= "Third article" (:title (get refs 3)))))))

(deftest the-highest-reference-is-not-dropped
  (testing "a record count of N yields keys 1..N, so the last element still cites"
    (is (= #{1 2 3} (set (keys (read-references refdat registry)))))))

(deftest empty-records-stay-out-of-the-map
  (testing "an unused slot contributes no key, so `get` distinguishes it from a blank citation"
    (let [with-hole (reference-dat [[1 2] [0 0] [5 6]])]
      (is (= #{1 3} (set (keys (read-references with-hole registry))))))))
