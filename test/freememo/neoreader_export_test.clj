(ns freememo.neoreader-export-test
  "Asserts freememo.neoreader-export/parse-highlights against the shape of a
   real Boox NeoReader export.

   Needs no database. Every fixture is hand-authored to match the markup of the
   reference export recorded in plans/boox-neoreader-google-drive-sync.md, and
   each one carries the spec item it demonstrates.

   Why fixtures and not the whole reference file: the export is 200 highlights
   of copyrighted book text. The structural facts a decoder can get wrong are
   all reproducible in a few blocks."
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.neoreader-export :as export]))

;; ── Fixture builders — the export's own markup, abbreviated ─────────

(defn- chapter-div [title]
  (str "<div style=\"font-size: 14pt; text-align: left;\">"
    "<span style=\"font-weight: bold; color: #48b4c1;\">" title "</span></div>"))

(defn- highlight-div [stamp quote]
  (str "<div style=\"padding-top: 1em; border-top: 1px dotted lightgray;\">"
    "<div style=\"font-size: 10pt; border-left: 5px solid #ed6c00; color: #888888;\">"
    stamp "</div>"
    "<div style=\"font-size: 12pt;\"><span>" quote "</span></div></div>"))

(defn- note-div [text]
  (str "<div><span style=\"color: #a9a9a9;\">注 | </span>"
    "<span style=\"color: #555555;\">" text "</span></div>"))

(def ^:private footer-div
  "The trailing band. Reproduced verbatim from the reference export, including
   the device name `Poke5P` — which does not match the `PokeP5` folder name."
  (str "<div style=\"margin-top: 2em;\"><hr style=\"height: 2px;\"/>"
    "<span style=\"font-size: 10pt; color: gray;\">"
    "BOOX Reading Notes from Poke5P</span></div>"))

(defn- export-doc [& body]
  (str "<!DOCTYPE html><html><head><title>T</title></head><body>"
    "<h2 style=\"font-size: 18pt;\">Make It Stick - Brown, Peter C_</h2>"
    "<h5 style=\"font-size: 12pt;\">Brown, Peter C.</h5>"
    (apply str body)
    footer-div
    "</body></html>"))

;; ── 3.2 — chapters, order, and note attachment ─────────────────────

(deftest parses-chapters-highlights-and-notes
  (testing "3.2 — every highlight carries its chapter, quote and stamp"
    (let [hs (export/parse-highlights
               (export-doc
                 (chapter-div "Preface")
                 (highlight-div "2026-05-26 20:47:30" "spaced repetition of key ideas")
                 (chapter-div "1. Learning Is Misunderstood")
                 (highlight-div "2026-05-26 20:52:24" "we mean acquiring knowledge")))]
      (is (= 2 (count hs)))
      (is (= ["Preface" "1. Learning Is Misunderstood"] (mapv :chapter hs)))
      (is (= ["2026-05-26 20:47:30" "2026-05-26 20:52:24"] (mapv :timestamp hs)))
      (is (= ["spaced repetition of key ideas" "we mean acquiring knowledge"]
            (mapv :quote hs)))
      (is (= [0 1] (mapv :index hs)) ":index counts from 0 in document order")))

  (testing "3.2 — a note attaches to the highlight it follows, not the next one"
    (let [hs (export/parse-highlights
               (export-doc
                 (chapter-div "3. Mix Up Your Practice")
                 (highlight-div "2026-05-31 19:17:01" "Is it still there when you need it")
                 (note-div "why The quickness of picking up a skill is not a great indicator.")
                 (highlight-div "2026-05-31 19:18:27" "practice is far more effective")))]
      (is (= ["why The quickness of picking up a skill is not a great indicator."]
            (:notes (first hs))))
      (is (= [] (:notes (second hs))) "the following highlight keeps no note")))

  (testing "3.2 — a note before any highlight has nothing to attach to"
    (let [hs (export/parse-highlights
               (export-doc (note-div "orphan") (highlight-div "2026-01-01 00:00:00" "q")))]
      (is (= 1 (count hs)))
      (is (= [] (:notes (first hs))))))

  (testing "3.2 — a highlight before any chapter heading carries a nil chapter"
    ;; Regression: the export opens with <h2> title and <h5> author as direct
    ;; children of body. Reading any non-blank child as a heading made the
    ;; author the chapter in force, so this highlight claimed a chapter titled
    ;; "Brown, Peter C." that no EPUB holds.
    (let [hs (export/parse-highlights
               (export-doc (highlight-div "2026-01-01 00:00:00" "preamble quote")))]
      (is (nil? (:chapter (first hs))))))

  (testing "3.2 — the book title and author never become a chapter"
    (let [hs (export/parse-highlights
               (export-doc
                 (chapter-div "Preface")
                 (highlight-div "2026-01-01 00:00:00" "q")))]
      (is (= ["Preface"] (mapv :chapter hs)))
      (is (not-any? #{"Make It Stick - Brown, Peter C_" "Brown, Peter C."}
            (map :chapter hs))))))

;; ── 3.3 — document order, never timestamp order ────────────────────

(deftest keeps-document-order-when-timestamps-descend
  (testing "3.3 — chapter 5 of the reference export descends within a chapter"
    (let [hs (export/parse-highlights
               (export-doc
                 (chapter-div "5. Avoid Illusions of Knowing")
                 (highlight-div "2026-06-13 20:59:56" "Pay attention to the cues")
                 (highlight-div "2026-06-13 20:59:33" "How ably you can explain a text")))]
      (is (= ["2026-06-13 20:59:56" "2026-06-13 20:59:33"] (mapv :timestamp hs))
        "a decoder that sorted by timestamp would swap these")))

  (testing "3.3 — the Notes chapter carries June stamps after an August stamp"
    (let [hs (export/parse-highlights
               (export-doc
                 (chapter-div "8. Make It Stick")
                 (highlight-div "2026-08-05 16:02:32" "Explain to Students How Learning Works")
                 (chapter-div "Notes")
                 (highlight-div "2026-06-13 10:45:53" "The story about Don Thomson")))]
      (is (= ["8. Make It Stick" "Notes"] (mapv :chapter hs)))
      (is (= [0 1] (mapv :index hs))))))

;; ── 3.4 — more than one note on a highlight ────────────────────────

(deftest accepts-two-notes-on-one-highlight
  (testing "3.4 — two consecutive note blocks both survive"
    (let [hs (export/parse-highlights
               (export-doc
                 (highlight-div "2026-01-01 00:00:00" "q")
                 (note-div "first")
                 (note-div "second")))]
      (is (= ["first" "second"] (:notes (first hs)))
        "neither note is dropped and neither overwrites the other")))

  (testing "3.4 — a blank note block contributes nothing"
    (let [hs (export/parse-highlights
               (export-doc (highlight-div "2026-01-01 00:00:00" "q") (note-div "   ")))]
      (is (= [] (:notes (first hs)))))))

;; ── 3.5 — the footer is discarded ──────────────────────────────────

(deftest discards-the-footer
  (testing "3.5 — the trailing band reaches no highlight, note or chapter"
    (let [hs (export/parse-highlights
               (export-doc (chapter-div "Suggested Reading")
                 (highlight-div "2026-08-05 16:04:51" "Suggested Reading")))
          all (str (mapv :quote hs) (mapv :notes hs) (mapv :chapter hs))]
      (is (= 1 (count hs)))
      (is (not (re-find #"BOOX Reading Notes" all)))
      (is (not (re-find #"Poke5P" all)))))

  (testing "3.5 — an <hr> earlier in the file skips that block without truncating"
    (let [hs (export/parse-highlights
               (export-doc
                 (highlight-div "2026-01-01 00:00:00" "before")
                 "<div><hr/><span>stray rule</span></div>"
                 (highlight-div "2026-01-02 00:00:00" "after")))]
      (is (= ["before" "after"] (mapv :quote hs))
        "a skip, not a stop — the second highlight still decodes"))))

;; ── Decoder robustness ─────────────────────────────────────────────

(deftest handles-degenerate-input
  (testing "nil and blank yield an empty vector, not a crash"
    (is (= [] (export/parse-highlights nil)))
    (is (= [] (export/parse-highlights "")))
    (is (= [] (export/parse-highlights "   "))))

  (testing "a quote that mentions a date is not mistaken for the stamp"
    (let [hs (export/parse-highlights
               (export-doc
                 (highlight-div "2026-01-01 00:00:00"
                   "In 2013-05-06 07:08:09 he published a piece")))]
      (is (= 1 (count hs)))
      (is (= "2026-01-01 00:00:00" (:timestamp (first hs))))
      (is (= "In 2013-05-06 07:08:09 he published a piece" (:quote (first hs)))
        "the date inside the quote survives, and the stamp does not leak in"))))
