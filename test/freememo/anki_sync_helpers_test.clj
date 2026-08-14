(ns freememo.anki-sync-helpers-test
  "Covers the Anki push's tag merge — the union that keeps a note's Marked
   status (the tag `marked`) alive across a re-push.

   AnkiConnect replaces a note's whole tag list whenever the request carries a
   `tags` key, and do-anki-push! re-pushes every already-synced card on every
   push. Sending the settings list bare therefore deleted the user's Marked,
   leech and hand-added tags. See plans/anki-push-tag-merge.md.

   Only the two pure fns are covered. do-anki-push! itself is CLJS-only and is
   verified against a real Anki collection by hand (checklist row 1.17). The
   note-not-reported gate that keeps an unknown tag list away from those fns
   lives inside it, so no test here reaches that branch.

   pre : the working directory is the repo root — t10 reads the push source as
         text, the way account_transfer_format_test reads db.clj.
   post: needs no database. freememo.anki-sync-helpers reaches no namespace that
         builds a connection pool at load."
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.anki-sync-helpers :as helpers]))

;; ── merge-note-tags ─────────────────────────────────────────────────────────

(deftest t1-marked-survives-a-push
  (testing "the defect: a marked note re-pushed keeps its mark"
    (is (= ["marked"] (helpers/merge-note-tags ["marked"] [])))
    (is (= ["marked" "physics"] (helpers/merge-note-tags ["marked"] ["physics"])))))

(deftest t2-empty-settings-tags-are-tag-neutral
  (testing "tags off sends [] — that must not shorten the list"
    (is (= ["marked" "leech" "mine"]
          (helpers/merge-note-tags ["marked" "leech" "mine"] [])))
    (is (= ["marked"] (helpers/merge-note-tags ["marked"] nil)))))

(deftest t3-case-insensitive-anki-spelling-wins
  (testing "Anki matches tags without regard to case — one tag, Anki's casing"
    (is (= ["marked"] (helpers/merge-note-tags ["marked"] ["Marked"])))
    (is (= ["Marked"] (helpers/merge-note-tags ["Marked"] ["marked"])))
    (testing "and among the added tags themselves"
      (is (= ["physics"] (helpers/merge-note-tags [] ["physics" "Physics"]))))))

(deftest t4-order-is-current-then-added
  (is (= ["marked" "a" "b"] (helpers/merge-note-tags ["marked"] ["a" "b"]))))

(deftest t5-added-tags-are-trimmed-blanks-dropped
  (is (= ["x"] (helpers/merge-note-tags [] ["  x  " "" "   " nil]))))

(deftest t6-a-push-never-shortens-a-tag-list
  (testing "the invariant, over every shape the push can hand in"
    (doseq [current [[] ["marked"] ["marked" "leech"] ["a" "b" "c"]]
            added [nil [] ["a"] ["marked"] ["A" "a"] ["" "  "]]]
      (let [merged (helpers/merge-note-tags current added)]
        (is (>= (count merged) (count current))
          (str "shortened: " (pr-str current) " + " (pr-str added)))
        (is (every? (set merged) current)
          (str "dropped a current tag: " (pr-str current) " + " (pr-str added)))))))

(deftest t7-a-note-with-no-tags-takes-the-settings-tags
  (testing "an empty current list is a real merge input, not a missing one"
    (is (= ["physics"] (helpers/merge-note-tags [] ["physics"])))
    (is (= [] (helpers/merge-note-tags [] [])))))

;; ── build-update-note ───────────────────────────────────────────────────────

(def ^:private fields {"Front" "q" "Back" "a"})

(deftest t8-update-note-carries-the-merged-tags
  (let [note (helpers/build-update-note 42 fields ["marked"] ["physics"])]
    (is (= 42 (:id note)))
    (is (= fields (:fields note)))
    (is (= ["marked" "physics"] (:tags note)))))

(deftest t9-the-tags-key-is-always-present
  (testing "omitting it is not safe: updateNoteModel reads note.get('tags', [])
            and assigns the result, so an absent key CLEARS every tag"
    (doseq [current [[] ["marked"]]
            added [nil [] ["physics"]]]
      (is (contains? (helpers/build-update-note 42 fields current added) :tags)
        (str "no tags key for " (pr-str current) " + " (pr-str added))))))

(deftest t10-both-update-actions-route-through-the-builder
  (testing "the bare :tags expression at two call sites is how the wipe reached
            both — every updateNote/updateNoteModel call must use the builder"
    (let [src (slurp "src/freememo/anki_sync_helpers.cljc")
          calls (count (re-seq #"anki-call! \"updateNote(?:Model)?\"" src))
          routed (count (re-seq #"anki-call! \"updateNote(?:Model)?\"[\s\S]{0,200}?build-update-note"
                         src))]
      (is (= 2 calls) (str "expected 2 update call sites, found " calls))
      (is (= calls routed)
        (str routed " of " calls " update call sites route through build-update-note")))))

;; ── IO pull fields ──────────────────────────────────────────────────────────
;;
;; Pull reads occlusion fields by name, so a typo in io-pull-field-names is the
;; whole failure surface. It is silent: the named field is absent from the note,
;; the diff skips it, and an Anki-side edit to that field is never pulled.

(deftest t11-io-pull-fields-exist-in-the-model
  (testing "every name pull looks for is a field the model actually has"
    (let [model (set helpers/io-field-names)]
      (doseq [[k field-name] helpers/io-pull-field-names]
        (is (contains? model field-name)
          (str k " reads " (pr-str field-name)
            ", which is not in io-field-names"))))))

(deftest t12-pull-claims-no-fm-owned-field
  (testing "pulling a generated field would overwrite io_fields with FM's own
            output — Links, the masks, Image and the hidden id are all FM's"
    (let [claimed (set (vals helpers/io-pull-field-names))]
      (doseq [fm-owned ["ID (hidden)" "Image" "Question Mask" "Answer Mask"
                        "Original Mask" "Links"]]
        (is (not (contains? claimed fm-owned))
          (str fm-owned " is FM-generated and must not be pulled"))))))

(deftest t13-every-user-editable-io-field-is-pulled
  (testing "a text field with no pull entry is edited in Anki and silently lost"
    (is (= #{:header :footer :remarks :sources :extra1 :extra2}
          (set (keys helpers/io-pull-field-names))))))

(deftest t14-the-first-io-field-stays-first
  (testing "Anki's duplicate check reads field 0, and build-io-note depends on
            'ID (hidden)' being unique per note"
    (is (= "ID (hidden)" (first helpers/io-field-names)))))

;; ── build-io-fields ─────────────────────────────────────────────────────────

(def ^:private io-card
  {:occlusion-group {:anki-key "k1" :mode "hide-all" :image-media-id 7
                     :geometry {:width 100 :height 80 :rects [{:x 1 :y 1 :w 2 :h 2 :ordinal 1}]}}
   :flashcards/mask_ordinal 1
   :flashcards/kind "occlusion"
   :flashcards/topic_id 42
   :flashcards/io_fields {:header "H" :footer "F" :remarks "R"
                          :sources "S" :extra1 "E1" :extra2 "E2"}
   :fm/bibliography-html "Compilers: principles, techniques, &amp; tools — (2007)"
   :fm/pdf-page 5})

(def ^:private io-settings {:app-base-url "https://example.test"})

(deftest t15-io-notes-carry-the-links-field
  (let [f (helpers/build-io-fields io-card io-settings {})]
    (testing "Links is produced by the one shared builder, not a second copy"
      (is (= (get (helpers/build-links-field io-card io-settings) "Links")
            (get f "Links"))))
    (testing "the citation line carries the page locator"
      (is (re-find #"— p\. 5" (get f "Links"))))
    (testing "the deep-link points at the card's own topic"
      (is (re-find #"https://example\.test/viewer/topic/42" (get f "Links"))))))

(deftest t16-build-io-fields-covers-every-canonical-field
  (testing "a field in the model with no value here is blanked by updateNote"
    (is (= (set helpers/io-field-names)
          (set (keys (helpers/build-io-fields io-card io-settings {})))))))
