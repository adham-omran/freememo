(ns freememo.account-import-test
  "The remaps that no schema walk would find.

   plans/user-data-export-import.md §5.4. Foreign keys are the easy class and the
   round-trip check covers them. These tests cover the four that a generic
   FK-driven remapper misses: media ids inside HTML text, media ids inside JSONB,
   topic ids inside `settings` KEYS, and the parent-before-child ordering that id
   order does not give.

   Pure — no database. Every function here takes its maps as arguments."
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.account-import :as im]))

(def ^:private media-map {1 101, 2 102})

;; ── Media URLs in HTML (§5.4.7) ────────────────────────────────────

(deftest rewrite-media-urls-translates-every-occurrence
  (is (= "<img src=\"/api/media/101\">"
        (im/rewrite-media-urls "<img src=\"/api/media/1\">" media-map)))
  (testing "several in one string, including a repeat"
    (is (= "<p><img src=\"/api/media/101\"><img src=\"/api/media/102\"><img src=\"/api/media/101\"></p>"
          (im/rewrite-media-urls
            "<p><img src=\"/api/media/1\"><img src=\"/api/media/2\"><img src=\"/api/media/1\"></p>"
            media-map)))))

(deftest rewrite-media-urls-leaves-an-unmapped-id-alone
  ;; A dead link beats a link to another topic's image. The archive referenced
  ;; media it did not carry, and inventing a target would silently show the wrong
  ;; picture on a card.
  (is (= "<img src=\"/api/media/999\">"
        (im/rewrite-media-urls "<img src=\"/api/media/999\">" media-map))))

(deftest rewrite-media-urls-passes-through-what-it-must-not-touch
  (is (nil? (im/rewrite-media-urls nil media-map)))
  (is (= "" (im/rewrite-media-urls "" media-map)))
  (is (= "<p>no images here</p>" (im/rewrite-media-urls "<p>no images here</p>" media-map)))
  (testing "the id translates wherever it appears, and any tail is left alone"
    ;; No route serves a tail today. If one ever does, the id must still point at
    ;; the importing user's media.
    (is (= "/api/media/101/thumb"
          (im/rewrite-media-urls "/api/media/1/thumb" {1 101})))))

(deftest rewrite-media-urls-does-not-match-a-non-numeric-id
  (is (= "/api/media/abc" (im/rewrite-media-urls "/api/media/abc" media-map))))

;; ── Media ids inside JSONB (§5.4.6, §5.4.7) ────────────────────────

(def ^:private rewrite-value-html #'im/rewrite-value-html)

(deftest jsonb-html-walk-reaches-io-fields
  ;; flashcards.io_fields is a map of six HTML fields (occlusion.clj:23).
  (is (= {:header "<p><img src=\"/api/media/101\"></p>" :footer nil :remarks "plain"}
        (rewrite-value-html
          {:header "<p><img src=\"/api/media/1\"></p>" :footer nil :remarks "plain"}
          media-map))))

(deftest jsonb-html-walk-reaches-overlapping-items-and-fields
  ;; overlapping holds :items (HTML) and a materialized :fields map (db.clj:492).
  (is (= {:question "q"
          :items ["<img src=\"/api/media/101\">" "<img src=\"/api/media/102\">"]
          :settings {:reveal-all? true}
          :fields {:Original "<img src=\"/api/media/101\">" :Text1 "x"}}
        (rewrite-value-html
          {:question "q"
           :items ["<img src=\"/api/media/1\">" "<img src=\"/api/media/2\">"]
           :settings {:reveal-all? true}
           :fields {:Original "<img src=\"/api/media/1\">" :Text1 "x"}}
          media-map))))

(deftest jsonb-html-walk-leaves-non-strings-alone
  (is (= {:n 7 :b true :nil nil :v [1 2 3]}
        (rewrite-value-html {:n 7 :b true :nil nil :v [1 2 3]} media-map))))

(def ^:private remap-score-geometry #'im/remap-score-geometry)

(deftest score-geometry-media-ids-translate
  ;; score_groups.geometry holds a :media-id per rect (db.clj:469-471) — the one
  ;; id inside JSONB that a FK-driven remapper cannot see.
  (is (= {:pages [{:page 1 :width 800 :height 600
                   :rects [{:x 1 :y 2 :w 3 :h 4 :ordinal 1 :media-id 101}
                           {:x 5 :y 6 :w 7 :h 8 :ordinal 2 :media-id 102}]}]}
        (remap-score-geometry
          {:pages [{:page 1 :width 800 :height 600
                    :rects [{:x 1 :y 2 :w 3 :h 4 :ordinal 1 :media-id 1}
                            {:x 5 :y 6 :w 7 :h 8 :ordinal 2 :media-id 2}]}]}
          media-map))))

(deftest score-geometry-keeps-an-unmapped-rect
  (is (= {:pages [{:rects [{:ordinal 1 :media-id 999}]}]}
        (remap-score-geometry {:pages [{:rects [{:ordinal 1 :media-id 999}]}]} media-map))))

(deftest score-geometry-tolerates-a-shape-it-does-not-know
  ;; Returned unchanged, never edited into a shape this recognises.
  (is (nil? (remap-score-geometry nil media-map)))
  (is (= {} (remap-score-geometry {} media-map)))
  (is (= {:pages nil} (remap-score-geometry {:pages nil} media-map)))
  (is (= {:pages [{:rects nil}]} (remap-score-geometry {:pages [{:rects nil}]} media-map))))

;; ── Settings keys (§5.4.8) ─────────────────────────────────────────

(def ^:private remap-settings-key #'im/remap-settings-key)
(def ^:private remap-settings-row #'im/remap-settings-row)
(def ^:private topic-map {7 707, 9 909})

(deftest every-id-bearing-settings-prefix-translates
  ;; One case per dynamic family in settings.clj. A family missing from
  ;; settings-key-prefixes silently keeps a foreign topic id, and the only symptom
  ;; is a preference that applies to somebody else's document.
  (doseq [prefix ["last_page_" "pdf_layout_" "pdf_focus_" "focus_reader_"
                  "extract_style_" "ocr_model_"
                  "card_model_" "goal_" "assistant_model_" "anki_preset_"
                  "hierarchy_open_" "pins_open_" "assistant_tab_"
                  "left_panel_tab_"]]
    (is (= (str prefix 707) (remap-settings-key (str prefix 7) topic-map))
      (str "prefix " prefix))))

(deftest a-settings-key-with-no-topic-id-is-untouched
  (doseq [k ["theme" "model" "card_count" "anki_deck" "fsrs_retention"]]
    (is (= k (remap-settings-key k topic-map)))))

(deftest a-settings-key-naming-an-unimported-topic-is-dropped
  (is (nil? (remap-settings-key "last_page_404" topic-map)))
  (is (:drop (remap-settings-row {:key "last_page_404" :value "3"} topic-map))))

(deftest last-document-translates-its-value-not-its-key
  (is (= {:key "last_document" :value "909"}
        (:row (remap-settings-row {:key "last_document" :value "9"} topic-map))))
  (is (:drop (remap-settings-row {:key "last_document" :value "404"} topic-map))))

(deftest a-plain-settings-row-survives-unchanged
  (is (= {:key "theme" :value "dark"}
        (:row (remap-settings-row {:key "theme" :value "dark"} topic-map)))))

;; ── Topic ordering (§5.4.1) ────────────────────────────────────────

(def ^:private ordered-topic-rows #'im/ordered-topic-rows)
(def ^:private topic-depth #'im/topic-depth)

(defn- inserts-parents-first?
  "Whether every row's parent appears before it. This is the property the import
   needs: a parent id must already be mapped when its child is inserted."
  [rows]
  (let [order (map :id rows)]
    (loop [seen #{} [r & more] rows]
      (cond (nil? r) true
            (and (:parent_id r) (not (contains? seen (:parent_id r)))
              (some #{(:parent_id r)} order)) false
            :else (recur (conj seen (:id r)) more)))))

(deftest ordering-survives-a-child-with-a-lower-id-than-its-parent
  ;; The case id order gets wrong: topic_move reparented 2 under 50, which was
  ;; created later. Sorting by id would insert 2 before 50 exists.
  (let [rows [{:id 2 :parent_id 50} {:id 50 :parent_id nil} {:id 51 :parent_id 2}]]
    (is (false? (inserts-parents-first? (sort-by :id rows)))
      "id order really is unsafe — otherwise this test guards nothing")
    (is (true? (inserts-parents-first? (ordered-topic-rows rows))))))

(deftest ordering-handles-a-deep-chain
  (let [rows (into [{:id 1 :parent_id nil}]
               (map (fn [i] {:id (inc i) :parent_id i}) (range 1 40)))]
    (is (true? (inserts-parents-first? (ordered-topic-rows (shuffle rows)))))))

(deftest ordering-terminates-on-a-cycle
  ;; A malformed archive must not hang the import. The depth walk stops at the
  ;; row count and the cyclic rows sort last.
  (let [rows [{:id 1 :parent_id 2} {:id 2 :parent_id 1}]]
    (is (= 2 (count (ordered-topic-rows rows))))
    (is (= 2 (topic-depth {1 2, 2 1} 1 2)))))

(deftest depth-is-zero-for-a-root
  (is (= 0 (topic-depth {1 nil} 1 10)))
  (is (= 2 (topic-depth {3 2, 2 1, 1 nil} 3 10))))

;; ── Manifest reading (§5.1.3) ──────────────────────────────────────

(deftest read-manifest-refuses-a-directory-with-no-manifest
  (let [dir (.toFile (java.nio.file.Files/createTempDirectory "no-manifest"
                       (make-array java.nio.file.attribute.FileAttribute 0)))]
    (try
      (let [r (im/read-manifest dir)]
        (is (false? (:ok r)))
        (is (re-find #"manifest\.edn" (:error r))))
      (finally (.delete dir)))))
