(ns freememo.library-row-model-test
  (:require [clojure.test :refer [deftest is]]
            [freememo.library-cards :as library]
            [freememo.navigation :as nav]
            [freememo.optimistic :as opt]))

(defn card [id kind]
  {:flashcards/id id :row-key [:card id] :member-ids [id] :member-states [:unpushed]
   :flashcards/kind kind :flashcards/topic_id 10 :flashcards/root_topic_id 20
   :flashcards/question (str "Question " id) :flashcards/answer (str "Answer " id)
   :flashcards/cloze (str "{{c1::Cloze " id "}}") :flashcards/overlapping {:items ["One" "Two"]}
   :root_title "Root" :topic_title "Page" :formatted_date "Oct 29"
   :span2? (not= kind "basic") :front-html (str "Front " id) :back-html (str "Back " id)})

(defn harness []
  {:editing (atom nil) :diff (atom nil) :selected (atom {}) :stats (atom nil) :navigation (atom [])})

(defn model [h c overlay visible?]
  (library/row-view-model c #(swap! (:navigation h) conj [%1 %2])
    (:editing h) (:diff h) (:selected h) @(:selected h)
    (library/selection-subjects @(:selected h)) 0 visible? overlay (:stats h)))

(def kind-cases
  [["basic" "Basic" true]
   ["cloze" "Cloze" true]
   ["overlapping" "Overlap" false]
   ["occlusion" "Occlusion" false]
   ["score" "Score" false]])

(deftest every-kind-preserves-its-display-and-editor-route
  (doseq [[kind badge stats?] kind-cases]
    (let [h (harness) c (card 1 kind) row (model h c {} true)]
      (is (= badge (:badge-label row)))
      (is (= stats? (:stats? row)))
      (is (= (:span2? c) (:span2? row)))
      (is (= "Front 1" (:front-html row)))
      (is (= "Root / Page" (:doc-label row)))
      ((get-in row [:actions :edit]))
      (if (#{"occlusion" "score"} kind)
        (is (= [[:viewer (nav/nav-topic 10 :library)]] @(:navigation h)))
        (do (is (= 1 (:id @(:editing h))))
            (is (= kind (:kind @(:editing h)))))))))

(deftest grouped-flags-and-member-actions-remain-complete
  (let [h (harness)
        c (assoc (card 2 "occlusion") :row-key [:occlusion 7]
            :member-ids [2 3] :member-states [:unpushed :modified])
        overlay {3 {:marked true :remarked true :suspended {:total 2 :suspended 1}}}
        row (model h c overlay true)]
    (is (false? (:diff? row)))
    (is (false? (:stats? row)))
    (is (= {:marked true :remarked true :suspended {:total 2 :suspended 1}} (:flags row)))
    (is (re-find #"card-anki-marked" (:class row)))
    (is (= "◐" (:direction-glyph row)))
    (is (= [2 3] (get-in row [:target :subjects 0 :member-ids])))))

(deftest recycled-model-actions-capture-the-current-card
  (let [h (harness)
        old (model h (card 1 "basic") {} true)
        current (model h (card 2 "cloze") {} true)
        deletes (atom [])]
    ((get-in old [:actions :edit]))
    ((get-in current [:actions :edit]))
    (is (= 2 (:id @(:editing h))))
    ((get-in current [:actions :diff]))
    (is (= 2 (:id @(:diff h))))
    ((get-in current [:actions :stats]))
    (is (= {:card 2} @(:stats h)))
    ((get-in current [:actions :select]) true)
    (is (= {[:card 2] [2]} @(:selected h)))
    ((get-in current [:actions :select]) false)
    (is (= {} @(:selected h)))
    (reset! (:selected h) {[:card 1] [1] [:card 2] [2]})
    (with-redefs [opt/enqueue-delete-card! #(swap! deletes conj %)]
      ((get-in current [:actions :delete])))
    (is (= [[2]] @deletes))
    (is (= {[:card 1] [1]} @(:selected h)))))

(deftest hidden-selection-and-document-action-use-current-scope
  (let [h (harness)]
    (reset! (:selected h) {[:card 3] [3]})
    (let [row (model h (card 3 "basic") {} false)]
      (is (false? (:selected? row)))
      ((get-in row [:actions :document]))
      (is (= [[:viewer (nav/nav-topic 10 :library)]] @(:navigation h))))))

(deftest large-selection-subjects-are-shared-by-every-windowed-row
  (let [h (harness)
        selected (into {} (map (fn [id] [[:card id] [id]])) (range 1 100001))
        subjects (library/selection-subjects selected)
        models (mapv (fn [id]
                       (library/row-view-model (card id "basic") (fn [& _])
                         (:editing h) (:diff h) (:selected h) selected subjects 0 true {} (:stats h)))
                 (range 1 16))]
    (is (= 100000 (count subjects)))
    (is (every? #(identical? subjects (get-in % [:target :subjects])) models))))
