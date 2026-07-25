(ns freememo.quiz-feedback
  "Graded-answer presentation, shared by the live quiz and the history detail views
   (plans/quiz-iteration.md §8.12).

   Extracted from freememo.quiz-page so freememo.quiz-dashboard can render the same
   verdict badge, keyword highlighting, fact provenance and concept popover without
   requiring quiz-page — which requires the dashboard, and would cycle."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [clojure.string :as str]
   #?(:clj [freememo.db :as db])))

;; ---------------------------------------------------------------------------
;; Keyword highlighting — pure, shared by reference + user answer
;; ---------------------------------------------------------------------------

(defn highlight-segments
  "Split text into [[segment highlighted?] ...] on case-insensitive keyword
   occurrences. Earliest match wins; ties go to the longest keyword.
   Post: (apply str (map first result)) = text — nothing added or lost, so a
   keyword absent from the text can never produce a phantom highlight."
  [text keywords]
  (let [t (str text)
        lower (str/lower-case t)
        kws (->> keywords (map str/lower-case) (remove str/blank?) distinct
              (sort-by count >) vec)]
    (if (or (empty? kws) (= t ""))
      [[t false]]
      (loop [from 0 acc []]
        (let [hit (reduce (fn [best kw]
                            (let [i (str/index-of lower kw from)]
                              (if (and i (or (nil? best)
                                             (< i (first best))
                                             (and (= i (first best))
                                               (> (count kw) (second best)))))
                                [i (count kw)]
                                best)))
                    nil kws)]
          (if (nil? hit)
            (into [] (remove #(= "" (first %)))
              (conj acc [(subs t from) false]))
            (let [[i len] hit
                  acc (cond-> acc (> i from) (conj [(subs t from i) false]))]
              (recur (+ i len) (conj acc [(subs t i (+ i len)) true])))))))))

(defn- entity-lexicon
  "[{:id :label :aliases}] → {lower-mention {:id :label}} for segment lookup."
  [entities]
  (into {}
    (comp (mapcat (fn [{:keys [id label aliases]}]
                    (map #(vector (str/lower-case (str %)) {:id id :label label})
                      (cons label aliases))))
      (remove (comp str/blank? first)))
    entities))

#?(:clj
   (defn entity-card* [user-id entity-id]
     (db/kg-entity-card user-id entity-id)))

;; ---------------------------------------------------------------------------
;; Views
;; ---------------------------------------------------------------------------

(def verdict-badge
  {"correct"   ["✓ Correct" "var(--color-success-dark, #2e7d32)"]
   "partial"   ["◐ Partial" "#b26a00"]
   "incorrect" ["✗ Incorrect" "var(--color-danger, #c62828)"]})

(e/defn EntityLinkedText
  "Text with entity mentions as concept links (click → popover via
   !entity-card) and matched keywords highlighted. Highlight ⊆ links: every
   mention comes from the graph's labels/aliases, so a click always resolves."
  [text entities matched-keywords !entity-card]
  (e/client
    (let [lex (entity-lexicon entities)
          matched (into #{} (map str/lower-case) matched-keywords)]
      (e/for [[idx seg hit] (e/diff-by first
                              (map-indexed (fn [i [s h]] [i s h])
                                (highlight-segments text (keys lex))))]
        (let [_ idx
              ent (when hit (lex (str/lower-case seg)))]
          (if ent
            (dom/span
              (dom/props {:role "button"
                          :style (cond-> {:cursor "pointer"
                                          :text-decoration "underline dotted"
                                          :text-underline-offset "3px"
                                          :border-radius "2px" :padding "0 2px"}
                                   (matched (str/lower-case seg))
                                   (assoc :background "var(--color-warning-bg, #fff3cd)"
                                     :font-weight "600"))})
              (dom/text seg)
              (dom/On "click" (fn [_] (reset! !entity-card ent)) nil))
            (dom/text seg)))))))

(e/defn EntityMention
  "A clickable entity label inside a fact line (nil id → plain text)."
  [label entity-id !entity-card]
  (e/client
    (if entity-id
      (dom/span
        (dom/props {:role "button"
                    :style {:cursor "pointer" :text-decoration "underline dotted"
                            :text-underline-offset "3px"}})
        (dom/text label)
        (dom/On "click" (fn [_] (reset! !entity-card {:id entity-id :label label})) nil))
      (dom/text label))))

(e/defn FactLine
  "One fact as `s — p → o (Doc, p.N)` with linkable entities. The provenance tail
   navigates to that document page when `navigate-source!` is supplied — otherwise
   it renders as inert text."
  [{:keys [subject_label predicate_label object_label object_literal
           doc_title page_number subject_entity_id object_entity_id graph_topic_id]}
   !entity-card navigate-source!]
  (e/client
    (dom/li
      (dom/props {:style {:font-size "13px" :padding "2px 0"}})
      (EntityMention subject_label subject_entity_id !entity-card)
      (dom/text (str " — " predicate_label " → "))
      (if object_label
        (EntityMention object_label object_entity_id !entity-card)
        (dom/text object_literal))
      (let [linkable? (and navigate-source! graph_topic_id)]
        (dom/span
          (dom/props (cond-> {:style (cond-> {:color "var(--color-text-secondary)"
                                              :font-size "12px" :margin-left "6px"}
                                       linkable? (assoc :cursor "pointer"
                                                   :text-decoration "underline dotted"))}
                       linkable? (assoc :role "button"
                                   :aria-label (str "Open " doc_title
                                                 (when page_number (str " page " page_number))))))
          (dom/text (str "(" doc_title (when page_number (str ", p." page_number)) ")"))
          (when linkable?
            (dom/On "click" (fn [_] (navigate-source! graph_topic_id page_number)) nil)))))))

(e/defn EntityCardPopover
  "Concept card: the clicked entity's label, aliases, and fact neighborhood.
   Fact rows link onward — the popover walks the graph."
  [user-id !entity-card entity-card]
  (e/client
    (when entity-card
      (let [card (e/server (entity-card* user-id (:id entity-card)))]
        (dom/div ; backdrop — click closes
          (dom/props {:style {:position "fixed" :inset "0" :z-index "1200"
                              :background "rgba(0,0,0,0.35)"
                              :display "flex" :align-items "center"
                              :justify-content "center"}})
          (dom/On "click" (fn [_] (reset! !entity-card nil)) nil)
          (dom/div
            (dom/props {:role "dialog" :aria-label (str "About " (:label card))
                        :style {:background "var(--color-bg-card)"
                                :color "var(--color-text-primary)"
                                :border-radius "8px" :padding "16px 20px"
                                :max-width "600px" :width "90%"
                                :max-height "70vh" :overflow-y "auto"
                                :box-shadow "0 8px 32px rgba(0,0,0,0.25)"}})
            (dom/On "click" (fn [e] (.stopPropagation e) nil) nil)
            (dom/h3 (dom/props {:style {:font-size "16px" :margin "0 0 4px"}})
              (dom/text (:label card)))
            (when (seq (:aliases card))
              (dom/p (dom/props {:style {:font-size "12px" :margin "0 0 8px"
                                         :color "var(--color-text-secondary)"}})
                (dom/text (str "Also: " (str/join ", " (:aliases card))))))
            (if (empty? (:facts card))
              (dom/p (dom/props {:style {:font-size "13px"}})
                (dom/text "No facts reference this concept."))
              (dom/ul (dom/props {:style {:margin "4px 0" :padding-left "20px"}})
                (e/for [f (e/diff-by :id (:facts card))]
                  (FactLine f !entity-card nil))))))))))

(e/defn QuizFeedback
  "Graded result: verdict, explanation, reference + user answer with entity
   mentions linked (matched keywords highlighted), missed facts with
   provenance — every entity click opens the concept popover."
  [result user-answer entities !entity-card navigate-source!]
  (e/client
    (let [{:keys [verdict explanation reference-answer matched-keywords missed-facts]} result
          [label color] (get verdict-badge verdict ["?" "inherit"])]
      (dom/div
        (dom/props {:style {:border "1px solid var(--color-border)" :border-radius "8px"
                            :padding "12px 16px" :margin-top "12px"}})
        (dom/div
          (dom/props {:style {:font-weight "700" :color color :font-size "15px"}})
          (dom/text label))
        (dom/p (dom/props {:style {:margin "8px 0" :font-size "14px"}})
          (dom/text explanation))
        (dom/div (dom/props {:style {:font-size "12px" :color "var(--color-text-secondary)"
                                     :margin-top "10px"}})
          (dom/text "Reference answer"))
        (dom/p (dom/props {:style {:font-size "14px" :margin "4px 0"}})
          (EntityLinkedText reference-answer entities matched-keywords !entity-card))
        (dom/div (dom/props {:style {:font-size "12px" :color "var(--color-text-secondary)"
                                     :margin-top "10px"}})
          (dom/text "Your answer"))
        (dom/p (dom/props {:style {:font-size "14px" :margin "4px 0"}})
          (EntityLinkedText user-answer entities matched-keywords !entity-card))
        (when (seq missed-facts)
          (dom/div
            (dom/div (dom/props {:style {:font-size "12px" :color "var(--color-text-secondary)"
                                         :margin-top "10px"}})
              (dom/text "You missed"))
            (dom/ul (dom/props {:style {:margin "4px 0" :padding-left "20px"}})
              (e/for [f (e/diff-by :id missed-facts)]
                (FactLine f !entity-card navigate-source!)))))))))

(e/defn EditedSinceNote
  "Marks history detail whose question has been reworded since it was answered.

   Renders nothing when the recorded wording is absent — a row written before the
   snapshot column existed is \"not recorded\", which must not be shown as
   \"unchanged\" nor as \"edited\".
   Pre:  `edited?` comes from question-curation/question-edited?."
  [edited?]
  (e/client
    (when edited?
      (dom/p
        (dom/props {:style {:font-size "12px" :margin "4px 0 0"
                            :color "var(--color-warning-dark, #b26a00)"}})
        (dom/text "This question has been edited since you answered.")))))
