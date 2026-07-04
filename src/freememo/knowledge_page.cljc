(ns freememo.knowledge-page
  "Knowledge tab — the fact-graph workbench (plans/knowledge-graph-quizzes.md M2).

   Three client-local sub-views (:documents default):
     :documents — root docs with fact stats; Distill / Cancel / Review actions
     [:review doc-id] — proposed-fact queue: approve / edit-literal / reject
     :entities — entity browser: search, rename, merge

   All queries re-run on the :kg-mutations channel; distill progress is the
   :distilling-docs set. Mutations bump via commands/bump! :distill."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.electric-scroll0 :refer [Scroll-window Tape]]
   [contrib.data :refer [clamp-left]]
   [clojure.string :as str]
   [freememo.icons :as icons]
   [freememo.commands :as commands]
   [freememo.modal-shell :as modal]
   [freememo.typeahead :as typeahead]
   #?(:clj [freememo.db :as db])
   #?(:clj [freememo.kg-extract :as kg])
   #?(:clj [freememo.kg-questions :as kgq])
   #?(:clj [freememo.toasts :as toasts])
   #?(:clj [freememo.user-state :as us])))

;; ---------------------------------------------------------------------------
;; Server queries — whole defns under #?(:clj …) so CLJS misuse warns loudly
;; instead of silently nil'ing; called only inside e/server.
;; ---------------------------------------------------------------------------

#?(:clj
   (defn get-knowledge-docs*
     "Root documents + fact counts, lean maps for the wire.
      Post: [{:id :title :kind :facts}] — :facts = approved count."
     [_kg-bump user-id]
     (let [stats (db/kg-fact-stats user-id)]
       (mapv (fn [t]
               (let [id (:topics/id t)]
                 {:id id
                  :title (:topics/title t)
                  :kind (:topics/kind t)
                  :facts (get-in stats [id "approved"] 0)
                  ;; distilled? — any fact of ANY status exists (status-agnostic),
                  ;; so a doc whose facts were all rejected still counts as distilled
                  ;; and re-distilling it is guarded by a confirm.
                  :distilled? (pos? (reduce + 0 (vals (get stats id))))}))
         (db/get-root-topics user-id)))))

#?(:clj
   (defn get-doc-facts* [_kg-bump user-id doc-id query]
     (db/get-kg-facts user-id doc-id "approved" query)))

#?(:clj
   (defn list-entities* [_kg-bump user-id query source-id]
     ;; 1000: virtual scroll owns the DOM cost; the wire cost is ~60 B/row.
     (db/list-kg-entities user-id query 1000 source-id)))

#?(:clj
   (defn get-questions* [_kg-bump user-id query source-id entity-id]
     (db/get-kg-questions user-id "approved" query
       {:source-id source-id :entity-id entity-id})))

#?(:clj
   (defn list-source-docs*
     "Lean [{:id :title}] of root documents — options for the source filters."
     [_kg-bump user-id]
     (mapv (fn [t] {:id (:topics/id t) :title (:topics/title t)})
       (db/get-root-topics user-id))))

#?(:clj
   (defn list-entity-options*
     "Lean [{:id :label}] of entities — options for the Questions entity filter."
     [_kg-bump user-id]
     (mapv (fn [ent] {:id (:id ent) :label (:label ent)})
       (db/list-kg-entities user-id nil 1000))))

#?(:clj
   (defn reject-question!*
     "Reject a question, logging an undo entry and surfacing an Undo toast."
     [user-id question-id]
     (when-let [undo-id (db/reject-kg-question! user-id question-id)]
       (commands/bump! user-id :generate-questions)
       (toasts/push! user-id {:level :success :message "Question rejected" :dedup? false
                              :actions [{:label "Undo" :undo-id undo-id}]}))
     :done))

#?(:clj
   (defn reject-fact!*
     "Reject a fact (retiring its live questions), logging an undo entry and
      surfacing an Undo toast."
     [user-id fact-id]
     (when-let [undo-id (db/reject-kg-fact! user-id fact-id)]
       (commands/bump! user-id :distill)
       (toasts/push! user-id {:level :success :message "Fact rejected" :dedup? false
                              :actions [{:label "Undo" :undo-id undo-id}]}))
     :done))

;; ---------------------------------------------------------------------------
;; Shared bits
;; ---------------------------------------------------------------------------

(def ^:private th-style
  {:text-align "left" :padding "6px 10px" :font-size "12px"
   :color "var(--color-text-secondary)" :border-bottom "1px solid var(--color-border)"})

(def ^:private fact-row-height 41)

(def ^:private fact-cell-style
  ;; No inline overflow here: .tape-scroll table td owns clipping and its
  ;; :has([data-tooltip]:hover) escape must win so tooltips render below the
  ;; control, uncut. Ellipsis lives on inner spans (fact-text-style).
  {:display "flex" :align-items "center" :padding-inline "10px"
   :font-size "13px" :border-bottom "1px solid var(--color-bg-subtle)"})

(def ^:private fact-text-style
  {:overflow "hidden" :text-overflow "ellipsis" :white-space "nowrap"})

(e/defn TapeTable
  "Shared virtualized zebra table (Tape idiom): fixed header + scroll body
   windowed by Scroll-window at fact-row-height rows.
   Pre:  Row is an e/fn of [i] rendering one dom/tr (nil-safe past the end);
         the caller's container bounds the height (flex column, min-height 0).
   Post: only the visible window (± overquery buffer) is mounted; total
         scroll height equals item-count rows."
  [headers grid-cols item-count Row]
  (e/client
    (dom/div
      (dom/props {:style {:display "flex" :flex-direction "column" :min-height "0" :flex "1"}})
      ;; Fixed header
      (dom/table
        (dom/props {:style {:width "100%" :display "grid" :grid-template-columns grid-cols
                            :flex-shrink "0"}})
        (dom/thead
          (dom/props {:style {:display "contents"}})
          (dom/tr
            (dom/props {:style {:display "contents"}})
            (e/for [h (e/diff-by identity headers)]
              (dom/th (dom/props {:style th-style}) (dom/text h))))))
      ;; Scrollable virtualized body
      (dom/div
        (dom/props {:style {:flex "1" :overflow-y "auto" :min-height "0"
                            :scrollbar-gutter "stable"}})
        (let [[offset limit] (Scroll-window fact-row-height item-count dom/node
                               {:overquery-factor 2})
              occluded-height (clamp-left (* fact-row-height (- item-count limit)) 0)]
          (dom/props {:class "tape-scroll"
                      :style {:--offset offset :--row-height (str fact-row-height "px")}})
          (dom/table
            (dom/props {:style {:width "100%" :display "grid"
                                :grid-template-columns grid-cols}})
            (e/for [i (Tape offset limit)]
              (Row i)))
          (dom/div (dom/props {:style {:height (str occluded-height "px")}})))))))

(e/defn SubViewTabs [!view view]
  ;; Settings-style horizontal nav (see settings-page/SettingsNav). Copies the
  ;; .settings-nav markup + CSS rather than sharing the component, which is bound
  ;; to settings-specific module state. Driven by the client-local !view atom
  ;; (not a URL hash); a [:facts id] vector view keeps Documents highlighted.
  (e/client
    (dom/nav
      (dom/props {:class "settings-nav"})
      (e/for [[k label] (e/diff-by first [[:documents "Documents"] [:entities "Entities"]
                                          [:questions "Questions"]])]
        (let [active? (or (= view k) (and (vector? view) (= k :documents)))]
          (dom/a
            (dom/props {:href "#"
                        :class (if active?
                                 "settings-nav__item settings-nav__item--active"
                                 "settings-nav__item")})
            (dom/text label)
            (dom/On "click" (fn [e] (.preventDefault e) (reset! !view k)) nil)))))))

;; ---------------------------------------------------------------------------
;; Documents view
;; ---------------------------------------------------------------------------

(e/defn DistillButton [user-id doc-id distilling? distilled? !redistill]
  (e/client
    (dom/button
      (dom/props {:class "btn btn-sm btn-secondary"
                  :disabled distilling?
                  :aria-label "Distill facts"
                  :data-tooltip "Extract facts from this document into the knowledge graph"})
      (if distilling?
        (icons/Icon :loader-2 :size 14 :class "spin")
        (icons/Icon :pen-sparkles :size 14))
      (dom/text (if distilling? " Distilling…" " Distill"))
      (if distilled?
        ;; Already distilled — confirm before spending credits/time again. The
        ;; single ConfirmModal lives in DocumentsView, keyed off this doc-id.
        (dom/On "click" (fn [_] (reset! !redistill doc-id)) nil)
        ;; First distill — nothing to lose, go straight through.
        (let [click (dom/On "click" (fn [_] {:id (str (random-uuid))}) nil)
              [t _] (e/Token click)]
          (when t
            (case (e/server (kg/start-distill! user-id doc-id))
              (t))))))
    (when distilling?
      (dom/button
        (dom/props {:class "btn btn-sm" :aria-label "Cancel distillation"})
        (dom/text "Cancel")
        (let [click (dom/On "click" (fn [_] {:id (str (random-uuid))}) nil)
              [t _] (e/Token click)]
          (when t
            (case (e/server (kg/cancel-distill! user-id doc-id))
              (t))))))))

(e/defn GenerateQuestionsButton
  "Kick an atomic-generation run for a document (background; toasts report)."
  [user-id doc-id generating?]
  (e/client
    (dom/button
      (dom/props {:class "btn btn-sm btn-secondary" :style {:margin-left "6px"}
                  :disabled generating?
                  :aria-label "Generate questions"
                  :data-tooltip "One question per fact that doesn't have one yet"})
      (if generating?
        (icons/Icon :loader-2 :size 14 :class "spin")
        (icons/Icon :graduation-cap :size 14))
      (dom/text (if generating? " Generating…" " Questions"))
      (let [click (dom/On "click" (fn [_] {:id (str (random-uuid))}) nil)
            [t _] (e/Token click)]
        (when t
          (case (e/server (kgq/start-atomic-generation! user-id doc-id))
            (t)))))
    (when generating?
      (dom/button
        (dom/props {:class "btn btn-sm" :aria-label "Cancel question generation"})
        (dom/text "Cancel")
        (let [click (dom/On "click" (fn [_] {:id (str (random-uuid))}) nil)
              [t _] (e/Token click)]
          (when t
            (case (e/server (kgq/cancel-atomic-generation! user-id doc-id))
              (t))))))))

(e/defn DocumentRow [user-id doc i !view distilling generating !redistill]
  (e/client
    (let [{:keys [id title kind facts distilled?]} doc]
      (dom/tr
        (dom/props {:class (when (even? i) "row-alt") :style {:--order (inc i)}})
        (dom/td (dom/props {:style fact-cell-style})
          (dom/span (dom/props {:style fact-text-style}) (dom/text title))
          (dom/span (dom/props {:style {:color "var(--color-text-secondary)" :font-size "11px"
                                        :margin-left "6px" :flex-shrink "0"}})
            (dom/text kind)))
        (dom/td (dom/props {:style fact-cell-style}) (dom/text (str facts)))
        (dom/td (dom/props {:style (merge fact-cell-style
                                     {:white-space "nowrap" :justify-content "flex-end"})})
          (DistillButton user-id id (contains? distilling id) distilled? !redistill)
          (when (pos? facts)
            (GenerateQuestionsButton user-id id (contains? generating id)))
          (dom/button
            (dom/props {:class "btn btn-sm btn-secondary"
                        :style {:margin-left "6px"}
                        :disabled (zero? facts)
                        :aria-label (str "Browse facts for " title)})
            (dom/text (str "Facts (" facts ")"))
            (dom/On "click" (fn [_] (reset! !view [:facts id])) nil)))))))

(e/defn DocumentsView [user-id !view kg-bump]
  (e/client
    (let [docs (e/server (get-knowledge-docs* kg-bump user-id))
          item-count (count docs)
          distilling (e/server (e/watch (us/get-atom user-id :distilling-docs)))
          generating (e/server (e/watch (us/get-atom user-id :generating-questions)))
          !redistill (atom nil)]              ; doc-id pending a re-distill confirm, or nil
      (dom/div
        (dom/props {:style {:display "flex" :flex-direction "column"
                            :height "calc(100vh - 160px)" :padding-top "8px"}})
        (if (zero? item-count)
          (dom/p (dom/props {:style {:padding "16px" :color "var(--color-text-secondary)"}})
            (dom/text "No documents yet — import one first."))
          (TapeTable ["Document" "Facts" ""]
            "minmax(240px,1fr) 70px minmax(340px,max-content)" item-count
            (e/fn [i]
              (when-let [doc (nth docs i nil)]
                (DocumentRow user-id doc i !view distilling generating !redistill)))))
        ;; Single re-distill confirm for the whole view (virtualization-safe;
        ;; per-row modals would ride the Tape's reused DOM nodes). !redistill holds
        ;; the doc-id to re-run; ConfirmModal passes it back as the payload.
        (modal/ConfirmModal !redistill
          "Re-distill document?"
          "This document is already distilled. Re-running spends credits and takes time, and only adds new facts — it won't change your edits or rejected facts."
          "Re-distill" "btn btn-primary"
          (e/fn [doc-id] (e/server (kg/start-distill! user-id doc-id))))))))

;; ---------------------------------------------------------------------------
;; Facts view — searchable browser over a document's approved facts.
;; Curation is by exception: facts land approved; wrong ones are rejected or
;; edited here (decision revised 2026-07-04). Virtualized via the Tape idiom;
;; row shape mirrors library_cards (41px rows, zebra via .row-alt).
;; ---------------------------------------------------------------------------

(e/defn RejectFactButton
  "Reject one fact (drops it from the graph views). DeleteCardButton idiom:
   stable event value, disabled while in flight, blocking JDBC via e/Offload."
  [user-id fact-id]
  (e/client
    (dom/button
      (dom/props {:class "btn-delete-x" :aria-label "Reject fact"
                  :data-tooltip "Reject — removes this fact from the graph"})
      (dom/text "×")
      (let [click (dom/On "click" (fn [e] #?(:cljs (.stopPropagation e)) fact-id) nil)
            [t _] (e/Token click)]
        (dom/props {:disabled (some? t)})
        (when t
          (let [result (e/server
                         (e/Offload
                           #(reject-fact!* user-id click)))]
            (case result
              (t))))))))

(e/defn FactObjectCell
  "Object cell: text, or an inline editor when this row is being edited.
   Enter saves (Offload), Escape cancels. Literal objects rewrite the literal;
   entity objects RELINK this fact to the matched-or-created entity — other
   facts of the old entity are untouched (rename-everywhere lives in the
   Entities tab). A relink that would duplicate an existing s/p/o toasts and
   leaves the fact unchanged."
  [user-id fact-id object_label object_literal !editing editing]
  (e/client
    (if (= editing fact-id)
      (dom/input
        (dom/props {:value (or object_label object_literal) :class "form-input"
                    :aria-label "Edit object"})
        (let [keyev (dom/On "keydown"
                      (fn [ev]
                        (let [k (.-key ev) v (.. ev -target -value)]
                          (cond
                            (= k "Enter") {:id (str (random-uuid)) :value v}
                            (= k "Escape") (do (reset! !editing nil) nil)
                            :else nil)))
                      nil)
              [t _] (e/Token keyev)
              literal? (some? object_literal)]
          (when t
            (let [result (e/server
                           (e/Offload
                             #(let [r (if literal?
                                        (do (db/update-kg-fact-literal! user-id fact-id (:value keyev))
                                          :done)
                                        (db/relink-kg-fact-object! user-id fact-id (:value keyev)))]
                                (when (= :duplicate r)
                                  (toasts/push! user-id
                                    {:level :error
                                     :message "That fact already exists in this document."}))
                                (commands/bump! user-id :distill)
                                (or r :done))))]
              (case result
                (do (reset! !editing nil) (t)))))))
      (dom/span (dom/props {:style fact-text-style})
        (dom/text (or object_label object_literal))))))

(e/defn FactRow [user-id fact i !editing editing]
  (e/client
    (let [{:keys [id subject_label predicate_label
                  object_label object_literal page_number]} fact]
      (dom/tr
        (dom/props {:class (when (even? i) "row-alt")
                    :style {:--order (inc i)}})
        (dom/td (dom/props {:style fact-cell-style})
          (dom/span (dom/props {:style fact-text-style}) (dom/text subject_label)))
        (dom/td (dom/props {:style (merge fact-cell-style
                                     {:color "var(--color-text-secondary)"})})
          (dom/span (dom/props {:style fact-text-style}) (dom/text predicate_label)))
        (dom/td (dom/props {:style fact-cell-style})
          (FactObjectCell user-id id object_label object_literal !editing editing))
        (dom/td (dom/props {:style (merge fact-cell-style
                                     {:color "var(--color-text-secondary)" :font-size "12px"})})
          (dom/text (if page_number (str "p." page_number) "—")))
        (dom/td (dom/props {:style (merge fact-cell-style
                                     {:justify-content "flex-end" :gap "4px"})})
          (when (not= editing id)
            (dom/button
              (dom/props {:class "btn btn-sm btn-secondary" :aria-label "Edit object"
                          :data-tooltip (if object_literal
                                          "Edit value"
                                          "Relink this fact's object (other facts unaffected)")})
              (icons/Icon :pen-line :size 14)
              (dom/On "click" (fn [_] (reset! !editing id)) nil)))
          (RejectFactButton user-id id))))))

(e/defn FactsView [user-id doc-id !view kg-bump]
  (e/client
    (let [!query (atom "") query (e/watch !query)
          facts (e/server (get-doc-facts* kg-bump user-id doc-id query))
          item-count (count facts)
          grid-cols "minmax(140px,1fr) minmax(130px,1fr) minmax(160px,1.2fr) 60px 110px"
          !editing (atom nil)               ; fact id whose literal is being edited
          editing (e/watch !editing)]
      (dom/div
        (dom/props {:style {:padding "0 16px" :display "flex" :flex-direction "column"
                            :height "calc(100vh - 160px)"}})
        (dom/div
          (dom/props {:style {:display "flex" :gap "8px" :align-items "center" :margin "10px 0"}})
          (dom/button
            (dom/props {:class "btn btn-sm"})
            (dom/text "← Documents")
            (dom/On "click" (fn [_] (reset! !view :documents)) nil))
          (dom/input
            (dom/props {:placeholder "Search facts…" :class "form-input"
                        :aria-label "Search facts"
                        :style {:max-width "320px"}})
            (dom/On "input" (fn [ev] (reset! !query (.. ev -target -value))) nil))
          (dom/span (dom/props {:style {:font-size "12px" :color "var(--color-text-secondary)"}})
            (dom/text (str item-count " fact" (when (not= 1 item-count) "s")))))
        (if (zero? item-count)
          (dom/p (dom/props {:style {:color "var(--color-text-secondary)"}})
            (dom/text (if (str/blank? query) "No facts yet — distill first." "No matches.")))
          (TapeTable ["Subject" "Predicate" "Object" "Page" ""] grid-cols item-count
            (e/fn [i]
              (when-let [fact (nth facts i nil)]
                (FactRow user-id fact i !editing editing)))))))))

;; ---------------------------------------------------------------------------
;; Questions view — searchable bank browser (same Tape/zebra shape as facts).
;; ---------------------------------------------------------------------------

(e/defn RejectQuestionButton
  "Reject one question (drops it from the bank). DeleteCardButton idiom."
  [user-id question-id]
  (e/client
    (dom/button
      (dom/props {:class "btn-delete-x" :aria-label "Reject question"
                  :data-tooltip "Reject — removes this question from the bank"})
      (dom/text "×")
      (let [click (dom/On "click" (fn [e] #?(:cljs (.stopPropagation e)) question-id) nil)
            [t _] (e/Token click)]
        (dom/props {:disabled (some? t)})
        (when t
          (let [result (e/server
                         (e/Offload
                           #(reject-question!* user-id click)))]
            (case result
              (t))))))))

(e/defn QuestionTextCell
  "Question or answer cell: text, or an inline editor when (= editing
   [qid field]). Enter saves (Offload), Escape cancels."
  [user-id qid field current !editing editing]
  (e/client
    (if (= editing [qid field])
      (dom/input
        (dom/props {:value current :class "form-input"
                    :aria-label (str "Edit " (name field))})
        (let [keyev (dom/On "keydown"
                      (fn [ev]
                        (let [k (.-key ev) v (.. ev -target -value)]
                          (cond
                            (= k "Enter") {:id (str (random-uuid)) :value v}
                            (= k "Escape") (do (reset! !editing nil) nil)
                            :else nil)))
                      nil)
              [t _] (e/Token keyev)
              question? (= field :question)]
          (when t
            (let [result (e/server
                           (e/Offload
                             #(do (db/update-kg-question! user-id qid
                                    (when question? (:value keyev))
                                    (when-not question? (:value keyev)))
                                (commands/bump! user-id :generate-questions)
                                :done)))]
              (case result
                (do (reset! !editing nil) (t)))))))
      (dom/span
        (dom/props {:style {:overflow "hidden" :text-overflow "ellipsis"
                            :white-space "nowrap" :cursor "text"}
                    :data-tooltip current})
        (dom/text current)
        (dom/On "dblclick" (fn [_] (reset! !editing [qid field])) nil)))))

(e/defn QuestionRow [user-id q i !editing editing]
  (e/client
    (let [{:keys [id kind question reference_answer fact_count]} q]
      (dom/tr
        (dom/props {:class (when (even? i) "row-alt")
                    :style {:--order (inc i)}})
        (dom/td (dom/props {:style fact-cell-style})
          (QuestionTextCell user-id id :question question !editing editing))
        (dom/td (dom/props {:style (merge fact-cell-style
                                     {:color "var(--color-text-secondary)"})})
          (QuestionTextCell user-id id :answer reference_answer !editing editing))
        (dom/td (dom/props {:style fact-cell-style})
          (dom/span
            (dom/props {:class "type-badge"
                        :style {:background (if (= kind "synthesis")
                                              "var(--color-badge-epub)"
                                              "var(--color-badge-pdf)")}})
            (dom/text kind)))
        (dom/td (dom/props {:style (merge fact-cell-style {:font-size "12px"})})
          (dom/text (str fact_count)))
        (dom/td (dom/props {:style (merge fact-cell-style
                                     {:justify-content "flex-end"})})
          (RejectQuestionButton user-id id))))))

(e/defn QuestionsView [user-id kg-bump]
  (e/client
    (let [!query (atom "") query (e/watch !query)
          !source-text (atom "") source-text (e/watch !source-text)
          !entity-text (atom "") entity-text (e/watch !entity-text)
          docs (e/server (list-source-docs* kg-bump user-id))
          ents (e/server (list-entity-options* kg-bump user-id))
          ;; Resolve picked title/label → id; blank / no-match = that filter off.
          source-id (some (fn [d] (when (= (:title d) source-text) (:id d))) docs)
          entity-id (some (fn [ent] (when (= (:label ent) entity-text) (:id ent))) ents)
          questions (e/server (get-questions* kg-bump user-id query source-id entity-id))
          item-count (count questions)
          grid-cols "minmax(220px,1.4fr) minmax(220px,1.4fr) 90px 55px 60px"
          !editing (atom nil)               ; [question-id :question|:answer]
          editing (e/watch !editing)]
      (dom/div
        (dom/props {:style {:padding "0 16px" :display "flex" :flex-direction "column"
                            :height "calc(100vh - 160px)"}})
        (dom/div
          (dom/props {:style {:display "flex" :gap "8px" :align-items "center" :margin "10px 0"
                              :flex-wrap "wrap"}})
          (dom/input
            (dom/props {:placeholder "Search questions…" :class "form-input"
                        :aria-label "Search questions"
                        :style {:max-width "320px"}})
            (dom/On "input" (fn [ev] (reset! !query (.. ev -target -value))) nil))
          (dom/div (dom/props {:style {:width "200px"}})
            (typeahead/Typeahead !source-text (mapv :title docs) "Filter by source…" nil nil))
          (dom/div (dom/props {:style {:width "200px"}})
            (typeahead/Typeahead !entity-text (mapv :label ents) "Filter by entity…" nil nil))
          (dom/span (dom/props {:style {:font-size "12px" :color "var(--color-text-secondary)"}})
            (dom/text (str item-count " question" (when (not= 1 item-count) "s")
                        " — double-click text to edit"))))
        (if (zero? item-count)
          (dom/p (dom/props {:style {:color "var(--color-text-secondary)"}})
            (dom/text (if (and (str/blank? query) (nil? source-id) (nil? entity-id))
                        "No questions yet — generate from a document or synthesize from an entity."
                        "No matches.")))
          (TapeTable ["Question" "Answer" "Kind" "Facts" ""] grid-cols item-count
            (e/fn [i]
              (when-let [q (nth questions i nil)]
                (QuestionRow user-id q i !editing editing)))))))))

;; ---------------------------------------------------------------------------
;; Entities view
;; ---------------------------------------------------------------------------

(e/defn SynthesizeButton
  "Generate multi-fact questions from this entity's neighborhood. One LLM
   call, inline (Offload); the server fn pushes its own outcome toast."
  [user-id entity-id label]
  (e/client
    (dom/button
      (dom/props {:class "btn btn-sm btn-secondary" :style {:margin-left "4px"}
                  :aria-label (str "Synthesize questions for " label)
                  :data-tooltip "Generate questions spanning this entity's facts"})
      (icons/Icon :graduation-cap :size 14)
      (let [click (dom/On "click" (fn [e] #?(:cljs (.stopPropagation e)) entity-id) nil)
            [t _] (e/Token click)]
        (dom/props {:disabled (some? t)})
        (when t
          (let [result (e/server
                         (e/Offload
                           #(do (kgq/generate-synthesis-questions! user-id click label)
                              :done)))]
            (case result
              (t))))))))

(e/defn EntityRow [user-id entity i !renaming renaming !merge-source merge-source !merge-confirm]
  (e/client
    (let [{:keys [id label aliases fact_count]} entity]
      (dom/tr
        (dom/props {:class (when (even? i) "row-alt") :style {:--order (inc i)}})
        (dom/td (dom/props {:style fact-cell-style})
          (if (= renaming id)
            (dom/input
              (dom/props {:value label :class "form-input" :aria-label "Rename entity"})
              (let [keyev (dom/On "keydown"
                            (fn [ev]
                              (let [k (.-key ev) v (.. ev -target -value)]
                                (cond
                                  (= k "Enter") {:id (str (random-uuid)) :value v}
                                  (= k "Escape") (do (reset! !renaming nil) nil)
                                  :else nil)))
                            nil)
                    [t _] (e/Token keyev)]
                (when t
                  (let [result (e/server
                                 (e/Offload
                                   #(do (db/rename-kg-entity! user-id id (:value keyev))
                                      (commands/bump! user-id :distill)
                                      :done)))]
                    (case result
                      (do (reset! !renaming nil) (t)))))))
            (dom/span (dom/props {:style fact-text-style}) (dom/text label))))
        (dom/td (dom/props {:style (merge fact-cell-style
                                     {:color "var(--color-text-secondary)" :font-size "12px"})})
          (dom/span (dom/props {:style fact-text-style})
            (dom/text (str/join ", " (or aliases [])))))
        (dom/td (dom/props {:style fact-cell-style}) (dom/text (str fact_count)))
        (dom/td (dom/props {:style (merge fact-cell-style
                                     {:white-space "nowrap" :justify-content "flex-end"})})
          (when-not (= renaming id)
            (dom/button
              (dom/props {:class "btn btn-sm btn-secondary" :aria-label "Rename entity"})
              (icons/Icon :pen-line :size 14)
              (dom/On "click" (fn [_] (reset! !renaming id)) nil)))
          (when (> fact_count 1)
            (SynthesizeButton user-id id label))
          (cond
            (nil? merge-source)
            (dom/button
              (dom/props {:class "btn btn-sm btn-secondary" :style {:margin-left "4px"}
                          :aria-label (str "Merge " label " into another entity")})
              (dom/text "Merge…")
              (dom/On "click" (fn [_] (reset! !merge-source id)) nil))

            (= merge-source id)
            (dom/span (dom/props {:style {:font-size "11px" :margin-left "4px"}})
              (dom/text "(merging)"))

            :else
            (dom/button
              (dom/props {:class "btn btn-sm btn-secondary" :style {:margin-left "4px"}
                          :aria-label (str "Merge into " label)})
              (dom/text "Merge here")
              ;; Merge is irreversible (no undo — see plan 6.5): confirm first.
              (dom/On "click"
                (fn [_] (reset! !merge-confirm {:target id :source merge-source :target-label label}))
                nil))))))))

(e/defn EntitiesView [user-id kg-bump]
  (e/client
    (let [!query (atom "") query (e/watch !query)
          !source-text (atom "") source-text (e/watch !source-text)
          !merge-source (atom nil) merge-source (e/watch !merge-source)
          !merge-confirm (atom nil) merge-confirm (e/watch !merge-confirm)
          !renaming (atom nil) renaming (e/watch !renaming)
          docs (e/server (list-source-docs* kg-bump user-id))
          ;; Resolve the picked title → its doc id; blank / no-match = no filter.
          source-id (some (fn [d] (when (= (:title d) source-text) (:id d))) docs)
          entities (e/server (list-entities* kg-bump user-id query source-id))
          item-count (count entities)]
      (dom/div
        (dom/props {:style {:padding "0 16px" :display "flex" :flex-direction "column"
                            :height "calc(100vh - 160px)"}})
        (dom/div
          (dom/props {:style {:display "flex" :gap "8px" :align-items "center" :margin "10px 0"
                              :flex-wrap "wrap"}})
          (dom/input
            (dom/props {:placeholder "Search entities…" :class "form-input"
                        :aria-label "Search entities"
                        :style {:max-width "320px"}})
            (dom/On "input" (fn [ev] (reset! !query (.. ev -target -value))) nil))
          (dom/div (dom/props {:style {:width "220px"}})
            (typeahead/Typeahead !source-text (mapv :title docs) "Filter by source…" nil nil))
          (dom/span (dom/props {:style {:font-size "12px" :color "var(--color-text-secondary)"}})
            (dom/text (str item-count " entit" (if (= 1 item-count) "y" "ies"))))
          (when merge-source
            (dom/span (dom/props {:style {:font-size "12px" :color "var(--color-text-secondary)"}})
              (dom/text (str "Merging #" merge-source " — pick the surviving entity, or "))
              (dom/button (dom/props {:class "btn btn-sm"})
                (dom/text "cancel")
                (dom/On "click" (fn [_] (reset! !merge-source nil)) nil)))))
        (if (zero? item-count)
          (dom/p (dom/props {:style {:color "var(--color-text-secondary)"}})
            (dom/text (if (and (str/blank? query) (nil? source-id))
                        "No entities yet — distill first." "No matches.")))
          (TapeTable ["Entity" "Aliases" "Facts" ""]
            "minmax(200px,1fr) minmax(160px,1fr) 60px minmax(220px,max-content)" item-count
            (e/fn [i]
              (when-let [entity (nth entities i nil)]
                (EntityRow user-id entity i !renaming renaming !merge-source merge-source !merge-confirm)))))
        ;; Guard the irreversible entity merge behind a confirm (merge undo is out
        ;; of scope, plan 6.5). One modal for the view; !merge-confirm carries the
        ;; {:target :source :target-label} payload set by the row's "Merge here".
        (modal/ConfirmModal !merge-confirm
          "Merge entities?"
          (str "Merge the other entity into " (:target-label merge-confirm)
               "? Its facts and aliases move over and the source entity is removed. This can't be undone.")
          "Merge" "btn btn-danger-fill"
          (e/fn [payload]
            (let [r (e/server (e/Offload #(do (db/merge-kg-entities! user-id (:target payload) (:source payload))
                                           (commands/bump! user-id :distill) :done)))]
              (case r (reset! !merge-source nil)))))))))

;; ---------------------------------------------------------------------------
;; Page
;; ---------------------------------------------------------------------------

(e/defn KnowledgePage [user-id]
  (e/client
    (let [!view (atom :documents)
          view (e/watch !view)
          kg-bump (e/server (e/watch (us/get-atom user-id :kg-mutations)))]
      (dom/div
        (dom/props {:style {:max-width "960px" :margin "0 auto" :padding-bottom "32px"}})
        (SubViewTabs !view view)
        (cond
          (= view :entities) (EntitiesView user-id kg-bump)
          (= view :questions) (QuestionsView user-id kg-bump)
          (vector? view) (FactsView user-id (second view) !view kg-bump)
          :else (dom/div (dom/props {:style {:padding "0 16px"}})
                  (DocumentsView user-id !view kg-bump)))))))
