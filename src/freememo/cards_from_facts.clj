(ns freememo.cards-from-facts
  "Bridge: knowledge-graph facts → flashcards. When a document has approved KG
   facts, the Generate action picks the facts a passage supports (one LLM call)
   and renders them to statement text, which the existing card generator turns
   into cards. Facts are the card source (not kg_questions); the passage only
   selects which facts. No facts, or none relevant → the caller falls back to
   raw-text generation."
  (:require
   [freememo.credits :as credits]
   [freememo.cards :as cards]
   [freememo.db :as db]
   [freememo.kg-llm :as llm]
   [taoensso.telemere :as tel]
   [clojure.string :as str]))

(defn weak-fact?
  "Whether a fact still needs practice, from its db/get-fact-mastery entry.
   Pre:  `m` is a get-fact-mastery value, or nil (fact has no linked approved
         question — never quizzable, hence never practiced).
   Post: true when the fact is untested, due, or has lapsed; nil ⇒ true."
  [m]
  (or (nil? m)
    (not (:tested? m))
    (:due? m)
    (pos? (:lapses m))))

(defn- fact->select-row
  "get-kg-facts display row → the {:id :s :p :o} shape the selector prompt
   consumes. :o is the object entity label, falling back to the literal."
  [{:keys [id subject_label predicate_label object_label object_literal]}]
  {:id id :s subject_label :p predicate_label :o (or object_label object_literal)})

(defn- fact->statement
  "Fact row → its one-line subject-predicate-object statement. Single source for
   render-facts and the logs so the logged text matches the generated-from text."
  [{:keys [subject_label predicate_label object_label object_literal]}]
  (str subject_label " " predicate_label " " (or object_label object_literal)))

(defn render-facts
  "Selected fact rows → a plain-text statement list — the facts-only content
   handed to the existing card generator (no passage text, per facts-only
   grounding)."
  [facts]
  (->> facts
    (map #(str "- " (fact->statement %) "."))
    (str/join "\n")))

(defn select-relevant-facts
  "LLM-select the facts `passage` supports out of `candidate-facts`
   (get-kg-facts display rows). Returns {:success true :facts rows :cost usd}
   with rows ⊆ candidate-facts (empty allowed — the passage may support none),
   or {:success false :error msg :error-type kw?}. Bills the call. Logs the
   model's raw ids, the dropped (out-of-candidate) ids, and the selected ids +
   statements under ::fact-selection-complete.
   Pre:  candidate-facts non-empty; passage a non-blank string; root-topic-id is
         the facts' graph_topic_id (log correlation).
   Post: every returned row's :id was present in candidate-facts (hallucinated
         ids are dropped — the model cannot invent a fact)."
  [user-id root-topic-id passage candidate-facts]
  (try
    (let [{:keys [api-key entry model-slug]} (llm/resolve-model+gate! user-id :fact-select)
          prompt (cards/load-prompt-template "select-facts.md")
          _ (when-not prompt (throw (ex-info "Failed to load select-facts prompt" {})))
          {:keys [parsed cost]} (llm/chat! api-key model-slug prompt
                                  (pr-str {:passage passage
                                           :facts (mapv fact->select-row candidate-facts)}))
          by-id (into {} (map (juxt :id identity)) candidate-facts)
          model-ids (into [] (filter int?) (when (sequential? parsed) parsed))
          dropped-ids (into [] (remove by-id) model-ids)
          relevant (into [] (comp (distinct) (keep by-id))
                     (when (sequential? parsed) parsed))
          ;; Reorder weak-first (untested/due/lapsed lead) so card generation
          ;; spends the card-count budget on the facts that most need practice.
          ;; Reorder only — every passage-supported fact stays eligible; a stable
          ;; sort preserves the model's order within each weak/mastered band.
          mastery (db/get-fact-mastery user-id (mapv :id relevant))
          prioritized (vec (sort-by #(if (weak-fact? (get mastery (:id %))) 0 1) relevant))]
      (credits/record-cost-charge! user-id :cards.fact-select (:id entry) cost)
      (tel/log! {:level :info :id ::fact-selection-complete
                 :data {:user-id user-id
                        :root-topic-id root-topic-id
                        :candidate-count (count candidate-facts)
                        :model-selected-ids model-ids
                        :dropped-ids dropped-ids
                        :selected-ids (mapv :id prioritized)
                        :selected-facts (mapv (fn [f] {:id (:id f) :stmt (fact->statement f)}) prioritized)
                        :cost-usd cost}}
        "Card-gen fact selection")
      {:success true :facts prioritized :cost cost})
    (catch Exception e
      (if (llm/insufficient-credits? e)
        (tel/log! {:level :info :id ::spend-refused :data {:user-id user-id}}
          "Fact selection refused: out of credits")
        (tel/error! {:id ::select-relevant-facts} e))
      {:success false
       :error (ex-message (llm/root-cause e))
       :error-type (when (llm/insufficient-credits? e) :insufficient-credits)})))
