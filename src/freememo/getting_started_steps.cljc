(ns freememo.getting-started-steps
  "The getting-started checklist as data, and the one function that turns a
   user's progress into what Home shows (plans/getting-started-checklist.md
   2.1). No requires, so `clj -M:test` reaches it with no browser and no
   database.")

(def steps
  "The six steps in the order Home lists them (2.3, 8.5.1). `:done-key` names
   the boolean of `db/get-getting-started-progress` that completes the step.
   Only Anki is optional (Q5)."
  [{:id :import :done-key :imported? :label "Import a document"
    :text "Add a PDF, web page or EPUB. A web page gives you text at once."}
   {:id :learn :done-key :read? :label "Read it in Learn"
    :text "Learn shows one document at a time. Press Next when you finish."}
   {:id :extract :done-key :extracted? :label "Extract a passage"
    :text "Select a passage, then press Extract."}
   {:id :cards :done-key :carded? :label "Make flashcards"
    :text "Generate cards from an extract."}
   {:id :quiz :done-key :reviewed? :label "Review a card"
    :text "Quiz asks each card when it is due."}
   {:id :anki :done-key :anki-pushed? :label "Send cards to Anki" :optional? true
    :text "Optional: send your cards to Anki on this computer."}])

(def ai-unavailable-text
  "The text of the flashcard step when the server has no OpenRouter key (2.4)."
  "AI is not set up on this server.")

(defn checklist
  "What Home shows for the getting-started checklist (2.1, 2.2).

   Pre : `progress` holds the booleans of `db/get-getting-started-progress`.
         `dismissed?` is the settings flag. `ai-configured?` is false when the
         operator set no OpenRouter key.
   Post: nil when the checklist hides: it is dismissed, or every required
         step that can be done is done. Otherwise one map per step, in
         `steps` order, with :state ∈ #{:done :next :pending :unavailable}.
         Exactly one step is :next, the first that is neither done nor
         unavailable. The flashcard step is :unavailable only while it is not
         done, because a card added by hand completes it with no AI."
  [{:keys [progress dismissed? ai-configured?]}]
  (let [items (mapv (fn [{:keys [id done-key] :as step}]
                      (cond
                        (get progress done-key) (assoc step :state :done)
                        (and (= id :cards) (not ai-configured?))
                        (assoc step :state :unavailable :text ai-unavailable-text)
                        :else (assoc step :state :pending)))
                steps)
        required-open? (some #(and (= :pending (:state %)) (not (:optional? %))) items)]
    (when (and (not dismissed?) required-open?)
      (let [next-index (first (keep-indexed (fn [i item] (when (= :pending (:state item)) i))
                                items))]
        (assoc-in items [next-index :state] :next)))))
