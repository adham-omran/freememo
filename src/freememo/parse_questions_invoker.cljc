(ns freememo.parse-questions-invoker
  "Own the document selection action that creates parsed basic cards."
  (:require
   [clojure.string :as str]
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.command-bus :as bus]
   [freememo.inline-error :refer [InlineError]]
   [freememo.result-line :refer [ResultLine]]
   [freememo.rich-text-editor :as editor]
   #?(:clj [freememo.cards :as cards])
   #?(:clj [freememo.commands :as commands])
   #?(:clj [freememo.question-card-parser :as parser])
   #?(:clj [freememo.text :as text])))

(defn- counted-noun [n singular]
  (str n " " singular (when (not= 1 n) "s")))

(defn- natural-line-list
  "Format one-based line numbers for a validation message."
  [line-numbers]
  (let [labels (mapv str line-numbers)]
    (case (count labels)
      0 ""
      1 (first labels)
      2 (str (first labels) " and " (second labels))
      (str (str/join ", " (butlast labels)) ", and " (last labels)))))

(defn invalid-question-lines-message
  "Return the actionable error for invalid physical lines."
  [line-numbers]
  (str "Could not parse "
    (if (= 1 (count line-numbers)) "line " "lines ")
    (natural-line-list line-numbers)
    ". Each nonblank line must contain one ? with text on both sides."))

(defn result-line-text
  "The count line beside the control, such as \"3 created, 2 skipped as
   duplicates\". A batch that skipped nothing reads \"3 created\"."
  [created-count duplicate-count]
  (if (zero? duplicate-count)
    (str created-count " created")
    (str created-count " created, " duplicate-count " skipped as "
      (if (= 1 duplicate-count) "duplicate" "duplicates"))))

(defn created-cards-highlight-selection?
  "True when a parse result created at least one card from the selection."
  [creation-result]
  (and (true? (:success creation-result))
       (pos? (or (:created creation-result) 0))))

(defn create-selected-question-cards!*
  "Parse and save one selected text block as an all-or-nothing basic-card batch.

   Pre : the caller supplies the current user, topic, root topic, and plain text.
   Post: invalid input writes no cards. Valid input calls save-cards once and
         returns the created and duplicate counts and the error text.
         It reports nothing else: the invoker shows both beside itself.
   Inv : only this server boundary decides whether the batch can persist."
  [user-id topic-id root-topic-id selected-text]
  #?(:clj
     (let [{:keys [cards invalid-line-numbers record-count]}
           (parser/parse-question-selection selected-text)]
       (cond
         (zero? record-count)
         {:success false
          :error "Select one or more nonblank question-and-answer lines."}

         (seq invalid-line-numbers)
         (let [validation-error (invalid-question-lines-message invalid-line-numbers)]
           {:success false :error validation-error
            :invalid-line-numbers invalid-line-numbers})

         :else
         (let [save-ready-cards
               (mapv (fn [{:keys [question answer]}]
                       {:q (text/escape-html question)
                        :a (text/escape-html answer)})
                 cards)
               ;; Plain selected text contains no prefilled pins. Saving with
               ;; bake disabled keeps this action limited to the parsed fields.
               save-result (cards/save-cards user-id topic-id root-topic-id
                             "basic" save-ready-cards false)]
           (if (:success save-result)
             (let [created-count (count (:ids save-result))
                   duplicate-count (or (:skipped save-result) 0)]
               (when (pos? created-count)
                 (commands/bump! user-id :parse-questions))
               (assoc save-result :created created-count))
             (let [save-error-message (str "Could not create cards: "
                                        (or (:error save-result) "Unknown error"))]
               (assoc save-result :error save-error-message))))))
     :cljs nil))

(e/defn ParseQuestionsInvoker
  "Host the Electric flow for the selection popover command.

   Pre : CardActionsHost supplies the card owner ids.
   Post: each invocation captures the current text and range. A batch that
         creates cards marks its source range with the card-source gold.
   Inv : the published command handle exists only while this component mounts,
         and the request atom is local, so a token fires only on an invocation.
         Its status span is always mounted. It shows the count line after a
         save and the error after a failure, and the next invocation clears
         the count."
  [user-id topic-id root-topic-id]
  (e/client
    (let [!parse-request (atom nil)
          !result-text (atom nil)
          parse-request (e/watch !parse-request)
          [t ?error] (e/Token parse-request)]
      (bus/publish-invoker! :parse-questions
        (fn []
          (reset! !result-text nil)
          (reset! !parse-request {:request-id (str (random-uuid))
                                  :selection (editor/get-selection!)})))
      (e/on-unmount (fn [] (bus/retract-invoker! :parse-questions)))
      (dom/span
        (dom/props {:class "parse-questions-status"})
        (ResultLine (e/watch !result-text))
        (InlineError ?error))
      (when t
        (let [creation-result (e/server
                                (e/Offload
                                  #(create-selected-question-cards!*
                                     user-id topic-id root-topic-id
                                     (get-in parse-request [:selection :text]))))]
          ;; e/Offload answers nil while the save runs. Spending the token
          ;; then would unmount this branch and cancel the save.
          (when (some? creation-result)
            (if (:success creation-result)
              (let [line (result-line-text (or (:created creation-result) 0)
                           (or (:skipped creation-result) 0))]
                ;; The line is set as the token spends, so the span never
                ;; shows a count for a run that has not finished.
                (e/on-unmount #(reset! !result-text line))
                (if (created-cards-highlight-selection? creation-result)
                  (case (e/client
                          (editor/highlight-range!
                            (get-in parse-request [:selection :index])
                            (get-in parse-request [:selection :length])
                            :color "var(--color-highlight-gold)"))
                    (t))
                  (t)))
              (t (:error creation-result)))))))))
