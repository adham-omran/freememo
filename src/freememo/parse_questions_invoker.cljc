(ns freememo.parse-questions-invoker
  "Own the document selection action that creates parsed basic cards."
  (:require
   [clojure.string :as str]
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.command-bus :as bus]
   [freememo.rich-text-editor :as editor]
   #?(:clj [freememo.cards :as cards])
   #?(:clj [freememo.commands :as commands])
   #?(:clj [freememo.question-card-parser :as parser])
   #?(:clj [freememo.text :as text])
   #?(:clj [freememo.toasts :as toasts])))

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

(defn card-creation-message
  "Report inserted cards and duplicates from one successful batch save."
  [created-count duplicate-count]
  (cond
    (zero? created-count)
    (str "No cards created. Skipped " (counted-noun duplicate-count "duplicate") ".")

    (zero? duplicate-count)
    (str "Created " (counted-noun created-count "card") ".")

    :else
    (str "Created " (counted-noun created-count "card") ". Skipped "
      (counted-noun duplicate-count "duplicate") ".")))

(defn created-cards-highlight-selection?
  "True when a parse result created at least one card from the selection."
  [creation-result]
  (and (true? (:success creation-result))
       (pos? (or (:created creation-result) 0))))

(defn create-selected-question-cards!*
  "Parse and save one selected text block as an all-or-nothing basic-card batch.

   Pre : the caller supplies the current user, topic, root topic, and plain text.
   Post: invalid input writes no cards. Valid input calls save-cards once and
         reports created and duplicate counts through the user's toast queue.
   Inv : only this server boundary decides whether the batch can persist."
  [user-id topic-id root-topic-id selected-text]
  #?(:clj
     (let [{:keys [cards invalid-line-numbers record-count]}
           (parser/parse-question-selection selected-text)]
       (cond
         (zero? record-count)
         (let [selection-error "Select one or more nonblank question-and-answer lines."]
           (toasts/push! user-id {:level :error :message selection-error})
           {:success false :error selection-error})

         (seq invalid-line-numbers)
         (let [validation-error (invalid-question-lines-message invalid-line-numbers)]
           (toasts/push! user-id {:level :error :message validation-error})
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
               (toasts/push! user-id
                 {:level :success
                  :message (card-creation-message created-count duplicate-count)})
               (assoc save-result :created created-count))
             (let [save-error-message (str "Could not create cards: "
                                        (or (:error save-result) "Unknown error"))]
               (toasts/push! user-id {:level :error :message save-error-message})
               (assoc save-result :error save-error-message))))))
     :cljs nil))

(e/defn ParseQuestionsInvoker
  "Host the hidden Electric flow for the selection popover command.

   Pre : a document toolbar supplies the card owner ids.
   Post: each accepted invocation captures the current text and range. A batch
         that creates cards marks its source range with the card-source gold.
   Inv : the published command handle exists only while this component mounts."
  [user-id topic-id root-topic-id]
  (e/client
    (dom/button
      (dom/props {:style {:display "none"}
                  :type "button" :aria-hidden "true" :tabindex "-1"})
      (let [node dom/node]
        (bus/publish-invoker! :parse-questions (fn [] (.click node)))
        (e/on-unmount (fn [] (bus/retract-invoker! :parse-questions))))
      (let [parse-request (dom/On "click"
                            (fn [_]
                              (let [selection (editor/get-selection!)]
                                {:request-id (str (random-uuid))
                                 :selection selection}))
                            nil)
            [t ?error] (e/Token parse-request)]
        (dom/props {:disabled (some? t)})
        (when t
          (let [creation-result (e/server
                                  (e/Offload
                                    #(create-selected-question-cards!*
                                       user-id topic-id root-topic-id
                                       (get-in parse-request [:selection :text]))))]
            (if (:success creation-result)
              (if (created-cards-highlight-selection? creation-result)
                (case (e/client
                        (editor/highlight-range!
                          (get-in parse-request [:selection :index])
                          (get-in parse-request [:selection :length])
                          :color "var(--color-highlight-gold)"))
                  (t))
                (t))
              (t (:error creation-result)))))))))
