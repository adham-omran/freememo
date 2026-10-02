(ns freememo.assistant-selection-cards
  "The token that turns a Generate cards click in the answer selection menu into
   one queued card job. Its own `e/defn`, so the panel's method stays small.
   See plans/assistant-answer-selection-actions.md §5.2."
  (:require
   [hyperfiddle.electric3 :as e]
   #?(:clj [freememo.assistant :as assistant])))

(e/defn SelectionCardsEffect
  "Queue cards for each new value of `job` = {:id :message-id :html}, or nil.
   Post: the job is queued, or its error is in `!error`. Success reports
   itself with a toast when the cards are saved."
  [job user-id page-topic-id !error]
  (e/client
    (let [[token _] (e/Token job)]
      (when token
        (let [result (e/server
                       (e/Offload
                         #(assistant/selection->cards!
                            user-id page-topic-id (:message-id job) (:html job))))]
          (case result
            (case (when (:error result) (reset! !error (:error result)))
              (token))))))))
