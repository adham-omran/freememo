(ns freememo.assistant-message-actions
  "Actions for one assistant answer."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [clojure.string :as str]
   [freememo.icons :as icons]
   #?(:clj [freememo.assistant :as assistant])))

(e/defn AssistantMessageActions
  [message user-id page-topic-id root-topic-id sending? !details-run !error]
  (e/client
    (let [status (:assistant_run/status message)
          content (:assistant_messages/content message)
          run-uuid (:assistant_run/uuid message)]
      (when run-uuid
        (dom/div
          (dom/props {:class "assistant-msg-actions"})
          (when-not (str/blank? content)
            (dom/button
              (dom/props {:class "assistant-msg-card" :type "button"
                          :title "Generate a flashcard from this reply"
                          :aria-label "Generate a flashcard from this reply"
                          :disabled sending?})
              (let [click (dom/On "click" identity nil)
                    [generate-token _] (e/Token click)
                    generating? (some? generate-token)]
                (dom/props {:disabled (or sending? generating?)})
                (dom/text (if generating? "✦…" "✦"))
                (when generate-token
                  (case (e/server
                          (e/Offload
                            #(assistant/reply->cards!
                               user-id page-topic-id root-topic-id content)))
                    (generate-token))))))
          (dom/button
            (dom/props {:class "assistant-msg-card assistant-msg-details"
                        :type "button" :title "Answer details"
                        :aria-label "Answer details"})
            (icons/Icon :info :size 14)
            (dom/On "click" (fn [_] (reset! !details-run run-uuid)) nil))
          (when (= "failed" status)
            (dom/button
              (dom/props {:class "assistant-msg-card" :type "button"
                          :title "Retry answer" :aria-label "Retry answer"
                          :disabled sending?})
              (dom/text "↻")
              (let [retry-click (dom/On "click" identity nil)
                    [retry-token _] (e/Token retry-click)]
                (when retry-token
                  (let [result (e/server
                                 (e/Offload
                                   #(assistant/retry-answer-run!
                                      user-id run-uuid (str (random-uuid)))))]
                    (case result
                      (case (reset! !error
                              (when-not (:ok result) (:error result)))
                        (retry-token)))))))))))))
