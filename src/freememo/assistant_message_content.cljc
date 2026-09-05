(ns freememo.assistant-message-content
  "Rendered content for one assistant answer."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [clojure.string :as str]
   [freememo.assistant-evidence :as evidence]
   [freememo.math :as math]))

(e/defn AssistantAnswerContent [message !evidence]
  (e/client
    (let [status (:assistant_run/status message)
          content (:assistant_messages/content message)]
      (dom/div
        (dom/props {:class "assistant-msg assistant-msg--assistant" :dir "auto"})
        (dom/On "click"
          (fn [event]
            (when-let [target (evidence/clicked-citation-target event)]
              (when-let [source (evidence/source-for-target
                                  (:assistant_run/evidence message) target)]
                (.preventDefault event)
                (reset! !evidence source))))
          nil)
        (if (str/blank? content)
          (when-not (or (= "failed" status) (= "stopped" status))
            (dom/span
              (dom/props {:class "assistant-msg-status"})
              (dom/text "Thinking…")))
          (do
            (set! (.-innerHTML dom/node)
              (or (:assistant_messages/content-html message) ""))
            (math/render-math! dom/node)))
        (when (= "stopped" status)
          (dom/div
            (dom/props {:class "assistant-msg-status"})
            (dom/text "Stopped")))
        (when (= "failed" status)
          (dom/div
            (dom/props {:class "assistant-msg-status assistant-msg-status--failed"})
            (dom/text
              (str "Failed"
                (when-let [failure (:assistant_run/error message)]
                  (str ": " failure))))))))))
