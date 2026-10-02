(ns freememo.assistant-message-content
  "Rendered content for one assistant answer."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [clojure.string :as str]
   [freememo.assistant-evidence :as evidence]
   [freememo.math :as math]))

(defn pin-transcript!
  "Scroll the transcript holding `node` to its bottom. CLJ no-op.

   KaTeX typesets on a promise, so an answer carrying math grows AFTER the
   transcript has re-pinned itself — which parked a finished answer with its
   last lines below the fold. This runs on set-html!'s after-render, the one
   moment the final height is known.

   Pre:  `node` is inside a transcript, or inside nothing (a detached leaf).
   Post: the enclosing transcript is at its bottom, or nothing happened."
  [node]
  #?(:cljs (when-let [transcript (.closest node ".assistant-panel__transcript")]
             (set! (.-scrollTop transcript) (.-scrollHeight transcript))
             nil)
     :clj nil))

(defn- announce-rendered!
  "Tell listeners above `node` that its answer HTML is final. The answer
   selection menu repaints annotation highlights on it, because a rewritten
   leaf has new text nodes and the old ranges collapsed. CLJ no-op."
  [node]
  #?(:cljs (.dispatchEvent node (js/CustomEvent. "assistant-answer-rendered"
                                  #js {:bubbles true}))
     :clj nil))

(e/defn AssistantRichTextLeaf [safe-html]
  (e/client
    (dom/div
      (dom/props {:class "assistant-msg__rich-text"})
      ;; Contract: Electric owns this div. math/set-html! owns every descendant.
      (let [node dom/node]
        (math/set-html! node safe-html
          #(do (pin-transcript! node) (announce-rendered! node)))))))

(e/defn AssistantAnswerContent [message !evidence]
  (e/client
    (let [status (:assistant_run/status message)
          content (:assistant_messages/content message)
          render-mode (:assistant_messages/render-mode message)]
      (dom/div
        ;; The answer selection menu reads both data attributes: the id names
        ;; the answer an annotation belongs to, and the status says whether
        ;; the leaf is still being rewritten.
        (dom/props {:class "assistant-msg assistant-msg--assistant" :dir "auto"
                    :data-message-id (:assistant_messages/id message)
                    :data-run-status status})
        (dom/On "click"
          (fn [event]
            (when-let [citation (evidence/clicked-citation event)]
              (when-let [source (evidence/source-for-citation
                                  (:assistant_run/evidence message) citation)]
                (.preventDefault event)
                (reset! !evidence source))))
          nil)
        (if (str/blank? content)
          (when-not (or (= "failed" status) (= "stopped" status))
            (dom/span
              (dom/props {:class "assistant-msg-status"})
              (dom/text "Thinking…")))
          (if (= "rich-html" render-mode)
            (AssistantRichTextLeaf (:assistant_messages/safe-html message))
            (dom/span
              (dom/props {:class "assistant-msg__plain-text"})
              (dom/text content))))
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
