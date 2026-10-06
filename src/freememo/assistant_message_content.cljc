(ns freememo.assistant-message-content
  "Rendered content for one assistant answer."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [clojure.string :as str]
   [freememo.assistant-evidence :as evidence]
   [freememo.command-bus :as bus]
   [freememo.math :as math]
   [freememo.question-pin-state :as pin-state]))

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

(defn- evidence-text
  "The text of retained evidence `source`. A row that kept its HTML crosses
   without its text (`assistant/evidence-wire-row`), so the text is read back
   from the HTML. The matcher removes all whitespace, so the two agree."
  [source]
  (or (:quoted_text source)
    #?(:cljs (when-let [html (:quoted_html source)]
               (let [t (.createElement js/document "template")]
                 (set! (.-innerHTML t) html)
                 (.. t -content -textContent)))
       :clj nil)))

(defn- pointed-passage
  "The passage the citation under `event` cites, as `pin-state/!finger` takes
   it, or nil when the pointer is not on a citation."
  [event sources]
  (when-let [source (some->> (evidence/clicked-citation event)
                      (evidence/source-for-citation sources))]
    (when-let [quote (not-empty (evidence-text source))]
      ;; Repository evidence names the repository node as its document and the
      ;; file's path as its title, a topic or not, so the pair is the document
      ;; key a pane compares (plans/commit-range-walkthrough.md §6.3.5).
      {:quote quote :anchor (:anchor source)
       :source-doc [(:document_topic_id source) (:document_title source)]})))

(defn- point-at-citation!
  "Light up the passage of the citation under the pointer in the open file,
   or stop when the pointer is on none (plans/anchored-questions.md §6.5.1)."
  [event sources]
  (pin-state/point! (pointed-passage event sources)))

(defn- scroll-to-citation!
  "A citation of the file a pin pane shows scrolls there instead of opening
   the evidence dialog (§6.5.4). A citation of another file of the open
   walkthrough step swaps that file into its canvas first
   (plans/commit-range-walkthrough.md §6.6.4). Post: true when it did either."
  [event sources]
  (let [passage (pointed-passage event sources)
        {:keys [question-pane-doc walkthrough-docs]} @bus/!ctx]
    (cond
      (nil? passage) false
      (and question-pane-doc (= question-pane-doc (:source-doc passage)))
      (do (pin-state/point! (assoc passage :scroll? true)) true)
      (contains? (set walkthrough-docs) (:source-doc passage))
      (do (pin-state/open-in-canvas! (assoc passage :scroll? true)) true)
      :else false)))

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
                (when-not (scroll-to-citation! event (:assistant_run/evidence message))
                  (reset! !evidence source)))))
          nil)
        (dom/On "mouseover"
          (fn [event] (point-at-citation! event (:assistant_run/evidence message)))
          nil)
        (dom/On "mouseleave" (fn [_] (pin-state/point! nil)) nil)
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
