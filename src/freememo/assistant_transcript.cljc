(ns freememo.assistant-transcript
  "Transcript area for the assistant panel."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.assistant-answer-selection :as answer-selection]
   [freememo.assistant-panel-rows :as rows]
   [freememo.assistant-transcript-entry :as transcript-entry]
   [freememo.measured-list :as measured]
   [freememo.transcript-scroll :as transcript-scroll]
   #?(:clj [freememo.assistant :as assistant])
   #?(:clj [freememo.user-state :as us])))

(defn scroll-to-bottom!
  "Follow the latest message after layout when the reader permits it."
  [node _dependency]
  (transcript-scroll/follow! node))

(defn retire-echo!
  "Remove the optimistic turn only after its durable row reaches the rendered window."
  [!echo node]
  (reset! !echo nil)
  (transcript-scroll/resume! node)
  nil)

(defn follow-submission!
  "Return a submitted turn to the latest region. The identifier prevents repeated effects."
  [node client-id]
  (when client-id (transcript-scroll/resume! node))
  nil)

(e/defn AssistantTranscript
  [{:keys [sending? echo active mode-entry web-search? active-run user-id page-topic-id
           root-topic-id seeded? annotations]}
   !submit !active !refs !details-run !evidence !error !echo
   !annotations !selection-cards !side !pending-fork]
  (e/client
    (let [!following (atom true)
          following? (e/watch !following)
          !node (atom nil)]
      (dom/div
        (dom/props {:class "assistant-panel__transcript-shell"})
        (dom/div
          (dom/props {:class "assistant-panel__transcript" :id "assistant-transcript"
                      :style {:overflow-anchor "none"}})
          (reset! !node dom/node)
          (e/for-by identity [_chat [active]]
            (e/on-unmount
              (transcript-scroll/install! dom/node #(reset! !following %)))
            (e/on-unmount
              (answer-selection/install! dom/node !active !annotations !selection-cards !side)))
          (let [scroller dom/node
                assistant-rev (e/server (e/watch (us/get-atom user-id :assistant-mutations)))
                stream-rev (e/server (e/watch (us/get-atom user-id :assistant-stream)))
                projection (e/server
                             (e/Offload #(assistant/transcript-index* assistant-rev user-id active)))
                message-count (e/server
                                (if (= active (:chat projection)) (count (:events projection)) 0))
                live-row (e/server
                           (e/Offload #(assistant/live-transcript-row*
                                         [assistant-rev stream-rev] user-id active)))]
            (follow-submission! scroller (:id echo))
            (answer-selection/paint! scroller annotations)
            ;; Keep the row owner mounted across empty and loading states.
            (dom/div
              (dom/props {:class "assistant-panel__rows assistant-panel__rows--windowed"})
              (let [window (measured/Window scroller message-count
                             {:estimate-height 160 :reset-key active :initial-anchor :end :follow-end? following? :max-mounted 150})
                    indices (mapv :index (:rows window))
                    tops (into {} (map (juxt :index :top)) (:rows window))
                    result (e/server
                             (e/Offload #(assistant/transcript-window* user-id projection indices)))]
                (dom/props {:style {:position "relative" :height (str (:height window) "px")
                                    :flex "0 0 auto"}})
                (e/for [display-message
                        (e/server
                          (e/diff-by :transcript/key
                            (assistant/transcript-window-rows result projection active indices live-row)))]
                  (let [index (:window/index display-message)]
                    (when (contains? tops index)
                      (dom/div
                        (dom/props {:class "assistant-transcript__window-row"
                                    :data-row-key (str (:transcript/key display-message))
                                    :data-transcript-index index
                                    :style {:position "absolute"
                                            :top "0" :transform (str "translateY(" (get tops index 0) "px)")
                                            :left 0 :right 0}})
                        (measured/Observe scroller index dom/node)
                        (when (and echo (= (:id echo) (:assistant_messages/client_id display-message)))
                          (retire-echo! !echo scroller))
                        (transcript-entry/AssistantTranscriptEntry
                          display-message user-id page-topic-id root-topic-id sending? active
                          !details-run !evidence !error !pending-fork)))))))
            (when (zero? message-count)
              (if (and (not sending?) (nil? echo))
                (when (and seeded? (nil? active))
                  (rows/EmptyChatStarters mode-entry web-search? !submit !active !refs))
                (dom/div
                  (dom/props {:class "assistant-panel__thinking"})
                  (dom/text "Starting…"))))
            (when-let [echo-text (:text echo)]
              (dom/div
                (dom/props {:class "assistant-msg assistant-msg--user"})
                (dom/text echo-text)))
            (when (and sending? (= "compact" (:kind active-run)))
              (dom/div
                (dom/props {:class "assistant-panel__thinking"})
                (dom/text "Compacting context…")))
            (scroll-to-bottom! scroller
              [active message-count stream-rev assistant-rev sending? (:id echo)])))
        (when-not following?
          (dom/button
            (dom/props {:type "button" :class "btn assistant-panel__latest"
                        :aria-controls "assistant-transcript"})
            (dom/text "Return to latest")
            (dom/On "click" (fn [_] (transcript-scroll/resume! @!node)) nil)))))))
