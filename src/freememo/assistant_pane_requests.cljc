(ns freememo.assistant-pane-requests
  "The commands that open the assistant pane before the AssistantPanel takes
   them (plans/anchored-questions.md §6.2.5, plans/question-map-tab.md §4.6.1,
   plans/commit-range-walkthrough.md §6.7.1, plans/document-question-pins.md
   §4.4). The AssistantPanel is not mounted while the pane is closed or on
   another tab, so RightSidePanel opens it, and the mounted panel takes the
   waiting invocation.

   Its own namespace for the 64KB method cap: Electric inlines same-namespace
   e/defn calls, and RightSidePanel is at the cap."
  (:require
   [hyperfiddle.electric3 :as e]
   [freememo.command-bus :as bus]
   [freememo.question-pin-state :as pin-state]))

(e/defn OpenForRequests
  "Open the assistant pane, through `show-right-pane!`, for each waiting Ask
   and walkthrough opener, and on the Map view for Show question map. With no
   map, the panel falls back to Chat once its read returns (§4.2.4)."
  [show-right-pane!]
  (e/client
    (e/for [_request (bus/Requests :ask-question-pins)]
      (show-right-pane! "assistant"))
    (e/for [_request (bus/Requests :ask-question-pin)]
      (show-right-pane! "assistant"))
    (e/for [_request (bus/Requests :ask-walkthrough-tutor)]
      (show-right-pane! "assistant"))
    (e/for [{:keys [invocation-id]} (bus/Requests :show-question-map)]
      (let [[t _] (e/Token invocation-id)]
        (when t
          (case (show-right-pane! "assistant")
            (case (pin-state/show-view! :map)
              (case (bus/consume! invocation-id)
                (t)))))))))
