(ns freememo.walkthrough-state
  "The walkthrough the viewer route names (plans/commit-range-walkthrough.md
   §6.6.7). The route is read in `ViewerContent` and the screen mounts deep
   inside `TopicPage`'s content column, with no common scope between them, so
   the value is a namespace-level atom, the `rich-text-editor/!navigate-topic`
   shape.

   `!route` is {:repo-id :walkthrough-id}, or nil when the route names none.

   NO requires, so both peers read it.")

(defonce !route (atom nil))

(defn route!
  "The viewer route now shows repository node `repo-id` with walkthrough
   `walkthrough-id` open, or no walkthrough when it is nil. Post: nil."
  [repo-id walkthrough-id]
  (reset! !route (when (and repo-id walkthrough-id)
                   {:repo-id repo-id :walkthrough-id walkthrough-id}))
  nil)

(defn open-on
  "The walkthrough id open on repository node `repo-id` in `route`, or nil."
  [route repo-id]
  (when (= repo-id (:repo-id route)) (:walkthrough-id route)))
