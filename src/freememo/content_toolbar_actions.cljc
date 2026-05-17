(ns freememo.content-toolbar-actions
  "Extract + Add new buttons for ContentToolbar (IR Tools group).
   Export, Pull from Anki, and Anki Sync moved into the unified Sync dropdown
   (`freememo.toolbar-sync-dropdown`) — they're rendered hidden inside that
   component so their refs/modals stay live, and dispatched via `.click()`
   from the menu items.

   Each button is its own e/defn (separate namespace) to stay under the JVM
   64KB method limit."
  (:require
   [hyperfiddle.electric3 :as e]
   [freememo.extract-topic-button :refer [ExtractTopicButton]]
   [freememo.add-card-button :refer [AddCardButton]]))

(e/defn ToolbarActions [cfg]
  (e/client
    (let [{:keys [user-id topic-id root-topic-id
                  context-mode mod-key card-type]} cfg]
      ;; IR Tools group: Extract + Add new. The Sync dropdown is rendered as
      ;; a sibling in ContentToolbar — keeping it separate from this e/defn
      ;; lets each stay small (JVM 64KB bytecode limit) and makes the toolbar
      ;; orchestration explicit in `content_toolbar.cljc`.
      (ExtractTopicButton user-id topic-id context-mode mod-key)
      (AddCardButton user-id topic-id root-topic-id card-type))))
