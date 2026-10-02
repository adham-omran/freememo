(ns freememo.content-toolbar-actions
  "Extract + Add new buttons for ContentToolbar (IR Tools group).
   Anki Sync is its own toolbar button (`freememo.anki-sync/AnkiSyncButton`).

   Each button is its own e/defn (separate namespace) to stay under the JVM
   64KB method limit."
  (:require
   [hyperfiddle.electric3 :as e]
   [freememo.doc-context :as dctx]
   [freememo.extract-topic-button :refer [ExtractTopicButton]]
   [freememo.add-card-button :refer [AddCardButton]]
   [freememo.parse-questions-invoker :refer [ParseQuestionsInvoker]]))

(e/defn ToolbarActions []
  (e/client
    (let [user-id dctx/user-id topic-id dctx/topic-id root-topic-id dctx/root-topic-id
          context-mode dctx/context-mode card-type dctx/card-type]
      ;; IR Tools group: Extract + Add new. The Anki Sync button is rendered as
      ;; a sibling in ContentToolbar — keeping it separate from this e/defn
      ;; lets each stay small (JVM 64KB bytecode limit) and makes the toolbar
      ;; orchestration explicit in `content_toolbar.cljc`.
      (ExtractTopicButton user-id topic-id context-mode)
      (AddCardButton user-id topic-id root-topic-id card-type)
      (ParseQuestionsInvoker user-id topic-id root-topic-id))))
