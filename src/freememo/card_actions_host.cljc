(ns freememo.card-actions-host
  "The owners of the document's card actions, mounted together and rendering
   no control (plans/toolbar-card-actions-fold.md §2).

   Extract, Add new card, Generate, Generate with prompt, Compare models and
   Parse questions used to live in toolbar buttons. Their controls are now the
   selection popover, the right-click menu and the compact toolbar pair, and
   all of them reach these owners through the command bus. Mounting the owners
   here, not in the toolbar, keeps every command available whatever the
   toolbar shows: reading mode, a collapsed toolbar, a popover-less surface.

   Each owner is its own e/defn in its own namespace, for the JVM 64KB method
   limit."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.doc-context :as dctx]
   [freememo.gen-settings :as gs]
   [freememo.command-bus :as bus]
   [freememo.extract-invoker :refer [ExtractInvoker]]
   [freememo.add-card-invoker :refer [AddCardInvoker]]
   [freememo.generate-invoker :refer [GenerateInvokers]]
   [freememo.card-compare :refer [CompareInvoker]]
   [freememo.parse-questions-invoker :refer [ParseQuestionsInvoker]]
   [freememo.document-generate-form :refer [DocumentGenerateForm]]
   [freememo.article-split-dialog :refer [ArticleSplitOwner]]
   #?(:clj [freememo.settings :as settings])))

(defn save-gen-settings!*
  "Save the whole gen-settings map. Server only; the var exists on CLJS because
   a reactive call site names it."
  [user-id gen-settings]
  #?(:clj (settings/save-generate-settings! user-id gen-settings)
     :cljs nil))

(e/defn GenSettingsOwner
  "Seed gen-settings from the user's saved values, and save them on request.

   Pre : the plain-CLJS writer (the selection popover) calls `assoc-setting!`
         first and then runs :persist-gen-settings.
   Post: the atom is seeded once, when the saved values arrive and nothing has
         set it yet. Each persist request saves the map as it stood at
         request time.
   Inv : the value saved comes from the request-time deref, never from an
         e/watch binding, so a save cannot race a just-issued swap!."
  [user-id]
  (e/client
    (let [saved (e/server (settings/get-generate-settings user-id))
          seeded (e/watch gs/!gen-settings)
          !persist-request (atom nil)
          persist-request (e/watch !persist-request)
          [t _] (e/Token persist-request)]
      (when (and (some? saved) (nil? seeded))
        (reset! gs/!gen-settings (gs/effective saved)))
      (bus/publish-invoker! :persist-gen-settings
        (fn [] (reset! !persist-request {:request-id (str (random-uuid))
                                         :settings (gs/current)})))
      (e/on-unmount (fn [] (bus/retract-invoker! :persist-gen-settings)))
      (when t
        (let [r (e/server (e/Offload #(save-gen-settings!* user-id (:settings persist-request))))]
          (when (some? r)
            (if (:success r) (t) (t (:error r)))))))))

(e/defn CardActionsHost
  "Pre : mounted by ToolbarBar, under the toolbar's doc-context bindings.
   Post: every card-action command and invoker is published while mounted."
  []
  (e/client
    (let [user-id dctx/user-id topic-id dctx/topic-id root-topic-id dctx/root-topic-id
          card-type (:kind (gs/effective (e/watch gs/!gen-settings)))]
      ;; display: contents, so the modals the owners mount sit in an element
      ;; of their own and never share child slots with the toolbar.
      (dom/div
        (dom/props {:class "card-actions-host"})
        (GenSettingsOwner user-id)
        (ExtractInvoker user-id topic-id)
        (ArticleSplitOwner user-id topic-id)
        (AddCardInvoker user-id topic-id root-topic-id card-type)
        (GenerateInvokers)
        (CompareInvoker)
        (ParseQuestionsInvoker user-id topic-id root-topic-id)
        (DocumentGenerateForm)))))
