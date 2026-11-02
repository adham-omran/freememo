(ns freememo.generate-invoker
  "The owner of :generate and :gen-prompt: card generation from the reader's
   selection, or from the whole content when nothing is selected, and the
   pre-prompt dialog. It renders no control except that dialog. The selection
   popover, the palette and ⌘G reach it through the command bus
   (plans/toolbar-card-actions-fold.md §2).

   Kind, count and context come from freememo.gen-settings, the one value the
   popover and the whole-document form also read."
  (:require
   [hyperfiddle.electric3 :as e]
   [freememo.doc-context :as dctx]
   [clojure.string :as str]
   [freememo.card-modals :refer [PromptDialog]]
   [freememo.content-toolbar-helpers :as helpers]
   [freememo.gen-settings :as gs]
   [freememo.selected-passage :as selected-passage]
   [freememo.command-bus :as bus]
   #?(:clj [freememo.activity :as activity])
   #?(:clj [freememo.settings :as settings])))

#?(:clj
   (defn- record-refusal!
     "Generate has no control that stays on screen through the result: the toolbar
      button is gone, and the popover closes. A refused run (queue full) therefore
      goes to the banner strip (plans/remove-toasts.md 2.5.1).
      Post: returns `outcome` unchanged."
     [user-id topic-id outcome]
     (when-not (= :enqueued outcome)
       (activity/record-failure! user-id
         {:error (or (not-empty (str (:error outcome))) "Could not start card generation.")
          :what "Generate cards"
          :subject-id topic-id}))
     outcome))

(defn enqueue-generation!*
  "Build the generation's content and context, then enqueue one run.
   Pre : `selection-html` is nil for a whole-content run. Server only; the var
         exists on CLJS because reactive call sites name it.
   Post: as `helpers/enqueue-card-gen!`."
  [{:keys [id selection-html content-text context-mode use-context topic-id
           root-topic-id page-number context-window is-code? user-id enc-key
           card-type card-count pre-prompt pre-prompt-mode]}]
  #?(:clj (let [{:keys [content context]}
                (helpers/build-gen-context*
                  {:selection-html selection-html
                   :content-text content-text :context-mode context-mode
                   :use-context use-context :topic-id topic-id
                   :root-topic-id root-topic-id :page-number page-number
                   :context-window context-window
                   :is-code? is-code? :user-id user-id})]
            (record-refusal! user-id topic-id
             (helpers/enqueue-card-gen!
              {:id (or id (str (java.util.UUID/randomUUID)))
               :content content
               :context context
               :card-type card-type
               :card-count card-count
               :user-id user-id
               :enc-key enc-key
               :topic-id topic-id
               :root-topic-id root-topic-id
               :pre-prompt pre-prompt
               :pre-prompt-mode pre-prompt-mode})))
     :cljs nil))

(defn- take-selection-html!
  "The reader's selection as HTML, marked gold, or nil when no surface holds
   non-blank text. Generate then reads the whole content."
  []
  #?(:clj nil
     :cljs (let [passage (selected-passage/current)]
             (selected-passage/mark! passage :color "var(--color-highlight-gold)")
             (when-not (str/blank? (:text passage))
               (:html passage)))))

(e/defn GenerateInvokers
  "Pre : mounted once per viewed topic, by CardActionsHost, under the toolbar's
         doc-context bindings (content-text, context-mode, page-number).
   Post: while LLM features are on, :generate and :gen-prompt are available.
         With no content to read, both do nothing, as the disabled buttons did.
         Each :generate invocation enqueues one generation; each :gen-prompt
         invocation opens PromptDialog, and each submit enqueues one.
   Inv : the token atoms are local, so a settings change never re-fires a run."
  []
  (e/client
    (let [user-id dctx/user-id enc-key dctx/enc-key topic-id dctx/topic-id root-topic-id dctx/root-topic-id
          page-number dctx/page-number content-text dctx/content-text context-mode dctx/context-mode
          llm-enabled? dctx/llm-enabled? is-code? dctx/is-code? code-has-source? dctx/code-has-source?
          {card-type :kind card-count :count use-context :context? context-window :context-pages}
          (gs/effective (e/watch gs/!gen-settings))
          ;; A forge file's source is content even when its notes are empty.
          no-content? (and (empty? content-text) (not code-has-source?))

          server-prompt-history (e/server (settings/get-pre-prompt-history user-id))
          server-pre-prompt-mode (e/server (settings/get-pre-prompt-mode user-id))
          !gen-click (atom nil)
          gen-click (e/watch !gen-click)
          !prompt-history (atom server-prompt-history)
          !history-save-trigger (atom nil)
          history-save-trigger (e/watch !history-save-trigger)
          !pre-prompt (atom "")
          !show-prompt-dialog (atom false)
          show-prompt-dialog (e/watch !show-prompt-dialog)
          !prompt-dialog-kind (atom nil)
          prompt-dialog-kind (e/watch !prompt-dialog-kind)
          !captured-selection (atom nil)
          captured-selection (e/watch !captured-selection)
          !prompt-submit (atom nil)
          prompt-submit (e/watch !prompt-submit)]

      (when (some? history-save-trigger)
        (e/server (settings/save-pre-prompt-history user-id history-save-trigger)))

      (when llm-enabled?
        (bus/publish-invoker! :generate
          (fn []
            (when-not no-content?
              (reset! !gen-click {:id (str (random-uuid))
                                  :selection-html (take-selection-html!)}))))
        (bus/publish-invoker! :gen-prompt
          (fn []
            (when-not no-content?
              (reset! !captured-selection (take-selection-html!))
              (reset! !prompt-dialog-kind card-type)
              (reset! !show-prompt-dialog true))))
        (e/on-unmount (fn []
                        (bus/retract-invoker! :generate)
                        (bus/retract-invoker! :gen-prompt))))

      (let [[t _] (e/Token gen-click)]
        (when t
          (case (e/server
                  (enqueue-generation!*
                    {:id (:id gen-click) :selection-html (:selection-html gen-click)
                     :content-text content-text :context-mode context-mode
                     :use-context use-context :topic-id topic-id
                     :root-topic-id root-topic-id :page-number page-number
                     :context-window context-window :is-code? is-code?
                     :user-id user-id :enc-key enc-key
                     :card-type card-type :card-count card-count}))
            (t))))

      (when (and llm-enabled? show-prompt-dialog)
        (PromptDialog {:!show !show-prompt-dialog
                       :!prompt-submit !prompt-submit
                       :!pre-prompt !pre-prompt
                       :!prompt-history !prompt-history
                       :!history-save-trigger !history-save-trigger
                       :captured-selection captured-selection
                       :prompt-dialog-kind prompt-dialog-kind
                       :kind-label (helpers/card-type-label prompt-dialog-kind)
                       :initial-mode server-pre-prompt-mode}))

      (let [submit prompt-submit
            [t _] (e/Token submit)]
        (when t
          (case (e/server
                  (let [{:keys [selection pre-prompt kind pre-prompt-mode]} submit]
                    (enqueue-generation!*
                      {:selection-html selection
                       :content-text content-text :context-mode context-mode
                       :use-context use-context :topic-id topic-id
                       :root-topic-id root-topic-id :page-number page-number
                       :context-window context-window :is-code? is-code?
                       :user-id user-id :enc-key enc-key
                       :card-type (or kind card-type) :card-count card-count
                       :pre-prompt pre-prompt :pre-prompt-mode pre-prompt-mode})))
            (t)))))))
