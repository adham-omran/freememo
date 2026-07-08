(ns freememo.content-toolbar-generate
  "Generate buttons, prompt dialog, and card-generation processors for ContentToolbar.
   Extracted to stay under JVM 64KB method limit."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.doc-context :as dctx]
   [clojure.string :as str]
   [freememo.rich-text-editor :as editor]
   [freememo.card-modals :refer [PromptDialog]]
   [freememo.content-toolbar-helpers :as helpers]
   [freememo.icons :as icons]
   [freememo.command-bus :as bus]
   #?(:clj [freememo.settings :as settings])))

(e/defn ToolbarGenerate []
  (e/client
    (let [user-id dctx/user-id enc-key dctx/enc-key topic-id dctx/topic-id root-topic-id dctx/root-topic-id
          page-number dctx/page-number content-text dctx/content-text context-mode dctx/context-mode
          mod-key dctx/mod-key llm-enabled? dctx/llm-enabled?
          card-type dctx/card-type card-count-val dctx/card-count-val use-context dctx/use-context
          context-window dctx/context-window
          gen-active? dctx/gen-active? gen-pending dctx/gen-pending gen-error dctx/gen-error

          ;; All generate/prompt atoms are LOCAL — never passed through a map.
          ;; Passing watched values through a cfg map causes reactive loops:
          ;; any map key change re-derives all destructured values, making
          ;; e/Token re-fire on unchanged values.
          server-prompt-history (e/server (settings/get-pre-prompt-history user-id))
          !gen-click (atom nil)
          gen-click (e/watch !gen-click)
          !prompt-history (atom server-prompt-history)
          !history-save-trigger (atom nil)
          history-save-trigger (e/watch !history-save-trigger)
          !pre-prompt (atom "")
          !show-prompt-dialog (atom false)
          show-prompt-dialog (e/watch !show-prompt-dialog)
          !prompt-dialog-kind (atom nil)
          !captured-selection (atom nil)
          captured-selection (e/watch !captured-selection)
          !prompt-submit (atom nil)
          prompt-submit (e/watch !prompt-submit)]

      ;; Persist prompt history to server
      (when (some? history-save-trigger)
        (e/server (settings/save-pre-prompt-history user-id history-save-trigger)))

      (when llm-enabled?
        ;; Generate buttons group
        (dom/div
          (dom/props {:class "toolbar-generate-group"
                      :style {:display "flex" :gap "8px"}})

          ;; Generate button — label and tooltip include the parameters
          ;; (count, type, context) so users see exactly what will happen.
          (let [no-content? (empty? content-text)
                type-label (case card-type
                             "basic" "Basic"
                             "cloze" "Cloze"
                             (str card-type))
                action-summary (str "Generate " card-count-val " " type-label
                                 (when use-context
                                   (str " · " context-window
                                     " page" (when (not= 1 context-window) "s")
                                     " context")))]
            (dom/button
              (dom/props {:class "btn btn-sm btn-primary"
                          :style {:background (cond no-content? "var(--color-disabled-bg)" gen-active? "var(--color-primary-light)" :else "var(--color-primary)")
                                  :cursor (if no-content? "not-allowed" "pointer")
                                  :font-weight "bold"}
                          :disabled no-content?
                          :aria-label action-summary
                          :data-tooltip (if no-content?
                                          "Extract text first to generate flashcards"
                                          (str action-summary " (" mod-key "+Shift+G)"))})
              (if (and gen-active? (nil? gen-error))
                (icons/Icon :loader-2 :size 16 :class "spin")
                (icons/Icon :sparkles :size 16))
              (dom/span
                (dom/props {:class "icon-label"})
                (dom/text (cond gen-error gen-error
                            gen-active? "Generating..."
                            :else "Generate")))
              (when (and gen-active? (nil? gen-error))
                (dom/span (dom/props {:class "icon-label"}) (dom/text (str " (" gen-pending ")"))))
              (let [node dom/node]
                (bus/publish-invoker! :generate (fn [] (.click node)))
                (e/on-unmount (fn [] (bus/retract-invoker! :generate))))
              (dom/On "click"
                (fn [_]
                  (let [sel (editor/get-selection-html!)]
                    (when sel
                      (editor/highlight-range! (:index sel) (:length sel) :color "var(--color-highlight-gold)"))
                    (reset! !gen-click {:id (str (random-uuid))
                                        :selection-html (when (and sel (not (str/blank? (:text sel))))
                                                          (:html sel))})))
                nil)))

          ;; Generate with Prompt button
          (dom/button
            (dom/props {:class "btn btn-sm btn-secondary"
                        :style {:font-weight "500"}
                        :aria-label "Generate with Prompt"
                        :data-tooltip "Add custom instructions to guide card generation"})
            (icons/Icon :pen-sparkles :size 16)
            (dom/span (dom/props {:class "icon-label"}) (dom/text "Generate with Prompt..."))
            (let [node dom/node]
              (bus/publish-invoker! :gen-prompt (fn [] (.click node)))
              (e/on-unmount (fn [] (bus/retract-invoker! :gen-prompt))))
            (dom/On "click" (fn [_]
                              (let [sel (editor/get-selection-html!)]
                                (when sel
                                  (editor/highlight-range! (:index sel) (:length sel) :color "var(--color-highlight-gold)"))
                                (reset! !captured-selection (when (and sel (not (str/blank? (:text sel))))
                                                              (:html sel))))
                              (reset! !prompt-dialog-kind card-type)
                              (reset! !show-prompt-dialog true))
              nil))))

      ;; Process generate button clicks via e/Token
      (let [[t _] (e/Token gen-click)]
        (when t
          (case (e/server
                  (let [{:keys [content context]}
                        (helpers/build-gen-context*
                          {:selection-html (:selection-html gen-click)
                           :content-text content-text :context-mode context-mode
                           :use-context use-context :topic-id topic-id
                           :root-topic-id root-topic-id :page-number page-number
                           :context-window context-window})]
                    (helpers/enqueue-card-gen!
                      {:id (:id gen-click)
                       :content content
                       :context context
                       :card-type card-type
                       :card-count card-count-val
                       :user-id user-id
                       :enc-key enc-key
                       :topic-id topic-id
                       :root-topic-id root-topic-id
                       :pre-prompt nil})))
            (t))))

      ;; Pre-prompt modal dialog (LLM only)
      (when (and llm-enabled? show-prompt-dialog)
        (PromptDialog {:!show !show-prompt-dialog
                       :!prompt-submit !prompt-submit
                       :!pre-prompt !pre-prompt
                       :!prompt-history !prompt-history
                       :!history-save-trigger !history-save-trigger
                       :captured-selection captured-selection
                       :prompt-dialog-kind (e/watch !prompt-dialog-kind)}))

      ;; Process prompt dialog submissions via e/Token
      (let [submit prompt-submit
            [t _] (e/Token submit)]
        (when t
          (case (e/server
                  (let [{:keys [selection pre-prompt kind]} submit
                        ct (or kind card-type)
                        {:keys [content context]}
                        (helpers/build-gen-context*
                          {:selection-html selection
                           :content-text content-text :context-mode context-mode
                           :use-context use-context :topic-id topic-id
                           :root-topic-id root-topic-id :page-number page-number
                           :context-window context-window})]
                    (helpers/enqueue-card-gen!
                      {:id (str (random-uuid))
                       :content content
                       :context context
                       :card-type ct
                       :card-count card-count-val
                       :user-id user-id
                       :enc-key enc-key
                       :topic-id topic-id
                       :root-topic-id root-topic-id
                       :pre-prompt pre-prompt})))
            (t)))))))
