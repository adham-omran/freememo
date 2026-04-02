(ns freememo.content-toolbar-generate
  "Generate buttons, prompt dialog, and card-generation processors for ContentToolbar.
   Extracted to stay under JVM 64KB method limit."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [clojure.string :as str]
   [freememo.rich-text-editor :as editor]
   [freememo.card-modals :refer [PromptDialog]]
   [freememo.content-toolbar-helpers :as helpers]
   [freememo.keyboard :as keyboard]
   #?(:clj [freememo.settings :as settings])))

(e/defn ToolbarGenerate [cfg]
  (e/client
    (let [{:keys [user-id enc-key topic-id root-topic-id page-number
                  content-text parent-content context-mode mod-key
                  source-ref llm-enabled?
                  card-type card-count-val use-context context-window
                  gen-active? gen-pending gen-error]} cfg

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
          (dom/props {:style {:display "flex" :gap "8px" :margin-left "auto"}})

          ;; Generate button
          (let [no-content? (empty? content-text)]
            (dom/button
              (dom/props {:class "btn btn-sm btn-primary"
                          :style {:background (cond no-content? "var(--color-disabled-bg)" gen-active? "var(--color-primary-light)" :else "var(--color-primary)")
                                  :cursor (if no-content? "not-allowed" "pointer")
                                  :font-weight "bold"}
                          :disabled no-content?
                          :title (if no-content? "Extract text first to generate flashcards"
                                   (str "Generate flashcards from editor text or selected text (" mod-key "+Shift+G)"))})
              (when (and gen-active? (nil? gen-error))
                (dom/span (dom/props {:class "spinner"})))
              (dom/text (cond gen-error gen-error
                          gen-active? "Generating..."
                          :else "Generate"))
              (when (and gen-active? (nil? gen-error))
                (dom/text (str " (" gen-pending ")")))
              (reset! keyboard/!generate-btn-ref dom/node)
              (e/on-unmount (fn [] (reset! keyboard/!generate-btn-ref nil)))
              (dom/On "click"
                (fn [_]
                  (let [sel (editor/get-selection!)]
                    (when sel
                      (editor/highlight-range! (:index sel) (:length sel) :color "var(--color-highlight-gold)"))
                    (reset! !gen-click {:id (str (random-uuid)) :selection-text (when sel (:text sel))})))
                nil)))

          ;; Generate with Prompt button
          (dom/button
            (dom/props {:class "btn btn-sm btn-secondary"
                        :style {:font-weight "500"}
                        :title "Add custom instructions to guide card generation"})
            (dom/text "Generate with Prompt...")
            (dom/On "click" (fn [_]
                              (let [sel (editor/get-selection!)]
                                (when sel
                                  (editor/highlight-range! (:index sel) (:length sel) :color "var(--color-highlight-gold)"))
                                (reset! !captured-selection (when sel (:text sel))))
                              (reset! !prompt-dialog-kind card-type)
                              (reset! !show-prompt-dialog true))
              nil))))

      ;; Process generate button clicks via e/Token
      (let [[?token _] (e/Token gen-click)]
        (when-some [token ?token]
          (let [enqueued (e/server
                           (let [sel-text (:selection-text gen-click)
                                 context (when use-context
                                           (case context-mode
                                             :extract (if sel-text content-text parent-content)
                                             :page (let [prev (helpers/get-context-pages* root-topic-id page-number context-window)]
                                                     (if sel-text
                                                       (if prev (str prev "\n\n---\n\n" content-text) content-text)
                                                       prev))))]
                             (helpers/enqueue-card-gen!
                               {:id (:id gen-click)
                                :content (or sel-text content-text)
                                :context context
                                :card-type card-type
                                :card-count card-count-val
                                :user-id user-id
                                :enc-key enc-key
                                :topic-id topic-id
                                :root-topic-id root-topic-id
                                :source-ref source-ref
                                :pre-prompt nil})))]
            (when enqueued (token)))))

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
            [?token _] (e/Token submit)]
        (when-some [token ?token]
          (let [enqueued (e/server
                           (let [{:keys [selection pre-prompt kind]} submit
                                 ct (or kind card-type)
                                 sel-text selection
                                 context (when use-context
                                           (case context-mode
                                             :extract (if sel-text content-text parent-content)
                                             :page (let [prev (helpers/get-context-pages* root-topic-id page-number context-window)]
                                                     (if sel-text
                                                       (if prev (str prev "\n\n---\n\n" content-text) content-text)
                                                       prev))))]
                             (helpers/enqueue-card-gen!
                               {:id (str (random-uuid))
                                :content (or sel-text content-text)
                                :context context
                                :card-type ct
                                :card-count card-count-val
                                :user-id user-id
                                :enc-key enc-key
                                :topic-id topic-id
                                :root-topic-id root-topic-id
                                :source-ref source-ref
                                :pre-prompt pre-prompt})))]
            (when enqueued (token))))))))
