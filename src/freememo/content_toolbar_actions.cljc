(ns freememo.content-toolbar-actions
  "Action buttons and card-generation processing for ContentToolbar.
   Extracted to stay under JVM 64KB method limit."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [clojure.string :as str]
   [freememo.rich-text-editor :as editor]
   [freememo.anki-sync :refer [AnkiSyncButton]]
   [freememo.card-modals :refer [ExportModal PromptDialog AddCardModal]]
   [freememo.content-toolbar-helpers :as helpers]
   [freememo.keyboard :as keyboard]
   #?(:clj [freememo.db :as db])))

(e/defn ToolbarActions [cfg !refresh]
  (e/client
    (let [{:keys [user-id enc-key topic-id root-topic-id page-number
                  content-text parent-content context-mode mod-key
                  source-ref unsynced-count llm-enabled?
                  card-type card-count-val use-context context-window
                  gen-active? gen-pending gen-error gen-click prompt-submit
                  captured-selection show-prompt-dialog prompt-dialog-kind
                  !gen-click !captured-selection !show-prompt-dialog !prompt-dialog-kind
                  !prompt-submit !pre-prompt !prompt-history !history-save-trigger]} cfg]

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
              nil)))) ;; end when llm-enabled? (generation UI)
      
      ;; Extract button — create child topic from selected text
      (let [!extract-state (atom {:pending nil :error nil})
            extract-state (e/watch !extract-state)
            pending (:pending extract-state)]
        (dom/button
          (dom/props {:class "btn btn-sm btn-primary"
                      :style {:font-weight "500"}
                      :title (if (= context-mode :extract)
                               (str "Extract selected text as a child topic (" mod-key "+Shift+E)")
                               (str "Extract selected text as a topic (" mod-key "+Shift+E)"))})
          (dom/text "Extract")
          (reset! keyboard/!extract-btn-ref dom/node)
          (e/on-unmount (fn [] (reset! keyboard/!extract-btn-ref nil)))
          (dom/On "click"
            (fn [_]
              (when-let [{:keys [html index length]} (editor/get-selection-html!)]
                (when (seq html)
                  (editor/highlight-range! index length)
                  (swap! !extract-state assoc :pending html :error nil))))
            nil))
        (when (:error extract-state)
          (dom/span
            (dom/props {:style {:color "var(--color-danger)" :font-size "12px"}})
            (dom/text (:error extract-state))))
        (let [[?token _] (e/Token pending)]
          (when-some [token ?token]
            (let [title (let [raw (str/replace (or pending "") #"<[^>]+>" "")]
                          (if (str/blank? raw) "Extract" (subs raw 0 (min 80 (count raw)))))
                  result (e/server
                           (helpers/create-extract-topic-safe! topic-id user-id pending title))]
              (if (:success result)
                (do (reset! !extract-state {:pending nil :error nil})
                  (token))
                (do (reset! !extract-state {:pending nil :error (or (:error result) "Failed to save extract")})
                  (token (or (:error result) "Failed to save extract"))))))))

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
                                :pre-prompt nil
                                :!refresh !refresh})))]
            (when enqueued (token)))))

      ;; Add new card button — uses AddCardModal with topic-id and root-topic-id
      (let [!show-add (atom false)
            show-add (e/watch !show-add)]
        (dom/button
          (dom/props {:class "btn btn-sm btn-secondary" :style {:font-weight "500"}})
          (dom/text "Add new")
          (dom/On "click" (fn [_] (reset! !show-add true)) nil))
        (when show-add
          (AddCardModal !show-add card-type topic-id root-topic-id !refresh source-ref)))

      ;; Separator
      (dom/span (dom/props {:style {:color "var(--color-border)"}}) (dom/text "|"))

      ;; Export button + modal
      (let [!show-export (atom false)
            show-export (e/watch !show-export)]
        (dom/button
          (dom/props {:class "btn btn-sm btn-secondary" :style {:font-weight "500"}})
          (dom/text (if (pos? unsynced-count)
                      (str "Export (" unsynced-count ")...")
                      "Export..."))
          (dom/On "click" (fn [_] (reset! !show-export true)) nil))
        (when show-export
          (ExportModal !show-export topic-id root-topic-id user-id)))

      ;; Separator
      (dom/span (dom/props {:style {:color "var(--color-border)"}}) (dom/text "|"))

      ;; Anki Sync button — pass root-topic-id as the doc-id equivalent
      (AnkiSyncButton user-id root-topic-id page-number card-type !refresh unsynced-count)

      ;; Pre-prompt modal dialog (LLM only)
      (when (and llm-enabled? show-prompt-dialog)
        (PromptDialog {:!show !show-prompt-dialog
                       :!prompt-submit !prompt-submit
                       :!pre-prompt !pre-prompt
                       :!prompt-history !prompt-history
                       :!history-save-trigger !history-save-trigger
                       :captured-selection captured-selection
                       :prompt-dialog-kind prompt-dialog-kind}))

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
                                :pre-prompt pre-prompt
                                :!refresh !refresh})))]
            (when enqueued (token))))))))
