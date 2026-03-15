(ns electric-starter-app.extract-toolbar
  "Card generation toolbar for extract page — adapted from ocr-page toolbar."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [electric-starter-app.rich-text-editor :as editor]
   [electric-starter-app.anki-sync :refer [AnkiSyncButton]]
   [electric-starter-app.ocr-modals :refer [ExportModal PromptDialog]]
   [electric-starter-app.extract-cards :as extract-cards]
   #?(:clj [electric-starter-app.cards :as cards])
   #?(:clj [electric-starter-app.settings :as settings])
   #?(:clj [electric-starter-app.db :as db])))

;; State map keys:
;; {:user-id :enc-key :doc-id :page-number :content-text :content-item-id}

(e/defn ExtractToolbar [state]
  (e/client
    (let [{:keys [user-id enc-key doc-id page-number content-text content-item-id]} state
          ;; Fetch document source reference for card propagation
          source-ref (e/server (db/get-document-source doc-id))
          ;; Load settings from server
          server-context-enabled (e/server (settings/get-context-enabled user-id))
          server-context-pages (e/server (settings/get-context-pages user-id))
          server-card-type (e/server (settings/get-card-type user-id))
          server-card-count (e/server (settings/get-card-count user-id))
          server-prompt-history (e/server (settings/get-pre-prompt-history user-id))

          ;; Initialize atoms with server values
          !use-context (atom server-context-enabled)
          use-context (e/watch !use-context)
          !context-window (atom server-context-pages)
          context-window (e/watch !context-window)
          !card-type (atom server-card-type)
          card-type (e/watch !card-type)
          !card-count (atom server-card-count)
          card-count-val (e/watch !card-count)
          !prompt-history (atom server-prompt-history)
          !history-save-trigger (atom nil)
          history-save-trigger (e/watch !history-save-trigger)

          ;; Pre-prompt state
          !pre-prompt (atom "")
          !show-prompt-dialog (atom false)
          show-prompt-dialog (e/watch !show-prompt-dialog)
          !prompt-dialog-kind (atom nil)
          prompt-dialog-kind (e/watch !prompt-dialog-kind)
          !captured-selection (atom nil)
          captured-selection (e/watch !captured-selection)

          ;; Generation queues
          !gen-state (atom {:queue [] :active nil :error nil})
          gen-state (e/watch !gen-state)
          !prompt-gen-state (atom {:queue [] :active nil :error nil})
          prompt-gen-state (e/watch !prompt-gen-state)

          ;; Server-side refresh value for reactive dependency
          refresh (e/server (e/watch extract-cards/!refresh))]

      ;; Persist prompt history to server when Generate is clicked
      (when (some? history-save-trigger)
        (e/server (settings/save-pre-prompt-history user-id history-save-trigger)))

      (dom/div
        (dom/props {:class "toolbar"})

        ;; Context checkbox + pages
        (dom/label
          (dom/props {:style {:display "flex" :align-items "center" :gap "4px" :font-size "13px"}
                      :title "Include context for better cards. With a selection: extract text. Without: original page text."})
          (dom/input
            (dom/props {:type "checkbox" :checked use-context})
            (let [change-event (dom/On "change" (fn [e] (-> e .-target .-checked)) nil)
                  [?token ?error] (e/Token change-event)]
              (when (some? change-event)
                (reset! !use-context change-event))
              (when-some [token ?token]
                (e/server (settings/save-context-enabled user-id change-event))
                (token))))
          (dom/text "Context"))
        (dom/input
          (dom/props {:type "number" :min "1" :max "10" :value (str context-window)
                      :disabled (not use-context)
                      :title "Number of previous pages to include (1-10)"
                      :style {:padding "2px 4px" :font-size "13px" :width "40px"
                              :opacity (if use-context "1" "0.5")}})
          (let [input-event (dom/On "change"
                              (fn [e] (let [v (-> e .-target .-value)]
                                        (if (seq v) (js/parseInt v) nil)))
                              nil)
                [?token ?error] (e/Token input-event)]
            (when (some? input-event)
              (reset! !context-window input-event))
            (when-some [token ?token]
              (e/server (settings/save-context-pages user-id input-event))
              (token))))
        (dom/span (dom/props {:style {:font-size "11px" :color "#999"}}) (dom/text "pages"))

        ;; Separator
        (dom/span (dom/props {:style {:color "#ccc"}}) (dom/text "|"))

        ;; Card type radios
        (dom/label
          (dom/props {:style {:display "flex" :align-items "center" :gap "4px" :font-size "13px"}
                      :title "Traditional question-answer flashcards"})
          (dom/input
            (dom/props {:type "radio" :name "extract-card-type" :value "basic"
                        :checked (= card-type "basic")})
            (let [change-event (dom/On "change" (fn [_] "basic") nil)
                  [?token ?error] (e/Token change-event)]
              (when (some? change-event)
                (reset! !card-type change-event))
              (when-some [token ?token]
                (e/server (settings/save-card-type user-id "basic"))
                (token))))
          (dom/text "Basic"))
        (dom/label
          (dom/props {:style {:display "flex" :align-items "center" :gap "4px" :font-size "13px"}
                      :title "Fill-in-the-blank deletion cards (e.g., 'Paris is the capital of [...]')"})
          (dom/input
            (dom/props {:type "radio" :name "extract-card-type" :value "cloze"
                        :checked (= card-type "cloze")})
            (let [change-event (dom/On "change" (fn [_] "cloze") nil)
                  [?token ?error] (e/Token change-event)]
              (when (some? change-event)
                (reset! !card-type change-event))
              (when-some [token ?token]
                (e/server (settings/save-card-type user-id "cloze"))
                (token))))
          (dom/text "Cloze"))

        ;; Separator
        (dom/span (dom/props {:style {:color "#ccc"}}) (dom/text "|"))

        ;; Card count
        (dom/span
          (dom/props {:style {:display "flex" :align-items "center" :gap "4px" :font-size "13px"}})
          (dom/text "#")
          (dom/input
            (dom/props {:type "number" :min "1" :max "50" :value (str card-count-val)
                        :title "Number of flashcards to generate (1-50)"
                        :style {:padding "2px 4px" :font-size "13px" :width "50px"}})
            (let [input-event (dom/On "change"
                                (fn [e] (let [v (-> e .-target .-value)]
                                          (if (seq v) (js/parseInt v) nil)))
                                nil)
                  [?token ?error] (e/Token input-event)]
              (when (some? input-event)
                (reset! !card-count input-event))
              (when-some [token ?token]
                (e/server (settings/save-card-count user-id input-event))
                (token))))
          (dom/span
            (dom/props {:style {:font-size "11px" :color "#999"}})
            (dom/text "(1-50)")))

        ;; Generate buttons group
        (dom/div
          (dom/props {:style {:display "flex" :gap "8px" :margin-left "auto"}})

          ;; Generate button
          (let [gen-pending (+ (count (:queue gen-state)) (if (:active gen-state) 1 0))
                gen-active? (pos? gen-pending)
                no-content? (empty? content-text)]
            (dom/button
              (dom/props {:class "btn btn-sm btn-primary"
                          :style {:background (cond no-content? "#94a3b8" gen-active? "var(--color-primary-light)" :else "var(--color-primary)")
                                  :cursor (if no-content? "not-allowed" "pointer")
                                  :font-weight "bold"}
                          :disabled no-content?
                          :title (if no-content? "Extract text first to generate flashcards"
                                   "Generate flashcards from editor text or selected text")})
              (dom/text (cond (:error gen-state) (:error gen-state)
                              gen-active? "Generating..."
                              :else "Generate"))
              (when (and gen-active? (nil? (:error gen-state)))
                (dom/text (str " (" gen-pending ")")))
              (dom/On "click"
                (fn [_]
                  (swap! !gen-state (fn [s]
                                      (-> s
                                        (update :queue conj {:id (str (random-uuid))
                                                             :selection (editor/get-selected-text!)})
                                        (assoc :error nil)))))
                nil)))

          ;; Generate with Prompt button
          (dom/button
            (dom/props {:class "btn btn-sm btn-secondary"
                        :style {:font-weight "500"}
                        :title "Add custom instructions to guide card generation"})
            (dom/text (or (:error prompt-gen-state) "Generate with Prompt..."))
            (let [pending (+ (count (:queue prompt-gen-state)) (if (:active prompt-gen-state) 1 0))]
              (when (and (pos? pending) (nil? (:error prompt-gen-state)))
                (dom/text (str " (" pending ")"))))
            (dom/On "click" (fn [_]
                              (reset! !captured-selection (editor/get-selected-text!))
                              (reset! !prompt-dialog-kind card-type)
                              (reset! !show-prompt-dialog true))
              nil)))

        ;; Extract button — create child extract from selected text
        (let [!extract-state (atom {:pending nil :error nil})
              extract-state (e/watch !extract-state)
              pending (:pending extract-state)]
          (dom/button
            (dom/props {:class "btn btn-sm btn-primary"
                        :style {:font-weight "500"}
                        :title "Extract selected text as a child content item"})
            (dom/text "Extract")
            (dom/On "click"
              (fn [_]
                (when-let [{:keys [text index length]} (editor/get-selection!)]
                  (when (seq text)
                    (editor/highlight-range! index length)
                    (swap! !extract-state assoc :pending text :error nil))))
              nil))
          (when (:error extract-state)
            (dom/span
              (dom/props {:style {:color "red" :font-size "12px"}})
              (dom/text (:error extract-state))))
          (let [[?token _] (e/Token pending)]
            (when-some [token ?token]
              (let [result (e/server (db/save-content-item doc-id page-number "html" pending content-item-id))]
                (if result
                  (do (e/server (swap! extract-cards/!refresh inc))
                    (reset! !extract-state {:pending nil :error nil})
                    (token))
                  (do (reset! !extract-state {:pending nil :error "Failed to save extract"})
                    (token "Failed to save extract")))))))

        ;; Auto-advance: when not processing and queue has items, start next
        (when (and (nil? (:active gen-state)) (seq (:queue gen-state)))
          (swap! !gen-state (fn [{:keys [queue]}]
                              {:active (first queue) :queue (vec (rest queue))})))

        ;; Queue processor — processes one generation request at a time
        (when-some [current (:active gen-state)]
          (let [[?token _?error] (e/Token (:id current))]
            (when-some [token ?token]
              (let [content (or (:selection current) content-text)
                    context-text (when use-context
                                   (if (:selection current)
                                     content-text
                                     (e/server (cards/get-extract-page-context doc-id page-number))))
                    generate-result (e/server
                                      (if (= card-type "basic")
                                        (cards/generate-basic-cards
                                          {:content content :context context-text
                                           :card-count card-count-val :user-id user-id :enc-key enc-key})
                                        (cards/generate-cloze-cards
                                          {:content content :context context-text
                                           :card-count card-count-val :user-id user-id :enc-key enc-key})))]
                (if-not (:success generate-result)
                  (do (token (:error generate-result))
                    (swap! !gen-state assoc :active nil :error (:error generate-result)))
                  (let [generated-cards (e/server (:cards generate-result))
                        save-result (e/server (cards/save-cards doc-id page-number card-type generated-cards content-item-id source-ref))]
                    (if (:success save-result)
                      (do (e/server (swap! extract-cards/!refresh inc))
                        (token)
                        (swap! !gen-state assoc :active nil))
                      (do (token (:error save-result))
                        (swap! !gen-state assoc :active nil)))))))))

        ;; Auto-advance for prompt queue
        (when (and (nil? (:active prompt-gen-state)) (seq (:queue prompt-gen-state)))
          (swap! !prompt-gen-state (fn [{:keys [queue]}]
                                     {:active (first queue) :queue (vec (rest queue))})))

        ;; Prompt queue processor
        (when-some [current (:active prompt-gen-state)]
          (let [[?token _?error] (e/Token (:id current))]
            (when-some [token ?token]
              (let [content (or (:selection current) content-text)
                    prompt-text (:pre-prompt current)
                    kind (:kind current)
                    context-text (when use-context
                                   (if (:selection current)
                                     content-text
                                     (e/server (cards/get-extract-page-context doc-id page-number))))
                    generate-result (e/server
                                      (if (= kind "basic")
                                        (cards/generate-basic-cards
                                          {:content content :context context-text
                                           :card-count card-count-val :user-id user-id
                                           :enc-key enc-key :pre-prompt prompt-text})
                                        (cards/generate-cloze-cards
                                          {:content content :context context-text
                                           :card-count card-count-val :user-id user-id
                                           :enc-key enc-key :pre-prompt prompt-text})))]
                (if-not (:success generate-result)
                  (do (token (:error generate-result))
                    (swap! !prompt-gen-state assoc :active nil :error (:error generate-result)))
                  (let [generated-cards (e/server (:cards generate-result))
                        save-result (e/server (cards/save-cards doc-id page-number kind generated-cards content-item-id source-ref))]
                    (if (:success save-result)
                      (do (e/server (swap! extract-cards/!refresh inc))
                        (token)
                        (swap! !prompt-gen-state assoc :active nil))
                      (do (token (:error save-result))
                        (swap! !prompt-gen-state assoc :active nil)))))))))

        ;; Add new card button
        (let [!show-add (atom false)
              show-add (e/watch !show-add)]
          (dom/button
            (dom/props {:class "btn btn-sm btn-secondary" :style {:font-weight "500"}})
            (dom/text "Add new")
            (dom/On "click" (fn [_] (reset! !show-add true)) nil))
          (when show-add
            (extract-cards/ExtractAddCardModal !show-add card-type doc-id page-number content-item-id source-ref)))

        ;; Separator
        (dom/span (dom/props {:style {:color "#ccc"}}) (dom/text "|"))

        ;; Export button + modal
        (let [!show-export (atom false)
              show-export (e/watch !show-export)]
          (dom/button
            (dom/props {:class "btn btn-sm btn-secondary" :style {:font-weight "500"}})
            (dom/text "Export...")
            (dom/On "click" (fn [_] (reset! !show-export true)) nil))
          (when show-export
            (ExportModal !show-export doc-id page-number user-id)))

        ;; Separator
        (dom/span (dom/props {:style {:color "#ccc"}}) (dom/text "|"))

        ;; Anki Sync button
        (AnkiSyncButton user-id doc-id page-number card-type extract-cards/!refresh 0))

      ;; Pre-prompt modal dialog
      (when show-prompt-dialog
        (PromptDialog {:!show !show-prompt-dialog
                       :!prompt-gen-state !prompt-gen-state
                       :!pre-prompt !pre-prompt
                       :!prompt-history !prompt-history
                       :!history-save-trigger !history-save-trigger
                       :captured-selection captured-selection
                       :prompt-dialog-kind prompt-dialog-kind})))))
