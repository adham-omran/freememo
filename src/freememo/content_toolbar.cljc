(ns freememo.content-toolbar
  "Unified card generation toolbar for both page-level and extract-level content.
   Replaces extract_toolbar.cljc and the inline toolbar in ocr_page.cljc.
   Uses topic-id + root-topic-id instead of doc-id + page-number + content-item-id."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [clojure.string :as str]
   [freememo.logging :as log]
   [freememo.rich-text-editor :as editor]
   [freememo.anki-sync :refer [AnkiSyncButton]]
   [freememo.card-modals :refer [ExportModal PromptDialog AddCardModal]]
   #?(:clj [freememo.cards :as cards])
   #?(:clj [freememo.settings :as settings])
   [freememo.keyboard :as keyboard]
   #?(:clj [freememo.db :as db])
   [freememo.util :refer [mac-platform?]]))

;; Unsynced card count — single topic-id
(defn get-unsynced-count* [_refresh topic-id]
  #?(:clj (db/get-unsynced-card-count topic-id)
     :cljs 0))

;; Save generated cards to DB using topic-id and root-topic-id
(defn save-cards-for-topic [topic-id root-topic-id kind generated-cards source-ref]
  #?(:clj
     (try
       (let [rows (mapv (fn [card]
                          (cond-> {:topic_id topic-id
                                   :root_topic_id root-topic-id
                                   :kind kind}
                            (= kind "basic") (assoc :question (:q card) :answer (:a card))
                            (= kind "cloze") (assoc :cloze (:c card))
                            source-ref (assoc :source_reference source-ref)))
                    generated-cards)]
         (db/insert-flashcards! rows)
         {:success true})
       (catch Exception e
         {:success false :error (.getMessage e)}))
     :cljs nil))

;; Get context from previous pages (for page-mode card generation)
(defn get-context-pages* [root-topic-id page-number context-window]
  #?(:clj
     (let [start-page (max 1 (- page-number context-window))
           end-page (dec page-number)]
       (when (>= end-page start-page)
         (let [pages (db/get-context-pages root-topic-id start-page end-page)]
           (when (seq pages)
             (->> pages
               (map :topics/content)
               (str/join "\n\n---\n\n"))))))
     :cljs nil))

;; Create an extract child topic — wrapped in try/catch outside e/defn
(defn create-extract-topic-safe! [parent-id user-id content title]
  #?(:clj (try
            (db/create-topic! {:parent-id parent-id
                               :user-id user-id
                               :content content
                               :kind "basic"
                               :title title})
            {:success true}
            (catch Exception e
              {:success false :error (.getMessage e)}))
     :cljs nil))

;; State map keys:
;; {:user-id       — user identifier
;;  :enc-key       — encryption key
;;  :topic-id      — the entity (page or extract topic) where cards are created
;;  :root-topic-id — the root topic for scoping (PDF root, standalone root, etc.)
;;  :page-number   — current page number (for page-level context queries, nil for non-PDF)
;;  :content-text  — full text (page text for :page mode, extract text for :extract mode)
;;  :parent-content — parent topic's content (for extract context, nil for page mode)
;;  :context-mode  — :page or :extract
;;  :context-tooltip — string for the Context checkbox title attribute
;;  :llm-enabled?  — whether LLM features are enabled
;; }

(e/defn ContentToolbar [state !refresh]
  (e/client
    (let [mod-key (if (mac-platform?) "Cmd" "Ctrl")
          {:keys [user-id enc-key topic-id root-topic-id page-number content-text
                  parent-content context-mode context-tooltip llm-enabled?]} state
          ;; Fetch source reference from root topic for card propagation
          source-ref (e/server (db/get-topic-source root-topic-id))
          ;; Unsynced card count — uses refresh wrapper for reactivity
          toolbar-refresh (e/server (e/watch !refresh))
          unsynced-count (e/server (get-unsynced-count* toolbar-refresh topic-id))
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

          ;; Unique radio group name to avoid collision when both page and extract toolbars exist
          radio-name (if (= context-mode :extract) "extract-card-type" "card-type")]

      ;; Persist prompt history to server when Generate is clicked
      (when (some? history-save-trigger)
        (e/server (settings/save-pre-prompt-history user-id history-save-trigger)))

      (dom/div
        (dom/props {:class "toolbar"})

        (when llm-enabled?
        ;; Context checkbox + pages
          (dom/label
            (dom/props {:style {:display "flex" :align-items "center" :gap "4px" :font-size "13px"}
                        :title (or context-tooltip "Include context for better cards")})
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
          (dom/span (dom/props {:style {:font-size "11px" :color "var(--color-text-hint)"}}) (dom/text "pages"))

        ;; Separator
          (dom/span (dom/props {:style {:color "var(--color-border)"}}) (dom/text "|"))

        ;; Card type radios
          (dom/label
            (dom/props {:style {:display "flex" :align-items "center" :gap "4px" :font-size "13px"}
                        :title "Traditional question-answer flashcards"})
            (dom/input
              (dom/props {:type "radio" :name radio-name :value "basic"
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
              (dom/props {:type "radio" :name radio-name :value "cloze"
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
          (dom/span (dom/props {:style {:color "var(--color-border)"}}) (dom/text "|"))

        ;; Card count with +/- stepper for touch devices
          (dom/span
            (dom/props {:class "card-count-control"
                        :style {:display "flex" :align-items "center" :gap "4px" :font-size "13px"}})
            (dom/text "#")

            ;; Decrement button (visible on touch only via CSS)
            (dom/button
              (dom/props {:class "card-count-btn"
                          :title "Decrease card count"
                          :style {:width "28px" :height "28px" :border "1px solid var(--color-border)"
                                  :border-radius "4px" :background "var(--color-bg-subtle)"
                                  :font-size "16px" :cursor "pointer" :display "flex"
                                  :align-items "center" :justify-content "center"
                                  :padding "0"}})
              (dom/text "\u2212")
              (e/for [[t e] (dom/On-all "click")]
                (when e
                  (swap! !card-count (fn [v] (max 1 (dec v))))
                  (e/server (settings/save-card-count user-id (max 1 (dec @!card-count)))))
                (t)))

            ;; Number input (keyboard suppressed on touch via inputmode)
            (dom/input
              (dom/props {:type "number" :min "1" :max "50" :value (str card-count-val)
                          :inputmode "none"
                          :title "Number of flashcards to generate (1-50)"
                          :class "card-count-input"
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

            ;; Increment button (visible on touch only via CSS)
            (dom/button
              (dom/props {:class "card-count-btn"
                          :title "Increase card count"
                          :style {:width "28px" :height "28px" :border "1px solid var(--color-border)"
                                  :border-radius "4px" :background "var(--color-bg-subtle)"
                                  :font-size "16px" :cursor "pointer" :display "flex"
                                  :align-items "center" :justify-content "center"
                                  :padding "0"}})
              (dom/text "+")
              (e/for [[t e] (dom/On-all "click")]
                (when e
                  (swap! !card-count (fn [v] (min 50 (inc v))))
                  (e/server (settings/save-card-count user-id (min 50 (inc @!card-count)))))
                (t))))

        ;; Generate buttons group
          (dom/div
            (dom/props {:style {:display "flex" :gap "8px" :margin-left "auto"}})

          ;; Generate button
            (let [gen-pending (+ (count (:queue gen-state)) (if (:active gen-state) 1 0))
                  gen-active? (pos? gen-pending)
                  no-content? (empty? content-text)]
              (dom/button
                (dom/props {:class "btn btn-sm btn-primary"
                            :style {:background (cond no-content? "var(--color-disabled-bg)" gen-active? "var(--color-primary-light)" :else "var(--color-primary)")
                                    :cursor (if no-content? "not-allowed" "pointer")
                                    :font-weight "bold"}
                            :disabled no-content?
                            :title (if no-content? "Extract text first to generate flashcards"
                                     (str "Generate flashcards from editor text or selected text (" mod-key "+Shift+G)"))})
                (when (and gen-active? (nil? (:error gen-state)))
                  (dom/span (dom/props {:class "spinner"})))
                (dom/text (cond (:error gen-state) (:error gen-state)
                            gen-active? "Generating..."
                            :else "Generate"))
                (when (and gen-active? (nil? (:error gen-state)))
                  (dom/text (str " (" gen-pending ")")))
                (reset! keyboard/!generate-btn-ref dom/node)
                (e/on-unmount (fn [] (reset! keyboard/!generate-btn-ref nil)))
                (dom/On "click"
                  (fn [_]
                    (let [sel (editor/get-selection!)]
                      (when sel
                        (editor/highlight-range! (:index sel) (:length sel) :color "var(--color-highlight-gold)"))
                      (swap! !gen-state (fn [s]
                                          (-> s
                                            (update :queue conj {:id (str (random-uuid))
                                                                 :selection (when sel (:text sel))
                                                                 :topic-id topic-id
                                                                 :root-topic-id root-topic-id
                                                                 :card-type card-type
                                                                 :content-text content-text
                                                                 :page-number page-number
                                                                 :source-ref source-ref})
                                            (assoc :error nil))))
                      (log/log-info (str "Card generation enqueued topic-id=" topic-id " card-type=" card-type " card-count=" card-count-val))))
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
                             (create-extract-topic-safe! topic-id user-id pending title))]
                (if (:success result)
                  (do (reset! !extract-state {:pending nil :error nil})
                    (token))
                  (do (reset! !extract-state {:pending nil :error (or (:error result) "Failed to save extract")})
                    (token (or (:error result) "Failed to save extract"))))))))

        (when llm-enabled?
        ;; Auto-advance: when not processing and queue has items, start next
          (when (and (nil? (:active gen-state)) (seq (:queue gen-state)))
            (swap! !gen-state (fn [{:keys [queue]}]
                                {:active (first queue) :queue (vec (rest queue))})))

        ;; Queue processor — processes one generation request at a time
        ;; Context computation differs by mode:
        ;; - :extract mode with selection: content-text as context; without: parent-content
        ;; - :page mode with selection: prev-pages + current page; without: prev-pages only
          (when-some [current (:active gen-state)]
            (let [[?token _?error] (e/Token (:id current))]
              (when-some [token ?token]
                (log/log-trace "Generation token opened")
                ;; Read captured values from queue item (not reactive bindings)
                (let [cur-topic-id (or (:topic-id current) topic-id)
                      cur-root-topic-id (or (:root-topic-id current) root-topic-id)
                      cur-card-type (or (:card-type current) card-type)
                      cur-content-text (or (:content-text current) content-text)
                      cur-page-number (or (:page-number current) page-number)
                      cur-source-ref (:source-ref current)
                      content (or (:selection current) cur-content-text)
                      context-text
                      (when use-context
                        (case context-mode
                          :extract
                          (if (:selection current)
                            cur-content-text
                            parent-content)

                          :page
                          (let [prev-context (e/server (get-context-pages* cur-root-topic-id cur-page-number context-window))]
                            (if (:selection current)
                              (if prev-context
                                (str prev-context "\n\n---\n\n" cur-content-text)
                                cur-content-text)
                              prev-context))))

                      generate-result (e/server
                                        (e/Offload
                                          #(if (= cur-card-type "basic")
                                             (cards/generate-basic-cards
                                               {:content content :context context-text
                                                :card-count card-count-val :user-id user-id :enc-key enc-key})
                                             (cards/generate-cloze-cards
                                               {:content content :context context-text
                                                :card-count card-count-val :user-id user-id :enc-key enc-key}))))]
                  (if-not (:success generate-result)
                    (do (token (:error generate-result))
                      (swap! !gen-state assoc :active nil :error (:error generate-result)))
                    (let [generated-cards (e/server (:cards generate-result))
                          save-result (e/server (save-cards-for-topic cur-topic-id cur-root-topic-id cur-card-type generated-cards cur-source-ref))]
                      (log/log-info (str "Card generation complete card-count=" (count generated-cards)))
                      (if (:success save-result)
                        (do (token)
                          (log/log-debug (str "Cards saved topic-id=" cur-topic-id " root=" cur-root-topic-id))
                          (swap! !gen-state assoc :active nil)
                          (e/server (swap! !refresh inc)))
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
                ;; Read captured values from queue item with fallback to reactive bindings
                (let [cur-topic-id (or (:topic-id current) topic-id)
                      cur-root-topic-id (or (:root-topic-id current) root-topic-id)
                      cur-content-text (or (:content-text current) content-text)
                      cur-page-number (or (:page-number current) page-number)
                      cur-source-ref (or (:source-ref current) source-ref)
                      content (or (:selection current) cur-content-text)
                      prompt-text (:pre-prompt current)
                      kind (:kind current)
                      context-text
                      (when use-context
                        (case context-mode
                          :extract
                          (if (:selection current)
                            cur-content-text
                            parent-content)

                          :page
                          (let [prev-context (e/server (get-context-pages* cur-root-topic-id cur-page-number context-window))]
                            (if (:selection current)
                              (if prev-context
                                (str prev-context "\n\n---\n\n" cur-content-text)
                                cur-content-text)
                              prev-context))))

                      generate-result (e/server
                                        (e/Offload
                                          #(if (= kind "basic")
                                             (cards/generate-basic-cards
                                               {:content content :context context-text
                                                :card-count card-count-val :user-id user-id
                                                :enc-key enc-key :pre-prompt prompt-text})
                                             (cards/generate-cloze-cards
                                               {:content content :context context-text
                                                :card-count card-count-val :user-id user-id
                                                :enc-key enc-key :pre-prompt prompt-text}))))]
                  (if-not (:success generate-result)
                    (do (token (:error generate-result))
                      (swap! !prompt-gen-state assoc :active nil :error (:error generate-result)))
                    (let [generated-cards (e/server (:cards generate-result))
                          save-result (e/server (save-cards-for-topic cur-topic-id cur-root-topic-id kind generated-cards cur-source-ref))]
                      (if (:success save-result)
                        (do (token)
                          (swap! !prompt-gen-state assoc :active nil)
                          (e/server (swap! !refresh inc)))
                        (do (token (:error save-result))
                          (swap! !prompt-gen-state assoc :active nil)))))))))) ;; end when llm-enabled? (queue processors)
        
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
        (AnkiSyncButton user-id root-topic-id page-number card-type !refresh unsynced-count))

      ;; Pre-prompt modal dialog (LLM only)
      (when (and llm-enabled? show-prompt-dialog)
        (PromptDialog {:!show !show-prompt-dialog
                       :!prompt-gen-state !prompt-gen-state
                       :!pre-prompt !pre-prompt
                       :!prompt-history !prompt-history
                       :!history-save-trigger !history-save-trigger
                       :captured-selection captured-selection
                       :prompt-dialog-kind prompt-dialog-kind})))))
