(ns electric-starter-app.ocr-page
  "OCR text extraction UI component."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.electric-scroll0 :refer [Scroll-window]]
   [contrib.data :refer [clamp-left]]
   [electric-starter-app.pdf-viewer-component :refer [PdfViewerComponent]]
   [electric-starter-app.rich-text-editor-component :refer [RichTextEditorComponent]]
   [electric-starter-app.rich-text-editor :as editor]
   [electric-starter-app.components :refer [Typeahead]]
   [electric-starter-app.anki-sync :refer [AnkiSyncButton]]
   [electric-starter-app.ocr-modals :refer [ExportModal PromptDialog EditCardModal]]
   #?(:clj [electric-starter-app.page :as page])
   #?(:clj [electric-starter-app.pdf :as pdf])
   #?(:clj [electric-starter-app.cards :as cards])
   #?(:clj [electric-starter-app.settings :as settings])
   #?(:clj [electric-starter-app.db :as db])))

#?(:clj (defonce !refresh (atom 0)))  ; Server-side refresh trigger
#?(:clj (defonce !extracting-pages (atom #{})))  ; #{[doc-id page-num]} currently extracting

;; Query wrapper: takes refresh arg to create Electric reactive dependency
#?(:clj (defn get-page-text* [_refresh document-id page-number]
          (page/get-page-text document-id page-number)))

;; Query wrapper for page done status
#?(:clj (defn get-page-done-status* [_refresh document-id page-number]
          (db/get-page-done-status document-id page-number)))

;; Query wrapper for cards
#?(:clj (defn get-cards* [_refresh document-id page-number]
          (cards/get-cards document-id page-number)))

;; Page completion stats for a document
#?(:clj (defn get-page-stats* [document-id]
          (let [pages (db/list-pages document-id)]
            {:done  (count (filter :pages/is_done pages))
             :total (count pages)})))

;; Drag helper for split-pane dividers
(defn start-drag!
  "Begin a split-pane drag. Call from a mousedown handler.
   axis: :x for horizontal, :y for vertical."
  [e axis !pct]
  #?(:clj nil
     :cljs
     (do
       (.preventDefault e)
       (let [horizontal? (= axis :x)
             start-pos   (if horizontal? (.-clientX e) (.-clientY e))
             start-pct   @!pct
             parent      (-> e .-target .-parentElement)
             parent-size (if horizontal? (.-offsetWidth parent) (.-offsetHeight parent))
             on-move     (fn [me]
                           (let [delta     (- (if horizontal? (.-clientX me) (.-clientY me)) start-pos)
                                 delta-pct (* (/ delta parent-size) 100)
                                 new-pct   (-> (+ start-pct delta-pct) (max 15) (min 85))]
                             (reset! !pct new-pct)))
             on-up       (atom nil)]
         (reset! on-up
           (fn [_]
             (.removeEventListener js/document "mousemove" on-move)
             (.removeEventListener js/document "mouseup" @on-up)))
         (.addEventListener js/document "mousemove" on-move)
         (.addEventListener js/document "mouseup" @on-up)))))

;; Delete button extracted into its own component to reduce macro-expansion depth in CardRow
(e/defn DeleteCardButton [id]
  (e/client
    (dom/button
      (dom/props {:style {:padding "2px 6px" :background "#dc3545" :color "white"
                          :border "none" :border-radius "3px" :cursor "pointer"
                          :font-size "12px" :line-height "1"}})
      (dom/text "\u00D7")
      (let [click-event (dom/On "click" identity nil)
            [?token ?error] (e/Token click-event)]
        (dom/props {:disabled (some? ?token)
                    :style {:padding "2px 6px"
                            :background (if (some? ?token) "#999" "#dc3545")
                            :color "white" :border "none" :border-radius "3px"
                            :cursor (if (some? ?token) "not-allowed" "pointer")
                            :font-size "12px" :line-height "1"}})
        (when ?error
          (dom/div
            (dom/props {:style {:color "red" :font-size "11px"}})
            (dom/text ?error)))
        (when-some [token ?token]
          (let [result (e/server (cards/delete-card id))]
            (if (:success result)
              (do (e/server (swap! !refresh inc)) (token))
              (token (:error result)))))))))

;; Card table row component. Accepts !editing-card atom directly to avoid
;; passing a plain fn as an e/defn parameter (which can confuse the macro expander).
(e/defn CardRow [card !editing-card]
  (e/client
    (let [id (e/server (:flashcards/id card))
          kind (e/server (:flashcards/kind card))
          question (e/server (:flashcards/question card))
          answer (e/server (:flashcards/answer card))
          cloze (e/server (:flashcards/cloze card))]
      (dom/tr
        ;; Front column
        (dom/td
          (dom/props {:style {:padding "6px 8px" :overflow "hidden" :text-overflow "ellipsis" :white-space "nowrap" :max-width "0"
                              :border-bottom "1px solid #e0e0e0"}})
          (dom/text (if (= kind "basic") question cloze)))
        ;; Back column
        (dom/td
          (dom/props {:style {:padding "6px 8px" :overflow "hidden" :text-overflow "ellipsis" :white-space "nowrap" :max-width "0"
                              :border-bottom "1px solid #e0e0e0"}})
          (dom/text (if (= kind "basic") (or answer "") "")))
        ;; Kind column
        (dom/td
          (dom/props {:style {:padding "6px 8px" :width "60px" :color "#666" :font-size "12px"
                              :border-bottom "1px solid #e0e0e0"}})
          (dom/text kind))
        ;; Edit column
        (dom/td
          (dom/props {:style {:padding "6px 4px" :width "40px" :text-align "center"
                              :border-bottom "1px solid #e0e0e0"}})
          (dom/button
            (dom/props {:style {:padding "2px 6px" :background "#007bff" :color "white"
                                :border "none" :border-radius "3px" :cursor "pointer"
                                :font-size "12px"}})
            (dom/text "\u270E")
            (dom/On "click" (fn [_]
                              (let [data {:id id :kind kind :question question :answer answer :cloze cloze}]
                                (println "EDIT CLICK data:" (pr-str data))
                                (reset! !editing-card data))) nil)))
        ;; Delete column
        (dom/td
          (dom/props {:style {:padding "6px 4px" :width "40px" :text-align "center"
                              :border-bottom "1px solid #e0e0e0"}})
          (DeleteCardButton id))))))

(e/defn OcrPage [user-id enc-key]
  (e/client
    (dom/div
      (dom/props {:style {:height "100%" :display "flex" :flex-direction "column" :overflow "hidden"}})
      (let [docs-result    (e/server (pdf/list-pdfs user-id))
            last-doc-id    (e/server (settings/get-last-document user-id))
            refresh        (e/server (e/watch !refresh))
            documents      (:documents docs-result)
            valid-last-doc (when (some #(= (:documents/id %) last-doc-id) documents)
                             last-doc-id)
            doc-by-name    (into {} (map (fn [d] [(:documents/filename d) (:documents/id d)]) documents))
            filenames      (mapv :documents/filename documents)
            last-doc-name  (some #(when (= (:documents/id %) valid-last-doc) (:documents/filename %)) documents)
            !selected-name (atom last-doc-name)
            selected-name  (e/watch !selected-name)
            selected-doc   (get doc-by-name selected-name)
            !doc-commit    (atom nil)
            doc-commit     (e/watch !doc-commit)
            [?commit-token _] (e/Token doc-commit)
            page-stats     (e/server
                             (when selected-doc
                               (let [_ refresh]
                                 (get-page-stats* selected-doc))))]

        ;; Save last document on definitive selection
        (when-some [token ?commit-token]
          (when-some [doc-id (get doc-by-name doc-commit)]
            (e/server (settings/save-last-document user-id doc-id)))
          (reset! !doc-commit nil)
          (token))

        (cond
          ;; Success with documents
          (and (:success docs-result) (seq documents))
          (dom/div
            (dom/props {:style {:flex-shrink "0" :padding "8px 0"
                                :display "flex" :align-items "center" :gap "12px"}})
            (dom/div
              (dom/props {:style {:flex "1" :position "relative"}})
              (Typeahead !selected-name filenames "Search documents..." !doc-commit))
            (when (and selected-doc page-stats)
              (dom/span
                (dom/props {:style {:font-size "13px" :color "#666" :white-space "nowrap"}})
                (dom/text (:done page-stats) " / " (:total page-stats) " pages done"))))

          ;; Success but no documents
          (:success docs-result)
          (dom/div
            (dom/props {:style {:padding "8px 0"}})
            (dom/p (dom/text "No documents yet. Upload a PDF on the PDF tab to get started.")))

          ;; Error case
          :else
          (dom/p
            (dom/props {:style {:color "red"}})
            (dom/text "Error loading documents: " (or (:error docs-result) "Unknown error"))))

        ;; Main content (shows when document is selected)
        (when selected-doc
          (let [initial-page    (e/server (settings/get-last-page user-id selected-doc))
                !page-to-save   (atom nil)
                page-to-save    (e/watch !page-to-save)
                [?page-token _] (e/Token page-to-save)
                !top-pct (atom 60)
                top-pct (e/watch !top-pct)
                !left-pct (atom 50)
                left-pct (e/watch !left-pct)
                ;; Bridge current-pdf-page via atom so it's available at outer scope
                !current-page (atom 1)
                current-pdf-page (e/watch !current-page)
                ;; Pre-prompt state (session-persistent)
                !pre-prompt (atom "")
                !show-prompt-dialog (atom false)
                show-prompt-dialog (e/watch !show-prompt-dialog)
                !prompt-dialog-kind (atom nil)
                prompt-dialog-kind (e/watch !prompt-dialog-kind)
                !captured-selection (atom nil)
                captured-selection (e/watch !captured-selection)
                ;; Generation queue — {:queue [...] :active nil-or-request}
                !gen-state (atom {:queue [] :active nil})
                gen-state (e/watch !gen-state)
                ;; Prompt generation queue (same shape)
                !prompt-gen-state (atom {:queue [] :active nil})
                prompt-gen-state (e/watch !prompt-gen-state)
                ;; Extraction tracking — server-side for true parallelism
                extracting-pages (e/server (e/watch !extracting-pages))
                ;; Shared server data — hoisted so both editor and bottom panel can use them
                dirty-data (e/watch editor/!dirty-html)
                save-result (when (some? dirty-data)
                              (e/server
                                (e/Offload
                                  #(page/save-page-html-impl
                                     (:doc-id dirty-data)
                                     (:page dirty-data)
                                     (:html dirty-data)))))
                text-result (e/server (get-page-text* refresh selected-doc current-pdf-page))
                page-text (e/server (when (:success text-result) (:text text-result)))
                is-done (e/server (get-page-done-status* refresh selected-doc current-pdf-page))]

            ;; Save last page when user navigates
            (when-some [token ?page-token]
              (e/server (settings/save-last-page user-id selected-doc page-to-save))
              (token))

            ;; Outer: vertical flex (top row / bottom panel)
            (dom/div
              (dom/props {:style {:flex "1" :display "flex" :flex-direction "column" :min-height "0" :overflow "hidden"}})

              ;; TOP ROW: PDF | Editor (horizontal flex)
              (dom/div
                (dom/props {:style {:height (str top-pct "%") :display "flex" :min-height "0" :overflow "hidden"}})

                ;; LEFT: PDF viewer
                (dom/div
                  (dom/props {:style {:width (str left-pct "%") :min-width "0" :overflow "hidden"}})
                  (reset! !current-page
                    (PdfViewerComponent {:document-id selected-doc
                                         :initial-page initial-page
                                         :on-navigate! (fn [p] (reset! !page-to-save p))})))

                ;; Horizontal drag handle
                (dom/div
                  (dom/props {:class "split-divider-h"
                              :title "Drag to resize panels"})
                  (dom/On "mousedown" (fn [e] (start-drag! e :x !left-pct)) nil))

                ;; RIGHT: Editor
                (dom/div
                  (dom/props {:style {:flex "1" :display "flex" :flex-direction "column" :min-width "0" :min-height "0" :overflow "hidden"}})

                  ;; Page header + extract button
                  (dom/div
                    (dom/props {:style {:display "flex" :align-items "center" :gap "12px" :padding "8px" :flex-shrink "0"}})
                    (dom/span
                      (dom/props {:style {:font-weight "bold" :font-size "16px"}})
                      (dom/text "Page " current-pdf-page))

                    ;; Mark as done checkbox
                    (dom/label
                      (dom/props {:style {:display "flex" :align-items "center" :gap "4px" :font-size "14px" :cursor "pointer"}
                                  :title "Mark this page as completed to track your extraction progress"})
                      (e/for-by identity [_page [current-pdf-page]]
                        (dom/input
                          (dom/props {:type "checkbox"})
                          (set! (.-checked dom/node) is-done)
                          (let [change-event (dom/On "change"
                                               (fn [e] {:checked (-> e .-target .-checked)
                                                        :page current-pdf-page})
                                               nil)
                                [?token ?error] (e/Token change-event)]
                            (when-some [token ?token]
                              (e/server (db/toggle-page-done selected-doc (:page change-event)))
                              (e/server (swap! !refresh inc))
                              (token)))))
                      (dom/text "Done"))

                    (let [extracting? (contains? extracting-pages [selected-doc current-pdf-page])]
                      (dom/button
                        (dom/props {:style {:padding "8px 16px"
                                            :background (if extracting? "#ccc" "#007bff")
                                            :color "white"
                                            :border "none"
                                            :border-radius "4px"
                                            :cursor (if extracting? "not-allowed" "pointer")
                                            :font-size "14px"}
                                    :disabled extracting?})
                        (dom/text (if extracting? "Extracting..." "Extract Text"))
                        (let [click-event (dom/On "click"
                                            (fn [_] {:id (str (random-uuid)) :page current-pdf-page})
                                            nil)
                              [?token ?error] (e/Token click-event)]
                          (when-some [token ?token]
                            (e/server
                              (let [page (:page click-event)
                                    doc selected-doc
                                    uid user-id
                                    ek enc-key]
                                (when-not (contains? @!extracting-pages [doc page])
                                  (swap! !extracting-pages conj [doc page])
                                  (do
                                    (future
                                      (try
                                        (let [result (page/extract-page-text uid doc page ek)]
                                          (when (:success result)
                                            (swap! !refresh inc)))
                                        (finally
                                          (swap! !extracting-pages disj [doc page]))))
                                    :started))))
                            (token)))))

                    ;; Save status indicator with fade-out
                    (when (some? dirty-data)
                      (let [is-success (:success save-result)
                            message (if is-success "Saved" (str "Save error: " (:error save-result)))
                            !show (atom true)
                            show (e/watch !show)]
                        (dom/span
                          (dom/props {:style {:margin-left "12px"
                                              :font-size "12px"
                                              :color (if is-success "#888" "red")
                                              :opacity (if show "1" "0")
                                              :transition "opacity 0.5s ease-out"}})
                          (dom/text message)
                          ;; Fade out after 2 seconds (only for successful saves)
                          (when is-success
                            (js/setTimeout
                              (fn [] (reset! !show false))
                              2000))))))

                  ;; Editor area
                  (dom/div
                    (dom/props {:style {:flex "1" :min-height "0" :overflow "hidden"}})
                    (if (:success text-result)
                      (dom/div
                        (dom/props {:style {:height "100%" :display "flex" :flex-direction "column" :overflow "hidden"}})
                        (RichTextEditorComponent {:initial-html (:text text-result)
                                                  :page-number current-pdf-page
                                                  :doc-id selected-doc}))
                      (dom/p
                        (dom/props {:style {:color "gray"}})
                        (dom/text "No text extracted yet. Click 'Extract Text' to process this page."))))))

              ;; Vertical drag handle (full width)
              (dom/div
                (dom/props {:class "split-divider-v"
                            :title "Drag to resize panels"})
                (dom/On "mousedown" (fn [e] (start-drag! e :y !top-pct)) nil))

              ;; BOTTOM PANEL: Options toolbar + Cards table (full width)
              (dom/div
                (dom/props {:style {:flex "1" :display "flex" :flex-direction "column" :min-height "0" :overflow "hidden"}})

                ;; Generation toolbar (compact single row)
                (when page-text
                  (let [;; Load settings from server
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
                        ;; Trigger for persisting history to server (set to new history vec on generate)
                        !history-save-trigger (atom nil)
                        history-save-trigger (e/watch !history-save-trigger)]

                    ;; Persist prompt history to server when Generate is clicked
                    (when (some? history-save-trigger)
                      (e/server (settings/save-pre-prompt-history user-id history-save-trigger)))

                    (dom/div
                      (dom/props {:style {:display "flex" :align-items "center" :gap "12px"
                                          :padding "8px 12px" :flex-shrink "0"
                                          :border-bottom "1px solid #e0e0e0" :background "#fafafa"}})

                      ;; Context checkbox + pages
                      (dom/label
                        (dom/props {:style {:display "flex" :align-items "center" :gap "4px" :font-size "13px"}
                                    :title "Include text from previous pages for better context"})
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
                        (dom/props {:style {:display "flex" :align-items "center" :gap "4px" :font-size "13px"}})
                        (dom/input
                          (dom/props {:type "radio" :name "card-type" :value "basic"
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
                        (dom/props {:style {:display "flex" :align-items "center" :gap "4px" :font-size "13px"}})
                        (dom/input
                          (dom/props {:type "radio" :name "card-type" :value "cloze"
                                      :checked (= card-type "cloze")})
                          (let [change-event (dom/On "change" (fn [_] "cloze") nil)
                                [?token ?error] (e/Token change-event)]
                            (when (some? change-event)
                              (reset! !card-type change-event))
                            (when-some [token ?token]
                              (e/server (settings/save-card-type user-id "cloze"))
                              (token))))
                        (dom/text "Cloze"))
                      ;; Help icon for card types
                      (dom/button
                        (dom/props {:class "help-icon"
                                    :style {:padding "2px 6px" :margin-left "4px" :background "transparent"
                                            :border "1px solid #ccc" :border-radius "50%" :cursor "help"
                                            :font-size "11px" :color "#666"}
                                    :title "Basic: Traditional question-answer flashcards. Cloze: Fill-in-the-blank deletion cards."})
                        (dom/text "?")
                        (dom/On "click"
                          (fn [e]
                            #?(:cljs (.preventDefault e))
                            #?(:cljs (js/alert "Basic: Traditional question-answer flashcards.\n\nCloze: Fill-in-the-blank deletion cards (e.g., 'Paris is the capital of [...]').")))
                          nil))

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

                        ;; Generate button — always enabled, enqueues requests
                        (dom/button
                          (dom/props {:style {:padding "4px 12px" :background "#28a745" :color "white"
                                              :border "none" :border-radius "4px" :cursor "pointer"
                                              :font-size "13px" :font-weight "bold"}
                                      :title "Generate flashcards from editor text or selected text"})
                          (dom/text "Generate")
                          (let [pending (+ (count (:queue gen-state)) (if (:active gen-state) 1 0))]
                            (when (pos? pending)
                              (dom/text (str " (" pending ")"))))
                          (dom/On "click"
                            (fn [_]
                              (swap! !gen-state update :queue conj
                                {:id    (str (random-uuid))
                                 :selection (editor/get-selected-text!)}))
                            nil))

                        ;; Generate with Prompt button
                        (dom/button
                          (dom/props {:style {:padding "4px 12px"
                                              :background "#17a2b8"
                                              :color "white"
                                              :border "none"
                                              :border-radius "4px"
                                              :cursor "pointer"
                                              :font-size "13px"
                                              :font-weight "500"}
                                      :title "Add custom instructions to guide card generation"})
                          (dom/text "Generate with Prompt...")
                          (let [pending (+ (count (:queue prompt-gen-state)) (if (:active prompt-gen-state) 1 0))]
                            (when (pos? pending)
                              (dom/text (str " (" pending ")"))))
                          (dom/On "click" (fn [_]
                                            (reset! !captured-selection (editor/get-selected-text!))
                                            (reset! !prompt-dialog-kind card-type)
                                            (reset! !show-prompt-dialog true))
                                  nil)))  ;; end Generate buttons group

                      ;; Auto-advance: when not processing and queue has items, start next
                      (when (and (nil? (:active gen-state)) (seq (:queue gen-state)))
                        (swap! !gen-state (fn [{:keys [queue]}]
                          {:active (first queue) :queue (vec (rest queue))})))

                      ;; Queue processor — processes one generation request at a time
                      (when-some [current (:active gen-state)]
                        (let [[?token _?error] (e/Token (:id current))]
                          (when-some [token ?token]
                            (let [content (or (:selection current) page-text)
                                  context-text (when use-context
                                                 (e/server (cards/get-context-pages selected-doc current-pdf-page context-window)))
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
                                    (swap! !gen-state assoc :active nil))
                                (let [generated-cards (e/server (:cards generate-result))
                                      save-result (e/server (cards/save-cards selected-doc current-pdf-page card-type generated-cards))]
                                  (if (:success save-result)
                                    (do (e/server (swap! !refresh inc))
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
                            (let [content (or (:selection current) page-text)
                                  prompt-text (:pre-prompt current)
                                  kind (:kind current)
                                  context-text (when use-context
                                                 (e/server (cards/get-context-pages selected-doc current-pdf-page context-window)))
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
                                    (swap! !prompt-gen-state assoc :active nil))
                                (let [generated-cards (e/server (:cards generate-result))
                                      save-result (e/server (cards/save-cards selected-doc current-pdf-page kind generated-cards))]
                                  (if (:success save-result)
                                    (do (e/server (swap! !refresh inc))
                                        (token)
                                        (swap! !prompt-gen-state assoc :active nil))
                                    (do (token (:error save-result))
                                        (swap! !prompt-gen-state assoc :active nil)))))))))

                      ;; Separator
                      (dom/span (dom/props {:style {:color "#ccc"}}) (dom/text "|"))

                      ;; Export button + modal
                      (let [!show-export (atom false)
                            show-export (e/watch !show-export)]
                        (dom/button
                          (dom/props {:style {:padding "4px 12px" :background "#6c757d" :color "white" :border "none"
                                              :border-radius "4px" :cursor "pointer" :font-size "13px" :font-weight "500"}})
                          (dom/text "Export...")
                          (dom/On "click" (fn [_] (reset! !show-export true)) nil))
                        (when show-export
                          (ExportModal !show-export selected-doc current-pdf-page user-id)))

                      ;; Separator
                      (dom/span (dom/props {:style {:color "#ccc"}}) (dom/text "|"))

                      ;; Anki Sync button
                      (AnkiSyncButton user-id selected-doc current-pdf-page card-type !refresh))

                    ;; Pre-prompt modal dialog
                    (when show-prompt-dialog
                      (PromptDialog {:!show              !show-prompt-dialog
                                     :!prompt-gen-state  !prompt-gen-state
                                     :!pre-prompt        !pre-prompt
                                     :!prompt-history    !prompt-history
                                     :!history-save-trigger !history-save-trigger
                                     :captured-selection captured-selection
                                     :prompt-dialog-kind prompt-dialog-kind}))))

                ;; Cards table with virtual scroll
                (let [cards-result (e/server (get-cards* refresh selected-doc current-pdf-page))
                      !editing-card (atom nil)
                      editing-card (e/watch !editing-card)]

                  (when editing-card
                    (EditCardModal !editing-card !refresh))

                  (if (:success cards-result)
                    (let [cards-vec (e/server (vec (:cards cards-result)))
                          card-count (e/server (count cards-vec))
                          row-height 36]
                      (dom/div
                        (dom/props {:style {:flex "1" :overflow-y "auto" :min-height "0"}})
                        (if (pos? card-count)
                          (let [[offset limit] (Scroll-window row-height card-count dom/node {:overquery-factor 1})
                                occluded-height (clamp-left (* row-height (- card-count limit)) 0)]
                            (dom/table
                              (dom/props {:style {:width "100%"
                                                  :border-collapse "separate"
                                                  :border-spacing "0"
                                                  :table-layout "fixed"
                                                  :font-size "13px"}})
                              (dom/tbody
                                (dom/props {:style {:position "relative"
                                                    :top (str (* offset row-height) "px")}})
                                (e/for [i (e/diff-by {} (range offset (+ offset limit)))]
                                  (let [card (e/server (nth cards-vec i nil))]
                                    (when card
                                      (CardRow card !editing-card))))))
                            (dom/div (dom/props {:style {:height (str occluded-height "px")}})))
                          (dom/p
                            (dom/props {:style {:color "gray" :font-size "13px" :padding "8px 12px"}})
                            (dom/text "No cards generated yet.")))))
                    (dom/div
                      (dom/props {:style {:color "red" :font-size "13px" :padding "8px 12px"}})
                      (dom/text "Error loading cards: " (:error cards-result)))))))))))))
