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
   [electric-starter-app.ocr-modals :refer [ExportModal PromptDialog EditCardModal AddCardModal]]
   [electric-starter-app.card-components :refer [CardRow get-cards*]]
   [electric-starter-app.content-toolbar :refer [ContentToolbar]]
   [electric-starter-app.content-card-table :refer [ContentCardTable]]
   #?(:clj [electric-starter-app.page :as page])
   #?(:clj [electric-starter-app.pdf :as pdf])
   #?(:clj [electric-starter-app.cards :as cards])
   #?(:clj [electric-starter-app.settings :as settings])
   #?(:clj [electric-starter-app.db :as db])))

#?(:clj (defonce !refresh (atom 0))) ; Server-side refresh trigger
#?(:clj (defonce !extracting-pages (atom #{}))) ; #{[doc-id page-num]} currently extracting
#?(:cljs (defonce !extracting-client? (atom false))) ; Client-side flag: set true immediately on click to prevent rapid re-clicks before server state round-trips

;; Query wrapper: takes refresh arg to create Electric reactive dependency
#?(:clj (defn get-page-text* [_refresh document-id page-number]
          (page/get-page-text document-id page-number)))

;; Query wrapper for page done status
#?(:clj (defn get-page-done-status* [_refresh document-id page-number]
          (db/get-page-done-status document-id page-number)))

;; Page completion stats for a document
#?(:clj (defn get-page-stats* [_refresh document-id]
          (let [pages (db/list-pages document-id)]
            {:done (count (filter :pages/is_done pages))
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
             start-pos (if horizontal? (.-clientX e) (.-clientY e))
             start-pct @!pct
             parent (-> e .-target .-parentElement)
             parent-size (if horizontal? (.-offsetWidth parent) (.-offsetHeight parent))
             on-move (fn [me]
                       (let [delta (- (if horizontal? (.-clientX me) (.-clientY me)) start-pos)
                             delta-pct (* (/ delta parent-size) 100)
                             new-pct (-> (+ start-pct delta-pct) (max 15) (min 85))]
                         (reset! !pct new-pct)))
             on-up (atom nil)]
         (reset! on-up
           (fn [_]
             (.removeEventListener js/document "mousemove" on-move)
             (.removeEventListener js/document "mouseup" @on-up)))
         (.addEventListener js/document "mousemove" on-move)
         (.addEventListener js/document "mouseup" @on-up)))))


(e/defn OcrPage [user-id enc-key !nav-target]
  (e/client
    (dom/div
      (dom/props {:style {:height "100%" :display "flex" :flex-direction "column" :overflow "hidden"}})
      (let [docs-result (e/server (pdf/list-pdfs user-id))
            last-doc-id (e/server (settings/get-last-document user-id))
            refresh (e/server (e/watch !refresh))
            documents (:documents docs-result)
            valid-last-doc (when (some #(= (:documents/id %) last-doc-id) documents)
                             last-doc-id)
            doc-by-name (into {} (map (fn [d] [(:documents/filename d) (:documents/id d)]) documents))
            id-to-name (into {} (map (fn [d] [(:documents/id d) (:documents/filename d)]) documents))
            source-by-id (into {} (map (fn [d] [(:documents/id d) (or (:documents/source_type d) "pdf")]) documents))
            filenames (mapv :documents/filename documents)
            last-doc-name (some #(when (= (:documents/id %) valid-last-doc) (:documents/filename %)) documents)
            !selected-name (atom last-doc-name)
            selected-name (e/watch !selected-name)
            selected-doc (get doc-by-name selected-name)
            is-pdf (= (get source-by-id selected-doc) "pdf")
            ocr-source-ref (e/server (when selected-doc (db/get-document-source selected-doc)))
            !doc-commit (atom nil)
            doc-commit (e/watch !doc-commit)
            [?commit-token _] (e/Token doc-commit)
            ;; Nav target override (from Queue tab)
            nav-target (e/watch !nav-target)
            ;; Holds page override for when we jump from Queue; set once, used as initial-page
            !nav-page (atom nil)
            !nav-specified (atom false)
            page-stats (e/server
                         (when selected-doc
                           (get-page-stats* refresh selected-doc)))]

        ;; Handle navigation from Queue tab — only consume after doc-name resolves
        (when-some [{:keys [doc-id page]} nav-target]
          (when-some [doc-name (get id-to-name doc-id)]
            (reset! !nav-specified true)
            (reset! !selected-name doc-name)
            (reset! !doc-commit doc-name)
            (reset! !nav-page page)
            (reset! !nav-target nil)))

        ;; Save last document on definitive selection
        (when-some [token ?commit-token]
          (when-some [doc-id (get doc-by-name doc-commit)]
            (e/server (settings/save-last-document user-id doc-id)))
          (reset! !doc-commit nil)
          (token))

        (cond
          ;; Success with documents
          (and (:success docs-result) (seq documents))
          (when-not (e/watch !nav-specified)
            (dom/div
              (dom/props {:style {:flex-shrink "0" :padding "8px 0"
                                  :display "flex" :align-items "center" :gap "12px"}})
              (dom/div
                (dom/props {:style {:flex "1" :position "relative"}})
                (Typeahead !selected-name filenames "Search documents..." !doc-commit))
              (when (and selected-doc page-stats)
                (dom/span
                  (dom/props {:style {:font-size "13px" :color "#666" :white-space "nowrap" :margin-right "8px"}})
                  (dom/text (:done page-stats) " / " (:total page-stats) " pages done")))))

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
          (let [initial-page (or (e/watch !nav-page) (e/server (settings/get-last-page user-id selected-doc)))
                !page-to-save (atom nil)
                page-to-save (e/watch !page-to-save)
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
                ;; Generation queue — {:queue [...] :active nil-or-request :error nil}
                !gen-state (atom {:queue [] :active nil :error nil})
                gen-state (e/watch !gen-state)
                ;; Prompt generation queue (same shape)
                !prompt-gen-state (atom {:queue [] :active nil :error nil})
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

                ;; LEFT: PDF viewer + divider (hidden for web articles)
                (when is-pdf
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
                    (dom/On "mousedown" (fn [e] (start-drag! e :x !left-pct)) nil)))

                ;; RIGHT: Editor (full width for web articles)
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

                    ;; Priority input
                    (let [page-priority (e/server (db/get-page-priority selected-doc current-pdf-page))]
                      (dom/label
                        (dom/props {:style {:display "flex" :align-items "center" :gap "4px" :font-size "14px"}
                                    :title "Page priority (0=highest, 100=lowest). Used to sort the reading queue."})
                        (dom/text "P:")
                        (e/for-by identity [_k [current-pdf-page]]
                          (dom/input
                            (dom/props {:type "number" :min "0" :max "100" :style {:width "52px" :font-size "13px" :padding "2px 4px"}})
                            (set! (.-value dom/node) (str (or page-priority 50)))
                            (let [change-event (dom/On "change" #(-> % .-target .-value js/parseInt) nil)
                                  [?token _] (e/Token change-event)]
                              (when-some [token ?token]
                                (e/server (db/set-page-priority selected-doc current-pdf-page change-event))
                                (e/server (swap! !refresh inc))
                                (token)))))))

                    (let [extracting? (contains? extracting-pages [selected-doc current-pdf-page])
                          client-extracting? (e/client (e/watch !extracting-client?))
                          disabled? (or extracting? client-extracting?)]
                      (dom/button
                        (dom/props {:style {:padding "8px 16px"
                                            :background (if disabled? "#ccc" "#007bff")
                                            :color "white"
                                            :border "none"
                                            :border-radius "4px"
                                            :cursor (if disabled? "not-allowed" "pointer")
                                            :font-size "14px"}
                                    :disabled disabled?})
                        (dom/text (if disabled? "Extracting..." "Extract Text"))
                        (let [click-event (dom/On "click"
                                            (fn [_]
                                              (when-not @!extracting-client?
                                                (reset! !extracting-client? true)
                                                {:id (str (random-uuid)) :page current-pdf-page}))
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
                            (token))
                          ;; Reset client flag once server confirms extraction is running (or completes)
                          (e/client
                            (when (not extracting?)
                              (reset! !extracting-client? false))))))

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

                  ;; Source reference — editable inline
                  (let [current-source (e/server (db/get-document-source selected-doc))]
                    (dom/div
                      (dom/props {:style {:display "flex" :align-items "center" :gap "6px"
                                          :padding "4px 8px" :flex-shrink "0"
                                          :background "#f9f9f9" :border-bottom "1px solid #eee"}})
                      (dom/span
                        (dom/props {:style {:font-size "11px" :color "#999" :flex-shrink "0"}})
                        (dom/text "Source:"))
                      (e/for-by identity [_k [selected-doc]]
                        (dom/input
                          (dom/props {:type "text"
                                      :placeholder "Source reference (auto-populated)"
                                      :title "Source reference — propagated to generated cards"
                                      :style {:flex "1" :padding "2px 6px" :font-size "12px" :color "#555"
                                              :border "1px solid #e0e0e0" :border-radius "3px"
                                              :background "white"}})
                          (set! (.-value dom/node) (or current-source ""))
                          (let [event (dom/On "change" #(-> % .-target .-value) nil)
                                [?token _] (e/Token event)]
                            (when-some [token ?token]
                              (e/server (db/update-document-source selected-doc event))
                              (token)))))))

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

              ;; BOTTOM PANEL: Shared toolbar + card table
              (dom/div
                (dom/props {:style {:flex "1" :display "flex" :flex-direction "column" :min-height "0" :overflow "hidden"}})

                (ContentToolbar {:user-id user-id
                                 :enc-key enc-key
                                 :doc-id selected-doc
                                 :page-number current-pdf-page
                                 :content-text page-text
                                 :content-item-id nil
                                 :context-mode :page
                                 :context-tooltip "Include context for better cards. With a selection: current page + N previous pages. Without: N previous pages."}
                  !refresh)

                (ContentCardTable {:query-mode :page
                                   :doc-id selected-doc
                                   :page-number current-pdf-page}
                  !refresh)))))))))

;; Old inline toolbar + card table code was here — removed, now uses shared ContentToolbar + ContentCardTable
(comment "dead code removed")
