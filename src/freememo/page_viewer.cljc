(ns freememo.page-viewer
  "OCR text extraction UI component."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.logging :as log]
   [freememo.pdf-viewer-component :refer [PdfViewerComponent]]
   [freememo.rich-text-editor-component :refer [RichTextEditorComponent]]
   [freememo.rich-text-editor :as editor]
   [freememo.typeahead :refer [Typeahead]]
   [freememo.anki-sync :refer [AnkiSyncButton]]
   [freememo.card-modals :refer [ExportModal PromptDialog EditCardModal AddCardModal]]
   [freememo.content-toolbar :refer [ContentToolbar]]
   [freememo.content-card-table :refer [ContentCardTable]]
   #?(:clj [freememo.page-ocr :as page])
   #?(:clj [freememo.cards :as cards])
   #?(:clj [freememo.settings :as settings])
   [freememo.keyboard :as keyboard]
   [freememo.util :as util]
   #?(:clj [freememo.db :as db])))

#?(:clj (defonce !refresh (atom 0))) ; Server-side refresh trigger
#?(:clj (defonce !scanning-pages (atom #{}))) ; #{[root-topic-id page-num]} currently scanning via OCR
#?(:clj (defonce !ocr-errors (atom {}))) ; {[root-topic-id page-num] "error message"}

;; Query wrapper: takes refresh arg to create Electric reactive dependency
#?(:clj (defn get-page-text* [_refresh parent-id page-number]
          (page/get-page-text parent-id page-number)))

;; Query wrapper for page done status
#?(:clj (defn get-page-done-status* [_refresh parent-id page-number]
          (db/get-page-done-status parent-id page-number)))

;; Page completion stats for a document
#?(:clj (defn get-page-stats* [_refresh parent-id]
          (let [pages (db/list-pages parent-id)]
            {:done (count (filter #(= "done" (:topics/status %)) pages))
             :total (count pages)})))

;; Query wrapper for topic source reference
#?(:clj (defn get-topic-source* [_refresh topic-id]
          (db/get-topic-source topic-id)))

;; Resolve page topic ID from (parent-id, page-number) — needed for card operations
#?(:clj (defn get-page-topic-id* [_refresh parent-id page-number]
          (when (and parent-id page-number)
            (let [pages (db/list-pages parent-id)]
              (:topics/id (first (filter #(= (:topics/page_number %) page-number) pages)))))))



;; Responsive split pane default — plain defn avoids #? inside e/defn (frame mismatch)
(defn default-split-pct []
  #?(:cljs (if (< (.-innerHeight js/window) 900) 50 75)
     :clj 75))

(e/defn OcrPage [user-id enc-key !nav-target llm-enabled?]
  (e/client
    (dom/div
      (dom/props {:style {:height "100%" :display "flex" :flex-direction "column" :overflow "hidden"}})
      (let [root-topics (e/server (db/get-root-topics user-id))
            last-doc-id (e/server (settings/get-last-document user-id))
            refresh (e/server (e/watch !refresh))
            ;; Find valid last doc among root topics
            valid-last-doc (when (some #(= (:topics/id %) last-doc-id) root-topics)
                             last-doc-id)
            ;; Build lookup maps using topic fields
            doc-by-name (into {} (map (fn [t] [(:topics/title t) (:topics/id t)]) root-topics))
            id-to-name (into {} (map (fn [t] [(:topics/id t) (:topics/title t)]) root-topics))
            kind-by-id (into {} (map (fn [t] [(:topics/id t) (or (:topics/kind t) "basic")]) root-topics))
            filenames (mapv :topics/title root-topics)
            last-doc-name (some #(when (= (:topics/id %) valid-last-doc) (:topics/title %)) root-topics)
            !selected-name (atom last-doc-name)
            selected-name (e/watch !selected-name)
            selected-doc (get doc-by-name selected-name)
            is-pdf (= (get kind-by-id selected-doc) "pdf")
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
        (when-some [{:keys [topic-id page]} nav-target]
          (when-some [doc-name (get id-to-name topic-id)]
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
          (seq root-topics)
          (when-not (e/watch !nav-specified)
            (dom/div
              (dom/props {:style {:flex-shrink "0" :padding "8px 0"
                                  :display "flex" :align-items "center" :gap "12px"}})
              (dom/div
                (dom/props {:style {:flex "1" :position "relative"}})
                (Typeahead !selected-name filenames "Search documents..." !doc-commit))
              (when (and selected-doc page-stats)
                (dom/span
                  (dom/props {:style {:font-size "13px" :color "var(--color-text-secondary)" :white-space "nowrap" :margin-right "var(--sp-2)"}})
                  (dom/text (:done page-stats) " / " (:total page-stats) " pages done")))))

          ;; No documents
          :else
          (dom/div
            (dom/props {:style {:padding "8px 0"}})
            (dom/p (dom/text "No documents yet. Go to the Documents tab to upload a PDF, paste an article, or import a URL."))))

        ;; Main content (shows when document is selected)
        (when selected-doc
          (let [initial-page (or (e/watch !nav-page) (e/server (settings/get-last-page user-id selected-doc)))
                !page-to-save (atom nil)
                page-to-save (e/watch !page-to-save)
                [?page-token _] (e/Token page-to-save)
                !top-pct (atom (default-split-pct))
                top-pct (e/watch !top-pct)
                !left-pct (atom 50)
                left-pct (e/watch !left-pct)
                ;; Bridge current-pdf-page via atom so it's available at outer scope
                !current-page (atom 1)
                current-pdf-page (e/watch !current-page)
                ;; Resolve page topic ID for card operations
                page-topic-id (e/server (get-page-topic-id* refresh selected-doc current-pdf-page))
                _ (log/log-debug (str "page-topic-id=" page-topic-id " for page=" current-pdf-page))
                ;; User settings
                scan-dpi (e/server (settings/get-scan-dpi user-id))
                card-font-size (e/server (settings/get-card-font-size user-id))
                ;; Extraction tracking — server-side for true parallelism
                scanning-pages (e/server (e/watch !scanning-pages))
                ocr-errors (e/server (e/watch !ocr-errors))
                ;; Auto-save: one-way editor → DB. Don't clear !dirty-html, don't bump !refresh.
                dirty-data (e/watch editor/!dirty-html)
                !last-saved-ocr (atom nil)
                _ (when (and (some? dirty-data)
                          (= (:topic-id dirty-data) page-topic-id)
                          (not= (:html dirty-data) (e/watch !last-saved-ocr)))
                    (log/log-debug (str "Auto-save triggered topic-id=" (:topic-id dirty-data))))
                save-result (when (and (some? dirty-data)
                                    (= (:topic-id dirty-data) page-topic-id)
                                    (not= (:html dirty-data) (e/watch !last-saved-ocr)))
                              (e/server
                                (e/Offload
                                  #(try
                                     (db/update-topic-content! (:topic-id dirty-data) (:html dirty-data))
                                     {:success true}
                                     (catch Exception e
                                       {:success false :error (.getMessage e)})))))
                _ (when (:success save-result)
                    (reset! !last-saved-ocr (:html dirty-data)))
                text-result (e/server (get-page-text* refresh selected-doc current-pdf-page))
                _ (log/log-debug (str "text-result success=" (:success text-result) " page=" current-pdf-page " html-len=" (count (:text text-result))))
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
                    (e/for-by identity [_k [selected-doc]]
                      (reset! !current-page
                        (PdfViewerComponent {:document-id selected-doc
                                             :initial-page initial-page
                                             :on-navigate! (fn [p] (reset! !page-to-save p))}))))

                  ;; Horizontal drag handle
                  (dom/div
                    (dom/props {:class "split-divider-h"
                                :title "Drag to resize panels"})
                    (dom/On "pointerdown" (fn [e] (util/start-drag! e :x !left-pct)) nil)))

                ;; RIGHT: Editor (full width for web articles)
                (dom/div
                  (dom/props {:style {:flex "1" :display "flex" :flex-direction "column" :min-width "0" :min-height "0" :overflow "hidden"}})

                  ;; Compact page header
                  (dom/div
                    (dom/props {:style {:display "flex" :align-items "center" :gap "var(--sp-2)" :padding "var(--sp-1) var(--sp-2)" :flex-shrink "0"}})
                    (dom/span
                      (dom/props {:style {:font-weight "600" :font-size "13px" :color "#444"}})
                      (dom/text "p." current-pdf-page))

                    ;; Mark as done checkbox
                    (dom/label
                      (dom/props {:style {:display "flex" :align-items "center" :gap "3px" :font-size "12px" :cursor "pointer"}
                                  :title "Mark this page as completed to track your extraction progress"})
                      (e/for-by identity [_page [current-pdf-page]]
                        (dom/input
                          (dom/props {:type "checkbox"})
                          (set! (.-checked dom/node) is-done)
                          (reset! keyboard/!done-btn-ref dom/node)
                          (e/on-unmount (fn [] (reset! keyboard/!done-btn-ref nil)))
                          (let [change-event (dom/On "change"
                                               (fn [e] {:checked (-> e .-target .-checked)
                                                        :page current-pdf-page})
                                               nil)
                                [?token ?error] (e/Token change-event)]
                            (when-some [token ?token]
                              (e/server (db/toggle-page-done! selected-doc (:page change-event)))
                              (e/server (swap! !refresh inc))
                              (token)))))
                      (dom/text "Done"))

                    (when (and is-pdf llm-enabled?)
                      (let [scanning? (contains? scanning-pages [selected-doc current-pdf-page])
                            disabled? scanning?]
                        (dom/button
                          (dom/props {:class "btn btn-sm btn-primary"
                                      :style {:padding "4px 12px" :font-size "14px"
                                              :background (if disabled? "#ccc" "var(--color-primary)")
                                              :cursor (if disabled? "not-allowed" "pointer")}
                                      :disabled disabled?})
                          (dom/text (if disabled? "Scanning..." "Scan Page"))
                          (reset! keyboard/!scan-btn-ref dom/node)
                          (e/on-unmount (fn [] (reset! keyboard/!scan-btn-ref nil)))
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
                                  (if (contains? @!scanning-pages [doc page])
                                    (log/log-info (str "OCR scan already in progress topic-id=" doc " page=" page))
                                    (do
                                      (swap! !scanning-pages conj [doc page])
                                      (swap! !ocr-errors dissoc [doc page])
                                      (log/log-info (str "OCR scan started topic-id=" doc " page=" page " scanning-pages=" (pr-str @!scanning-pages)))
                                      (future
                                        (try
                                          (log/log-info (str "OCR future executing topic-id=" doc " page=" page))
                                          (let [result (page/extract-page-text uid doc page ek scan-dpi)]
                                            (if (:success result)
                                              (do (log/log-info (str "OCR scan complete topic-id=" doc " page=" page))
                                                (log/log-debug (str "OCR incrementing !refresh, current=" @!refresh))
                                                (swap! !refresh inc))
                                              (do (log/log-info (str "OCR scan failed topic-id=" doc " page=" page " error=" (:error result)))
                                                (swap! !ocr-errors assoc [doc page] (:error result)))))
                                          (catch Exception e
                                            (log/log-info (str "OCR scan exception topic-id=" doc " page=" page " ex=" (.getMessage e)))
                                            (swap! !ocr-errors assoc [doc page] (.getMessage e)))
                                          (finally
                                            (log/log-info (str "OCR scan cleanup topic-id=" doc " page=" page " removing from scanning-pages"))
                                            (swap! !scanning-pages disj [doc page]))))
                                      :started))))
                              (token)))))) ;; end when llm-enabled?
                    
                    ;; OCR error display — auto-dismiss after 3 seconds
                    (when-let [ocr-err (get ocr-errors [selected-doc current-pdf-page])]
                      (let [!show (atom true)
                            show (e/watch !show)]
                        (dom/div
                          (dom/props {:style {:padding "6px 10px" :background "#fef2f2" :border "1px solid #fecaca"
                                              :border-radius "var(--radius-sm)" :font-size "13px" :color "#991b1b"
                                              :margin-top "var(--sp-1)"
                                              :opacity (if show "1" "0")
                                              :transition "opacity 0.5s ease-out"}})
                          (dom/text ocr-err)
                          (e/client
                            (js/setTimeout (fn [] (reset! !show false)) 3000)))))

                    ;; Save status indicator with fade-out
                    (when (some? dirty-data)
                      (let [is-success (:success save-result)
                            message (if is-success "Saved" (str "Save error: " (:error save-result)))
                            !show (atom true)
                            show (e/watch !show)]
                        (dom/span
                          (dom/props {:style {:margin-left "12px"
                                              :font-size "12px"
                                              :color (if is-success "var(--color-text-secondary)" "var(--color-danger)")
                                              :opacity (if show "1" "0")
                                              :transition "opacity 0.5s ease-out"}})
                          (dom/text message)
                          ;; Fade out after 2 seconds (only for successful saves)
                          (when is-success
                            (js/setTimeout
                              (fn [] (reset! !show false))
                              2000))))))

                  ;; Source reference — collapsed to compact editable field in page header
                  (let [current-source (e/server (get-topic-source* refresh selected-doc))
                        !editing-source (atom false)
                        editing-source (e/watch !editing-source)]
                    (if editing-source
                      ;; Expanded source editor (shown on click)
                      (dom/div
                        (dom/props {:style {:display "flex" :align-items "center" :gap "6px"
                                            :padding "3px 8px" :flex-shrink "0"
                                            :background "var(--color-bg-subtle)" :border-bottom "1px solid var(--color-border)"}})
                        (dom/span
                          (dom/props {:style {:font-size "11px" :color "var(--color-text-hint)" :flex-shrink "0"}})
                          (dom/text "Source:"))
                        (e/for-by identity [_k [selected-doc]]
                          (dom/input
                            (dom/props {:type "text"
                                        :placeholder "Source reference"
                                        :style {:flex "1" :padding "2px 6px" :font-size "12px" :color "#555"
                                                :border "1px solid var(--color-border)" :border-radius "3px"
                                                :background "var(--color-bg-card)"}})
                            (set! (.-value dom/node) (or current-source ""))
                            (let [event (dom/On "change" #(-> % .-target .-value) nil)
                                  [?token _] (e/Token event)]
                              (when-some [token ?token]
                                (e/server (db/update-topic-source! selected-doc event))
                                (e/server (swap! !refresh inc))
                                (token)
                                (reset! !editing-source false)))))
                        (dom/button
                          (dom/props {:style {:padding "2px 8px" :font-size "11px" :background "#f0f0f0"
                                              :border "1px solid var(--color-border)" :border-radius "3px" :cursor "pointer"}})
                          (dom/text "Close")
                          (dom/On "click" (fn [_] (reset! !editing-source false)) nil)))
                      ;; Collapsed: just a small clickable source indicator
                      (when (seq current-source)
                        (dom/span
                          (dom/props {:style {:font-size "11px" :color "var(--color-text-hint)" :cursor "pointer"
                                              :padding "0 8px" :flex-shrink "0"
                                              :overflow "hidden" :text-overflow "ellipsis" :white-space "nowrap"
                                              :max-width "200px"}
                                      :title (str "Source: " current-source " (click to edit)")})
                          (dom/text current-source)
                          (dom/On "click" (fn [_] (reset! !editing-source true)) nil)))))

                  ;; Editor area
                  (dom/div
                    (dom/props {:style {:flex "1" :min-height "0" :overflow "hidden"}})
                    (if (:success text-result)
                      (dom/div
                        (dom/props {:style {:height "100%" :display "flex" :flex-direction "column" :overflow "hidden"}})
                        (RichTextEditorComponent {:initial-html (:text text-result)
                                                  :topic-id page-topic-id}))
                      (dom/p
                        (dom/props {:style {:color "gray"}})
                        (dom/text "No text scanned yet. Click 'Scan Page' to process this page."))))))

              ;; Vertical drag handle (full width)
              (dom/div
                (dom/props {:class "split-divider-v"
                            :title "Drag to resize panels"})
                (dom/On "pointerdown" (fn [e] (util/start-drag! e :y !top-pct)) nil))

              ;; BOTTOM PANEL: Shared toolbar + card table
              (dom/div
                (dom/props {:style {:flex "1" :display "flex" :flex-direction "column" :min-height "0" :overflow "hidden"}})

                (ContentToolbar {:user-id user-id
                                 :enc-key enc-key
                                 :topic-id page-topic-id
                                 :root-topic-id selected-doc
                                 :page-number current-pdf-page
                                 :content-text page-text
                                 :context-mode :page
                                 :context-tooltip "Include context for better cards. With a selection: current page + N previous pages. Without: N previous pages."
                                 :llm-enabled? llm-enabled?}
                  !refresh)

                (ContentCardTable {:topic-id page-topic-id
                                   :card-font-size card-font-size}
                  !refresh)))))))))
