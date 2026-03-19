(ns electric-starter-app.import-page
  "Import tab — Upload PDF, paste articles, or import from URL."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [electric-starter-app.util :as util]
   #?(:clj [electric-starter-app.db :as db])
   #?(:clj [electric-starter-app.wikipedia :as wiki])
   #?(:clj [electric-starter-app.html-cleaner :as cleaner])
   #?(:clj [electric-starter-app.extractor :as extractor])))

;; Server-side wrappers
(defn format-timestamp* [ts]
  #?(:clj (when ts
            (let [fmt (java.time.format.DateTimeFormatter/ofPattern "MMM d, yyyy")
                  instant (if (instance? java.sql.Timestamp ts)
                            (.toInstant ts)
                            (.toInstant ts))
                  ldt (java.time.LocalDateTime/ofInstant instant (java.time.ZoneId/systemDefault))]
              (.format ldt fmt)))
     :cljs nil))

(defn save-web-document* [user-id title html source-type source-url]
  #?(:clj (db/save-web-document user-id title (cleaner/clean-html html) source-type source-url)
     :cljs nil))

(defn clean-html* [html]
  #?(:clj (cleaner/clean-html html)
     :cljs nil))

(defn fetch-url* [url]
  #?(:clj (wiki/fetch-url url)
     :cljs nil))

(defn try-auto-extract*
  "Best-effort auto-extract: segments HTML into topics, highlights section headings
   in the page HTML, and batch-inserts content items. Returns nil.
   Logs and swallows errors so the import still succeeds."
  [document-id page-number html]
  #?(:clj (try
            (let [start (System/currentTimeMillis)
                  result (extractor/extract-and-annotate html)
                  elapsed (- (System/currentTimeMillis) start)]
              (when result
                (let [{:keys [sections annotated-html]} result
                      clean-sections (mapv #(update % :content cleaner/clean-html) sections)]
                  (println (str "INFO [import] auto-extract: " (count clean-sections)
                             " topics in " elapsed "ms"))
                  ;; Save content items
                  (when (seq clean-sections)
                    (db/batch-create-content-items document-id page-number clean-sections))
                  ;; Update page text with highlighted HTML (bypasses cleaner — Quill-compatible)
                  (when annotated-html
                    (db/save-page-text document-id page-number annotated-html))))
              nil)
            (catch Exception e
              (println "WARN [import] auto-extract failed:" (.getMessage e))
              nil))
     :cljs nil))

;; Paste Import Modal
(e/defn PasteImportModal [!show user-id !refresh !nav-target navigate!]
  (e/client
    (let [!title (atom "")
          !url (atom "")
          !html (atom nil)
          !auto-extract (atom true)]
      (dom/div
        (dom/props {:class "modal-backdrop"})
        (dom/On "click" (fn [e]
                          (when (= (.-target e) (.-currentTarget e))
                            (reset! !show false)))
          nil)
        (dom/div
          (dom/props {:class "modal-content modal-lg" :style {:max-height "80vh" :display "flex" :flex-direction "column"}})
          (dom/h3
            (dom/props {:style {:margin "0 0 16px 0" :font-size "16px"}})
            (dom/text "Import Web Article"))

          ;; Title
          (dom/label
            (dom/props {:class "label" :style {:color "var(--color-text-secondary)"}})
            (dom/text "Title"))
          (dom/input
            (dom/props {:type "text" :placeholder "Article title"
                        :value (e/watch !title)
                        :class "input input-full" :style {:margin-bottom "var(--sp-3)"}})
            (dom/On "input" (fn [e] (reset! !title (-> e .-target .-value))) nil))

          ;; Source URL (optional)
          (dom/label
            (dom/props {:class "label" :style {:color "var(--color-text-secondary)"}})
            (dom/text "Source URL (optional)"))
          (dom/input
            (dom/props {:type "text" :placeholder "https://..."
                        :value (e/watch !url)
                        :class "input input-full" :style {:margin-bottom "var(--sp-3)"}})
            (dom/On "input" (fn [e] (reset! !url (-> e .-target .-value))) nil))

          ;; Paste area
          (dom/label
            (dom/props {:class "label" :style {:color "var(--color-text-secondary)"}})
            (dom/text "Content (paste from browser with Ctrl+V)"))
          (dom/div
            (dom/props {:contenteditable "true"
                        :style {:flex "1" :min-height "200px" :max-height "400px" :overflow-y "auto"
                                :padding "var(--sp-3)" :border "1px solid var(--color-border)" :border-radius "var(--radius-sm)"
                                :font-size "14px" :line-height "1.6" :margin-bottom "var(--sp-4)"
                                :background "var(--color-bg-subtle)"}})
            (dom/On "paste"
              (fn [e]
                (let [cd (.-clipboardData e)
                      html-data (.getData cd "text/html")
                      text-data (.getData cd "text/plain")
                      content (if (seq html-data) html-data (str "<p>" text-data "</p>"))]
                  (reset! !html content)
                  ;; Auto-fill title from first heading if empty
                  #?(:cljs
                     (when (empty? @!title)
                       (let [tmp (js/document.createElement "div")]
                         (set! (.-innerHTML tmp) content)
                         (when-let [h (or (.querySelector tmp "h1")
                                        (.querySelector tmp "h2")
                                        (.querySelector tmp "h3"))]
                           (reset! !title (.-textContent h))))))))
              nil))

          ;; Options
          (dom/div
            (dom/props {:style {:margin-bottom "var(--sp-3)" :display "flex" :flex-direction "column" :gap "8px"}})
            (dom/label
              (dom/props {:style {:display "flex" :align-items "center" :gap "8px" :font-size "13px" :cursor "pointer"}})
              (dom/input (dom/props {:type "checkbox"})
                (set! (.-checked dom/node) (e/watch !auto-extract))
                (dom/On "change" (fn [e] (reset! !auto-extract (-> e .-target .-checked))) nil))
              (dom/text "Auto-extract into topics")))

          ;; Buttons
          (dom/div
            (dom/props {:style {:display "flex" :gap "var(--sp-2)" :justify-content "flex-end"}})
            (dom/button
              (dom/props {:class "btn btn-secondary"})
              (dom/text "Cancel")
              (dom/On "click" (fn [_] (reset! !show false)) nil))
            (dom/button
              (dom/props {:class "btn btn-primary" :style {:font-weight "600"}})
              (let [event (dom/On "click"
                            (fn [_] {:title @!title :html @!html :url @!url
                                     :auto-extract @!auto-extract})
                            nil)
                    [?token ?error] (e/Token event)
                    importing? (some? ?token)]
                (dom/props {:disabled importing?
                            :style {:cursor (if importing? "not-allowed" "pointer")}})
                (dom/text (if importing? "Processing..." "Import"))
                (when ?error
                  (dom/div (dom/props {:style {:color "var(--color-danger)" :font-size "12px" :margin-top "var(--sp-1)"}})
                    (dom/text ?error)))
                (when-some [token ?token]
                  (let [title-val (:title event)
                        html-val (:html event)
                        url-val (:url event)
                        ae? (:auto-extract event)]
                    (if (and (seq title-val) (seq html-val))
                      (let [doc-id (e/server
                                     (let [did (save-web-document* user-id title-val html-val "web"
                                                 (when (seq url-val) url-val))]
                                       (when (and ae? did)
                                         (try-auto-extract*
                                           did 1
                                           ;; Use the cleaned HTML (same as what save-web-document stored)
                                           (clean-html* html-val)))
                                       did))]
                        (if doc-id
                          (do (e/server (swap! !refresh inc))
                            (token)
                            (reset! !show false)
                            (reset! !nav-target {:doc-id doc-id})
                            (navigate! :learn))
                          (token "Failed to save article")))
                      (token "Title and content are required"))))))))))))

;; URL Import Modal — accepts any URL, auto-detects Wikipedia
(e/defn URLImportModal [!show user-id !refresh !nav-target navigate!]
  (e/client
    (let [!url (atom "")
          !auto-extract (atom true)]
      (dom/div
        (dom/props {:class "modal-backdrop"})
        (dom/On "click" (fn [e]
                          (when (= (.-target e) (.-currentTarget e))
                            (reset! !show false)))
          nil)
        (dom/div
          (dom/props {:class "modal-content" :style {:width "500px" :max-width "90%"}})
          (dom/h3
            (dom/props {:style {:margin "0 0 16px 0" :font-size "16px"}})
            (dom/text "Import from URL"))
          (dom/p
            (dom/props {:style {:margin "0 0 12px 0" :font-size "13px" :color "var(--color-text-secondary)"}})
            (dom/text "Paste a Wikipedia or any web page URL. Wikipedia articles are fetched via API for best quality."))
          (dom/input
            (dom/props {:type "text" :placeholder "https://en.wikipedia.org/wiki/..."
                        :value (e/watch !url)
                        :class "input input-full" :style {:padding "10px 12px" :margin-bottom "var(--sp-3)"}})
            (dom/On "input" (fn [e] (reset! !url (-> e .-target .-value))) nil))

          ;; Options
          (dom/div
            (dom/props {:style {:margin-bottom "var(--sp-3)" :display "flex" :flex-direction "column" :gap "8px"}})
            (dom/label
              (dom/props {:style {:display "flex" :align-items "center" :gap "8px" :font-size "13px" :cursor "pointer"}})
              (dom/input (dom/props {:type "checkbox"})
                (set! (.-checked dom/node) (e/watch !auto-extract))
                (dom/On "change" (fn [e] (reset! !auto-extract (-> e .-target .-checked))) nil))
              (dom/text "Auto-extract into topics")))

          (dom/div
            (dom/props {:style {:display "flex" :gap "var(--sp-2)" :justify-content "flex-end"}})
            (dom/button
              (dom/props {:class "btn btn-secondary"})
              (dom/text "Cancel")
              (dom/On "click" (fn [_] (reset! !show false)) nil))
            (dom/button
              (let [event (dom/On "click"
                            (fn [_] {:url @!url :auto-extract @!auto-extract})
                            nil)
                    [?token ?error] (e/Token event)
                    importing? (some? ?token)]
                (dom/props {:class "btn btn-primary"
                            :style {:font-weight "600"
                                    :cursor (if importing? "not-allowed" "pointer")}
                            :disabled importing?})
                (dom/text (if importing? "Processing..." "Import"))
                (when ?error
                  (dom/div (dom/props {:style {:color "var(--color-danger)" :font-size "12px" :margin-top "var(--sp-1)"}})
                    (dom/text ?error)))
                (when-some [token ?token]
                  (let [url-val (:url event)
                        ae? (:auto-extract event)]
                    (if (seq url-val)
                      (let [result (e/server
                                     (let [fetch-result (fetch-url* url-val)]
                                       (if (:success fetch-result)
                                         (let [raw-html (:html fetch-result)
                                               title (:title fetch-result)
                                               did (save-web-document* user-id title raw-html
                                                     (:source-type fetch-result) (:url fetch-result))]
                                           (when (and ae? did)
                                            ;; raw-html is already cleaned by fetch-url*
                                             (try-auto-extract* did 1 raw-html))
                                           {:success true :doc-id did})
                                         {:success false :error (:error fetch-result)})))]
                        (if (:success result)
                          (if (:doc-id result)
                            (do (e/server (swap! !refresh inc))
                              (token)
                              (reset! !show false)
                              (reset! !nav-target {:doc-id (:doc-id result)})
                              (navigate! :learn))
                            (token "Failed to save article"))
                          (token (:error result))))
                      (token "Please enter a URL"))))))))))))

;; Main Import page component
(e/defn ImportPage [user-id !refresh !nav-target navigate! enc-key llm-enabled?]
  (e/client
    (dom/div
      (dom/props {:style {:padding "var(--sp-4)" :max-width "720px" :width "100%" :margin "0 auto"}})
      (dom/h2
        (dom/props {:style {:margin "0 0 4px 0" :font-size "20px"}})
        (dom/text "Import"))
      (dom/p
        (dom/props {:style {:margin "0 0 24px 0" :font-size "14px" :color "var(--color-text-secondary)"}})
        (dom/text "Add content to your library."))

      ;; Import cards
      (dom/div
        (dom/props {:style {:display "grid" :grid-template-columns "repeat(auto-fill, minmax(200px, 1fr))"
                            :gap "12px"}})

        ;; Upload PDF card
        (let [!file-input (atom nil)]
          (dom/div
            (dom/props {:style {:border "1px solid var(--color-border)" :border-radius "var(--radius-md)"
                                :padding "20px" :cursor "pointer" :transition "border-color 0.15s, box-shadow 0.15s"
                                :background "var(--color-bg-surface)"}})
            (dom/On "mouseenter" (fn [e] (set! (.-borderColor (.-style (.-currentTarget e))) "var(--color-primary)")
                                   (set! (.-boxShadow (.-style (.-currentTarget e))) "0 0 0 1px var(--color-primary)")) nil)
            (dom/On "mouseleave" (fn [e] (set! (.-borderColor (.-style (.-currentTarget e))) "var(--color-border)")
                                   (set! (.-boxShadow (.-style (.-currentTarget e))) "none")) nil)
            (dom/On "click" (fn [_] (when-some [inp @!file-input] (.click inp))) nil)
            (dom/div
              (dom/props {:style {:font-size "24px" :margin-bottom "8px"}})
              (dom/text "\uD83D\uDCC4"))
            (dom/div
              (dom/props {:style {:font-size "14px" :font-weight "600" :margin-bottom "4px"}})
              (dom/text "Upload PDF"))
            (dom/div
              (dom/props {:style {:font-size "12px" :color "var(--color-text-secondary)"}})
              (dom/text "Select a PDF file from your device"))
            ;; Hidden file input (AJAX upload)
            (dom/input
              (dom/props {:type "file" :accept "application/pdf" :style {:display "none"}})
              (reset! !file-input dom/node)
              (dom/On "change"
                (fn [e]
                  (let [file (-> e .-target .-files (aget 0))]
                    (when file
                      (let [form-data (js/FormData.)]
                        (.append form-data "file" file)
                        (-> (js/fetch "/api/upload-pdf" (clj->js {:method "POST" :body form-data}))
                          (.then (fn [resp] (.json resp)))
                          (.then (fn [^js data]
                                   (when (.-success data)
                                     (reset! !nav-target {:doc-id (.-doc_id data)})
                                     (navigate! :learn))))
                          (.catch (fn [err] (js/console.error "Upload failed:" err))))))))
                nil))))

        ;; Paste Article card
        (let [!show-paste (atom false)
              show-paste (e/watch !show-paste)]
          (dom/div
            (dom/props {:style {:border "1px solid var(--color-border)" :border-radius "var(--radius-md)"
                                :padding "20px" :cursor "pointer" :transition "border-color 0.15s, box-shadow 0.15s"
                                :background "var(--color-bg-surface)"}})
            (dom/On "mouseenter" (fn [e] (set! (.-borderColor (.-style (.-currentTarget e))) "var(--color-primary)")
                                   (set! (.-boxShadow (.-style (.-currentTarget e))) "0 0 0 1px var(--color-primary)")) nil)
            (dom/On "mouseleave" (fn [e] (set! (.-borderColor (.-style (.-currentTarget e))) "var(--color-border)")
                                   (set! (.-boxShadow (.-style (.-currentTarget e))) "none")) nil)
            (dom/On "click" (fn [_] (reset! !show-paste true)) nil)
            (dom/div
              (dom/props {:style {:font-size "24px" :margin-bottom "8px"}})
              (dom/text "\uD83D\uDCCB"))
            (dom/div
              (dom/props {:style {:font-size "14px" :font-weight "600" :margin-bottom "4px"}})
              (dom/text "Paste Article"))
            (dom/div
              (dom/props {:style {:font-size "12px" :color "var(--color-text-secondary)"}})
              (dom/text "Paste HTML content from your clipboard")))
          (when show-paste
            (PasteImportModal !show-paste user-id !refresh !nav-target navigate!)))

        ;; Import URL card
        (let [!show-url (atom false)
              show-url (e/watch !show-url)]
          (dom/div
            (dom/props {:style {:border "1px solid var(--color-border)" :border-radius "var(--radius-md)"
                                :padding "20px" :cursor "pointer" :transition "border-color 0.15s, box-shadow 0.15s"
                                :background "var(--color-bg-surface)"}})
            (dom/On "mouseenter" (fn [e] (set! (.-borderColor (.-style (.-currentTarget e))) "var(--color-primary)")
                                   (set! (.-boxShadow (.-style (.-currentTarget e))) "0 0 0 1px var(--color-primary)")) nil)
            (dom/On "mouseleave" (fn [e] (set! (.-borderColor (.-style (.-currentTarget e))) "var(--color-border)")
                                   (set! (.-boxShadow (.-style (.-currentTarget e))) "none")) nil)
            (dom/On "click" (fn [_] (reset! !show-url true)) nil)
            (dom/div
              (dom/props {:style {:font-size "24px" :margin-bottom "8px"}})
              (dom/text "\uD83C\uDF10"))
            (dom/div
              (dom/props {:style {:font-size "14px" :font-weight "600" :margin-bottom "4px"}})
              (dom/text "Import URL"))
            (dom/div
              (dom/props {:style {:font-size "12px" :color "var(--color-text-secondary)"}})
              (dom/text "Fetch from any web page or Wikipedia")))
          (when show-url
            (URLImportModal !show-url user-id !refresh !nav-target navigate!))))

      ;; Recent imports
      (let [refresh (e/server (e/watch !refresh))
            recent-docs (e/server
                          (vec (take 5 (db/get-documents user-id))))]
        (when (seq recent-docs)
          (dom/div
            (dom/props {:style {:margin-top "32px"}})
            (dom/h3
              (dom/props {:style {:font-size "14px" :font-weight "600" :color "var(--color-text-secondary)"
                                  :margin "0 0 12px 0"}})
              (dom/text "Recent imports"))
            (e/for-by :documents/id [doc recent-docs]
              (let [doc-id (:documents/id doc)
                    filename (or (:documents/filename doc) "")
                    source-type (or (:documents/source_type doc) "pdf")
                    uploaded (e/server (format-timestamp* (:documents/uploaded_at doc)))]
                (dom/div
                  (dom/props {:style {:display "flex" :align-items "center" :gap "8px"
                                      :padding "8px 0" :border-bottom "1px solid var(--color-border)"}})
                  (dom/span
                    (dom/props {:class "type-badge"
                                :style {:padding "2px 8px" :font-size "10px"
                                        :background (case source-type
                                                      "wikipedia" "#fef3c7"
                                                      "web" "#e0f2fe"
                                                      "#dcfce7")}})
                    (dom/text (case source-type "wikipedia" "Wiki" "web" "Web" "PDF")))
                  (dom/a
                    (dom/props {:style {:flex "1" :font-size "14px" :color "var(--color-primary)" :cursor "pointer"
                                        :text-decoration "none" :overflow "hidden" :text-overflow "ellipsis" :white-space "nowrap"}})
                    (dom/text (util/display-name filename))
                    (dom/On "click"
                      (fn [_]
                        (reset! !nav-target {:doc-id doc-id})
                        (navigate! :learn))
                      nil))
                  (dom/span
                    (dom/props {:style {:font-size "12px" :color "var(--color-text-hint)" :white-space "nowrap" :flex-shrink "0"}})
                    (dom/text (or uploaded ""))))))))))))
