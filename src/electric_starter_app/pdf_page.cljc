(ns electric-starter-app.pdf-page
  "Document upload, web import, and management UI."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.electric-scroll0 :refer [Scroll-window Tape]]
   [contrib.data :refer [clamp-left]]
   #?(:clj [electric-starter-app.pdf :as pdf])
   #?(:clj [electric-starter-app.db :as db])
   #?(:clj [electric-starter-app.wikipedia :as wiki])
   #?(:clj [electric-starter-app.html-cleaner :as cleaner])
   #?(:clj [clojure.string :as str])
   [electric-starter-app.card-components :as card-components]))

#?(:clj (defonce !refresh (atom 0))) ; Server-side refresh trigger

;; Query wrapper: takes refresh arg to create Electric reactive dependency
#?(:clj (defn list-pdfs* [_refresh user-id] (pdf/list-pdfs user-id)))

;; Helper: format bytes to human-readable
#?(:clj
   (defn format-bytes [bytes]
     (if (nil? bytes)
       "-"
       (cond
         (< bytes 1024) (str bytes " B")
         (< bytes (* 1024 1024)) (format "%.1f KB" (double (/ bytes 1024)))
         :else (format "%.1f MB" (double (/ bytes 1024 1024)))))))

;; Helper: format timestamp to "2026-03-14 10:11"
#?(:clj
   (defn format-timestamp [ts]
     (when ts
       (let [fmt (java.time.format.DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm")]
         (.format (.toLocalDateTime ts) fmt)))))

#?(:clj
   (defn filter-docs [docs filter-text]
     (if (or (nil? filter-text) (str/blank? filter-text))
       docs
       (let [q (str/lower-case (str/trim filter-text))]
         (filterv #(str/includes? (str/lower-case (or (:documents/filename %) "")) q) docs)))))

;; Server-side wrappers
(defn save-web-document* [user-id title html source-type source-url]
  #?(:clj (db/save-web-document user-id title (cleaner/clean-html html) source-type source-url)
     :cljs nil))

(defn fetch-url* [url]
  #?(:clj (wiki/fetch-url url)
     :cljs nil))

;; Paste Import Modal
(e/defn PasteImportModal [!show user-id]
  (e/client
    (let [!title (atom "")
          !url (atom "")
          !html (atom nil)]
      (dom/div
        (dom/props {:style {:position "fixed" :top "0" :left "0" :width "100%" :height "100%"
                            :background "rgba(0,0,0,0.3)" :display "flex" :align-items "center"
                            :justify-content "center" :z-index "1000"}})
        (dom/On "click" (fn [e]
                          (when (= (.-target e) (.-currentTarget e))
                            (reset! !show false)))
          nil)
        (dom/div
          (dom/props {:style {:background "white" :border-radius "8px" :padding "24px"
                              :width "600px" :max-height "80vh" :display "flex" :flex-direction "column"
                              :box-shadow "0 4px 20px rgba(0,0,0,0.25)"}})
          (dom/h3
            (dom/props {:style {:margin "0 0 16px 0" :font-size "16px"}})
            (dom/text "Import Web Article"))

          ;; Title
          (dom/label
            (dom/props {:style {:font-size "13px" :color "#555" :margin-bottom "4px"}})
            (dom/text "Title"))
          (dom/input
            (dom/props {:type "text" :placeholder "Article title"
                        :value (e/watch !title)
                        :style {:width "100%" :padding "8px 12px" :margin-bottom "12px"
                                :border "1px solid #ccc" :border-radius "4px" :font-size "14px"
                                :box-sizing "border-box"}})
            (dom/On "input" (fn [e] (reset! !title (-> e .-target .-value))) nil))

          ;; Source URL (optional)
          (dom/label
            (dom/props {:style {:font-size "13px" :color "#555" :margin-bottom "4px"}})
            (dom/text "Source URL (optional)"))
          (dom/input
            (dom/props {:type "text" :placeholder "https://..."
                        :value (e/watch !url)
                        :style {:width "100%" :padding "8px 12px" :margin-bottom "12px"
                                :border "1px solid #ccc" :border-radius "4px" :font-size "14px"
                                :box-sizing "border-box"}})
            (dom/On "input" (fn [e] (reset! !url (-> e .-target .-value))) nil))

          ;; Paste area
          (dom/label
            (dom/props {:style {:font-size "13px" :color "#555" :margin-bottom "4px"}})
            (dom/text "Content (paste from browser with Ctrl+V)"))
          (dom/div
            (dom/props {:contenteditable "true"
                        :style {:flex "1" :min-height "200px" :max-height "400px" :overflow-y "auto"
                                :padding "12px" :border "1px solid #ccc" :border-radius "4px"
                                :font-size "14px" :line-height "1.6" :margin-bottom "16px"
                                :background "#fafafa"}})
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

          ;; Buttons
          (dom/div
            (dom/props {:style {:display "flex" :gap "8px" :justify-content "flex-end"}})
            (dom/button
              (dom/props {:style {:padding "8px 16px" :background "#f0f0f0" :color "#333"
                                  :border "1px solid #ccc" :border-radius "4px" :cursor "pointer"
                                  :font-size "14px"}})
              (dom/text "Cancel")
              (dom/On "click" (fn [_] (reset! !show false)) nil))
            (dom/button
              (dom/props {:style {:padding "8px 16px" :background "#2563eb" :color "white"
                                  :border "none" :border-radius "4px" :cursor "pointer"
                                  :font-size "14px" :font-weight "600"}})
              (dom/text "Import")
              (let [event (dom/On "click"
                            (fn [_] {:title @!title :html @!html :url @!url})
                            nil)
                    [?token ?error] (e/Token event)]
                (when ?error
                  (dom/div (dom/props {:style {:color "red" :font-size "12px" :margin-top "4px"}})
                    (dom/text ?error)))
                (when-some [token ?token]
                  (let [title-val (:title event)
                        html-val (:html event)
                        url-val (:url event)]
                    (if (and (seq title-val) (seq html-val))
                      (let [result (e/server (save-web-document* user-id title-val html-val "web" (when (seq url-val) url-val)))]
                        (if result
                          (do (e/server (swap! !refresh inc))
                            (reset! !show false)
                            (token))
                          (token "Failed to save article")))
                      (token "Title and content are required"))))))))))))

;; URL Import Modal — accepts any URL, auto-detects Wikipedia
(e/defn URLImportModal [!show user-id]
  (e/client
    (let [!url (atom "")]
      (dom/div
        (dom/props {:style {:position "fixed" :top "0" :left "0" :width "100%" :height "100%"
                            :background "rgba(0,0,0,0.3)" :display "flex" :align-items "center"
                            :justify-content "center" :z-index "1000"}})
        (dom/On "click" (fn [e]
                          (when (= (.-target e) (.-currentTarget e))
                            (reset! !show false)))
          nil)
        (dom/div
          (dom/props {:style {:background "white" :border-radius "8px" :padding "24px"
                              :width "500px" :box-shadow "0 4px 20px rgba(0,0,0,0.25)"}})
          (dom/h3
            (dom/props {:style {:margin "0 0 16px 0" :font-size "16px"}})
            (dom/text "Import from URL"))
          (dom/p
            (dom/props {:style {:margin "0 0 12px 0" :font-size "13px" :color "#666"}})
            (dom/text "Paste a Wikipedia or any web page URL. Wikipedia articles are fetched via API for best quality."))
          (dom/input
            (dom/props {:type "text" :placeholder "https://en.wikipedia.org/wiki/..."
                        :value (e/watch !url)
                        :style {:width "100%" :padding "10px 12px" :border "1px solid #ccc"
                                :border-radius "4px" :font-size "14px" :box-sizing "border-box"
                                :margin-bottom "16px"}})
            (dom/On "input" (fn [e] (reset! !url (-> e .-target .-value))) nil))
          (dom/div
            (dom/props {:style {:display "flex" :gap "8px" :justify-content "flex-end"}})
            (dom/button
              (dom/props {:style {:padding "8px 16px" :background "#f0f0f0" :color "#333"
                                  :border "1px solid #ccc" :border-radius "4px" :cursor "pointer"
                                  :font-size "14px"}})
              (dom/text "Cancel")
              (dom/On "click" (fn [_] (reset! !show false)) nil))
            (dom/button
              (let [event (dom/On "click" (fn [_] @!url) nil)
                    [?token ?error] (e/Token event)
                    importing? (some? ?token)]
                (dom/props {:style {:padding "8px 16px"
                                    :background (if importing? "#93c5fd" "#2563eb")
                                    :color "white"
                                    :border "none" :border-radius "4px"
                                    :cursor (if importing? "not-allowed" "pointer")
                                    :font-size "14px" :font-weight "600"}
                            :disabled importing?})
                (dom/text (if importing? "Importing..." "Import"))
                (when ?error
                  (dom/div (dom/props {:style {:color "red" :font-size "12px" :margin-top "4px"}})
                    (dom/text ?error)))
                (when-some [token ?token]
                  (if (seq event)
                    (let [result (e/server (fetch-url* event))]
                      (if (:success result)
                        (let [doc-id (e/server (save-web-document* user-id
                                                 (:title result) (:html result)
                                                 (:source-type result) (:url result)))]
                          (if doc-id
                            (do (e/server (swap! !refresh inc))
                              (reset! !show false)
                              (token))
                            (token "Failed to save article")))
                        (token (:error result))))
                    (token "Please enter a URL")))))))))))


;; Main Documents page component
(e/defn PdfPage [user-id !nav-target navigate!]
  (e/client
    (dom/div
      (dom/props {:style {:padding "16px" :max-width "900px" :height "100%" :display "flex" :flex-direction "column"}})
      (dom/h2
        (dom/props {:style {:margin "0 0 12px 0" :font-size "20px"}})
        (dom/text "Documents"))

      ;; Import buttons row
      (dom/div
        (dom/props {:style {:display "flex" :gap "8px" :margin-bottom "12px" :flex-wrap "wrap"}})

        ;; Upload PDF
        (let [!file-input (atom nil)]
          (dom/form
            (dom/props {:action "/api/upload-pdf" :method "post" :enctype "multipart/form-data"})
            (dom/input
              (dom/props {:type "file" :name "file" :accept "application/pdf"
                          :style {:display "none"}})
              (reset! !file-input dom/node)
              (dom/On "change" (fn [e] (.. e -target -form submit)) nil))
            (dom/button
              (dom/props {:type "button"
                          :style {:padding "8px 20px" :background "#2563eb" :color "white"
                                  :border "none" :border-radius "6px" :cursor "pointer"
                                  :font-size "14px" :font-weight "600"}})
              (dom/text "Upload PDF")
              (dom/On "click" (fn [_] (when-some [inp @!file-input] (.click inp))) nil))))

        ;; Paste Article
        (let [!show-paste (atom false)
              show-paste (e/watch !show-paste)]
          (dom/button
            (dom/props {:style {:padding "8px 20px" :background "#2563eb" :color "white"
                                :border "none" :border-radius "6px" :cursor "pointer"
                                :font-size "14px" :font-weight "600"}})
            (dom/text "Paste Article")
            (dom/On "click" (fn [_] (reset! !show-paste true)) nil))
          (when show-paste
            (PasteImportModal !show-paste user-id)))

        ;; Import from URL
        (let [!show-url (atom false)
              show-url (e/watch !show-url)]
          (dom/button
            (dom/props {:style {:padding "8px 20px" :background "#2563eb" :color "white"
                                :border "none" :border-radius "6px" :cursor "pointer"
                                :font-size "14px" :font-weight "600"}})
            (dom/text "Import URL")
            (dom/On "click" (fn [_] (reset! !show-url true)) nil))
          (when show-url
            (URLImportModal !show-url user-id))))

      ;; Search filter
      (let [!filter-text (atom "")
            filter-text (e/watch !filter-text)]
        (dom/input
          (dom/props {:type "text" :placeholder "Filter documents..."
                      :style {:width "100%" :max-width "400px" :padding "8px 12px"
                              :margin-bottom "12px" :border "1px solid #ccc" :border-radius "4px"
                              :font-size "14px"}})
          (dom/On "input" (fn [e] (reset! !filter-text (-> e .-target .-value))) nil))

        ;; Document table with virtual scroll
        (e/server
          (let [refresh (e/watch !refresh)
                docs-result (list-pdfs* refresh user-id)]
            (e/client
              (if (:success docs-result)
                (let [all-docs (e/server (:documents docs-result))
                      total-doc-count (e/server (count all-docs))
                      docs-vec (e/server (vec (filter-docs all-docs filter-text)))
                      doc-count (e/server (count docs-vec))
                      row-height 40]
                  (dom/div
                    (dom/props {:style {:flex "1" :display "flex" :flex-direction "column" :min-height "0"}})

                    ;; Table header
                    (dom/table
                      (dom/props {:style {:width "100%" :border-collapse "collapse" :font-size "14px" :table-layout "fixed" :flex-shrink "0"}})
                      (dom/thead
                        (dom/tr
                          (dom/th (dom/props {:style {:text-align "left" :padding "8px 10px" :border-bottom "2px solid #e0e0e0" :font-weight "600" :color "#444"}}) (dom/text "Name"))
                          (dom/th (dom/props {:style {:text-align "center" :padding "8px 10px" :border-bottom "2px solid #e0e0e0" :font-weight "600" :color "#444" :width "60px"}}) (dom/text "Type"))
                          (dom/th (dom/props {:style {:text-align "right" :padding "8px 10px" :border-bottom "2px solid #e0e0e0" :font-weight "600" :color "#444" :width "80px"}}) (dom/text "Size"))
                          (dom/th (dom/props {:style {:text-align "left" :padding "8px 10px" :border-bottom "2px solid #e0e0e0" :font-weight "600" :color "#444" :width "140px"}}) (dom/text "Uploaded"))
                          (dom/th (dom/props {:style {:text-align "center" :padding "8px 10px" :border-bottom "2px solid #e0e0e0" :font-weight "600" :color "#444" :width "80px"}}) (dom/text "Actions")))))

                    (if (pos? doc-count)
                      ;; Scrollable body
                      (dom/div
                        (dom/props {:style {:flex "1" :overflow-y "auto" :min-height "0"}})
                        (let [[offset limit] (Scroll-window row-height doc-count dom/node {:overquery-factor 1})
                              occluded-height (clamp-left (* row-height (- doc-count limit)) 0)]
                          (dom/props {:class "tape-scroll"
                                      :style {:--offset offset :--row-height (str row-height "px")}})
                          (dom/table
                            (dom/props {:style {:width "100%" :border-collapse "collapse" :font-size "14px" :table-layout "fixed"
                                                :grid-template-columns "1fr 60px 80px 140px 80px"}})
                            (e/for [i (Tape offset limit)]
                              (let [item (e/server (nth docs-vec i nil))]
                                (when item
                                  (let [id (e/server (:documents/id item))
                                        filename (e/server (:documents/filename item))
                                        file-size (e/server (format-bytes (:documents/file_size item)))
                                        uploaded (e/server (format-timestamp (:documents/uploaded_at item)))
                                        source-type (e/server (or (:documents/source_type item) "pdf"))
                                        type-label (e/server (case source-type
                                                               "wikipedia" "Wiki"
                                                               "web" "Web"
                                                               "PDF"))
                                        type-color (e/server (case source-type
                                                               "wikipedia" "#fef3c7"
                                                               "web" "#e0f2fe"
                                                               "#dcfce7"))]
                                    (dom/tr
                                      (dom/props {:style {:border-bottom "1px solid #f0f0f0" :height (str row-height "px")
                                                          :--order (inc i)}})
                                        ;; Name — clickable link
                                      (dom/td
                                        (dom/props {:style {:padding "8px 10px" :overflow "hidden" :text-overflow "ellipsis" :white-space "nowrap"}})
                                        (dom/span
                                          (dom/props {:style {:color "#2563eb" :cursor "pointer" :text-decoration "underline"}
                                                      :title "Open in Learn tab"})
                                          (dom/text filename)
                                          (dom/On "click"
                                            (fn [_]
                                              (reset! !nav-target {:doc-id id})
                                              (navigate! :learn))
                                            nil)))
                                        ;; Type badge
                                      (dom/td
                                        (dom/props {:style {:padding "8px 10px" :text-align "center" :width "60px"}})
                                        (dom/span
                                          (dom/props {:style {:padding "2px 8px" :border-radius "4px" :font-size "11px"
                                                              :font-weight "600" :background type-color}})
                                          (dom/text type-label)))
                                        ;; Size
                                      (dom/td
                                        (dom/props {:style {:padding "8px 10px" :text-align "right" :color "#555" :width "80px"}})
                                        (dom/text file-size))
                                        ;; Uploaded
                                      (dom/td
                                        (dom/props {:style {:padding "8px 10px" :color "#555" :width "140px"}})
                                        (dom/text (or uploaded "-")))
                                        ;; Delete
                                      (dom/td
                                        (dom/props {:style {:padding "8px 10px" :text-align "center" :width "80px"}})
                                        (let [!deleting (atom false)
                                              deleting (e/watch !deleting)]
                                          (dom/button
                                            (dom/props {:disabled deleting
                                                        :style {:padding "3px 10px" :background (if deleting "#999" "#dc3545")
                                                                :color "white" :border "none" :border-radius "3px"
                                                                :cursor (if deleting "not-allowed" "pointer")
                                                                :font-size "12px"}})
                                            (dom/text (if deleting "..." "Delete"))
                                            (let [click-event (dom/On "click"
                                                                (fn [_]
                                                                  #?(:cljs
                                                                     (when (js/confirm "Delete this document? All pages, extracts, and cards will be permanently removed.")
                                                                       id)
                                                                     :clj nil))
                                                                nil)
                                                  [?token ?error] (e/Token click-event)]
                                              (when ?error
                                                (dom/span
                                                  (dom/props {:style {:color "red" :font-size "11px" :margin-left "4px"}})
                                                  (dom/text ?error)))
                                              (when-some [token ?token]
                                                (reset! !deleting true)
                                                (let [note-ids (e/server (db/get-anki-note-ids-for-document click-event))
                                                      result (e/server (pdf/delete-pdf user-id click-event))]
                                                  (if (:success result)
                                                    (do (reset! !deleting false)
                                                      (e/server (swap! !refresh inc))
                                                      (e/client (card-components/try-delete-anki-notes! note-ids))
                                                      (token))
                                                    (do (reset! !deleting false)
                                                      (token (:error result))))))))))))))))
                          (dom/div (dom/props {:style {:height (str occluded-height "px")}}))))

                      ;; Empty state
                      (dom/p
                        (dom/props {:style {:color "#888" :font-size "14px" :padding "16px 0"}})
                        (dom/text (if (zero? total-doc-count)
                                    "No documents yet. Upload a PDF, paste an article, or import from URL above."
                                    "No documents match your search."))))))

                (dom/div
                  (dom/props {:style {:color "red"}})
                  (dom/text "Error loading documents: " (:error docs-result)))))))))))
