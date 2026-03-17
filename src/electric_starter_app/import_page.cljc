(ns electric-starter-app.import-page
  "Import tab — Upload PDF, paste articles, or import from URL."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   #?(:clj [electric-starter-app.db :as db])
   #?(:clj [electric-starter-app.wikipedia :as wiki])
   #?(:clj [electric-starter-app.html-cleaner :as cleaner])))

;; Server-side wrappers
(defn save-web-document* [user-id title html source-type source-url]
  #?(:clj (db/save-web-document user-id title (cleaner/clean-html html) source-type source-url)
     :cljs nil))

(defn fetch-url* [url]
  #?(:clj (wiki/fetch-url url)
     :cljs nil))

;; Paste Import Modal
(e/defn PasteImportModal [!show user-id !refresh]
  (e/client
    (let [!title (atom "")
          !url (atom "")
          !html (atom nil)]
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

          ;; Buttons
          (dom/div
            (dom/props {:style {:display "flex" :gap "var(--sp-2)" :justify-content "flex-end"}})
            (dom/button
              (dom/props {:class "btn btn-secondary"})
              (dom/text "Cancel")
              (dom/On "click" (fn [_] (reset! !show false)) nil))
            (dom/button
              (dom/props {:class "btn btn-primary" :style {:font-weight "600"}})
              (dom/text "Import")
              (let [event (dom/On "click"
                            (fn [_] {:title @!title :html @!html :url @!url})
                            nil)
                    [?token ?error] (e/Token event)]
                (when ?error
                  (dom/div (dom/props {:style {:color "var(--color-danger)" :font-size "12px" :margin-top "var(--sp-1)"}})
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
(e/defn URLImportModal [!show user-id !refresh]
  (e/client
    (let [!url (atom "")]
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
                        :class "input input-full" :style {:padding "10px 12px" :margin-bottom "var(--sp-4)"}})
            (dom/On "input" (fn [e] (reset! !url (-> e .-target .-value))) nil))
          (dom/div
            (dom/props {:style {:display "flex" :gap "var(--sp-2)" :justify-content "flex-end"}})
            (dom/button
              (dom/props {:class "btn btn-secondary"})
              (dom/text "Cancel")
              (dom/On "click" (fn [_] (reset! !show false)) nil))
            (dom/button
              (let [event (dom/On "click" (fn [_] @!url) nil)
                    [?token ?error] (e/Token event)
                    importing? (some? ?token)]
                (dom/props {:class "btn btn-primary"
                            :style {:font-weight "600"
                                    :background (if importing? "var(--color-primary-light)" "var(--color-primary)")
                                    :cursor (if importing? "not-allowed" "pointer")}
                            :disabled importing?})
                (dom/text (if importing? "Importing..." "Import"))
                (when ?error
                  (dom/div (dom/props {:style {:color "var(--color-danger)" :font-size "12px" :margin-top "var(--sp-1)"}})
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

;; Main Import page component
(e/defn ImportPage [user-id !refresh]
  (e/client
    (dom/div
      (dom/props {:style {:padding "var(--sp-4)" :max-width "720px"}})
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
            ;; Hidden form
            (dom/form
              (dom/props {:action "/api/upload-pdf" :method "post" :enctype "multipart/form-data"
                          :style {:display "none"}})
              (dom/input
                (dom/props {:type "file" :name "file" :accept "application/pdf"})
                (reset! !file-input dom/node)
                (dom/On "change" (fn [e] (.. e -target -form submit)) nil)))))

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
            (PasteImportModal !show-paste user-id !refresh)))

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
            (URLImportModal !show-url user-id !refresh)))))))
