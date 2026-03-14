(ns electric-starter-app.pdf-page
  "PDF upload and management UI."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   #?(:clj [electric-starter-app.pdf :as pdf])
   #?(:clj [clojure.string :as str])))

#?(:clj (defonce !refresh (atom 0))) ; Server-side refresh trigger

;; Query wrapper: takes refresh arg to create Electric reactive dependency
#?(:clj (defn list-pdfs* [_refresh user-id] (pdf/list-pdfs user-id)))

;; Helper: format bytes to human-readable
#?(:clj
   (defn format-bytes [bytes]
     (if (nil? bytes)
       "Unknown size"
       (cond
         (< bytes 1024) (str bytes " B")
         (< bytes (* 1024 1024)) (format "%.1f KB" (double (/ bytes 1024)))
         :else (format "%.1f MB" (double (/ bytes 1024 1024)))))))

#?(:clj
   (defn filter-docs [docs filter-text]
     (if (or (nil? filter-text) (str/blank? filter-text))
       docs
       (let [q (str/lower-case (str/trim filter-text))]
         (filterv #(str/includes? (str/lower-case (or (:documents/filename %) "")) q) docs)))))

;; PDF list item component
(e/defn PdfListItem [user-id doc !nav-target navigate!]
  (e/client
    (let [id (e/server (:documents/id doc))
          filename (e/server (:documents/filename doc))
          file-size (e/server (:documents/file_size doc))
          uploaded-at (e/server (str (:documents/uploaded_at doc)))

          !deleting (atom false)
          deleting (e/watch !deleting)]

      (dom/div
        (dom/props {:style {:display "flex"
                            :align-items "center"
                            :justify-content "space-between"
                            :padding "12px"
                            :border-bottom "1px solid #e0e0e0"
                            :background "white"}})
        (dom/div
          (dom/strong (dom/text filename))
          (dom/text " ")
          (dom/span
            (dom/props {:style {:color "gray"}})
            (dom/text "(" (e/server (format-bytes file-size)) ", " uploaded-at ")")))

        (dom/div
          (dom/props {:style {:display "flex" :gap "8px"}})

          ;; Open button — navigates to Learn tab with this document
          (dom/button
            (dom/props {:style {:padding "4px 12px" :background "#2563eb" :color "white"
                                :border "none" :border-radius "4px" :cursor "pointer"
                                :font-size "13px"}})
            (dom/text "Open")
            (dom/On "click"
              (fn [_]
                (reset! !nav-target {:doc-id id})
                (navigate! :learn))
              nil))

          ;; Delete button
          (dom/button
            (dom/props {:disabled deleting})
            (dom/text (if deleting "Deleting..." "Delete"))
            (let [click-event (dom/On "click" (fn [_] id) nil)
                  [?token ?error] (e/Token click-event)]

              (when ?error
                (dom/text " ")
                (dom/span
                  (dom/props {:style {:color "red"}})
                  (dom/text ?error)))

              (when-some [token ?token]
                (reset! !deleting true)
                (let [result (e/server (pdf/delete-pdf user-id click-event))]
                  (if (:success result)
                    (do
                      (e/server (swap! !refresh inc))
                      (token))
                    (do
                      (reset! !deleting false)
                      (token (:error result)))))))))))))

;; Main PDF page component
(e/defn PdfPage [user-id !nav-target navigate!]
  (e/client
    (dom/div
      (dom/h1 (dom/text "PDF Documents"))

      ;; Upload form
      (let [!file-input (atom nil)]
        (dom/form
          (dom/props {:action "/api/upload-pdf" :method "post" :enctype "multipart/form-data"})
          (dom/input
            (dom/props {:type "file" :name "file" :accept "application/pdf"
                        :style {:display "none"}})
            (reset! !file-input dom/node)
            (dom/On "change" (fn [e] (.. e -target -form submit)) nil))
          (dom/button
            (dom/props {:type "button"})
            (dom/text "Upload PDF")
            (dom/On "click" (fn [_] (when-some [inp @!file-input] (.click inp))) nil))))

      ;; Search filter
      (let [!filter-text (atom "")
            filter-text (e/watch !filter-text)]
        (dom/input
          (dom/props {:type "text" :placeholder "Filter documents..."
                      :style {:width "100%" :max-width "400px" :padding "8px 12px"
                              :margin "12px 0" :border "1px solid #ccc" :border-radius "4px"
                              :font-size "14px"}})
          (dom/On "input" (fn [e] (reset! !filter-text (-> e .-target .-value))) nil))

        ;; Document list
        (dom/h2 (dom/text "Uploaded Documents"))
        (e/server
          (let [refresh (e/watch !refresh)
                docs-result (list-pdfs* refresh user-id)]
            (e/client
              (if (:success docs-result)
                (let [filtered (e/server (filter-docs (vec (:documents docs-result)) filter-text))]
                  (if (seq filtered)
                    (dom/div
                      (dom/props {:style {:max-height "400px"
                                          :overflow-y "auto"
                                          :border "1px solid #e0e0e0"
                                          :border-radius "4px"}})
                      (e/server
                        (e/for-by :documents/id [doc filtered]
                          (PdfListItem user-id doc !nav-target navigate!))))
                    (dom/p (dom/text "No documents match."))))
                (dom/div
                  (dom/props {:style {:color "red"}})
                  (dom/text "Error loading documents: " (:error docs-result)))))))))))
