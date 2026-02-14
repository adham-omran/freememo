(ns electric-starter-app.ocr-page
  "OCR text extraction UI component."
  (:require
    [hyperfiddle.electric3 :as e]
    [hyperfiddle.electric-dom3 :as dom]
    [electric-starter-app.pdf-viewer-component :refer [PdfViewerComponent]]
    [electric-starter-app.rich-text-editor-component :refer [RichTextEditorComponent]]
    #?(:clj [electric-starter-app.page :as page])
    #?(:clj [electric-starter-app.pdf :as pdf])))

#?(:clj (defonce !refresh (atom 0)))  ; Server-side refresh trigger

;; Query wrapper: takes refresh arg to create Electric reactive dependency
#?(:clj (defn get-page-text* [_refresh document-id page-number]
          (page/get-page-text document-id page-number)))

(e/defn OcrPage []
  (e/client
    (dom/div
      (dom/h1 (dom/text "OCR Text Extraction"))

      ;; Step 1: Select a document
      (dom/h2 (dom/text "1. Select a PDF Document"))
      (let [docs-result (e/server (pdf/list-pdfs))
            !selected-doc (atom nil)
            selected-doc (e/watch !selected-doc)]

        (when (:success docs-result)
          (if (seq (:documents docs-result))
            (dom/div
              (dom/select
                (dom/props {:style {:margin-bottom "20px"}})
                (dom/option
                  (dom/props {:value ""})
                  (dom/text "-- Choose a document --"))
                (e/server
                  (e/for-by :documents/id [doc (:documents docs-result)]
                    (e/client
                      (let [id (e/server (:documents/id doc))
                            filename (e/server (:documents/filename doc))]
                        (dom/option
                          (dom/props {:value (str id)})
                          (dom/text filename))))))
                (dom/On "change"
                  (fn [e]
                    (let [val (-> e .-target .-value)]
                      (if (seq val)
                        (js/parseInt val)
                        nil)))
                  nil))
              (reset! !selected-doc
                (dom/On "change"
                  (fn [e]
                    (let [val (-> e .-target .-value)]
                      (if (seq val)
                        (js/parseInt val)
                        nil)))
                  nil)))
            (dom/p (dom/text "No documents available. Please upload a PDF first."))))

        ;; PDF Viewer and OCR extraction (shows when document is selected)
        (when selected-doc
          ;; Render PDF viewer and capture the current page number it returns
          (dom/div
            (dom/h2 (dom/text "PDF Preview"))
            (dom/p
              (dom/props {:style {:color "#666" :margin-bottom "10px"}})
              (dom/text "Navigate through the PDF using the controls below. The current page will be used for text extraction.")))

          (let [current-pdf-page (PdfViewerComponent {:document-id selected-doc})]

            (dom/div
              ;; Show current page from PDF viewer
              (dom/h2 (dom/text "2. Extract Text from Current Page"))
              (dom/p
                (dom/props {:style {:color "#666" :margin-bottom "10px"}})
                (dom/text "Currently viewing page " current-pdf-page " in the PDF viewer above."))

              ;; Extract button
              (dom/button
                (dom/props {:style {:padding "10px 20px"
                                    :background "#007bff"
                                    :color "white"
                                    :border "none"
                                    :border-radius "4px"
                                    :cursor "pointer"
                                    :font-size "14px"
                                    :margin-bottom "20px"}})
                (dom/text "Extract Text from Page " current-pdf-page)
                (let [click-event (dom/On "click" identity nil)
                      [?token ?error] (e/Token click-event)]

                  (dom/props {:disabled (some? ?token)
                              :style {:padding "10px 20px"
                                      :background (if (some? ?token) "#ccc" "#007bff")
                                      :color "white"
                                      :border "none"
                                      :border-radius "4px"
                                      :cursor (if (some? ?token) "not-allowed" "pointer")
                                      :font-size "14px"
                                      :margin-bottom "20px"}})

                  (when ?error
                    (dom/div
                      (dom/props {:style {:color "red" :margin-top "10px"}})
                      (dom/text "Error: " ?error)))

                  (when-some [token ?token]
                    (let [result (e/server (page/extract-page-text selected-doc current-pdf-page))]
                      (if (:success result)
                        (do
                          (e/server (swap! !refresh inc))  ; Trigger refresh to update text display
                          (token))  ; Success - closes token
                        (token (:error result)))))))  ; Error

              ;; Show extracted text
              ;; Electric reactivity: When current-pdf-page, selected-doc, or refresh change, this query
              ;; automatically re-runs and updates the UI - no page refresh needed!
              (dom/h2 (dom/text "Extracted Text"))
              (let [refresh (e/server (e/watch !refresh))  ; Create reactive dependency on refresh atom
                    text-result (e/server (get-page-text* refresh selected-doc current-pdf-page))]
                (if (:success text-result)
                  (let [!edited-html (atom (:text text-result))
                        edited-html (e/watch !edited-html)
                        !save-success (atom nil)
                        save-success (e/watch !save-success)]

                    (dom/div
                      ;; Rich text editor
                      (reset! !edited-html
                        (RichTextEditorComponent {:initial-html (:text text-result)
                                                   :on-change (fn [html] (reset! !edited-html html))}))

                      ;; Save button
                      (dom/button
                        (dom/props {:style {:margin-top "10px"
                                           :padding "10px 20px"
                                           :background "#28a745"
                                           :color "white"
                                           :border "none"
                                           :border-radius "4px"
                                           :cursor "pointer"
                                           :font-size "14px"}})
                        (dom/text "Save Changes")

                        (let [click-event (dom/On "click" identity nil)
                              [?token ?error] (e/Token click-event)]

                          (dom/props {:disabled (some? ?token)
                                      :style {:margin-top "10px"
                                             :padding "10px 20px"
                                             :background (if (some? ?token) "#ccc" "#28a745")
                                             :color "white"
                                             :border "none"
                                             :border-radius "4px"
                                             :cursor (if (some? ?token) "not-allowed" "pointer")
                                             :font-size "14px"}})

                          (when ?error
                            (dom/div
                              (dom/props {:style {:color "red" :margin-top "10px"}})
                              (dom/text "Error: " ?error)))

                          (when-some [token ?token]
                            (let [result (e/server (page/save-page-html-impl
                                                    selected-doc
                                                    current-pdf-page
                                                    edited-html))]
                              (if (:success result)
                                (do
                                  (e/server (swap! !refresh inc))
                                  (reset! !save-success true)
                                  #?(:cljs (js/setTimeout #(reset! !save-success false) 3000))
                                  (token))
                                (token (:error result)))))))

                      ;; Success message
                      (when save-success
                        (dom/div
                          (dom/props {:style {:color "green"
                                             :margin-top "10px"
                                             :padding "10px"
                                             :background "#d4edda"
                                             :border "1px solid #c3e6cb"
                                             :border-radius "4px"}})
                          (dom/text "✓ Changes saved successfully!")))))
                  (dom/p
                    (dom/props {:style {:color "gray"}})
                    (dom/text "No text extracted yet. Click 'Extract Text' to process this page.")))))))))))
