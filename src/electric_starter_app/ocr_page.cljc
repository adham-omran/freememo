(ns electric-starter-app.ocr-page
  "OCR text extraction UI component."
  (:require
    [hyperfiddle.electric3 :as e]
    [hyperfiddle.electric-dom3 :as dom]
    [electric-starter-app.pdf-viewer-component :refer [PdfViewerComponent]]
    [electric-starter-app.rich-text-editor-component :refer [RichTextEditorComponent]]
    [electric-starter-app.rich-text-editor :as editor]
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
              (dom/h2 (dom/text "Extracted Text"))
              (let [refresh (e/server (e/watch !refresh))
                    text-result (e/server (get-page-text* refresh selected-doc current-pdf-page))]
                (if (:success text-result)
                  (let [!save-status (atom nil)]

                    (dom/div
                      ;; Rich text editor — no callbacks, pure imperative widget
                      (RichTextEditorComponent {:initial-html (:text text-result)})

                      ;; Save button — reads HTML client-side, POSTs via fetch
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
                        (dom/On "click"
                          (fn [_e]
                            (let [html (editor/get-current-html!)]
                              (when html
                                (let [form-data (js/FormData.)]
                                  (.append form-data "document_id" (str selected-doc))
                                  (.append form-data "page_number" (str current-pdf-page))
                                  (.append form-data "html" html)
                                  (-> (js/fetch "/api/save-page-text"
                                                (clj->js {:method "POST" :body form-data}))
                                      (.then (fn [resp]
                                               (if (.-ok resp)
                                                 (reset! !save-status {:success true})
                                                 (reset! !save-status {:success false :error "Server error"}))))
                                      (.catch (fn [err]
                                                (reset! !save-status {:success false :error (str err)}))))))))
                          nil))

                      ;; Status message
                      (let [save-status (e/watch !save-status)]
                        (when save-status
                          (dom/div
                            (dom/props {:style {:margin-top "10px"
                                               :padding "10px"
                                               :border-radius "4px"
                                               :color (if (:success save-status) "green" "red")
                                               :background (if (:success save-status) "#d4edda" "#f8d7da")
                                               :border (str "1px solid " (if (:success save-status) "#c3e6cb" "#f5c6cb"))}})
                            (dom/text (if (:success save-status)
                                        "Changes saved successfully!"
                                        (str "Error: " (:error save-status)))))))))
                  (dom/p
                    (dom/props {:style {:color "gray"}})
                    (dom/text "No text extracted yet. Click 'Extract Text' to process this page.")))))))))))
