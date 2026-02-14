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
          ;; Side-by-side layout: PDF viewer (left) + editor panel (right)
          (dom/div
            (dom/props {:style {:display "flex" :gap "16px" :align-items "stretch"}})

            ;; Left: PDF viewer
            (let [current-pdf-page
                  (dom/div
                    (dom/props {:style {:flex "1" :min-width "0"}})
                    (PdfViewerComponent {:document-id selected-doc}))]

              ;; Right: extract button + editor + save status
              (dom/div
                (dom/props {:style {:flex "1" :display "flex" :flex-direction "column" :min-width "0"}})

                ;; Page header + extract button
                (dom/div
                  (dom/props {:style {:display "flex" :align-items "center" :gap "12px" :margin-bottom "12px"}})
                  (dom/span
                    (dom/props {:style {:font-weight "bold" :font-size "16px"}})
                    (dom/text "Page " current-pdf-page))
                  (dom/button
                    (dom/props {:style {:padding "8px 16px"
                                        :background "#007bff"
                                        :color "white"
                                        :border "none"
                                        :border-radius "4px"
                                        :cursor "pointer"
                                        :font-size "14px"}})
                    (dom/text "Extract Text")
                    (let [click-event (dom/On "click" identity nil)
                          [?token ?error] (e/Token click-event)]

                      (dom/props {:disabled (some? ?token)
                                  :style {:padding "8px 16px"
                                          :background (if (some? ?token) "#ccc" "#007bff")
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
                        (let [result (e/server (page/extract-page-text selected-doc current-pdf-page))]
                          (if (:success result)
                            (do
                              (e/server (swap! !refresh inc))
                              (token))
                            (token (:error result))))))))

                ;; Editor + save status
                (let [refresh (e/server (e/watch !refresh))
                      text-result (e/server (get-page-text* refresh selected-doc current-pdf-page))]
                  (if (:success text-result)
                    (dom/div
                      (dom/props {:style {:flex "1" :display "flex" :flex-direction "column" :min-height "0"}})
                      (RichTextEditorComponent {:initial-html (:text text-result)
                                                :page-number current-pdf-page
                                                :doc-id selected-doc})

                      ;; Auto-save: use stored page/doc from dirty-html map, not current reactive values
                      (let [dirty-data (e/watch editor/!dirty-html)]
                        (when (some? dirty-data)
                          (let [result (e/server
                                         (e/Offload
                                           #(page/save-page-html-impl
                                              (:doc-id dirty-data)
                                              (:page dirty-data)
                                              (:html dirty-data))))]
                            (dom/div
                              (dom/props {:style {:margin-top "6px"
                                                 :font-size "12px"
                                                 :color (if (:success result) "#888" "red")}})
                              (dom/text (if (:success result) "Saved" (str "Save error: " (:error result)))))))))
                    (dom/p
                      (dom/props {:style {:color "gray"}})
                      (dom/text "No text extracted yet. Click 'Extract Text' to process this page."))))))))))))
