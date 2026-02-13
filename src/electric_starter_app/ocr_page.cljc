(ns electric-starter-app.ocr-page
  "OCR text extraction UI component."
  (:require
    [hyperfiddle.electric3 :as e]
    [hyperfiddle.electric-dom3 :as dom]
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

        ;; Step 2 & 3: Select page and extract (only show if document selected)
        (when selected-doc
          (dom/div
            (dom/h2 (dom/text "2. Enter Page Number"))
            (let [!page-num (atom 1)
                  page-num (e/watch !page-num)]
              (dom/div
                (dom/input
                  (dom/props {:type "number" :min "1" :value page-num :style {:margin-bottom "20px"}})
                  (dom/On "input"
                    (fn [e] (-> e .-target .-value js/parseInt))
                    1))
                (reset! !page-num
                  (dom/On "input"
                    (fn [e] (-> e .-target .-value js/parseInt))
                    1)))

              ;; Step 3: Extract button
              (dom/h2 (dom/text "3. Extract Text"))
              (dom/button
                (dom/text "Extract Text from Page " page-num)
                (let [click-event (dom/On "click" identity nil)
                      [?token ?error] (e/Token click-event)]

                  (dom/props {:disabled (some? ?token)})

                  (when ?error
                    (dom/div
                      (dom/props {:style {:color "red" :margin-top "10px"}})
                      (dom/text "Error: " ?error)))

                  (when-some [token ?token]
                    (let [result (e/server (page/extract-page-text selected-doc page-num))]
                      (if (:success result)
                        (do
                          (e/server (swap! !refresh inc))  ; Trigger refresh to update text display
                          (token))  ; Success - closes token
                        (token (:error result)))))))  ; Error

              ;; Step 4 & 5: Show extracted text
              ;; Electric reactivity: When page-num, selected-doc, or refresh change, this query
              ;; automatically re-runs and updates the UI - no page refresh needed!
              (dom/h2 (dom/text "Extracted Text"))
              (let [refresh (e/server (e/watch !refresh))  ; Create reactive dependency on refresh atom
                    text-result (e/server (get-page-text* refresh selected-doc page-num))]
                (if (:success text-result)
                  (dom/div
                    (dom/props {:style {:white-space "pre-wrap"
                                        :padding "10px"
                                        :border "1px solid #ccc"
                                        :background-color "#f9f9f9"
                                        :max-height "400px"
                                        :overflow-y "auto"}})
                    (dom/text (:text text-result)))
                  (dom/p
                    (dom/props {:style {:color "gray"}})
                    (dom/text "No text extracted yet. Click 'Extract Text' to process this page.")))))))))))
