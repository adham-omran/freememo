(ns electric-starter-app.pdf-viewer-component
  "PDF viewer UI component using PDF.js."
  (:require
    [hyperfiddle.electric3 :as e]
    [hyperfiddle.electric-dom3 :as dom]
    [electric-starter-app.pdf-viewer :as viewer]))

(e/defn PdfViewerComponent
  "Renders a PDF viewer for the given document ID.
   Props: {:document-id <int>}"
  [{:keys [document-id]}]
  (e/client
    (let [!page (atom 1)
          !total (atom 0)
          !container (atom nil)
          !viewer-div (atom nil)
          page (e/watch !page)
          total (e/watch !total)]

      (dom/div
        (dom/props {:style {:height "600px"
                            :display "flex"
                            :flex-direction "column"
                            :border "1px solid #ccc"
                            :border-radius "4px"
                            :overflow "hidden"}})

        ;; Toolbar
        (dom/div
          (dom/props {:class "pdf-toolbar"
                      :style {:background "#f5f5f5"
                              :border-bottom "1px solid #ddd"
                              :padding "8px"
                              :display "flex"
                              :align-items "center"
                              :gap "12px"
                              :flex-wrap "wrap"}})

          ;; Page navigation section
          (dom/div
            (dom/props {:style {:display "flex" :align-items "center" :gap "4px"}})

            ;; Previous button
            (dom/button
              (dom/props {:title "Previous Page"
                          :disabled (= page 1)
                          :style {:padding "6px 12px"
                                  :cursor (if (= page 1) "not-allowed" "pointer")
                                  :background (if (= page 1) "#e0e0e0" "#fff")
                                  :border "1px solid #ccc"
                                  :border-radius "3px"}})
              (dom/text "◀")
              (e/for [_ (dom/On-all "click")]
                (when (> page 1)
                  (swap! !page dec)
                  (viewer/go-to-page! @!page))))

            ;; Page number input
            (dom/input
              (dom/props {:type "number"
                          :value (str page)
                          :min "1"
                          :max (str total)
                          :style {:width "60px"
                                  :padding "4px"
                                  :text-align "center"
                                  :border "1px solid #ccc"
                                  :border-radius "3px"}})
              (e/for [e (dom/On-all "change")]
                (let [val (js/parseInt (.. e -target -value))]
                  (when (and (>= val 1) (<= val total))
                    (reset! !page val)
                    (viewer/go-to-page! val)))))

            ;; Page count
            (dom/span
              (dom/props {:style {:color "#666"}})
              (dom/text "of " total))

            ;; Next button
            (dom/button
              (dom/props {:title "Next Page"
                          :disabled (= page total)
                          :style {:padding "6px 12px"
                                  :cursor (if (= page total) "not-allowed" "pointer")
                                  :background (if (= page total) "#e0e0e0" "#fff")
                                  :border "1px solid #ccc"
                                  :border-radius "3px"}})
              (dom/text "▶")
              (e/for [_ (dom/On-all "click")]
                (when (< page total)
                  (swap! !page inc)
                  (viewer/go-to-page! @!page)))))

          ;; Zoom controls section
          (dom/div
            (dom/props {:style {:display "flex"
                                :align-items "center"
                                :gap "4px"
                                :margin-left "20px"}})

            (dom/button
              (dom/props {:title "Zoom Out"
                          :style {:padding "6px 12px"
                                  :cursor "pointer"
                                  :background "#fff"
                                  :border "1px solid #ccc"
                                  :border-radius "3px"}})
              (dom/text "−")
              (e/for [_ (dom/On-all "click")] (viewer/zoom! 0.9)))

            (dom/button
              (dom/props {:title "Zoom In"
                          :style {:padding "6px 12px"
                                  :cursor "pointer"
                                  :background "#fff"
                                  :border "1px solid #ccc"
                                  :border-radius "3px"}})
              (dom/text "+")
              (e/for [_ (dom/On-all "click")] (viewer/zoom! 1.1)))

            (dom/button
              (dom/props {:title "Reset Zoom"
                          :style {:padding "6px 12px"
                                  :cursor "pointer"
                                  :background "#fff"
                                  :border "1px solid #ccc"
                                  :border-radius "3px"}})
              (dom/text "100%")
              (e/for [_ (dom/On-all "click")] (viewer/set-zoom! 1.0)))))

        ;; Viewer wrapper (relative positioning for absolute container inside)
        (dom/div
          (dom/props {:style {:flex "1"
                              :position "relative"}})

          ;; Viewer container (must be absolutely positioned for PDF.js)
          (dom/div
            (dom/props {:class "pdf-viewer-container"
                        :style {:position "absolute"
                                :top "0"
                                :left "0"
                                :right "0"
                                :bottom "0"
                                :overflow "auto"
                                :background "#525659"}})
            (reset! !container dom/node)

            (dom/div
              (dom/props {:class "pdfViewer"})
              (reset! !viewer-div dom/node)

              ;; Initialize PDF.js viewer after DOM elements exist
              (js/setTimeout
                (fn []
                  (let [pdf-url (str "/api/pdf/" document-id)]
                    (viewer/init-viewer!
                      @!container
                      @!viewer-div
                      pdf-url
                      (fn [pdf _]
                        ;; On PDF loaded
                        (reset! !total (.-numPages pdf))
                        ;; Listen for page changes from PDF.js
                        (when-let [{:keys [event-bus]} @viewer/!viewer-state]
                          (.on event-bus "pagechanging"
                               (fn [e] (reset! !page (.-pageNumber e)))))))))
                100))))))))
