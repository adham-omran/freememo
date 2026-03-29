(ns freememo.pdf-viewer-component
  "PDF viewer UI component using PDF.js."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.pdf-viewer :as viewer]))

(e/defn PdfViewerComponent
  "Renders a PDF viewer for the given document ID and exposes current page number.
   Props: {:document-id <int>, :initial-page <int>, :on-navigate! <fn>}
   Returns: The current page number (for OCR integration)."
  [{:keys [document-id initial-page on-navigate!]}]
  (e/client
    (let [!page (atom (or initial-page 1))
          !total (atom 0)
          !container (atom nil)
          !viewer-div (atom nil)
          !input-val (atom (str (or initial-page 1)))
          !inp-focused (atom false)
          page (e/watch !page)
          total (e/watch !total)
          input-val (e/watch !input-val)
          inp-focused (e/watch !inp-focused)]

      ;; Sync page → input-val when not typing
      (when (and (not inp-focused) (not= input-val (str page)))
        (reset! !input-val (str page)))

      (dom/div
        (dom/props {:style {:height "100%"
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
                          :disabled (or (= page 1) (= total 0))
                          :style {:padding "6px 12px"
                                  :cursor (if (or (= page 1) (= total 0)) "not-allowed" "pointer")
                                  :background (if (or (= page 1) (= total 0)) "#e0e0e0" "#fff")
                                  :border "1px solid #ccc"
                                  :border-radius "3px"}})
              (dom/text "◀")
              (dom/On "click"
                (fn [_]
                  (let [p @!page t @!total]
                    (when (and (> p 1) (> t 0))
                      (let [new-page (dec p)]
                        (reset! !page new-page)
                        (viewer/go-to-page! new-page)
                        (when on-navigate! (on-navigate! new-page))))))
                nil))

            ;; Typed page input
            (dom/input
              (dom/props {:type "text"
                          :value input-val
                          :style {:width "40px" :text-align "center" :padding "4px"
                                  :border "1px solid #ccc" :border-radius "3px"
                                  :font-size "14px"}})
              (dom/On "focus" (fn [_] (reset! !inp-focused true)) nil)
              (dom/On "blur"
                (fn [e]
                  ;; Capture value before any state changes that might trigger re-render
                  (let [raw (-> e .-target .-value)]
                    (reset! !inp-focused false)
                    (let [n (js/parseInt raw)]
                      (when (and (not (js/isNaN n)) (>= n 1) (<= n @!total))
                        (let [clamped (max 1 (min @!total n))]
                          (reset! !page clamped)
                          (viewer/go-to-page! clamped)
                          (when on-navigate! (on-navigate! clamped)))))))
                nil)
              (dom/On "keydown"
                (fn [e]
                  (when (= "Enter" (.-key e))
                    (.blur (.-target e))))
                nil)
              (let [v (dom/On "input" (fn [e] (-> e .-target .-value)) nil)]
                (when (some? v) (reset! !input-val v))))

            (dom/span
              (dom/props {:style {:color "#333" :padding "0 4px"}})
              (dom/text "of " total))

            ;; Next button
            (dom/button
              (dom/props {:title "Next Page"
                          :disabled (or (>= page total) (= total 0))
                          :style {:padding "6px 12px"
                                  :cursor (if (or (>= page total) (= total 0)) "not-allowed" "pointer")
                                  :background (if (or (>= page total) (= total 0)) "#e0e0e0" "#fff")
                                  :border "1px solid #ccc"
                                  :border-radius "3px"}})
              (dom/text "▶")
              (dom/On "click"
                (fn [_]
                  (let [p @!page t @!total]
                    (when (and (< p t) (> t 0))
                      (let [new-page (inc p)]
                        (reset! !page new-page)
                        (viewer/go-to-page! new-page)
                        (when on-navigate! (on-navigate! new-page))))))
                nil)))

          ;; Zoom controls section
          (dom/div
            (dom/props {:style {:display "flex"
                                :align-items "center"
                                :gap "4px"
                                :margin-left "20px"
                                :padding-left "20px"
                                :border-left "1px solid #ddd"}})

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
            (dom/select
              (dom/props {:style {:padding "6px 8px"
                                  :cursor "pointer"
                                  :background "#fff"
                                  :border "1px solid #ccc"
                                  :border-radius "3px"}})
              (dom/option (dom/props {:value "page-width"}) (dom/text "Page Width"))
              (dom/option (dom/props {:value "page-fit"}) (dom/text "Page Fit"))
              (dom/option (dom/props {:value "0.5"}) (dom/text "50%"))
              (dom/option (dom/props {:value "0.75"}) (dom/text "75%"))
              (dom/option (dom/props {:value "1.0"}) (dom/text "100%"))
              (dom/option (dom/props {:value "1.25"}) (dom/text "125%"))
              (dom/option (dom/props {:value "1.5"}) (dom/text "150%"))
              (dom/option (dom/props {:value "2.0"}) (dom/text "200%"))
              (e/for [[t e] (dom/On-all "change")]
                (when e
                  (let [v (-> e .-target .-value)]
                    (case v
                      "page-width" (viewer/set-zoom-fit!)
                      "page-fit" (viewer/set-zoom-page-fit!)
                      (let [scale (js/parseFloat v)]
                        (when-not (js/isNaN scale)
                          (viewer/set-zoom! scale))))))
                (t)))))

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
            (e/on-unmount
              (fn []
                (js/console.log "[PDF-COMP] unmount")
                (viewer/destroy-viewer!)))

            (dom/div
              (dom/props {:class "pdfViewer"})
              (reset! !viewer-div dom/node)

              ;; Initialize PDF.js viewer after DOM elements exist.
              ;; CRITICAL: document-id and initial-page must ONLY appear inside fn closures.
              ;; Electric treats fn bodies as opaque (no reactive tracking).
              ;;
              ;; It doesn't matter if this setTimeout fires multiple times —
              ;; init-viewer! has its own generation guard that skips stale async
              ;; callbacks. Only the latest init-viewer! call's promise proceeds.
              (js/setTimeout
                (fn []
                  (js/console.log "[PDF-COMP] setTimeout fired")
                  (viewer/init-viewer!
                    @!container
                    @!viewer-div
                    (str "/api/pdf/" document-id)
                    (fn [^js pdf _]
                      (js/console.log "[PDF-COMP] on-ready, numPages=" (.-numPages pdf))
                      (reset! !total (.-numPages pdf))
                      (viewer/on-page-change! (fn [page-num]
                                                (reset! !page page-num)
                                                (when on-navigate! (on-navigate! page-num))))
                      (when (and initial-page (> initial-page 1))
                        (viewer/go-to-page-after-load! initial-page))
                      (viewer/setup-pinch-zoom! @!container))))
                100)))))

      ;; Return current page number for OCR integration
      page)))
