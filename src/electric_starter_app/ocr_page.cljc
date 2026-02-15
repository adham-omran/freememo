(ns electric-starter-app.ocr-page
  "OCR text extraction UI component."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.electric-scroll0 :refer [Scroll-window]]
   [contrib.data :refer [clamp-left]]
   [electric-starter-app.pdf-viewer-component :refer [PdfViewerComponent]]
   [electric-starter-app.rich-text-editor-component :refer [RichTextEditorComponent]]
   [electric-starter-app.rich-text-editor :as editor]
   [clojure.string :as str]
   #?(:clj [electric-starter-app.page :as page])
   #?(:clj [electric-starter-app.pdf :as pdf])
   #?(:clj [electric-starter-app.cards :as cards])
   #?(:clj [electric-starter-app.settings :as settings])))

#?(:clj (defonce !refresh (atom 0)))  ; Server-side refresh trigger

;; Query wrapper: takes refresh arg to create Electric reactive dependency
#?(:clj (defn get-page-text* [_refresh document-id page-number]
          (page/get-page-text document-id page-number)))

;; Browser download helper (ClojureScript only)
#?(:cljs
   (defn trigger-download! [filename content]
     (let [blob (js/Blob. #js [content] #js {:type "text/csv"})
           url (.createObjectURL js/URL blob)
           a (.createElement js/document "a")]
       (set! (.-href a) url)
       (set! (.-download a) filename)
       (.click a)
       (.revokeObjectURL js/URL url))))

;; Query wrapper for cards
#?(:clj (defn get-cards* [_refresh document-id page-number]
          (cards/get-cards document-id page-number)))

;; Drag helper for split-pane dividers
(defn start-drag!
  "Begin a split-pane drag. Call from a mousedown handler.
   axis: :x for horizontal, :y for vertical."
  [e axis !pct]
  #?(:clj nil
     :cljs
     (do
       (.preventDefault e)
       (let [horizontal? (= axis :x)
             start-pos   (if horizontal? (.-clientX e) (.-clientY e))
             start-pct   @!pct
             parent      (-> e .-target .-parentElement)
             parent-size (if horizontal? (.-offsetWidth parent) (.-offsetHeight parent))
             on-move     (fn [me]
                           (let [delta     (- (if horizontal? (.-clientX me) (.-clientY me)) start-pos)
                                 delta-pct (* (/ delta parent-size) 100)
                                 new-pct   (-> (+ start-pct delta-pct) (max 15) (min 85))]
                             (reset! !pct new-pct)))
             on-up       (atom nil)]
         (reset! on-up
           (fn [_]
             (.removeEventListener js/document "mousemove" on-move)
             (.removeEventListener js/document "mouseup" @on-up)))
         (.addEventListener js/document "mousemove" on-move)
         (.addEventListener js/document "mouseup" @on-up)))))

;; Card table row component
(e/defn CardRow [card]
  (e/client
    (let [id (e/server (:flashcards/id card))
          kind (e/server (:flashcards/kind card))
          question (e/server (:flashcards/question card))
          answer (e/server (:flashcards/answer card))
          cloze (e/server (:flashcards/cloze card))]
      (dom/tr
        ;; Front column
        (dom/td
          (dom/props {:style {:padding "6px 8px" :overflow "hidden" :text-overflow "ellipsis" :white-space "nowrap" :max-width "0"
                              :border-bottom "1px solid #e0e0e0"}})
          (dom/text (if (= kind "basic") question cloze)))
        ;; Back column
        (dom/td
          (dom/props {:style {:padding "6px 8px" :overflow "hidden" :text-overflow "ellipsis" :white-space "nowrap" :max-width "0"
                              :border-bottom "1px solid #e0e0e0"}})
          (dom/text (if (= kind "basic") (or answer "") "")))
        ;; Kind column
        (dom/td
          (dom/props {:style {:padding "6px 8px" :width "60px" :color "#666" :font-size "12px"
                              :border-bottom "1px solid #e0e0e0"}})
          (dom/text kind))
        ;; Delete column
        (dom/td
          (dom/props {:style {:padding "6px 4px" :width "40px" :text-align "center"
                              :border-bottom "1px solid #e0e0e0"}})
          (dom/button
            (dom/props {:style {:padding "2px 6px"
                                :background "#dc3545"
                                :color "white"
                                :border "none"
                                :border-radius "3px"
                                :cursor "pointer"
                                :font-size "12px"
                                :line-height "1"}})
            (dom/text "\u00D7")
            (let [click-event (dom/On "click" identity nil)
                  [?token ?error] (e/Token click-event)]

              (dom/props {:disabled (some? ?token)
                          :style {:padding "2px 6px"
                                  :background (if (some? ?token) "#999" "#dc3545")
                                  :color "white"
                                  :border "none"
                                  :border-radius "3px"
                                  :cursor (if (some? ?token) "not-allowed" "pointer")
                                  :font-size "12px"
                                  :line-height "1"}})

              (when ?error
                (dom/div
                  (dom/props {:style {:color "red" :font-size "11px"}})
                  (dom/text ?error)))

              (when-some [token ?token]
                (let [result (e/server (cards/delete-card id))]
                  (if (:success result)
                    (do
                      (e/server (swap! !refresh inc))
                      (token))
                    (token (:error result))))))))))))

(e/defn OcrPage []
  (e/client
    (dom/div
      (dom/props {:style {:height "100%" :display "flex" :flex-direction "column" :overflow "hidden"}})
      (let [docs-result (e/server (pdf/list-pdfs))
            !selected-doc (atom nil)
            selected-doc (e/watch !selected-doc)]

        (cond
          ;; Success with documents
          (and (:success docs-result) (seq (:documents docs-result)))
          (dom/div
            (dom/props {:style {:flex-shrink "0" :padding "8px 0"}})
            (dom/select
              (dom/props {:style {:margin-bottom "0"}})
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
              (reset! !selected-doc
                (dom/On "change"
                  (fn [e]
                    (let [val (-> e .-target .-value)]
                      (if (seq val)
                        (js/parseInt val)
                        nil)))
                  nil))))

          ;; Success but no documents
          (:success docs-result)
          (dom/p (dom/text "No documents available. Please upload a PDF first."))

          ;; Error case
          :else
          (dom/p
            (dom/props {:style {:color "red"}})
            (dom/text "Error loading documents: " (or (:error docs-result) "Unknown error"))))

        ;; Main content (shows when document is selected)
        (when selected-doc
          (let [!top-pct (atom 60)
                top-pct (e/watch !top-pct)
                !left-pct (atom 50)
                left-pct (e/watch !left-pct)
                ;; Bridge current-pdf-page via atom so it's available at outer scope
                !current-page (atom 1)
                current-pdf-page (e/watch !current-page)
                ;; Shared server data — hoisted so both editor and bottom panel can use them
                dirty-data (e/watch editor/!dirty-html)
                save-result (when (some? dirty-data)
                              (e/server
                                (e/Offload
                                  #(page/save-page-html-impl
                                     (:doc-id dirty-data)
                                     (:page dirty-data)
                                     (:html dirty-data)))))
                refresh (e/server (e/watch !refresh))
                text-result (e/server (get-page-text* refresh selected-doc current-pdf-page))
                page-text (e/server (when (:success text-result) (:text text-result)))]

            ;; Outer: vertical flex (top row / bottom panel)
            (dom/div
              (dom/props {:style {:flex "1" :display "flex" :flex-direction "column" :min-height "0" :overflow "hidden"}})

              ;; TOP ROW: PDF | Editor (horizontal flex)
              (dom/div
                (dom/props {:style {:height (str top-pct "%") :display "flex" :min-height "0" :overflow "hidden"}})

                ;; LEFT: PDF viewer
                (dom/div
                  (dom/props {:style {:width (str left-pct "%") :min-width "0" :overflow "hidden"}})
                  (reset! !current-page (PdfViewerComponent {:document-id selected-doc})))

                ;; Horizontal drag handle
                (dom/div
                  (dom/props {:class "split-divider-h"})
                  (dom/On "mousedown" (fn [e] (start-drag! e :x !left-pct)) nil))

                ;; RIGHT: Editor
                (dom/div
                  (dom/props {:style {:flex "1" :display "flex" :flex-direction "column" :min-width "0" :min-height "0" :overflow "hidden"}})

                  ;; Page header + extract button
                  (dom/div
                    (dom/props {:style {:display "flex" :align-items "center" :gap "12px" :padding "8px" :flex-shrink "0"}})
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
                              (token (:error result)))))))

                    ;; Save status indicator with fade-out
                    (when (some? dirty-data)
                      (let [is-success (:success save-result)
                            message (if is-success "Saved" (str "Save error: " (:error save-result)))
                            !show (atom true)
                            show (e/watch !show)]
                        (dom/span
                          (dom/props {:style {:margin-left "12px"
                                              :font-size "12px"
                                              :color (if is-success "#888" "red")
                                              :opacity (if show "1" "0")
                                              :transition "opacity 0.5s ease-out"}})
                          (dom/text message)
                          ;; Fade out after 2 seconds (only for successful saves)
                          (when is-success
                            (js/setTimeout
                              (fn [] (reset! !show false))
                              2000))))))

                  ;; Editor area
                  (dom/div
                    (dom/props {:style {:flex "1" :min-height "0" :overflow "hidden"}})
                    (if (:success text-result)
                      (dom/div
                        (dom/props {:style {:height "100%" :display "flex" :flex-direction "column" :overflow "hidden"}})
                        (RichTextEditorComponent {:initial-html (:text text-result)
                                                  :page-number current-pdf-page
                                                  :doc-id selected-doc}))
                      (dom/p
                        (dom/props {:style {:color "gray"}})
                        (dom/text "No text extracted yet. Click 'Extract Text' to process this page."))))))

              ;; Vertical drag handle (full width)
              (dom/div
                (dom/props {:class "split-divider-v"})
                (dom/On "mousedown" (fn [e] (start-drag! e :y !top-pct)) nil))

              ;; BOTTOM PANEL: Options toolbar + Cards table (full width)
              (dom/div
                (dom/props {:style {:flex "1" :display "flex" :flex-direction "column" :min-height "0" :overflow "hidden"}})

                ;; Generation toolbar (compact single row)
                (when page-text
                  (let [;; Load settings from server
                        server-context-enabled (e/server (settings/get-context-enabled))
                        server-context-pages (e/server (settings/get-context-pages))
                        server-card-type (e/server (settings/get-card-type))
                        server-card-count (e/server (settings/get-card-count))

                        ;; Initialize atoms with server values
                        !use-context (atom server-context-enabled)
                        use-context (e/watch !use-context)
                        !context-window (atom server-context-pages)
                        context-window (e/watch !context-window)
                        !card-type (atom server-card-type)
                        card-type (e/watch !card-type)
                        !card-count (atom server-card-count)
                        card-count-val (e/watch !card-count)]

                    (dom/div
                      (dom/props {:style {:display "flex" :align-items "center" :gap "12px"
                                          :padding "8px 12px" :flex-shrink "0"
                                          :border-bottom "1px solid #e0e0e0" :background "#fafafa"}})

                      ;; Context checkbox + pages
                      (dom/label
                        (dom/props {:style {:display "flex" :align-items "center" :gap "4px" :font-size "13px"}})
                        (dom/input
                          (dom/props {:type "checkbox" :checked use-context})
                          (let [change-event (dom/On "change" (fn [e] (-> e .-target .-checked)) nil)
                                [?token ?error] (e/Token change-event)]
                            (when (some? change-event)  ; Only reset when event actually fires
                              (reset! !use-context change-event))
                            (when-some [token ?token]
                              (e/server (settings/save-context-enabled change-event))
                              (token))))
                        (dom/text "Context"))
                      (dom/input
                        (dom/props {:type "number" :min "1" :max "10" :value (str context-window)
                                    :disabled (not use-context)
                                    :style {:padding "2px 4px" :font-size "13px" :width "40px"
                                            :opacity (if use-context "1" "0.5")}})
                        (let [input-event (dom/On "change"  ; Use "change" instead of "input" - fires on blur/enter
                                            (fn [e] (let [v (-> e .-target .-value)]
                                                      (if (seq v) (js/parseInt v) nil)))
                                            nil)
                              [?token ?error] (e/Token input-event)]
                          (when (some? input-event)  ; Only reset when event actually fires
                            (reset! !context-window input-event))
                          (when-some [token ?token]
                            (e/server (settings/save-context-pages input-event))
                            (token))))
                      (dom/span (dom/props {:style {:font-size "13px" :color "#666"}}) (dom/text "pg"))

                      ;; Separator
                      (dom/span (dom/props {:style {:color "#ccc"}}) (dom/text "|"))

                      ;; Card type radios
                      (dom/label
                        (dom/props {:style {:display "flex" :align-items "center" :gap "4px" :font-size "13px"}})
                        (dom/input
                          (dom/props {:type "radio" :name "card-type" :value "basic"
                                      :checked (= card-type "basic")})
                          (let [change-event (dom/On "change" (fn [_] "basic") nil)
                                [?token ?error] (e/Token change-event)]
                            (when (some? change-event)  ; Only reset when event actually fires
                              (reset! !card-type change-event))
                            (when-some [token ?token]
                              (e/server (settings/save-card-type "basic"))
                              (token))))
                        (dom/text "Basic"))
                      (dom/label
                        (dom/props {:style {:display "flex" :align-items "center" :gap "4px" :font-size "13px"}})
                        (dom/input
                          (dom/props {:type "radio" :name "card-type" :value "cloze"
                                      :checked (= card-type "cloze")})
                          (let [change-event (dom/On "change" (fn [_] "cloze") nil)
                                [?token ?error] (e/Token change-event)]
                            (when (some? change-event)  ; Only reset when event actually fires
                              (reset! !card-type change-event))
                            (when-some [token ?token]
                              (e/server (settings/save-card-type "cloze"))
                              (token))))
                        (dom/text "Cloze"))

                      ;; Separator
                      (dom/span (dom/props {:style {:color "#ccc"}}) (dom/text "|"))

                      ;; Card count
                      (dom/span
                        (dom/props {:style {:display "flex" :align-items "center" :gap "4px" :font-size "13px"}})
                        (dom/text "#")
                        (dom/input
                          (dom/props {:type "number" :min "1" :max "50" :value (str card-count-val)
                                      :style {:padding "2px 4px" :font-size "13px" :width "50px"}})
                          (let [input-event (dom/On "change"  ; Use "change" instead of "input" - fires on blur/enter
                                              (fn [e] (let [v (-> e .-target .-value)]
                                                        (if (seq v) (js/parseInt v) nil)))
                                              nil)
                                [?token ?error] (e/Token input-event)]
                            (when (some? input-event)  ; Only reset when event actually fires
                              (reset! !card-count input-event))
                            (when-some [token ?token]
                              (e/server (settings/save-card-count input-event))
                              (token)))))

                      ;; Generate button
                      (dom/button
                        (dom/props {:style {:padding "4px 12px"
                                            :background "#28a745"
                                            :color "white"
                                            :border "none"
                                            :border-radius "4px"
                                            :cursor "pointer"
                                            :font-size "13px"
                                            :font-weight "bold"
                                            :margin-left "auto"}})
                        (dom/text "Generate")
                        (let [click-event (dom/On "click" identity nil)
                              [?token ?error] (e/Token click-event)]

                          (dom/props {:disabled (some? ?token)
                                      :style {:padding "4px 12px"
                                              :background (if (some? ?token) "#999" "#28a745")
                                              :color "white"
                                              :border "none"
                                              :border-radius "4px"
                                              :cursor (if (some? ?token) "not-allowed" "pointer")
                                              :font-size "13px"
                                              :font-weight "bold"
                                              :margin-left "auto"}})

                          (when ?token
                            (dom/text " ..."))

                          (when ?error
                            (dom/div
                              (dom/props {:style {:color "red" :font-size "12px"}})
                              (dom/text "Error: " ?error)))

                          (when-some [token ?token]
                            (let [selected-text (editor/get-selected-text!)
                                  content (or selected-text page-text)
                                  context-text (when use-context
                                                 (e/server (cards/get-context-pages selected-doc current-pdf-page context-window)))
                                  generate-result (e/server
                                                    (if (= card-type "basic")
                                                      (cards/generate-basic-cards
                                                        {:content content
                                                         :context context-text
                                                         :card-count card-count-val})
                                                      (cards/generate-cloze-cards
                                                        {:content content
                                                         :context context-text
                                                         :card-count card-count-val})))]

                              (if-not (:success generate-result)
                                (token (:error generate-result))
                                (let [generated-cards (e/server (:cards generate-result))
                                      save-result (e/server (cards/save-cards selected-doc current-pdf-page card-type generated-cards))]
                                  (if (:success save-result)
                                    (do
                                      (e/server (swap! !refresh inc))
                                      (token))
                                    (token (:error save-result))))))))))

                      ;; Separator
                      (dom/span (dom/props {:style {:color "#ccc"}}) (dom/text "|"))

                      ;; Export button
                      (let [!show-export (atom false)
                            show-export (e/watch !show-export)]
                        (dom/button
                          (dom/props {:style {:padding "4px 12px" :background "#6c757d" :color "white" :border "none"
                                              :border-radius "4px" :cursor "pointer" :font-size "13px" :font-weight "500"}})
                          (dom/text "Export...")
                          (dom/On "click" (fn [_] (reset! !show-export true)) nil))

                        ;; Export modal
                        (when show-export
                          (let [!export-scope (atom "Current Page")
                                export-scope (e/watch !export-scope)
                                !export-kind (atom "Both")
                                export-kind (e/watch !export-kind)
                                !use-header (atom false)
                                use-header (e/watch !use-header)
                                !header-text (atom "")
                                header-text (e/watch !header-text)]

                            (dom/div
                              (dom/props {:style {:position "fixed" :top "0" :left "0" :width "100%" :height "100%"
                                                  :background "rgba(0,0,0,0.5)" :display "flex" :align-items "center"
                                                  :justify-content "center" :z-index "1000"}})
                              (dom/On "click" (fn [_] (reset! !show-export false)) nil)

                              (dom/div
                                (dom/props {:style {:background "white" :border-radius "8px" :padding "24px"
                                                    :width "400px" :box-shadow "0 4px 6px rgba(0,0,0,0.1)"}})
                                (dom/On "click" (fn [e] (.stopPropagation e)) nil)

                                (dom/h3 (dom/props {:style {:margin-top "0" :margin-bottom "20px"}})
                                  (dom/text "Export Cards"))

                                (dom/div
                                  (dom/props {:style {:margin-bottom "16px"}})
                                  (dom/label
                                    (dom/props {:style {:display "flex" :align-items "center" :gap "8px" :margin-bottom "8px"}})
                                    (dom/input (dom/props {:type "checkbox" :checked use-header})
                                      (reset! !use-header (dom/On "change" (fn [e] (-> e .-target .-checked)) false)))
                                    (dom/text "Add custom header to each card"))
                                  (when use-header
                                    (dom/input
                                      (dom/props {:type "text" :value header-text :placeholder "e.g., Chapter 5: Accounting"
                                                  :style {:width "100%" :padding "8px" :border "1px solid #ccc"
                                                          :border-radius "4px" :font-size "14px"}})
                                      (reset! !header-text (dom/On "input" (fn [e] (-> e .-target .-value)) "")))))

                                (dom/div
                                  (dom/props {:style {:margin-bottom "16px"}})
                                  (dom/label (dom/props {:style {:display "block" :margin-bottom "4px" :font-weight "500"}})
                                    (dom/text "Scope:"))
                                  (dom/select
                                    (dom/props {:style {:width "100%" :padding "8px" :border "1px solid #ccc"
                                                        :border-radius "4px" :font-size "14px"}})
                                    (dom/option (dom/props {:value "Current Page"}) (dom/text "Current Page"))
                                    (dom/option (dom/props {:value "Entire Doc"}) (dom/text "Entire Document"))
                                    (reset! !export-scope (dom/On "change" (fn [e] (-> e .-target .-value)) "Current Page"))))

                                (dom/div
                                  (dom/props {:style {:margin-bottom "24px"}})
                                  (dom/label (dom/props {:style {:display "block" :margin-bottom "4px" :font-weight "500"}})
                                    (dom/text "Card Type:"))
                                  (dom/select
                                    (dom/props {:style {:width "100%" :padding "8px" :border "1px solid #ccc"
                                                        :border-radius "4px" :font-size "14px"}})
                                    (dom/option (dom/props {:value "Both"}) (dom/text "Both (Basic + Cloze)"))
                                    (dom/option (dom/props {:value "Basic"}) (dom/text "Basic Only"))
                                    (dom/option (dom/props {:value "Cloze"}) (dom/text "Cloze Only"))
                                    (reset! !export-kind (dom/On "change" (fn [e] (-> e .-target .-value)) "Both"))))

                                (dom/div
                                  (dom/props {:style {:display "flex" :justify-content "flex-end" :gap "12px"}})
                                  (dom/button
                                    (dom/props {:style {:padding "8px 16px" :background "#f8f9fa" :color "#333"
                                                        :border "1px solid #ccc" :border-radius "4px" :cursor "pointer" :font-size "14px"}})
                                    (dom/text "Cancel")
                                    (dom/On "click" (fn [_] (reset! !show-export false)) nil))

                                  (dom/button
                                    (dom/props {:style {:padding "8px 16px" :background "#28a745" :color "white" :border "none"
                                                        :border-radius "4px" :cursor "pointer" :font-size "14px" :font-weight "500"}})
                                    (dom/text "Export")
                                    (let [click-event (dom/On "click" identity nil)
                                          [?token ?error] (e/Token click-event)]
                                      (dom/props {:disabled (some? ?token)
                                                  :style {:padding "8px 16px"
                                                          :background (if (some? ?token) "#999" "#28a745")
                                                          :color "white" :border "none" :border-radius "4px"
                                                          :cursor (if (some? ?token) "not-allowed" "pointer")
                                                          :font-size "14px" :font-weight "500"}})
                                      (when ?error
                                        (dom/div (dom/props {:style {:color "red" :font-size "12px" :margin-top "8px"}})
                                          (dom/text "Error: " ?error)))
                                      (when-some [token ?token]
                                        (if (= export-kind "Both")
                                          (let [basic-result (e/server
                                                              (cards/export-cards-csv
                                                                {:document-id selected-doc
                                                                 :page-number (when (= export-scope "Current Page") current-pdf-page)
                                                                 :kind "basic"
                                                                 :header-text (when use-header header-text)}))
                                                cloze-result (e/server
                                                              (cards/export-cards-csv
                                                                {:document-id selected-doc
                                                                 :page-number (when (= export-scope "Current Page") current-pdf-page)
                                                                 :kind "cloze"
                                                                 :header-text (when use-header header-text)}))]
                                            (if (and (:success basic-result) (:success cloze-result))
                                              (do
                                                (trigger-download! (:filename basic-result) (:csv basic-result))
                                                (trigger-download! (:filename cloze-result) (:csv cloze-result))
                                                (reset! !show-export false)
                                                (token))
                                              (token (str "Export failed: " (or (:error basic-result) (:error cloze-result))))))
                                          (let [export-result (e/server
                                                                (cards/export-cards-csv
                                                                  {:document-id selected-doc
                                                                   :page-number (when (= export-scope "Current Page") current-pdf-page)
                                                                   :kind (str/lower-case export-kind)
                                                                   :header-text (when use-header header-text)}))]
                                            (if (:success export-result)
                                              (do
                                                (trigger-download! (:filename export-result) (:csv export-result))
                                                (reset! !show-export false)
                                                (token))
                                              (token (:error export-result))))))))))))))))

                ;; Cards table with virtual scroll
                (let [cards-result (e/server (get-cards* refresh selected-doc current-pdf-page))]
                  (if (:success cards-result)
                    (if (seq (:cards cards-result))
                      (let [cards-vec (e/server (vec (:cards cards-result)))
                            card-count (e/server (count cards-vec))
                            row-height 36]
                        ;; Scroll viewport
                        (dom/div
                          (dom/props {:style {:flex "1" :overflow-y "auto" :min-height "0"}})
                          (let [[offset limit] (Scroll-window row-height card-count dom/node {:overquery-factor 1})
                                occluded-height (clamp-left (* row-height (- card-count limit)) 0)]
                            ;; Cards table
                            (dom/table
                              (dom/props {:style {:width "100%"
                                                  :border-collapse "separate"
                                                  :border-spacing "0"
                                                  :table-layout "fixed"
                                                  :font-size "13px"}})
                              ;; Virtual-scrolled body
                              (dom/tbody
                                (dom/props {:style {:position "relative"
                                                    :top (str (* offset row-height) "px")}})
                                (e/for [i (e/diff-by {} (range offset (+ offset limit)))]
                                  (let [card (e/server (nth cards-vec i nil))]
                                    (when card
                                      (CardRow card))))))
                            ;; Spacer for full scroll height
                            (dom/div (dom/props {:style {:height (str occluded-height "px")}})))))
                      (dom/p
                        (dom/props {:style {:color "gray" :font-size "13px" :padding "8px 12px"}})
                        (dom/text "No cards generated yet.")))
                    (dom/div
                      (dom/props {:style {:color "red" :font-size "13px" :padding "8px 12px"}})
                      (dom/text "Error loading cards: " (:error cards-result)))))))))))))
