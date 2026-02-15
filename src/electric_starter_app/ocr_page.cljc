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
   #?(:clj [electric-starter-app.page :as page])
   #?(:clj [electric-starter-app.pdf :as pdf])
   #?(:clj [electric-starter-app.cards :as cards])))

#?(:clj (defonce !refresh (atom 0)))  ; Server-side refresh trigger

;; Query wrapper: takes refresh arg to create Electric reactive dependency
#?(:clj (defn get-page-text* [_refresh document-id page-number]
          (page/get-page-text document-id page-number)))

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
                  (let [!use-context (atom false)
                        use-context (e/watch !use-context)
                        !card-type (atom "basic")
                        card-type (e/watch !card-type)
                        !context-window (atom 3)
                        context-window (e/watch !context-window)
                        !card-count (atom 20)
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
                          (reset! !use-context
                            (dom/On "change" (fn [e] (-> e .-target .-checked)) false)))
                        (dom/text "Context"))
                      (dom/input
                        (dom/props {:type "number" :min "1" :max "10" :value (str context-window)
                                    :disabled (not use-context)
                                    :style {:padding "2px 4px" :font-size "13px" :width "40px"
                                            :opacity (if use-context "1" "0.5")}})
                        (reset! !context-window
                          (dom/On "input"
                            (fn [e] (let [v (-> e .-target .-value)]
                                      (if (seq v) (js/parseInt v) 3)))
                            3)))
                      (dom/span (dom/props {:style {:font-size "13px" :color "#666"}}) (dom/text "pg"))

                      ;; Separator
                      (dom/span (dom/props {:style {:color "#ccc"}}) (dom/text "|"))

                      ;; Card type radios
                      (dom/label
                        (dom/props {:style {:display "flex" :align-items "center" :gap "4px" :font-size "13px"}})
                        (dom/input
                          (dom/props {:type "radio" :name "card-type" :value "basic"
                                      :checked (= card-type "basic")})
                          (dom/On "change" (fn [_] (reset! !card-type "basic")) nil))
                        (dom/text "Basic"))
                      (dom/label
                        (dom/props {:style {:display "flex" :align-items "center" :gap "4px" :font-size "13px"}})
                        (dom/input
                          (dom/props {:type "radio" :name "card-type" :value "cloze"
                                      :checked (= card-type "cloze")})
                          (dom/On "change" (fn [_] (reset! !card-type "cloze")) nil))
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
                          (reset! !card-count
                            (dom/On "input"
                              (fn [e] (let [v (-> e .-target .-value)]
                                        (if (seq v) (js/parseInt v) 20)))
                              20))))

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
                                    (token (:error save-result))))))))))))

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
