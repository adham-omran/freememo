(ns electric-starter-app.ocr-page
  "OCR text extraction UI component."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
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
#?(:cljs
   (defn make-draggable!
     "Attach mousedown listener to divider-el. On drag, compute delta as % of
      parent container size along axis (:x or :y) and reset! !pct, clamped to [15,85]."
     [divider-el axis !pct]
     (let [horizontal? (= axis :x)]
       (.addEventListener divider-el "mousedown"
         (fn [e]
           (.preventDefault e)
           (let [start-pos    (if horizontal? (.-clientX e) (.-clientY e))
                 start-pct    @!pct
                 parent       (.-parentElement divider-el)
                 parent-size  (if horizontal?
                                (.-offsetWidth parent)
                                (.-offsetHeight parent))
                 on-move      (fn [me]
                                (let [delta     (- (if horizontal? (.-clientX me) (.-clientY me)) start-pos)
                                      delta-pct (* (/ delta parent-size) 100)
                                      new-pct   (-> (+ start-pct delta-pct) (max 15) (min 85))]
                                  (reset! !pct new-pct)))
                 on-up        (atom nil)]
             (reset! on-up
               (fn [_]
                 (.removeEventListener js/document "mousemove" on-move)
                 (.removeEventListener js/document "mouseup" @on-up)))
             (.addEventListener js/document "mousemove" on-move)
             (.addEventListener js/document "mouseup" @on-up)))))))

;; Card display component
(e/defn CardItem [card]
  (e/client
    (let [id (e/server (:flashcards/id card))
          kind (e/server (:flashcards/kind card))
          question (e/server (:flashcards/question card))
          answer (e/server (:flashcards/answer card))
          cloze (e/server (:flashcards/cloze card))]
      (dom/div
        (dom/props {:style {:border "1px solid #ddd"
                            :border-radius "8px"
                            :padding "12px"
                            :margin-bottom "12px"
                            :background "#f9f9f9"}})
        ;; Card content
        (if (= kind "basic")
          (dom/div
            (dom/div
              (dom/props {:style {:font-weight "bold" :margin-bottom "8px"}})
              (dom/text "Q: " question))
            (dom/div
              (dom/props {:style {:margin-bottom "8px"}})
              (dom/text "A: " answer)))
          (dom/div
            (dom/props {:style {:font-family "monospace" :margin-bottom "8px"}})
            (dom/text cloze)))

        ;; Delete button
        (dom/button
          (dom/props {:style {:padding "4px 8px"
                              :background "#dc3545"
                              :color "white"
                              :border "none"
                              :border-radius "4px"
                              :cursor "pointer"
                              :font-size "12px"}})
          (dom/text "Delete")
          (let [click-event (dom/On "click" identity nil)
                [?token ?error] (e/Token click-event)]

            (dom/props {:disabled (some? ?token)
                        :style {:padding "4px 8px"
                                :background (if (some? ?token) "#999" "#dc3545")
                                :color "white"
                                :border "none"
                                :border-radius "4px"
                                :cursor (if (some? ?token) "not-allowed" "pointer")
                                :font-size "12px"}})

            (when ?error
              (dom/div
                (dom/props {:style {:color "red" :margin-top "4px" :font-size "12px"}})
                (dom/text "Error: " ?error)))

            (when-some [token ?token]
              (let [result (e/server (cards/delete-card id))]
                (if (:success result)
                  (do
                    (e/server (swap! !refresh inc))
                    (token))
                  (token (:error result)))))))))))

(e/defn OcrPage []
  (e/client
    (dom/div
      (dom/props {:style {:height "100%" :display "flex" :flex-direction "column" :overflow "hidden"}})
      (let [docs-result (e/server (pdf/list-pdfs))
            !selected-doc (atom nil)
            selected-doc (e/watch !selected-doc)]

        (when (:success docs-result)
          (if (seq (:documents docs-result))
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
                    nil)))
            (dom/p (dom/text "No documents available. Please upload a PDF first."))))

        ;; PDF Viewer and OCR extraction (shows when document is selected)
        (when selected-doc
          ;; State atoms for split pane percentages
          (let [!left-pct (atom 50)
                left-pct (e/watch !left-pct)
                !top-pct (atom 50)
                top-pct (e/watch !top-pct)
                !mid-pct (atom 25)
                mid-pct (e/watch !mid-pct)]

            ;; Horizontal split container
            (dom/div
              (dom/props {:style {:flex "1" :display "flex" :min-height "0" :overflow "hidden"}})

              ;; LEFT: PDF viewer
              (let [current-pdf-page
                    (dom/div
                      (dom/props {:style {:width (str left-pct "%") :min-width "0" :overflow "hidden"}})
                      (PdfViewerComponent {:document-id selected-doc}))]

                ;; Horizontal drag handle
                (dom/div
                  (dom/props {:class "split-divider-h"})
                  (let [divider-el dom/node]
                    (js/setTimeout #(make-draggable! divider-el :x !left-pct) 50)))

                ;; RIGHT: 3-panel stack
                (dom/div
                  (dom/props {:style {:flex "1" :display "flex" :flex-direction "column" :min-width "0" :min-height "0" :overflow "hidden"}})

                  ;; Hoist all shared bindings so panels are siblings
                  (let [dirty-data (e/watch editor/!dirty-html)
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

                    ;; RIGHT-TOP: header + editor
                    (dom/div
                      (dom/props {:style {:height (str top-pct "%") :display "flex" :flex-direction "column" :min-height "0" :overflow "hidden"}})

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
                            (dom/text "No text extracted yet. Click 'Extract Text' to process this page."))))) ;; end RIGHT-TOP

                    ;; Vertical drag handle 1
                    (dom/div
                      (dom/props {:class "split-divider-v"})
                      (let [divider-el dom/node]
                        (js/setTimeout #(make-draggable! divider-el :y !top-pct) 50)))

                    ;; RIGHT-MID: card options
                    (when (:success text-result)
                      (let [!input-mode (atom "full")
                            input-mode (e/watch !input-mode)
                            !card-type (atom "basic")
                            card-type (e/watch !card-type)
                            !context-window (atom 3)
                            context-window (e/watch !context-window)
                            !card-count (atom 20)
                            card-count (e/watch !card-count)]

                        (dom/div
                          (dom/props {:style {:height (str mid-pct "%") :overflow-y "auto" :padding "12px" :min-height "0"}})
                          (dom/h3 (dom/props {:style {:margin-top "0"}}) (dom/text "Generate Flashcards"))

                          ;; Input mode selection
                          (dom/div
                            (dom/props {:style {:margin-bottom "12px"}})
                            (dom/label
                              (dom/props {:style {:display "block" :margin-bottom "8px" :font-weight "bold"}})
                              (dom/text "Input Mode:"))
                            (dom/div
                              (dom/label
                                (dom/props {:style {:display "block" :margin-bottom "4px"}})
                                (dom/input
                                  (dom/props {:type "radio" :name "input-mode" :value "full"
                                              :checked (= input-mode "full")})
                                  (dom/On "change"
                                    (fn [_] (reset! !input-mode "full"))
                                    nil))
                                (dom/text " Full page text"))
                              (dom/label
                                (dom/props {:style {:display "block" :margin-bottom "4px"}})
                                (dom/input
                                  (dom/props {:type "radio" :name "input-mode" :value "selected"
                                              :checked (= input-mode "selected")})
                                  (dom/On "change"
                                    (fn [_] (reset! !input-mode "selected"))
                                    nil))
                                (dom/text " Selected text only"))
                              (dom/label
                                (dom/props {:style {:display "block" :margin-bottom "4px"}})
                                (dom/input
                                  (dom/props {:type "radio" :name "input-mode" :value "context"
                                              :checked (= input-mode "context")})
                                  (dom/On "change"
                                    (fn [_] (reset! !input-mode "context"))
                                    nil))
                                (dom/text " With context from previous pages"))))

                          ;; Context window (conditional)
                          (when (= input-mode "context")
                            (dom/div
                              (dom/props {:style {:margin-bottom "12px" :margin-left "20px"}})
                              (dom/label
                                (dom/props {:style {:display "block" :margin-bottom "8px"}})
                                (dom/text "Context window (number of previous pages):"))
                              (dom/input
                                (dom/props {:type "number" :min "1" :max "10" :value (str context-window)
                                            :style {:padding "8px" :font-size "14px" :width "80px"}})
                                (reset! !context-window
                                  (dom/On "input"
                                    (fn [e]
                                      (let [val (-> e .-target .-value)]
                                        (if (seq val)
                                          (js/parseInt val)
                                          3)))
                                    3)))))

                          ;; Card type selection
                          (dom/div
                            (dom/props {:style {:margin-bottom "12px"}})
                            (dom/label
                              (dom/props {:style {:display "block" :margin-bottom "8px" :font-weight "bold"}})
                              (dom/text "Card Type:"))
                            (dom/div
                              (dom/label
                                (dom/props {:style {:display "block" :margin-bottom "4px"}})
                                (dom/input
                                  (dom/props {:type "radio" :name "card-type" :value "basic"
                                              :checked (= card-type "basic")})
                                  (dom/On "change"
                                    (fn [_] (reset! !card-type "basic"))
                                    nil))
                                (dom/text " Basic (Q&A)"))
                              (dom/label
                                (dom/props {:style {:display "block"}})
                                (dom/input
                                  (dom/props {:type "radio" :name "card-type" :value "cloze"
                                              :checked (= card-type "cloze")})
                                  (dom/On "change"
                                    (fn [_] (reset! !card-type "cloze"))
                                    nil))
                                (dom/text " Cloze Deletion"))))

                          ;; Card count input
                          (dom/div
                            (dom/props {:style {:margin-bottom "12px"}})
                            (dom/label
                              (dom/props {:style {:display "block" :margin-bottom "8px" :font-weight "bold"}})
                              (dom/text "Number of Cards:"))
                            (dom/input
                              (dom/props {:type "number" :min "1" :max "50" :value (str card-count)
                                          :style {:padding "8px" :font-size "14px" :width "100px"}})
                              (reset! !card-count
                                (dom/On "input"
                                  (fn [e]
                                    (let [val (-> e .-target .-value)]
                                      (if (seq val)
                                        (js/parseInt val)
                                        20)))
                                  20))))

                          ;; Generate button
                          (dom/button
                            (dom/props {:style {:padding "10px 20px"
                                                :background "#28a745"
                                                :color "white"
                                                :border "none"
                                                :border-radius "4px"
                                                :cursor "pointer"
                                                :font-size "14px"
                                                :font-weight "bold"}})
                            (dom/text "Generate Cards")
                            (let [click-event (dom/On "click" identity nil)
                                  [?token ?error] (e/Token click-event)]

                              (dom/props {:disabled (some? ?token)
                                          :style {:padding "10px 20px"
                                                  :background (if (some? ?token) "#999" "#28a745")
                                                  :color "white"
                                                  :border "none"
                                                  :border-radius "4px"
                                                  :cursor (if (some? ?token) "not-allowed" "pointer")
                                                  :font-size "14px"
                                                  :font-weight "bold"}})

                              (when ?token
                                (dom/text " Generating..."))

                              (when ?error
                                (dom/div
                                  (dom/props {:style {:color "red" :margin-top "10px" :font-size "14px"}})
                                  (dom/text "Error: " ?error)))

                              (when-some [token ?token]
                                ;; CLIENT: Get selected text if needed
                                (let [selected-text (when (= input-mode "selected")
                                                      (editor/get-selected-text!))]
                                  ;; Validate selection
                                  (when (and (= input-mode "selected")
                                             (nil? selected-text))
                                    (token "No text selected. Please select text in the editor first."))

                                  ;; Determine content (page-text from outer scope)
                                  (let [content (case input-mode
                                                  "full" page-text
                                                  "selected" selected-text
                                                  "context" page-text
                                                  page-text)
                                        ;; SERVER: Get context if needed
                                        context-text (when (= input-mode "context")
                                                       (e/server (cards/get-context-pages selected-doc current-pdf-page context-window)))
                                        ;; SERVER: Generate cards
                                        generate-result (e/server
                                                          (if (= card-type "basic")
                                                            (cards/generate-basic-cards
                                                              {:content content
                                                               :context context-text
                                                               :card-count card-count})
                                                            (cards/generate-cloze-cards
                                                              {:content content
                                                               :context context-text
                                                               :card-count card-count})))]

                                    (if-not (:success generate-result)
                                      (token (:error generate-result))
                                      (let [generated-cards (e/server (:cards generate-result))
                                            save-result (e/server (cards/save-cards selected-doc current-pdf-page card-type generated-cards))]
                                        (if (:success save-result)
                                          (do
                                            (e/server (swap! !refresh inc))
                                            (token))
                                          (token (:error save-result)))))))))))

                        ;; Vertical drag handle 2
                        (dom/div
                          (dom/props {:class "split-divider-v"})
                          (let [divider-el dom/node]
                            (js/setTimeout #(make-draggable! divider-el :y !mid-pct) 50)))

                        ;; RIGHT-BOTTOM: cards list
                        (dom/div
                          (dom/props {:style {:flex "1" :overflow-y "auto" :padding "12px" :min-height "0"}})
                          (dom/h4 (dom/props {:style {:margin-top "0"}}) (dom/text "Generated Cards"))
                          (let [cards-result (e/server (get-cards* refresh selected-doc current-pdf-page))]
                            (if (:success cards-result)
                              (if (seq (:cards cards-result))
                                (dom/div
                                  (e/server
                                    (e/for-by :flashcards/id [card (:cards cards-result)]
                                      (e/client (CardItem card)))))
                                (dom/p
                                  (dom/props {:style {:color "gray" :font-size "14px"}})
                                  (dom/text "No cards generated yet.")))
                              (dom/div
                                (dom/props {:style {:color "red" :font-size "14px"}})
                                (dom/text "Error loading cards: " (:error cards-result)))))))))))))))))))
