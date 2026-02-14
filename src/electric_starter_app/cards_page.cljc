(ns electric-starter-app.cards-page
  "Flashcard generation UI component."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [electric-starter-app.rich-text-editor :as editor]
   #?(:clj [electric-starter-app.cards :as cards])
   #?(:clj [electric-starter-app.page :as page])
   #?(:clj [electric-starter-app.pdf :as pdf])))

#?(:clj (defonce !refresh (atom 0)))  ; Server-side refresh trigger

;; Query wrapper: takes refresh arg to create Electric reactive dependency
#?(:clj (defn get-cards* [_refresh document-id page-number]
          (cards/get-cards document-id page-number)))

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

;; Main cards page component
(e/defn CardsPage []
  (e/client
    (dom/div
      (dom/h1 (dom/text "Flashcard Generation"))

      (let [docs-result (e/server (pdf/list-pdfs))
            !selected-doc (atom nil)
            selected-doc (e/watch !selected-doc)
            !page-number (atom 1)
            page-number (e/watch !page-number)
            !input-mode (atom "full")  ; "full", "selected", "context"
            input-mode (e/watch !input-mode)
            !card-type (atom "basic")  ; "basic" or "cloze"
            card-type (e/watch !card-type)
            !context-window (atom 3)
            context-window (e/watch !context-window)
            !card-count (atom 20)
            card-count (e/watch !card-count)]

        ;; Document selector
        (when (:success docs-result)
          (if (seq (:documents docs-result))
            (dom/div
              (dom/props {:style {:margin-bottom "20px"}})
              (dom/label
                (dom/props {:style {:display "block" :margin-bottom "8px" :font-weight "bold"}})
                (dom/text "Select Document:"))
              (dom/select
                (dom/props {:style {:padding "8px" :font-size "14px" :width "100%" :max-width "400px"}})
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
            (dom/p (dom/text "No documents available. Please upload a PDF first."))))

        ;; Configuration panel (shows when document is selected)
        (when selected-doc
          (dom/div
            (dom/props {:style {:margin-top "20px" :padding "20px" :border "1px solid #ddd" :border-radius "8px" :background "#f5f5f5"}})

            ;; Page number input
            (dom/div
              (dom/props {:style {:margin-bottom "16px"}})
              (dom/label
                (dom/props {:style {:display "block" :margin-bottom "8px" :font-weight "bold"}})
                (dom/text "Page Number:"))
              (dom/input
                (dom/props {:type "number" :min "1" :value (str page-number)
                            :style {:padding "8px" :font-size "14px" :width "100px"}})
                (reset! !page-number
                  (dom/On "input"
                    (fn [e]
                      (let [val (-> e .-target .-value)]
                        (if (seq val)
                          (js/parseInt val)
                          1)))
                    1))))

            ;; Input mode selection
            (dom/div
              (dom/props {:style {:margin-bottom "16px"}})
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

            ;; Context window (shown only when "context" mode is selected)
            (when (= input-mode "context")
              (dom/div
                (dom/props {:style {:margin-bottom "16px" :margin-left "20px"}})
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
              (dom/props {:style {:margin-bottom "16px"}})
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
              (dom/props {:style {:margin-bottom "16px"}})
              (dom/label
                (dom/props {:style {:display "block" :margin-bottom "8px" :font-weight "bold"}})
                (dom/text "Number of Cards to Generate:"))
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
              (dom/props {:style {:padding "12px 24px"
                                  :background "#28a745"
                                  :color "white"
                                  :border "none"
                                  :border-radius "4px"
                                  :cursor "pointer"
                                  :font-size "16px"
                                  :font-weight "bold"}})
              (dom/text "Generate Cards")
              (let [click-event (dom/On "click" identity nil)
                    [?token ?error] (e/Token click-event)]

                (dom/props {:disabled (some? ?token)
                            :style {:padding "12px 24px"
                                    :background (if (some? ?token) "#999" "#28a745")
                                    :color "white"
                                    :border "none"
                                    :border-radius "4px"
                                    :cursor (if (some? ?token) "not-allowed" "pointer")
                                    :font-size "16px"
                                    :font-weight "bold"}})

                (when ?token
                  (dom/text " Generating..."))

                (when ?error
                  (dom/div
                    (dom/props {:style {:color "red" :margin-top "10px" :font-weight "normal" :font-size "14px"}})
                    (dom/text "Error: " ?error)))

                (when-some [token ?token]
                  (let [page-result (e/server (page/get-page-text selected-doc page-number))]
                    (if-not (:success page-result)
                      (token "No text found for this page. Please extract text first.")
                      (let [page-text (e/server (:text page-result))
                            content (case input-mode
                                      "full" page-text
                                      "selected" (or (editor/get-selected-text!) page-text)
                                      "context" page-text
                                      page-text)
                            context-text (when (= input-mode "context")
                                           (e/server (cards/get-context-pages selected-doc page-number context-window)))
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
                                save-result (e/server (cards/save-cards selected-doc page-number card-type generated-cards))]
                            (if (:success save-result)
                              (do
                                (e/server (swap! !refresh inc))
                                (token))
                              (token (:error save-result)))))))))))

          ;; Display generated cards
          (dom/div
            (dom/props {:style {:margin-top "30px"}})
            (dom/h2 (dom/text "Generated Cards"))
            (let [refresh (e/server (e/watch !refresh))
                  cards-result (e/server (get-cards* refresh selected-doc page-number))]
              (if (:success cards-result)
                (if (seq (:cards cards-result))
                  (dom/div
                    (e/server
                      (e/for-by :flashcards/id [card (:cards cards-result)]
                        (e/client (CardItem card)))))
                  (dom/p
                    (dom/props {:style {:color "gray"}})
                    (dom/text "No cards generated yet. Configure options above and click 'Generate Cards'.")))
                (dom/div
                  (dom/props {:style {:color "red"}})
                  (dom/text "Error loading cards: " (:error cards-result)))))))))))
)
