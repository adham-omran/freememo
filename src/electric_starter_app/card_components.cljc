(ns electric-starter-app.card-components
  "Shared card display components — used by both ocr-page and extract-page."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   #?(:clj [electric-starter-app.cards :as cards])))

;; Query wrapper: takes refresh arg to create Electric reactive dependency
;; Plain defn visible on both CLJ and CLJS — callers wrap in e/server
(defn get-cards* [_refresh document-id page-number]
  #?(:clj (cards/get-cards document-id page-number)
     :cljs nil))

;; Query wrapper for extract-level cards
(defn get-cards-by-extract* [_refresh content-item-id]
  #?(:clj (cards/get-cards-by-content-item content-item-id)
     :cljs nil))

;; Delete button — parameterized with !refresh atom
(e/defn DeleteCardButton [id !refresh]
  (e/client
    (dom/button
      (dom/props {:style {:padding "2px 6px" :background "#dc3545" :color "white"
                          :border "none" :border-radius "3px" :cursor "pointer"
                          :font-size "12px" :line-height "1"}})
      (dom/text "\u00D7")
      (let [click-event (dom/On "click" (fn [_] id) nil)
            [?token ?error] (e/Token click-event)]
        (dom/props {:disabled (some? ?token)
                    :style {:padding "2px 6px"
                            :background (if (some? ?token) "#999" "#dc3545")
                            :color "white" :border "none" :border-radius "3px"
                            :cursor (if (some? ?token) "not-allowed" "pointer")
                            :font-size "12px" :line-height "1"}})
        (when ?error
          (dom/div
            (dom/props {:style {:color "red" :font-size "11px"}})
            (dom/text ?error)))
        (when-some [token ?token]
          (let [result (e/server (cards/delete-card click-event))]
            (if (:success result)
              (do (e/server (swap! !refresh inc)) (token))
              (token (:error result)))))))))

;; Card table row component
(e/defn CardRow [card !editing-card !refresh]
  (e/client
    (let [id (e/server (:flashcards/id card))
          kind (e/server (:flashcards/kind card))
          question (e/server (:flashcards/question card))
          answer (e/server (:flashcards/answer card))
          cloze (e/server (:flashcards/cloze card))]
      (dom/tr
        ;; Front column
        (dom/td
          (dom/props {:style {:padding "6px 8px"
                              :border-bottom "1px solid #e0e0e0"}})
          (dom/text (if (= kind "basic") question cloze)))
        ;; Back column
        (dom/td
          (dom/props {:style {:padding "6px 8px"
                              :border-bottom "1px solid #e0e0e0"}})
          (dom/text (if (= kind "basic") (or answer "") "")))
        ;; Kind column
        (dom/td
          (dom/props {:style {:padding "6px 8px" :width "60px" :font-size "12px"
                              :border-bottom "1px solid #e0e0e0"}})
          (dom/text kind))
        ;; Edit column
        (dom/td
          (dom/props {:style {:padding "6px 4px" :width "40px" :text-align "center"
                              :border-bottom "1px solid #e0e0e0"}})
          (dom/button
            (dom/props {:style {:padding "2px 6px" :background "#2563eb" :color "white"
                                :border "none" :border-radius "3px" :cursor "pointer"
                                :font-size "12px"}})
            (dom/text "\u270E")
            (dom/On "click" (fn [_]
                              (let [data {:id id :kind kind :question question :answer answer :cloze cloze}]
                                (println "EDIT CLICK data:" (pr-str data))
                                (reset! !editing-card data))) nil)))
        ;; Delete column
        (dom/td
          (dom/props {:style {:padding "6px 4px" :width "40px" :text-align "center"
                              :border-bottom "1px solid #e0e0e0"}})
          (DeleteCardButton id !refresh))))))
