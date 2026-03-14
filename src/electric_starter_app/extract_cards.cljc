(ns electric-starter-app.extract-cards
  "Card table component for extract page — virtual-scrolled card list with edit/delete."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.electric-scroll0 :refer [Scroll-window]]
   [contrib.data :refer [clamp-left]]
   [electric-starter-app.card-components :refer [CardRow get-cards-by-extract*]]
   [electric-starter-app.ocr-modals :refer [EditCardModal AddCardModal]]))

#?(:clj (defonce !refresh (atom 0)))

;; Wrapper that bridges !refresh from this namespace (same-namespace bridging works in Electric)
(e/defn ExtractAddCardModal [!show-add card-type doc-id page-number content-item-id]
  (AddCardModal !show-add card-type doc-id page-number !refresh content-item-id))

(e/defn ExtractCardTable [content-item-id]
  (e/client
    (let [refresh (e/server (e/watch !refresh))
          cards-result (e/server (get-cards-by-extract* refresh content-item-id))
          !editing-card (atom nil)
          editing-card (e/watch !editing-card)]

      (when editing-card
        (EditCardModal !editing-card !refresh))

      (if (:success cards-result)
        (let [cards-vec (e/server (vec (:cards cards-result)))
              card-count (e/server (count cards-vec))
              row-height 36]
          (dom/div
            (dom/props {:style {:flex "1" :overflow-y "auto" :min-height "0"}})
            (if (pos? card-count)
              (let [[offset limit] (Scroll-window row-height card-count dom/node {:overquery-factor 1})
                    occluded-height (clamp-left (* row-height (- card-count limit)) 0)]
                (dom/table
                  (dom/props {:style {:width "100%"
                                      :border-collapse "separate"
                                      :border-spacing "0"
                                      :table-layout "fixed"
                                      :font-size "13px"}})
                  (dom/tbody
                    (dom/props {:style {:position "relative"
                                        :top (str (* offset row-height) "px")}})
                    (e/for [i (e/diff-by {} (range offset (+ offset limit)))]
                      (let [card (e/server (nth cards-vec i nil))]
                        (when card
                          (CardRow card !editing-card !refresh))))))
                (dom/div (dom/props {:style {:height (str occluded-height "px")}})))
              (dom/p
                (dom/props {:style {:color "gray" :font-size "13px" :padding "8px 12px"}})
                (dom/text "No cards generated yet.")))))
        (dom/div
          (dom/props {:style {:color "red" :font-size "13px" :padding "8px 12px"}})
          (dom/text "Error loading cards: " (:error cards-result)))))))
