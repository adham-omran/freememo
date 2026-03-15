(ns electric-starter-app.content-card-table
  "Unified card table component — virtual-scrolled card list with edit/delete.
   Supports two query modes:
     :page    — query by (doc-id, page-number) via get-cards*
     :extract — query by content-item-id via get-cards-by-extract*"
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.electric-scroll0 :refer [Scroll-window Tape]]
   [contrib.data :refer [clamp-left]]
   [electric-starter-app.card-components :refer [CardRow get-cards* get-cards-by-extract*]]
   [electric-starter-app.ocr-modals :refer [EditCardModal]]))

(e/defn ContentCardTable [{:keys [query-mode doc-id page-number content-item-id]} !refresh]
  (e/client
    (let [refresh (e/server (e/watch !refresh))
          cards-result (e/server
                         (case query-mode
                           :page (get-cards* refresh doc-id page-number)
                           :extract (get-cards-by-extract* refresh content-item-id)))
          !editing-card (atom nil)
          editing-card (e/watch !editing-card)]

      (when editing-card
        (EditCardModal !editing-card !refresh))

      (if (:success cards-result)
        (let [cards-vec (e/server (vec (:cards cards-result)))
              card-count (e/server (count cards-vec))
              unsynced-count (e/server (count (filter #(or (nil? (:flashcards/anki_synced_at %))
                                                           (and (:flashcards/updated_at %)
                                                                (pos? (compare (str (:flashcards/updated_at %))
                                                                               (str (:flashcards/anki_synced_at %))))))
                                                      (:cards cards-result))))
              row-height 54]
          (dom/div
            (dom/props {:style {:flex "1" :overflow-y "auto" :min-height "0"}})
            (when (pos? card-count)
              (dom/div
                (dom/props {:style {:padding "4px 12px" :font-size "12px" :color "#888"}})
                (dom/text (str card-count " card" (when (not= card-count 1) "s")
                               (when (pos? unsynced-count) (str " (" unsynced-count " unsynced)"))))))
            (if (pos? card-count)
              (let [[offset limit] (Scroll-window row-height card-count dom/node {:overquery-factor 1})
                    occluded-height (clamp-left (* row-height (- card-count limit)) 0)]
                (dom/props {:class "tape-scroll"
                            :style {:--offset offset :--row-height (str row-height "px")}})
                (dom/table
                  (dom/props {:style {:width "100%"
                                      :border-collapse "separate"
                                      :border-spacing "0"
                                      :table-layout "fixed"
                                      :font-size "13px"
                                      :grid-template-columns "24px 1fr 1fr 60px 40px 40px"}})
                  (e/for [i (Tape offset limit)]
                    (let [card (e/server (nth cards-vec i nil))]
                      (when card
                        (CardRow card !editing-card !refresh (inc i))))))
                (dom/div (dom/props {:style {:height (str occluded-height "px")}})))
              (dom/p
                (dom/props {:style {:color "gray" :font-size "13px" :padding "8px 12px"}})
                (dom/text "No cards yet. Use the Generate button above to create flashcards from this content.")))))
        (dom/div
          (dom/props {:style {:color "red" :font-size "13px" :padding "8px 12px"}})
          (dom/text "Error loading cards: " (:error cards-result)))))))
