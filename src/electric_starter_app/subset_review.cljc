(ns electric-starter-app.subset-review
  "Subset review session — review a subtree of the knowledge tree."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [electric-starter-app.ocr-page :refer [OcrPage]]
   [electric-starter-app.extract-page :refer [ExtractPage]]
   [electric-starter-app.util :as util]
   #?(:clj [electric-starter-app.db :as db])))

#?(:clj (defonce !refresh (atom 0)))

;; Server wrappers — _refresh creates reactive dependency
(defn get-subset-queue* [_refresh user-id root-id]
  #?(:clj (vec (db/get-subset-review-queue user-id root-id))
     :cljs nil))

;; Badge display for topic kinds
(defn kind-badge [kind parent-id]
  (case kind
    "pdf" ["PDF" "#dcfce7"]
    "epub" ["EPUB" "#f3e8ff"]
    ("web" "wikipedia") ["Web" "#e0f2fe"]
    (if parent-id
      ["Extract" "#44C2FF"]
      ["Topic" "#f3e8ff"])))

;; Bottom bar — Next button with split behavior (outstanding vs non-outstanding)
(e/defn SubsetBottomBar [topic-id outstanding? !queue-idx]
  (e/client
    (dom/div
      (dom/props {:style {:display "flex" :align-items "center" :justify-content "center"
                          :gap "12px" :padding "6px 16px" :flex-shrink "0"
                          :border-top "1px solid var(--color-border)"}})

      ;; Outstanding indicator
      (dom/span
        (dom/props {:style {:font-size "12px" :color (if outstanding? "#16a34a" "var(--color-text-hint)")}})
        (dom/text (if outstanding? "Due — will advance schedule" "Not due — read-only")))

      ;; Next button
      (dom/button
        (dom/props {:class (if outstanding? "btn btn-primary" "btn btn-secondary")
                    :style {:padding "8px 28px" :font-size "15px" :font-weight "600"}})
        (dom/text "Next")
        (let [event (dom/On "click" (fn [_] (str (random-uuid))) nil)
              [?token _error] (e/Token event)]
          (when-some [token ?token]
            (e/server
              (if outstanding?
                (db/advance-topic! topic-id)
                (db/touch-topic! topic-id)))
            (token)
            (swap! !queue-idx inc)))))))

;; Session header — banner, counter, back button
(e/defn SubsetSessionHeader [item !queue-idx idx total outstanding-remaining root-name on-exit!]
  (e/client
    (let [kind (:topics/kind item)
          parent-id (:topics/parent_id item)
          [type-label type-color] (kind-badge kind parent-id)
          outstanding? (:outstanding? item)]
      (dom/div
        (dom/props {:class "header-bar" :style {:gap "8px"}})

        ;; Back to Library
        (dom/button
          (dom/props {:class "btn btn-sm btn-secondary"})
          (dom/text "\u2190 Back to Library")
          (dom/On "click" (fn [_] (on-exit!)) nil))

        ;; Subset Review banner
        (dom/span
          (dom/props {:style {:font-size "13px" :font-weight "600" :color "var(--color-primary)"
                              :padding "2px 8px" :background "#eff6ff" :border-radius "var(--radius-sm)"}})
          (dom/text (str "Subset Review: " (util/display-name root-name))))

        ;; Type badge
        (dom/span
          (dom/props {:class "type-badge" :style {:padding "2px 8px" :background type-color}})
          (dom/text type-label))

        ;; Outstanding badge
        (dom/span
          (dom/props {:style {:padding "2px 6px" :border-radius "4px" :font-size "10px" :font-weight "600"
                              :background (if outstanding? "#dcfce7" "#f3f4f6")
                              :color (if outstanding? "#16a34a" "#6b7280")}})
          (dom/text (if outstanding? "Due" "Not due")))

        ;; Counter
        (dom/span
          (dom/props {:style {:margin-left "auto" :color "var(--color-text-secondary)" :font-size "13px"}})
          (dom/text (str (inc idx) " / " total
                      (when (pos? outstanding-remaining)
                        (str "  (" outstanding-remaining " outstanding)")))))))))

;; Main subset review session — no more topic-type parameter
(e/defn SubsetReviewSession [user-id enc-key root-id root-name on-exit! llm-enabled?]
  (e/client
    (let [!queue-idx (atom 0)
          idx (e/watch !queue-idx)
          refresh (e/server (e/watch !refresh))
          queue-vec (e/server (get-subset-queue* refresh user-id root-id))
          total (count queue-vec)]
      (dom/div
        (dom/props {:style {:height "100%" :display "flex" :flex-direction "column" :overflow "hidden"}})

        (if (or (zero? total) (>= idx total))
          ;; Empty or all done
          (dom/div
            (dom/props {:style {:flex "1" :display "flex" :flex-direction "column"
                                :align-items "center" :justify-content "center" :gap "16px"}})
            (dom/div
              (dom/props {:style {:font-size "24px" :color "var(--color-text-primary)"}})
              (dom/text (if (zero? total)
                          "Nothing to review"
                          "Subset review complete!")))
            (dom/div
              (dom/props {:style {:font-size "14px" :color "var(--color-text-secondary)"}})
              (dom/text (if (zero? total)
                          "All items in this subtree have been reviewed today."
                          (str "Reviewed " total " topic" (when (not= total 1) "s") "."))))
            (dom/button
              (dom/props {:class "btn btn-primary" :style {:padding "10px 28px" :font-size "15px" :font-weight "600"}})
              (dom/text "Back to Library")
              (dom/On "click" (fn [_] (on-exit!)) nil)))

          ;; Active topic
          (let [item (nth queue-vec idx nil)
                kind (:topics/kind item)
                topic-id (:topics/id item)
                outstanding? (:outstanding? item)
                show-pdf? (= kind "pdf")
                ;; Count remaining outstanding items (from current idx onward)
                outstanding-remaining (count (filter :outstanding? (subvec queue-vec idx)))]
            (when item
              ;; Header
              (SubsetSessionHeader item !queue-idx idx total outstanding-remaining root-name on-exit!)

              ;; Content
              (if show-pdf?
                (dom/div
                  (dom/props {:style {:flex "1" :min-height "0" :display "flex" :flex-direction "column" :overflow "hidden"}})
                  (dom/div
                    (dom/props {:style {:flex "1" :min-height "0" :overflow "hidden"}})
                    (let [!nav (atom {:topic-id topic-id})]
                      (OcrPage user-id enc-key !nav llm-enabled?)))
                  (SubsetBottomBar topic-id outstanding? !queue-idx))

                (dom/div
                  (dom/props {:style {:flex "1" :min-height "0" :display "flex" :flex-direction "column" :overflow "hidden"}})
                  (dom/div
                    (dom/props {:style {:flex "1" :min-height "0" :overflow "hidden"}})
                    (ExtractPage user-id enc-key topic-id nil nil llm-enabled?))
                  (SubsetBottomBar topic-id outstanding? !queue-idx))))))))))
