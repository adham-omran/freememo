(ns electric-starter-app.pdf-page
  "Library list view — shows root topics (PDFs, EPUBs, web articles, standalone topics)."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.electric-scroll0 :refer [Scroll-window Tape]]
   [contrib.data :refer [clamp-left]]
   [electric-starter-app.util :as util]
   [electric-starter-app.card-components :as card-components]
   #?(:clj [electric-starter-app.db :as db])))

#?(:clj (defonce !refresh (atom 0)))

(defn format-bytes [n]
  #?(:clj (cond
            (nil? n) "0 B"
            (< n 1024) (str n " B")
            (< n (* 1024 1024)) (format "%.1f KB" (/ (double n) 1024))
            :else (format "%.1f MB" (/ (double n) (* 1024 1024))))
     :cljs nil))

(defn format-timestamp [ts]
  #?(:clj (when ts
            (let [inst (.toInstant ts)
                  ldt (java.time.LocalDateTime/ofInstant inst (java.time.ZoneId/systemDefault))]
              (.format ldt (java.time.format.DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm"))))
     :cljs nil))

(defn list-topics* [_refresh user-id]
  #?(:clj (db/get-root-topics user-id)
     :cljs nil))

(e/defn DocumentListView [user-id !nav-target navigate! !library-refresh]
  (e/client
    (let [refresh (e/server (e/watch !refresh))
          _ (e/server (e/watch !library-refresh))
          topics-vec (e/server (vec (list-topics* refresh user-id)))
          topic-count (e/server (count topics-vec))
          row-height 48
          grid-cols "2fr 70px 80px 150px 160px"]

        ;; Table header
      (dom/table
        (dom/props {:style {:width "100%" :display "grid" :grid-template-columns grid-cols :font-size "14px" :flex-shrink "0"}})
        (let [th-style {:padding "8px 10px" :border-bottom "2px solid var(--color-border)" :font-weight "600" :color "var(--color-text-label)"}]
          (dom/thead
            (dom/props {:style {:display "contents"}})
            (dom/tr
              (dom/props {:style {:display "contents"}})
              (dom/th (dom/props {:style (merge th-style {:text-align "left"})}) (dom/text "Name"))
              (dom/th (dom/props {:style (merge th-style {:text-align "center"})}) (dom/text "Type"))
              (dom/th (dom/props {:style (merge th-style {:text-align "right"})}) (dom/text "Size"))
              (dom/th (dom/props {:style (merge th-style {:text-align "left"})}) (dom/text "Created"))
              (dom/th (dom/props {:style (merge th-style {:text-align "center"})}) (dom/text "Actions"))))))

      (if (pos? topic-count)
          ;; Scrollable body
        (dom/div
          (dom/props {:style {:flex "1" :overflow-y "auto" :min-height "0"}})
          (let [[offset limit] (Scroll-window row-height topic-count dom/node {:overquery-factor 1})
                occluded-height (clamp-left (* row-height (- topic-count limit)) 0)]
            (dom/props {:class "tape-scroll"
                        :style {:--offset offset :--row-height (str row-height "px")}})
            (dom/table
              (dom/props {:style {:width "100%" :grid-template-columns grid-cols :font-size "14px"}})
              (e/for [i (Tape offset limit)]
                (let [item (e/server (nth topics-vec i nil))]
                  (when item
                    (let [id (e/server (:topics/id item))
                          title (e/server (or (:topics/title item) "Untitled"))
                          file-size (e/server (format-bytes (:topics/file_size item)))
                          created (e/server (format-timestamp (:topics/created_at item)))
                          kind (e/server (or (:topics/kind item) "basic"))
                          type-label (e/server (case kind
                                                 "pdf" "PDF"
                                                 ("web" "wikipedia") "Web"
                                                 "epub" "EPUB"
                                                 "basic" "Topic"
                                                 "Topic"))
                          type-color (e/server (case kind
                                                 "pdf" "#dcfce7"
                                                 ("web" "wikipedia") "#e0f2fe"
                                                 "epub" "#f3e8ff"
                                                 "basic" "#f3e8ff"
                                                 "#f3e8ff"))]
                      (dom/tr
                        (dom/props {:style {:display "contents" :height (str row-height "px")
                                            :--order (inc i)}})
                          ;; Name — clickable link
                        (dom/td
                          (dom/props {:style {:padding "8px 10px" :overflow "hidden" :text-overflow "ellipsis" :white-space "nowrap"}})
                          (dom/span
                            (dom/props {:style {:color "var(--color-primary)" :cursor "pointer" :text-decoration "underline"}
                                        :title "Open in Learn tab"})
                            (dom/text (util/display-name title))
                            (dom/On "click"
                              (fn [_]
                                (reset! !nav-target {:topic-id id :kind kind :title title})
                                (navigate! :learn))
                              nil)))
                          ;; Type badge
                        (dom/td
                          (dom/props {:style {:padding "8px 10px" :text-align "center"}})
                          (dom/span
                            (dom/props {:style {:padding "2px 8px" :border-radius "4px" :font-size "11px"
                                                :font-weight "600" :background type-color}})
                            (dom/text type-label)))
                          ;; Size
                        (dom/td
                          (dom/props {:style {:padding "8px 10px" :text-align "right" :color "var(--color-text-secondary)"}})
                          (dom/text (or file-size "-")))
                          ;; Created
                        (dom/td
                          (dom/props {:style {:padding "8px 10px" :color "var(--color-text-secondary)"}})
                          (dom/text (or created "-")))
                          ;; Actions: Review + Delete
                        (dom/td
                          (dom/props {:style {:padding "8px 10px" :text-align "center" :display "flex" :gap "4px" :justify-content "center" :align-items "center"}})
                          (dom/button
                            (dom/props {:class "btn btn-sm btn-secondary"
                                        :style {:padding "3px 10px"}
                                        :title "Review all topics under this document"})
                            (dom/text "Review")
                            (dom/On "click"
                              (fn [e]
                                (.stopPropagation e)
                                (reset! !nav-target {:subset-review {:root-id id
                                                                     :root-name title}})
                                (navigate! :learn))
                              nil))
                          (let [!deleting (atom nil)
                                deleting (= (e/watch !deleting) id)
                                !show-confirm (atom nil)
                                show-confirm (= (e/watch !show-confirm) id)]
                            (dom/button
                              (dom/props {:disabled deleting
                                          :class "btn btn-sm btn-danger-fill"
                                          :style {:padding "3px 10px"
                                                  :background (if deleting "#999" "var(--color-danger)")
                                                  :cursor (if deleting "not-allowed" "pointer")}})
                              (dom/text (if deleting "..." "Delete"))
                              (dom/On "click" (fn [_] (reset! !show-confirm id)) nil))
                            (when show-confirm
                              (dom/div
                                (dom/props {:class "modal-backdrop"})
                                (dom/On "click" (fn [_] (reset! !show-confirm nil)) nil)
                                (dom/On "keydown" (fn [e] (when (= (.-key e) "Escape") (reset! !show-confirm nil))) nil)
                                (dom/div
                                  (dom/props {:class "modal-content modal-sm"})
                                  (dom/On "click" (fn [e] (.stopPropagation e)) nil)
                                  (dom/div
                                    (dom/props {:class "confirm-modal-body"})
                                    (dom/p (dom/text "Delete this topic? All children, extracts, and cards will be permanently removed.")))
                                  (dom/div
                                    (dom/props {:class "confirm-modal-actions"})
                                    (dom/button
                                      (dom/props {:class "btn btn-secondary"})
                                      (dom/text "Cancel")
                                      (dom/On "click" (fn [_] (reset! !show-confirm nil)) nil))
                                    (dom/button
                                      (dom/props {:class "btn btn-danger-fill"})
                                      (dom/text "Delete")
                                      (let [event (dom/On "click" (fn [_] id) nil)
                                            [?token ?error] (e/Token event)]
                                        (when ?error
                                          (dom/span
                                            (dom/props {:style {:color "var(--color-danger)" :font-size "11px"}})
                                            (dom/text ?error)))
                                        (when-some [token ?token]
                                          (reset! !deleting id)
                                          (reset! !show-confirm nil)
                                          (let [note-ids (e/server (vec (db/get-all-anki-note-ids event)))]
                                            (e/server (db/delete-topic-for-user! user-id event))
                                            (reset! !deleting nil)
                                            (e/server (swap! !refresh inc))
                                            (e/client (card-components/try-delete-anki-notes! note-ids))
                                            (token)))))))))))))))))
            (dom/div (dom/props {:style {:height (str occluded-height "px")}}))))

          ;; Empty state
        (dom/p
          (dom/props {:style {:color "var(--color-text-hint)" :font-size "14px" :padding "var(--sp-4) 0"}})
          (dom/text "No documents yet. Import content from the Import tab."))))))
