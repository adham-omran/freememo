(ns electric-starter-app.pdf-page
  "Document list view — virtual-scrolled table of documents with delete."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.electric-scroll0 :refer [Scroll-window Tape]]
   [contrib.data :refer [clamp-left]]
   [electric-starter-app.util :as util]
   #?(:clj [electric-starter-app.pdf :as pdf])
   #?(:clj [electric-starter-app.db :as db])
   #?(:clj [clojure.string :as str])
   [electric-starter-app.card-components :as card-components]))

;; Query wrapper: takes refresh arg to create Electric reactive dependency
#?(:clj (defn list-pdfs* [_refresh user-id] (pdf/list-pdfs user-id)))

;; Helper: format bytes to human-readable
#?(:clj
   (defn format-bytes [bytes]
     (if (nil? bytes)
       "-"
       (cond
         (< bytes 1024) (str bytes " B")
         (< bytes (* 1024 1024)) (format "%.1f KB" (double (/ bytes 1024)))
         :else (format "%.1f MB" (double (/ bytes 1024 1024)))))))

;; Helper: format timestamp to "2026-03-14 10:11"
#?(:clj
   (defn format-timestamp [ts]
     (when ts
       (let [fmt (java.time.format.DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm")]
         (.format (.toLocalDateTime ts) fmt)))))

#?(:clj
   (defn filter-docs [docs filter-text]
     (if (or (nil? filter-text) (str/blank? filter-text))
       docs
       (let [q (str/lower-case (str/trim filter-text))]
         (filterv #(str/includes? (str/lower-case (or (:documents/filename %) "")) q) docs)))))

;; Document list view — renders table header + virtual-scrolled rows
;; Receives !refresh and filter-text from parent (LibraryPage)
(e/defn DocumentListView [user-id !nav-target navigate! !refresh filter-text]
  (e/client
    (e/server
      (let [refresh (e/watch !refresh)
            docs-result (list-pdfs* refresh user-id)]
        (e/client
          (if (:success docs-result)
            (let [all-docs (e/server (:documents docs-result))
                  total-doc-count (e/server (count all-docs))
                  docs-vec (e/server (vec (filter-docs all-docs filter-text)))
                  doc-count (e/server (count docs-vec))
                  row-height 40]
              (dom/div
                (dom/props {:style {:flex "1" :display "flex" :flex-direction "column" :min-height "0"}})

                ;; Table header
                (let [grid-cols "1fr 60px 80px 140px 140px"]
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
                          (dom/th (dom/props {:style (merge th-style {:text-align "left"})}) (dom/text "Uploaded"))
                          (dom/th (dom/props {:style (merge th-style {:text-align "center"})}) (dom/text "Actions"))))))

                  (if (pos? doc-count)
                  ;; Scrollable body
                    (dom/div
                      (dom/props {:style {:flex "1" :overflow-y "auto" :min-height "0"}})
                      (let [[offset limit] (Scroll-window row-height doc-count dom/node {:overquery-factor 1})
                            occluded-height (clamp-left (* row-height (- doc-count limit)) 0)]
                        (dom/props {:class "tape-scroll"
                                    :style {:--offset offset :--row-height (str row-height "px")}})
                        (dom/table
                          (dom/props {:style {:width "100%" :grid-template-columns grid-cols :font-size "14px"}})
                          (e/for [i (Tape offset limit)]
                            (let [item (e/server (nth docs-vec i nil))]
                              (when item
                                (let [id (e/server (:documents/id item))
                                      filename (e/server (:documents/filename item))
                                      file-size (e/server (format-bytes (:documents/file_size item)))
                                      uploaded (e/server (format-timestamp (:documents/uploaded_at item)))
                                      source-type (e/server (or (:documents/source_type item) "pdf"))
                                      type-label (e/server (case source-type
                                                             "wikipedia" "Wiki"
                                                             "web" "Web"
                                                             "PDF"))
                                      type-color (e/server (case source-type
                                                             "wikipedia" "#fef3c7"
                                                             "web" "#e0f2fe"
                                                             "#dcfce7"))]
                                  (dom/tr
                                    (dom/props {:style {:border-bottom "1px solid #f0f0f0" :height (str row-height "px")
                                                        :--order (inc i)}})
                                    ;; Name — clickable link
                                    (dom/td
                                      (dom/props {:style {:padding "8px 10px" :overflow "hidden" :text-overflow "ellipsis" :white-space "nowrap"}})
                                      (dom/span
                                        (dom/props {:style {:color "var(--color-primary)" :cursor "pointer" :text-decoration "underline"}
                                                    :title "Open in Learn tab"})
                                        (dom/text (util/display-name filename))
                                        (dom/On "click"
                                          (fn [_]
                                            (reset! !nav-target {:doc-id id})
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
                                      (dom/text file-size))
                                    ;; Uploaded
                                    (dom/td
                                      (dom/props {:style {:padding "8px 10px" :color "var(--color-text-secondary)"}})
                                      (dom/text (or uploaded "-")))
                                    ;; Actions: Review + Delete
                                    (dom/td
                                      (dom/props {:style {:padding "8px 10px" :text-align "center" :display "flex" :gap "4px" :justify-content "center" :align-items "center"}})
                                      (dom/button
                                        (dom/props {:class "btn btn-sm btn-secondary"
                                                    :style {:padding "3px 10px"}
                                                    :title "Review all extracts in this document"})
                                        (dom/text "Review")
                                        (dom/On "click"
                                          (fn [e]
                                            (.stopPropagation e)
                                            (reset! !nav-target {:subset-review {:topic-type "document"
                                                                                 :root-id id
                                                                                 :root-name filename}})
                                            (navigate! :learn))
                                          nil))
                                      (let [!deleting (atom false)
                                            deleting (e/watch !deleting)]
                                        (dom/button
                                          (dom/props {:disabled deleting
                                                      :class "btn btn-sm btn-danger-fill"
                                                      :style {:padding "3px 10px"
                                                              :background (if deleting "#999" "var(--color-danger)")
                                                              :cursor (if deleting "not-allowed" "pointer")}})

                                          (dom/text (if deleting "..." "Delete"))
                                          (let [click-event (dom/On "click"
                                                              (fn [_]
                                                                #?(:cljs
                                                                   (when (js/confirm "Delete this document? All pages, extracts, and cards will be permanently removed.")
                                                                     id)
                                                                   :clj nil))
                                                              nil)
                                                [?token ?error] (e/Token click-event)]
                                            (when ?error
                                              (dom/span
                                                (dom/props {:style {:color "var(--color-danger)" :font-size "11px" :margin-left "var(--sp-1)"}})
                                                (dom/text ?error)))
                                            (when-some [token ?token]
                                              (reset! !deleting true)
                                              (let [note-ids (e/server (db/get-anki-note-ids-for-document click-event))
                                                    result (e/server (pdf/delete-pdf user-id click-event))]
                                                (if (:success result)
                                                  (do (reset! !deleting false)
                                                    (e/server (swap! !refresh inc))
                                                    (e/client (card-components/try-delete-anki-notes! note-ids))
                                                    (token))
                                                  (do (reset! !deleting false)
                                                    (token (:error result))))))))))))))))
                        (dom/div (dom/props {:style {:height (str occluded-height "px")}}))))

                  ;; Empty state
                    (dom/p
                      (dom/props {:style {:color "var(--color-text-hint)" :font-size "14px" :padding "var(--sp-4) 0"}})
                      (dom/text (if (zero? total-doc-count)
                                  "No documents yet. Import content from the Import tab."
                                  "No documents match your search.")))))))

            (dom/div
              (dom/props {:style {:color "var(--color-danger)"}})
              (dom/text "Error loading documents: " (:error docs-result)))))))))
