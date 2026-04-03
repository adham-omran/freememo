(ns freememo.learn-page
  "Learn tab — incremental reading with spaced review queue."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.logging :as log]
   [freememo.navigation :as nav]
   #?(:clj [freememo.user-state :as us])
   [freememo.learn-session :refer [LearnSession]]
   [freememo.subset-review :refer [SubsetReviewSession]]
   [freememo.page-viewer :as page-viewer :refer [OcrPage]]
   [freememo.extract-page :refer [ExtractPage]]
   [freememo.util :as util]
   #?(:clj [freememo.db :as db])
   [freememo.card-components :as card-components]))

;; Per-user refresh via user-state registry

(defn get-browse-page-stats* [_refresh parent-id]
  #?(:clj (let [pages (db/list-pages parent-id)
                remaining (sort (map :topics/page_number
                                  (remove #(= "done" (:topics/status %)) pages)))]
            {:done (- (count pages) (count remaining))
             :total (count pages)
             :remaining remaining})
     :cljs nil))

(defn get-learning-queue* [_refresh user-id]
  #?(:clj (vec (db/get-learning-queue user-id))
     :cljs nil))

(defn get-learning-queue-count* [_refresh user-id]
  #?(:clj (db/get-learning-queue-count user-id)
     :cljs 0))

(defn get-total-topic-count* [_refresh user-id]
  #?(:clj (db/get-total-topic-count user-id)
     :cljs 0))

(defn get-inactive-topics* [_refresh user-id]
  #?(:clj (vec (db/get-inactive-topics user-id))
     :cljs nil))

;; Badge display for topic kinds
(defn kind-badge [kind]
  (case kind
    "pdf" ["PDF" "var(--color-badge-pdf)"]
    "epub" ["EPUB" "var(--color-badge-epub)"]
    ("web" "wikipedia") ["Web" "var(--color-badge-web)"]
    ["Topic" "var(--color-badge-epub)"]))

(e/defn LearnBrowseTopic [user-id enc-key topic-id !nav-state llm-enabled? origin navigate!]
  (e/client
    (let [exists? (e/server (some? (db/get-topic topic-id)))]
      (if exists?
        (dom/div
          (dom/props {:style {:height "100%" :display "flex" :flex-direction "column" :overflow "hidden"}})
          (ExtractPage user-id enc-key topic-id
            (fn
              ([_tab] (if origin (navigate! origin) (reset! !nav-state (nav/nav-overview))))
              ([_tab _nav] (if origin (navigate! origin) (reset! !nav-state (nav/nav-overview)))))
            (fn [root-id page kind]
              (if (= kind "pdf")
                (reset! !nav-state (nav/nav-browse-pdf root-id page origin))
                (reset! !nav-state (nav/nav-browse-topic root-id origin))))
            llm-enabled? origin))
        (do (reset! !nav-state (nav/nav-overview)) nil)))))

(e/defn LearnBrowseDoc [user-id enc-key nav !nav-state llm-enabled? origin navigate!]
  (e/client
    (dom/div
      (dom/props {:style {:height "100%" :display "flex" :flex-direction "column" :overflow "hidden"}})
      (let [topic-id (:topic-id nav)
            doc-title (e/server (:topics/title (db/get-topic topic-id)))
            pv-refresh (e/server (e/watch (us/get-atom user-id :refresh)))
            page-stats (e/server (get-browse-page-stats* pv-refresh topic-id))]
        (dom/div
          (dom/props {:class "header-bar" :style {:gap "12px"}})
          (dom/button
            (dom/props {:class "btn btn-sm btn-secondary"})
            (dom/text (case origin :queue "Back to Queue" :library "Back to Library" "Back to Overview"))
            (dom/On "click" (fn [_] (if origin (navigate! origin) (reset! !nav-state (nav/nav-overview)))) nil))
          (dom/span
            (dom/props {:style {:color "var(--color-text-secondary)" :font-size "14px"}})
            (dom/text (str "Browsing: " (or doc-title "document"))))
          (when (and page-stats (pos? (:total page-stats)))
            (let [remaining (:remaining page-stats)
                  tooltip (cond
                            (empty? remaining) "All pages done!"
                            (<= (count remaining) 20)
                            (str "Remaining: " (clojure.string/join ", " remaining))
                            :else
                            (str "Remaining: " (clojure.string/join ", " (take 20 remaining))
                              " ... and " (- (count remaining) 20) " more"))]
              (dom/span
                (dom/props {:style {:color "var(--color-text-secondary)" :font-size "13px" :margin-left "auto" :cursor "default"}
                            :data-tooltip tooltip})
                (dom/text (:done page-stats) " / " (:total page-stats) " pages done"))))))
      (dom/div
        (dom/props {:style {:flex "1" :min-height "0" :overflow "hidden"}})
        (let [!nav (atom nav)]
          (OcrPage user-id enc-key !nav llm-enabled?))))))

(e/defn LearnOverview [user-id !nav-state navigate!]
  (e/client
    (dom/div
      (dom/props {:class "page-container"
                  :style {:height "100%" :display "flex" :flex-direction "column"}})

      (let [refresh (e/server (e/watch (us/get-atom user-id :refresh)))
            due-count (e/server (get-learning-queue-count* refresh user-id))
            total-count (e/server (get-total-topic-count* refresh user-id))]

        ;; Header with count and Learn button
        (dom/div
          (dom/props {:style {:display "flex" :align-items "center" :gap "16px" :margin-bottom "16px" :flex-shrink "0"}})
          (when (pos? due-count)
            (dom/button
              (dom/props {:class "btn btn-primary" :style {:padding "8px 24px" :font-size "15px" :font-weight "600"}})
              (dom/text "Start Learning")
              (dom/On "click" (fn [_] (reset! !nav-state (nav/nav-session))) nil)))
          (dom/span
            (dom/props {:style {:color "var(--color-text-secondary)" :font-size "14px"}
                        :title "Topics scheduled for review"})
            (dom/text (str due-count " topics due"))))

        (if (pos? due-count)
          ;; Top 10 due topics
          (let [items-vec (e/server (get-learning-queue* refresh user-id))
                item-count (e/server (count items-vec))
                display-limit 10
                grid-cols "70px 1fr"]
            (dom/div
              (dom/props {:style {:flex "1" :display "flex" :flex-direction "column" :min-height "0"}})

              ;; Table header
              (dom/div
                (dom/props {:style {:display "grid" :grid-template-columns grid-cols :font-size "14px" :flex-shrink "0"}})
                (let [th-base {:padding "8px 10px" :border-bottom "2px solid var(--color-border)" :font-weight "600" :color "var(--color-text-primary)"}]
                  (dom/div (dom/props {:style (merge th-base {:text-align "center"})}) (dom/text "Type"))
                  (dom/div (dom/props {:style (merge th-base {:text-align "left"})}) (dom/text "Title"))))

              ;; Table body — top 10 rows
              (dom/div
                (dom/props {:style {:overflow-y "auto" :min-height "0"}})
                (e/for-by identity [i (e/server (vec (range (min display-limit item-count))))]
                  (let [item (e/server (nth items-vec i nil))]
                    (when item
                      (let [kind (e/server (:topics/kind item))
                            parent-id (e/server (:topics/parent_id item))
                            title (e/server (or (:topics/title item) "-"))
                            content (e/server (or (:topics/content item) ""))
                            is-root (nil? parent-id)
                            display-title (if is-root
                                            (util/display-name title)
                                            (let [preview (util/extract-preview content 80)]
                                              (if (seq preview) preview title)))
                            [type-label type-color] (kind-badge kind)]
                        (dom/div
                          (dom/props {:style {:display "grid" :grid-template-columns grid-cols
                                              :border-bottom "1px solid var(--color-bg-subtle)" :height "40px"
                                              :align-items "center" :font-size "14px"}})
                          (dom/div
                            (dom/props {:style {:padding "8px 10px" :text-align "center"}})
                            (dom/span
                              (dom/props {:class "type-badge" :style {:padding "2px 8px" :background type-color}})
                              (dom/text type-label)))
                          (dom/div
                            (dom/props {:style {:padding "8px 10px" :overflow "hidden" :text-overflow "ellipsis" :white-space "nowrap"}})
                            (dom/text display-title))))))))

              ;; "N remaining" link to Queue
              (when (> item-count display-limit)
                (dom/div
                  (dom/props {:style {:padding "12px 10px" :font-size "13px" :color "var(--color-text-secondary)"}})
                  (dom/text (str "Showing " display-limit " of " item-count " due. "))
                  (dom/a
                    (dom/props {:style {:color "var(--color-primary)" :cursor "pointer" :text-decoration "none" :font-weight "500"}})
                    (dom/text "See full queue →")
                    (dom/On "click" (fn [_] (navigate! :queue)) nil))))))

          ;; Empty state
          (dom/p
            (dom/props {:style {:color "var(--color-text-secondary)" :font-size "14px" :margin-top "24px"}})
            (dom/text (if (zero? total-count)
                        "No topics yet. Import a document from the Import tab to start learning."
                        "All caught up! No topics due for review."))))

        ;; Inactive items section (done)
        (let [!show-inactive (atom false)
              show-inactive (e/watch !show-inactive)
              inactive (when show-inactive
                         (e/server (get-inactive-topics* refresh user-id)))]
          (dom/div
            (dom/props {:style {:margin-top "24px" :border-top "1px solid var(--color-border)" :padding-top "12px"}})
            (dom/button
              (dom/props {:class "btn btn-sm btn-secondary" :style {:color "var(--color-text-secondary)"}})
              (dom/text (if show-inactive "Hide inactive" "Show inactive"))
              (dom/On "click" (fn [_] (swap! !show-inactive not)) nil))

            (when (and show-inactive (seq inactive))
              (dom/div
                (dom/props {:style {:margin-top "8px"}})
                (e/for-by :topics/id [item inactive]
                  (let [item-id (:topics/id item)
                        kind (:topics/kind item)
                        parent-id (:topics/parent_id item)
                        raw-title (or (:topics/title item) "-")
                        title (if parent-id
                                (util/extract-preview raw-title 80)
                                (util/display-name raw-title))
                        item-status (or (:topics/status item) "done")
                        [type-label type-color] (kind-badge kind)]
                    (dom/div
                      (dom/props {:style {:display "flex" :align-items "center" :gap "8px"
                                          :padding "8px 10px" :border-bottom "1px solid var(--color-bg-subtle)"}})
                      ;; Status badge
                      (dom/span
                        (dom/props {:style {:padding "2px 6px" :border-radius "4px" :font-size "10px" :font-weight "600"
                                            :background (if (= item-status "done") "var(--color-success-light)" "var(--color-bg-hover)")
                                            :color (if (= item-status "done") "var(--color-success-dark)" "var(--color-text-hint)")}})
                        (dom/text item-status))
                      (dom/span
                        (dom/props {:class "type-badge" :style {:padding "2px 8px" :background type-color}})
                        (dom/text type-label))
                      (dom/span
                        (dom/props {:style {:flex "1" :font-size "13px" :overflow "hidden"
                                            :text-overflow "ellipsis" :white-space "nowrap"}})
                        (dom/text title))
                      ;; Restore button
                      (dom/button
                        (dom/props {:class "btn btn-sm btn-primary" :style {:padding "3px 10px" :font-size "12px"}})
                        (dom/text "Restore")
                        (let [event (dom/On "click" (fn [_] :restore) nil)
                              [?token _] (e/Token event)]
                          (when-some [token ?token]
                            (e/server (db/restore-topic! item-id))
                            (e/server (swap! (us/get-atom user-id :refresh) inc))
                            (token))))
                      ;; Delete button
                      (let [!show-confirm-delete (atom false)
                            show-confirm-delete (e/watch !show-confirm-delete)]
                        (dom/button
                          (dom/props {:class "btn btn-sm btn-danger-fill" :style {:padding "3px 10px" :font-size "12px"}})
                          (dom/text "Delete")
                          (dom/On "click" (fn [_] (reset! !show-confirm-delete true)) nil))
                        (dom/div
                          (dom/props {:class "modal-backdrop"
                                      :style {:display (if show-confirm-delete "flex" "none")}})
                          (dom/On "click" (fn [_] (reset! !show-confirm-delete false)) nil)
                          (dom/On "keydown" (fn [e] (when (= (.-key e) "Escape") (reset! !show-confirm-delete false))) nil)
                          (dom/div
                            (dom/props {:class "modal-content modal-sm"})
                            (dom/On "click" (fn [e] (.stopPropagation e)) nil)
                            (dom/div
                              (dom/props {:class "confirm-modal-body"})
                              (dom/p (dom/text "Permanently delete this item?")))
                            (dom/div
                              (dom/props {:class "confirm-modal-actions"})
                              (dom/button
                                (dom/props {:class "btn btn-secondary"})
                                (dom/text "Cancel")
                                (dom/On "click" (fn [_] (reset! !show-confirm-delete false)) nil))
                              (dom/button
                                (dom/props {:class "btn btn-danger-fill"})
                                (dom/text "Delete")
                                (let [event (dom/On "click" (fn [_] :confirmed) nil)
                                      [?token _] (e/Token event)]
                                  (when-some [token ?token]
                                    (let [is-root (nil? parent-id)
                                          note-ids (e/server
                                                     (if is-root
                                                       (db/get-all-anki-note-ids item-id)
                                                       (db/get-anki-note-ids item-id)))]
                                      (e/server (db/delete-topic! item-id))
                                      (e/server (swap! (us/get-atom user-id :refresh) inc))
                                      (e/client (card-components/try-delete-anki-notes! note-ids))
                                      (reset! !show-confirm-delete false)
                                      (token))))))))))))))))))))

(e/defn LearnPage [user-id enc-key !nav-state navigate! llm-enabled?]
  (e/client
    (let [nav-state (e/watch !nav-state)
          nav-type (:type nav-state)
          !queue-idx (atom 0)]

      (case nav-type
        :overview
        (LearnOverview user-id !nav-state navigate!)

        :browse-pdf
        (LearnBrowseDoc user-id enc-key
          {:topic-id (:topic-id nav-state) :page (:page nav-state)}
          !nav-state llm-enabled? (:origin nav-state) navigate!)

        :browse-topic
        (LearnBrowseTopic user-id enc-key (:topic-id nav-state)
          !nav-state llm-enabled? (:origin nav-state) navigate!)

        :session
        (let [refresh (e/server (e/watch (us/get-atom user-id :refresh)))
              queue-vec (e/server (get-learning-queue* refresh user-id))]
          (LearnSession user-id enc-key queue-vec !queue-idx !nav-state navigate! llm-enabled?))

        :subset-review
        (SubsetReviewSession user-id enc-key (:root-id nav-state) (:root-name nav-state)
          (fn [] (navigate! :library))
          llm-enabled?)

        ;; Default — show overview
        (LearnOverview user-id !nav-state navigate!)))))
