(ns electric-starter-app.learn-page
  "Learn tab — incremental reading with spaced review queue."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [electric-starter-app.learn-session :refer [LearnSession]]
   [electric-starter-app.subset-review :refer [SubsetReviewSession]]
   [electric-starter-app.ocr-page :refer [OcrPage]]
   [electric-starter-app.util :as util]
   #?(:clj [electric-starter-app.db :as db])
   [electric-starter-app.card-components :as card-components]))

#?(:clj (defonce !refresh (atom 0)))

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

(defn restore-topic* [topic-type id]
  #?(:clj (db/restore-topic topic-type id)
     :cljs nil))

(defn delete-content-item* [id]
  #?(:clj (db/delete-content-item id)
     :cljs nil))

(e/defn LearnBrowse [user-id enc-key nav !mode llm-enabled?]
  (e/client
    (let [doc-id (:doc-id nav)
          filename (e/server
                     (when doc-id
                       (-> (db/get-documents-by-id user-id doc-id)
                         first
                         :documents/filename)))]
      (dom/div
        (dom/props {:style {:height "100%" :display "flex" :flex-direction "column" :overflow "hidden"}})

        ;; Header bar
        (dom/div
          (dom/props {:class "header-bar" :style {:gap "12px"}})
          (dom/button
            (dom/props {:class "btn btn-sm btn-secondary"})
            (dom/text "Back to Overview")
            (dom/On "click" (fn [_] (reset! !mode :overview)) nil))
          (dom/span
            (dom/props {:style {:color "var(--color-text-secondary)" :font-size "14px"}})
            (dom/text (str "Browsing: " (or filename "document")))))

        ;; OcrPage workspace
        (dom/div
          (dom/props {:style {:flex "1" :min-height "0" :overflow "hidden"}})
          (let [!nav (atom nav)]
            (OcrPage user-id enc-key !nav llm-enabled?)))))))

(e/defn LearnOverview [user-id !mode navigate!]
  (e/client
    (dom/div
      (dom/props {:class "page-container"
                  :style {:height "100%" :display "flex" :flex-direction "column"}})

      (let [refresh (e/server (e/watch !refresh))
            due-count (e/server (get-learning-queue-count* refresh user-id))
            total-count (e/server (get-total-topic-count* refresh user-id))]

        ;; Header with count and Learn button
        (dom/div
          (dom/props {:style {:display "flex" :align-items "center" :gap "16px" :margin-bottom "16px" :flex-shrink "0"}})
          (when (pos? due-count)
            (dom/button
              (dom/props {:class "btn btn-primary" :style {:padding "8px 24px" :font-size "15px" :font-weight "600"}})
              (dom/text "Start Learning")
              (dom/On "click" (fn [_] (reset! !mode :session)) nil)))
          (dom/span
            (dom/props {:style {:color "var(--color-text-secondary)" :font-size "14px"}
                        :title "Documents and extracts scheduled for review"})
            (dom/text (str due-count " topics due"))))

        (if (pos? due-count)
          ;; Top 10 due topics (lightweight preview, not the full queue)
          (let [items-vec (e/server (get-learning-queue* refresh user-id))
                item-count (e/server (count items-vec))
                display-limit 10
                grid-cols "70px 2fr 50px 70px"]
            (dom/div
              (dom/props {:style {:flex "1" :display "flex" :flex-direction "column" :min-height "0"}})

              ;; Table header
              (dom/div
                (dom/props {:style {:display "grid" :grid-template-columns grid-cols :font-size "14px" :flex-shrink "0"}})
                (let [th-base {:padding "8px 10px" :border-bottom "2px solid var(--color-border)" :font-weight "600" :color "var(--color-text-primary)"}]
                  (dom/div (dom/props {:style (merge th-base {:text-align "center"})}) (dom/text "Type"))
                  (dom/div (dom/props {:style (merge th-base {:text-align "left"})}) (dom/text "Title"))
                  (dom/div (dom/props {:style (merge th-base {:text-align "center"})}) (dom/text "Pri"))
                  (dom/div (dom/props {:style (merge th-base {:text-align "center"})}) (dom/text "Due"))))

              ;; Table body — top 10 rows
              (dom/div
                (dom/props {:style {:overflow-y "auto" :min-height "0"}})
                (e/for-by identity [i (e/server (vec (range (min display-limit item-count))))]
                  (let [item (e/server (nth items-vec i nil))]
                    (when item
                      (let [topic-type (e/server (:topic_type item))
                            filename (e/server (or (:filename item) "-"))
                            content (e/server (or (:content item) ""))
                            priority (e/server (or (:priority item) 50))
                            interval (e/server (or (:interval_days item) 1.0))
                            is-doc (= topic-type "document")
                            display-title (if is-doc
                                            (util/display-name filename)
                                            (util/extract-preview content 80))
                            type-label (if is-doc "Doc" "Extract")
                            type-color (if is-doc "#dcfce7" "#44C2FF")
                            due-str (cond
                                      (< interval 1.0) (str (int (* interval 24)) "h")
                                      (= interval 1.0) "1d"
                                      :else (str (int interval) "d"))]
                        (dom/div
                          (dom/props {:style {:display "grid" :grid-template-columns grid-cols
                                              :border-bottom "1px solid #f0f0f0" :height "40px"
                                              :align-items "center" :font-size "14px"}})
                          (dom/div
                            (dom/props {:style {:padding "8px 10px" :text-align "center"}})
                            (dom/span
                              (dom/props {:class "type-badge" :style {:padding "2px 8px" :background type-color}})
                              (dom/text type-label)))
                          (dom/div
                            (dom/props {:style {:padding "8px 10px" :overflow "hidden" :text-overflow "ellipsis" :white-space "nowrap"}})
                            (dom/text display-title))
                          (dom/div
                            (dom/props {:style {:padding "8px 10px" :text-align "center" :color "#555"}})
                            (dom/text (str priority)))
                          (dom/div
                            (dom/props {:style {:padding "8px 10px" :text-align "center" :color "var(--color-text-secondary)" :font-size "12px"}})
                            (dom/text due-str))))))))

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
                (e/for-by :id [item inactive]
                  (let [topic-type (:topic_type item)
                        item-id (:id item)
                        raw-title (or (:title item) "-")
                        title (if (= topic-type "extract")
                                (util/extract-preview raw-title 80)
                                (util/display-name raw-title))
                        item-status (or (:status item) "done")
                        type-label (if (= topic-type "document") "Doc" "Extract")]
                    (dom/div
                      (dom/props {:style {:display "flex" :align-items "center" :gap "8px"
                                          :padding "8px 10px" :border-bottom "1px solid #f0f0f0"}})
                      ;; Status badge
                      (dom/span
                        (dom/props {:style {:padding "2px 6px" :border-radius "4px" :font-size "10px" :font-weight "600"
                                            :background (if (= item-status "done") "#dcfce7" "#f3f4f6")
                                            :color (if (= item-status "done") "#16a34a" "#6b7280")}})
                        (dom/text item-status))
                      (dom/span
                        (dom/props {:class "type-badge" :style {:padding "2px 8px"
                                                                :background (if (= topic-type "document") "#dcfce7" "#44C2FF")}})
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
                            (e/server (restore-topic* topic-type item-id))
                            (e/server (swap! !refresh inc))
                            (token))))
                      ;; Delete button
                      (dom/button
                        (dom/props {:class "btn btn-sm btn-danger-fill" :style {:padding "3px 10px" :font-size "12px"}})

                        (dom/text "Delete")
                        (let [event (dom/On "click"
                                      (fn [_]
                                        #?(:cljs
                                           (when (js/confirm "Permanently delete this item?")
                                             :delete)
                                           :clj nil))
                                      nil)
                              [?token _] (e/Token event)]
                          (when-some [token ?token]
                            (let [note-ids (e/server
                                             (if (= topic-type "document")
                                               (db/get-anki-note-ids-for-document item-id)
                                               (db/get-anki-note-ids-for-content-item item-id)))]
                              (e/server
                                (if (= topic-type "document")
                                  (db/delete-document user-id item-id)
                                  (db/delete-content-item item-id)))
                              (e/server (swap! !refresh inc))
                              (e/client (card-components/try-delete-anki-notes! note-ids))
                              (token))))))))))))))))

(e/defn LearnPage [user-id enc-key !nav-target navigate-to-extract! navigate! llm-enabled?]
  (e/client
    (let [!mode (atom :overview)
          mode (e/watch !mode)
          !browse-nav (atom nil)
          browse-nav (e/watch !browse-nav)
          !queue-idx (atom 0)
          !subset-state (atom nil)
          ;; Reactive watch — fires when "Open" or "View Source" sets nav-target
          nav-val (e/watch !nav-target)]

      ;; Consume nav-target reactively
      (when (= nav-val :start-session)
        (reset! !mode :session)
        (reset! !nav-target nil))
      (when (and (map? nav-val) (:doc-id nav-val))
        (reset! !browse-nav nav-val)
        (reset! !mode :browse)
        (reset! !nav-target nil))
      (when (and (map? nav-val) (:subset-review nav-val))
        (reset! !subset-state (:subset-review nav-val))
        (reset! !mode :subset-review)
        (reset! !nav-target nil))

      (case mode
        :overview
        (LearnOverview user-id !mode navigate!)

        :browse
        (when browse-nav
          (LearnBrowse user-id enc-key browse-nav !mode llm-enabled?))

        :session
        (let [refresh (e/server (e/watch !refresh))
              queue-vec (e/server (get-learning-queue* refresh user-id))]
          (LearnSession user-id enc-key queue-vec !queue-idx !mode !refresh !nav-target navigate-to-extract!
            (fn [doc-id page]
              (reset! !browse-nav {:doc-id doc-id :page page})
              (reset! !mode :browse))
            llm-enabled?))

        :subset-review
        (when-let [{:keys [topic-type root-id root-name]} (e/watch !subset-state)]
          (SubsetReviewSession user-id enc-key topic-type root-id root-name
            (fn [] (navigate! :library))
            llm-enabled?))))))
