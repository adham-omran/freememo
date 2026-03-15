(ns electric-starter-app.learn-page
  "Learn tab — incremental reading with spaced review queue."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.electric-scroll0 :refer [Scroll-window Tape]]
   [contrib.data :refer [clamp-left]]
   [electric-starter-app.learn-session :refer [LearnSession]]
   [electric-starter-app.ocr-page :refer [OcrPage]]
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

(defn get-dismissed-topics* [_refresh user-id]
  #?(:clj (vec (db/get-dismissed-topics user-id))
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

(e/defn LearnOverview [user-id !mode]
  (e/client
    (dom/div
      (dom/props {:style {:padding "16px" :max-width "900px" :height "100%" :display "flex" :flex-direction "column"}})

      (let [refresh (e/server (e/watch !refresh))
            due-count (e/server (get-learning-queue-count* refresh user-id))
            total-count (e/server (get-total-topic-count* refresh user-id))]

        ;; Header with count and Learn button
        (dom/div
          (dom/props {:style {:display "flex" :align-items "center" :gap "16px" :margin-bottom "16px" :flex-shrink "0"}})
          (dom/h2
            (dom/props {:style {:margin "0" :font-size "20px"}})
            (dom/text "Learn"))
          (dom/span
            (dom/props {:style {:color "var(--color-text-secondary)" :font-size "14px"}
                        :title "Documents and extracts scheduled for review"})
            (dom/text (str due-count " topics due")))
          (when (pos? due-count)
            (dom/button
              (dom/props {:class "btn btn-primary" :style {:padding "8px 24px" :font-size "15px" :font-weight "600"}})
              (dom/text "Start Learning")
              (dom/On "click" (fn [_] (reset! !mode :session)) nil))))

        (if (pos? due-count)
          ;; Virtual-scrolled list of due topics
          (let [items-vec (e/server (get-learning-queue* refresh user-id))
                item-count (e/server (count items-vec))
                row-height 40]
            (dom/div
              (dom/props {:style {:flex "1" :display "flex" :flex-direction "column" :min-height "0"}})

              ;; Table header
              (dom/table
                (dom/props {:style {:width "100%" :border-collapse "collapse" :font-size "14px" :flex-shrink "0"}})
                (dom/thead
                  (let [th-base {:padding "8px 10px" :border-bottom "2px solid var(--color-border)" :font-weight "600" :color "var(--color-text-primary)"}]
                    (dom/tr
                      (dom/th (dom/props {:style (merge th-base {:text-align "center" :width "70px"})}) (dom/text "Type"))
                      (dom/th (dom/props {:style (merge th-base {:text-align "left"})}) (dom/text "Document"))
                      (dom/th (dom/props {:style (merge th-base {:text-align "center" :width "60px"})}) (dom/text "Page"))
                      (dom/th (dom/props {:style (merge th-base {:text-align "center" :width "60px"})}) (dom/text "Pri"))
                      (dom/th (dom/props {:style (merge th-base {:text-align "center" :width "80px"})}) (dom/text "Interval"))
                      (dom/th (dom/props {:style (merge th-base {:text-align "left"})}) (dom/text "Content"))))))

              ;; Scrollable body
              (dom/div
                (dom/props {:style {:flex "1" :overflow-y "auto" :min-height "0"}})
                (let [[offset limit] (Scroll-window row-height item-count dom/node {:overquery-factor 1})
                      occluded-height (clamp-left (* row-height (- item-count limit)) 0)]
                  (dom/props {:class "tape-scroll"
                              :style {:--offset offset :--row-height (str row-height "px")}})
                  (dom/table
                    (dom/props {:style {:width "100%" :border-collapse "collapse" :font-size "14px"}})
                    (e/for [i (Tape offset limit)]
                      (let [item (e/server (nth items-vec i nil))]
                        (when item
                          (let [topic-type (:topic_type item)
                                filename (or (:filename item) "-")
                                page-num (:page_number item)
                                priority (or (:priority item) 50)
                                interval (or (:interval_days item) 1.0)
                                content (or (:content item) "")
                                is-doc (= topic-type "document")
                                truncated (if is-doc
                                            filename
                                            (if (> (count content) 60)
                                              (str (subs content 0 60) "...")
                                              content))
                                type-label (if is-doc "Doc" "Extract")
                                type-color (if is-doc "#dcfce7" "#44C2FF")
                                interval-str (if (< interval 1.0)
                                               (str (int (* interval 24)) "h")
                                               (str (int interval) "d"))]
                            (dom/tr
                              (dom/props {:style {:border-bottom "1px solid #f0f0f0" :height (str row-height "px")
                                                  :--order (inc i)}})
                              (dom/td
                                (dom/props {:style {:padding "8px 10px" :text-align "center" :width "70px"}})
                                (dom/span
                                  (dom/props {:class "type-badge" :style {:padding "2px 8px" :background type-color}})
                                  (dom/text type-label)))
                              (dom/td
                                (dom/props {:style {:padding "8px 10px" :overflow "hidden" :text-overflow "ellipsis" :white-space "nowrap"}})
                                (dom/text filename))
                              (dom/td
                                (dom/props {:style {:padding "8px 10px" :text-align "center" :color "#555" :width "60px"}})
                                (dom/text (if is-doc "-" (str page-num))))
                              (dom/td
                                (dom/props {:style {:padding "8px 10px" :text-align "center" :color "#555" :width "60px"}})
                                (dom/text (str priority)))
                              (dom/td
                                (dom/props {:style {:padding "8px 10px" :text-align "center" :color "var(--color-text-secondary)" :font-size "12px" :width "80px"}})
                                (dom/text interval-str))
                              (dom/td
                                (dom/props {:style {:padding "8px 10px" :overflow "hidden" :text-overflow "ellipsis" :white-space "nowrap"}})
                                (dom/text truncated))))))))
                  (dom/div (dom/props {:style {:height (str occluded-height "px")}}))))))

          ;; Empty state
          (dom/p
            (dom/props {:style {:color "var(--color-text-secondary)" :font-size "14px" :margin-top "24px"}})
            (dom/text (if (zero? total-count)
                        "No topics yet. Import a document from the Documents tab to start learning."
                        "All caught up! No topics due for review."))))

        ;; Dismissed items section
        (let [!show-dismissed (atom false)
              show-dismissed (e/watch !show-dismissed)
              dismissed (when show-dismissed
                          (e/server (get-dismissed-topics* refresh user-id)))]
          (dom/div
            (dom/props {:style {:margin-top "24px" :border-top "1px solid var(--color-border)" :padding-top "12px"}})
            (dom/button
              (dom/props {:class "btn btn-sm btn-secondary" :style {:color "var(--color-text-secondary)"}})
              (dom/text (if show-dismissed "Hide dismissed" "Show dismissed"))
              (dom/On "click" (fn [_] (swap! !show-dismissed not)) nil))

            (when (and show-dismissed (seq dismissed))
              (dom/div
                (dom/props {:style {:margin-top "8px"}})
                (e/for-by :id [item dismissed]
                  (let [topic-type (:topic_type item)
                        item-id (:id item)
                        title (or (:title item) "-")
                        type-label (if (= topic-type "document") "Doc" "Extract")]
                    (dom/div
                      (dom/props {:style {:display "flex" :align-items "center" :gap "8px"
                                          :padding "8px 10px" :border-bottom "1px solid #f0f0f0"}})
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

(e/defn LearnPage [user-id enc-key !nav-target navigate-to-extract! llm-enabled?]
  (e/client
    (let [!mode (atom :overview)
          mode (e/watch !mode)
          !browse-nav (atom nil)
          browse-nav (e/watch !browse-nav)
          !queue-idx (atom 0)
          ;; Reactive watch — fires when "Open" or "View Source" sets nav-target
          nav-val (e/watch !nav-target)]

      ;; Consume nav-target reactively: switch to browse mode when set
      (when (and (map? nav-val) (:doc-id nav-val))
        (reset! !browse-nav nav-val)
        (reset! !mode :browse)
        (reset! !nav-target nil))

      (case mode
        :overview
        (LearnOverview user-id !mode)

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
            llm-enabled?))))))
