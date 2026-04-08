(ns freememo.status-page
  "Status tab — progress overview for all documents."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.electric-scroll0 :refer [Scroll-window Tape]]
   [contrib.data :refer [clamp-left]]
   [freememo.navigation :as nav]
   #?(:clj [freememo.user-state :as us])
   #?(:clj [freememo.db :as db])))

(defn kind-badge [kind]
  (case kind
    "pdf" ["PDF" "var(--color-badge-pdf)"]
    "epub" ["EPUB" "var(--color-badge-epub)"]
    ("web" "wikipedia") ["Web" "var(--color-badge-web)"]
    "markdown" ["MD" "var(--color-badge-web)"]
    ["Doc" "var(--color-badge-epub)"]))

(defn parse-row [row]
  {:id (:topics/id row)
   :title (or (:topics/title row) "Untitled")
   :kind (or (:topics/kind row) "basic")
   :created-at (:topics/created_at row)
   :total-items (or (:total_items row) 0)
   :done-items (or (:done_items row) 0)
   :total-cards (or (:total_cards row) 0)
   :synced-cards (or (:synced_cards row) 0)})

(defn done-pct [{:keys [done-items total-items]}]
  (if (pos? total-items)
    (/ (double done-items) total-items)
    0.0))

(defn synced-pct [{:keys [synced-cards total-cards]}]
  (if (pos? total-cards)
    (/ (double synced-cards) total-cards)
    0.0))

(defn completion-status [{:keys [done-items total-items total-cards]}]
  (cond
    (= done-items total-items) :complete
    (or (pos? done-items) (pos? total-cards)) :in-progress
    :else :not-started))

(defn unsynced? [{:keys [total-cards synced-cards]}]
  (and (pos? total-cards) (< synced-cards total-cards)))

(defn apply-filters [rows kind-filter status-filter]
  (cond->> rows
    (not= kind-filter "all")
    (filter #(= (:kind %) kind-filter))
    (= status-filter "unsynced")
    (filter unsynced?)
    (and (not= status-filter "all") (not= status-filter "unsynced"))
    (filter #(= (completion-status %) (keyword status-filter)))))

(defn sort-rows [rows sort-col sort-dir]
  (let [key-fn (case sort-col
                 :document :title
                 :done done-pct
                 :synced synced-pct
                 done-pct)
        ;; For pct columns, items where the denominator is 0 (N/A) sort to bottom
        na? (case sort-col
              :synced #(zero? (:total-cards %))
              nil)
        cmp (if (= sort-dir :asc) compare (fn [a b] (compare b a)))]
    (if na?
      (let [{applicable false not-applicable true} (group-by na? rows)]
        (into (vec (sort-by key-fn cmp applicable)) not-applicable))
      (sort-by key-fn cmp rows))))

(defn get-document-status* [_refresh user-id]
  #?(:clj (mapv parse-row (db/get-document-status user-id))
     :cljs []))

(e/defn StatusPage [user-id navigate!]
  (e/client
    (dom/div
      (dom/props {:class "page-container"
                  :style {:display "flex" :flex-direction "column" :height "100%"}})

      (let [refresh (e/server (e/watch (us/get-atom user-id :refresh)))
            all-rows (e/server (get-document-status* refresh user-id))
            !kind-filter (atom "all")
            kind-filter (e/watch !kind-filter)
            !status-filter (atom "all")
            status-filter (e/watch !status-filter)
            !sort-col (atom :done)
            sort-col (e/watch !sort-col)
            !sort-dir (atom :asc)
            sort-dir (e/watch !sort-dir)
            filtered (apply-filters all-rows kind-filter status-filter)
            sorted (vec (sort-rows filtered sort-col sort-dir))
            item-count (count sorted)
            row-height 36
            grid-cols "1fr 90px 90px"]

        ;; Header row: title + filters
        (dom/div
          (dom/props {:style {:display "flex" :align-items "center" :gap "12px" :margin-bottom "12px" :flex-shrink "0"}})
          (dom/span
            (dom/props {:style {:font-size "16px" :font-weight "600" :color "var(--color-text-primary)"}})
            (dom/text "Progress"))
          (dom/span
            (dom/props {:style {:color "var(--color-text-secondary)" :font-size "13px"}})
            (dom/text (str item-count " document" (when (not= item-count 1) "s"))))

          ;; Spacer
          (dom/div (dom/props {:style {:flex "1"}}))

          ;; Kind filter
          (dom/select
            (dom/props {:class "input"})
            (dom/option (dom/props {:value "all"}) (dom/text "All kinds"))
            (dom/option (dom/props {:value "pdf"}) (dom/text "PDF"))
            (dom/option (dom/props {:value "epub"}) (dom/text "EPUB"))
            (dom/option (dom/props {:value "web"}) (dom/text "Web"))
            (dom/option (dom/props {:value "markdown"}) (dom/text "Markdown"))
            (e/for [[t ev] (dom/On-all "change")]
              (when ev (reset! !kind-filter (-> ev .-target .-value)))
              (t)))

          ;; Status filter
          (dom/select
            (dom/props {:class "input"})
            (dom/option (dom/props {:value "all"}) (dom/text "All statuses"))
            (dom/option (dom/props {:value "not-started"}) (dom/text "Not Started"))
            (dom/option (dom/props {:value "in-progress"}) (dom/text "In Progress"))
            (dom/option (dom/props {:value "complete"}) (dom/text "Complete"))
            (dom/option (dom/props {:value "unsynced"}) (dom/text "Unsynced"))
            (e/for [[t ev] (dom/On-all "change")]
              (when ev (reset! !status-filter (-> ev .-target .-value)))
              (t))))

        (let [th-base {:padding "8px 6px" :border-bottom "2px solid var(--color-border)"
                       :font-weight "600" :font-size "13px" :color "var(--color-text-primary)" :cursor "pointer" :user-select "none"}
              arrow (fn [col] (when (= sort-col col) (if (= sort-dir :asc) " \u25B2" " \u25BC")))
              toggle-sort! (fn [col]
                             (if (= col @!sort-col)
                               (swap! !sort-dir #(if (= % :asc) :desc :asc))
                               (do (reset! !sort-col col)
                                 (reset! !sort-dir :asc))))]
          (dom/div
            (dom/props {:style {:display "flex" :flex-direction "column" :min-height "0" :flex "1"}})

            ;; Fixed header
            (dom/table
              (dom/props {:style {:width "100%" :display "grid" :grid-template-columns grid-cols :flex-shrink "0"}})
              (dom/thead
                (dom/props {:style {:display "contents"}})
                (dom/tr
                  (dom/props {:style {:display "contents"}})
                  (dom/th
                    (dom/props {:style (merge th-base {:text-align "left" :padding "8px 10px"})})
                    (dom/text (str "Document" (arrow :document)))
                    (dom/On "click" (fn [_] (toggle-sort! :document)) nil))
                  (dom/th
                    (dom/props {:style (merge th-base {:text-align "center"})})
                    (dom/text (str "Done" (arrow :done)))
                    (dom/On "click" (fn [_] (toggle-sort! :done)) nil))
                  (dom/th
                    (dom/props {:style (merge th-base {:text-align "center"})})
                    (dom/text (str "Synced" (arrow :synced)))
                    (dom/On "click" (fn [_] (toggle-sort! :synced)) nil)))))

            ;; Scrollable body
            (dom/div
              (dom/props {:style {:flex "1" :overflow-y "auto" :min-height "0" :scrollbar-gutter "stable"}})
              (let [[offset limit] (Scroll-window row-height item-count dom/node {:overquery-factor 1})
                    occluded-height (clamp-left (* row-height (- item-count limit)) 0)]
                (dom/props {:class "tape-scroll"
                            :style {:--offset offset :--row-height (str row-height "px")}})
                (dom/table
                  (dom/props {:style {:width "100%" :display "grid" :grid-template-columns grid-cols :font-size "13px"}})
                  (if (pos? item-count)
                    (e/for [i (Tape offset limit)]
                      (let [item (nth sorted i nil)]
                        (when item
                          (let [{:keys [id title kind total-items done-items total-cards synced-cards]} item
                                [badge-text badge-color] (kind-badge kind)
                                is-pdf? (= kind "pdf")]
                            (dom/tr
                              (dom/props {:style {:border-bottom "1px solid var(--color-bg-subtle)" :height (str row-height "px")
                                                  :cursor "pointer" :--order (inc i)}})
                              (dom/On "click"
                                (fn [_]
                                  (if is-pdf?
                                    (navigate! :viewer (nav/nav-browse-pdf id 1 :status))
                                    (navigate! :viewer (nav/nav-browse-topic id :status))))
                                nil)
                              ;; Document + badge
                              (dom/td
                                (dom/props {:style {:padding "4px 10px" :overflow "hidden" :text-overflow "ellipsis"
                                                    :white-space "nowrap" :display "flex" :align-items "center" :gap "8px"}})
                                (dom/span
                                  (dom/props {:class "type-badge" :style {:background badge-color :flex-shrink "0"}})
                                  (dom/text badge-text))
                                (dom/span
                                  (dom/props {:style {:overflow "hidden" :text-overflow "ellipsis" :white-space "nowrap"}
                                              :data-tooltip title})
                                  (dom/text title)))
                              ;; Done
                              (dom/td
                                (dom/props {:style {:padding "4px 6px" :text-align "center"
                                                    :color (cond
                                                             (and (pos? total-items) (= done-items total-items)) "var(--color-success-dark)"
                                                             (pos? done-items) "var(--color-text-primary)"
                                                             :else "var(--color-text-secondary)")}})
                                (dom/text (str done-items " / " total-items)))
                              ;; Synced
                              (dom/td
                                (dom/props {:style {:padding "4px 6px" :text-align "center" :color "var(--color-text-secondary)"}})
                                (dom/text (if (pos? total-cards)
                                            (str synced-cards " / " total-cards)
                                            "\u2013"))))))))
                    (dom/tr
                      (dom/td
                        (dom/props {:style {:grid-column "1 / -1" :text-align "center" :padding "24px 12px"
                                            :color "var(--color-text-secondary)" :font-size "13px"}})
                        (dom/text "No documents yet")))))
                (dom/div (dom/props {:style {:height (str occluded-height "px")}}))))))))))
