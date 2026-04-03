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
   :total-pages (or (:total_pages row) 0)
   :ocrd-pages (or (:ocrd_pages row) 0)
   :done-pages (or (:done_pages row) 0)
   :total-cards (or (:total_cards row) 0)
   :synced-cards (or (:synced_cards row) 0)})

(defn done-pct [{:keys [done-pages total-pages]}]
  (if (pos? total-pages)
    (/ (double done-pages) total-pages)
    0.0))

(defn ocr-pct [{:keys [ocrd-pages total-pages]}]
  (if (pos? total-pages)
    (/ (double ocrd-pages) total-pages)
    0.0))

(defn synced-pct [{:keys [synced-cards total-cards]}]
  (if (pos? total-cards)
    (/ (double synced-cards) total-cards)
    0.0))

(defn completion-status [{:keys [done-pages total-pages]}]
  (cond
    (zero? total-pages) :not-started
    (= done-pages total-pages) :complete
    (zero? done-pages) :not-started
    :else :in-progress))

(defn apply-filters [rows kind-filter status-filter]
  (cond->> rows
    (not= kind-filter "all")
    (filter #(= (:kind %) kind-filter))
    (not= status-filter "all")
    (filter #(= (completion-status %) (keyword status-filter)))))

(defn sort-rows [rows sort-col sort-dir]
  (let [key-fn (case sort-col
                 :document :title
                 :ocrd ocr-pct
                 :cards :total-cards
                 :done done-pct
                 :synced synced-pct
                 done-pct)
        ;; For pct columns, items where the denominator is 0 (N/A) sort to bottom
        na? (case sort-col
              :ocrd   #(zero? (:total-pages %))
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
      (dom/props {:style {:padding "16px 24px" :display "flex" :flex-direction "column" :height "100%" :min-height "0"}})

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
            grid-cols "1fr 90px 70px 90px 90px"]

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
            (dom/props {:style {:padding "4px 8px" :font-size "13px" :border "1px solid var(--color-border)"
                                :border-radius "4px" :background "var(--color-bg-primary)" :color "var(--color-text-primary)"}})
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
            (dom/props {:style {:padding "4px 8px" :font-size "13px" :border "1px solid var(--color-border)"
                                :border-radius "4px" :background "var(--color-bg-primary)" :color "var(--color-text-primary)"}})
            (dom/option (dom/props {:value "all"}) (dom/text "All statuses"))
            (dom/option (dom/props {:value "not-started"}) (dom/text "Not Started"))
            (dom/option (dom/props {:value "in-progress"}) (dom/text "In Progress"))
            (dom/option (dom/props {:value "complete"}) (dom/text "Complete"))
            (e/for [[t ev] (dom/On-all "change")]
              (when ev (reset! !status-filter (-> ev .-target .-value)))
              (t))))

        (if (pos? item-count)
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
                      (dom/text (str "OCR'd" (arrow :ocrd)))
                      (dom/On "click" (fn [_] (toggle-sort! :ocrd)) nil))
                    (dom/th
                      (dom/props {:style (merge th-base {:text-align "center"})})
                      (dom/text (str "Cards" (arrow :cards)))
                      (dom/On "click" (fn [_] (toggle-sort! :cards)) nil))
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
                (dom/props {:style {:flex "1" :overflow-y "auto" :min-height "0"}})
                (let [[offset limit] (Scroll-window row-height item-count dom/node {:overquery-factor 1})
                      occluded-height (clamp-left (* row-height (- item-count limit)) 0)]
                  (dom/props {:class "tape-scroll"
                              :style {:--offset offset :--row-height (str row-height "px")}})
                  (dom/table
                    (dom/props {:style {:width "100%" :display "grid" :grid-template-columns grid-cols :font-size "13px"}})
                    (e/for [i (Tape offset limit)]
                      (let [item (nth sorted i nil)]
                        (when item
                          (let [{:keys [id title kind total-pages ocrd-pages done-pages total-cards synced-cards]} item
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
                              ;; OCR'd
                              (dom/td
                                (dom/props {:style {:padding "4px 6px" :text-align "center" :color "var(--color-text-secondary)"}})
                                (dom/text (if is-pdf?
                                            (str ocrd-pages " / " total-pages)
                                            "\u2013")))
                              ;; Cards
                              (dom/td
                                (dom/props {:style {:padding "4px 6px" :text-align "center" :color "var(--color-text-secondary)"}})
                                (dom/text (str total-cards)))
                              ;; Done
                              (dom/td
                                (dom/props {:style {:padding "4px 6px" :text-align "center"
                                                    :color (cond
                                                             (and (pos? total-pages) (= done-pages total-pages)) "var(--color-success-dark)"
                                                             (pos? done-pages) "var(--color-text-primary)"
                                                             :else "var(--color-text-secondary)")}})
                                (dom/text (str done-pages " / " total-pages)))
                              ;; Synced
                              (dom/td
                                (dom/props {:style {:padding "4px 6px" :text-align "center" :color "var(--color-text-secondary)"}})
                                (dom/text (if (pos? total-cards)
                                            (str synced-cards " / " total-cards)
                                            "\u2013")))))))))
                  (dom/div (dom/props {:style {:height (str occluded-height "px")}}))))))

          ;; Empty state
          (dom/div
            (dom/props {:style {:display "flex" :align-items "center" :justify-content "center"
                                :height "100%" :color "var(--color-text-secondary)" :font-size "15px"}})
            (dom/text "No documents yet")))))))
