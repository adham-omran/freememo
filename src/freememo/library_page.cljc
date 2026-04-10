(ns freememo.library-page
  "Library tab — knowledge tree view with filter."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.knowledge-tree :refer [DocumentTreeView]]))

(e/defn LibraryPage [user-id navigate! refresh]
  (e/client
    (let [!filter-text (atom "")
          filter-text (e/watch !filter-text)
          !sort-key (atom "recent")
          sort-key (e/watch !sort-key)
          !kind-filter (atom "all")
          kind-filter (e/watch !kind-filter)
          !status-filter (atom "all")
          status-filter (e/watch !status-filter)]
      (dom/div
        (dom/props {:class "page-container"
                    :style {:height "100%" :display "flex" :flex-direction "column"}})

        ;; Header row: search + filters + sort
        (dom/div
          (dom/props {:style {:display "flex" :align-items "center" :gap "12px"
                              :margin-bottom "12px" :flex-wrap "wrap"}})

          (dom/input
            (dom/props {:type "text" :placeholder "Filter documents..."
                        :class "input" :style {:flex "1" :min-width "140px"}})
            (dom/On "input" (fn [e] (reset! !filter-text (-> e .-target .-value))) nil))

          (dom/select
            (dom/props {:class "input"})
            (dom/option (dom/props {:value "all"}) (dom/text "All kinds"))
            (dom/option (dom/props {:value "pdf"}) (dom/text "PDF"))
            (dom/option (dom/props {:value "epub"}) (dom/text "EPUB"))
            (dom/option (dom/props {:value "web"}) (dom/text "Web"))
            (dom/option (dom/props {:value "markdown"}) (dom/text "Markdown"))
            (dom/On "change" (fn [e] (reset! !kind-filter (-> e .-target .-value))) nil))

          (dom/select
            (dom/props {:class "input"})
            (dom/option (dom/props {:value "all"}) (dom/text "All statuses"))
            (dom/option (dom/props {:value "not-started"}) (dom/text "Not Started"))
            (dom/option (dom/props {:value "in-progress"}) (dom/text "In Progress"))
            (dom/option (dom/props {:value "complete"}) (dom/text "Complete"))
            (dom/option (dom/props {:value "unsynced"}) (dom/text "Unsynced"))
            (dom/On "change" (fn [e] (reset! !status-filter (-> e .-target .-value))) nil))

          (dom/select
            (dom/props {:class "input" :style {:width "160px" :flex-shrink "0"}})
            (dom/option (dom/props {:value "recent"}) (dom/text "Recently added"))
            (dom/option (dom/props {:value "oldest"}) (dom/text "Oldest first"))
            (dom/option (dom/props {:value "alpha"}) (dom/text "Alphabetical"))
            (dom/option (dom/props {:value "done"}) (dom/text "Least done"))
            (dom/option (dom/props {:value "synced"}) (dom/text "Least synced"))
            (dom/On "change" (fn [e] (reset! !sort-key (-> e .-target .-value))) nil)))

        ;; Tree view
        (DocumentTreeView user-id navigate! refresh filter-text sort-key kind-filter status-filter)))))
