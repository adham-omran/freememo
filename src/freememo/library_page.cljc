(ns freememo.library-page
  "Library tab — knowledge tree view with filter."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.knowledge-tree :refer [DocumentTreeView]]))

(e/defn LibraryPage [user-id !nav-target navigate! !refresh]
  (e/client
    (let [!filter-text (atom "")
          filter-text (e/watch !filter-text)
          !sort-key (atom "recent")
          sort-key (e/watch !sort-key)]
      (dom/div
        (dom/props {:class "page-container"
                    :style {:height "100%" :display "flex" :flex-direction "column"}})

        ;; Header row: search filter + sort
        (dom/div
          (dom/props {:style {:display "flex" :align-items "center" :gap "12px"
                              :margin-bottom "12px"}})

          (dom/input
            (dom/props {:type "text" :placeholder "Filter documents..."
                        :class "input" :style {:flex "1"}})
            (dom/On "input" (fn [e] (reset! !filter-text (-> e .-target .-value))) nil))

          (dom/select
            (dom/props {:class "input" :style {:width "160px" :flex-shrink "0"}})
            (dom/option (dom/props {:value "recent"}) (dom/text "Recently added"))
            (dom/option (dom/props {:value "oldest"}) (dom/text "Oldest first"))
            (dom/option (dom/props {:value "alpha"}) (dom/text "Alphabetical"))
            (dom/On "change" (fn [e] (reset! !sort-key (-> e .-target .-value))) nil)))

        ;; Tree view
        (DocumentTreeView user-id !nav-target navigate! !refresh filter-text sort-key)))))
