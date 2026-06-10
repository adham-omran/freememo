(ns freememo.library-page
  "Library tab — documents tree or all-cards view, toggled via URL sub-route.
   /library/documents = knowledge tree, /library/cards = LibraryCardsView.
   Bare /library is a legacy alias for the documents tree."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.router5 :as r]
   [freememo.knowledge-tree :refer [DocumentTreeView]]
   [freememo.library-cards :refer [LibraryCardsView LibraryViewToggle]]))

(e/defn LibraryPage [user-id navigate! refresh]
  (e/client
    (let [[sub] r/route
          cards-view? (= 'cards sub)]
      (dom/div
        (dom/props {:class "page-container"
                    :style {:height "100%" :display "flex" :flex-direction "column"}})
        (if cards-view?
          (LibraryCardsView user-id navigate! refresh)
          (let [!filter-text (atom "")
                filter-text (e/watch !filter-text)
                !kind-filter (atom "all")
                kind-filter (e/watch !kind-filter)
                !status-filter (atom "all")
                status-filter (e/watch !status-filter)
                !sort-col (atom :added)
                sort-col (e/watch !sort-col)
                !sort-dir (atom :desc)
                sort-dir (e/watch !sort-dir)
                !tree-expanded (atom false)
                tree-expanded (e/watch !tree-expanded)
                ;; Sort command from column header clicks — [col default-dir]
                !sort-cmd (atom nil)
                sort-cmd (e/watch !sort-cmd)
                ;; React to header sort clicks — must be referenced to avoid Electric optimizing it away
                sort-applied (when (some? sort-cmd)
                               (let [[col default-dir] sort-cmd]
                                 (if (= col sort-col)
                                   (reset! !sort-dir (if (= sort-dir :asc) :desc :asc))
                                   (do (reset! !sort-col col)
                                     (reset! !sort-dir default-dir)))
                                 (reset! !sort-cmd nil)
                                 true))]

            ;; Reference sort-applied so Electric evaluates the binding
            (when sort-applied nil)

            ;; Header row: view toggle + search + filters + expand toggle
            (dom/div
              (dom/props {:style {:display "flex" :align-items "center" :gap "12px"
                                  :margin-bottom "12px" :flex-wrap "wrap"}})

              (LibraryViewToggle navigate! false)

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

              (dom/button
                (dom/props {:class "input"
                            :style {:flex-shrink "0" :cursor "pointer"}})
                (dom/text (if tree-expanded "Collapse All" "Expand All"))
                (dom/On "click" (fn [_] (swap! !tree-expanded not)) nil)))

            ;; Tree view
            (DocumentTreeView user-id navigate! refresh filter-text
              sort-col sort-dir kind-filter status-filter tree-expanded !sort-cmd)))))))
