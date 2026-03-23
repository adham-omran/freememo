(ns electric-starter-app.library-page
  "Library tab — knowledge tree view with filter."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [electric-starter-app.contents-page :refer [DocumentTreeView]]))

(e/defn LibraryPage [user-id !nav-target navigate! !refresh]
  (e/client
    (let [!filter-text (atom "")
          filter-text (e/watch !filter-text)]
      (dom/div
        (dom/props {:class "page-container"
                    :style {:height "100%" :display "flex" :flex-direction "column"}})

        ;; Header row: search filter
        (dom/div
          (dom/props {:style {:display "flex" :align-items "center" :gap "12px"
                              :margin-bottom "12px"}})

          (dom/input
            (dom/props {:type "text" :placeholder "Filter documents..."
                        :class "input" :style {:flex "1" :max-width "400px"}})
            (dom/On "input" (fn [e] (reset! !filter-text (-> e .-target .-value))) nil)))

        ;; Tree view
        (DocumentTreeView user-id !nav-target navigate! !refresh filter-text)))))
