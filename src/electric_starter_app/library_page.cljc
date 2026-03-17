(ns electric-starter-app.library-page
  "Library tab — merged document list + contents tree with view toggle."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [electric-starter-app.pdf-page :refer [DocumentListView]]
   [electric-starter-app.contents-page :refer [DocumentTreeView]]))

(e/defn LibraryPage [user-id !nav-target navigate! !refresh]
  (e/client
    (let [!view-mode (atom :list)
          view-mode (e/watch !view-mode)
          !filter-text (atom "")
          filter-text (e/watch !filter-text)]
      (dom/div
        (dom/props {:style {:padding "var(--sp-4)" :max-width "900px" :height "100%"
                            :display "flex" :flex-direction "column"}})

        ;; Header row: title + toggle
        (dom/div
          (dom/props {:style {:display "flex" :align-items "center" :justify-content "space-between"
                              :margin-bottom "12px"}})
          (dom/h2
            (dom/props {:style {:margin "0" :font-size "20px"}})
            (dom/text "Library"))

          ;; Segmented toggle
          (dom/div
            (dom/props {:style {:display "flex" :border "1px solid var(--color-border)"
                                :border-radius "var(--radius-md)" :overflow "hidden"}})
            (dom/button
              (dom/props {:style {:padding "4px 12px" :font-size "12px" :border "none" :cursor "pointer"
                                  :font-weight (if (= view-mode :list) "600" "400")
                                  :background (if (= view-mode :list) "var(--color-primary)" "transparent")
                                  :color (if (= view-mode :list) "#fff" "var(--color-text-secondary)")}})
              (dom/text "List")
              (dom/On "click" (fn [_] (reset! !view-mode :list)) nil))
            (dom/button
              (dom/props {:style {:padding "4px 12px" :font-size "12px" :border "none" :cursor "pointer"
                                  :border-left "1px solid var(--color-border)"
                                  :font-weight (if (= view-mode :tree) "600" "400")
                                  :background (if (= view-mode :tree) "var(--color-primary)" "transparent")
                                  :color (if (= view-mode :tree) "#fff" "var(--color-text-secondary)")}})
              (dom/text "Tree")
              (dom/On "click" (fn [_] (reset! !view-mode :tree)) nil))))

        ;; Search filter
        (dom/input
          (dom/props {:type "text" :placeholder "Filter documents..."
                      :class "input" :style {:width "100%" :max-width "400px" :margin-bottom "var(--sp-3)"}})
          (dom/On "input" (fn [e] (reset! !filter-text (-> e .-target .-value))) nil))

        ;; View content
        (if (= view-mode :list)
          (DocumentListView user-id !nav-target navigate! !refresh filter-text)
          (DocumentTreeView user-id !nav-target navigate! !refresh filter-text))))))
