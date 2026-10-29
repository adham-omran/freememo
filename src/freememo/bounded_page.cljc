(ns freememo.bounded-page
  "Fixed pages keep mounted rows and transferred rows bounded."
  (:require [hyperfiddle.electric3 :as e]
            [hyperfiddle.electric-dom3 :as dom]))

(defn page-index [requested total size]
  (min (max 0 (long (or requested 0)))
       (max 0 (quot (dec (max 1 total)) size))))

(defn page-items [items requested size]
  (let [items (vec items)
        start (* size (page-index requested (count items) size))]
    (subvec items start (min (count items) (+ start size)))))

(e/defn PageControls [!page total size label]
  (e/client
   (let [page (page-index (e/watch !page) total size)
         last-page (page-index total total size)]
     (when (> total size)
       (dom/nav
        (dom/props {:class "audit-page-controls" :aria-label label})
        (dom/button
         (dom/props {:type "button" :class "btn btn-sm" :disabled (zero? page)})
         (dom/text "Previous")
         (dom/On "click" (fn [_] (reset! !page (max 0 (dec page)))) nil))
        (dom/span (dom/props {:role "status"})
                  (dom/text (str (inc (* size page)) "–" (min total (* size (inc page))) " of " total)))
        (dom/button
         (dom/props {:type "button" :class "btn btn-sm" :disabled (= page last-page)})
         (dom/text "Next")
         (dom/On "click" (fn [_] (reset! !page (min last-page (inc page)))) nil)))))))
