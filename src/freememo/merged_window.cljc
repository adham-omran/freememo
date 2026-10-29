(ns freememo.merged-window
  "Page ranges and save ownership for bounded Focus reader bodies.")

(def page-limit 20)

(defn page-range [page total]
  (let [total (max 1 (or total 1))
        page (min total (max 1 (or page 1)))
        start (inc (* page-limit (quot (dec page) page-limit)))]
    [start (min total (+ start (dec page-limit)))]))

(defn body-key [doc-id merged? page-numbers]
  [doc-id (boolean merged?) (when merged? (vec page-numbers))])

(defn install-allowed? [dirty saved-edit-id]
  (or (nil? dirty) (and (:edit-id dirty) (= (:edit-id dirty) saved-edit-id))))

(defn edit-entry [editor-state html edit-id]
  (let [scope (:save-scope editor-state)]
    {:html html :topic-id (or (:editor-doc-id scope) (:topic-id editor-state))
     :save-scope scope :edit-id edit-id}))
