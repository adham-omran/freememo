(ns freememo.merged-window
  "Page ranges and save ownership for bounded Focus reader bodies.")

(def page-limit 20)
(def max-page-limit 40)

(defn page-range [page total]
  (let [total (max 1 (or total 1))
        page (min total (max 1 (or page 1)))
        start (inc (* page-limit (quot (dec page) page-limit)))]
    [start (min total (+ start (dec page-limit)))]))

(def overlap-pages 10)
(def edge-pages 4)

(defn continuous-range
  "Keep the current page inside an overlapping bounded range."
  [previous page total]
  (let [total (max 1 (or total 1))
        page (min total (max 1 (or page 1)))
        [start end] previous
        current-size (when (and start end) (inc (- end start)))
        valid? (and start end (<= start page end) (<= end total)
                    (<= (min page-limit total) current-size max-page-limit))
        window-size (if valid? current-size page-limit)
        next-start (cond
                     (not valid?) (max 1 (- page overlap-pages))
                     (and (< end total) (> page (- end edge-pages))) (+ start overlap-pages)
                     :else start)
        next-start (max 1 (min next-start (max 1 (inc (- total window-size)))))]
    [next-start (min total (+ next-start (dec window-size)))]))

(defonce !viewport-edge (atom nil))
(defonce !blocked-range (atom nil))

(defn edge-range [previous page total edge]
  (let [[start end] previous
        valid? (and (= previous (:range edge)) (= page (:page edge)))
        window-size (inc (- end start))
        grow? (and valid? (= :forward (:direction edge)) (= start page)
                   (< window-size max-page-limit))
        window-size (if grow?
                      (min max-page-limit (max window-size (or (:capacity edge) page-limit)))
                      window-size)
        next-start (if (and valid? (not grow?))
                     (case (:direction edge)
                       :forward (min (+ start overlap-pages) page)
                       :backward (- start overlap-pages)
                       start)
                     start)
        next-start (max 1 (min next-start (max 1 (inc (- total window-size)))))]
    [next-start (min total (+ next-start (dec window-size)))]))

(defn continuous-range!
  "Retain range identity across ordinary page reports and viewport edges."
  ([!range root-id page total] (continuous-range! !range root-id page total nil))
  ([!range root-id page total edge]
   (let [state @!range
         previous (when (= root-id (:root-id state)) (:range state))
         next-range (continuous-range previous page total)
         next-range (if (and (= previous next-range) (= root-id (:root-id edge))
                             (integer? page) (pos? (or total 0)))
                      (edge-range next-range page total edge)
                      next-range)]
     (reset! !range {:root-id root-id :range next-range})
     next-range)))

(defn body-key [doc-id merged? page-numbers]
  [doc-id (boolean merged?) (when merged? (vec page-numbers))])

(defn install-allowed? [dirty saved-edit-id]
  (or (nil? dirty) (and (:edit-id dirty) (= (:edit-id dirty) saved-edit-id))))

(defn edit-entry [editor-state html edit-id]
  (let [scope (:save-scope editor-state)]
    {:html html :topic-id (or (:editor-doc-id scope) (:topic-id editor-state))
     :save-scope scope :edit-id edit-id}))

(defn save-needed? [dirty user-id saved-edit-id local-saved-id]
  (and (some? dirty)
       (= user-id (get-in dirty [:save-scope :user-id]))
       (some? (:edit-id dirty))
       (not= (:edit-id dirty) saved-edit-id)
       (not= (:edit-id dirty) local-saved-id)))

(defn snapshot-tag [request]
  (select-keys request [:user-id :root-id :range :fence]))

(defn read-request!
  "Read a changed window after its latest edit saves. Freeze installed requests."
  [!request user-id root-id page-range installed-scope dirty saved-edit-id]
  (let [dirty-scope (:save-scope dirty)
        dirty (when (and (= user-id (:user-id dirty-scope))
                         (= root-id (or (:pdf-root-id dirty-scope) (:source-root-id dirty-scope)))) dirty)
        previous @!request
        pages (:page-numbers installed-scope)
        installed-key [(:user-id installed-scope) (:pdf-root-id installed-scope) [(first pages) (last pages)]]
        requested-key [user-id root-id page-range]
        previous-key [(:user-id previous) (:root-id previous) (:range previous)]
        request (if (= requested-key installed-key previous-key)
                  previous
                  {:user-id user-id :root-id root-id :range page-range :fence (:edit-id dirty)
                   :ready? (install-allowed? dirty saved-edit-id)})]
    (reset! !request request)
    request))

(defn snapshot-current? [received expected]
  (= received expected))
