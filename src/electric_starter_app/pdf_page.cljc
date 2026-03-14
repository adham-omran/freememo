(ns electric-starter-app.pdf-page
  "PDF upload and management UI."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.electric-scroll0 :refer [Scroll-window]]
   [contrib.data :refer [clamp-left]]
   #?(:clj [electric-starter-app.pdf :as pdf])
   #?(:clj [clojure.string :as str])))

#?(:clj (defonce !refresh (atom 0))) ; Server-side refresh trigger

;; Query wrapper: takes refresh arg to create Electric reactive dependency
#?(:clj (defn list-pdfs* [_refresh user-id] (pdf/list-pdfs user-id)))

;; Helper: format bytes to human-readable
#?(:clj
   (defn format-bytes [bytes]
     (if (nil? bytes)
       "?"
       (cond
         (< bytes 1024) (str bytes " B")
         (< bytes (* 1024 1024)) (format "%.1f KB" (double (/ bytes 1024)))
         :else (format "%.1f MB" (double (/ bytes 1024 1024)))))))

;; Helper: format timestamp to "2026-03-14 10:11"
#?(:clj
   (defn format-timestamp [ts]
     (when ts
       (let [fmt (java.time.format.DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm")]
         (.format (.toLocalDateTime ts) fmt)))))

#?(:clj
   (defn filter-docs [docs filter-text]
     (if (or (nil? filter-text) (str/blank? filter-text))
       docs
       (let [q (str/lower-case (str/trim filter-text))]
         (filterv #(str/includes? (str/lower-case (or (:documents/filename %) "")) q) docs)))))

;; Main PDF page component
(e/defn PdfPage [user-id !nav-target navigate!]
  (e/client
    (dom/div
      (dom/props {:style {:padding "16px" :max-width "900px" :height "100%" :display "flex" :flex-direction "column"}})
      (dom/h2
        (dom/props {:style {:margin "0 0 12px 0" :font-size "20px"}})
        (dom/text "PDF Documents"))

      ;; Upload form
      (let [!file-input (atom nil)]
        (dom/form
          (dom/props {:action "/api/upload-pdf" :method "post" :enctype "multipart/form-data"})
          (dom/input
            (dom/props {:type "file" :name "file" :accept "application/pdf"
                        :style {:display "none"}})
            (reset! !file-input dom/node)
            (dom/On "change" (fn [e] (.. e -target -form submit)) nil))
          (dom/button
            (dom/props {:type "button"
                        :style {:padding "8px 20px" :background "#2563eb" :color "white"
                                :border "none" :border-radius "6px" :cursor "pointer"
                                :font-size "14px" :font-weight "600" :margin-bottom "12px"}})
            (dom/text "Upload PDF")
            (dom/On "click" (fn [_] (when-some [inp @!file-input] (.click inp))) nil))))

      ;; Search filter
      (let [!filter-text (atom "")
            filter-text (e/watch !filter-text)]
        (dom/input
          (dom/props {:type "text" :placeholder "Filter documents..."
                      :style {:width "100%" :max-width "400px" :padding "8px 12px"
                              :margin-bottom "12px" :border "1px solid #ccc" :border-radius "4px"
                              :font-size "14px"}})
          (dom/On "input" (fn [e] (reset! !filter-text (-> e .-target .-value))) nil))

        ;; Document table with virtual scroll
        (e/server
          (let [refresh (e/watch !refresh)
                docs-result (list-pdfs* refresh user-id)]
            (e/client
              (if (:success docs-result)
                (let [docs-vec (e/server (vec (filter-docs (:documents docs-result) filter-text)))
                      doc-count (e/server (count docs-vec))
                      row-height 40]
                  (dom/div
                    (dom/props {:style {:flex "1" :display "flex" :flex-direction "column" :min-height "0"}})

                    ;; Table header
                    (dom/table
                      (dom/props {:style {:width "100%" :border-collapse "collapse" :font-size "14px" :table-layout "fixed" :flex-shrink "0"}})
                      (dom/thead
                        (dom/tr
                          (dom/th (dom/props {:style {:text-align "left" :padding "8px 10px" :border-bottom "2px solid #e0e0e0" :font-weight "600" :color "#444"}}) (dom/text "Name"))
                          (dom/th (dom/props {:style {:text-align "right" :padding "8px 10px" :border-bottom "2px solid #e0e0e0" :font-weight "600" :color "#444" :width "80px"}}) (dom/text "Size"))
                          (dom/th (dom/props {:style {:text-align "left" :padding "8px 10px" :border-bottom "2px solid #e0e0e0" :font-weight "600" :color "#444" :width "140px"}}) (dom/text "Uploaded"))
                          (dom/th (dom/props {:style {:text-align "center" :padding "8px 10px" :border-bottom "2px solid #e0e0e0" :font-weight "600" :color "#444" :width "80px"}}) (dom/text "Actions")))))

                    (if (pos? doc-count)
                      ;; Scrollable body
                      (dom/div
                        (dom/props {:style {:flex "1" :overflow-y "auto" :min-height "0"}})
                        (let [[offset limit] (Scroll-window row-height doc-count dom/node {:overquery-factor 1})
                              occluded-height (clamp-left (* row-height (- doc-count limit)) 0)]
                          (dom/table
                            (dom/props {:style {:width "100%" :border-collapse "collapse" :font-size "14px" :table-layout "fixed"}})
                            (dom/tbody
                              (dom/props {:style {:position "relative" :top (str (* offset row-height) "px")}})
                              (e/for [i (e/diff-by {} (range offset (+ offset limit)))]
                                (let [item (e/server (nth docs-vec i nil))]
                                  (when item
                                    (let [id (e/server (:documents/id item))
                                          filename (e/server (:documents/filename item))
                                          file-size (e/server (format-bytes (:documents/file_size item)))
                                          uploaded (e/server (format-timestamp (:documents/uploaded_at item)))]
                                      (dom/tr
                                        (dom/props {:style {:border-bottom "1px solid #f0f0f0" :height (str row-height "px")}})
                                        ;; Name — clickable link
                                        (dom/td
                                          (dom/props {:style {:padding "8px 10px" :overflow "hidden" :text-overflow "ellipsis" :white-space "nowrap"}})
                                          (dom/span
                                            (dom/props {:style {:color "#2563eb" :cursor "pointer" :text-decoration "underline"}
                                                        :title "Open in Learn tab"})
                                            (dom/text filename)
                                            (dom/On "click"
                                              (fn [_]
                                                (reset! !nav-target {:doc-id id})
                                                (navigate! :learn))
                                              nil)))
                                        ;; Size
                                        (dom/td
                                          (dom/props {:style {:padding "8px 10px" :text-align "right" :color "#555" :width "80px"}})
                                          (dom/text file-size))
                                        ;; Uploaded
                                        (dom/td
                                          (dom/props {:style {:padding "8px 10px" :color "#555" :width "140px"}})
                                          (dom/text (or uploaded "-")))
                                        ;; Delete
                                        (dom/td
                                          (dom/props {:style {:padding "8px 10px" :text-align "center" :width "80px"}})
                                          (let [!deleting (atom false)
                                                deleting (e/watch !deleting)]
                                            (dom/button
                                              (dom/props {:disabled deleting
                                                          :style {:padding "3px 10px" :background (if deleting "#999" "#dc3545")
                                                                  :color "white" :border "none" :border-radius "3px"
                                                                  :cursor (if deleting "not-allowed" "pointer")
                                                                  :font-size "12px"}})
                                              (dom/text (if deleting "..." "Delete"))
                                              (let [click-event (dom/On "click" (fn [_] id) nil)
                                                    [?token ?error] (e/Token click-event)]
                                                (when ?error
                                                  (dom/span
                                                    (dom/props {:style {:color "red" :font-size "11px" :margin-left "4px"}})
                                                    (dom/text ?error)))
                                                (when-some [token ?token]
                                                  (reset! !deleting true)
                                                  (let [result (e/server (pdf/delete-pdf user-id click-event))]
                                                    (if (:success result)
                                                      (do (e/server (swap! !refresh inc))
                                                        (token))
                                                      (do (reset! !deleting false)
                                                        (token (:error result)))))))))))))))))
                          (dom/div (dom/props {:style {:height (str occluded-height "px")}}))))

                      ;; Empty state
                      (dom/p
                        (dom/props {:style {:color "#888" :font-size "14px" :padding "16px 0"}})
                        (dom/text "No documents match.")))))

                (dom/div
                  (dom/props {:style {:color "red"}})
                  (dom/text "Error loading documents: " (:error docs-result)))))))))))
