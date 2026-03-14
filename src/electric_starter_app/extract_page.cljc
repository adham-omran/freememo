(ns electric-starter-app.extract-page
  "Dedicated extract view — centered rich text editor + card generation toolbar + card table."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [electric-starter-app.rich-text-editor :as editor]
   [electric-starter-app.rich-text-editor-component :refer [RichTextEditorComponent]]
   [electric-starter-app.extract-toolbar :refer [ExtractToolbar]]
   [electric-starter-app.extract-cards :as extract-cards :refer [ExtractCardTable]]
   [electric-starter-app.ocr-page :refer [start-drag!]]
   #?(:clj [electric-starter-app.db :as db])))

(e/defn ExtractPage [user-id enc-key content-item-id navigate!]
  (e/client
    (dom/div
      (dom/props {:style {:height "100%" :display "flex" :flex-direction "column" :overflow "hidden"}})

      (if (some? content-item-id)
        (let [item (e/server (when content-item-id (db/get-content-item-by-id content-item-id)))
              doc-id (e/server (:content_items/document_id item))
              page-num (e/server (:content_items/page_number item))
              content (e/server (or (:content_items/content item) ""))
              filename (e/server
                         (when doc-id
                           (-> (db/get-documents-by-id user-id doc-id)
                             first
                             :documents/filename)))]

          ;; Auto-save dirty edits to content_items table
          (let [dirty-data (e/watch editor/!dirty-html)]
            (when (some? dirty-data)
              (e/server
                (e/Offload
                  #(try
                     (db/update-content-item-content content-item-id (:html dirty-data))
                     {:success true}
                     (catch Exception e
                       (println "ERROR [extract-page save]:" (.getMessage e))
                       {:success false}))))))

          ;; Header: breadcrumb + back button (hidden when embedded in learn session)
          (when navigate!
            (dom/div
              (dom/props {:style {:display "flex" :align-items "center" :gap "12px"
                                  :padding "8px 16px" :flex-shrink "0"
                                  :border-bottom "1px solid #e0e0e0"}})
              (dom/button
                (dom/props {:style {:padding "4px 12px" :background "#f0f0f0" :border "1px solid #ccc"
                                    :border-radius "4px" :cursor "pointer" :font-size "13px"}})
                (dom/text "Back to Learn")
                (dom/On "click" (fn [_] (navigate! :learn)) nil))
              (dom/span
                (dom/props {:style {:color "#888" :font-size "13px"}})
                (dom/text (str (or filename "Unknown") " — p. " page-num)))))

          ;; Split pane: editor top / toolbar+cards bottom
          (let [!top-pct (atom 60)
                top-pct (e/watch !top-pct)]

            ;; Editor area
            (dom/div
              (dom/props {:style {:height (str top-pct "%") :min-height "0" :overflow "auto"
                                  :display "flex" :justify-content "center"
                                  :padding "24px 16px"}})
              (dom/div
                (dom/props {:style {:width "100%" :max-width "800px"
                                    :display "flex" :flex-direction "column"}})
                (RichTextEditorComponent {:initial-html content
                                          :page-number page-num
                                          :doc-id doc-id})))

            ;; Draggable divider
            (dom/div
              (dom/props {:class "split-divider-v" :title "Drag to resize panels"})
              (dom/On "mousedown" (fn [e] (start-drag! e :y !top-pct)) nil))

            ;; Bottom: toolbar + cards
            (dom/div
              (dom/props {:style {:flex "1" :display "flex" :flex-direction "column" :min-height "0" :overflow "hidden"}})

              ;; Toolbar
              (ExtractToolbar {:user-id user-id
                               :enc-key enc-key
                               :doc-id doc-id
                               :page-number page-num
                               :content-text content
                               :content-item-id content-item-id})

              ;; Card table
              (ExtractCardTable content-item-id))))

        ;; No content-item-id
        (dom/div
          (dom/props {:style {:padding "32px" :text-align "center" :color "#888"}})
          (dom/text "No extract selected.")
          (dom/div
            (dom/props {:style {:margin-top "12px"}})
            (dom/button
              (dom/props {:style {:padding "6px 16px" :background "#f0f0f0" :border "1px solid #ccc"
                                  :border-radius "4px" :cursor "pointer"}})
              (dom/text "Go to Learn")
              (dom/On "click" (fn [_] (navigate! :learn)) nil))))))))
