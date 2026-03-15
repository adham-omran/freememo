(ns electric-starter-app.extract-page
  "Dedicated extract view — centered rich text editor + card generation toolbar + card table."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [electric-starter-app.rich-text-editor :as editor]
   [electric-starter-app.rich-text-editor-component :refer [RichTextEditorComponent]]
   [electric-starter-app.content-toolbar :refer [ContentToolbar]]
   [electric-starter-app.content-card-table :refer [ContentCardTable]]
   [electric-starter-app.ocr-page :refer [start-drag!]]
   #?(:clj [electric-starter-app.db :as db])
   [electric-starter-app.card-components :as card-components]))

#?(:clj (defonce !refresh (atom 0)))

(e/defn ExtractPage [user-id enc-key content-item-id navigate! view-source! llm-enabled?]
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
              (dom/button
                (dom/props {:style {:padding "4px 10px" :background "#dc3545" :color "white"
                                    :border "none" :border-radius "4px" :cursor "pointer"
                                    :font-size "12px"}
                            :title "Delete this extract and its cards"})
                (dom/text "Delete")
                (let [event (dom/On "click"
                              (fn [_]
                                #?(:cljs
                                   (when (js/confirm "Delete this extract and all its cards?")
                                     :delete)
                                   :clj nil))
                              nil)
                      [?token _error] (e/Token event)]
                  (when-some [token ?token]
                    (let [note-ids (e/server (db/get-anki-note-ids-for-content-item content-item-id))]
                      (e/server (db/delete-content-item content-item-id))
                      (e/client (card-components/try-delete-anki-notes! note-ids))
                      (token)
                      (navigate! :learn)))))
              (if view-source!
                (dom/span
                  (dom/props {:style {:color "#2563eb" :font-size "13px" :cursor "pointer"
                                      :text-decoration "underline"}
                              :title "View source PDF page"})
                  (dom/text (str (or filename "Unknown") " — p. " page-num))
                  (dom/On "click" (fn [_] (view-source! doc-id page-num)) nil))
                (dom/span
                  (dom/props {:style {:color "#888" :font-size "13px"}})
                  (dom/text (str (or filename "Unknown") " — p. " page-num))))))

          ;; Split pane: editor top / toolbar+cards bottom
          (let [!top-pct (atom 75)
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

              ;; Shared toolbar
              (ContentToolbar {:user-id user-id
                               :enc-key enc-key
                               :doc-id doc-id
                               :page-number page-num
                               :content-text content
                               :content-item-id content-item-id
                               :context-mode :extract
                               :context-tooltip "Include context for better cards. With a selection: extract text. Without: original page text."
                               :llm-enabled? llm-enabled?}
                !refresh)

              ;; Shared card table
              (ContentCardTable {:query-mode :extract
                                 :content-item-id content-item-id}
                !refresh))))

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
