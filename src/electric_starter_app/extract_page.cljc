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
   #?(:clj [electric-starter-app.settings :as settings])
   [electric-starter-app.card-components :as card-components]))

#?(:clj (defonce !refresh (atom 0)))

(e/defn ExtractPage [user-id enc-key content-item-id navigate! view-source! llm-enabled?]
  (e/client
    (dom/div
      (dom/props {:style {:height "100%" :display "flex" :flex-direction "column" :overflow "hidden"}})

      (if (some? content-item-id)
        (let [card-font-size (e/server (settings/get-card-font-size user-id))
              item (e/server (when content-item-id (db/get-content-item-by-id content-item-id)))
              doc-id (e/server (:content_items/document_id item))
              page-num (e/server (:content_items/page_number item))
              content (e/server (or (:content_items/content item) ""))
              extract-status (e/server (or (:content_items/status item) "active"))
              filename (e/server
                         (when doc-id
                           (-> (db/get-documents-by-id user-id doc-id)
                             first
                             :documents/filename)))]

          ;; Clear stale dirty-html from other pages/components on mount
          (e/client (reset! editor/!dirty-html nil))

          ;; Auto-save dirty edits to content_items table
          ;; Guard: only save when the edit belongs to THIS extract (not stale page-level edits)
          (let [dirty-data (e/watch editor/!dirty-html)]
            (when (and (some? dirty-data)
                       (= (:content-item-id dirty-data) content-item-id))
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
              (dom/props {:style {:display "flex" :align-items "center" :gap "var(--sp-3)"
                                  :padding "var(--sp-2) var(--sp-4)" :flex-shrink "0"
                                  :border-bottom "1px solid var(--color-border)"}})
              (dom/button
                (dom/props {:class "btn btn-sm btn-secondary" :style {:background "#f0f0f0"}})
                (dom/text "Back to Learn")
                (dom/On "click" (fn [_] (navigate! :learn)) nil))
              (if (= extract-status "active")
                ;; Active: show Done
                (dom/span
                  (dom/props {:style {:display "contents"}})
                  (dom/button
                    (dom/props {:class "btn btn-sm btn-secondary"
                                :style {:color "#16a34a" :border "1px solid #16a34a"}
                                :title "Mark as fully processed (extracted/carded everything useful)"})
                    (dom/text "Done")
                    (let [event (dom/On "click" (fn [_] (str (random-uuid))) nil)
                          [?token _] (e/Token event)]
                      (when-some [token ?token]
                        (e/server (db/done-topic "extract" content-item-id))
                        (token)
                        (navigate! :learn))))
)
                ;; Done: show Restore
                (dom/span
                  (dom/props {:style {:display "contents"}})
                  (dom/span
                    (dom/props {:style {:font-size "12px" :color "#16a34a" :font-weight "600"}})
                    (dom/text extract-status))
                  (dom/button
                    (dom/props {:class "btn btn-sm btn-secondary"
                                :style {:color "var(--color-primary)" :border "1px solid var(--color-primary)"}
                                :title "Restore to active review queue"})
                    (dom/text "Restore")
                    (let [event (dom/On "click" (fn [_] (str (random-uuid))) nil)
                          [?token _] (e/Token event)]
                      (when-some [token ?token]
                        (e/server (db/restore-topic "extract" content-item-id))
                        (token)
                        (navigate! :learn))))))
              (dom/button
                (dom/props {:class "btn btn-sm btn-danger-fill" :style {:padding "4px 10px" :font-size "12px"}
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
                  (dom/props {:style {:color "var(--color-primary)" :font-size "13px" :cursor "pointer"
                                      :text-decoration "underline"}
                              :title "View source PDF page"})
                  (dom/text (str (or filename "Unknown") " — p. " page-num))
                  (dom/On "click" (fn [_] (view-source! doc-id page-num)) nil))
                (dom/span
                  (dom/props {:style {:color "var(--color-text-secondary)" :font-size "13px"}})
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
                (dom/props {:style {:width "100%"
                                    :display "flex" :flex-direction "column"}})
                (RichTextEditorComponent {:initial-html content
                                          :page-number page-num
                                          :doc-id doc-id
                                          :content-item-id content-item-id})))

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
                                 :content-item-id content-item-id
                                 :card-font-size card-font-size}
                !refresh))))

        ;; No content-item-id
        (dom/div
          (dom/props {:style {:padding "32px" :text-align "center" :color "var(--color-text-secondary)"}})
          (dom/text "No extract selected.")
          (dom/div
            (dom/props {:style {:margin-top "12px"}})
            (dom/button
              (dom/props {:class "btn btn-sm btn-secondary" :style {:padding "6px 16px" :background "#f0f0f0"}})
              (dom/text "Go to Learn")
              (dom/On "click" (fn [_] (navigate! :learn)) nil))))))))
