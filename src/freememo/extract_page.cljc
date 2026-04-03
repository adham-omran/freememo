(ns freememo.extract-page
  "Dedicated extract view — centered rich text editor + card generation toolbar + card table."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.logging :as log]
   [freememo.rich-text-editor :as editor]
   [freememo.rich-text-editor-component :refer [RichTextEditorComponent]]
   [freememo.content-toolbar :refer [ContentToolbar]]
   [freememo.content-toolbar-helpers :as ct-helpers]
   [freememo.anki-sync-panels :as sync-panels]
   #?(:clj [freememo.user-state :as us])
   [freememo.content-card-table :refer [ContentCardTable]]
   [freememo.util :refer [start-drag!]]
   #?(:clj [freememo.db :as db])
   #?(:clj [freememo.settings :as settings])
   [freememo.keyboard :as keyboard]
   [freememo.card-components :as card-components]))

;; Per-user refresh via user-state registry

;; Query wrapper: takes refresh arg to create Electric reactive dependency
#?(:clj (defn get-topic-by-id* [_refresh topic-id]
          (when topic-id (db/get-topic topic-id))))


;; Responsive split pane default — plain defn avoids #? inside e/defn (frame mismatch)
(defn default-split-pct []
  #?(:cljs (if (< (.-innerHeight js/window) 900) 50 75)
     :clj 75))

(e/defn ExtractPage [user-id enc-key topic-id navigate! view-source! llm-enabled? origin]
  (e/client
    (dom/div
      (dom/props {:style {:height "100%" :display "flex" :flex-direction "column" :overflow "hidden"}})

      (if (some? topic-id)
        (let [card-font-size (e/server (settings/get-card-font-size user-id))
              refresh (e/server (e/watch (us/get-atom user-id :refresh)))
              card-gen-status (e/server (e/watch (us/get-atom user-id :card-gen-status)))
              sync-mutations (e/server (e/watch (us/get-atom user-id :sync-mutations)))
              card-mutations (e/server (e/watch (us/get-atom user-id :card-mutations)))
              refresh (+ refresh (hash card-gen-status) sync-mutations card-mutations)
              topic (e/server (get-topic-by-id* refresh topic-id))
              content (e/server (or (:topics/content topic) ""))
              extract-status (e/server (or (:topics/status topic) "active"))
              ;; Parent topic — for page_number and parent content (context)
              parent-id (e/server (:topics/parent_id topic))
              parent-topic (e/server (when parent-id (db/get-topic parent-id)))
              page-number (e/server (:topics/page_number parent-topic))
              parent-title (e/server (:topics/title parent-topic))
              parent-content (e/server (or (:topics/content parent-topic) ""))
              ;; Root topic — for filename, kind, source scoping
              root-topic-id (e/server (db/get-root-topic-id topic-id))
              root-topic (e/server (when (not= root-topic-id topic-id) (db/get-topic root-topic-id)))
              filename (e/server (:topics/title (or root-topic topic)))
              root-kind (e/server (:topics/kind (or root-topic topic)))]

          ;; Auto-save dirty edits to topic content
          ;; Auto-save: one-way editor → DB. Don't clear !dirty-html, don't bump !refresh.
          ;; Track last-saved to prevent re-saving identical content.
          (let [dirty-data (e/watch editor/!dirty-html)
                !last-saved (atom nil)]
            (when (and (some? dirty-data)
                    (= (:topic-id dirty-data) topic-id)
                    (not= (:html dirty-data) (e/watch !last-saved)))
              (log/log-debug (str "Extract auto-save topic-id=" topic-id))
              (let [html-to-save (:html dirty-data)
                    result (e/server
                             (e/Offload
                               #(try
                                  (db/update-topic-content! topic-id html-to-save)
                                  {:success true}
                                  (catch Exception e
                                    (log/log-error (str "Extract save error: " (.getMessage e)))
                                    {:success false :error (.getMessage e)}))))]
                (when (:success result)
                  (reset! !last-saved html-to-save)))))


          ;; Header: breadcrumb + back button (hidden when embedded in learn session)
          (when navigate!
            (dom/div
              (dom/props {:style {:display "flex" :align-items "center" :gap "var(--sp-3)"
                                  :padding "var(--sp-2) var(--sp-4)" :flex-shrink "0"
                                  :border-bottom "1px solid var(--color-border)"}})
              (dom/button
                (dom/props {:class "btn btn-sm btn-secondary" :style {:background "var(--color-bg-subtle)"}})
                (dom/text (case origin :queue "Back to Queue" :library "Back to Library" "Back to Learn"))
                (dom/On "click" (fn [_] (navigate! (or origin :learn))) nil))
              (if (= extract-status "active")
                ;; Active: show Done
                (dom/span
                  (dom/props {:style {:display "contents"}})
                  (dom/button
                    (dom/props {:class "btn btn-sm btn-secondary"
                                :style {:color "var(--color-success-dark)" :border "1px solid var(--color-success-dark)"}
                                :title "Mark as fully processed (extracted/carded everything useful)"})
                    (dom/text "Done")
                    (reset! keyboard/!done-btn-ref dom/node)
                    (e/on-unmount (fn [] (reset! keyboard/!done-btn-ref nil)))
                    (let [event (dom/On "click" (fn [_] (str (random-uuid))) nil)
                          [?token _] (e/Token event)]
                      (when-some [token ?token]
                        (e/server (db/done-topic! topic-id))
                        (token)
                        (navigate! (or origin :learn))))))

                ;; Done: show Restore
                (dom/span
                  (dom/props {:style {:display "contents"}})
                  (dom/span
                    (dom/props {:style {:font-size "12px" :color "var(--color-success-dark)" :font-weight "600"}})
                    (dom/text extract-status))
                  (dom/button
                    (dom/props {:class "btn btn-sm btn-secondary"
                                :style {:color "var(--color-primary)" :border "1px solid var(--color-primary)"}
                                :title "Restore to active review queue"})
                    (dom/text "Restore")
                    (let [event (dom/On "click" (fn [_] (str (random-uuid))) nil)
                          [?token _] (e/Token event)]
                      (when-some [token ?token]
                        (e/server (db/restore-topic! topic-id))
                        (token)
                        (navigate! (or origin :learn)))))))
              (let [!delete-state (atom nil)
                    delete-state (e/watch !delete-state)]
                ;; Reactive navigation — fires when delete completes
                (when (= delete-state :deleted)
                  (navigate! (or origin :learn)))
                (dom/button
                  (dom/props {:class "btn btn-sm btn-danger-fill" :style {:padding "4px 10px" :font-size "12px"}
                              :title "Delete this extract and its cards"})
                  (dom/text "Delete")
                  (dom/On "click" (fn [_] (reset! !delete-state :confirming)) nil))
                (when (= delete-state :confirming)
                  (dom/div
                    (dom/props {:class "modal-backdrop"})
                    (dom/On "click" (fn [_] (reset! !delete-state nil)) nil)
                    (dom/On "keydown" (fn [e] (when (= (.-key e) "Escape") (reset! !delete-state nil))) nil)
                    (dom/div
                      (dom/props {:class "modal-content modal-sm"})
                      (dom/On "click" (fn [e] (.stopPropagation e)) nil)
                      (dom/div
                        (dom/props {:class "confirm-modal-body"})
                        (dom/p (dom/text "Delete this extract and all its cards?")))
                      (dom/div
                        (dom/props {:class "confirm-modal-actions"})
                        (dom/button
                          (dom/props {:class "btn btn-secondary"})
                          (dom/text "Cancel")
                          (dom/On "click" (fn [_] (reset! !delete-state nil)) nil))
                        (dom/button
                          (dom/props {:class "btn btn-danger-fill"})
                          (dom/text "Delete")
                          (let [event (dom/On "click" (fn [_] :confirmed) nil)
                                [?token _] (e/Token event)]
                            (when-some [token ?token]
                              (let [note-ids (e/server (db/get-anki-note-ids topic-id))]
                                (e/server (db/delete-topic! topic-id))
                                (e/client (card-components/try-delete-anki-notes! note-ids))
                                (log/log-info (str "Topic deleted topic-id=" topic-id))
                                (token)
                                (reset! !delete-state :deleted))))))))))
              (let [parent-is-intermediate (and parent-id root-topic-id (not= parent-id root-topic-id))
                    label (if parent-is-intermediate
                            (or parent-title "Untitled")
                            (if (nil? page-number)
                              (or filename "Untitled")
                              (str (or filename "Unknown") " \u2014 p. " page-number)))]
                (if (and view-source! parent-id)
                  (dom/span
                    (dom/props {:style {:color "var(--color-primary)" :font-size "13px" :cursor "pointer"
                                        :text-decoration "underline"}
                                :title "View source"})
                    (dom/text label)
                    (dom/On "click" (fn [_] (view-source! (if (and parent-is-intermediate (not= root-kind "pdf")) parent-id root-topic-id) page-number root-kind)) nil))
                  (dom/span
                    (dom/props {:style {:color "var(--color-text-secondary)" :font-size "13px"}})
                    (dom/text label))))))

          ;; Split pane: editor top / toolbar+cards bottom
          (let [!top-pct (atom (default-split-pct))
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
                                          :topic-id topic-id})))

            ;; Draggable divider
            (dom/div
              (dom/props {:class "split-divider-v" :title "Drag to resize panels"})
              (dom/On "pointerdown" (fn [e] (start-drag! e :y !top-pct)) nil))

            ;; Bottom: toolbar + cards
            (dom/div
              (dom/props {:style {:flex "1" :display "flex" :flex-direction "column" :min-height "0" :overflow "hidden"}})

              ;; Shared toolbar — content-text from !dirty-html (live) or server content (initial)
              (let [dirty (e/watch editor/!dirty-html)
                    live-content (if (and dirty (= (:topic-id dirty) topic-id))
                                   (:html dirty)
                                   content)]
                (ContentToolbar {:user-id user-id
                                 :enc-key enc-key
                                 :topic-id topic-id
                                 :root-topic-id root-topic-id
                                 :page-number page-number
                                 :content-text live-content
                                 :parent-content parent-content
                                 :context-mode :extract
                                 :context-tooltip "Include context for better cards. With a selection: extract text. Without: original page text."
                                 :llm-enabled? llm-enabled?}
                  refresh))

              ;; Shared card table
              (ContentCardTable {:topic-id topic-id
                                 :card-font-size card-font-size
                                 :user-id user-id}
                refresh))))

        ;; No topic-id
        (dom/div
          (dom/props {:style {:padding "32px" :text-align "center" :color "var(--color-text-secondary)"}})
          (dom/text "No extract selected.")
          (dom/div
            (dom/props {:style {:margin-top "12px"}})
            (dom/button
              (dom/props {:class "btn btn-sm btn-secondary" :style {:padding "6px 16px" :background "var(--color-bg-subtle)"}})
              (dom/text "Go to Learn")
              (dom/On "click" (fn [_] (navigate! (or origin :learn))) nil))))))))
