(ns freememo.content-toolbar-actions
  "Extract, Add, Export, Anki Sync, Done/Delete buttons for ContentToolbar.
   Extracted to stay under JVM 64KB method limit."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [clojure.string :as str]
   [freememo.rich-text-editor :as editor]
   [freememo.anki-sync :refer [AnkiSyncButton]]
   [freememo.card-modals :refer [ExportModal AddCardModal]]
   [freememo.content-toolbar-helpers :as helpers]
   [freememo.keyboard :as keyboard]
   [freememo.logging :as log]
   [freememo.card-components :as card-components]
   #?(:clj [freememo.db :as db])))

(e/defn ToolbarActions [cfg]
  (e/client
    (let [{:keys [user-id topic-id root-topic-id page-number
                  context-mode mod-key source-ref unsynced-count
                  card-type extract-status navigate! origin]} cfg]

      ;; Extract button — create child topic from selected text
      (let [!extract-state (atom {:pending nil :error nil})
            extract-state (e/watch !extract-state)
            pending (:pending extract-state)]
        (dom/button
          (dom/props {:class "btn btn-sm btn-primary"
                      :style {:font-weight "500"}
                      :title (if (= context-mode :extract)
                               (str "Extract selected text as a child topic (" mod-key "+Shift+E)")
                               (str "Extract selected text as a topic (" mod-key "+Shift+E)"))})
          (dom/text "Extract")
          (reset! keyboard/!extract-btn-ref dom/node)
          (e/on-unmount (fn [] (reset! keyboard/!extract-btn-ref nil)))
          (dom/On "click"
            (fn [_]
              (when-let [{:keys [html index length]} (editor/get-selection-html!)]
                (when (seq html)
                  (editor/highlight-range! index length)
                  (swap! !extract-state assoc :pending html :error nil))))
            nil))
        (when (:error extract-state)
          (dom/span
            (dom/props {:style {:color "var(--color-danger)" :font-size "12px"}})
            (dom/text (:error extract-state))))
        (let [[?token _] (e/Token pending)]
          (when-some [token ?token]
            (let [title (let [raw (str/replace (or pending "") #"<[^>]+>" "")]
                          (if (str/blank? raw) "Extract" (subs raw 0 (min 80 (count raw)))))
                  result (e/server
                           (helpers/create-extract-topic-safe! topic-id user-id pending title))]
              (if (:success result)
                (do (reset! !extract-state {:pending nil :error nil})
                  (token))
                (do (reset! !extract-state {:pending nil :error (or (:error result) "Failed to save extract")})
                  (token (or (:error result) "Failed to save extract"))))))))

      ;; Done/Restore button — only for extract topics (not PDF pages)
      (when (some? extract-status)
        (if (= extract-status "active")
          ;; Active: show Done button
          (let [!done-click (atom nil)
                done-click (e/watch !done-click)]
            (dom/button
              (dom/props {:class "btn btn-sm btn-secondary"
                          :style {:color "var(--color-success-dark)" :border "1px solid var(--color-success-dark)"
                                  :font-weight "500"}
                          :title "Mark as fully processed (extracted/carded everything useful)"})
              (dom/text "Done")
              (reset! keyboard/!done-btn-ref dom/node)
              (e/on-unmount (fn [] (reset! keyboard/!done-btn-ref nil)))
              (dom/On "click" (fn [_] (reset! !done-click (str (random-uuid)))) nil))
            (let [[?token _] (e/Token done-click)]
              (when-some [token ?token]
                (e/server (db/done-topic! topic-id))
                (token)
                (when navigate! (navigate! (or origin :learn))))))

          ;; Done status: show Restore button
          (let [!restore-click (atom nil)
                restore-click (e/watch !restore-click)]
            (dom/button
              (dom/props {:class "btn btn-sm btn-secondary"
                          :style {:color "var(--color-primary)" :border "1px solid var(--color-primary)"
                                  :font-weight "500"}
                          :title "Restore to active review queue"})
              (dom/text "Restore")
              (reset! keyboard/!done-btn-ref dom/node)
              (e/on-unmount (fn [] (reset! keyboard/!done-btn-ref nil)))
              (dom/On "click" (fn [_] (reset! !restore-click (str (random-uuid)))) nil))
            (let [[?token _] (e/Token restore-click)]
              (when-some [token ?token]
                (e/server (db/restore-topic! topic-id))
                (token)
                (when navigate! (navigate! (or origin :learn))))))))

      ;; Delete button — hidden, triggered via overflow menu proxy
      (when (some? extract-status)
        (let [!delete-state (atom nil)
              delete-state (e/watch !delete-state)]
          ;; Reactive navigation — fires when delete completes
          (when (= delete-state :deleted)
            (when navigate! (navigate! (or origin :learn))))
          (dom/button
            (dom/props {:class "btn btn-sm btn-danger-fill toolbar-overflow-item"
                        :style {:padding "4px 10px" :font-size "12px"}
                        :title "Delete this extract and its cards"})
            (dom/text "Delete")
            (reset! keyboard/!delete-btn-ref dom/node)
            (e/on-unmount (fn [] (reset! keyboard/!delete-btn-ref nil)))
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
                          (reset! !delete-state :deleted)))))))))))

      ;; Add new card button
      (let [!show-add (atom false)
            show-add (e/watch !show-add)]
        (dom/button
          (dom/props {:class "btn btn-sm btn-secondary toolbar-overflow-item" :style {:font-weight "500"}})
          (dom/text "Add new")
          (reset! keyboard/!add-new-btn-ref dom/node)
          (e/on-unmount (fn [] (reset! keyboard/!add-new-btn-ref nil)))
          (dom/On "click" (fn [_] (reset! !show-add true)) nil))
        (when show-add
          (AddCardModal !show-add card-type topic-id root-topic-id source-ref user-id)))

      ;; Separator
      (dom/span (dom/props {:class "toolbar-overflow-item" :style {:color "var(--color-border)"}}) (dom/text "|"))

      ;; Export button + modal
      (let [!show-export (atom false)
            show-export (e/watch !show-export)]
        (dom/button
          (dom/props {:class "btn btn-sm btn-secondary toolbar-overflow-item" :style {:font-weight "500"}})
          (dom/text (if (pos? unsynced-count)
                      (str "Export (" unsynced-count ")...")
                      "Export..."))
          (reset! keyboard/!export-btn-ref dom/node)
          (e/on-unmount (fn [] (reset! keyboard/!export-btn-ref nil)))
          (dom/On "click" (fn [_] (reset! !show-export true)) nil))
        (when show-export
          (ExportModal !show-export topic-id root-topic-id user-id)))

      ;; Separator
      (dom/span (dom/props {:class "toolbar-overflow-item" :style {:color "var(--color-border)"}}) (dom/text "|"))

      ;; Anki Sync button
      (AnkiSyncButton user-id root-topic-id page-number card-type unsynced-count))))
