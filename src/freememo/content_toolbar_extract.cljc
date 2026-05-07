(ns freememo.content-toolbar-extract
  "Done/Restore and Delete buttons for extract topics in ContentToolbar.
   Extracted to stay under JVM 64KB method limit."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.logging :as log]
   [freememo.card-components :as card-components]
   [freememo.keyboard :as keyboard]
   #?(:clj [freememo.db :as db])
   #?(:clj [freememo.user-state :as us])))

(e/defn ExtractActions [cfg]
  (e/client
    (let [{:keys [user-id topic-id extract-status navigate! origin]} cfg]

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
                          (e/server (swap! (us/get-atom user-id :refresh) inc))
                          (e/client (card-components/try-delete-anki-notes! note-ids))
                          (log/log-info (str "Topic deleted topic-id=" topic-id))
                          (token)
                          (reset! !delete-state :deleted))))))))))))))
