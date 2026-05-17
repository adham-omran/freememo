(ns freememo.content-toolbar-extract
  "Done/Restore and Delete buttons for extract topics in ContentToolbar.
   Extracted to stay under JVM 64KB method limit."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.card-components :as card-components]
   [freememo.keyboard :as keyboard]
   [freememo.navigation :as nav]
   #?(:clj [taoensso.telemere :as tel])
   #?(:clj [freememo.db :as db])
   #?(:clj [freememo.user-state :as us])))

(defn delete-topic-with-parent!
  "Server-only: snapshot parent_id + anki note ids, delete the topic, bump
   :tree-mutations, and return {:parent-id _ :note-ids _}. Returns parent_id
   captured BEFORE deletion so the client can navigate up.

   Intentionally does NOT bump :refresh — TopicPage watches :refresh, and
   bumping it during the same WS tick as this fn's return propagates an
   overview re-query that unmounts the toolbar subtree before the client-side
   handler can call navigate!, stranding the user on the now-deleted topic's
   URL. :tree-mutations is watched only by HierarchySidePanel and
   DocumentTreeView (per CLAUDE.md), so bumping it is safe."
  [user-id topic-id]
  #?(:clj
     (let [pid (:topics/parent_id (db/get-topic topic-id))
           note-ids (vec (db/get-anki-note-ids topic-id))]
       (db/delete-topic! topic-id)
       (swap! (us/get-atom user-id :tree-mutations) inc)
       (tel/log! :info (str "Topic deleted topic-id=" topic-id " parent-id=" pid))
       {:parent-id pid :note-ids note-ids})
     :cljs nil))

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
        ;; !delete-state tracks ONLY the modal: nil | :confirming.
        ;; Navigation happens directly in the click handler (matching the
        ;; Done/Restore pattern). A reactive watch on a post-delete :deleted
        ;; status would race with TopicPage's :refresh-driven re-query — the
        ;; overview becomes nil after delete, extract-status becomes nil, this
        ;; whole subtree unmounts, the atom is destroyed before its watch can
        ;; fire, and navigation never runs (URL stays on the deleted topic).
        (let [!delete-state (atom nil)
              delete-state (e/watch !delete-state)]
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
                        ;; result MUST be inside when-some so e/server stays
                        ;; mounted until the response arrives. The (when (some?
                        ;; result) ...) guard waits for the server roundtrip
                        ;; before firing any side effects — without it, (token)
                        ;; closes ?token, the subtree unmounts, and the cond
                        ;; never gets to see the resolved value.
                        (let [result (e/server (delete-topic-with-parent! user-id topic-id))]
                          (when (some? result)
                            (e/client (card-components/try-delete-anki-notes! (:note-ids result)))
                            (token)
                            (reset! !delete-state nil)
                            (when navigate!
                              (cond
                                (:parent-id result) (navigate! :viewer (nav/nav-topic (:parent-id result) origin))
                                origin (navigate! origin)
                                :else (navigate! :library)))))))))))))))))
