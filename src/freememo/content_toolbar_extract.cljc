(ns freememo.content-toolbar-extract
  "Done/Restore, History, and Delete buttons for topics in ContentToolbar.
   Extracted to stay under JVM 64KB method limit."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.card-components :as card-components]
   [freememo.history-modal :refer [HistoryModal]]
   [freememo.icons :as icons]
   [freememo.keyboard :as keyboard]
   [freememo.navigation :as nav]
   #?(:clj [freememo.db :as db])
   #?(:clj [freememo.staged-delete :as staged-delete])
   #?(:clj [freememo.user-state :as us])))

(defn delete-topic-with-parent!
  "Server-only: stage the topic (subtree + cards) for deletion, returning
   {:parent-id _ :note-ids _ :entry-id _} for the client's navigate-up + Anki
   cleanup. Reversible for 12h via the undo log.

   Stages via freememo.staged-delete/stage-deletion! with refresh? false:
   TopicPage watches :refresh, and bumping it during the same WS tick as this
   fn's return propagates an overview re-query that unmounts the toolbar
   subtree before the client handler can call navigate!, stranding the user on
   the now-hidden topic's URL. :tree-mutations (bumped inside stage-deletion!)
   is watched only by HierarchySidePanel and DocumentTreeView, so it is safe."
  [user-id topic-id]
  #?(:clj (or (staged-delete/stage-deletion! user-id topic-id false)
            {:parent-id nil :note-ids []})
     :cljs nil))

(e/defn ExtractActions [cfg]
  (e/client
    (let [{:keys [user-id topic-id root-topic-id extract-status navigate! origin on-done!]} cfg
          ;; Local modal state — CLJS-side atom flipped by the History button
          ;; and the modal's own dismiss handlers (backdrop click / Escape / X).
          !history-open? (atom false)
          ;; Reactive :refresh value powers the modal's re-query on session
          ;; mutations (advance/touch/postpone/done/restore/priority-change all
          ;; bump :refresh, so the table updates immediately after each event).
          history-refresh (e/server (e/watch (us/get-atom user-id :refresh)))
          ;; History scopes to the document, not the page. In PDF context
          ;; (extract-status nil per content_toolbar.cljc's pass-through),
          ;; :root-topic-id is the PDF root (topic_page.cljc:518 passes
          ;; `(or pdf-root-id root-topic-id)`). For extract/wiki/epub topics
          ;; (extract-status non-nil), each topic has its own rep schedule —
          ;; show its own history.
          history-topic-id (if extract-status topic-id (or root-topic-id topic-id))]

      ;; History button — visible for all topic kinds (per spec §8.1). Sits
      ;; immediately before Done/Restore so when extract-status is set the
      ;; order reads History → Done; on PDF roots (extract-status nil) it
      ;; stands alone.
      (dom/button
        (dom/props {:class "btn btn-sm btn-secondary"
                    :style {:font-weight "500"}
                    :aria-label "History"
                    :data-tooltip "View repetition history for this topic"})
        (icons/Icon :history :size 16)
        (dom/span (dom/props {:class "icon-label"}) (dom/text "History"))
        (dom/On "click" (fn [_] (reset! !history-open? true)) nil))

      ;; Modal mounted unconditionally — its body is gated on `@!history-open?`,
      ;; so an unopened modal is a cheap no-op in the reactive graph.
      (HistoryModal history-topic-id !history-open? history-refresh)

      ;; Done/Restore button — only for extract topics (not PDF pages).
      ;; `busy` holds the active branch open during the queue-advance transition:
      ;; the :refresh bump (after server done!) propagates faster than the queue
      ;; advance, so without the hold, the button visibly flips Done→Restore for
      ;; a frame before the next topic mounts. Mirrors the Next button's
      ;; !busy/:disabled pattern (learn_session.cljc:99-116). In doc-context
      ;; (no on-done!) busy stays false; branch flip happens normally.
      (when (some? extract-status)
        (let [!busy (atom false)
              busy (e/watch !busy)]
          (if (or busy (= extract-status "active"))
            ;; Active (or transitioning): show Done button
            (let [!done-click (atom nil)
                  done-click (e/watch !done-click)]
              (dom/button
                (dom/props {:class "btn btn-sm btn-secondary"
                            :style {:color "var(--color-success-dark)" :border "1px solid var(--color-success-dark)"
                                    :font-weight "500"}
                            :disabled busy
                            :aria-label "Done"
                            :data-tooltip "Mark as fully processed (extracted/carded everything useful)"})
                (icons/Icon :check :size 16)
                (dom/span (dom/props {:class "icon-label"}) (dom/text "Done"))
                (reset! keyboard/!done-btn-ref dom/node)
                (e/on-unmount (fn [] (reset! keyboard/!done-btn-ref nil)))
                (dom/On "click" (fn [_] (when-not @!busy (reset! !done-click (str (random-uuid))))) nil))
              ;; on-done! is set in queue contexts (/learn, subset-review) —
              ;; advances !queue-idx so the next item loads after the server
              ;; completes. Registered inside `(when t)` per CLAUDE.md "Local
              ;; mutations that unmount the containing component MUST move
              ;; into e/on-unmount". busy is set only when on-done! is present
              ;; so doc-context never holds the branch.
              ;;
              ;; Electric stabilizes the `let [!busy ...]` binding across
              ;; topic-id changes, so the same atom persists into the next
              ;; queue item. Reset busy in the SAME on-unmount callback as
              ;; on-done! — both writes settle before the next Electric tick,
              ;; so the new topic renders with busy=false (button enabled).
              ;; Splitting into two e/on-unmount calls would risk a frame
              ;; where the new topic reads busy=true.
              (let [[t _] (e/Token done-click)]
                (when t
                  (when on-done!
                    (reset! !busy true)
                    (e/on-unmount (fn [] (reset! !busy false) (on-done!))))
                  (case (e/server (e/Offload #(do (db/done-topic! topic-id) :ok)))
                    (case (e/server (swap! (us/get-atom user-id :refresh) inc))
                      (t))))))

          ;; Done status: show Restore button
          (let [!restore-click (atom nil)
                restore-click (e/watch !restore-click)]
            (dom/button
              (dom/props {:class "btn btn-sm btn-secondary"
                          :style {:color "var(--color-primary)" :border "1px solid var(--color-primary)"
                                  :font-weight "500"}
                          :aria-label "Restore"
                          :data-tooltip "Restore to active review queue"})
              (icons/Icon :rotate-ccw :size 16)
              (dom/span (dom/props {:class "icon-label"}) (dom/text "Restore"))
              (reset! keyboard/!done-btn-ref dom/node)
              (e/on-unmount (fn [] (reset! keyboard/!done-btn-ref nil)))
              (dom/On "click" (fn [_] (reset! !restore-click (str (random-uuid)))) nil))
            (let [[t _] (e/Token restore-click)]
              (when t
                (case (e/server (e/Offload #(do (db/restore-topic! topic-id) :ok)))
                  (case (e/server (swap! (us/get-atom user-id :refresh) inc))
                    (if (and navigate! origin)
                      (case (navigate! origin) (t))
                      (t))))))))))

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
                        :aria-label "Delete"
                        :data-tooltip "Delete this extract and its cards"})
            (icons/Icon :trash-2 :size 16)
            (dom/span (dom/props {:class "icon-label"}) (dom/text "Delete"))
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
                  (dom/p (dom/text "Delete this extract and all its cards? You can undo for 12 hours.")))
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
                          [t _] (e/Token event)]
                      (when t
                        (let [result (e/server (e/Offload #(delete-topic-with-parent! user-id topic-id)))]
                          (case result
                            (case (e/client (card-components/try-delete-anki-notes! (:note-ids result)))
                              (if navigate!
                                (case (cond
                                        (:parent-id result) (navigate! :viewer (nav/nav-topic (:parent-id result) origin))
                                        origin (navigate! origin)
                                        :else (navigate! :library))
                                  (case (reset! !delete-state nil) (t)))
                                (case (reset! !delete-state nil) (t))))))))))))))))))
