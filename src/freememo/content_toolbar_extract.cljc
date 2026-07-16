(ns freememo.content-toolbar-extract
  "Done/Restore, History, and Delete buttons for topics in ContentToolbar.
   Extracted to stay under JVM 64KB method limit."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [taoensso.telemere :as tel]
   [freememo.doc-context :as dctx]
   [freememo.card-components :as card-components]
   [freememo.history-modal :refer [HistoryModal]]
   [freememo.icons :as icons]
   [freememo.modal-shell :as modal]
   [freememo.commands :as commands]
   [freememo.command-bus :as bus]
   [freememo.logging :as log]
   [freememo.navigation :as nav]
   [freememo.tooltip :as tooltip]
   #?(:clj [freememo.db :as db])
   #?(:clj [freememo.staged-delete :as staged-delete])
   #?(:clj [freememo.user-state :as us])))

(defn delete-topic-with-parent!
  "Server-only: stage the topic (subtree + cards) for deletion, returning
   {:success true :parent-id _ :note-ids _ :entry-id _} for the client's
   navigate-up + Anki cleanup, or {:success false :error _} when the topic
   isn't owned by user-id, is already gone, or the stage failed. Reversible
   for 12h via the undo log.

   Stages via freememo.staged-delete/stage-deletion! with refresh? false:
   TopicPage watches :refresh, and bumping it during the same WS tick as this
   fn's return propagates an overview re-query that unmounts the toolbar
   subtree before the client handler can call navigate!, stranding the user on
   the now-hidden topic's URL. :tree-mutations (bumped inside stage-deletion!)
   is watched only by HierarchySidePanel and DocumentTreeView, so it is safe."
  [user-id topic-id]
  #?(:clj (try
            (if-let [r (staged-delete/stage-deletion! user-id topic-id false)]
              (assoc r :success true)
              {:success false :error "Could not delete — topic not found or already removed"})
            (catch Exception e
              (tel/error! {:id ::delete-topic-with-parent!} e)
              {:success false :error "Failed to delete"}))
     :cljs nil))

(defn- done-topic!*
  "Mark topic-id done and bump :done. Returns {:success true} or
   {:success false :error _} — a throw from the DB write no longer strands
   the caller's token."
  [user-id topic-id]
  #?(:clj (try
            (db/done-topic! topic-id)
            (commands/bump! user-id :done)
            {:success true}
            (catch Exception e
              (tel/error! {:id ::done-topic!} e)
              {:success false :error "Failed to mark as done"}))
     :cljs nil))

(defn- restore-topic!*
  "Restore topic-id to active and bump :restore. Same failure contract as
   done-topic!*."
  [user-id topic-id]
  #?(:clj (try
            (db/restore-topic! topic-id)
            (commands/bump! user-id :restore)
            {:success true}
            (catch Exception e
              (tel/error! {:id ::restore-topic!} e)
              {:success false :error "Failed to restore"}))
     :cljs nil))

(e/defn ExtractActions []
  (e/client
    (let [user-id dctx/user-id topic-id dctx/topic-id root-topic-id dctx/root-topic-id
          extract-status dctx/extract-status navigate! dctx/navigate! origin dctx/origin on-done! dctx/on-done!
          ;; Local modal state — CLJS-side atom flipped by the History button
          ;; and the modal's own dismiss handlers (backdrop click / Escape / X).
          !history-open? (atom false)
          ;; Reactive :refresh value powers the modal's re-query on session
          ;; mutations (advance/touch/postpone/done/restore/priority-change all
          ;; bump :refresh, so the table updates immediately after each event).
          history-refresh (e/server (e/watch (us/get-atom user-id :refresh)))
          ;; Review-unit topic: the entity History AND Priority act on. In PDF
          ;; context (extract-status nil per content_toolbar.cljc's pass-through),
          ;; :root-topic-id is the PDF root (topic_page.cljc passes
          ;; `(or pdf-root-id root-topic-id)`), so both scope to the document,
          ;; not the page — matching get-learning-queue, which excludes
          ;; kind 'page'. For extract/wiki/epub (extract-status non-nil), each
          ;; topic is its own review unit.
          review-topic-id (if extract-status topic-id (or root-topic-id topic-id))]

      ;; History button — visible for all topic kinds (per spec §8.1). Sits
      ;; immediately before Done/Restore so when extract-status is set the
      ;; order reads History → Done; on PDF roots (extract-status nil) it
      ;; stands alone.
      (dom/button
        (dom/props {:class "btn btn-sm btn-secondary"
                    :style {:font-weight "500"}
                    :aria-label "History"})
        (tooltip/Tooltip! "View repetition history for this topic")
        (icons/Icon :history :size 16)
        (dom/span (dom/props {:class "icon-label"}) (dom/text "History"))
        (dom/On "click" (fn [_] (reset! !history-open? true)) nil))

      ;; Modal mounted unconditionally — its body is gated on `@!history-open?`,
      ;; so an unopened modal is a cheap no-op in the reactive graph.
      (HistoryModal review-topic-id !history-open? history-refresh)

      ;; Priority control moved into the Document-Options modal (C5); no longer
      ;; an inline stepper here.

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
                (dom/props {:class "btn btn-sm btn-secondary toolbar-done-btn"
                            :style {:color "var(--color-success-dark)" :border "1px solid var(--color-success-dark)"
                                    :font-weight "500"}
                            :disabled busy
                            :aria-label "Done"})
                (tooltip/Tooltip! "Mark as fully processed (extracted/carded everything useful)")
                (icons/Icon :check :size 16)
                (dom/span (dom/props {:class "icon-label"}) (dom/text "Done"))
                (let [node dom/node]
                  (bus/publish-invoker! :done (fn [] (.click node)))
                  (e/on-unmount (fn [] (bus/retract-invoker! :done))))
                (dom/On "click" (fn [_] (when-not @!busy (reset! !done-click (str (random-uuid))))) nil))
              ;; on-done! (queue contexts /learn, subset-review) advances the
              ;; queue and unmounts this component, so it is registered as
              ;; e/on-unmount inside the SUCCESS frame: it fires only after
              ;; done-topic! commits and (t) unmounts the frame.
              ;; Do NOT reset !busy in the token body — that re-renders this
              ;; branch, recreates the !done-click atom, and un-fires the token
              ;; before done-topic! dispatches (verified: Done silently did
              ;; nothing). !busy stays false; the button is brief enough that a
              ;; disable flag is unnecessary.
              (let [[t _] (e/Token done-click)]
                (when t
                  (let [r (e/server (e/Offload #(done-topic!* user-id topic-id)))]
                    (case r
                      (if (:success r)
                        (do (when on-done! (e/on-unmount (fn [] (on-done!))))
                            (t))
                        (t (:error r))))))))

          ;; Done status: show Restore button
          (let [!restore-click (atom nil)
                restore-click (e/watch !restore-click)]
            (dom/button
              (dom/props {:class "btn btn-sm btn-secondary toolbar-restore-btn"
                          :style {:color "var(--color-primary-text)" :border "1px solid var(--color-primary)"
                                  :font-weight "500"}
                          :aria-label "Restore"})
              (tooltip/Tooltip! "Restore to active review queue")
              (icons/Icon :rotate-ccw :size 16)
              (dom/span (dom/props {:class "icon-label"}) (dom/text "Restore"))
              (let [node dom/node]
                (bus/publish-invoker! :restore (fn [] (.click node)))
                (e/on-unmount (fn [] (bus/retract-invoker! :restore))))
              (dom/On "click" (fn [_] (reset! !restore-click (str (random-uuid)))) nil))
            (let [[t _] (e/Token restore-click)]
              (when t
                (let [r (e/server (e/Offload #(restore-topic!* user-id topic-id)))]
                  (case r
                    (if (:success r)
                      (if (and navigate! origin)
                        (case (navigate! origin) (t))
                        (t))
                      (t (:error r)))))))))))

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
                        :aria-label "Delete"})
            (tooltip/Tooltip! "Delete this extract and its cards")
            (icons/Icon :trash-2 :size 16)
            (dom/span (dom/props {:class "icon-label"}) (dom/text "Delete"))
            (let [node dom/node]
              (bus/publish-invoker! :delete-document (fn [] (.click node)))
              (e/on-unmount (fn [] (bus/retract-invoker! :delete-document))))
            (dom/On "click" (fn [_] (reset! !delete-state :confirming)) nil))
          (when (= delete-state :confirming)
            (dom/div
              (dom/props {:class "modal-backdrop"})
              (dom/On "click" (fn [_] (reset! !delete-state nil)) nil)
              (modal/ModalEscape (fn [] (reset! !delete-state nil)) "Confirm delete")
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
                          (if (:success result)
                            (case (e/client (card-components/try-delete-anki-notes! (:note-ids result)))
                              (if navigate!
                                (case (cond
                                        (:parent-id result) (navigate! :viewer (nav/nav-topic (:parent-id result) origin))
                                        origin (navigate! origin)
                                        :else (navigate! :library))
                                  (case (reset! !delete-state nil) (t)))
                                (case (reset! !delete-state nil) (t))))
                            (t (:error result))))))))))))))))
