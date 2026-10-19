(ns freememo.target-modals
  "The modals a target command opens from any route, mounted once in Main
   (plans/custom-context-menu.md 4.5.1, 4.6).

   A context-menu row on a Search result or in a card field has no modal of its
   own to open. A handler sets a request atom here instead, and Main's
   `TargetModals` renders the modal. Mounting at Main also settles placement:
   the modal is a sibling of every page and of the card modals' `pointer-events:
   none` overlay, never inside one (CLAUDE.md, \"A Stacked Modal Must Not Nest
   Inside a pointer-events: none Overlay\"), and later in DOM order, so it
   paints over a card modal at the same z-index."
  (:require [hyperfiddle.electric3 :as e]
            [freememo.card-history-modal :refer [CardHistoryModal]]
            [freememo.document-options :refer [DocumentOptionsModal]]
            [freememo.occlusion-modal :refer [OcclusionModal]]))

;; {:mode :create :image-media-id :topic-id :root-topic-id} | nil — the shape
;; freememo.occlusion-modal reads.
(defonce !occlusion-request (atom nil))

;; {:topic-id :pdf?} | nil, and whether the dialog is open. Two atoms because
;; DocumentOptionsModal closes itself by resetting the second.
(defonce !document-options (atom nil))
(defonce !document-options-open? (atom false))

;; [:card id] | [:occlusion-group id] | nil, and whether the dialog is open —
;; CardHistoryModal closes itself by resetting the second.
(defonce !card-history-scope (atom nil))
(defonce !card-history-open? (atom false))

(defn open-card-history!
  "Open the edit history of `scope`. Returns nil."
  [scope]
  (reset! !card-history-scope scope)
  (reset! !card-history-open? true)
  nil)

(defn open-document-options!
  "Open Document options for the document root `topic-id`. Returns nil."
  [topic-id pdf?]
  (reset! !document-options {:topic-id topic-id :pdf? (boolean pdf?)})
  (reset! !document-options-open? true)
  nil)

(e/defn TargetModals
  "Headless until a request arrives. Mounted once in Main."
  [user-id]
  (e/client
    (OcclusionModal !occlusion-request user-id)
    (when-some [scope (e/watch !card-history-scope)]
      (CardHistoryModal scope user-id !card-history-open?))
    (let [{:keys [topic-id pdf?]} (e/watch !document-options)]
      (when (and (e/watch !document-options-open?) topic-id)
        ;; At a root every Document options target is the root itself, the
        ;; targets the library row menu passed. viewer? false drops Dismiss
        ;; and Edit, which need the viewer's bibliography modal.
        (DocumentOptionsModal user-id
          {:root-id topic-id :bib-id topic-id :review-id topic-id :item-id topic-id}
          pdf? !document-options-open? nil false)))))
