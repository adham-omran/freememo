(ns freememo.library-row-actions
  "Library document-row action menu — collapses the per-row Rename / Move-to-top
   / Review / Delete / Document-options buttons behind one `⋯` trigger.

   Own namespace (like toolbar_generate_dropdown) to stay under the JVM 64KB
   bytecode limit that already forced DocumentRow/DeleteConfirmModal out of
   knowledge_tree.

   Positioning: the tree body is `overflow-y:auto`, so an absolute menu would
   clip at the scroll box; the menu is `position:fixed` at the trigger's rect
   instead (mirrors editor_image_menu). Escape / outside-mousedown / body-scroll
   all close it."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.navigation :as nav]
   [freememo.icons :as icons]
   [freememo.document-options :refer [DocumentOptionsModal DocumentOptionsTrigger]]
   [freememo.tooltip :as tooltip]
   [freememo.neoreader-kinds :as nrkinds]
   #?(:cljs [freememo.neoreader-picker :as neoreader])))

;; NeoReader sync — syncable document roots only. Kept a plain defn, not inline in the
;; reactive body: it loads the Google Picker and stores a shared promise in a
;; defonce atom, and Electric re-evaluates a dom body unpredictably (CLAUDE.md
;; 'JS Library Init Side Effects in e/defn'). The reader conditional lives here,
;; OUTSIDE any e/defn, for the same reason — a #?(:cljs …) inside a reactive
;; body compiles a different signal count per peer.
;;
;; Every outcome the user should see arrives as a server-pushed toast, because
;; the server holds the counts. The two phases the server never sees — a
;; cancelled Picker and a Picker that will not load — reach the console only.
;; That is a real gap: a blocked Google request looks like nothing happening.
(defn start-neoreader-sync! [topic-id]
  #?(:cljs (neoreader/sync-document! topic-id
             (str (.-pathname js/location) (.-search js/location))
             (fn [status]
               (case (:phase status)
                 :error (js/console.error "NeoReader sync:" (:message status))
                 :cancelled (js/console.info "NeoReader sync cancelled")
                 nil)))
     :clj nil))

;; Escape + click-outside + body-scroll → close. Kept a plain (defn) so the
;; addEventListener calls run once per (when open …) mount/unmount cycle, never
;; in the reactive e/defn body (CLAUDE.md 'JS Library Init Side Effects in
;; e/defn'). Returns a 0-arg cleanup fn removing all three listeners.
(defn install-row-menu-listeners! [!open !scroll-node]
  #?(:cljs
     (let [scroll-node @!scroll-node
           on-key (fn [e] (when (= (.-key e) "Escape") (reset! !open false)))
           on-mouse (fn [e]
                      (let [t (.-target e)]
                        (when-not (or (.closest t ".row-actions-menu")
                                    (.closest t ".row-actions-trigger"))
                          (reset! !open false))))
           on-scroll (fn [_] (reset! !open false))]
       (.addEventListener js/document "keydown" on-key)
       (.addEventListener js/document "mousedown" on-mouse)
       (when scroll-node (.addEventListener scroll-node "scroll" on-scroll))
       (fn []
         (.removeEventListener js/document "keydown" on-key)
         (.removeEventListener js/document "mousedown" on-mouse)
         (when scroll-node (.removeEventListener scroll-node "scroll" on-scroll))))
     :clj (fn [] nil)))

;; Pre:  id = the row's topic id; !editing-id / !drop-cmd / !show-confirm are the
;;       DocumentTreeView mutation atoms; !scroll-node holds the scroll container.
;; Post: choosing an item runs exactly its pre-existing effect, then closes the
;;       menu; the row's navigate-on-click never fires from a menu interaction
;;       (every item + the trigger stop propagation).
;; Invariant: item visibility matches the buttons that rendered on this row
;;       before — Rename always, Move-to-top when-not root, Review when children,
;;       Delete / Document-options when root.
(e/defn RowActionsMenu [user-id id title is-root has-children kind dismissed?
                        navigate! !editing-id !drop-cmd !show-confirm !scroll-node !dismiss-cmd]
  (e/client
    (let [!open (atom false)
          open (e/watch !open)
          ;; Document options, mounted outside the menu: the menu closes on
          ;; Escape and on an outside mousedown, which would tear the modal
          ;; down under a save in flight.
          !options-open (atom false)
          options-open? (e/watch !options-open)
          ;; fixed coords, recomputed from the trigger rect on each open
          !pos (atom {:top 0 :right 0})
          pos (e/watch !pos)]
      (dom/div
        (dom/props {:class "row-actions" :style {:display "inline-flex"}})
        (dom/button
          (dom/props {:class "btn btn-sm btn-secondary row-actions-trigger tooltip-right"
                      :style {:padding "2px 8px" :font-size "14px" :line-height "1"}
                      :aria-haspopup "menu" :aria-expanded (if open "true" "false")
                      :aria-label "Row actions"})
          (tooltip/Tooltip! "Actions")
          (dom/text "⋯")
          (dom/On "click"
            (fn [e]
              (.stopPropagation e)
              (let [r (.getBoundingClientRect (.-currentTarget e))]
                (reset! !pos {:top (+ (.-bottom r) 4)
                              :right (- (.-innerWidth js/window) (.-right r))}))
              (swap! !open not))
            nil))

        (when open
          (let [cleanup (install-row-menu-listeners! !open !scroll-node)]
            (e/on-unmount cleanup)
            ;; Portal into <body>. The row carries a transform (virtual-scroll
            ;; positioning), which makes position:fixed resolve against the ROW
            ;; box, not the viewport — so the trigger-rect coords land far off.
            ;; Mounting in body escapes the transformed containing block.
            (binding [dom/node js/document.body]
            (dom/div
              (dom/props {:class "toolbar-dropdown-menu row-actions-menu"
                          :role "menu"
                          :style {:position "fixed" :left "auto"
                                  :top (str (:top pos) "px")
                                  :right (str (:right pos) "px")}})

              ;; Rename — always. Sets !editing-id; the title cell (Column 1)
              ;; swaps to its inline editor, unchanged by this menu.
              (dom/button
                (dom/props {:class "toolbar-dropdown-item" :role "menuitem"})
                (icons/Icon :pen-line :size 16)
                (dom/span (dom/text "Rename"))
                (dom/On "click"
                  (fn [e] (.stopPropagation e) (reset! !editing-id id) (reset! !open false))
                  nil))

              ;; Move to top level — non-root only (root has no parent to leave).
              (when-not is-root
                (dom/button
                  (dom/props {:class "toolbar-dropdown-item" :role "menuitem"})
                  (icons/Icon :upload :size 16)
                  (dom/span (dom/text "Move to top level"))
                  (dom/On "click"
                    (fn [e] (.stopPropagation e) (reset! !drop-cmd {:src id :dst nil}) (reset! !open false))
                    nil)))

              ;; Review — only when the topic has children to review.
              (when has-children
                (dom/button
                  (dom/props {:class "toolbar-dropdown-item" :role "menuitem"})
                  (icons/Icon :graduation-cap :size 16)
                  (dom/span (dom/text "Review"))
                  (dom/On "click"
                    (fn [e] (.stopPropagation e)
                      (navigate! :viewer (nav/nav-subset-review id title))
                      (reset! !open false))
                    nil)))

              ;; Sync with NeoReader — document roots of a syncable kind. A Boox
              ;; export exists per book, so a chapter or page row has nothing to
              ;; sync. The first click opens the Google Picker to bind a file;
              ;; later clicks reuse it. The kind set is NOT a literal here: the
              ;; server route gates on the same var, and a literal would let a
              ;; button appear over a route that 404s.
              (when (and is-root (nrkinds/syncable-kind? kind))
                (dom/button
                  (dom/props {:class "toolbar-dropdown-item" :role "menuitem"})
                  (icons/Icon :cloud-download :size 16)
                  (dom/span (dom/text "Sync with NeoReader"))
                  (dom/On "click"
                    (fn [e] (.stopPropagation e)
                      (start-neoreader-sync! id)
                      (reset! !open false))
                    nil)))

              ;; Delete — root documents only.
              (when is-root
                (dom/button
                  (dom/props {:class "toolbar-dropdown-item" :role "menuitem"
                              :style {:color "var(--color-danger)"}})
                  (icons/Icon :trash-2 :size 16)
                  (dom/span (dom/text "Delete"))
                  (dom/On "click"
                    (fn [e] (.stopPropagation e) (reset! !show-confirm id) (reset! !open false))
                    nil)))

              ;; Dismiss / Undismiss — root only. Removes this document + its
              ;; whole subtree from the Learning Queue (or restores it). The
              ;; server roundtrip is delegated to DocumentTreeView via
              ;; !dismiss-cmd so it runs in a scope that outlives this menu.
              (when is-root
                (dom/button
                  (dom/props {:class "toolbar-dropdown-item" :role "menuitem"})
                  (icons/Icon (if dismissed? :rotate-ccw :x) :size 16)
                  (dom/span (dom/text (if dismissed? "Undismiss" "Dismiss")))
                  (dom/On "click"
                    (fn [e] (.stopPropagation e)
                      (reset! !dismiss-cmd {:id id :dismissed? dismissed?})
                      (reset! !open false))
                    nil)))

              ;; Document options — root only. The trigger closes this menu and
              ;; opens the modal mounted below.
              (when is-root
                (DocumentOptionsTrigger !options-open "toolbar-dropdown-item"
                  (fn [] (reset! !open false))))))))

        ;; Portal into <body>, for the transform reason above. At root every
        ;; Document options target is this row's id. viewer? false drops
        ;; Dismiss (this menu has its own) and Edit (no bibliography modal
        ;; here), so !show-bib is nil.
        (when options-open?
          (binding [dom/node js/document.body]
            (DocumentOptionsModal user-id {:root-id id :bib-id id :review-id id :item-id id}
              (= kind "pdf") !options-open nil false)))))))
