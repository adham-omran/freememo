(ns electric-starter-app.keyboard)

;; Button refs — available on both CLJ and CLJS (no reader conditional)
;; to avoid frame mismatch when referenced in e/defn bodies.
(defonce !extract-btn-ref (atom nil))
(defonce !generate-btn-ref (atom nil))
(defonce !scan-btn-ref (atom nil))
(defonce !anki-sync-btn-ref (atom nil))
(defonce !done-btn-ref (atom nil))
(defonce !postpone-btn-ref (atom nil))

#?(:cljs
   (defn- click-ref! [ref-atom e]
     (when-let [btn @ref-atom]
       (when-not (.-disabled btn)
         (.preventDefault e)
         (.click btn)))))

;; Capture-phase listener on js/document — fires before any child
;; element can stopPropagation, guaranteeing global hotkeys work
;; even when Quill editor or modals have focus.
#?(:cljs
   (defn- handle-keydown! [e]
     (when (and (.-shiftKey e) (or (.-metaKey e) (.-ctrlKey e)))
       (case (.toLowerCase (.-key e))
         "e" (click-ref! !extract-btn-ref e)    ; Extract topic
         "g" (click-ref! !generate-btn-ref e)   ; Generate cards
         "s" (click-ref! !scan-btn-ref e)        ; Scan Page (OCR)
         "x" (click-ref! !anki-sync-btn-ref e)  ; Anki Sync
         "d" (click-ref! !done-btn-ref e)        ; Mark Done
         "l" (click-ref! !postpone-btn-ref e)    ; Postpone (L = Later)
         nil))))

#?(:cljs
   (defonce _init-keyboard-listener
     (do (.addEventListener js/document "keydown"
           handle-keydown!
           #js {:capture true})
         true)))
