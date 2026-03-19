(ns electric-starter-app.keyboard)

;; Available on both CLJ and CLJS so e/defn bodies can reference
;; without reader conditionals (avoids frame mismatch).
(defonce !extract-btn-ref (atom nil))

;; Capture-phase listener on js/document — fires before any child
;; element can stopPropagation, guaranteeing global hotkeys work
;; even when Quill editor or modals have focus.
#?(:cljs
   (defn- handle-keydown! [e]
     (cond
       ;; Cmd+Shift+E → Extract topic from selection
       ;; Note: Mac+Cmd reports .key as lowercase even with Shift held
       (and (.-shiftKey e)
            (or (.-metaKey e) (.-ctrlKey e))
            (= (.toLowerCase (.-key e)) "e"))
       (do (.preventDefault e)
           (when-let [btn @!extract-btn-ref]
             (when-not (.-disabled btn)
               (.click btn)))))))

#?(:cljs
   (defonce _init-keyboard-listener
     (do (.addEventListener js/document "keydown"
           handle-keydown!
           #js {:capture true})
         true)))
