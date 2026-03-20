(ns electric-starter-app.keyboard
  #?(:cljs (:import [goog.ui KeyboardShortcutHandler]))
  #?(:cljs (:require [goog.events :as gevents])))

;; Button refs — available on both CLJ and CLJS (no reader conditional)
;; to avoid frame mismatch when referenced in e/defn bodies.
(defonce !extract-btn-ref (atom nil))
(defonce !generate-btn-ref (atom nil))
(defonce !scan-btn-ref (atom nil))
(defonce !anki-sync-btn-ref (atom nil))
(defonce !done-btn-ref (atom nil))
(defonce !postpone-btn-ref (atom nil))

#?(:cljs
   (def ^:private shortcut->ref
     {"extract"   !extract-btn-ref
      "generate"  !generate-btn-ref
      "scan"      !scan-btn-ref
      "anki-sync" !anki-sync-btn-ref
      "done"      !done-btn-ref}))

#?(:cljs
   (defn- on-shortcut [e]
     (when-let [ref-atom (get shortcut->ref (.-identifier e))]
       (when-let [btn @ref-atom]
         (when-not (.-disabled btn)
           (.click btn))))))

#?(:cljs
   (defonce handler
     (let [h (KeyboardShortcutHandler. js/document)]
       ;; Allow shortcuts to fire even in contentEditable (Quill editor).
       ;; Without this, Closure blocks all shortcuts when focus is in
       ;; contentEditable — modifierShortcutsAreGlobal_ only applies to
       ;; form elements (input/textarea/select), not contentEditable.
       (.setAllShortcutsAreGlobal h true)
       (.registerShortcut h "extract"   "meta+shift+e")
       (.registerShortcut h "generate"  "meta+shift+g")
       (.registerShortcut h "scan"      "meta+shift+s")
       (.registerShortcut h "anki-sync" "meta+shift+x")
       (.registerShortcut h "done"      "meta+shift+d")
       (gevents/listen h "shortcut" on-shortcut)
       h)))
