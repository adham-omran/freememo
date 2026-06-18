(ns freememo.keyboard
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

;; Hidden trigger for global undo (Cmd-Shift-Z). Clicking it undoes the
;; newest live action; see freememo.undo-history-modal/UndoNewestTrigger.
(defonce !undo-newest-btn-ref (atom nil))

;; Overflow menu proxy refs (toolbar buttons hidden on mobile, clicked via ⋮ menu)
(defonce !gen-prompt-btn-ref (atom nil))
(defonce !add-new-btn-ref (atom nil))
(defonce !export-btn-ref (atom nil))
(defonce !delete-btn-ref (atom nil))
(defonce !pull-anki-btn-ref (atom nil))

#?(:cljs
   (def ^:private shortcut->ref
     {"extract" !extract-btn-ref
      "generate" !generate-btn-ref
      "scan" !scan-btn-ref
      "anki-sync" !anki-sync-btn-ref
      "done" !done-btn-ref
      "undo-newest" !undo-newest-btn-ref}))

#?(:cljs
   (defn- in-quill-editor?
     "True when focus is inside a Quill rich-text editor, where Cmd-Shift-Z
      must stay Quill's redo rather than trigger global undo."
     []
     (when-let [el js/document.activeElement]
       (boolean (.closest el ".ql-editor")))))

#?(:cljs
   (defn- on-shortcut [e]
     ;; Yield Cmd-Shift-Z to Quill's redo while editing rich text.
     (when-not (and (= (.-identifier e) "undo-newest") (in-quill-editor?))
       (when-let [ref-atom (get shortcut->ref (.-identifier e))]
         (when-let [btn @ref-atom]
           (when-not (.-disabled btn)
             (.click btn)))))))

#?(:cljs
   (defonce handler
     (let [h (KeyboardShortcutHandler. js/document)]
       ;; Allow shortcuts to fire even in contentEditable (Quill editor).
       ;; Without this, Closure blocks all shortcuts when focus is in
       ;; contentEditable — modifierShortcutsAreGlobal_ only applies to
       ;; form elements (input/textarea/select), not contentEditable.
       (.setAllShortcutsAreGlobal h true)
       (.registerShortcut h "extract" "meta+shift+e")
       (.registerShortcut h "generate" "meta+shift+g")
       (.registerShortcut h "scan" "meta+shift+s")
       (.registerShortcut h "anki-sync" "meta+shift+x")
       (.registerShortcut h "done" "meta+shift+d")
       (.registerShortcut h "undo-newest" "meta+shift+z")
       (gevents/listen h "shortcut" on-shortcut)
       h)))
