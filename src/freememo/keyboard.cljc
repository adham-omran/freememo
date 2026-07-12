(ns freememo.keyboard
  "Global keyboard shortcuts, driven by the command registry: every
   freememo.commands entry with a :bind is registered on one goog
   KeyboardShortcutHandler and dispatched through command-bus/dispatch!.
   No per-action wiring lives here — adding a shortcut = adding :bind to a
   registry entry. Shortcut identifiers are (name command-id), so the
   handler's event round-trips back to the registry id."
  (:require [freememo.commands :as commands]
            [freememo.command-bus :as bus]
            #?(:cljs [goog.events :as gevents]))
  #?(:cljs (:import [goog.ui KeyboardShortcutHandler])))

#?(:cljs
   (defn- in-quill-editor?
     "True when focus is inside a Quill rich-text editor, where Cmd-Shift-Z
      must stay Quill's redo rather than trigger global undo."
     []
     (when-let [el js/document.activeElement]
       (boolean (.closest el ".ql-editor")))))

#?(:cljs
   (defn- on-shortcut [e]
     (let [command-id (keyword (.-identifier e))]
       ;; Yield Cmd-Shift-Z to Quill's redo while editing rich text.
       (when-not (and (= command-id :undo-newest) (in-quill-editor?))
         (bus/dispatch! command-id)))))

#?(:cljs
   (defonce handler
     (let [h (KeyboardShortcutHandler. js/document)]
       ;; Allow shortcuts to fire even in contentEditable (Quill editor).
       ;; Without this, Closure blocks all shortcuts when focus is in
       ;; contentEditable — modifierShortcutsAreGlobal_ only applies to
       ;; form elements (input/textarea/select), not contentEditable.
       (.setAllShortcutsAreGlobal h true)
       (doseq [[id bind] (commands/bindings)]
         (.registerShortcut h (name id) (commands/effective-bind id bind)))
       (gevents/listen h "shortcut" on-shortcut)
       h)))
