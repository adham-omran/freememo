(ns freememo.keyboard
  "Global keyboard shortcuts, driven by the command registry: every
   freememo.commands entry with a :bind is registered on one goog
   KeyboardShortcutHandler and dispatched through command-bus/dispatch!.
   No per-action wiring lives here — adding a shortcut = adding :bind to a
   registry entry. Shortcut identifiers are (name command-id), so the
   handler's event round-trips back to the registry id.

   Suppression is CONDITIONAL, which is what lets chords be one modifier
   short (⌘D, not ⌘⇧D) without eating the browser action everywhere. goog
   defaults alwaysPreventDefault_ to true and calls preventDefault BEFORE it
   dispatches, so a listener cannot opt out; this handler turns that off and
   suppresses only when dispatch! reports the command actually ran. A chord
   pressed where its command is unavailable — :when excludes the tab, or the
   preparer resolves no payload — falls through to the browser untouched.
   See plans/short-keyboard-chords.md."
  (:require [freememo.commands :as commands]
            [freememo.command-bus :as bus]
            #?(:cljs [goog.events :as gevents]))
  #?(:cljs (:import [goog.ui KeyboardShortcutHandler])))

#?(:cljs
   (def ^:private text-input-types
     "<input> types the user edits as text, where the browser keeps its own undo
      stack. Matched against the IDL .type property, which reports \"text\" for a
      missing or invalid type attribute."
     #{"text" "search" "email" "url" "password" "number"}))

#?(:cljs
   (defn- text-editing-focus?
     "True when the focused element owns text editing, so a chord the user means
      as a text command (⌘Z) must reach it instead of the app.

      Reads the isContentEditable IDL property, NOT a [contenteditable] selector
      and not a .closest walk: the property resolves inheritance and reports
      false for Quill's contenteditable=\"false\" embed blots, which the selector
      matches. It also subsumes .ql-editor — Quill focuses its contenteditable
      root — so no Quill-specific branch is needed.

      A readonly field is excluded: it holds no undo stack, so yielding ⌘Z
      there would spend the keystroke on nothing.

      Post: true iff document.activeElement is an editable text surface."
     []
     (let [el js/document.activeElement]
       (boolean
         (and el
              (or (.-isContentEditable el)
                  (and (or (= "TEXTAREA" (.-tagName el))
                           (and (= "INPUT" (.-tagName el))
                                (contains? text-input-types (.-type el))))
                       (not (.-readOnly el)))))))))

#?(:cljs
   (defn- on-shortcut
     "goog \"shortcut\" listener.

      Returning literally `false` is what suppresses the browser default —
      goog's fireListeners accumulates `listenerFn.call(…) !== false`, so `nil`
      is NOT a suppress signal and leaves the keydown to the browser.

      Suppression follows OWNERSHIP, not outcome. `applies-here?` asks whether
      the chord is the app's on this screen; `dispatch!` asks whether anything
      can serve it right now. Keying suppression to the second one would hand
      the browser ⌘S the moment a toolbar button unmounts — which is every
      non-PDF topic, since :scan's only publisher sits behind (when is-pdf? …).
      A key the app owns stays swallowed even when it does nothing.

      :undo-newest yields inside a text surface, where ⌘Z is the browser's and
      the editor's undo. ⌘⇧Z is unbound, so Quill's redo needs no carve-out.

      Pre : the handler was built with setAlwaysPreventDefault false.
      Post: false iff the command applies on the active tab; nil otherwise,
            and the browser keeps its default action.
      :keyboard is what makes chord adoption measurable — dispatch! defaults
      to :button, so a chord that omitted it would be indistinguishable from
      a click on the same command. The dispatch runs on the unavailable path
      too, because dispatch! records an :unavailable trail entry there and a
      chord a user keeps retrying is the signal that a rule is wrong."
     [e]
     (let [command-id (keyword (.-identifier e))]
       (when-not (and (= command-id :undo-newest) (text-editing-focus?))
         (bus/dispatch! command-id nil :keyboard)
         (when (bus/applies-here? command-id)
           false)))))

#?(:cljs
   (defonce handler
     (let [h (KeyboardShortcutHandler. js/document)]
       ;; Allow shortcuts to fire even in contentEditable (Quill editor).
       ;; Without this, Closure blocks all shortcuts when focus is in
       ;; contentEditable — modifierShortcutsAreGlobal_ only applies to
       ;; form elements (input/textarea/select), not contentEditable.
       (.setAllShortcutsAreGlobal h true)
       ;; Hand suppression to on-shortcut. With the goog default (true), every
       ;; registered chord is preventDefault'd before dispatch runs, so ⌘D
       ;; would be dead on every tab where :done cannot act.
       (.setAlwaysPreventDefault h false)
       (doseq [[id bind] (commands/bindings)]
         (.registerShortcut h (name id) (commands/effective-bind id bind)))
       (gevents/listen h "shortcut" on-shortcut)
       h)))
