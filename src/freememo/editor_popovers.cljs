(ns freememo.editor-popovers
  "The popovers that an editor's Escape closes before it leaves the editor.

   Every popover here lives on `document.body` and listens for Escape on
   `document` in the bubble phase. That listener runs AFTER an enclosing
   modal's `ModalEscape`, so on its own it can never take Escape first. The
   editor's Escape handler (`a11y/install-editor-escape!`) asks this registry
   instead, and stops the key when a popover took it.

   The popovers keep their own document listeners. Focus is not always in the
   editor — a click inside the format card moves it there — and then only the
   popover's own listener sees Escape.")

;; entry id → {:open? (fn [] bool) :close! (fn [] any)}
(defonce ^:private !closers (atom {}))

(defn register!
  "Add a popover to the registry. Returns a 0-arg fn that removes it.

   Pre : `open?` is a 0-arg predicate that is cheap to call; `close!` is a
         0-arg fn that makes `open?` false.
   Post: `close-open!` considers this popover until the returned fn runs."
  [open? close!]
  (let [id (gensym "popover")]
    (swap! !closers assoc id {:open? open? :close! close!})
    (fn unregister [] (swap! !closers dissoc id))))

(defn close-open!
  "Close every registered popover that is open. Returns true when it closed
   at least one.

   Post: no registered popover reports `open?`."
  []
  (let [open (filterv #((:open? %)) (vals @!closers))]
    (doseq [{:keys [close!]} open] (close!))
    (boolean (seq open))))
