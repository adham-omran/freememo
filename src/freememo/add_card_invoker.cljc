(ns freememo.add-card-invoker
  "The :add-new command's owner: opens AddCardModal, with the reader's
   selection in the front field when there is one. It renders no control. The
   right-click menu, the compact toolbar pair, the palette and ⌘⇧A all reach it
   through the command bus (plans/toolbar-card-actions-fold.md §2)."
  (:require
   [clojure.string :as str]
   [hyperfiddle.electric3 :as e]
   [freememo.selected-passage :as selected-passage]
   [freememo.card-modals :refer [AddCardModal]]
   [freememo.command-bus :as bus]))

(defn- prefill-from-selection!
  "The modal's prefill map for the reader's current selection.
   Post: {:primary html} when a surface holds non-blank text, and that passage
         is marked gold. Otherwise {:primary \"\"} and nothing is marked. Gold is
         Generate's mark: \"a card was made from this passage\"."
  []
  #?(:clj {:primary ""}
     :cljs (let [passage (selected-passage/current)
                 taken? (not (str/blank? (:text passage)))]
             (when taken?
               (selected-passage/mark! passage :color "var(--color-highlight-gold)"))
             {:primary (if taken? (:html passage) "")})))

(e/defn AddCardInvoker
  "Pre : mounted once per viewed topic, by CardActionsHost.
   Post: while mounted, :add-new is available, and each invocation opens one
         AddCardModal seeded with the current selection."
  [user-id topic-id root-topic-id card-type]
  (e/client
    (let [!show-add (atom false)
          show-add (e/watch !show-add)
          !card-kind (atom card-type)
          ;; Only :primary is ever set here. Create copy… fills the rest.
          !prefill (atom {})]
      (bus/publish-invoker! :add-new
        (fn []
          (reset! !prefill (prefill-from-selection!))
          (reset! !show-add true)))
      (e/on-unmount (fn [] (bus/retract-invoker! :add-new)))
      (when show-add
        (AddCardModal (fn [] (reset! !show-add false))
          !card-kind !prefill topic-id root-topic-id user-id nil)))))
