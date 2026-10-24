(ns freememo.document-generate-form
  "The small form the right-click row \"Generate cards…\" opens: count, kind
   and context for one run over the whole document, with no selection
   (plans/toolbar-card-actions-fold.md §6).

   The form seeds from freememo.gen-settings each time it opens and never
   writes back (§6.3): a one-off run does not change the defaults the popover
   and ⌘G use."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.electric-forms5 :as forms]
   [freememo.doc-context :as dctx]
   [freememo.gen-settings :as gs]
   [freememo.generate-invoker :as gen]
   [freememo.command-bus :as bus]
   [freememo.modal-shell :as modal]))

;; Kind is an A1-fallback select: Forms5 has no tracked select, and
;; RadioPicker! shows each raw value ("basic") as a <dt> above its radio.
;; Pre : !kind holds a kind string. Post: returns (e/amb), no Forms5 edit.
(e/defn KindSelect [!kind]
  (let [kind (e/watch !kind)]
    (dom/select
      (dom/props {:class "input input-full" :aria-label "Card kind"})
      (dom/On "change" (fn [e] (reset! !kind (-> e .-target .-value))) nil)
      (e/for [k (e/diff-by identity gs/kinds)]
        (dom/option
          (dom/props {:value k :selected (= k kind)})
          (dom/text (gs/kind-labels k))))))
  (e/amb))

(e/defn CancelButton [!open]
  (dom/button
    (dom/props {:class "btn btn-secondary" :type "button"})
    (dom/text "Cancel")
    (dom/On "click" (fn [_] (reset! !open false)) nil))
  (e/amb))

(e/defn FormBody
  "Pre : `initial` is a full settings map, read once when the form opened.
   Post: Generate enqueues one whole-document run with the form's values and
         closes the form. The saved settings are not touched."
  [!open initial]
  (let [user-id dctx/user-id enc-key dctx/enc-key topic-id dctx/topic-id
        root-topic-id dctx/root-topic-id page-number dctx/page-number
        content-text dctx/content-text context-mode dctx/context-mode
        is-code? dctx/is-code?
        !kind (atom (e/snapshot (:kind initial)))
        commits (forms/Form! initial
                  (e/fn Fields [{:keys [context?] card-count :count context-pages :context-pages}]
                    (e/amb
                      (dom/label (dom/props {:class "label"}) (dom/text "Cards"))
                      (forms/Input! :count card-count :type "number"
                        :min (str gs/count-min) :max (str gs/count-max)
                        :class "input input-full" :aria-label "Cards to generate")
                      (dom/label (dom/props {:class "label"}) (dom/text "Kind"))
                      (KindSelect !kind)
                      (dom/label
                        (dom/props {:class "document-generate-form__context"})
                        (e/amb
                          (forms/Checkbox! :context? (boolean context?))
                          (do (dom/text "Include context") (e/amb))))
                      (forms/Input! :context-pages context-pages :type "number"
                        :min (str gs/context-pages-min) :max (str gs/context-pages-max)
                        :class "input input-full" :aria-label "Context pages")
                      (dom/div
                        (dom/props {:class "document-generate-form__buttons"})
                        (e/amb
                          (forms/SubmitButton! :label "Generate" :class "btn btn-primary")
                          (CancelButton !open)))))
                  :Parse (e/fn [fields _tempid]
                           [`GenerateDocumentCards
                            (gs/from-form (assoc fields :kind (e/watch !kind)) initial)])
                  :type :command
                  :show-buttons false)]
    (e/for [[token cmd] (e/diff-by first (e/as-vec commits))]
      (let [{card-type :kind card-count :count use-context :context? context-window :context-pages}
            (nth cmd 1)]
        (case (e/server
                (gen/enqueue-generation!*
                  {:selection-html nil
                   :content-text content-text :context-mode context-mode
                   :use-context use-context :topic-id topic-id
                   :root-topic-id root-topic-id :page-number page-number
                   :context-window context-window :is-code? is-code?
                   :user-id user-id :enc-key enc-key
                   :card-type card-type :card-count card-count}))
          (do (reset! !open false) (token)))))))

(e/defn DocumentGenerateForm
  "The :open-document-generate-form invoker's owner. It is not a registry
   command: the right-click row's handler runs it through `bus/run-invoker!`.
   Pre : mounted once per viewed topic, by CardActionsHost.
   Post: while LLM features are on, each invocation opens the form, seeded
         from the current gen-settings."
  []
  (e/client
    (let [!open (atom false)
          open? (e/watch !open)]
      (when dctx/llm-enabled?
        (bus/publish-invoker! :open-document-generate-form
          (fn [] (reset! !open true)))
        (e/on-unmount (fn [] (bus/retract-invoker! :open-document-generate-form))))
      (when open?
        (dom/div
          (dom/props {:class "modal-backdrop" :tabindex "-1"})
          (modal/ModalEscape (fn [] (reset! !open false)) "Generate cards")
          (dom/On "keydown" modal/mod-enter-request-submit! nil)
          (dom/On "click"
            (fn [e]
              (when (= (.-target e) (.-currentTarget e))
                (reset! !open false)))
            nil)
          (dom/div
            (dom/props {:class "modal-content document-generate-form"})
            (dom/h3 (dom/text "Generate cards from this document"))
            (FormBody !open (e/snapshot (gs/current)))))))))
