(ns freememo.document-body
  "TopicPage's render body (layout-persist + the document layout column tree),
   in its own namespace so its cross-ns calls to document-view components are
   NOT inlined (Electric inlines same-namespace e/defn calls, which is what
   pushed TopicPage past the JVM 64KB-per-method limit). TopicPage does data
   resolution and hands the resolved values in as a prop map."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.document-view :refer [DocumentColumns DocumentToolbars]]
   [freememo.bottom-panel :refer [BottomPanel]]
   [freememo.util :as util]
   #?(:clj [freememo.settings :as settings])))

(e/defn DocumentBody
  "Renders the document body for a resolved topic. All values arrive in `m`
   (>20 values, so a map is mandatory — Clojure caps fixed arity at 20). `m` is
   forwarded verbatim to DocumentToolbars/DocumentColumns: building per-child map
   literals here is what overflowed the 64KB method, so we pass the one map
   through and let each child destructure the keys it needs."
  [{:keys [user-id page-topic-id pdf-root-id is-pdf? reading-mode?
           card-font-size card-refresh !top-pct t-layout layout-save] :as m}]
  (e/client
    ;; Persist layout on toggle
    (when t-layout
      (if (and is-pdf? pdf-root-id)
        (let [r (e/server (e/Offload #(settings/save-pdf-layout user-id pdf-root-id layout-save)))]
          (when (some? r)
            (if (:success r) (case r (t-layout)) (t-layout (:error r)))))
        (t-layout)))

    (dom/div
      (dom/props {:style {:flex "1" :display "flex" :flex-direction "column"
                          :min-height "0" :overflow "hidden"}})

      ;; Above-the-content chrome: command bar, PDF toolbar, biblio auto-open
      ;; + modal. Forward the whole prop map (no new literal here).
      (DocumentToolbars m)

      ;; TOP REGION: hierarchy | content (PdfPane / EditorPane) | pins.
      (DocumentColumns m)

      ;; Card table + its resize handle — hidden in reading-mode? (B1).
      ;; Add-Card still works; success surfaces as a toast (card_modals).
      (when-not reading-mode?
        ;; Vertical drag handle: resizes the top region ↕ the bottom bar.
        (dom/div
          (dom/props {:class "split-divider-v" :title "Drag to resize panels"})
          (dom/On "pointerdown" (fn [e] (util/start-drag! e :y !top-pct)) nil)))

      ;; BOTTOM BAR (full width): the card table.
      (when-not reading-mode?
        (BottomPanel
          {:user-id user-id
           :topic-id page-topic-id
           :card-font-size card-font-size}
           card-refresh)))
    ))
