(ns freememo.right-side-panel
  "Right-side panel: the AI assistant, and — on a poem — الشرح beside it.

   The panel can collapse and resize. It keeps the former right-panel settings
   keys so an existing layout does not reset after Pins moves to the left panel.

   The subtree remounts only when root-topic-id changes. Local chat and panel
   state therefore stays stable during page navigation within one document.

   The tab row mirrors the left panel's, down to the settings shape: the chosen
   tab is persisted per document as `right_panel_tab_<pane-scope-id>`. It is
   rendered only for a poem, because a topic with one tab needs no switch."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.hierarchy-side-panel :refer [SidePanelShell]]
   [freememo.assistant-panel :refer [AssistantPanel]]
   [freememo.poem-panel :as poem :refer [PoemPanel]]
   [freememo.command-bus :as bus]
   [freememo.viewport :as viewport]
   #?(:clj [freememo.settings :as settings])
   #?(:clj [freememo.toasts :as toasts])))

(defn push-toast!*
  "Push one toast for `user-id`. The `:show-poem-panel` command needs this: its
   handler is a plain client fn, and the toast queue is a per-user SERVER atom,
   so a refusal has to cross the wire through a mounted component. This one is
   mounted on every viewer topic, which is exactly the surface that command
   declares."
  [user-id level message]
  #?(:clj (do (toasts/push! user-id {:level level :message message}) nil)
     :cljs nil))

(def ^:private not-a-poem-message "This document is not a poem")

(def ^:private tab-ids
  "The tab ids this panel can select. A client mirror of
   `settings/right-panel-tabs`, which is CLJ-only and cannot be read from the
   client fn that guards a tab switch. The two are checked against each other by
   nothing; keep them in step."
  #{"assistant" "poem"})

(e/defn RightPanelTabs
  "The Assistant/الشرح switch. Rendered through SidePanelShell's Header-extra,
   in the shape the left panel's tab row uses."
  [tab !tab !tab-save]
  (e/client
    (dom/div
      (dom/props {:class "side-panel__tabs" :role "tablist"
                  :aria-label "Right panel"})
      (e/for [entry (e/diff-by :id [{:id "assistant" :label "Assistant"}
                                    {:id "poem" :label "الشرح"}])]
        (let [id (:id entry)
              selected? (= tab id)]
          (dom/button
            (dom/props {:class (str "side-panel__tab"
                                 (when selected? " side-panel__tab--active"))
                        :type "button" :role "tab"
                        :aria-selected (str selected?)})
            (dom/text (:label entry))
            (dom/On "click"
              (fn [_] (reset! !tab id) (reset! !tab-save id)) nil)))))))

(e/defn RightSidePanel [page-topic-id root-topic-id user-id poem?]
  (e/client
    (e/for-by identity [_k [root-topic-id]]
      (let [;; A saved state restores at every Viewer viewport, including a state
            ;; a command set. Only a document with no saved state starts closed on
            ;; a phone, where the open panel covers the document. A deref, not a
            ;; watch: rotating the phone must not re-create !open?.
            default-open?   (not @viewport/!phone?)
            persisted-open? (e/server (settings/get-assistant-open user-id root-topic-id default-open?))
            initial-open?   persisted-open?
            !open? (atom initial-open?)
            open? (e/watch !open?)
            !save (atom nil)
            save-val (e/watch !save)
            [t _] (e/Token save-val)
            persisted-width (e/server (settings/get-assistant-width user-id root-topic-id))
            !width-px (atom persisted-width)
            width-px (e/watch !width-px)
            !width-save (atom nil)
            width-save (e/watch !width-save)
            [tw _] (e/Token width-save)
            persisted-tab (e/server (settings/get-right-panel-tab user-id root-topic-id))
            ;; nil until a tab is chosen in this session. The atom takes NO
            ;; reactive value on purpose: `show-right-pane!` below is an
            ;; `e/snapshot` closure over it, and `(atom persisted-tab)` was
            ;; rebuilt whenever that server read re-emitted, so the snapshot
            ;; reset an atom nothing watched. Measured: a word tap saved "poem"
            ;; through `!tab-save` (no reactive input, so never rebuilt) and
            ;; switched nothing visible.
            !tab (atom nil)
            ;; A non-poem topic has no الشرح tab, so a persisted "poem" must not
            ;; select a body that cannot render. The persisted value is left
            ;; alone — navigating back to the poem restores it.
            tab (let [saved (or (e/watch !tab) persisted-tab)]
                  (if (and (= poem/panel-tab saved) (not poem?)) "assistant" saved))
            !tab-save (atom nil)
            tab-save (e/watch !tab-save)
            [tt _] (e/Token tab-save)
            ;; The refusal path of :show-poem-panel. The handler is a plain
            ;; client fn with no Electric scope of its own, so it writes a
            ;; request here and this frame makes the server call. The id makes
            ;; every request a distinct value: two refusals in a row would
            ;; otherwise leave the watched value unchanged and the second would
            ;; open no token.
            !toast-request (atom nil)
            toast-request (e/watch !toast-request)
            [t-toast _] (e/Token toast-request)
            toggle-assistant-pane!
            (e/snapshot
              (fn []
                (let [next-open? (not @!open?)]
                  (reset! !open? next-open?)
                  (reset! !save next-open?))))
            show-right-pane!
            (e/snapshot
              (fn [tab-id]
                (when (tab-ids tab-id)
                  (reset! !open? true)
                  (reset! !save true)
                  (reset! !tab tab-id)
                  (reset! !tab-save tab-id))))
            request-poem-panel!
            (e/snapshot
              (fn [poem-topic-id]
                (if (some? poem-topic-id)
                  (show-right-pane! poem/panel-tab)
                  (reset! !toast-request {:id (random-uuid)
                                          :message not-a-poem-message}))))]

        (bus/publish-ctx! {:toggle-assistant-pane! toggle-assistant-pane!
                           :show-right-pane! show-right-pane!
                           :request-poem-panel! request-poem-panel!})
        (e/on-unmount #(bus/retract-ctx! [:toggle-assistant-pane! :show-right-pane!
                                          :request-poem-panel!]))

        (when t
          (let [r (e/server (e/Offload #(settings/save-assistant-open user-id root-topic-id save-val)))]
            (case r
              (if (:success r) (t) (t (:error r))))))

        (when tw
          (let [r (e/server (e/Offload #(settings/save-assistant-width user-id root-topic-id width-save)))]
            (case r
              (if (:success r) (tw) (tw (:error r))))))

        (when tt
          (let [r (e/server (e/Offload #(settings/save-right-panel-tab user-id root-topic-id tab-save)))]
            (case r
              (if (:success r) (tt) (tt (:error r))))))

        (when t-toast
          (case (e/server (e/Offload #(push-toast!* user-id :info (:message toast-request))))
            (do (e/on-unmount #(reset! !toast-request nil))
                (t-toast))))

        (SidePanelShell
          {:side :left
           :class "assistant-side-panel"
           :toggle-aria-label "Toggle assistant panel"
           :resize-aria-label "Resize assistant panel"
           :open? open? :!open? !open? :!save !save
           :width-px width-px :!width-px !width-px :!width-save !width-save}
          (e/fn []
            (if poem?
              (RightPanelTabs tab !tab !tab-save)
              (dom/span
                (dom/props {:class "side-panel__title"})
                (dom/text "Assistant"))))
          (e/fn []
            (if (and poem? (= poem/panel-tab tab))
              (PoemPanel page-topic-id root-topic-id user-id)
              (AssistantPanel page-topic-id root-topic-id user-id))))))))
