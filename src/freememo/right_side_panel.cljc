(ns freememo.right-side-panel
  "Right-side collapsible/resizable panel: Pins, AI Assistant, and — on video
   topics — Transcript.

   Owns the panel chrome — collapse toggle, resize handle, and per-document
   open/width persistence (reusing the pins_open_/pins_width_ settings the pins
   panel used before it gained tabs). The active tab is persisted per document
   as assistant_tab_<root-id>. Tab bodies are rendered by freememo.pin-side-panel
   (PinsBody), freememo.assistant-panel (AssistantPanel), and
   freememo.video-transcript (VideoTranscriptPane).

   The Transcript tab is conditional on kind='video'. A persisted \"transcript\"
   tab therefore has to fall back — otherwise navigating from a video to a PDF
   would select a tab that is not rendered and show an empty panel.

   Frame isolation: the subtree remounts only when root-topic-id changes (i.e.
   navigating between documents), so open/width/tab/active-chat state persists
   across page scrolls within one document but resets across documents."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.doc-context :as dctx]
   [freememo.hierarchy-side-panel :refer [SidePanelShell]]
   [freememo.pin-side-panel :refer [PinsBody]]
   [freememo.assistant-panel :refer [AssistantPanel]]
   [freememo.video-transcript :refer [VideoTranscriptPane]]
   [freememo.viewport :as viewport]
   #?(:clj [freememo.settings :as settings])))

(defn resolve-tab
  "The tab actually shown, given the persisted preference and whether this topic
   is a video.

   Owns the default because it is the only place that knows the topic's kind:
   a video with no saved preference opens on its transcript, everything else on
   pins. `settings/get-assistant-tab` returns nil for 'never chosen' precisely
   so this branch is reachable — it used to default to \"pins\" itself, which
   made the video default dead code.

   Pre:  `persisted` is a tab name or nil.
   Post: one of \"pins\" | \"assistant\" | \"transcript\"; never \"transcript\"
         when `video?` is false, and never nil."
  [persisted video?]
  (cond
    (= persisted "transcript") (if video? "transcript" "pins")
    (= persisted "assistant")  "assistant"
    (= persisted "pins")       "pins"
    ;; nil (or anything unrecognized) = no preference → kind decides.
    video?                     "transcript"
    :else                      "pins"))

(e/defn RightSidePanel [page-topic-id root-topic-id user-id]
  (e/client
    (e/for-by identity [_k [root-topic-id]]
      (let [phone?          (e/watch viewport/!phone?)
            compact?        (e/watch viewport/!compact?)
            persisted-open? (e/server (settings/get-pins-open user-id root-topic-id))
            ;; Phone and portrait-tablet (compact) default to collapsed regardless
            ;; of the persisted desktop pref — the toggle still opens it manually.
            initial-open?   (and (not phone?) (not compact?) persisted-open?)
            !open? (atom initial-open?)
            open? (e/watch !open?)
            !save (atom nil)
            save-val (e/watch !save)
            [t _] (e/Token save-val)
            persisted-width (e/server (settings/get-pins-width user-id root-topic-id))
            !width-px (atom persisted-width)
            width-px (e/watch !width-px)
            !width-save (atom nil)
            width-save (e/watch !width-save)
            [tw _] (e/Token width-save)
            video? dctx/is-video?
            persisted-tab (e/server (settings/get-assistant-tab user-id root-topic-id))
            !tab (atom (resolve-tab persisted-tab video?))
            ;; Re-resolved on read, not just at seed: the seed runs once per
            ;; root-topic-id frame, and `video?` can arrive after it.
            tab (resolve-tab (e/watch !tab) video?)
            !tab-save (atom nil)
            tab-save (e/watch !tab-save)
            [tt _] (e/Token tab-save)]

        (when t
          (let [r (e/server (e/Offload #(settings/save-pins-open user-id root-topic-id save-val)))]
            (case r
              (if (:success r) (t) (t (:error r))))))

        (when tw
          (let [r (e/server (e/Offload #(settings/save-pins-width user-id root-topic-id width-save)))]
            (case r
              (if (:success r) (tw) (tw (:error r))))))

        (when tt
          (let [r (e/server (e/Offload #(settings/save-assistant-tab user-id root-topic-id tab-save)))]
            (case r (if (:success r) (tt) (tt (:error r))))))

        (SidePanelShell
          {:side :left
           :class "pin-side-panel"
           :toggle-aria-label "Toggle side panel"
           :resize-aria-label "Resize side panel"
           :open? open? :!open? !open? :!save !save
           :width-px width-px :!width-px !width-px :!width-save !width-save}
          ;; Tab switch (only when open, via SidePanelShell).
          (e/fn []
            (dom/div
              (dom/props {:class "side-panel__tabs" :role "tablist"})
              (dom/button
                (dom/props {:class (str "side-panel__tab"
                                     (when (= tab "pins") " side-panel__tab--active"))
                            :role "tab" :aria-selected (str (= tab "pins"))})
                (dom/text "Pins")
                (dom/On "click"
                  (fn [_] (reset! !tab "pins") (reset! !tab-save "pins")) nil))
              (dom/button
                (dom/props {:class (str "side-panel__tab"
                                     (when (= tab "assistant") " side-panel__tab--active"))
                            :role "tab" :aria-selected (str (= tab "assistant"))})
                (dom/text "Assistant")
                (dom/On "click"
                  (fn [_] (reset! !tab "assistant") (reset! !tab-save "assistant")) nil))
              (when video?
                (dom/button
                  (dom/props {:class (str "side-panel__tab"
                                       (when (= tab "transcript") " side-panel__tab--active"))
                              :role "tab" :aria-selected (str (= tab "transcript"))})
                  (dom/text "Transcript")
                  (dom/On "click"
                    (fn [_] (reset! !tab "transcript") (reset! !tab-save "transcript")) nil)))))
          ;; Active tab body.
          (e/fn []
            (case tab
              "assistant" (AssistantPanel page-topic-id root-topic-id user-id)
              "transcript" (binding [dctx/video-topic-id page-topic-id]
                             (VideoTranscriptPane))
              (PinsBody page-topic-id user-id))))))))
