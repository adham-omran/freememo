(ns freememo.transcribe-button
  "Transcribe toolbar button for audio topics. Runs Whisper on the topic's
   stored audio and replaces the editor content with the transcript. Warns
   before replacing existing text; on confirm it clears the dirty-edit guard so
   the post-transcribe reactive reload isn't suppressed."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [clojure.string :as str]
   [freememo.rich-text-editor :as editor]
   [freememo.icons :as icons]
   #?(:clj [freememo.user-state :as us])
   #?(:clj [freememo.transcribe :as transcribe])))

(e/defn TranscribeButton [user-id topic-id enc-key]
  (e/client
    (let [!confirm (atom nil)
          confirm (e/watch !confirm)
          !fire (atom nil)
          fire (e/watch !fire)
          transcribing? (e/server
                          (contains? (e/watch (us/get-atom user-id :transcribing-topics)) topic-id))
          ;; A fresh id per launch so e/Token re-opens on each confirmed start.
          start! (fn [] (reset! !fire {:id (str (random-uuid))}))]
      (dom/button
        (dom/props {:class "btn btn-sm btn-primary"
                    :style {:font-weight "500"}
                    :aria-label "Transcribe"
                    :data-tooltip "Transcribe the audio into the editor"
                    :disabled transcribing?})
        (icons/Icon :mic :size 16)
        (dom/span (dom/props {:class "icon-label"})
          (dom/text (if transcribing? "Transcribing…" "Transcribe")))
        ;; Empty editor → transcribe directly. Non-empty → warn first.
        (dom/On "click"
          (fn [_]
            (when-not transcribing?
              (let [html (editor/get-current-html!)]
                (if (str/blank? (str/replace (or html "") #"<[^>]+>" ""))
                  (start!)
                  (reset! !confirm :confirming)))))
          nil))
      ;; Fire effect — launches the async server task once per !fire value.
      (let [[t _] (e/Token fire)]
        (when t
          (case (e/server (transcribe/start-transcribe! user-id topic-id enc-key))
            (do (reset! !fire nil) (t)))))
      ;; Warn-before-replace modal.
      (when (= confirm :confirming)
        (dom/div
          (dom/props {:class "modal-backdrop"})
          (dom/On "click" (fn [_] (reset! !confirm nil)) nil)
          (dom/On "keydown" (fn [e] (when (= (.-key e) "Escape") (reset! !confirm nil))) nil)
          (dom/div
            (dom/props {:class "modal-content modal-sm"})
            (dom/On "click" (fn [e] (.stopPropagation e)) nil)
            (dom/div
              (dom/props {:class "confirm-modal-body"})
              (dom/p (dom/text "Transcribing will replace the current editor content. Continue?")))
            (dom/div
              (dom/props {:class "confirm-modal-actions"})
              (dom/button
                (dom/props {:class "btn btn-secondary"})
                (dom/text "Cancel")
                (dom/On "click" (fn [_] (reset! !confirm nil)) nil))
              (dom/button
                (dom/props {:class "btn btn-primary"})
                (dom/text "Replace")
                (dom/On "click"
                  (fn [_]
                    ;; Discard unsaved edits so the reactive reload after the
                    ;; transcript is written isn't suppressed by the dirty guard.
                    (reset! editor/!dirty-html nil)
                    (reset! !confirm nil)
                    (start!))
                  nil)))))))))
