(ns freememo.extract-topic-button
  "Extract toolbar button — creates a child topic from the editor selection.
   Extracted from content_toolbar_actions so each e/defn stays under the JVM
   64KB bytecode limit."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [clojure.string :as str]
   [freememo.rich-text-editor :as editor]
   [freememo.content-toolbar-helpers :as helpers]
   [freememo.keyboard :as keyboard]))

(e/defn ExtractTopicButton [user-id topic-id context-mode mod-key]
  (e/client
    (let [!extract-state (atom {:pending nil :error nil})
          extract-state (e/watch !extract-state)
          pending (:pending extract-state)]
      (dom/button
        (dom/props {:class "btn btn-sm btn-primary"
                    :style {:font-weight "500"}
                    :title (if (= context-mode :extract)
                             (str "Extract selected text as a child topic (" mod-key "+Shift+E)")
                             (str "Extract selected text as a topic (" mod-key "+Shift+E)"))})
        (dom/text "Extract")
        (reset! keyboard/!extract-btn-ref dom/node)
        (e/on-unmount (fn [] (reset! keyboard/!extract-btn-ref nil)))
        (dom/On "click"
          (fn [_]
            (when-let [{:keys [html index length]} (editor/get-selection-html!)]
              (when (seq html)
                (editor/highlight-range! index length)
                (swap! !extract-state assoc :pending html :error nil))))
          nil))
      (when (:error extract-state)
        (dom/span
          (dom/props {:style {:color "var(--color-danger)" :font-size "12px"}})
          (dom/text (:error extract-state))))
      (let [[?token _] (e/Token pending)]
        (when-some [token ?token]
          (let [title (let [raw (str/replace (or pending "") #"<[^>]+>" "")]
                        (if (str/blank? raw) "Extract" (subs raw 0 (min 80 (count raw)))))
                result (e/server
                         (helpers/create-extract-topic-safe! topic-id user-id pending title))]
            (if (:success result)
              (do (reset! !extract-state {:pending nil :error nil})
                (token))
              (do (reset! !extract-state {:pending nil :error (or (:error result) "Failed to save extract")})
                (token (or (:error result) "Failed to save extract"))))))))))
