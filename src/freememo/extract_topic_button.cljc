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
   [freememo.keyboard :as keyboard]
   [freememo.logging :as log]
   #?(:clj [freememo.user-state :as us])))

(e/defn ExtractTopicButton [user-id topic-id context-mode mod-key]
  (e/client
    (let [!error (atom nil)
          error (e/watch !error)]
      (dom/button
        (dom/props {:class "btn btn-sm btn-primary"
                    :style {:font-weight "500"}
                    :title (if (= context-mode :extract)
                             (str "Extract selected text as a child topic (" mod-key "+Shift+E)")
                             (str "Extract selected text as a topic (" mod-key "+Shift+E)"))})
        (dom/text "Extract")
        (reset! keyboard/!extract-btn-ref dom/node)
        (e/on-unmount (fn [] (reset! keyboard/!extract-btn-ref nil)))
        ;; Event-driven token (one-shot per click) — prevents the e/server
        ;; body from re-firing if upstream reactive deps change while the
        ;; token is open (e.g. :refresh bump after success).
        ;;
        ;; e/Offload wraps the server fn to (a) memoize the thunk so settling
        ;; re-evaluations don't trigger duplicate inserts, and (b) return nil
        ;; while pending so we can guard the success branch with `some?`.
        (let [click-html (dom/On "click"
                           (fn [_]
                             (let [click-id (.toString (js/Date.now))]
                               (log/log-debug (str "[EXTRACT-CLICK " click-id "] handler fired"))
                               (when-let [{:keys [html index length]} (editor/get-selection-html!)]
                                 (log/log-debug (str "[EXTRACT-CLICK " click-id "] selection captured len=" (count html)))
                                 (when (seq html)
                                   (editor/highlight-range! index length)
                                   (reset! !error nil)
                                   (log/log-debug (str "[EXTRACT-CLICK " click-id "] returning html"))
                                   html))))
                           nil)
              [t _] (e/Token click-html)]
          (when t
            (log/log-debug (str "[EXTRACT-WHEN-SOME] token activated, html-len=" (count click-html)))
            (let [html click-html
                  title (let [raw (str/replace (or html "") #"<[^>]+>" "")]
                          (if (str/blank? raw) "Extract" (subs raw 0 (min 80 (count raw)))))
                  result (e/server
                           (e/Offload
                             #(helpers/create-extract-topic-safe! topic-id user-id html title)))]
              (case result
                (do (log/log-debug (str "[EXTRACT-RESULT] result=" (pr-str result)))
                  (if (:success result)
                    (do (log/log-debug "[EXTRACT-SUCCESS] closing token + bumping :tree-mutations")
                      (case (e/server
                              (do (log/log-info "[EXTRACT-BUMP-TREE] bumping :tree-mutations")
                                (swap! (us/get-atom user-id :tree-mutations) inc)))
                        (t)))
                    (do (log/log-debug (str "[EXTRACT-ERROR] err=" (pr-str (:error result))))
                      (reset! !error (or (:error result) "Failed to save extract"))
                      (t (or (:error result) "Failed to save extract"))))))))))
      (when error
        (dom/span
          (dom/props {:style {:color "var(--color-danger)" :font-size "12px"}})
          (dom/text error))))))
