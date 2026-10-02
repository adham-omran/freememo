(ns freememo.extract-topic-button
  "Extract toolbar button — creates a child topic from the editor selection.
   Extracted from content_toolbar_actions so each e/defn stays under the JVM
   64KB bytecode limit."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [clojure.string :as str]
   [freememo.selected-passage :as selected-passage]
   [freememo.content-toolbar-helpers :as helpers]
   [freememo.icons :as icons]
   [freememo.commands :as commands]
   [freememo.command-bus :as bus]
   [freememo.commands :as commands]
   [freememo.logging :as log]
   [freememo.tooltip :as tooltip]))

(e/defn ExtractTopicButton [user-id topic-id context-mode]
  (e/client
    (let [!error (atom nil)
          error (e/watch !error)
          chord (commands/display-chord :extract)]
      (dom/button
        ;; A ghost button: Generate is the toolbar's one filled accent
        ;; (plans/scannable-settings-toolbar-accent.md §2).
        (dom/props {:class "btn btn-sm btn-secondary"
                    :aria-label "Extract"})
        (dom/props {:data-tour "extract"})
        (tooltip/Tooltip! (if (= context-mode :extract)
                            (str "Extract selected text as a child topic (" chord ")")
                            (str "Extract selected text as a topic (" chord ")")))
        (icons/Icon :scissors :size 16)
        (dom/span (dom/props {:class "icon-label icon-label--tier-2"}) (dom/text "Extract"))
        (let [node dom/node]
          (bus/publish-invoker! :extract (fn [] (.click node)))
          (e/on-unmount (fn [] (bus/retract-invoker! :extract))))
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
                               ;; Either code-pane view wins over the notes, as
                               ;; for Generate (freememo.selected-passage). The
                               ;; pane is read-only, so only an editor passage is
                               ;; marked.
                               (when-let [{:keys [html surface] :as passage} (selected-passage/current)]
                                 (log/log-debug (str "[EXTRACT-CLICK " click-id "] " surface " selection len=" (count html)))
                                 (selected-passage/mark! passage)
                                 (reset! !error nil)
                                 {:html html :code-pane? (not= :editor surface)})))
                           nil)
              [t _] (e/Token click-html)]
          (when t
            (log/log-debug (str "[EXTRACT-WHEN-SOME] token activated, html-len=" (count (:html click-html))))
            (let [html (:html click-html)
                  code-pane? (:code-pane? click-html)
                  title (let [raw (str/replace (or html "") #"<[^>]+>" "")]
                          (if (str/blank? raw) "Extract" (subs raw 0 (min 80 (count raw)))))
                  result (e/server
                           (e/Offload
                             #(if code-pane?
                                (helpers/create-code-pane-extract! topic-id user-id html title)
                                (helpers/create-extract-topic-safe! topic-id user-id html title))))]
              (case result
                (do (log/log-debug (str "[EXTRACT-RESULT] result=" (pr-str result)))
                  (if (:success result)
                    (do (log/log-debug "[EXTRACT-SUCCESS] closing token + bumping :extract views")
                      (case (e/server (commands/bump! user-id :extract))
                        (t)))
                    (do (log/log-debug (str "[EXTRACT-ERROR] err=" (pr-str (:error result))))
                      (reset! !error (or (:error result) "Failed to save extract"))
                      (t (or (:error result) "Failed to save extract"))))))))))
      (when error
        (dom/span
          (dom/props {:style {:color "var(--color-danger-text)" :font-size "12px"}})
          (dom/text error))))))
