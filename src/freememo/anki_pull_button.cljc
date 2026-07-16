(ns freememo.anki-pull-button
  "Pull-from-Anki toolbar button. Extracted from content_toolbar_actions
   so each e/defn stays under the JVM 64KB bytecode limit."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.anki-sync-helpers :as anki]
   [freememo.icons :as icons]
   [freememo.command-bus :as bus]
   [freememo.tooltip :as tooltip]
   #?(:clj [freememo.anki-sync-server :as sync])))

(e/defn PullFromAnkiButton [user-id root-topic-id]
  (e/client
    (let [!pull-phase (atom nil)
          pull-phase (e/watch !pull-phase)
          !pull-result (atom nil)
          !pull-error (atom nil)
          !pull-updates (atom nil)
          pull-result (e/watch !pull-result)
          pull-error (e/watch !pull-error)
          in-flight? (boolean (#{:pulling :recording} pull-phase))
          label (case pull-phase
                  :pulling "Pulling..."
                  :recording "Saving..."
                  :done (let [{:keys [updates deleted]} pull-result
                              u (count (or updates []))
                              d (count (or deleted []))]
                          (cond
                            (and (zero? u) (zero? d)) "Pulled — up to date"
                            (and (pos? u) (pos? d)) (str "Pulled — " u " updated, " d " deleted")
                            (pos? u) (str "Pulled — " u " updated")
                            :else (str "Pulled — " d " deleted")))
                  :error (str "Pull failed: " (or (:message pull-error) "error"))
                  "Pull from Anki")]

      (dom/button
        (dom/props {:class "btn btn-sm btn-secondary"
                    :style {:background "var(--color-bg-subtle)"
                            :color "var(--color-text-primary)"
                            :font-weight "500"}
                    :disabled in-flight?
                    :aria-label "Pull from Anki"})
        (tooltip/Tooltip! (case pull-phase
                            :error (or (:message pull-error) "Pull failed")
                            "Pull edits from Anki for this document."))
        (icons/Icon :cloud-download :size 16)
        (dom/span (dom/props {:class "icon-label"}) (dom/text label))
        (let [node dom/node]
          (bus/publish-invoker! :pull-anki (fn [] (.click node)))
          (e/on-unmount (fn [] (bus/retract-invoker! :pull-anki))))
        (dom/On "click"
          (fn [_]
            (reset! !pull-phase :pulling)
            (reset! !pull-result nil)
            (reset! !pull-error nil)
            (reset! !pull-updates nil))
          nil))

      ;; Phase :pulling — fetch synced cards for the entire doc, then run pull task.
      (when (= pull-phase :pulling)
        (let [cards-result (e/server (e/Offload #(sync/get-cards-for-sync
                                                    {:user-id user-id
                                                     :topic-id nil
                                                     :root-topic-id root-topic-id})))]
          (if-not (:success cards-result)
            (do (reset! !pull-error {:message (:error cards-result) :source :server})
              (reset! !pull-phase :error))
            (anki/run-pull! (:cards cards-result)
              {:!phase !pull-phase
               :!result !pull-result
               :!error !pull-error
               :!pull-updates !pull-updates}))))

      ;; Phase :error — ship a client-origin pull exception (browser stack +
      ;; context) to the server log. Re-fires per error episode: the block
      ;; unmounts when pull-phase leaves :error, so a fresh Token is minted on
      ;; the next failure. No-op for server-origin errors (logger gates on
      ;; :source :client).
      (when (= pull-phase :error)
        (let [[t _] (e/Token ::pull-error-report)]
          (when t
            (case (e/server (sync/log-client-sync-error! user-id root-topic-id pull-error))
              (t)))))

      ;; Phase :recording — single e/server call applies updates and bumps
      ;; sync-mutations atomically (Electric drops sibling intermediate
      ;; e/server side effects in do-bodies — keep them in one server fn).
      (when (and (= pull-phase :recording) (some? (e/watch !pull-updates)))
        (let [pull-data (e/watch !pull-updates)
              updates (:updates pull-data)
              deleted (:deleted pull-data)
              [t _] (e/Token [::toolbar-record-pull root-topic-id])]
          (when t
            (let [result (e/server (e/Offload #(sync/apply-pull-updates user-id updates deleted)))]
              (when (some? result)
                (if (:success result)
                  (case (e/client (reset! !pull-phase :done)) (t))
                  (case (e/client (reset! !pull-error {:message (:error result) :source :server})
                          (reset! !pull-phase :error))
                    (t)))))))))))
