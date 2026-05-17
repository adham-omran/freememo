(ns freememo.anki-pull-button
  "Pull-from-Anki toolbar button. Extracted from content_toolbar_actions
   so each e/defn stays under the JVM 64KB bytecode limit."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.anki-sync-helpers :as anki]
   [freememo.keyboard :as keyboard]
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
                  :error (str "Pull failed: " (or pull-error "error"))
                  "Pull from Anki")]

      (dom/button
        (dom/props {:class "btn btn-sm btn-secondary toolbar-overflow-item"
                    :style {:background "var(--color-bg-subtle)"
                            :color "var(--color-text-primary)"
                            :font-weight "500"}
                    :disabled in-flight?
                    :title (case pull-phase
                             :error (or pull-error "Pull failed")
                             "Pull edits from Anki for this document. Reads notes by their stored Anki note id.")})
        (dom/text label)
        (reset! keyboard/!pull-anki-btn-ref dom/node)
        (e/on-unmount (fn [] (reset! keyboard/!pull-anki-btn-ref nil)))
        (dom/On "click"
          (fn [_]
            (reset! !pull-phase :pulling)
            (reset! !pull-result nil)
            (reset! !pull-error nil)
            (reset! !pull-updates nil))
          nil))

      ;; Phase :pulling — fetch synced cards for the entire doc, then run pull task.
      (when (= pull-phase :pulling)
        (let [cards-result (e/server (sync/get-cards-for-sync
                                       {:user-id user-id
                                        :topic-id nil
                                        :root-topic-id root-topic-id}))]
          (if-not (:success cards-result)
            (do (reset! !pull-error (:error cards-result))
              (reset! !pull-phase :error))
            (anki/run-pull! (:cards cards-result)
              {:!phase !pull-phase
               :!result !pull-result
               :!error !pull-error
               :!pull-updates !pull-updates}))))

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
                  (case (e/client (reset! !pull-error (:error result))
                          (reset! !pull-phase :error))
                    (t)))))))))))
