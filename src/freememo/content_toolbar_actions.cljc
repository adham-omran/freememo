(ns freememo.content-toolbar-actions
  "Extract, Add, Export, Pull from Anki, and Anki Sync buttons for ContentToolbar.
   Extracted to stay under JVM 64KB method limit."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [clojure.string :as str]
   [freememo.rich-text-editor :as editor]
   [freememo.anki-sync :refer [AnkiSyncButton]]
   [freememo.anki-sync-helpers :as anki]
   [freememo.card-modals :refer [ExportModal AddCardModal]]
   [freememo.content-toolbar-helpers :as helpers]
   [freememo.keyboard :as keyboard]
   #?(:clj [freememo.anki-sync-server :as sync])))

(e/defn ToolbarActions [cfg]
  (e/client
    (let [{:keys [user-id topic-id root-topic-id page-number
                  context-mode mod-key source-ref unsynced-count
                  card-type]} cfg]

      ;; Extract button — create child topic from selected text
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
                  (token (or (:error result) "Failed to save extract"))))))))

      ;; Add new card button
      (let [!show-add (atom false)
            show-add (e/watch !show-add)
            !card-kind (atom card-type)
            !captured-selection (atom "")]
        (dom/button
          (dom/props {:class "btn btn-sm btn-secondary toolbar-overflow-item" :style {:font-weight "500"}})
          (dom/text "Add new")
          (reset! keyboard/!add-new-btn-ref dom/node)
          (e/on-unmount (fn [] (reset! keyboard/!add-new-btn-ref nil)))
          (dom/On "click"
            (fn [_]
              (reset! !captured-selection (or (editor/get-selected-text!) ""))
              (reset! !show-add true))
            nil))
        (when show-add
          (AddCardModal !show-add !card-kind !captured-selection topic-id root-topic-id source-ref user-id)))

      ;; Separator
      (dom/span (dom/props {:class "toolbar-overflow-item" :style {:color "var(--color-border)"}}) (dom/text "|"))

      ;; Export button + modal
      (let [!show-export (atom false)
            show-export (e/watch !show-export)]
        (dom/button
          (dom/props {:class "btn btn-sm btn-secondary toolbar-overflow-item" :style {:font-weight "500"}})
          (dom/text (if (pos? unsynced-count)
                      (str "Export (" unsynced-count ")...")
                      "Export..."))
          (reset! keyboard/!export-btn-ref dom/node)
          (e/on-unmount (fn [] (reset! keyboard/!export-btn-ref nil)))
          (dom/On "click" (fn [_] (reset! !show-export true)) nil))
        (when show-export
          (ExportModal !show-export topic-id root-topic-id user-id)))

      ;; Separator
      (dom/span (dom/props {:class "toolbar-overflow-item" :style {:color "var(--color-border)"}}) (dom/text "|"))

      ;; Pull from Anki — full-doc pull, no modal needed.
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
                [?token _] (e/Token [::toolbar-record-pull root-topic-id])]
            (when-some [token ?token]
              (let [result (e/server (sync/apply-pull-updates user-id updates deleted))]
                (if (:success result)
                  (do (reset! !pull-phase :done)
                    (token))
                  (do (reset! !pull-error (:error result))
                    (reset! !pull-phase :error)
                    (token))))))))

      ;; Anki Sync button
      (AnkiSyncButton user-id root-topic-id page-number card-type unsynced-count))))
