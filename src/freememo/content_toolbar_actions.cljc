(ns freememo.content-toolbar-actions
  "Extract, Add, Export, and Anki Sync buttons for ContentToolbar.
   Extracted to stay under JVM 64KB method limit."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [clojure.string :as str]
   [freememo.rich-text-editor :as editor]
   [freememo.anki-sync :refer [AnkiSyncButton]]
   [freememo.card-modals :refer [ExportModal AddCardModal]]
   [freememo.content-toolbar-helpers :as helpers]
   [freememo.keyboard :as keyboard]))

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
            show-add (e/watch !show-add)]
        (dom/button
          (dom/props {:class "btn btn-sm btn-secondary" :style {:font-weight "500"}})
          (dom/text "Add new")
          (dom/On "click" (fn [_] (reset! !show-add true)) nil))
        (when show-add
          (AddCardModal !show-add card-type topic-id root-topic-id source-ref)))

      ;; Separator
      (dom/span (dom/props {:style {:color "var(--color-border)"}}) (dom/text "|"))

      ;; Export button + modal
      (let [!show-export (atom false)
            show-export (e/watch !show-export)]
        (dom/button
          (dom/props {:class "btn btn-sm btn-secondary" :style {:font-weight "500"}})
          (dom/text (if (pos? unsynced-count)
                      (str "Export (" unsynced-count ")...")
                      "Export..."))
          (dom/On "click" (fn [_] (reset! !show-export true)) nil))
        (when show-export
          (ExportModal !show-export topic-id root-topic-id user-id)))

      ;; Separator
      (dom/span (dom/props {:style {:color "var(--color-border)"}}) (dom/text "|"))

      ;; Anki Sync button
      (AnkiSyncButton user-id root-topic-id page-number card-type unsynced-count))))
