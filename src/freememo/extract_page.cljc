(ns freememo.extract-page
  "Dedicated extract view — centered rich text editor + card generation toolbar + card table."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.logging :as log]
   [freememo.rich-text-editor :as editor]
   [freememo.rich-text-editor-component :refer [RichTextEditorComponent]]
   [freememo.content-toolbar :refer [ContentToolbar]]
   [freememo.content-toolbar-helpers :as ct-helpers]
   [freememo.anki-sync-panels :as sync-panels]
   #?(:clj [freememo.user-state :as us])
   [freememo.content-card-table :refer [ContentCardTable]]
   [freememo.util :refer [start-drag!]]
   #?(:clj [freememo.db :as db])
   #?(:clj [freememo.settings :as settings])
   #?(:clj [freememo.wikipedia :as wiki])))

;; Per-user refresh via user-state registry

;; Query wrapper: takes refresh arg to create Electric reactive dependency
#?(:clj (defn get-topic-by-id* [_refresh topic-id]
          (when topic-id (db/get-topic topic-id))))

(defn import-wikipedia-url*
  "Import a Wikipedia article by URL. Returns {:already-exists true :title ...} or {:imported true :title ...} or {:error ...}."
  [user-id url]
  #?(:clj
     (try
       (let [title (wiki/extract-wiki-title url)]
         (if-not title
           {:error "Not a valid Wikipedia URL"}
           (if-let [existing (db/find-web-topic-by-title user-id title)]
             {:already-exists true :title title :topic-id (:topics/id existing)}
             (let [result (wiki/fetch-url url)]
               (if-not (:success result)
                 {:error (:error result)}
                 (let [topic-id (db/create-web-topic! user-id (:title result) (:html result) (:url result))]
                   {:imported true :title (:title result) :topic-id topic-id}))))))
       (catch Exception e
         {:error (.getMessage e)}))
     :cljs nil))


;; Responsive split pane default — plain defn avoids #? inside e/defn (frame mismatch)
(defn default-split-pct []
  #?(:cljs (if (< (.-innerHeight js/window) 900) 50 75)
     :clj 75))

(e/defn ExtractPage [{:keys [user-id enc-key topic-id navigate! view-source! llm-enabled? origin
                             queue-position priority on-priority-change!]}]
  (e/client
    (dom/div
      (dom/props {:style {:height "100%" :display "flex" :flex-direction "column" :overflow "hidden"}})

      (if (some? topic-id)
        (let [card-font-size (e/server (settings/get-card-font-size user-id))
              refresh (e/server (e/watch (us/get-atom user-id :refresh)))
              sync-mutations (e/server (e/watch (us/get-atom user-id :sync-mutations)))
              card-mutations (e/server (e/watch (us/get-atom user-id :card-mutations)))
              card-refresh (+ refresh sync-mutations card-mutations)
              topic (e/server (get-topic-by-id* refresh topic-id))
              content (e/server (or (:topics/content topic) ""))
              extract-status (e/server (or (:topics/status topic) "active"))
              ;; Parent topic — for page_number and parent content (context)
              parent-id (e/server (:topics/parent_id topic))
              parent-topic (e/server (when parent-id (db/get-topic parent-id)))
              page-number (e/server (:topics/page_number parent-topic))
              parent-title (e/server (:topics/title parent-topic))
              parent-content (e/server (or (:topics/content parent-topic) ""))
              ;; Root topic — for filename, kind, source scoping
              root-topic-id (e/server (db/get-root-topic-id topic-id))
              root-topic (e/server (when (not= root-topic-id topic-id) (db/get-topic root-topic-id)))
              filename (e/server (:topics/title (or root-topic topic)))
              root-kind (e/server (:topics/kind (or root-topic topic)))]

          ;; Auto-save dirty edits to topic content
          ;; Auto-save: one-way editor → DB. Don't clear !dirty-html, don't bump !refresh.
          ;; Track last-saved to prevent re-saving identical content.
          (let [dirty-data (e/watch editor/!dirty-html)
                !last-saved (atom nil)]
            (when (and (some? dirty-data)
                    (= (:topic-id dirty-data) topic-id)
                    (not= (:html dirty-data) (e/watch !last-saved)))
              (log/log-debug (str "Extract auto-save topic-id=" topic-id))
              (let [html-to-save (:html dirty-data)
                    result (e/server
                             (e/Offload
                               #(try
                                  (db/update-topic-content! topic-id html-to-save)
                                  {:success true}
                                  (catch Exception e
                                    (log/log-error (str "Extract save error: " (.getMessage e)))
                                    {:success false :error (.getMessage e)}))))]
                (when (:success result)
                  (reset! !last-saved html-to-save)))))


          ;; Wikipedia link import — watches !import-url atom from Quill tooltip
          (let [import-data (e/watch editor/!import-url)
                [?token _] (e/Token import-data)]
            (when-some [token ?token]
              (let [url (:url import-data)
                    result (e/server
                             (e/Offload
                               #(import-wikipedia-url* user-id url)))]
                ;; Guard: e/Offload is async — result is nil while pending.
                ;; Only close token when the real result arrives.
                (when (some? result)
                  (let [status (cond
                                 (:imported result) :done
                                 (:already-exists result) :already-exists
                                 :else :error)]
                    (reset! editor/!import-status status)
                    (token)
                    (when (= status :done)
                      (e/server (swap! (us/get-atom user-id :refresh) inc))))))))

          ;; Title breadcrumb bar
          (dom/div
            (dom/props {:style {:display "flex" :align-items "center" :position "relative"
                                :padding "4px var(--sp-3)" :flex-shrink "0"
                                :justify-content "center"}})
            (let [parent-is-intermediate (and parent-id root-topic-id (not= parent-id root-topic-id))
                  label (if parent-is-intermediate
                          (or parent-title "Untitled")
                          (if (nil? page-number)
                            (or filename "Untitled")
                            (str (or filename "Unknown") " \u2014 p. " page-number)))]
              ;; Title (absolutely centered, independent of right-side items)
              (dom/span
                (dom/props {:style {:font-size "14px"
                                    :overflow "hidden" :text-overflow "ellipsis" :white-space "nowrap"
                                    :color (if (and view-source! parent-id) "var(--color-primary)" "var(--color-text-secondary)")
                                    :cursor (when (and view-source! parent-id) "pointer")
                                    :text-decoration (when (and view-source! parent-id) "underline")}
                            :title (when (and view-source! parent-id) "View source")})
                (dom/text label)
                (when (and view-source! parent-id)
                  (dom/On "click" (fn [_] (view-source! (if (and parent-is-intermediate (not= root-kind "pdf")) parent-id root-topic-id) page-number root-kind)) nil))))

            ;; Learn session: priority + queue counter (absolute right)
            (when queue-position
              (dom/label
                (dom/props {:style {:display "flex" :align-items "center" :gap "4px" :font-size "12px"
                                    :color "var(--color-text-secondary)"
                                    :position "absolute" :right "var(--sp-3)" :top "50%"
                                    :transform "translateY(-50%)"}
                            :title "Priority (0=highest, 100=lowest)"})
                (dom/text "Priority")
                (e/for-by identity [_k [topic-id]]
                  (dom/input
                    (dom/props {:type "number" :min "0" :max "100"
                                :style {:width "48px" :font-size "12px" :padding "2px 4px"
                                        :border "1px solid var(--color-border)" :border-radius "var(--radius-sm)" :text-align "center"}})
                    (set! (.-value dom/node) (str (or priority 50)))
                    (let [change-event (dom/On "change" (fn [e] (-> e .-target .-value js/parseInt)) nil)
                          [?token _] (e/Token change-event)]
                      (when-some [token ?token]
                        (when on-priority-change! (on-priority-change! change-event))
                        (token))))))
              (dom/span
                (dom/props {:style {:color "var(--color-text-secondary)" :font-size "13px" :margin-left "8px" :flex-shrink "0"}})
                (dom/text queue-position))))

          ;; Split pane: editor top / toolbar+cards bottom
          (let [!top-pct (atom (default-split-pct))
                top-pct (e/watch !top-pct)]

            ;; Editor area
            (dom/div
              (dom/props {:style {:height (str top-pct "%") :min-height "0" :overflow "auto"
                                  :display "flex" :justify-content "center"
                                  :padding "8px 16px"}})
              (dom/div
                (dom/props {:style {:width "100%"
                                    :display "flex" :flex-direction "column"}})
                (RichTextEditorComponent {:initial-html content
                                          :topic-id topic-id})))

            ;; Draggable divider
            (dom/div
              (dom/props {:class "split-divider-v" :title "Drag to resize panels"})
              (dom/On "pointerdown" (fn [e] (start-drag! e :y !top-pct)) nil))

            ;; Bottom: toolbar + cards
            (dom/div
              (dom/props {:style {:flex "1" :display "flex" :flex-direction "column" :min-height "0"}})

              ;; Shared toolbar — content-text from !dirty-html (live) or server content (initial)
              (let [dirty (e/watch editor/!dirty-html)
                    live-content (if (and dirty (= (:topic-id dirty) topic-id))
                                   (:html dirty)
                                   content)]
                (ContentToolbar {:user-id user-id
                                 :enc-key enc-key
                                 :topic-id topic-id
                                 :root-topic-id root-topic-id
                                 :page-number page-number
                                 :content-text live-content
                                 :parent-content parent-content
                                 :context-mode :extract
                                 :context-tooltip "Include context for better cards. With a selection: extract text. Without: original page text."
                                 :llm-enabled? llm-enabled?
                                 :extract-status extract-status
                                 :navigate! navigate!
                                 :origin origin}
                  card-refresh))

              ;; Shared card table
              (ContentCardTable {:topic-id topic-id
                                 :card-font-size card-font-size
                                 :user-id user-id}
                card-refresh))))

        ;; No topic-id
        (dom/div
          (dom/props {:style {:padding "32px" :text-align "center" :color "var(--color-text-secondary)"}})
          (dom/text "No extract selected.")
          (dom/div
            (dom/props {:style {:margin-top "12px"}})
            (dom/button
              (dom/props {:class "btn btn-sm btn-secondary" :style {:padding "6px 16px" :background "var(--color-bg-subtle)"}})
              (dom/text "Go to Learn")
              (dom/On "click" (fn [_] (navigate! (or origin :learn))) nil))))))))
