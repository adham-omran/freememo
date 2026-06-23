(ns freememo.editor-pane
  "EditorPane sub-component for TopicPage.
   Renders the compact page-header (PDF mode) + auto-save + wiki import + rich-text editor area."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.logging :as log]
   [freememo.rich-text-editor :as editor]
   [freememo.rich-text-editor-component :refer [RichTextEditorComponent]]
   [freememo.keyboard :as keyboard]
   [freememo.copy-text :as copy]
   [freememo.navigation :as nav]
   [clojure.string :as str]
   #?(:cljs [freememo.editor-pin-menu :as pin-menu])
   #?(:clj [freememo.page-ocr :as page])
   #?(:clj [freememo.toasts :as toasts])
   #?(:clj [freememo.user-state :as us])
   #?(:clj [freememo.db :as db])
   #?(:clj [freememo.web-import :as web-import])
   #?(:clj [missionary.core :as m]))
  #?(:clj (:import [missionary Cancelled]
                   [java.util.concurrent Executors])))

;; ---------------------------------------------------------------------------
;; Pin contextmenu helper — wraps CLJS-only install-contextmenu! so the
;; reader conditional lives in a plain defn body, not in the Electric graph.
;; ---------------------------------------------------------------------------

(defn install-pin-contextmenu!
  "Attach pin contextmenu to container-el. Returns a cleanup fn.
   CLJS-only; returns nil on CLJ."
  [container-el get-topic-id get-pin-count on-pin!]
  #?(:cljs (pin-menu/install-contextmenu!
             container-el get-topic-id get-pin-count on-pin!)
     :clj nil))

(defn count-pins-for-topic*
  "Server-side pin count. Takes _pin-rev as first arg so the value is part of
   the call site — forces Electric to subscribe to the :pin-mutations channel
   that produced _pin-rev. Matches pin_side_panel/get-pins-for-topic*."
  [_pin-rev topic-id]
  #?(:clj (count (db/get-pins topic-id))
     :cljs 0))

;; ---------------------------------------------------------------------------
;; OCR scan infrastructure (moved from page_viewer.cljc)
;; ---------------------------------------------------------------------------

#?(:clj (defonce ocr-executor (Executors/newFixedThreadPool 3)))

(defn start-ocr-scan!
  "Submit an OCR scan as a Missionary task on the bounded executor.
   Timeout after 30s. Cancel function stored in per-user :scan-cancellers."
  [uid doc page ek dpi]
  #?(:clj
     (do
       (swap! (us/get-atom uid :scanning-pages) conj [doc page])
       (swap! (us/get-atom uid :ocr-errors) dissoc [doc page])
       (log/log-info (str "OCR scan started topic-id=" doc " page=" page))
       (let [cancel-fn
             ((m/timeout
                (m/via ocr-executor (page/extract-page-text uid doc page ek dpi))
                30000
                {:success false :error "Scan timed out after 30 seconds"})
              (fn [result]
                (swap! (us/get-atom uid :scan-cancellers) dissoc [doc page])
                (if (:success result)
                  (do (log/log-info (str "OCR scan complete topic-id=" doc " page=" page))
                    (swap! (us/get-atom uid :refresh) inc))
                  (do (log/log-info (str "OCR scan failed topic-id=" doc " page=" page " error=" (:error result)))
                    (swap! (us/get-atom uid :ocr-errors) assoc [doc page] (:error result))
                    (toasts/push! uid
                      {:level :error
                       :message (:error result)
                       :actions (if (= :insufficient-credits (:error-type result))
                                  [{:label "Top up credits" :nav :settings}]
                                  [])})))
                (swap! (us/get-atom uid :scanning-pages) disj [doc page]))
              (fn [e]
                (swap! (us/get-atom uid :scan-cancellers) dissoc [doc page])
                (when-not (instance? Cancelled e)
                  (log/log-info (str "OCR scan error topic-id=" doc " page=" page " ex=" (.getMessage e)))
                  (swap! (us/get-atom uid :ocr-errors) assoc [doc page] (.getMessage e))
                  (toasts/push! uid {:level :error :message (.getMessage e)}))
                (swap! (us/get-atom uid :scanning-pages) disj [doc page])))]
         (swap! (us/get-atom uid :scan-cancellers) assoc [doc page] cancel-fn))
       nil)
     :cljs nil))

;; ---------------------------------------------------------------------------
;; EditorPane
;; ---------------------------------------------------------------------------

(e/defn EditorPane
  [{:keys [user-id enc-key topic-id audio-topic-id is-pdf-page? root-topic-id page-number
           scan-dpi llm-enabled?
           static-content scanning-pages ocr-errors
           on-imported-navigate!]}]
  (e/client
    (dom/div
      (dom/props {:style {:flex "1" :display "flex" :flex-direction "column"
                          :min-width "0" :min-height "0" :overflow "hidden"}})

      ;; -----------------------------------------------------------------------
      ;; Auto-save + save-status: watch editor/!dirty-html, save when topic-id
      ;; matches. We compute save-result once and use it for both last-saved
      ;; tracking and the status indicator (PDF mode).
      ;; -----------------------------------------------------------------------
      (let [dirty-data (e/watch editor/!dirty-html)
            !last-saved (atom nil)
            needs-save? (and (some? dirty-data)
                          (= (:topic-id dirty-data) topic-id)
                          (not= (:html dirty-data) (e/watch !last-saved)))
            save-result (when needs-save?
                          (log/log-debug (str "EditorPane auto-save topic-id=" topic-id))
                          (e/server
                            (e/Offload
                              #(try
                                 (db/update-topic-content! topic-id (:html dirty-data))
                                 {:success true}
                                 (catch Exception ex
                                   {:success false :error (.getMessage ex)})))))]
        (when (:success save-result)
          (reset! !last-saved (:html dirty-data)))
        (when (and (some? save-result) (not (:success save-result)))
          (log/log-error (str "EditorPane auto-save error topic-id=" topic-id
                           " err=" (:error save-result))))

        ;; -----------------------------------------------------------------------
        ;; Wikipedia link import effect
        ;; -----------------------------------------------------------------------
        (let [import-data (e/watch editor/!import-url)
              [t _] (e/Token import-data)]
          (when t
            (e/on-unmount #(reset! editor/!import-url nil))
            (let [url (:url import-data)
                  result (e/server
                           (e/Offload
                             #(web-import/import-url!* user-id url)))]
              (case result
                (let [status (cond
                               (and (:ok result) (= :imported (:flow result)))       :done
                               (and (:ok result) (= :already-exists (:flow result))) :already-exists
                               :else :error)]
                  (reset! editor/!import-status status)
                  (if (and (#{:done :already-exists} status)
                        (:topic-id result) on-imported-navigate!)
                    (case (on-imported-navigate! (:topic-id result)) (t))
                    (t)))))))

        ;; -----------------------------------------------------------------------
        ;; Audio player (audio topics) — sits on top of the editor; the
        ;; Transcribe action lives in the ContentToolbar below.
        ;; -----------------------------------------------------------------------
        (when audio-topic-id
          (dom/div
            (dom/props {:style {:padding "var(--sp-2)" :flex-shrink "0"
                                :border-bottom "1px solid var(--color-border)"}})
            (dom/element "audio"
              (dom/props {:controls true
                          :src (str "/api/audio/" audio-topic-id)
                          :style {:width "100%"}}))))

        ;; -----------------------------------------------------------------------
        ;; Page header (PDF mode only)
        ;; -----------------------------------------------------------------------
        (when is-pdf-page?
          (dom/div
            (dom/props {:style {:display "flex" :align-items "center"
                                :gap "var(--sp-2)" :padding "var(--sp-1) var(--sp-2)"
                                :flex-shrink "0"}})

            ;; Done checkbox
            (dom/label
              (dom/props {:style {:display "flex" :align-items "center" :gap "3px" :font-size "12px" :cursor "pointer"}
                          :title "Mark this page as completed to track your extraction progress"})
              (e/for-by identity [_page [page-number]]
                (dom/input
                  (dom/props {:type "checkbox"})
                  (let [is-done (e/server (when (and root-topic-id page-number)
                                            (db/get-page-done-status root-topic-id page-number)))]
                    (set! (.-checked dom/node) (boolean is-done)))
                  (reset! keyboard/!done-btn-ref dom/node)
                  (e/on-unmount (fn [] (reset! keyboard/!done-btn-ref nil)))
                  (let [change-event (dom/On "change"
                                       (fn [ev] {:checked (-> ev .-target .-checked)
                                                 :page page-number})
                                       nil)
                        [t _] (e/Token change-event)]
                    (when t
                      (case (e/server (db/toggle-page-done! root-topic-id (:page change-event)))
                        (case (e/server (swap! (us/get-atom user-id :meta-refresh) inc))
                          (t)))))))
              (dom/text (str "Mark page " page-number " as done")))

            ;; Scan Page button (AI OCR) — shown whenever LLM features are on.
            (when llm-enabled?
              (let [scanning? (contains? scanning-pages [root-topic-id page-number])
                    disabled? scanning?]
                (dom/button
                  (dom/props {:class "btn btn-sm btn-primary"
                              :style {:padding "4px 12px" :font-size "14px"
                                      :background (if disabled? "var(--color-border)" "var(--color-primary)")
                                      :cursor (if disabled? "not-allowed" "pointer")}
                              :disabled disabled?})
                  (dom/text (if disabled? "Scanning..." "Scan Page"))
                  (reset! keyboard/!scan-btn-ref dom/node)
                  (e/on-unmount (fn [] (reset! keyboard/!scan-btn-ref nil)))
                  (let [click-event (dom/On "click"
                                      (fn [_] {:id (str (random-uuid)) :page page-number})
                                      nil)
                        [t _] (e/Token click-event)]
                    (when t
                      (case (e/server
                              (let [pg (:page click-event)
                                    doc root-topic-id
                                    uid user-id
                                    ek enc-key]
                                (if (contains? @(us/get-atom uid :scanning-pages) [doc pg])
                                  (log/log-info (str "OCR scan already in progress topic-id=" doc " page=" pg))
                                  (start-ocr-scan! uid doc pg ek scan-dpi))))
                        (t)))))))

            ;; Copy text — single native-extract button. Reads the per-PDF
            ;; extraction style (reactive on :settings-refresh): set → run that
            ;; engine and save silently; unset → run both + compare modal.
            (let [settings-refresh (e/server (e/watch (us/get-atom user-id :settings-refresh)))
                  extract-style (e/server (copy/get-extract-style* settings-refresh user-id root-topic-id))]
              (copy/CopyTextButton user-id root-topic-id page-number extract-style)
              (copy/CopyAllTextButton user-id root-topic-id page-number extract-style))

            ;; OCR error display — auto-dismiss after 3 seconds
            (when-let [ocr-err (get ocr-errors [root-topic-id page-number])]
              (let [!show (atom true)
                    show (e/watch !show)]
                (dom/div
                  (dom/props {:style {:padding "6px 10px"
                                      :background "var(--color-danger-bg)"
                                      :border "1px solid var(--color-danger-light)"
                                      :border-radius "var(--radius-sm)"
                                      :font-size "13px"
                                      :color "var(--color-danger-dark)"
                                      :margin-top "var(--sp-1)"
                                      :opacity (if show "1" "0")
                                      :transition "opacity 0.5s ease-out"}})
                  (dom/text ocr-err)
                  (e/client
                    (js/setTimeout (fn [] (reset! !show false)) 3000)))))

            ;; Save status indicator with fade-out (reuses save-result from auto-save above)
            (when (some? save-result)
              (let [is-success (:success save-result)
                    message (if is-success "Saved" (str "Save error: " (:error save-result)))
                    !show (atom true)
                    show (e/watch !show)]
                (dom/span
                  (dom/props {:style {:margin-left "12px"
                                      :font-size "12px"
                                      :color (if is-success "var(--color-text-secondary)" "var(--color-danger)")
                                      :opacity (if show "1" "0")
                                      :transition "opacity 0.5s ease-out"}})
                  (dom/text message)
                  (when is-success
                    (e/client
                      (js/setTimeout (fn [] (reset! !show false)) 2000))))))))

        ;; -----------------------------------------------------------------------
        ;; Pin-request atom — CLJS contextmenu callback fires into this atom;
        ;; e/Token picks it up and runs the server-side set-pin! call.
        ;; Shape: {:media-id <int> :placement "front"|"back" :topic-id <int>}
        ;; or nil (no pending request).
        ;; -----------------------------------------------------------------------
        (let [!pin-request (atom nil)
              pin-request (e/watch !pin-request)
              ;; Reactive pin count — used for K1 cap check in the context menu
              ;; Watch :pin-mutations so the menu's K1 cap check refreshes
              ;; after the side panel adds/removes a pin.
              ;; CRITICAL: pin-rev must be PASSED INTO the server fn (not
              ;; ignored via `_` binding), otherwise Electric optimizes the
              ;; subscription away — see pin_side_panel.cljc/get-pins-for-topic*
              ;; for the working precedent.
              pin-rev (e/server (e/watch (us/get-atom user-id :pin-mutations)))
              pin-count (e/server (count-pins-for-topic* pin-rev topic-id))]

          ;; e/Token: handle one pin-request at a time
          (let [[t _] (e/Token pin-request)]
            (when t
              (e/on-unmount #(reset! !pin-request nil))
              (let [{:keys [media-id placement topic-id]} pin-request
                    result (e/server
                             (e/Offload
                               #(try
                                  (db/set-pin! {:topic-id topic-id
                                                :media-id media-id
                                                :placement placement})
                                  {:success true}
                                  (catch clojure.lang.ExceptionInfo ex
                                    (let [data (ex-data ex)]
                                      {:success false :reason (:reason data)}))
                                  (catch Exception ex
                                    {:success false :error (.getMessage ex)}))))]
                (case result
                  (if (:success result)
                    (case (e/server (swap! (us/get-atom user-id :pin-mutations) inc))
                      (t))
                    (do (log/log-debug (str "EditorPane set-pin! failed: " result))
                      (t)))))))

          ;; -----------------------------------------------------------------------
          ;; Editor area
          ;; -----------------------------------------------------------------------
          (dom/div
            (dom/props {:style {:flex "1" :min-height "0" :overflow "hidden"}})
            (if (nil? static-content)
              ;; Genuinely loading — server fetch in flight
              (dom/p
                (dom/props {:style {:color "var(--color-text-hint)"}})
                (dom/span (dom/props {:class "spinner"}))
                (dom/text "Loading text..."))
              ;; Render editor for any string (including "") — PDF pages without
              ;; scanned text yet are editable
              (dom/div
                (dom/props {:style {:height "100%" :display "flex" :flex-direction "column"
                                    :overflow "hidden"}
                            :data-role "editor-host"})
                (RichTextEditorComponent {:initial-html static-content
                                          :topic-id topic-id})
                ;; Install contextmenu listener on this container after mount.
                ;; The accessor closures MUST be created here (inside the Electric
                ;; reactive let), not inside js/setTimeout — closures created in
                ;; plain CLJS callbacks snapshot values once; closures created in
                ;; the reactive body capture signal references and re-read the
                ;; latest value at invocation (CLAUDE.md "fn closures capture
                ;; signal references"). Without this, the K1 cap check is stale
                ;; until full page refresh.
                (let [host dom/node
                      !cleanup-fn (atom nil)
                      get-topic-id (fn [] topic-id)
                      get-pin-count (fn [] pin-count)
                      on-pin-fn (fn [req] (reset! !pin-request req))]
                  (js/setTimeout
                    (fn []
                      (reset! !cleanup-fn
                        (install-pin-contextmenu! host get-topic-id get-pin-count on-pin-fn)))
                    0)
                  (e/on-unmount (fn [] (when-let [f @!cleanup-fn] (f)))))))))))))

