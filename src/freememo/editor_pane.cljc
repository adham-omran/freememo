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
   [freememo.navigation :as nav]
   [clojure.string :as str]
   #?(:cljs [freememo.editor-pin-menu :as pin-menu])
   #?(:clj [freememo.page-ocr :as page])
   #?(:clj [freememo.user-state :as us])
   #?(:clj [freememo.db :as db])
   #?(:clj [freememo.wikipedia :as wiki])
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
                    (swap! (us/get-atom uid :ocr-errors) assoc [doc page] (:error result))))
                (swap! (us/get-atom uid :scanning-pages) disj [doc page]))
              (fn [e]
                (swap! (us/get-atom uid :scan-cancellers) dissoc [doc page])
                (when-not (instance? Cancelled e)
                  (log/log-info (str "OCR scan error topic-id=" doc " page=" page " ex=" (.getMessage e)))
                  (swap! (us/get-atom uid :ocr-errors) assoc [doc page] (.getMessage e)))
                (swap! (us/get-atom uid :scanning-pages) disj [doc page])))]
         (swap! (us/get-atom uid :scan-cancellers) assoc [doc page] cancel-fn)))
     :cljs nil))

;; ---------------------------------------------------------------------------
;; Wikipedia import helper (moved from extract_page.cljc)
;; ---------------------------------------------------------------------------

(defn import-wikipedia-url*
  "Import a Wikipedia article by URL.
   Returns {:already-exists true :title ... :topic-id ...}
          {:imported true :title ... :topic-id ...}
       or {:error ...}."
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

;; ---------------------------------------------------------------------------
;; EditorPane
;; ---------------------------------------------------------------------------

(e/defn EditorPane
  [{:keys [user-id enc-key topic-id is-pdf-page? root-topic-id page-number
           scan-dpi llm-enabled? enable-ai? enable-pdfbox? enable-pdfjs?
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
              [?token _] (e/Token import-data)]
          (when-some [token ?token]
            (let [url (:url import-data)
                  result (e/server
                           (e/Offload
                             #(import-wikipedia-url* user-id url)))]
              (when (some? result)
                (let [status (cond
                               (:imported result) :done
                               (:already-exists result) :already-exists
                               :else :error)]
                  (reset! editor/!import-status status)
                  (token)
                  (when (= status :done)
                    (e/server (swap! (us/get-atom user-id :refresh) inc))
                    (e/server (swap! (us/get-atom user-id :tree-mutations) inc)))
                  (when (and (:topic-id result) on-imported-navigate!)
                    (on-imported-navigate! (:topic-id result))))))))

        ;; -----------------------------------------------------------------------
        ;; Page header (PDF mode only)
        ;; -----------------------------------------------------------------------
        (when is-pdf-page?
          (dom/div
            (dom/props {:style {:display "flex" :align-items "center"
                                :gap "var(--sp-2)" :padding "var(--sp-1) var(--sp-2)"
                                :flex-shrink "0"}})

            ;; p.<page-number> indicator
            (dom/span
              (dom/props {:style {:font-weight "600" :font-size "13px"
                                  :color "var(--color-text-primary)"}})
              (dom/text "p." page-number))

            ;; Done checkbox
            (dom/label
              (dom/props {:style {:display "flex" :align-items "center"
                                  :gap "3px" :font-size "12px" :cursor "pointer"}
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
                        [?token _] (e/Token change-event)]
                    (when-some [token ?token]
                      (e/server (db/toggle-page-done! root-topic-id (:page change-event)))
                      (e/server (swap! (us/get-atom user-id :meta-refresh) inc))
                      (token)))))
              (dom/text "Done"))

            ;; Scan Page button (AI OCR)
            (when (and llm-enabled? enable-ai?)
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
                        [?token _] (e/Token click-event)]
                    (when-some [token ?token]
                      (e/server
                        (let [pg (:page click-event)
                              doc root-topic-id
                              uid user-id
                              ek enc-key]
                          (if (contains? @(us/get-atom uid :scanning-pages) [doc pg])
                            (log/log-info (str "OCR scan already in progress topic-id=" doc " page=" pg))
                            (do
                              (start-ocr-scan! uid doc pg ek scan-dpi)
                              :started))))
                      (token))))))

            ;; Extract (PDFBox) button
            (when enable-pdfbox?
              (let [scanning? (contains? scanning-pages [root-topic-id page-number])
                    disabled? scanning?]
                (dom/button
                  (dom/props {:class "btn btn-sm"
                              :style {:padding "4px 12px" :font-size "14px"
                                      :background (if disabled? "var(--color-border)" "var(--color-bg-card)")
                                      :border "1px solid var(--color-border)"
                                      :color "var(--color-text-primary)"
                                      :cursor (if disabled? "not-allowed" "pointer")}
                              :disabled disabled?
                              :data-tooltip "Extract text directly from the PDF (no AI)"})
                  (dom/text (if disabled? "Extracting..." "Extract (PDFBox)"))
                  (let [click-event (dom/On "click"
                                      (fn [_] {:id (str (random-uuid)) :page page-number})
                                      nil)
                        [?token _] (e/Token click-event)]
                    (when-some [token ?token]
                      (let [pg (:page click-event)
                            result (e/server
                                     (let [doc root-topic-id
                                           uid user-id
                                           p pg]
                                       (e/Offload
                                         #(do
                                            (swap! (us/get-atom uid :scanning-pages) conj [doc p])
                                            (swap! (us/get-atom uid :ocr-errors) dissoc [doc p])
                                            (try
                                              (let [r (page/extract-page-text-pdfbox doc p)]
                                                (if (:success r)
                                                  (swap! (us/get-atom uid :refresh) inc)
                                                  (swap! (us/get-atom uid :ocr-errors) assoc [doc p] (:error r)))
                                                r)
                                              (finally
                                                (swap! (us/get-atom uid :scanning-pages) disj [doc p])))))))]
                        (when result
                          (token))))))))

            ;; Extract (PDF.js) button
            (when enable-pdfjs?
              (let [scanning? (contains? scanning-pages [root-topic-id page-number])
                    disabled? scanning?
                    !pdfjs-result (atom nil)
                    pdfjs-result (e/watch !pdfjs-result)]
                (dom/button
                  (dom/props {:class "btn btn-sm"
                              :style {:padding "4px 12px" :font-size "14px"
                                      :background (if disabled? "var(--color-border)" "var(--color-bg-card)")
                                      :border "1px solid var(--color-border)"
                                      :color "var(--color-text-primary)"
                                      :cursor (if disabled? "not-allowed" "pointer")}
                              :disabled disabled?
                              :data-tooltip "Extract text using the PDF.js text layer (no AI)"})
                  (dom/text (if disabled? "Extracting..." "Extract (PDF.js)"))
                  (dom/On "click"
                    (fn [_]
                      #?(:cljs
                         (let [pg page-number
                               doc root-topic-id]
                           (-> (freememo.pdf-viewer/get-page-text-content! pg)
                             (.then (fn [text]
                                      (reset! !pdfjs-result {:id (str (random-uuid))
                                                             :page pg
                                                             :doc doc
                                                             :text text})))
                             (.catch (fn [err]
                                       (reset! !pdfjs-result {:id (str (random-uuid))
                                                              :page pg
                                                              :doc doc
                                                              :error (str err)})))))
                         :clj nil))
                    nil))

                ;; Persist PDF.js extraction result server-side
                (let [[?pdfjs-token _] (e/Token pdfjs-result)]
                  (when-some [token ?pdfjs-token]
                    (let [{:keys [page doc text error]} pdfjs-result
                          result (e/server
                                   (e/Offload
                                     #(do
                                        (swap! (us/get-atom user-id :scanning-pages) conj [doc page])
                                        (swap! (us/get-atom user-id :ocr-errors) dissoc [doc page])
                                        (try
                                          (cond
                                            error (do
                                                    (swap! (us/get-atom user-id :ocr-errors) assoc [doc page] error)
                                                    {:success false :error error})
                                            :else (let [r (page/save-pdfjs-text! doc page text)]
                                                    (if (:success r)
                                                      (swap! (us/get-atom user-id :refresh) inc)
                                                      (swap! (us/get-atom user-id :ocr-errors) assoc [doc page] (:error r)))
                                                    r))
                                          (finally
                                            (swap! (us/get-atom user-id :scanning-pages) disj [doc page]))))))]
                      (when result
                        (token)
                        (reset! !pdfjs-result nil)))))))

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
          (let [[?pin-token _] (e/Token pin-request)]
            (when-some [token ?pin-token]
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
                (when (some? result)
                  (token)
                  (reset! !pin-request nil)
                  (when (:success result)
                    (e/server (swap! (us/get-atom user-id :pin-mutations) inc)))
                  (when-not (:success result)
                    (log/log-debug (str "EditorPane set-pin! failed: " result)))))))

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

