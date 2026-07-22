(ns freememo.editor-pane
  "EditorPane sub-component for TopicPage.
   Renders the compact page-header (PDF mode) + auto-save + wiki import + rich-text editor area."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.doc-context :as dctx]
   [freememo.loading :as loading]
   [freememo.logging :as log]
   [freememo.rich-text-editor :as editor]
   [freememo.rich-text-editor-component :refer [RichTextEditorComponent]]
   [freememo.navigation :as nav]
   [freememo.occlusion-modal :refer [OcclusionModal]]
   [clojure.string :as str]
   [freememo.commands :as commands]
   #?(:cljs [freememo.editor-pin-menu :as pin-menu])
   #?(:clj [freememo.user-state :as us])
   #?(:clj [freememo.db :as db])
   #?(:clj [freememo.web-import :as web-import])))

;; ---------------------------------------------------------------------------
;; Pin contextmenu helper — wraps CLJS-only install-contextmenu! so the
;; reader conditional lives in a plain defn body, not in the Electric graph.
;; ---------------------------------------------------------------------------

(defn install-pin-contextmenu!
  "Attach pin contextmenu to container-el. Returns a cleanup fn.
   CLJS-only; returns nil on CLJ."
  [container-el get-topic-id get-pin-count on-pin! on-occlude!]
  #?(:cljs (pin-menu/install-contextmenu!
             container-el get-topic-id get-pin-count on-pin! on-occlude!)
     :clj nil))

(defn count-pins-for-topic*
  "Server-side pin count. Takes _pin-rev as first arg so the value is part of
   the call site — forces Electric to subscribe to the :pin-mutations channel
   that produced _pin-rev. Matches pin_side_panel/get-pins-for-topic*."
  [_pin-rev topic-id]
  #?(:clj (count (db/get-pins topic-id))
     :cljs 0))

;; OCR scan infrastructure (start-ocr-scan! + ocr-executor) moved to
;; freememo.pdf-toolbar, which now hosts the Scan Page button.

;; ---------------------------------------------------------------------------
;; EditorPane
;; ---------------------------------------------------------------------------

(e/defn EditorPane
  []
  (e/client
    (let [user-id dctx/user-id topic-id dctx/topic-id audio-topic-id dctx/audio-topic-id
          root-topic-id dctx/root-topic-id
          is-pdf-page? dctx/is-pdf-page? static-content dctx/static-content
          effective-content-topic-id dctx/effective-content-topic-id
          on-imported-navigate! dctx/on-imported-navigate!]
    (dom/div
      (dom/props {:style {:flex "1" :display "flex" :flex-direction "column"
                          :min-width "0" :min-height "0" :overflow "hidden"}})

      ;; -----------------------------------------------------------------------
      ;; Auto-save + save-status: watch editor/!dirty-html and write on every
      ;; edit whose topic-id matches. save-result is computed once and reused
      ;; for last-saved tracking and the status indicator (PDF mode).
      ;;
      ;; The trigger MUST read dirty-data directly. needs-save? → save-result
      ;; keeps dirty-data on a demanded reactive path — it feeds the e/server
      ;; write and the status DOM, so Electric runs it. A prior debounce parked
      ;; the write behind a value-discarded let-binding that Electric never
      ;; evaluated, so no edit ever saved. !last-saved dedups repeat values, so
      ;; a matched edit writes once per distinct html.
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
        ;; Save-status indicator (PDF mode). The page-header's other controls
        ;; (done-checkbox, Scan, Compare-OCR, Copy text/all) moved to PdfToolbar;
        ;; this lightweight save feedback stays with the editor.
        ;; -----------------------------------------------------------------------
        (when (and is-pdf-page? (some? save-result))
          (let [is-success (:success save-result)
                message (if is-success "Saved" (str "Save error: " (:error save-result)))
                !show (atom true)
                show (e/watch !show)]
            (dom/div
              (dom/props {:style {:display "flex" :justify-content "flex-end"
                                  :padding "var(--sp-1) var(--sp-2)" :flex-shrink "0"}})
              (dom/span
                (dom/props {:style {:font-size "12px"
                                    :color (if is-success "var(--color-text-secondary)" "var(--color-danger)")
                                    :opacity (if show "1" "0")
                                    :transition "opacity 0.5s ease-out"}})
                (dom/text message)
                (when is-success
                  (e/client
                    (js/setTimeout (fn [] (reset! !show false)) 2000)))))))

        ;; -----------------------------------------------------------------------
        ;; Pin-request atom — CLJS contextmenu callback fires into this atom;
        ;; e/Token picks it up and runs the server-side set-pin! call.
        ;; Shape: {:media-id <int> :placement "front"|"back" :topic-id <int>}
        ;; or nil (no pending request).
        ;; -----------------------------------------------------------------------
        (let [!pin-request (atom nil)
              pin-request (e/watch !pin-request)
              ;; Occlusion-request atom — the context menu's Image Occlusion
              ;; item fires into this; OcclusionModal (mounted below) opens
              ;; while it is non-nil. Shape: see freememo.occlusion-modal.
              !occlusion-request (atom nil)
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
                                  (log/audit! {:id ::add-pin :user-id user-id :action :create
                                               :entity :pin :entity-id media-id})
                                  {:success true}
                                  (catch clojure.lang.ExceptionInfo ex
                                    (let [data (ex-data ex)]
                                      {:success false :reason (:reason data)}))
                                  (catch Exception ex
                                    {:success false :error (.getMessage ex)}))))]
                (case result
                  (if (:success result)
                    (case (e/server (commands/bump! user-id :add-pin))
                      (t))
                    (do (log/log-debug (str "EditorPane set-pin! failed: " result))
                      (t)))))))

          ;; -----------------------------------------------------------------------
          ;; Editor area
          ;; -----------------------------------------------------------------------
          (dom/div
            (dom/props {:style {:flex "1" :min-height "0" :overflow "hidden"}})
            (if (or (nil? static-content)
                  ;; Stale body from the previous topic: on a switch the content
                  ;; re-fetch is pending and its value latches (never nil), so the
                  ;; old text would otherwise sit under the new topic-id until the
                  ;; new body swaps in. The fetched-for id lags the live topic-id
                  ;; here → show the spinner instead of the stale text. No-op for
                  ;; real PDFs (id pinned to page-topic-id in DocumentViewState).
                  (not= effective-content-topic-id topic-id))
              ;; Loading — first fetch in flight, or a topic switch mid-load.
              (loading/Spinner)
              ;; Render editor for any string (including "") — PDF pages without
              ;; scanned text yet are editable
              (dom/div
                (dom/props {:style {:height "100%" :display "flex" :flex-direction "column"
                                    :overflow "hidden"}
                            :data-role "editor-host"})
                (binding [dctx/initial-html static-content]
                  (RichTextEditorComponent))
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
                      on-pin-fn (fn [req] (reset! !pin-request req))
                      on-occlude-fn (fn [{:keys [media-id topic-id]}]
                                      (reset! !occlusion-request
                                        {:mode :create
                                         :image-media-id media-id
                                         :topic-id topic-id
                                         :root-topic-id root-topic-id}))]
                  (js/setTimeout
                    (fn []
                      (reset! !cleanup-fn
                        (install-pin-contextmenu! host get-topic-id get-pin-count
                          on-pin-fn on-occlude-fn)))
                    0)
                  (e/on-unmount (fn [] (when-let [f @!cleanup-fn] (f)))))))

            ;; Image-occlusion authoring modal (context-menu entry point).
            (OcclusionModal !occlusion-request user-id))))))))

