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
   #?(:cljs [freememo.editor-image-menu :as image-menu])
   #?(:clj [freememo.user-state :as us])
   #?(:clj [freememo.db :as db])
   #?(:clj [freememo.settings :as settings])
   #?(:clj [freememo.image-rehost :as image-rehost])
   #?(:clj [freememo.toasts :as toasts])
   #?(:clj [freememo.web-import :as web-import])))

;; ---------------------------------------------------------------------------
;; Image contextmenu helper — wraps CLJS-only install-contextmenu! so the
;; reader conditional lives in a plain defn body, not in the Electric graph.
;; ---------------------------------------------------------------------------

;; ---------------------------------------------------------------------------
;; The editor's link channel — server half
;; ---------------------------------------------------------------------------

(defn document-base-url*
  "The address `topic-id` was imported from, for resolving its relative hrefs.

   Pre : `topic-id` may be nil.
   Post: an absolute http(s) URL, or nil when the topic has no effective
         source — which is the honest answer for 9680 SuperMemo topics and
         every hand-written one, and is what makes a relative link :no-base.
   Inv : resolves through the ancestor walk, so a page inherits its build's
         address. That address is the BUILD's for a Sphinx import, which is
         why those documents keep their links in-app instead of relative."
  [topic-id]
  #?(:clj (some-> (db/resolve-effective-source-id topic-id)
            db/get-source
            :sources/url
            not-empty)
     :cljs nil))

(defn link-import-navigates?*
  "Whether Import should follow the page it just found.

   Pre : `_refresh` creates the reactive dependency; `user-id` is logged in.
   Post: true unless the user turned it off, defaulting to the behaviour that
         existed before the setting.
   Inv : the key lives in `freememo.settings` and is read through it, so the
         Settings checkbox and this click-path read cannot name different
         strings.
   Inv : only a DUPLICATE can navigate immediately. A fresh import has nowhere
         to go — its topic does not exist until the fetch lands — so this
         governs the duplicate case and the completion toast governs the rest."
  [_refresh user-id]
  #?(:clj (settings/get-link-import-navigates user-id)
     :cljs true))

(defn announce-duplicate!
  "Tell the user what Import found, when it found it at once.

   Pre : `result` is `start-url-import!*`'s return.
   Post: :announced. A duplicate toasts with a button that opens the topic; a
         queued fetch says so; anything else is an error the user can see.
   Inv : always returns a value, so a `case` in the caller can sequence on it."
  [user-id result]
  #?(:clj
     (do (case (:flow result)
           :already-exists
           (toasts/push! user-id
             {:level :info
              :message (str "Already imported — \"" (:title result) "\"")
              :actions [{:label "Open"
                         :nav [:viewer {:type :topic :topic-id (:topic-id result)}]}]})

           (:queued :busy)
           (toasts/push! user-id
             {:level :info :message "Importing in the background…"})

           (toasts/push! user-id
             {:level :error :message "Could not start the import."}))
         :announced)
     :cljs :announced))

(defn push-link-toast!
  "Surface what a click could only explain — a missing heading, or a relative
   href with no address to resolve against.

   Pre : `data` is `editor/!link-toast`'s value.
   Post: :announced."
  [user-id data]
  #?(:clj (do (toasts/push! user-id {:level (or (:level data) :info)
                                     :message (:message data)})
              :announced)
     :cljs :announced))

(defn install-image-contextmenu!
  "Attach the image contextmenu to container-el. Returns a cleanup fn.
   CLJS-only; returns nil on CLJ."
  [container-el get-topic-id get-pin-count on-pin! on-occlude! on-error!]
  #?(:cljs (image-menu/install-contextmenu!
             container-el get-topic-id get-pin-count on-pin! on-occlude! on-error!)
     :clj nil))

(defn push-image-menu-toast!
  "Surface an image-menu failure to the user. Server-only; the menu itself
   runs outside Electric's frame and cannot reach `toasts/push!`.
   Pre : `message` is a user-presentable string. Post: returns the toast id."
  [user-id message]
  #?(:clj (toasts/push! user-id {:level :error :message message})
     :cljs nil))

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
                                 ;; Re-host pasted images before persisting.
                                 ;; topics.content has no length cap, so a data:
                                 ;; URI here is stored silently and unbounded —
                                 ;; unlike the card path, which rejects. A
                                 ;; failed re-host keeps the base64 and still
                                 ;; saves: losing the user's text to protect the
                                 ;; column would be the worse trade.
                                 (let [{:keys [html]}
                                       (image-rehost/rehost-data-uris!
                                         {:html (:html dirty-data) :user-id user-id})]
                                   (db/update-topic-content! topic-id html))
                                 {:success true}
                                 (catch Exception ex
                                   {:success false :error (.getMessage ex)})))))]
        (when (:success save-result)
          (reset! !last-saved (:html dirty-data)))
        (when (and (some? save-result) (not (:success save-result)))
          (log/log-error (str "EditorPane auto-save error topic-id=" topic-id
                           " err=" (:error save-result))))

        ;; -----------------------------------------------------------------------
        ;; The document's own address, for resolving relative hrefs
        ;; -----------------------------------------------------------------------
        ;; nil when the topic has no effective source, which is what makes a
        ;; relative link :no-base rather than a wrong guess. 9680 SuperMemo
        ;; topics are in that state.
        (let [base-url (e/server (document-base-url* topic-id))]
          (e/client (reset! editor/!document-base-url base-url)))

        ;; -----------------------------------------------------------------------
        ;; Link import
        ;; -----------------------------------------------------------------------
        ;; `start-url-import!*` answers a duplicate synchronously and hands the
        ;; fetch off to a thread that publishes its Activity membership, so a
        ;; reader never waits on a network round trip inside their editor. The
        ;; import's own outcome arrives later as a toast; only the immediate
        ;; answer is handled here.
        (let [import-data (e/watch editor/!import-url)
              navigate-on-import? (e/server
                                    (let [rev (e/watch (us/get-atom user-id :settings-refresh))]
                                      (link-import-navigates?* rev user-id)))
              [t _] (e/Token import-data)]
          (when t
            (e/on-unmount #(reset! editor/!import-url nil))
            (let [url (:url import-data)
                  result (e/server (e/Offload #(web-import/start-url-import!* user-id url)))]
              (case result
                (let [dupe? (= :already-exists (:flow result))]
                  (reset! editor/!import-status
                    (case (:flow result)
                      :already-exists :already-exists
                      :queued :queued
                      :busy :queued
                      :error))
                  (if (and dupe? (:topic-id result) navigate-on-import?)
                    ;; A duplicate is the one case with somewhere to go now.
                    (case (e/server (announce-duplicate! user-id result))
                      (case (on-imported-navigate! (:topic-id result)) (t)))
                    (case (e/server (announce-duplicate! user-id result)) (t))))))))

        ;; -----------------------------------------------------------------------
        ;; In-document topic link
        ;; -----------------------------------------------------------------------
        ;; An imported documentation tree rewrites its cross-references to
        ;; /viewer/topic/<id>, and a click on one arrives here through the same
        ;; navigation bridge the wiki import uses. The href cannot do this on its
        ;; own — Chrome does not follow anchor clicks inside a contenteditable —
        ;; so the editor's click listener publishes the id and this routes it.
        (let [nav-data (e/watch editor/!navigate-topic)
              [t _] (e/Token nav-data)]
          (when t
            (e/on-unmount #(reset! editor/!navigate-topic nil))
            ;; The anchor outlives the navigation: the target document has not
            ;; loaded yet, so the scroll cannot happen here. init-editor! reads
            ;; this once its content is in the DOM.
            (reset! editor/!pending-anchor (:fragment nav-data))
            (if (and (:topic-id nav-data) on-imported-navigate!)
              (case (on-imported-navigate! (:topic-id nav-data)) (t))
              (t))))

        ;; -----------------------------------------------------------------------
        ;; A click the editor could only explain
        ;; -----------------------------------------------------------------------
        ;; A fragment with no matching heading, or a relative href in a document
        ;; with no source address. The DOM listener cannot reach the toast queue,
        ;; which lives per-user on the server.
        (let [toast-data (e/watch editor/!link-toast)
              [t _] (e/Token toast-data)]
          (when t
            (e/on-unmount #(reset! editor/!link-toast nil))
            (case (e/server (push-link-toast! user-id toast-data)) (t))))

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
              ;; Menu-error atom — the context menu's copy/save actions run in
              ;; raw promise callbacks, outside Electric's frame, so they can't
              ;; call e/server to push a toast. Same client→server bridge as
              ;; !pin-request. Carries an :id because two consecutive identical
              ;; messages must still be two distinct e/Token emissions.
              ;; Shape: {:id uuid :message "..."} or nil.
              !menu-error (atom nil)
              menu-error (e/watch !menu-error)
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

          ;; e/Token: surface one image-menu failure at a time as a toast.
          (let [[et _] (e/Token menu-error)]
            (when et
              (e/on-unmount #(reset! !menu-error nil))
              (case (e/server (push-image-menu-toast! user-id (:message menu-error)))
                (et))))

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
                                         :root-topic-id root-topic-id}))
                      on-error-fn (fn [message]
                                    (reset! !menu-error {:id (random-uuid)
                                                         :message message}))]
                  (js/setTimeout
                    (fn []
                      (reset! !cleanup-fn
                        (install-image-contextmenu! host get-topic-id get-pin-count
                          on-pin-fn on-occlude-fn on-error-fn)))
                    0)
                  (e/on-unmount (fn [] (when-let [f @!cleanup-fn] (f)))))))

            ;; Image-occlusion authoring modal (context-menu entry point).
            (OcclusionModal !occlusion-request user-id))))))))

