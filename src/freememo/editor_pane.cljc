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
   [clojure.string :as str]
   [freememo.commands :as commands]
   [freememo.merged-page-bridge :refer [MergedPageBridge]]
   [freememo.merged-window-navigation :refer [MergedWindowNavigation]]
   [freememo.merged-window :as merged-window]
   [freememo.editor-pin-layer :refer [EditorPinLayer]]
   [freememo.reading-anchor :as reading-anchor]
   [freememo.reading-position-view :refer [ReadingPosition]]
   [freememo.context-menu-surface :as menu-surface]
   #?(:clj [freememo.user-state :as us])
   #?(:clj [freememo.db :as db])
   #?(:clj [freememo.settings :as settings])
   #?(:clj [freememo.image-rehost :as image-rehost])
   #?(:clj [freememo.toasts :as toasts])
   #?(:clj [freememo.web-import :as web-import])))

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

(defn save-editor-html!*
  "Persist an editor body. TWO DESTINATIONS, one image-rehost step.

   Per-page mode writes the body to `topic-id`, as it always has.

   Merged mode splits it back across `pdf-root-id`'s page rows
   (plans/merged-page-reading.md item 3.2). The editor holds one page range
   there, so writing it to `topic-id` would put every page's text — boundary
   markers included — into one page row.

   `page-numbers` is the list the merge reported. The editor cannot supply it:
   a page whose marker the reader deleted has no marker left to report.

   Image rehosting runs first either way. `topics.content` has no length cap, so
   a `data:` URI is stored silently and unbounded, unlike the card path which
   rejects. A failed rehost keeps the base64 and still saves — losing the user's
   text to protect the column would be the worse trade.

   Body-level reader conditional, not whole-form: this symbol is named inside
   `(e/server (e/Offload #(…)))`, which Electric compiles on BOTH peers, so the
   var has to exist on CLJS."
  [{:keys [user-id topic-id html merged? pdf-root-id page-numbers]}]
  #?(:clj
     (try
       (when-not (db/owns-topic? user-id (if merged? pdf-root-id topic-id))
         (throw (ex-info "This document is not available." {})))
       (let [{rehosted :html} (image-rehost/rehost-data-uris! {:html html :user-id user-id})]
         (if merged?
           (let [r (db/save-merged-pages! pdf-root-id rehosted page-numbers)]
             {:success true :written (:written r)})
           (do (db/update-topic-content! topic-id rehosted)
               {:success true})))
       (catch Exception ex
         {:success false :error (.getMessage ex)}))
     :cljs nil))

(e/defn EditorPane
  []
  (e/client
    (let [user-id dctx/user-id topic-id dctx/topic-id audio-topic-id dctx/audio-topic-id
          root-topic-id dctx/root-topic-id
          is-pdf-page? dctx/is-pdf-page? static-content dctx/static-content
          effective-content-topic-id dctx/effective-content-topic-id
          on-imported-navigate! dctx/on-imported-navigate!
          merged? dctx/merged? pdf-root-id dctx/pdf-root-id
          merged-page-numbers dctx/merged-page-numbers
          ;; Whether a Quill instance is already up. The spinner gate below
          ;; reads it so a transiently nil body cannot destroy the editor.
          ;; A dedicated flag, not a read of !editor-state — see that var.
          editor-live? (e/watch editor/!editor-ready?)]
    (dom/div
      ;; `editor-pane` is what `.editor-hidden` hides (document-view/ContentColumn).
      ;; `editor-text` is the getting-started spotlight's anchor for Extract and
      ;; Generate, whose controls appear on a selection here.
      (dom/props {:class "editor-pane" :data-tour "editor-text"
                  :style {:flex "1" :display "flex" :flex-direction "column"
                          :min-width "0" :min-height "0" :overflow "hidden"}})

      ;; -----------------------------------------------------------------------
      ;; Auto-save + save-status: watch editor/!dirty-html and write on every
      ;; edit whose captured owner matches. save-result is computed once and reused
      ;; for last-saved tracking and the status indicator (PDF mode).
      ;;
      ;; The trigger MUST read dirty-data directly. needs-save? → save-result
      ;; keeps dirty-data on a demanded reactive path — it feeds the e/server
      ;; write and the status DOM, so Electric runs it. A prior debounce parked
      ;; the write behind a value-discarded let-binding that Electric never
      ;; evaluated, so no edit ever saved. !last-saved dedups repeat values, so
      ;; a matched edit writes once per edit identity.
      ;; -----------------------------------------------------------------------
      (let [dirty-data (e/watch editor/!dirty-html)
            !last-saved (atom nil)
            ;; The dirty tag is the DOCUMENT id, which merged reading makes
            ;; distinct from the page — see doc-context/editor-doc-id.
            doc-id (or dctx/editor-doc-id topic-id)
            needs-save? (merged-window/save-needed? dirty-data user-id
                          (e/watch editor/!saved-edit-id) (e/watch !last-saved))
            save-result (when needs-save?
                          (log/log-debug (str "EditorPane auto-save topic-id=" topic-id))
                          (e/server
                            (e/Offload
                              #(assoc (save-editor-html!*
                                        (merge (:save-scope dirty-data)
                                          {:user-id user-id :html (:html dirty-data)}))
                                 :saved-edit-id (:edit-id dirty-data)))))]
        (when (:success save-result)
          (reset! !last-saved (:saved-edit-id save-result))
          (reset! editor/!saved-edit-id (:saved-edit-id save-result)))
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

        ;; Merged reading's two-way link with the page state. Its own namespace
        ;; because inlining it here pushed this e/defn past the 64KB method cap.
        (MergedPageBridge)
        (MergedWindowNavigation)

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

        ;; The pin count the context menu's pin rows check (the K1 cap of 2).
        ;; Watch :pin-mutations so the count refreshes after the side panel or a
        ;; pin row adds or removes a pin. CRITICAL: pin-rev must be PASSED INTO
        ;; the server fn (not ignored via `_` binding), otherwise Electric
        ;; optimizes the subscription away — see
        ;; pin_side_panel.cljc/get-pins-for-topic* for the working precedent.
        (let [pin-rev (e/server (e/watch (us/get-atom user-id :pin-mutations)))
              pin-count (e/server (count-pins-for-topic* pin-rev topic-id))]

          ;; -----------------------------------------------------------------------
          ;; Editor area
          ;; -----------------------------------------------------------------------
          (dom/div
            (dom/props {:style {:flex "1" :min-height "0" :overflow "hidden"}})
            ;; `e/Reconcile` collapses this branch swap into a change. Without
            ;; it the swap emits shrink+grow, which ripples up the DAG and fires
            ;; `e/on-unmount` — and the unmount this branch reaches is
            ;; RichTextEditorComponent's, which calls `destroy-editor!`.
            (e/Reconcile
             (if (or (and (nil? static-content) (not editor-live?))
                  ;; Stale body from the previous topic: on a switch the content
                  ;; re-fetch is pending and its value latches (never nil), so the
                  ;; old text would otherwise sit under the new topic-id until the
                  ;; new body swaps in. The fetched-for id lags the live topic-id
                  ;; here → show the spinner instead of the stale text. No-op for
                  ;; real PDFs (id pinned to page-topic-id in DocumentViewState).
                  (not= effective-content-topic-id topic-id))
              ;; Loading — the FIRST fetch, or a topic switch mid-load.
              ;;
              ;; `editor-live?` is what keeps a nil content from tearing the
              ;; editor down. This branch unmounts RichTextEditorComponent, whose
              ;; e/on-unmount calls destroy-editor! — so a body that goes
              ;; transiently nil rebuilt the whole Quill DOM.
              ;;
              ;; A focus-reader toggle does exactly that: the mode's new body is
              ;; a different query, so `static-content` is nil for one tick while
              ;; it resolves. Measured before this guard: `.ql-editor` absent for
              ;; 75ms per toggle, a new Quill instance each time, and about 1400
              ;; nodes rebuilt under the editor host.
              ;;
              ;; A nil content with an editor already up means "the body for THIS
              ;; document is in flight". Holding the current body for that tick is
              ;; both cheaper and less jarring than a spinner. `set-content!`
              ;; swaps it in when it arrives.
              (loading/Spinner)
              ;; Render editor for any string (including "") — PDF pages without
              ;; scanned text yet are editable
              (dom/div
                (dom/props {:style {:height "100%" :display "flex" :flex-direction "column"
                                    :overflow "hidden"}
                            :data-role "editor-host"})
                (binding [dctx/initial-html static-content]
                  (RichTextEditorComponent))
                ;; Question pins on the text (plans/document-question-pins.md
                ;; §3.4). Beside the editor, never inside it.
                (EditorPinLayer)
                ;; Lend the topic to the context menu's image rows (pin, image
                ;; occlusion). The editor is plain JS and knows no topic; the
                ;; Quill installer reads these facts at right-click time
                ;; (plans/custom-context-menu.md 4.1.1). Re-run on every change,
                ;; so the pin cap check never reads a stale count.
                (menu-surface/provide-facts! dom/node
                  {:topic-id topic-id :root-topic-id root-topic-id :pin-count pin-count})))))

          ;; The reader's place in the notes (plans/reading-position-memory.md
          ;; §4 3.5.4). A nil topic mounts no tracker, so a PDF-backed or media
          ;; kind (§4 0.2) keeps no position and needs no conditional frame.
          ;; Renders nothing.
          (ReadingPosition "editor"
            (when (contains? reading-anchor/editor-kinds dctx/kind) topic-id))))))))

