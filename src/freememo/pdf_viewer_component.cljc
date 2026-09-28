(ns freememo.pdf-viewer-component
  "PDF viewer UI component using PDF.js."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.doc-context :as dctx]
   [freememo.logging :as log]
   [freememo.navigation :as nav]
   [freememo.pdf-viewer :as viewer]
   #?(:clj [freememo.settings :as settings])))

(e/defn PdfViewerComponent
  "Renders a PDF viewer for the given document ID and exposes current page number.
   Props: {:document-id <int>, :initial-page <int>, :on-navigate! <fn>,
           :on-total! <fn>, :target-page <int>,
           :is-live? <bool>, :has-file? <bool>, :reload-nonce <any>}
   The viewer chrome (page-nav, zoom, layout-toggle) lives in PdfToolbar, not
   here; this component renders only the scrollable PDF surface. `on-total!` is
   called with the page count whenever it's known/changes, so PdfToolbar can
   render \"of N\". For a Live Document with no blob yet (is-live? ∧ ¬has-file?)
   the viewer stays un-initialized (PDF.js never fetches a 404); the add-photos
   empty-state is rendered upstream by DocumentBody, which preempts this pane.
   `reload-nonce` (the page count) is folded into the pdf url as ?v=<nonce>, so
   when it changes (e.g. a Live Document append) a new PDF.js viewer loads the
   fresh blob — see the versioned-key reconciliation below.
   Each load opens at the document's stored fit (settings/get-pdf-fit), read
   again for every versioned key, so a Live Document append keeps a fit chosen
   since the document opened.
   Returns: The current page number (for OCR integration)."
  []
  (e/client
    ;; e/snapshot seeds the atoms ONCE at first mount. Without it, Electric
    ;; re-evaluates (atom …) on subsequent reactive cycles when callers
    ;; rebuild prop closures, recreating !page and silently throwing away
    ;; scroll-induced page changes (observed: scroll to p14 → atom reset → p15).
    (let [user-id dctx/user-id document-id dctx/document-id initial-page dctx/initial-page
          on-navigate! dctx/on-navigate! on-total! dctx/on-total!
          target-page dctx/target-page is-live? dctx/is-live?
          has-file? dctx/has-file? reload-nonce dctx/reload-nonce
          seed-page (e/snapshot (or initial-page 1))
          !page (atom seed-page)
          !total (atom 0)
          !container (atom nil)
          !viewer-div (atom nil)
          ;; Per-mount stable refs: !timer-id for unmount clearTimeout;
          ;; !requested-doc-id holds the versioned doc key (id + reload-nonce)
          ;; already loaded, so init runs exactly once per (doc, version).
          !timer-id (atom nil)
          !requested-doc-id (atom nil)
          show-empty? (and is-live? (not has-file?))
          page (e/watch !page)
          total (e/watch !total)]

      ;; External page-jump request (e.g. hierarchy click). The viewer's own
      ;; on-page-change callback (registered at init time) is the single
      ;; source of truth for !page + on-navigate! — calling them directly here
      ;; rebuilds the on-navigate closure identity, which re-fires the
      ;; setTimeout below and destroys/reinits the viewer mid-jump.
      (when (and target-page (pos? total) (not= target-page page))
        (viewer/go-to-page! target-page))

      (dom/div
        (dom/props {:style {:height "100%"
                            :display "flex"
                            :flex-direction "column"
                            :border "1px solid var(--color-border)"
                            :border-radius "4px"
                            :overflow "hidden"}})

        ;; Viewer chrome (page-nav, zoom, layout-toggle) now lives in
        ;; PdfToolbar; this component renders only the scrollable PDF surface.

        ;; Viewer wrapper (relative positioning for absolute container inside)
        (dom/div
          (dom/props {:style {:flex "1"
                              :position "relative"}})

          ;; Viewer container — mounted ONCE and never remounted per topic.
          ;; That fixes the :diff-corruption WS crash that the previous
          ;; per-document e/for-by remount caused. The PDF.js viewer inside it
          ;; is NOT kept: each (doc, version) builds a new one through
          ;; init-viewer!, never set-document! (see the keyed-fit comment below
          ;; and CLAUDE.md, "Every Document Change Builds a New PDF.js Viewer").
          (dom/div
            (dom/props {:class "pdf-viewer-container"
                        :style {:position "absolute"
                                :top "0"
                                :left "0"
                                :right "0"
                                :bottom "0"
                                :overflow "auto"
                                :background "var(--color-pdf-bg)"}})
            (reset! !container dom/node)
            (e/on-unmount
              (fn []
                (log/log-debug "PDF-COMP unmount")
                (when-let [t @!timer-id] (js/clearTimeout t))
                (viewer/destroy-viewer!)))

            (dom/div
              (dom/props {:class "pdfViewer"})
              (reset! !viewer-div dom/node)

              ;; The pdf url carries ?v=<reload-nonce> so a byte change (a
              ;; Live Document append bumps the page count) yields a NEW versioned
              ;; key: a distinct IndexedDB cache key, which forces a fresh fetch.
              ;; !requested-doc-id holds the versioned key, so init runs exactly
              ;; once per (doc, version). A reload-nonce change re-evaluates
              ;; this let. document-id /
              ;; reload-nonce / initial-page appear only here or inside the
              ;; on-ready closure (Electric treats fn bodies as opaque).
              (let [version  (or reload-nonce 0)
                    pdf-url  (str "/api/pdf/" document-id "?v=" version)
                    desired-key (str document-id "?v=" version)
                    ;; The stored fit, paired with the key it was read for.
                    ;; e/Offload keeps the PREVIOUS result while a new read is
                    ;; in flight, so on a swap or a version bump the old pair
                    ;; stays visible until the new one lands. Loading on the
                    ;; old pair would open the new document at the old
                    ;; document's fit.
                    ;;
                    ;; So `fit` is EMPTY for that window, never nil: the calls
                    ;; that take it wait, and nothing else moves. A `fit` in
                    ;; the `when` test below would remount its branch on every
                    ;; document change. The branch's `(nil? @viewer/!viewer-state)`
                    ;; test runs only when the branch mounts, because Electric
                    ;; does not re-run a deref of an unchanged atom. Mounted once
                    ;; with no viewer, it sends every later document through
                    ;; init-viewer!. Remounted, it finds the live viewer and
                    ;; calls set-document!, where PDF.js 3.11 throws: its
                    ;; AnnotationEditorUIManager.destroy calls destroy on the
                    ;; alt-text manager, which this viewer never gets.
                    keyed-fit (e/server
                                (e/Offload
                                  #(vector desired-key (settings/get-pdf-fit user-id document-id))))
                    fit (if (= desired-key (nth keyed-fit 0)) (nth keyed-fit 1) (e/amb))
                    on-ready (fn [^js pdf _]
                               (let [n (.-numPages pdf)]
                                 (reset! !total n)
                                 (when on-total! (on-total! n)))
                               ;; Single boundary gate for page-anchor persistence.
                               ;; `pagechanging` fires for the restore jump AND the
                               ;; post-fit reflow drift (restore to 20, reflow slides
                               ;; to 30) — neither is user navigation. While
                               ;; viewer/restoring?, drop the event so !page (hence
                               ;; current-page, the URL anchor, and last-page) keeps
                               ;; its seeded target instead of chasing the drift.
                               ;; Imperative read on purpose: an Electric e/watch of
                               ;; the flag does NOT observe this atom's mutation.
                               (viewer/on-page-change! (fn [page-num]
                                                         (when-not (viewer/restoring?)
                                                           (reset! !page page-num)
                                                           (when on-navigate! (on-navigate! page-num)))))
                               ;; A PDF-page extract's "Go to page" button primes
                               ;; nav/!pending-page-jump then navigates here; consume
                               ;; it post-load (go-to-page is reliable only after
                               ;; pagesloaded). Overrides the last-page resume.
                               (let [pending @nav/!pending-page-jump
                                     jump (when (and pending (= (:root pending) document-id))
                                            (:page pending))
                                     resume-page (or jump initial-page 1)]
                                 (when jump (reset! nav/!pending-page-jump nil))
                                 (when (> resume-page 1)
                                   (viewer/go-to-page-after-load! resume-page)))
                               (viewer/setup-pinch-zoom! @!container))]
                (when (and (not show-empty?) (not= desired-key @!requested-doc-id))
                  (reset! !requested-doc-id desired-key)
                  (if (nil? @viewer/!viewer-state)
                    ;; Always taken: this test runs once, when the branch mounts,
                    ;; and no viewer exists then (see keyed-fit above). Build a
                    ;; new viewer, deferred so DOM nodes exist.
                    (reset! !timer-id
                      (js/setTimeout
                        (fn []
                          (reset! !timer-id nil)
                          (viewer/init-viewer! @!container @!viewer-div pdf-url fit on-ready))
                        100))
                    ;; Not reached while this branch stays mounted. If a remount
                    ;; reaches it, PDF.js 3.11 throws (see keyed-fit above).
                    (viewer/set-document! pdf-url fit on-ready))))))))

      ;; Return current page number for OCR integration
      page)))
