(ns freememo.pdf-viewer-component
  "PDF viewer UI component using PDF.js."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.doc-context :as dctx]
   [freememo.logging :as log]
   [freememo.navigation :as nav]
   [freememo.pdf-viewer :as viewer]))

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
   when it changes (e.g. a Live Document append) the viewer fetches and swaps the
   fresh blob in place — see the versioned-key reconciliation below.
   Returns: The current page number (for OCR integration)."
  []
  (e/client
    ;; e/snapshot seeds the atoms ONCE at first mount. Without it, Electric
    ;; re-evaluates (atom …) on subsequent reactive cycles when callers
    ;; rebuild prop closures, recreating !page and silently throwing away
    ;; scroll-induced page changes (observed: scroll to p14 → atom reset → p15).
    (let [document-id dctx/document-id initial-page dctx/initial-page
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
          ;; already loaded, so init/swap runs exactly once per (doc, version).
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

          ;; Viewer container — created ONCE (init-viewer!) and the document is
          ;; swapped IN-PLACE (set-document!) when document-id changes; never
          ;; remounted per topic. Mirrors the persistent Quill editor
          ;; (rich_text_editor_component) and fixes the :diff-corruption WS crash
          ;; that the previous per-document e/for-by remount caused.
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
              ;; key: a distinct IndexedDB cache key (forces a fresh fetch) and a
              ;; new !loaded-doc-id (so set-document!'s same-key no-op guard fires
              ;; and swaps the fresh blob in place). !requested-doc-id holds the
              ;; versioned key, so init/swap runs exactly once per (doc, version);
              ;; reload-nonce changing re-evaluates this let. document-id /
              ;; reload-nonce / initial-page appear only here or inside the
              ;; on-ready closure (Electric treats fn bodies as opaque).
              (let [version  (or reload-nonce 0)
                    pdf-url  (str "/api/pdf/" document-id "?v=" version)
                    desired-key (str document-id "?v=" version)
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
                    ;; No viewer yet → create once, deferred so DOM nodes exist.
                    (reset! !timer-id
                      (js/setTimeout
                        (fn []
                          (reset! !timer-id nil)
                          (viewer/init-viewer! @!container @!viewer-div pdf-url on-ready))
                        100))
                    ;; Viewer exists, different doc OR new version → swap in place.
                    (viewer/set-document! pdf-url on-ready))))))))

      ;; Return current page number for OCR integration
      page)))
