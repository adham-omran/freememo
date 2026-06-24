(ns freememo.pdf-viewer-component
  "PDF viewer UI component using PDF.js."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [clojure.string]
   [freememo.logging :as log]
   [freememo.navigation :as nav]
   [freememo.pdf-viewer :as viewer]))

;; LiveDocAddPhotos — Upload / Take-photo buttons backed by two hidden file
;; inputs. A selected batch POSTs to /api/append-images; on success the server
;; bumps :refresh, which re-derives the page count and reloads the viewer (no
;; explicit reload call needed here). `compact?` shrinks it for the toolbar.
(e/defn LiveDocAddPhotos [{:keys [document-id compact?]}]
  (e/client
    (let [!upload-input (atom nil)
          !camera-input (atom nil)
          !busy (atom false)
          busy (e/watch !busy)
          send! (fn [files]
                  (when (and (seq files) (not @!busy))
                    (reset! !busy true)
                    (let [fd (js/FormData.)]
                      (.append fd "doc_id" (str document-id))
                      (doseq [f files] (.append fd "images" f))
                      (-> (js/fetch "/api/append-images" (clj->js {:method "POST" :body fd}))
                        (.then (fn [r] (.json r)))
                        (.then (fn [^js d]
                                 (reset! !busy false)
                                 (when-not (.-success d)
                                   (js/alert (or (.-error d) "Append failed")))))
                        (.catch (fn [e]
                                  (reset! !busy false)
                                  (js/console.error "Append failed:" e)
                                  (js/alert "Append failed — please try again.")))))))
          btn-style {:padding (if compact? "6px 10px" "10px 16px")
                     :cursor (if busy "wait" "pointer")
                     :background "var(--color-bg-card)"
                     :border "1px solid var(--color-border)"
                     :border-radius "3px" :font-size "14px"}]
      (dom/button
        (dom/props {:title "Upload images" :disabled busy :style btn-style})
        (dom/text (if compact? "＋ Photos" (if busy "Adding…" "Upload images")))
        (dom/On "click" (fn [_] (when-some [inp @!upload-input] (.click inp))) nil))
      (dom/button
        (dom/props {:title "Take a photo" :disabled busy
                    :style (assoc btn-style :margin-left "4px")})
        (dom/text (if compact? "📷" "Take photo"))
        (dom/On "click" (fn [_] (when-some [inp @!camera-input] (.click inp))) nil))
      ;; Hidden inputs: upload (multi) + camera (capture rear camera on mobile).
      (dom/input
        (dom/props {:type "file" :accept "image/*" :multiple true :style {:display "none"}})
        (reset! !upload-input dom/node)
        (dom/On "change"
          (fn [e] (send! (array-seq (-> e .-target .-files)))
            (set! (-> e .-target .-value) "")) nil))
      (dom/input
        (dom/props {:type "file" :accept "image/*" :capture "environment" :style {:display "none"}})
        (reset! !camera-input dom/node)
        (dom/On "change"
          (fn [e] (send! (array-seq (-> e .-target .-files)))
            (set! (-> e .-target .-value) "")) nil)))))

(e/defn PdfViewerComponent
  "Renders a PDF viewer for the given document ID and exposes current page number.
   Props: {:document-id <int>, :initial-page <int>, :on-navigate! <fn>,
           :on-total! <fn>, :target-page <int>,
           :is-live? <bool>, :has-file? <bool>, :reload-nonce <any>}
   The viewer chrome (page-nav, zoom, layout-toggle) lives in PdfToolbar, not
   here; this component renders only the scrollable PDF surface. `on-total!` is
   called with the page count whenever it's known/changes, so PdfToolbar can
   render \"of N\". For a Live Document with no blob yet (is-live? ∧ ¬has-file?)
   it shows an add-photos empty-state instead of initializing PDF.js.
   `reload-nonce` (the page count) changing forces a reload of the same document.
   Returns: The current page number (for OCR integration)."
  [{:keys [document-id initial-page on-navigate! on-total!
           target-page
           is-live? has-file? reload-nonce]}]
  (e/client
    ;; e/snapshot seeds the atoms ONCE at first mount. Without it, Electric
    ;; re-evaluates (atom …) on subsequent reactive cycles when callers
    ;; rebuild prop closures, recreating !page and silently throwing away
    ;; scroll-induced page changes (observed: scroll to p14 → atom reset → p15).
    (let [seed-page (e/snapshot (or initial-page 1))
          !page (atom seed-page)
          !total (atom 0)
          !container (atom nil)
          !viewer-div (atom nil)
          ;; Per-mount stable refs: !timer-id for unmount clearTimeout;
          ;; !requested-doc-id dedupes init/swap to fire once per document-id.
          !timer-id (atom nil)
          !requested-doc-id (atom nil)
          ;; Tracks the reload-nonce already applied to the mounted doc, so a
          ;; nonce bump (new live-doc pages) reloads exactly once.
          !requested-reload (atom nil)
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

      ;; Live-doc reload: after the viewer is initialized for this doc, a
      ;; reload-nonce (page count) change means pages were appended — force a
      ;; re-fetch. Guarded on init-done so it never races the initial load.
      (when (and is-live? has-file?
                 (= document-id @!requested-doc-id)
                 (some? @viewer/!viewer-state)
                 (not= reload-nonce @!requested-reload))
        (reset! !requested-reload reload-nonce)
        (viewer/reload-document! (str "/api/pdf/" document-id)
          (fn [^js pdf _]
            (let [n (.-numPages pdf)]
              (reset! !total n)
              (when on-total! (on-total! n))))))

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

          ;; Blob-less Live Document: invite the first batch. Rendered as an
          ;; overlay; the viewer container below stays un-initialized (the init
          ;; block is gated on (not show-empty?)) so PDF.js never fetches a 404.
          (when show-empty?
            (dom/div
              (dom/props {:style {:position "absolute" :inset "0" :z-index "1"
                                  :display "flex" :flex-direction "column"
                                  :align-items "center" :justify-content "center"
                                  :gap "16px" :padding "24px" :text-align "center"
                                  :background "var(--color-pdf-bg)"
                                  :color "var(--color-text-secondary)"}})
              (dom/div (dom/props {:style {:font-size "15px" :font-weight "600"
                                           :color "var(--color-text-primary)"}})
                (dom/text "Empty Live Document"))
              (dom/div (dom/props {:style {:font-size "13px" :max-width "320px"}})
                (dom/text "Take photos or upload images of your material — each becomes a page you can keep adding to."))
              (dom/div (dom/props {:style {:display "flex" :gap "8px"}})
                (LiveDocAddPhotos {:document-id document-id :compact? false}))))

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

              ;; document-id and initial-page appear ONLY inside fn closures
              ;; (Electric treats fn bodies as opaque). !requested-doc-id dedupes
              ;; so this fires exactly once per document-id change.
              (let [pdf-url (str "/api/pdf/" document-id)
                    on-ready (fn [^js pdf _]
                               (let [n (.-numPages pdf)]
                                 (reset! !total n)
                                 (when on-total! (on-total! n)))
                               (viewer/on-page-change! (fn [page-num]
                                                         (reset! !page page-num)
                                                         (when on-navigate! (on-navigate! page-num))))
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
                (when (and (not show-empty?) (not= document-id @!requested-doc-id))
                  (reset! !requested-doc-id document-id)
                  ;; Mark this nonce applied so the reload effect doesn't fire
                  ;; redundantly right after the initial load.
                  (reset! !requested-reload reload-nonce)
                  (if (nil? @viewer/!viewer-state)
                    ;; No viewer yet → create once, deferred so DOM nodes exist.
                    (reset! !timer-id
                      (js/setTimeout
                        (fn []
                          (reset! !timer-id nil)
                          (viewer/init-viewer! @!container @!viewer-div pdf-url on-ready))
                        100))
                    ;; Viewer exists, different doc → swap in place (no remount).
                    (viewer/set-document! pdf-url on-ready))))))))

      ;; Return current page number for OCR integration
      page)))
