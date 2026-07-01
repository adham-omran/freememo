(ns freememo.pdf-viewer-component
  "PDF viewer UI component using PDF.js."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.doc-context :as dctx]
   [clojure.string]
   [freememo.logging :as log]
   [freememo.navigation :as nav]
   [freememo.pdf-viewer :as viewer]))

;; LiveDocAddPhotos — Upload / Take-photo buttons feeding a client-side staging
;; strip. Selected files are staged (HEIC/HEIF converted to JPEG via
;; /api/heic-preview so every browser can preview them), shown as thumbnails
;; the user can rotate (↻, clockwise) or remove (✕), then committed as a batch
;; to /api/append-images with a parallel rotation-angle array. On success the
;; server bumps :refresh, which re-derives the page count and reloads the
;; viewer. Nothing leaves the device until commit, and every phase is visible —
;; replacing the prior fire-and-forget upload with no progress indication.
;; `compact?` shrinks it for the toolbar.
;;
;; Thumbnails set `image-orientation: none` so the browser renders the same raw
;; pixels the server's ImageIO will — keeping preview == saved page. The ↻
;; control (CSS rotate, angle sent to the server) is the sole orientation
;; control; EXIF orientation is intentionally not auto-applied on either side.
(def ^:private heic-name-rx #"(?i)\.hei[cf]$")
(def ^:private heic-type-rx #"(?i)image/hei[cf]")

(e/defn LiveDocAddPhotos []
  (e/client
    (let [document-id dctx/document-id compact? dctx/compact?
          !upload-input (atom nil)
          !camera-input (atom nil)
          !busy         (atom false)         ; commit in flight
          !staged       (atom [])            ; [{:id :name :status :rotation :url :payload :error}]
          !counter      (atom 0)             ; monotonic id source
          !commit-error (atom nil)
          busy          (e/watch !busy)
          staged        (e/watch !staged)
          commit-error  (e/watch !commit-error)
          converting?   (boolean (some #(= :converting (:status %)) staged))
          set-entry!    (fn [id f] (swap! !staged (fn [v] (mapv #(if (= (:id %) id) (f %) %) v))))
          stage-files!  (fn [files]
                          (doseq [f files]
                            (let [id   (swap! !counter inc)
                                  nm   (.-name f)
                                  heic? (boolean (or (re-find heic-type-rx (or (.-type f) ""))
                                                     (re-find heic-name-rx (or nm ""))))]
                              (if heic?
                                (do
                                  (swap! !staged conj {:id id :name nm :status :converting :rotation 0})
                                  (let [fd (js/FormData.)]
                                    (.append fd "image" f)
                                    (-> (js/fetch "/api/heic-preview" (clj->js {:method "POST" :body fd}))
                                      (.then (fn [^js r] (if (.-ok r) (.blob r) (js/Promise.reject "convert"))))
                                      (.then (fn [blob]
                                               (let [url (js/URL.createObjectURL blob)
                                                     jpg (js/File. (clj->js [blob]) (str nm ".jpg") (clj->js {:type "image/jpeg"}))]
                                                 (set-entry! id #(assoc % :status :ready :url url :payload jpg)))))
                                      (.catch (fn [_] (set-entry! id #(assoc % :status :error
                                                                        :error "Couldn’t read this image")))))))
                                (let [url (js/URL.createObjectURL f)]
                                  (swap! !staged conj {:id id :name nm :status :ready
                                                       :rotation 0 :url url :payload f}))))))
          rotate!       (fn [id] (set-entry! id #(update % :rotation (fn [d] (mod (+ (or d 0) 90) 360)))))
          remove!       (fn [id] (when-some [e (first (filter #(= (:id %) id) @!staged))]
                                   (when (:url e) (js/URL.revokeObjectURL (:url e))))
                          (swap! !staged (fn [v] (vec (remove #(= (:id %) id) v)))))
          commit!       (fn []
                          (let [items @!staged]
                            (when (and (seq items) (not @!busy)
                                       (not-any? #(= :converting (:status %)) items))
                              (reset! !busy true)
                              (reset! !commit-error nil)
                              (let [fd (js/FormData.)]
                                (.append fd "doc_id" (str document-id))
                                (doseq [it items] (.append fd "images" (:payload it)))
                                (.append fd "rotations" (js/JSON.stringify (clj->js (mapv :rotation items))))
                                (-> (js/fetch "/api/append-images" (clj->js {:method "POST" :body fd}))
                                  (.then (fn [^js r] (.json r)))
                                  (.then (fn [^js d]
                                           (reset! !busy false)
                                           (if (.-success d)
                                             ;; Server bumped :refresh → the new page count returns as
                                             ;; reload-nonce, which versions the viewer's pdf url (a new
                                             ;; cache key), so the viewer swaps in the fresh blob in
                                             ;; place. No client-side eviction needed.
                                             (do (doseq [it items] (when (:url it) (js/URL.revokeObjectURL (:url it))))
                                                 (reset! !staged []))
                                             (reset! !commit-error (or (.-error d) "Upload failed")))))
                                  (.catch (fn [_]
                                            (reset! !busy false)
                                            (reset! !commit-error "Upload failed — please try again."))))))))
          btn-style     {:padding (if compact? "6px 10px" "10px 16px")
                         :cursor (if busy "wait" "pointer")
                         :background "var(--color-bg-card)"
                         :border "1px solid var(--color-border)"
                         :border-radius "3px" :font-size "14px"}]
      ;; Release any outstanding object URLs if this component is torn down
      ;; (e.g. navigating away) with images still staged.
      (e/on-unmount (fn [] (doseq [it @!staged] (when (:url it) (js/URL.revokeObjectURL (:url it))))))
      (dom/button
        (dom/props {:title "Upload images" :disabled busy :style btn-style})
        (dom/text (if compact? "＋ Photos" "Upload images"))
        (dom/On "click" (fn [_] (when-some [inp @!upload-input] (.click inp))) nil))
      (dom/button
        (dom/props {:title "Take a photo" :disabled busy
                    :style (assoc btn-style :margin-left "4px")})
        (dom/text (if compact? "📷" "Take photo"))
        (dom/On "click" (fn [_] (when-some [inp @!camera-input] (.click inp))) nil))
      ;; Commit button — appears once images are staged; blocked while any image
      ;; is still converting or a commit is in flight (visibly disabled, never a
      ;; silent no-op).
      (when (seq staged)
        (dom/button
          (dom/props {:title "Add staged images as pages"
                      :disabled (or busy converting?)
                      :style (assoc btn-style :margin-left "4px"
                               :background "var(--color-accent, #2d6cdf)" :color "white")})
          (dom/text (cond busy "Uploading…"
                          converting? "Preparing…"
                          :else (str (if compact? "✓ " "Add ") (count staged)
                                  (when-not compact? " page(s)"))))
          (dom/On "click" (fn [_] (commit!)) nil)))
      ;; Staging strip — one thumbnail per selected image.
      (when (seq staged)
        (dom/div
          (dom/props {:style {:display "flex" :flex-wrap "wrap" :gap "8px"
                              :margin-top "8px"}})
          (e/for [entry (e/diff-by :id staged)]
            (let [id (:id entry) st (:status entry)]
              (dom/div
                (dom/props {:style {:position "relative" :width "72px" :height "72px"
                                    :border "1px solid var(--color-border)" :border-radius "4px"
                                    :overflow "hidden" :display "flex"
                                    :align-items "center" :justify-content "center"
                                    :background "var(--color-bg-card)" :font-size "11px"}})
                (case st
                  :converting (dom/text "Preparing…")
                  :error (dom/div (dom/props {:style {:color "#c0392b" :padding "4px" :text-align "center"}})
                           (dom/text (or (:error entry) "Error")))
                  (dom/img
                    (dom/props {:src (:url entry)
                                :style {:width "100%" :height "100%" :object-fit "cover"
                                        :image-orientation "none"
                                        :transform (str "rotate(" (:rotation entry) "deg)")}})))
                ;; Rotate (only meaningful for a viewable image).
                (when (= st :ready)
                  (dom/button
                    (dom/props {:title "Rotate 90° clockwise"
                                :style {:position "absolute" :bottom "2px" :left "2px"
                                        :font-size "12px" :line-height "1" :padding "2px 4px"
                                        :border "none" :border-radius "3px" :cursor "pointer"
                                        :background "rgba(0,0,0,0.55)" :color "white"}})
                    (dom/text "↻")
                    (dom/On "click" (fn [_] (rotate! id)) nil)))
                ;; Remove.
                (dom/button
                  (dom/props {:title "Remove"
                              :style {:position "absolute" :top "2px" :right "2px"
                                      :font-size "12px" :line-height "1" :padding "2px 5px"
                                      :border "none" :border-radius "3px" :cursor "pointer"
                                      :background "rgba(0,0,0,0.55)" :color "white"}})
                  (dom/text "✕")
                  (dom/On "click" (fn [_] (remove! id)) nil)))))))
      ;; Inline commit error — replaces the prior js/alert.
      (when commit-error
        (dom/div
          (dom/props {:style {:margin-top "6px" :color "#c0392b" :font-size "12px"}})
          (dom/text commit-error)))
      ;; Hidden inputs: upload (multi) + camera (capture rear camera on mobile).
      (dom/input
        (dom/props {:type "file" :accept "image/*" :multiple true :style {:display "none"}})
        (reset! !upload-input dom/node)
        (dom/On "change"
          (fn [e] (stage-files! (array-seq (-> e .-target .-files)))
            (set! (-> e .-target .-value) "")) nil))
      (dom/input
        (dom/props {:type "file" :accept "image/*" :capture "environment" :style {:display "none"}})
        (reset! !camera-input dom/node)
        (dom/On "change"
          (fn [e] (stage-files! (array-seq (-> e .-target .-files)))
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
                (binding [dctx/compact? false] (LiveDocAddPhotos)))))

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
