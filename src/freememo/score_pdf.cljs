(ns freememo.score-pdf
  "CLJS-only pdf.js page snapshots + rect crops for Score cards.

   Renders pages of the ALREADY-LOADED document (pdf-viewer's !pdf-doc — the
   same PDFDocumentProxy the viewer displays) to offscreen canvases at a fixed
   snapshot scale. Rect geometry is stored in this snapshot-pixel space, so
   drawing (score-rect-editor) and cropping here share one coordinate system —
   no translation anywhere.

   User-accepted premise: pdf.js render quality at snapshot scale is
   sufficient for the cropped card images."
  (:require [freememo.pdf-viewer :as viewer]))

(def snapshot-scale
  "Page raster scale for rect drawing and crops (2.0 ≈ 144 DPI)."
  2.0)

(defn page-count []
  (when-let [^js doc @viewer/!pdf-doc]
    (.-numPages doc)))

(defn render-page
  "Render 1-based `page-num` of the loaded PDF to an offscreen canvas at
   snapshot-scale. Post: Promise<canvas>; rejects when no PDF is loaded."
  [page-num]
  (if-let [^js doc @viewer/!pdf-doc]
    (-> (.getPage doc page-num)
      (.then (fn [^js page]
               (let [^js vp (.getViewport page #js {:scale snapshot-scale})
                     canvas (js/document.createElement "canvas")
                     ctx (.getContext canvas "2d")]
                 (set! (.-width canvas) (js/Math.floor (.-width vp)))
                 (set! (.-height canvas) (js/Math.floor (.-height vp)))
                 (-> (.render page #js {:canvasContext ctx :viewport vp})
                   (.-promise)
                   (.then (fn [] canvas)))))))
    (js/Promise.reject (js/Error. "No PDF loaded"))))

(defn crop-blob
  "Crop rect {:x :y :w :h} (snapshot px) out of `canvas`.
   Post: Promise<Blob image/png>."
  [^js canvas {:keys [x y w h]}]
  (js/Promise.
    (fn [resolve reject]
      (let [out (js/document.createElement "canvas")
            ctx (.getContext out "2d")
            ow (js/Math.max 1 (js/Math.round w))
            oh (js/Math.max 1 (js/Math.round h))]
        (set! (.-width out) ow)
        (set! (.-height out) oh)
        (.drawImage ctx canvas x y w h 0 0 ow oh)
        (.toBlob out
          (fn [b] (if b (resolve b) (reject (js/Error. "Canvas export failed"))))
          "image/png")))))

(defn upload-media
  "POST a blob to /api/upload-media. Post: Promise<media-id>."
  [blob filename]
  (let [fd (js/FormData.)]
    (.append fd "file" blob filename)
    (-> (js/fetch "/api/upload-media"
          #js {:method "POST" :credentials "same-origin" :body fd})
      (.then (fn [^js r]
               (if (.-ok r)
                 (.json r)
                 (throw (js/Error. (str "Upload failed: " (.-status r)))))))
      (.then (fn [^js j]
               (if (and (.-success j) (.-id j))
                 (.-id j)
                 (throw (js/Error. (or (.-error j) "Upload failed")))))))))
