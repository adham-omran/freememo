(ns freememo.live-doc
  "Live Document business logic: append camera/upload images as A4 pages to a
   PDF, building the document lazily on the first batch. Storage and quota are
   delegated to freememo.db; PDF assembly uses PDFBox."
  (:require
   [freememo.db :as db]
   [freememo.quota :as quota]
   [freememo.user-state :as us]
   [taoensso.telemere :as tel])
  (:import
   [org.apache.pdfbox Loader]
   [org.apache.pdfbox.pdmodel PDDocument PDPage PDPageContentStream]
   [org.apache.pdfbox.pdmodel.common PDRectangle]
   [org.apache.pdfbox.pdmodel.graphics.image LosslessFactory JPEGFactory]
   [java.awt.image BufferedImage]
   [java.io ByteArrayInputStream ByteArrayOutputStream]
   [javax.imageio ImageIO]))

;; ~0.25in margin in PDF points (72 pt/in).
(def ^:private page-margin 18.0)

;; Above this source-byte size a page is embedded as JPEG (lossy, ~0.85) rather
;; than lossless, to bound blob growth from large photos. Below it, behaviour is
;; unchanged (lossless embed).
(def ^:private compress-threshold-bytes (* 10 1024 1024))
(def ^:private jpeg-quality (float 0.85))

(defn- rotate-image
  "Return `src` rotated clockwise by `deg` ∈ {0,90,180,270}.
   Pre:  deg is a non-negative multiple of 90 (handler normalizes).
   Post: deg=0 → src unchanged; 90/270 → dimensions swapped; result is an
         opaque TYPE_INT_RGB image (alpha flattened onto the rotated canvas)."
  ^BufferedImage [^BufferedImage src deg]
  (let [d (mod (long deg) 360)]
    (if (zero? d)
      src
      (let [w (.getWidth src) h (.getHeight src)
            swap? (or (= d 90) (= d 270))
            nw (if swap? h w) nh (if swap? w h)
            dst (BufferedImage. nw nh BufferedImage/TYPE_INT_RGB)
            g (.createGraphics dst)]
        (.translate g (/ (- nw w) 2.0) (/ (- nh h) 2.0))
        (.rotate g (Math/toRadians d) (/ w 2.0) (/ h 2.0))
        (.drawImage g src 0 0 nil)
        (.dispose g)
        dst))))

(defn- ->opaque-rgb
  "Draw `src` onto a fresh opaque TYPE_INT_RGB image so JPEG embedding (which
   has no alpha channel) never fails. No-op when `src` is already opaque RGB."
  ^BufferedImage [^BufferedImage src]
  (if (= (.getType src) BufferedImage/TYPE_INT_RGB)
    src
    (let [dst (BufferedImage. (.getWidth src) (.getHeight src) BufferedImage/TYPE_INT_RGB)
          g (.createGraphics dst)]
      (.drawImage g src 0 0 nil)
      (.dispose g)
      dst)))

(defn- add-image-page!
  "Append one A4 page to `doc` showing `img-bytes`, rotated clockwise by `deg`,
   scaled to fit within the page margins, centered, aspect-ratio preserved.
   Images whose source bytes exceed `compress-threshold-bytes` are embedded as
   JPEG (lossy); smaller ones stay lossless.
   Throws ex-info {:reason :bad-image} when the bytes are not a decodable image."
  [^PDDocument doc ^bytes img-bytes deg]
  (let [decoded (ImageIO/read (ByteArrayInputStream. img-bytes))]
    (when (nil? decoded)
      (throw (ex-info "Unreadable image" {:reason :bad-image})))
    (let [bimg (rotate-image decoded deg)
          compress? (> (alength img-bytes) compress-threshold-bytes)
          page (PDPage. PDRectangle/A4)
          _ (.addPage doc page)
          pdimg (if compress?
                  (JPEGFactory/createFromImage doc (->opaque-rgb bimg) jpeg-quality)
                  (LosslessFactory/createFromImage doc bimg))
          box (.getMediaBox page)
          pw (.getWidth box) ph (.getHeight box)
          max-w (- pw (* 2 page-margin))
          max-h (- ph (* 2 page-margin))
          iw (.getWidth pdimg) ih (.getHeight pdimg)
          scale (min (/ max-w iw) (/ max-h ih))
          dw (* iw scale) dh (* ih scale)
          x (/ (- pw dw) 2.0)
          y (/ (- ph dh) 2.0)]
      (with-open [cs (PDPageContentStream. doc page)]
        (.drawImage cs pdimg (float x) (float y) (float dw) (float dh))))))

(defn append-images!
  "Append `images` (a non-empty seq of image byte-arrays) as A4 pages to the
   Live Document `topic-id`'s PDF, creating the PDF if it has none yet.
   Pre:  topic-id is a kind='pdf', is_live=true topic owned by user-id — the
         HTTP handler enforces this before calling.
   Post: on success the blob gains one A4 page per image, page stubs are created,
         and usage_bytes is bumped by the blob growth. A quota failure persists
         nothing (the db tx aborts).
   `rotations` is a seq of clockwise degrees ∈ {0,90,180,270} index-aligned to
   `images`; missing/short entries default to 0 (no rotation).
   Returns {:success true :pages-added K :doc_id id}
        or {:success false :error <msg> :code <code>}."
  [user-id topic-id images rotations]
  (try
    (let [existing (db/get-topic-file topic-id)
          ^PDDocument doc (if existing
                            (Loader/loadPDF ^bytes (:topic_files/file_data existing))
                            (PDDocument.))]
      (try
        (let [prev-total (.getNumberOfPages doc)
              prev-size (long (or (:topic_files/file_size existing) 0))
              degs (concat (or rotations []) (repeat 0))]
          (doseq [[idx img deg] (map vector (range) images degs)]
            (try
              (add-image-page! doc img deg)
              (catch clojure.lang.ExceptionInfo e
                (throw (ex-info (.getMessage e) (assoc (ex-data e) :image-index idx))))))
          (let [baos (ByteArrayOutputStream.)
                _ (.save doc baos)
                new-bytes (.toByteArray baos)
                new-size (alength new-bytes)
                new-total (.getNumberOfPages doc)
                delta (max 0 (- new-size prev-size))
                {:keys [pages-added]} (db/commit-live-append!
                                        user-id topic-id new-bytes new-size
                                        delta prev-total new-total)]
            (swap! (us/get-atom user-id :refresh) inc)
            (swap! (us/get-atom user-id :tree-mutations) inc)
            {:success true :pages-added pages-added :doc_id topic-id}))
        (finally (.close doc))))
    (catch clojure.lang.ExceptionInfo e
      (let [data (ex-data e)]
        (cond
          (quota/quota-error? data)
          (case (:reason data)
            :file-too-large {:success false
                             :error "Batch exceeds the per-file limit"
                             :code "file-too-large"
                             :limit (:limit data) :incoming (:incoming data)}
            :over-quota {:success false
                         :error "Storage quota exceeded — delete documents to free space"
                         :code "over-quota"
                         :used (:used data) :limit (:limit data) :incoming (:incoming data)})

          (= (:reason data) :bad-image)
          {:success false
           :error (if-some [i (:image-index data)]
                    (str "Image " (inc (long i)) " is not a readable image")
                    "One of the files is not a readable image")
           :code "invalid-file-type"}

          :else
          (do (tel/error! {:id ::append-images} e)
              {:success false :error (str "Failed to append images: " (.getMessage e))}))))
    (catch Exception e
      (tel/error! {:id ::append-images} e)
      {:success false :error (str "Failed to append images: " (.getMessage e))})))
