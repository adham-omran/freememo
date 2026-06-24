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
   [org.apache.pdfbox.pdmodel.graphics.image LosslessFactory]
   [java.io ByteArrayInputStream ByteArrayOutputStream]
   [javax.imageio ImageIO]))

;; ~0.25in margin in PDF points (72 pt/in).
(def ^:private page-margin 18.0)

(defn- add-image-page!
  "Append one A4 page to `doc` showing `img-bytes` scaled to fit within the
   page margins, centered, aspect-ratio preserved.
   Throws ex-info {:reason :bad-image} when the bytes are not a decodable image."
  [^PDDocument doc ^bytes img-bytes]
  (let [bimg (ImageIO/read (ByteArrayInputStream. img-bytes))]
    (when (nil? bimg)
      (throw (ex-info "Unreadable image" {:reason :bad-image})))
    (let [page (PDPage. PDRectangle/A4)
          _ (.addPage doc page)
          pdimg (LosslessFactory/createFromImage doc bimg)
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
   Returns {:success true :pages-added K :doc_id id}
        or {:success false :error <msg> :code <code>}."
  [user-id topic-id images]
  (try
    (let [existing (db/get-topic-file topic-id)
          ^PDDocument doc (if existing
                            (Loader/loadPDF ^bytes (:topic_files/file_data existing))
                            (PDDocument.))]
      (try
        (let [prev-total (.getNumberOfPages doc)
              prev-size (long (or (:topic_files/file_size existing) 0))]
          (doseq [img images] (add-image-page! doc img))
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
          {:success false :error "One of the files is not a readable image"
           :code "invalid-file-type"}

          :else
          (do (tel/error! {:id ::append-images} e)
              {:success false :error (str "Failed to append images: " (.getMessage e))}))))
    (catch Exception e
      (tel/error! {:id ::append-images} e)
      {:success false :error (str "Failed to append images: " (.getMessage e))})))
