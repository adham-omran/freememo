(ns freememo.image-actions
  "What the context menu's image rows do to an <img>: copy it, save it, copy
   its address, and resolve it to a media id for a pin or an occlusion.

   Each action reports a failure through `on-error!` with a message a user can
   read. The caller decides how the message reaches the user.

   Formerly `freememo.editor-image-menu`, which also drew its own menu. The
   menu is freememo.context-menu now, and the rows are registry commands
   (plans/custom-context-menu.md)."
  (:require [clojure.string :as str]
            [freememo.client-errors :as ce]
            [freememo.util :as util]))

;; ---------------------------------------------------------------------------
;; Bytes — shared by the clipboard, download, and media-upload paths
;; ---------------------------------------------------------------------------

(defn- data-uri->blob
  "Decode a base64 data: URI into a Blob.
   Pre : `data-uri` starts with \"data:\" and carries a base64 payload.
   Post: returns a Blob whose type is the URI's declared MIME type.
   Inv : synchronous; performs no I/O."
  [data-uri]
  (let [parts   (str/split data-uri #",")
        header  (first parts)
        b64     (second parts)
        mime    (-> header (str/replace "data:" "") (str/replace ";base64" ""))
        bin-str (js/atob b64)
        n       (count bin-str)
        buf     (js/Uint8Array. n)]
    (dotimes [i n]
      (aset buf i (.charCodeAt bin-str i)))
    (js/Blob. (clj->js [buf]) (clj->js {:type mime}))))

(defn src->blob!
  "Resolve an <img> src to a Blob carrying its bytes.

   Pre : `src` is a non-blank string — a data: URI, a /api/media/<id> path,
         or an absolute http(s) URL.
   Post: resolves to a Blob; rejects with a js/Error whose message is safe to
         show a user.
   Inv : uploads nothing and mutates no DOM. Cross-origin sources succeed only
         when the origin sends permissive CORS headers; otherwise the fetch
         rejects, which is the caller's signal to fall back."
  [src]
  (if (str/starts-with? src "data:")
    (js/Promise.resolve (data-uri->blob src))
    (-> (js/fetch src (clj->js {:credentials "same-origin"}))
      (.then (fn [r]
               (if (.-ok r)
                 (.blob r)
                 (js/Promise.reject
                   (js/Error. (str "Server returned " (.-status r))))))))))

(defn- blob->png!
  "Re-encode a Blob as image/png, or pass it through if it already is.

   `ClipboardItem` accepts image/png in every browser that supports it at
   all; JPEG, WebP, GIF and SVG must be rasterised first.

   Pre : `blob` is an image Blob.
   Post: resolves to a Blob of type image/png; rejects if the image cannot be
         decoded or encoded.
   Inv : draws from a same-origin object URL, so the canvas is never tainted —
         a cross-origin image that got this far already passed CORS."
  [blob]
  (if (= "image/png" (.-type blob))
    (js/Promise.resolve blob)
    (js/Promise.
      (fn [resolve reject]
        (let [url (js/URL.createObjectURL blob)
              img (js/Image.)]
          (set! (.-onload img)
            (fn [_]
              (let [w (.-naturalWidth img)
                    h (.-naturalHeight img)]
                (.revokeObjectURL js/URL url)
                ;; An SVG with no intrinsic width/height reports 0x0 in
                ;; Chrome. Rasterising it would hand back a blank PNG, which
                ;; is worse than a stated failure.
                (if (or (zero? w) (zero? h))
                  (reject (js/Error. "This image has no intrinsic size to copy"))
                  (let [canvas (js/document.createElement "canvas")]
                    (set! (.-width canvas) w)
                    (set! (.-height canvas) h)
                    (.drawImage (.getContext canvas "2d") img 0 0)
                    (.toBlob canvas
                      (fn [png]
                        (if png
                          (resolve png)
                          (reject (js/Error. "Could not encode this image as PNG"))))
                      "image/png"))))))
          (set! (.-onerror img)
            (fn [_]
              (.revokeObjectURL js/URL url)
              (reject (js/Error. "Could not decode this image"))))
          (set! (.-src img) url))))))

(defn- absolute-url
  "Resolve `src` against the current document URL.
   Pre : `src` is any URL string, absolute or relative. data: URIs are
         returned unchanged (they are already absolute and have no base).
   Post: returns an absolute URL string."
  [src]
  (if (str/starts-with? src "data:")
    src
    (.-href (js/URL. src (.. js/window -location -href)))))

(defn- filename-for
  "Derive a download filename for `src` given the fetched blob's MIME type.

   Pre : `src` as in `src->blob!`; `mime` is the blob's type, possibly blank.
   Post: returns a non-blank filename carrying an extension.
   Inv : never returns \"download\" — an unnamed file is the failure this
         exists to prevent."
  [src mime]
  (let [ext (util/mime->ext mime)]
    (cond
      (re-matches #"/api/media/\d+" src)
      (str "media-" (last (str/split src #"/")) "." ext)

      (str/starts-with? src "data:")
      (str "image." ext)

      :else
      (let [path (try (.-pathname (js/URL. (absolute-url src)))
                      (catch :default _ ""))
            base (last (str/split path #"/"))]
        (if (str/blank? base)
          (str "image." ext)
          (if (str/includes? base ".")
            base
            (str base "." ext)))))))

;; ---------------------------------------------------------------------------
;; Menu actions
;; ---------------------------------------------------------------------------

(defn- clipboard-unavailable-message
  "Nil when the async clipboard API is usable, else why it is not.

   `navigator.clipboard` is exposed only in a secure context — https, or
   localhost. Reaching a dev server over plain http on a LAN or tailscale IP
   leaves it undefined, and every clipboard action fails with a TypeError
   that means nothing to a user.

   Pre : none. Post: nil, or a message naming the actual cause."
  []
  (cond
    (not (.-isSecureContext js/window))
    "Copying needs a secure connection — open the app over https or on localhost."

    (nil? (.-clipboard js/navigator))
    "This browser doesn't support copying images."

    :else nil))

(defn- write-image-to-clipboard!
  "Fetch, rasterise to PNG, and write to the clipboard.
   Pre : the clipboard API is available — `copy-image!` owns that check.
   Post: as `copy-image!`."
  [src on-error!]
  (-> (src->blob! src)
    (.then blob->png!)
    (.then (fn [png]
             (.write js/navigator.clipboard
               (clj->js [(js/ClipboardItem. (clj->js {"image/png" png}))]))))
    (.catch (fn [err]
              (ce/report! :image-menu/copy err)
              (on-error! (str "Couldn't copy this image. " (.-message err)))))))

(defn copy-image!
  "Write the image at `src` to the clipboard as PNG.
   Pre : `src` as in `src->blob!`; caller is in a user-gesture task.
   Post: on success the clipboard holds the image; on any failure — including
         an unavailable clipboard API — `on-error!` is called exactly once
         with a user-presentable message and nothing is written."
  [src on-error!]
  (if-let [blocked (clipboard-unavailable-message)]
    (on-error! blocked)
    (write-image-to-clipboard! src on-error!)))

(defn save-image!
  "Download the image at `src` under a derived filename.
   Pre : `src` as in `src->blob!`; caller is in a user-gesture task.
   Post: on success a download is initiated and the object URL is revoked; on
         failure `on-error!` is called once and no download starts."
  [src on-error!]
  (-> (src->blob! src)
    (.then (fn [blob]
             (let [url (js/URL.createObjectURL blob)
                   a   (js/document.createElement "a")]
               (set! (.-href a) url)
               (set! (.-download a) (filename-for src (.-type blob)))
               (.appendChild (.-body js/document) a)
               (.click a)
               (.remove a)
               (.revokeObjectURL js/URL url))))
    (.catch (fn [err]
              (ce/report! :image-menu/save err)
              (on-error! (str "Couldn't save this image. " (.-message err)))))))

(defn copy-image-address!
  "Write the image's absolute URL to the clipboard as text.
   Pre : `src` as in `src->blob!`.
   Post: on success the clipboard holds the URL; otherwise `on-error!` is
         called exactly once. This is the fallback for when the bytes are
         unreachable, so its own failure leaves the user no further option —
         which is why the message must name the cause."
  [src on-error!]
  (if-let [blocked (clipboard-unavailable-message)]
    (on-error! blocked)
    (-> (.writeText js/navigator.clipboard (absolute-url src))
      (.catch (fn [err]
                (ce/report! :image-menu/copy-address err)
                (on-error! "Couldn't copy the image address."))))))

;; ---------------------------------------------------------------------------
;; Media id resolution
;; ---------------------------------------------------------------------------

(defn resolve-src->media-id!
  "Resolve an <img> src to a numeric media id.
   - If src matches /api/media/<id>, parse and return a resolved js/Promise<id>.
   - Otherwise, upload the bytes to /api/upload-media and return
     a js/Promise<id> that resolves after the server confirms.
   Returns a js/Promise that resolves to an integer id or rejects on error."
  [src]
  (if (str/starts-with? src "/api/media/")
    ;; Fast path: extract id from URL
    (let [parts  (str/split src #"/")
          id-str (last parts)
          id     (js/parseInt id-str 10)]
      (if (js/isNaN id)
        (js/Promise.reject (js/Error. (str "Cannot parse media id from src: " src)))
        (js/Promise.resolve id)))
    ;; Slow path: upload raw bytes to get a media id
    (js/Promise.
      (fn [resolve reject]
        (-> (src->blob! src)
          (.then (fn [blob]
                   (let [ext  (util/mime->ext (.-type blob))
                         form (js/FormData.)]
                     (.append form "file" blob (str "image." ext))
                     (js/fetch "/api/upload-media"
                       (clj->js {:method "POST"
                                 :credentials "same-origin"
                                 :body form})))))
          (.then (fn [r]
                   (if (.-ok r)
                     (.json r)
                     (js/Promise.reject (js/Error. (str "Upload failed: " (.-status r)))))))
          (.then (fn [json]
                   (let [id (.-id json)]
                     (if id
                       (resolve id)
                       (reject (js/Error. "Upload response missing id"))))))
          (.catch reject))))))
