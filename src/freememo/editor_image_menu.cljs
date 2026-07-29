(ns freememo.editor-image-menu
  "CLJS-only helper: right-click context menu for <img> blots in the main
   Quill editor. Provides `install-contextmenu!` which attaches a contextmenu
   listener to the editor container div.

   The menu has six items plus a divider:
     - Copy image            -> clipboard, as PNG
     - Save image            -> download, with a derived filename
     ---
     - Pin to topic — Front
     - Pin to topic — Back
     - Image Occlusion…
     - Cancel

   Copy image address is offered as a third clipboard item when Copy image is
   unlikely to succeed on its own — see `show-menu!`.

   The three clipboard/download actions are self-contained here. The three
   app actions are not: a pin choice invokes `on-pin!` with
   {:src :placement}, Image Occlusion invokes `on-occlude!`, and a failed
   copy or save invokes `on-error!` with a user-presentable message. The
   caller (editor_pane.cljc) owns what happens next — db/set-pin!, opening
   the occlusion modal, pushing a toast.

   Formerly `freememo.editor-pin-menu`; renamed once pinning became a
   minority of the menu."
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
              (let [canvas (js/document.createElement "canvas")]
                (set! (.-width canvas) (.-naturalWidth img))
                (set! (.-height canvas) (.-naturalHeight img))
                (.drawImage (.getContext canvas "2d") img 0 0)
                (.revokeObjectURL js/URL url)
                (.toBlob canvas
                  (fn [png]
                    (if png
                      (resolve png)
                      (reject (js/Error. "Could not encode this image as PNG"))))
                  "image/png"))))
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

(defn- copy-image!
  "Write the image at `src` to the clipboard as PNG.
   Pre : `src` as in `src->blob!`; caller is in a user-gesture task.
   Post: on success the clipboard holds the image; on failure `on-error!` is
         called once with a user-presentable message and nothing is written."
  [src on-error!]
  (-> (src->blob! src)
    (.then blob->png!)
    (.then (fn [png]
             (.write js/navigator.clipboard
               (clj->js [(js/ClipboardItem. (clj->js {"image/png" png}))]))))
    (.catch (fn [err]
              (ce/report! :image-menu/copy err)
              (on-error! (str "Couldn't copy this image. " (.-message err)))))))

(defn- save-image!
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

(defn- copy-image-address!
  "Write the image's absolute URL to the clipboard as text.
   Pre : `src` as in `src->blob!`.
   Post: on success the clipboard holds the URL. This is the fallback that
         works when the bytes are unreachable, so its own failure is reported
         but not otherwise recoverable."
  [src on-error!]
  (-> (.writeText js/navigator.clipboard (absolute-url src))
    (.catch (fn [err]
              (ce/report! :image-menu/copy-address err)
              (on-error! "Couldn't copy the image address.")))))

(defn- same-origin-bytes?
  "True when the image's bytes are reachable without relying on the origin's
   CORS policy — i.e. a data: URI or an app-served media path.
   Pre : `src` is a string. Post: boolean."
  [src]
  (or (str/starts-with? src "data:")
      (str/starts-with? src "/api/media/")))

;; ---------------------------------------------------------------------------
;; Menu DOM
;; ---------------------------------------------------------------------------

;; Listeners installed alongside the menu element. remove-menu! must detach
;; them, or a menu dismissed by clicking one of its own buttons would leave
;; the document-level handlers attached until some later click.
(defonce ^:private !menu-teardown (atom nil))

(defn- remove-menu!
  "Remove the menu element and detach every listener installed with it.
   Pre : none — idempotent. Post: no #image-context-menu in the document and
         no menu-owned document listeners remain."
  []
  (when-let [teardown @!menu-teardown]
    (teardown)
    (reset! !menu-teardown nil))
  (when-let [existing (.querySelector js/document "#image-context-menu")]
    (.remove existing)))

(defn- make-button
  "Create a styled menu button element."
  [label disabled? tooltip on-click]
  (let [btn (js/document.createElement "button")]
    (set! (.-textContent btn) label)
    (set! (.-disabled btn) disabled?)
    (when disabled?
      (.setAttribute btn "data-tooltip" tooltip)
      (.setAttribute btn "title" tooltip))
    (.addEventListener btn "click"
      (fn [e]
        (.stopPropagation e)
        (remove-menu!)
        (when (and (not disabled?) on-click)
          (on-click))))
    btn))

(defn- make-divider []
  (let [d (js/document.createElement "div")]
    (set! (.-className d) "image-context-menu__divider")
    d))

(defn- show-menu!
  "Render the image context menu at [x y].

   Pre : `src` is the right-clicked image's src; `pin-count` is the topic's
         current pin count.
   Post: exactly one #image-context-menu exists in the document, with its
         dismissal listeners registered in !menu-teardown.
   Inv : pin items are disabled at the K1 cap of 2; the clipboard and download
         items are never disabled — a failure surfaces through `on-error!`
         rather than as an inert button."
  [x y src pin-count on-pin! on-occlude! on-error!]
  (remove-menu!)
  (let [capped? (>= pin-count 2)
        cap-tip "Max 2 pins. Remove one first."
        menu    (js/document.createElement "div")]
    (set! (.-id menu) "image-context-menu")
    (.setAttribute menu "class" "image-context-menu")
    (set! (.-style.left menu) (str x "px"))
    (set! (.-style.top menu) (str y "px"))
    ;; Clipboard/download first — the items a user already expects to find
    ;; at the top of an image context menu.
    (.appendChild menu
      (make-button "Copy image" false nil
        #(copy-image! src on-error!)))
    (.appendChild menu
      (make-button "Save image" false nil
        #(save-image! src on-error!)))
    ;; Cross-origin bytes depend on the remote origin's CORS headers, so
    ;; "Copy image" may legitimately fail there. Offer the address as a
    ;; guaranteed-working alternative rather than lengthening the menu for
    ;; the same-origin case, where it is redundant.
    (when-not (same-origin-bytes? src)
      (.appendChild menu
        (make-button "Copy image address" false nil
          #(copy-image-address! src on-error!))))
    (.appendChild menu (make-divider))
    (.appendChild menu
      (make-button "Pin to topic — Front" capped? cap-tip
        #(on-pin! {:src src :placement "front"})))
    (.appendChild menu
      (make-button "Pin to topic — Back" capped? cap-tip
        #(on-pin! {:src src :placement "back"})))
    (.appendChild menu
      (make-button "Image Occlusion…" false nil
        #(on-occlude! {:src src})))
    (.appendChild menu
      (make-button "Cancel" false nil nil))
    (.appendChild js/document.body menu)
    ;; Dismissal: Escape, or a click outside the menu. The outside-click
    ;; listener is registered on a later tick so the contextmenu event that
    ;; opened the menu doesn't immediately close it.
    (let [on-key (fn [e] (when (= "Escape" (.-key e))
                           (.stopPropagation e)
                           (remove-menu!)))
          on-doc-click (fn [e] (when-not (.contains menu (.-target e))
                                 (remove-menu!)))
          timer (js/setTimeout
                  #(.addEventListener js/document "click" on-doc-click)
                  0)]
      (.addEventListener js/document "keydown" on-key)
      (reset! !menu-teardown
        (fn []
          (js/clearTimeout timer)
          (.removeEventListener js/document "keydown" on-key)
          (.removeEventListener js/document "click" on-doc-click))))))

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

;; ---------------------------------------------------------------------------
;; Public API
;; ---------------------------------------------------------------------------

(defn install-contextmenu!
  "Attach a contextmenu event listener to `container-el` (the Quill wrapper div).
   `get-topic-id` — zero-arg fn returning current topic-id (called at event time).
   `get-pin-count` — zero-arg fn returning current pin count for the topic.
   `on-pin!` — fn called with {:media-id <int> :placement \"front\"|\"back\"
                               :topic-id <int>} after src is resolved.
   `on-occlude!` — fn called with {:media-id <int> :topic-id <int>} after src
                   is resolved, when the user picks Image Occlusion.
   `on-error!` — fn called with a user-presentable message string when a
                 clipboard, download, or media-resolution step fails.
   Returns a cleanup fn that removes the listener and any open menu."
  [container-el get-topic-id get-pin-count on-pin! on-occlude! on-error!]
  (let [handler
        (fn [e]
          ;; Only intercept right-clicks on <img> elements inside .ql-editor
          (let [target    (.-target e)
                tag       (when target (.-tagName target))
                editor-el (.closest target ".ql-editor")]
            (when (and (= "IMG" tag) editor-el)
              (.preventDefault e)
              (.stopPropagation e)
              (let [src       (.getAttribute target "src")
                    x         (.-pageX e)
                    y         (.-pageY e)
                    pin-count (get-pin-count)]
                (show-menu! x y src pin-count
                  (fn [{:keys [src placement]}]
                    (-> (resolve-src->media-id! src)
                      (.then (fn [media-id]
                               (on-pin! {:media-id media-id
                                         :placement placement
                                         :topic-id (get-topic-id)})))
                      (.catch (fn [err]
                                (js/console.warn "[ImageMenu] resolve failed:" (str err))
                                (ce/report! :image-menu/resolve err)
                                (on-error! "Couldn't pin this image.")))))
                  (fn [{:keys [src]}]
                    (-> (resolve-src->media-id! src)
                      (.then (fn [media-id]
                               (on-occlude! {:media-id media-id
                                             :topic-id (get-topic-id)})))
                      (.catch (fn [err]
                                (js/console.warn "[ImageMenu] occlusion resolve failed:" (str err))
                                (ce/report! :image-menu/occlusion-resolve err)
                                (on-error! "Couldn't open image occlusion for this image.")))))
                  on-error!)))))]
    (.addEventListener container-el "contextmenu" handler)
    ;; Return cleanup fn
    (fn []
      (.removeEventListener container-el "contextmenu" handler)
      (remove-menu!))))
