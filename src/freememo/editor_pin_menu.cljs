(ns freememo.editor-pin-menu
  "CLJS-only helper: right-click context menu for <img> blots in the main
   Quill editor. Provides `install-contextmenu!` which attaches a contextmenu
   listener to the editor container div.

   The menu has four items:
     - Pin to topic — Front
     - Pin to topic — Back
     - Image Occlusion…
     - Cancel

   When a pin action is chosen the supplied `on-pin!` callback is invoked with
   {:src <img-src> :placement \"front\"|\"back\"}.  When Image Occlusion is
   chosen, `on-occlude!` is invoked with {:media-id <int> :topic-id <int>}
   after the img src is resolved. The caller (editor_pane.cljc) owns what
   happens next (db/set-pin! / opening the occlusion modal)."
  (:require [clojure.string :as str]
            [freememo.client-errors :as ce]))

;; ---------------------------------------------------------------------------
;; Menu DOM builder
;; ---------------------------------------------------------------------------

(defn- remove-menu! []
  (when-let [existing (.querySelector js/document "#pin-context-menu")]
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

(defn- show-menu!
  "Render the pin context menu at [x y]. Calls on-pin! with
   {:src src :placement placement} when user selects front/back, or
   on-occlude! with {:src src} for Image Occlusion.
   If pin-count >= 2, the pin items are disabled."
  [x y src pin-count on-pin! on-occlude!]
  (remove-menu!)
  (let [capped? (>= pin-count 2)
        cap-tip "Max 2 pins. Remove one first."
        menu (js/document.createElement "div")]
    (set! (.-id menu) "pin-context-menu")
    (.setAttribute menu "class" "pin-context-menu")
    ;; Position absolutely at cursor
    (set! (.-style.left menu) (str x "px"))
    (set! (.-style.top menu) (str y "px"))
    ;; Build items
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
    ;; Dismiss on next click anywhere outside the menu
    (js/setTimeout
      (fn []
        (.addEventListener js/document "click"
          (fn dismiss [e]
            (when-not (.contains menu (.-target e))
              (remove-menu!)
              (.removeEventListener js/document "click" dismiss)))))
      0)))

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
    (let [parts (str/split src #"/")
          id-str (last parts)
          id (js/parseInt id-str 10)]
      (if (js/isNaN id)
        (js/Promise.reject (js/Error. (str "Cannot parse media id from src: " src)))
        (js/Promise.resolve id)))
    ;; Slow path: upload raw bytes to get a media id
    (js/Promise.
      (fn [resolve reject]
        ;; Fetch the image as a blob (works for data URIs and remote URLs)
        (-> (if (str/starts-with? src "data:")
              ;; data URI — decode locally without a fetch
              (let [parts (str/split src #",")
                    header (first parts)
                    b64 (second parts)
                    mime (-> header
                           (str/replace "data:" "")
                           (str/replace ";base64" ""))
                    bin-str (js/atob b64)
                    n (count bin-str)
                    buf (js/Uint8Array. n)
                    _ (dotimes [i n]
                        (aset buf i (.charCodeAt bin-str i)))
                    blob (js/Blob. (clj->js [buf]) (clj->js {:type mime}))]
                (js/Promise.resolve blob))
              ;; Remote URL — fetch the bytes
              (-> (js/fetch src (clj->js {:credentials "same-origin"}))
                (.then (fn [r]
                         (if (.-ok r)
                           (.blob r)
                           (js/Promise.reject (js/Error. (str "Fetch failed: " (.-status r)))))))))
          (.then (fn [blob]
                   (let [ext (last (str/split (.-type blob) #"/"))
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
   Returns a cleanup fn that removes the listener."
  [container-el get-topic-id get-pin-count on-pin! on-occlude!]
  (let [handler
        (fn [e]
          ;; Only intercept right-clicks on <img> elements inside .ql-editor
          (let [target (.-target e)
                tag (when target (.-tagName target))
                editor-el (.closest target ".ql-editor")]
            (when (and (= "IMG" tag) editor-el)
              (.preventDefault e)
              (.stopPropagation e)
              (let [src (.getAttribute target "src")
                    x (.-pageX e)
                    y (.-pageY e)
                    pin-count (get-pin-count)]
                (show-menu! x y src pin-count
                  (fn [{:keys [src placement]}]
                    (-> (resolve-src->media-id! src)
                      (.then (fn [media-id]
                               (on-pin! {:media-id media-id
                                         :placement placement
                                         :topic-id (get-topic-id)})))
                      (.catch (fn [err]
                                (js/console.warn "[PinMenu] resolve failed:" (str err))
                                (ce/report! :pin-menu/resolve err)))))
                  (fn [{:keys [src]}]
                    (-> (resolve-src->media-id! src)
                      (.then (fn [media-id]
                               (on-occlude! {:media-id media-id
                                             :topic-id (get-topic-id)})))
                      (.catch (fn [err]
                                (js/console.warn "[PinMenu] occlusion resolve failed:" (str err))
                                (ce/report! :pin-menu/occlusion-resolve err))))))))))]
    (.addEventListener container-el "contextmenu" handler)
    ;; Return cleanup fn
    (fn [] (.removeEventListener container-el "contextmenu" handler))))
