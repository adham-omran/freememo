(ns electric-starter-app.pdf-viewer
  "PDF.js viewer integration utilities."
  (:require
    [hyperfiddle.electric3 :as e]
    [hyperfiddle.electric-dom3 :as dom]))

;; Global viewer state (similar to electric-fiddle reference)
(defonce !viewer-state (atom nil))

(defn init-viewer!
  "Initialize PDF.js viewer with given container and PDF URL.
   Calls on-ready callback when PDF is loaded with (pdf viewer) args."
  [container viewer-div pdf-url on-ready]
  #?(:clj nil
     :cljs
     (when (and container viewer-div (.-pdfjsLib js/window) (.-pdfjsViewer js/window))
       (let [^js pdfjs        (.-pdfjsLib js/window)
             ^js viewer-ns    (.-pdfjsViewer js/window)
             ^js event-bus    (new (.-EventBus viewer-ns))
             ^js link-service (new (.-PDFLinkService viewer-ns) (clj->js {:eventBus event-bus}))
             ^js viewer       (new (.-PDFViewer viewer-ns)
                                   (clj->js {:container container
                                             :viewer viewer-div
                                             :eventBus event-bus
                                             :linkService link-service
                                             :textLayerMode 2      ; Enable text selection
                                             :annotationMode 2}))] ; Enable annotations
         (.setViewer link-service viewer)
         (reset! !viewer-state {:viewer viewer
                                :event-bus event-bus
                                :link-service link-service})

         ;; Load PDF document
         (-> (.getDocument pdfjs pdf-url)
             (.-promise)
             (.then (fn [^js pdf]
                      (.setDocument viewer pdf)
                      (.setDocument link-service pdf nil)
                      (when on-ready (on-ready pdf viewer))))
             (.catch (fn [err] (js/console.error "PDF load error:" err))))))))

(defn go-to-page!
  "Navigate to specific page number (1-indexed)."
  [page-num]
  #?(:clj nil
     :cljs (when-let [{:keys [viewer]} @!viewer-state]
             (let [^js v viewer]
               (set! (.-currentPageNumber v) page-num)))))

(defn zoom!
  "Zoom by multiplication factor (e.g., 1.1 = 110%, 0.9 = 90%)."
  [factor]
  #?(:clj nil
     :cljs (when-let [{:keys [viewer]} @!viewer-state]
             (let [^js v viewer]
               (set! (.-currentScale v) (* (.-currentScale v) factor))))))

(defn set-zoom!
  "Set absolute zoom level (e.g., 1.0 = 100%, 1.5 = 150%)."
  [scale]
  #?(:clj nil
     :cljs (when-let [{:keys [viewer]} @!viewer-state]
             (let [^js v viewer]
               (set! (.-currentScale v) scale)))))
