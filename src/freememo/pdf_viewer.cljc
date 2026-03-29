(ns freememo.pdf-viewer
  "PDF.js viewer integration utilities."
  (:require
    [hyperfiddle.electric3 :as e]
    [hyperfiddle.electric-dom3 :as dom]))

;; Global viewer state (similar to electric-fiddle reference)
(defonce !viewer-state (atom nil))

;; Track the loaded PDFDocumentProxy so we can destroy it on cleanup
(defonce !pdf-doc (atom nil))

;; Init generation — incremented in init-viewer!; stale async callbacks check & skip
(defonce !init-gen (atom 0))


(defn destroy-viewer!
  "Tear down the current PDF.js viewer and release resources."
  []
  #?(:clj nil
     :cljs
     (do
       (js/console.log "[PDF] destroy-viewer!" "has-viewer?" (some? @!viewer-state) "has-doc?" (some? @!pdf-doc))
       (when-let [{:keys [viewer]} @!viewer-state]
         (try (.cleanup ^js viewer) (catch :default _)))
       (when-let [^js doc @!pdf-doc]
         (try (.destroy doc) (catch :default _)))
       (reset! !pdf-doc nil)
       (reset! !viewer-state nil))))

;; Copy-to-clipboard helper — registered once globally
#?(:cljs
   (defonce _clipboard-listeners
     (let [copy-pdf-selection!
           (fn []
             (let [sel (js/window.getSelection)
                   text (.toString sel)
                   anchor (.-anchorNode sel)
                   in-pdf? (when anchor
                             (some? (.closest (if (= 1 (.-nodeType anchor))
                                               anchor
                                               (.-parentElement anchor))
                                     ".pdfViewer")))]
               (when (and in-pdf? (seq text))
                 (-> (js/navigator.clipboard.writeText text)
                     (.catch (fn [err] (js/console.warn "Clipboard write failed:" err))))
                 true)))]
       ;; Cmd+C / Ctrl+C
       (.addEventListener js/document "keydown"
         (fn [^js e]
           (when (and (or (.-metaKey e) (.-ctrlKey e))
                      (= "c" (.-key e))
                      (not (.-defaultPrevented e)))
             (when (copy-pdf-selection!)
               (.preventDefault e))))
         true)
       ;; Right-click: pre-write to clipboard when context menu opens
       (.addEventListener js/document "contextmenu"
         (fn [_] (copy-pdf-selection!))
         true)
       true)))

(defn init-viewer!
  "Initialize PDF.js viewer with given container and PDF URL.
   Destroys any previous viewer first.
   Calls on-ready callback when PDF is loaded with (pdf viewer) args."
  [container viewer-div pdf-url on-ready]
  #?(:clj nil
     :cljs
     (when (and container viewer-div (.-pdfjsLib js/window) (.-pdfjsViewer js/window))
       ;; Increment generation FIRST — this is a plain defn, not Electric reactive,
       ;; so swap! always increments correctly. Any previous init's async callbacks
       ;; will see a stale generation and skip.
       (let [my-gen (swap! !init-gen inc)]
         (js/console.log "[PDF] init-viewer!" "gen=" my-gen "url=" pdf-url)
         ;; Destroy previous viewer to release memory and detach event listeners
         (destroy-viewer!)
         ;; Clear any residual DOM from old viewer
         (set! (.-innerHTML viewer-div) "")
         (set! (.-scrollTop container) 0)

         (let [^js pdfjs        (.-pdfjsLib js/window)
               ^js viewer-ns    (.-pdfjsViewer js/window)
               ^js event-bus    (new (.-EventBus viewer-ns))
               ^js link-service (new (.-PDFLinkService viewer-ns) (clj->js {:eventBus event-bus}))
               ^js viewer       (new (.-PDFViewer viewer-ns)
                                     (clj->js {:container container
                                               :viewer viewer-div
                                               :eventBus event-bus
                                               :linkService link-service
                                               :textLayerMode 2
                                               :annotationMode 2}))]
           (.setViewer link-service viewer)
           (reset! !viewer-state {:viewer viewer
                                  :event-bus event-bus
                                  :link-service link-service})

           ;; Load PDF document
           (-> (.getDocument pdfjs pdf-url)
               (.-promise)
               (.then (fn [^js pdf]
                        ;; Guard: only proceed if this is still the latest init.
                        ;; If another init-viewer! ran while we were loading,
                        ;; our viewer was already destroyed — skip silently.
                        (if (= my-gen @!init-gen)
                          (do
                            (js/console.log "[PDF] document loaded, gen=" my-gen "numPages=" (.-numPages pdf))
                            (reset! !pdf-doc pdf)
                            (.setDocument viewer pdf)
                            (.setDocument link-service pdf nil)
                            ;; Set page-width zoom after pages are initialized
                            (.on event-bus "pagesloaded"
                                 (fn []
                                   (js/console.log "[PDF] pagesloaded: scale=" (.-currentScale viewer)
                                                   "scrollH=" (.-scrollHeight container)
                                                   "pages=" (.-pagesCount viewer))
                                   (set! (.-currentScaleValue viewer) "page-width")
                                   (js/console.log "[PDF] after page-width: scale=" (.-currentScale viewer)
                                                   "scrollH=" (.-scrollHeight container)))
                                 (clj->js {:once true}))
                            (when on-ready (on-ready pdf viewer)))
                          (js/console.log "[PDF] SKIPPING stale init, gen=" my-gen "current=" @!init-gen))))
               (.catch (fn [err] (js/console.error "PDF load error:" err)))))))))

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

(defn set-zoom-fit!
  []
  #?(:clj nil
     :cljs (when-let [{:keys [viewer]} @!viewer-state]
             (let [^js v viewer]
               (set! (.-currentScaleValue v) "page-width")))))

(defn set-zoom-page-fit!
  []
  #?(:clj nil
     :cljs (when-let [{:keys [viewer]} @!viewer-state]
             (let [^js v viewer]
               (set! (.-currentScaleValue v) "page-fit")))))

(defn go-to-page-after-load!
  "Navigate to page once PDF.js fires pagesloaded (pages not ready at setDocument time)."
  [page-num]
  #?(:clj nil
     :cljs (when-let [{:keys [event-bus]} @!viewer-state]
             (let [^js eb event-bus]
               (.on eb "pagesloaded"
                    (fn [] (go-to-page! page-num))
                    (clj->js {:once true}))))))

(defn on-page-change!
  "Register a callback for PDF.js page change events."
  [callback]
  #?(:clj nil
     :cljs (when-let [{:keys [event-bus]} @!viewer-state]
             (let [^js eb event-bus]
               (.on eb "pagechanging"
                    (fn [^js e] (callback (.-pageNumber e))))))))

(defn setup-pinch-zoom!
  "Enable pinch-to-zoom on the PDF viewer container for touch devices."
  [container]
  #?(:clj nil
     :cljs
     (let [!pinch-start-dist (atom nil)
           !pinch-start-scale (atom nil)
           touch-dist (fn [^js e]
                        (let [t (.-touches e)]
                          (when (>= (.-length t) 2)
                            (let [^js t0 (.item t 0)
                                  ^js t1 (.item t 1)
                                  dx (- (.-clientX t0) (.-clientX t1))
                                  dy (- (.-clientY t0) (.-clientY t1))]
                              (js/Math.sqrt (+ (* dx dx) (* dy dy)))))))]
       (.addEventListener container "touchstart"
         (fn [^js e]
           (when (>= (.-length (.-touches e)) 2)
             (reset! !pinch-start-dist (touch-dist e))
             (reset! !pinch-start-scale
               (when-let [{:keys [viewer]} @!viewer-state]
                 (.-currentScale ^js viewer)))))
         #js {:passive true})
       (.addEventListener container "touchmove"
         (fn [^js e]
           (when-let [start-dist @!pinch-start-dist]
             (when-let [start-scale @!pinch-start-scale]
               (when-let [curr-dist (touch-dist e)]
                 (when (> start-dist 0)
                   (.preventDefault e)
                   (let [ratio (/ curr-dist start-dist)
                         new-scale (* start-scale (min 5 (max 0.25 ratio)))]
                     (set-zoom! new-scale)))))))
         #js {:passive false})
       (.addEventListener container "touchend"
         (fn [^js e]
           (when (< (.-length (.-touches e)) 2)
             (reset! !pinch-start-dist nil)
             (reset! !pinch-start-scale nil)))
         #js {:passive true}))))
