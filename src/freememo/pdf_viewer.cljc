(ns freememo.pdf-viewer
  "PDF.js viewer integration utilities."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.pdf-cache :as pdf-cache]))

;; Global viewer state (similar to electric-fiddle reference)
(defonce !viewer-state (atom nil))

;; Track the loaded PDFDocumentProxy so we can destroy it on cleanup
(defonce !pdf-doc (atom nil))

;; Init generation — incremented in init-viewer!/set-document!; stale async callbacks check & skip
(defonce !init-gen (atom 0))

;; doc-id currently displayed by the mounted viewer (nil = none loaded). The
;; persistent PdfViewerComponent reads this to decide init-viewer! vs in-place
;; set-document! on a document-id change.
(defonce !loaded-doc-id (atom nil))


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
       (reset! !loaded-doc-id nil)
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

#?(:cljs
   (defn- start-load!
     "Fetch pdf-url (cache-first) and `.setDocument` onto the viewer currently in
      `!viewer-state`, guarded by my-gen so stale loads skip. Shared by
      init-viewer! (fresh viewer) and set-document! (existing viewer, in-place
      swap). Destroys the previously-loaded PDFDocumentProxy before replacing."
     [pdf-url my-gen on-ready]
     (when-let [{:keys [^js viewer ^js event-bus ^js link-service]} @!viewer-state]
       (let [^js pdfjs (.-pdfjsLib js/window)
             doc-id (-> pdf-url (.split "/") (.pop) js/parseInt)
             use-pdf (fn [^js pdf]
                       (if (= my-gen @!init-gen)
                         (do
                           (js/console.log "[PDF] document loaded, gen=" my-gen "numPages=" (.-numPages pdf))
                           (when-let [^js old @!pdf-doc] (try (.destroy old) (catch :default _)))
                           (reset! !pdf-doc pdf)
                           (reset! !loaded-doc-id doc-id)
                           (.setDocument viewer pdf)
                           (.setDocument link-service pdf nil)
                           (.on event-bus "pagesloaded"
                             (fn [] (set! (.-currentScaleValue viewer) "page-width"))
                             (clj->js {:once true}))
                           (when on-ready (on-ready pdf viewer)))
                         (js/console.log "[PDF] SKIPPING stale load, gen=" my-gen "current=" @!init-gen)))]
         (-> (pdf-cache/cache-get doc-id)
           (.then (fn [cached]
                    (if cached
                      (-> (.getDocument pdfjs (clj->js {:data (.slice cached)})) (.-promise))
                      (do (js/console.log "[PDF] cache MISS, fetching doc-id=" doc-id)
                        (-> (js/fetch pdf-url)
                          (.then (fn [^js resp]
                                   (when-not (.-ok resp)
                                     (throw (js/Error. (str "PDF fetch failed: " (.-status resp)))))
                                   (.arrayBuffer resp)))
                          (.then (fn [^js buf]
                                   (pdf-cache/cache-put doc-id buf)
                                   (-> (.getDocument pdfjs (clj->js {:data (.slice buf)})) (.-promise)))))))))
           (.then use-pdf)
           (.catch (fn [err] (js/console.error "PDF load error:" err))))))))

(defn init-viewer!
  "Create the PDF.js viewer ONCE in the given container and load pdf-url.
   Destroys any previous viewer first. For swapping the document on an
   already-created viewer (no recreate), use set-document! instead.
   Calls on-ready when the PDF is loaded with (pdf viewer) args."
  [container viewer-div pdf-url on-ready]
  #?(:clj nil
     :cljs
     (when (and container viewer-div (.-pdfjsLib js/window) (.-pdfjsViewer js/window))
       ;; Increment generation FIRST — plain defn, so swap! always increments.
       ;; Any previous init's async callbacks see a stale gen and skip.
       (let [my-gen (swap! !init-gen inc)]
         (js/console.log "[PDF] init-viewer!" "gen=" my-gen "url=" pdf-url)
         (destroy-viewer!)
         (set! (.-innerHTML viewer-div) "")
         (set! (.-scrollTop container) 0)
         (let [^js viewer-ns (.-pdfjsViewer js/window)
               ^js event-bus (new (.-EventBus viewer-ns))
               ^js link-service (new (.-PDFLinkService viewer-ns) (clj->js {:eventBus event-bus}))
               ^js viewer (new (.-PDFViewer viewer-ns)
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
           (start-load! pdf-url my-gen on-ready))))))

(defn set-document!
  "Swap the displayed PDF on the EXISTING viewer (no destroy/recreate). Used when
   the topic advances to a different PDF while the viewer stays mounted, avoiding
   the frame-churn that corrupts the Electric incseq diff. No-op if no viewer is
   mounted (caller should init-viewer! first)."
  [pdf-url on-ready]
  #?(:clj nil
     :cljs
     (when (some? @!viewer-state)
       (let [my-gen (swap! !init-gen inc)]
         (js/console.log "[PDF] set-document!" "gen=" my-gen "url=" pdf-url)
         (start-load! pdf-url my-gen on-ready)))))

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

(defn get-page-text-content!
  "Extract plain text from a single page using PDF.js's text layer.
   page-num is 1-based. Returns Promise<String>. Inserts paragraph breaks
   at large Y-coordinate gaps and line breaks at smaller ones; CLJ-side
   returns nil. Resolves with empty string when no PDF is loaded or the
   text layer is missing (typical for scanned PDFs)."
  [page-num]
  #?(:clj nil
     :cljs
     (if-let [^js doc @!pdf-doc]
       (-> (.getPage doc page-num)
         (.then (fn [^js page] (.getTextContent page)))
         (.then (fn [^js content]
                  (let [items (.-items content)
                        n (.-length items)
                        parts #js []
                        state #js {:prev_y nil :prev_h nil}]
                    (loop [i 0]
                      (when (< i n)
                        (let [^js item (aget items i)
                              txt (.-str item)
                              tform (.-transform item)
                              this-y (when tform (aget tform 5))
                              this-h (.-height item)]
                          (when (and txt (pos? (.-length txt)) (some? this-y))
                            (let [prev-y (.-prev_y state)
                                  prev-h (.-prev_h state)]
                              (when (and (some? prev-y) (some? prev-h))
                                (let [dy (- prev-y this-y)
                                      line-h (js/Math.max prev-h (or this-h 0) 1)]
                                  (cond
                                    (> dy (* 1.5 line-h)) (.push parts "\n\n")
                                    (> dy (* 0.3 line-h)) (.push parts "\n"))))
                              (.push parts txt)
                              (set! (.-prev_y state) this-y)
                              (when (and this-h (pos? this-h))
                                (set! (.-prev_h state) this-h))))
                          (recur (inc i)))))
                    (.join parts ""))))
         (.catch (fn [err]
                   (js/console.error "[PDF] getTextContent error:" err)
                   "")))
       (js/Promise.resolve ""))))

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
