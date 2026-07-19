(ns freememo.pdf-viewer
  "PDF.js viewer integration utilities."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [taoensso.telemere :as tel]
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

;; True while a programmatic page restore AND its post-fit reflow settle window
;; are in progress. During this window `pagechanging` events originate from the
;; restore jump and PDF.js's own layout reflow — NOT the user — so page-anchor
;; persistence (URL segment, last-page) must ignore them. Cleared by the settle
;; logic in go-to-page-after-load!. See restoring? for the read-side predicate.
(defonce !restoring? (atom false))

;; Reflow settle detection. The per-frame pin treats the anchor page as settled
;; once its scroll position is unchanged for -stable-frames consecutive frames
;; (~100ms at 60fps); -max-ms is a hard ceiling so the pin can never stick.
(def ^:private reflow-stable-frames 6)
(def ^:private reflow-max-ms 3000)

;; Release fn of the in-flight page-pin, if any. A new pin supersedes the previous
;; one (rapid re-zoom) so pagerendered/gesture listeners never stack.
(defonce !pin-cancel (atom nil))

(defn restoring?
  "True iff a programmatic restore + reflow-settle window is active.
   Contract: page-anchor persisters (set-url-page! caller, last-page save) MUST
   skip while this holds — a page change here is not user navigation."
  []
  #?(:clj false :cljs @!restoring?))

;; Live mirror of the viewer's zoom, kept in sync by the scalechanging listener
;; registered once per viewer in init-viewer!. The toolbar e/watches these so the
;; % field and fit indicator reflect EVERY zoom source — −/+ buttons, % field,
;; fit toggle, ctrl+wheel — closing the desync the old uncontrolled <select> had.
(defonce !scale (atom 1.0))                 ; numeric currentScale
(defonce !scale-mode (atom "page-width"))   ; currentScaleValue: "page-width"|"page-fit"|numeric-string

;; UI zoom bounds. Clamped in zoom!/set-zoom!/set-zoom-pct! so every path — incl.
;; ctrl+wheel — stays inside them.
(def ^:private min-scale 0.25)   ; 25%
(def ^:private max-scale 5.0)    ; 500%

(defn- clamp-scale [s] (max min-scale (min max-scale s)))

(declare zoom!)   ; defined below; referenced by init-viewer!'s ctrl+wheel handler


(defn destroy-viewer!
  "Tear down the current PDF.js viewer and release resources."
  []
  #?(:clj nil
     :cljs
     (do
       (tel/log! {:level :debug :id ::destroy :data {:has-viewer? (some? @!viewer-state) :has-doc? (some? @!pdf-doc)}} "destroy-viewer!")
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
                   (.catch (fn [err] (tel/log! {:level :warn :id ::clipboard-write :error err} "clipboard write failed"))))
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
   (defn- pdf-url->doc-id
     "Cache/identity key for a /api/pdf/<id>?v=<version> url — the last path
      segment WITH its version query. Including the version makes a same-id byte
      change (e.g. a Live Document append) a DISTINCT key: the IndexedDB cache
      misses (forcing a fresh fetch) and set-document!'s same-key no-op guard
      fires. Unversioned urls key on the bare id string."
     [pdf-url]
     (-> pdf-url (.split "/") (.pop))))

#?(:cljs
   (defn- start-load!
     "Fetch pdf-url (cache-first) and `.setDocument` onto the viewer currently in
      `!viewer-state`, guarded by my-gen so stale loads skip. Shared by
      init-viewer! (fresh viewer) and set-document! (existing viewer, in-place
      swap). Destroys the previously-loaded PDFDocumentProxy before replacing."
     [pdf-url my-gen on-ready]
     (when-let [{:keys [^js viewer ^js event-bus ^js link-service]} @!viewer-state]
       (let [^js pdfjs (.-pdfjsLib js/window)
             doc-id (pdf-url->doc-id pdf-url)
             use-pdf (fn [^js pdf]
                       (cond
                         (not= my-gen @!init-gen)
                         (tel/log! {:level :debug :id ::skip-stale :data {:gen my-gen :current @!init-gen}} "skipping stale load")
                         ;; Already showing this doc — re-running .setDocument would
                         ;; null-deref in PDF.js AnnotationEditorUIManager.destroy.
                         (= doc-id @!loaded-doc-id)
                         (tel/log! {:level :debug :id ::skip-redundant :data {:doc-id doc-id}} "skipping redundant load")
                         :else
                         (do
                           (tel/log! {:level :debug :id ::doc-loaded :data {:gen my-gen :pages (.-numPages pdf)}} "document loaded")
                           (when-let [^js old @!pdf-doc] (try (.destroy old) (catch :default _)))
                           (reset! !pdf-doc pdf)
                           (reset! !loaded-doc-id doc-id)
                           (.setDocument viewer pdf)
                           (.setDocument link-service pdf nil)
                           (.on event-bus "pagesloaded"
                             (fn []
                               (js/console.log (str "[PDFDBG " (.toFixed (.now js/performance) 0) "] pagesloaded -> set fit page-width; currentScale before=" (.-currentScale viewer)))
                               (set! (.-currentScaleValue viewer) "page-width")
                               (js/console.log (str "[PDFDBG " (.toFixed (.now js/performance) 0) "] fit applied; currentScale after=" (.-currentScale viewer) " currentPage=" (.-currentPageNumber viewer))))
                             (clj->js {:once true}))
                           (when on-ready (on-ready pdf viewer)))))]
         (-> (pdf-cache/cache-get doc-id)
           (.then (fn [cached]
                    (if cached
                      (-> (.getDocument pdfjs (clj->js {:data (.slice cached)})) (.-promise))
                      (do (tel/log! {:level :debug :id ::cache-miss :data {:doc-id doc-id}} "cache miss, fetching")
                        (-> (js/fetch pdf-url)
                          (.then (fn [^js resp]
                                   (when-not (.-ok resp)
                                     (throw (js/Error. (str "PDF fetch failed: " (.-status resp)))))
                                   (.arrayBuffer resp)))
                          (.then (fn [^js buf]
                                   (pdf-cache/cache-put doc-id buf)
                                   (-> (.getDocument pdfjs (clj->js {:data (.slice buf)})) (.-promise)))))))))
           (.then use-pdf)
           (.catch (fn [err] (tel/log! {:level :error :id ::pdf-load :error err} "PDF load error"))))))))

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
         (tel/log! {:level :debug :id ::init :data {:gen my-gen :url pdf-url}} "init-viewer!")
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
           ;; Zoom sync (once per viewer — event-bus lives for the viewer's life,
           ;; reused across set-document! swaps, so no listener stacking). Mirrors
           ;; PDF.js scale into !scale/!scale-mode for the toolbar to e/watch.
           (.on event-bus "scalechanging"
             (fn [^js e]
               (reset! !scale (.-currentScale viewer))
               (reset! !scale-mode (.-currentScaleValue viewer))
               (js/console.log (str "[PDFDBG " (.toFixed (.now js/performance) 0)
                                    "] scalechanging -> scale=" (.-scale e)
                                    " scaleValue=" (.-currentScaleValue viewer)
                                    " page=" (.-currentPageNumber viewer)))))
           ;; ctrl+wheel zoom (native-viewer parity). passive:false so we can
           ;; preventDefault the browser's page-zoom. zoom! keeps the page anchor.
           (.addEventListener container "wheel"
             (fn [^js e]
               (when (.-ctrlKey e)
                 (.preventDefault e)
                 (zoom! (if (pos? (.-deltaY e)) 0.9 1.1))))
             #js {:passive false})
           (start-load! pdf-url my-gen on-ready))))))

(defn set-document!
  "Swap the displayed PDF on the EXISTING viewer (no destroy/recreate). Used when
   the topic advances to a different PDF while the viewer stays mounted, avoiding
   the frame-churn that corrupts the Electric incseq diff. No-op if no viewer is
   mounted (caller should init-viewer! first)."
  [pdf-url on-ready]
  #?(:clj nil
     :cljs
     (when (and (some? @!viewer-state)
             (not= (pdf-url->doc-id pdf-url) @!loaded-doc-id))
       (let [my-gen (swap! !init-gen inc)]
         (tel/log! {:level :debug :id ::set-document :data {:gen my-gen :url pdf-url}} "set-document!")
         (start-load! pdf-url my-gen on-ready)))))

(defn go-to-page!
  "Navigate to specific page number (1-indexed)."
  [page-num]
  #?(:clj nil
     :cljs (when-let [{:keys [viewer]} @!viewer-state]
             (let [^js v viewer]
               (js/console.log (str "[PDFDBG " (.toFixed (.now js/performance) 0) "] go-to-page! target=" page-num
                                    " (was currentPage=" (.-currentPageNumber v) " scale=" (.-currentScale v) ")"))
               (set! (.-currentPageNumber v) page-num)
               (js/console.log (str "[PDFDBG " (.toFixed (.now js/performance) 0) "] go-to-page! done; currentPage=" (.-currentPageNumber v)))))))

(defn- pin-anchor-through-reflow!
  "Lock the viewport onto anchor-page across a layout reflow (restore fit, or zoom
   scale change) by re-scrolling it into view every animation frame until its
   position stops moving for reflow-stable-frames.

   Why per-frame, not event-driven: on a slow machine the reflow commits over many
   frames with gaps that no single event (pagechanging/pagerendered) reliably
   covers, so an event + quiet-timer always leaves a hole a late drift slips
   through (observed: zoom p42 → held, released, then drifted to 38 and persisted).
   Why scrollPageIntoView, not a fixed scrollTop: it targets the page's OWN moving
   position, so the viewport shows anchor-page the whole time — no flash of a
   neighbour and no reliance on when PDF.js commits positions.

   Persistence is suppressed (!restoring?) for the window; a user gesture ends it
   early; reflow-max-ms caps it; a prior pin is superseded so nothing stacks.

   Pre:  viewer mounted; a reflow is in flight or imminent.
   Post: viewer sits on anchor-page; !restoring? true during, false after.
   Invariant: !restoring? cleared on exactly one path (release, idempotent).
   Trade-off: anchors to the page top — within-page scroll offset is not kept."
  [anchor-page]
  #?(:cljs
     (when-let [{:keys [viewer]} @!viewer-state]
       (when-let [cancel @!pin-cancel] (cancel))     ; supersede any active pin
       (let [^js v         viewer
             ^js container (.-container v)
             released?   (atom false)
             !on-gesture (atom nil)
             !self       (atom nil)
             !last       (atom nil)
             !stable     (atom 0)
             gestures    ["wheel" "touchstart" "keydown" "pointerdown"]
             release (fn []
                       (when-not @released?
                         (reset! released? true)
                         (when (and container @!on-gesture)
                           (doseq [ev gestures] (.removeEventListener container ev @!on-gesture)))
                         (when (identical? @!pin-cancel @!self) (reset! !pin-cancel nil))
                         (reset! !restoring? false)
                         (js/console.log (str "[PDFDBG " (.toFixed (.now js/performance) 0) "] pin: released on page=" anchor-page "; !restoring?=false"))))
             frame (fn frame []
                     (when-not @released?
                       (.scrollPageIntoView v (clj->js {:pageNumber anchor-page}))
                       (let [st (.-scrollTop container)]
                         (if (= st @!last)
                           (swap! !stable inc)
                           (do (reset! !stable 0) (reset! !last st))))
                       (if (>= @!stable reflow-stable-frames)
                         (release)
                         (js/requestAnimationFrame frame))))]
         (reset! !self release)
         (reset! !pin-cancel release)
         (reset! !restoring? true)
         (reset! !on-gesture (fn [] (release)))
         (when container
           (doseq [ev gestures]
             (.addEventListener container ev @!on-gesture #js {:once true :passive true})))
         (js/requestAnimationFrame frame)
         (js/setTimeout release reflow-max-ms)))))

(defn zoom!
  "Zoom by multiplication factor (1.1 = in, 0.9 = out), clamped to [min,max],
   preserving the viewport via scroll-ratio anchoring (see set-scale-anchored!).
   Pre: viewer mounted. Post: scale == clamp-scale(before×factor); the content at the
   viewport top is unchanged."
  [factor]
  #?(:clj nil
     :cljs (when-let [{:keys [viewer]} @!viewer-state]
             (let [^js v   viewer
                   anchor  (.-currentPageNumber v)]
               (js/console.log (str "[PDFDBG " (.toFixed (.now js/performance) 0) "] zoom! factor=" factor
                                    " scaleBefore=" (.-currentScale v) " page=" anchor " scrollTop=" (.-scrollTop (.-container v))))
               (set! (.-currentScale v) (clamp-scale (* (.-currentScale v) factor)))
               (pin-anchor-through-reflow! anchor)
               (js/console.log (str "[PDFDBG " (.toFixed (.now js/performance) 0) "] zoom! done scaleAfter=" (.-currentScale v) " page=" (.-currentPageNumber v) " scrollTop=" (.-scrollTop (.-container v))))))))

(defn set-zoom!
  "Set absolute zoom level (e.g., 1.0 = 100%, 1.5 = 150%)."
  [scale]
  #?(:clj nil
     :cljs (when-let [{:keys [viewer]} @!viewer-state]
             (let [^js v viewer]
               (js/console.log (str "[PDFDBG " (.toFixed (.now js/performance) 0) "] set-zoom! scale=" scale
                                    " scaleBefore=" (.-currentScale v) " page=" (.-currentPageNumber v)))
               (set! (.-currentScale v) (clamp-scale scale))
               (js/console.log (str "[PDFDBG " (.toFixed (.now js/performance) 0) "] set-zoom! done scaleAfter=" (.-currentScale v) " scaleValue=" (.-currentScaleValue v) " page=" (.-currentPageNumber v)))))))

(defn set-zoom-pct!
  "Set zoom to pct percent (150 → 1.5), clamped to [25,500], preserving the viewport
   via scroll-ratio anchoring (see set-scale-anchored!).
   Pre: pct is a finite number. Post: currentScale == clamp-scale(pct/100); the
   content at the viewport top is unchanged."
  [pct]
  #?(:clj nil
     :cljs (when-let [{:keys [viewer]} @!viewer-state]
             (let [^js v   viewer
                   anchor  (.-currentPageNumber v)]
               (set! (.-currentScale v) (clamp-scale (/ pct 100)))
               (pin-anchor-through-reflow! anchor)))))

(defn rotate-ccw!
  "Rotate all pages 90° counter-clockwise (viewer.pagesRotation −= 90)."
  []
  #?(:clj nil
     :cljs (when-let [{:keys [viewer]} @!viewer-state]
             (let [^js v viewer]
               (js/console.log (str "[PDFDBG " (.toFixed (.now js/performance) 0) "] rotate-ccw! from=" (.-pagesRotation v)))
               (set! (.-pagesRotation v) (- (.-pagesRotation v) 90))))))

(defn set-zoom-fit!
  "Fit page width, holding the current page across the reflow (like zoom!)."
  []
  #?(:clj nil
     :cljs (when-let [{:keys [viewer]} @!viewer-state]
             (let [^js v   viewer
                   anchor  (.-currentPageNumber v)]
               (js/console.log (str "[PDFDBG " (.toFixed (.now js/performance) 0) "] set-zoom-fit! (page-width) scaleBefore=" (.-currentScale v) " page=" anchor))
               (set! (.-currentScaleValue v) "page-width")
               (pin-anchor-through-reflow! anchor)))))

(defn set-zoom-page-fit!
  "Fit whole page, holding the current page across the reflow (like zoom!)."
  []
  #?(:clj nil
     :cljs (when-let [{:keys [viewer]} @!viewer-state]
             (let [^js v   viewer
                   anchor  (.-currentPageNumber v)]
               (js/console.log (str "[PDFDBG " (.toFixed (.now js/performance) 0) "] set-zoom-page-fit! (page-fit) scaleBefore=" (.-currentScale v) " page=" anchor))
               (set! (.-currentScaleValue v) "page-fit")
               (pin-anchor-through-reflow! anchor)))))

(defn go-to-page-after-load!
  "Restore to page-num and hold it there through the post-fit reflow.

   The app sets the page-width fit on `pagesloaded`; that fit re-lays-out every
   page over the following frames, sliding a single jump off the target (observed:
   restore to 20 → viewport settles on 30). pin-anchor-through-reflow! re-asserts
   the page across the whole reflow.

   Pre:  called once per load, before pagesloaded; !viewer-state has an event-bus.
   Post: viewer sits on page-num once reflow settles; !restoring? is true from
         arm-time through the window and false after — so persisters ignore the
         reflow's `pagechanging` noise and honor genuine user navigation after."
  [page-num]
  #?(:clj nil
     :cljs
     (when-let [{:keys [event-bus]} @!viewer-state]
       (let [^js eb event-bus]
         ;; Suppress from arm-time (before pagesloaded); pin takes over on load.
         (reset! !restoring? true)
         (js/console.log (str "[PDFDBG " (.toFixed (.now js/performance) 0) "] go-to-page-after-load! armed for page=" page-num "; !restoring?=true"))
         (.on eb "pagesloaded"
           (fn []
             (js/console.log (str "[PDFDBG " (.toFixed (.now js/performance) 0) "] restore: pagesloaded fired; pinning to " page-num " until reflow settles"))
             (pin-anchor-through-reflow! page-num))
           (clj->js {:once true}))))))

(defn on-page-change!
  "Register a callback for PDF.js page change events."
  [callback]
  #?(:clj nil
     :cljs (when-let [{:keys [event-bus]} @!viewer-state]
             (let [^js eb event-bus]
               (.on eb "pagechanging"
                 (fn [^js e]
                   (js/console.log (str "[PDFDBG " (.toFixed (.now js/performance) 0) "] pagechanging event -> page=" (.-pageNumber e)))
                   (callback (.-pageNumber e))))))))

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
                   (tel/log! {:level :error :id ::get-text-content :error err} "getTextContent error")
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
