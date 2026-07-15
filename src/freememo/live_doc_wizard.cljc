(ns freememo.live-doc-wizard
  "Live-Document upload wizard: the blob-less empty-state body (LiveDocEmptyState)
   and the add-photos flow (LiveDocAddPhotos) — a single [+] launcher that opens
   an Upload/Take-photo action sheet, then an editor modal where each staged
   photo is rotated and cropped before the batch is committed to
   /api/append-images.

   Split out of freememo.pdf-viewer-component so the PDF viewer no longer owns
   the wizard. Rotation and crop are METADATA: the original file bytes are sent
   unchanged with parallel `rotations`/`crops` arrays, and the server bakes both
   (freememo.live-doc). The Konva crop editor and canvas thumbnails live in the
   CLJS-only freememo.live-doc-image-editor; every JS-touching step called from a
   reactive body goes through a platform-split defn (CLJ/CLJS frame-slot rule),
   mirroring freememo.score-rect-modal.

   Nothing leaves the device until Done; Escape / backdrop / ✕ cancels and
   discards the staged batch (revoking its object URLs)."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.doc-context :as dctx]
   [freememo.modal-shell :as modal]
   #?(:cljs [freememo.live-doc-image-editor :as img-ed])))

(def ^:private heic-name-rx #"(?i)\.hei[cf]$")
(def ^:private heic-type-rx #"(?i)image/hei[cf]")
(def ^:private thumb-px 72)

;; ---------------------------------------------------------------------------
;; Platform wrappers — reader conditionals live in plain defns, never in the
;; e/defn reactive bodies (CLJ/CLJS frame signal-count rule). Each reads the
;; live entry from `!staged` at call time so the reactive body depends only on
;; the mount key (active-id × rotation), not on mutable entry contents.
;; ---------------------------------------------------------------------------

(defn mount-crop-editor!
  "Async: raster the active photo at its rotation and mount the Konva crop
   editor over it, seeding the stored crop and reporting changes via `set-crop!`.
   Guarded on container.isConnected — the modal may unmount mid-load."
  [!handle container !staged active-id set-crop!]
  #?(:cljs
     (do (when-let [entry (first (filter #(= (:id %) active-id) @!staged))]
           (when (= :ready (:status entry))
             (-> (img-ed/load-image (:url entry))
               (.then (fn [img]
                        (when (.-isConnected container)
                          (let [oriented (img-ed/oriented-canvas img (:rotation entry))]
                            (reset! !handle
                              (img-ed/init!
                                {:container container
                                 :canvas oriented
                                 :crop (:crop entry)
                                 :on-change (fn [c] (set-crop! active-id c))}))))))
               (.catch (fn [e] (js/console.error "[live-doc-wizard] editor load failed:" e))))))
         nil)
     :clj nil))

(defn destroy-crop-editor! [!handle]
  #?(:cljs (do (img-ed/destroy! @!handle) (reset! !handle nil) nil)
     :clj nil))

(defn paint-thumb!
  "Async: draw entry's rotated+cropped photo into `canvas` at thumb-px."
  [canvas entry]
  #?(:cljs
     (do (when (= :ready (:status entry))
           (-> (img-ed/load-image (:url entry))
             (.then (fn [img]
                      (when (.-isConnected canvas)
                        (img-ed/draw-thumb! canvas img (:rotation entry) (:crop entry) thumb-px))))
             (.catch (fn [_] nil))))
         nil)
     :clj nil))

;; ---------------------------------------------------------------------------

(e/defn LiveDocAddPhotos
  "Single [+] launcher + Upload/Take-photo action sheet + editor modal.
   Reads dctx/document-id (the Live Document's PDF root) and dctx/compact?
   (toolbar look vs wizard look for the launcher)."
  []
  (e/client
    (let [document-id dctx/document-id compact? dctx/compact?
          !upload-input (atom nil)
          !camera-input (atom nil)
          !busy         (atom false)
          !staged       (atom [])   ; [{:id :name :status :rotation :crop :url :payload :error}]
          !counter      (atom 0)
          !commit-error (atom nil)
          !open?        (atom false) ; editor modal
          !sheet-open?  (atom false) ; action sheet
          !active-id    (atom nil)   ; entry shown in the editor
          busy          (e/watch !busy)
          staged        (e/watch !staged)
          commit-error  (e/watch !commit-error)
          open?         (e/watch !open?)
          sheet-open?   (e/watch !sheet-open?)
          active-id     (e/watch !active-id)
          converting?   (boolean (some #(= :converting (:status %)) staged))
          active        (first (filter #(= (:id %) active-id) staged))
          active-rot    (:rotation active)
          set-entry!    (fn [id f] (swap! !staged (fn [v] (mapv #(if (= (:id %) id) (f %) %) v))))
          stage-files!  (fn [files]
                          (doseq [f files]
                            (let [id   (swap! !counter inc)
                                  nm   (.-name f)
                                  heic? (boolean (or (re-find heic-type-rx (or (.-type f) ""))
                                                     (re-find heic-name-rx (or nm ""))))]
                              (reset! !open? true)
                              (reset! !active-id id)
                              (if heic?
                                (do
                                  (swap! !staged conj {:id id :name nm :status :converting :rotation 0 :crop nil})
                                  (let [fd (js/FormData.)]
                                    (.append fd "image" f)
                                    (-> (js/fetch "/api/heic-preview" (clj->js {:method "POST" :body fd}))
                                      (.then (fn [^js r] (if (.-ok r) (.blob r) (js/Promise.reject "convert"))))
                                      (.then (fn [blob]
                                               (let [url (js/URL.createObjectURL blob)
                                                     jpg (js/File. (clj->js [blob]) (str nm ".jpg") (clj->js {:type "image/jpeg"}))]
                                                 (set-entry! id #(assoc % :status :ready :url url :payload jpg)))))
                                      (.catch (fn [_] (set-entry! id #(assoc % :status :error
                                                                        :error "Couldn’t read this image")))))))
                                (let [url (js/URL.createObjectURL f)]
                                  (swap! !staged conj {:id id :name nm :status :ready
                                                       :rotation 0 :crop nil :url url :payload f}))))))
          ;; Rotating invalidates the prior crop rect (axes swap) → reset it.
          rotate!       (fn [id] (set-entry! id #(-> % (update :rotation (fn [d] (mod (+ (or d 0) 90) 360)))
                                                     (assoc :crop nil))))
          set-crop!     (fn [id c] (set-entry! id #(assoc % :crop c)))
          remove!       (fn [id]
                          (when-some [e (first (filter #(= (:id %) id) @!staged))]
                            (when (:url e) (js/URL.revokeObjectURL (:url e))))
                          (swap! !staged (fn [v] (vec (remove #(= (:id %) id) v))))
                          (when (= id @!active-id)
                            (reset! !active-id (:id (first @!staged)))))
          revoke-all!   (fn [] (doseq [it @!staged] (when (:url it) (js/URL.revokeObjectURL (:url it)))))
          cancel!       (fn [] (revoke-all!) (reset! !staged []) (reset! !active-id nil)
                          (reset! !sheet-open? false) (reset! !open? false))
          open-sheet!   (fn [] (reset! !sheet-open? true))
          pick!         (fn [!inp] (reset! !sheet-open? false)
                          (when-some [inp @!inp] (.click inp)))
          commit!       (fn []
                          (let [items @!staged]
                            (when (and (seq items) (not @!busy)
                                       (not-any? #(= :converting (:status %)) items))
                              (reset! !busy true)
                              (reset! !commit-error nil)
                              (let [fd (js/FormData.)]
                                (.append fd "doc_id" (str document-id))
                                (doseq [it items] (.append fd "images" (:payload it)))
                                (.append fd "rotations" (js/JSON.stringify (clj->js (mapv :rotation items))))
                                (.append fd "crops" (js/JSON.stringify (clj->js (mapv #(:crop %) items))))
                                (-> (js/fetch "/api/append-images" (clj->js {:method "POST" :body fd}))
                                  (.then (fn [^js r] (.json r)))
                                  (.then (fn [^js d]
                                           (reset! !busy false)
                                           (if (.-success d)
                                             ;; Server bumped page count → reload-nonce versions the pdf
                                             ;; url; the viewer swaps the fresh blob in place. Empty-state
                                             ;; gate flips once the doc has a file, restoring normal layout.
                                             (do (revoke-all!) (reset! !staged [])
                                                 (reset! !active-id nil) (reset! !open? false))
                                             (reset! !commit-error (or (.-error d) "Upload failed")))))
                                  (.catch (fn [_]
                                            (reset! !busy false)
                                            (reset! !commit-error "Upload failed — please try again."))))))))
          launcher-style {:padding (if compact? "6px 10px" "12px 20px")
                          :cursor (if busy "wait" "pointer")
                          :background (if compact? "var(--color-bg-card)" "var(--color-accent, #2d6cdf)")
                          :color (if compact? "var(--color-text-primary)" "white")
                          :border (if compact? "1px solid var(--color-border)" "none")
                          :border-radius "6px" :font-size (if compact? "16px" "15px")
                          :font-weight "600" :line-height "1"}
          btn-style      {:padding "10px 14px" :cursor "pointer" :font-size "14px"
                          :background "var(--color-bg-card)" :color "var(--color-text-primary)"
                          :border "1px solid var(--color-border)" :border-radius "6px"
                          :text-align "left"}]
      (e/on-unmount (fn [] (revoke-all!)))

      ;; ── Launcher: single [+] (compact toolbar glyph, or the wizard button) ──
      (dom/button
        (dom/props {:title "Add photos" :disabled busy :style launcher-style})
        (dom/text (if compact? "＋" "＋ Add photos"))
        (dom/On "click" (fn [_] (open-sheet!)) nil))

      ;; ── Action sheet: Upload / Take photo (z above the editor modal) ──
      (when sheet-open?
        (dom/div
          (dom/props {:style {:position "fixed" :inset "0" :z-index "1200"
                              :display "flex" :align-items "center" :justify-content "center"
                              :background "rgba(0,0,0,0.4)"}})
          (dom/On "click" (fn [e] (when (= (.-target e) (.-currentTarget e)) (reset! !sheet-open? false))) nil)
          (modal/ModalEscape (fn [] (reset! !sheet-open? false)) "Add photos")
          (dom/div
            (dom/props {:class "modal-content"
                        :style {:width "280px" :max-width "90vw" :display "flex"
                                :flex-direction "column" :gap "10px"}})
            (dom/button (dom/props {:style btn-style})
              (dom/text "🖼  Upload images")
              (dom/On "click" (fn [_] (pick! !upload-input)) nil))
            (dom/button (dom/props {:style btn-style})
              (dom/text "📷  Take photo")
              (dom/On "click" (fn [_] (pick! !camera-input)) nil)))))

      ;; ── Hidden inputs: upload (multi) + camera (rear on mobile) ──
      (dom/input
        (dom/props {:type "file" :accept "image/*" :multiple true :style {:display "none"}})
        (reset! !upload-input dom/node)
        (dom/On "change"
          (fn [e] (stage-files! (array-seq (-> e .-target .-files)))
            (set! (-> e .-target .-value) "")) nil))
      (dom/input
        (dom/props {:type "file" :accept "image/*" :capture "environment" :style {:display "none"}})
        (reset! !camera-input dom/node)
        (dom/On "change"
          (fn [e] (stage-files! (array-seq (-> e .-target .-files)))
            (set! (-> e .-target .-value) "")) nil))

      ;; ── Editor modal ──
      (when open?
        (dom/div
          (dom/props {:class "modal-backdrop" :tabindex "-1"})
          (modal/ModalEscape cancel! "Edit photos")
          (dom/On "click" (fn [e] (when (= (.-target e) (.-currentTarget e)) (cancel!))) nil)
          (dom/div
            (dom/props {:class "modal-content"
                        :style {:width "760px" :max-width "95vw" :display "flex"
                                :flex-direction "column" :gap "12px"}})
            (dom/On "click" (fn [e] (.stopPropagation e)) nil)
            (dom/div
              (dom/props {:style {:display "flex" :align-items "center" :gap "8px"}})
              (dom/h3 (dom/props {:style {:margin "0" :font-size "16px"}})
                (dom/text "Edit photos"))
              (dom/span (dom/props {:style {:margin-left "auto" :font-size "12px"
                                            :color "var(--color-text-secondary)"}})
                (dom/text "Rotate to set orientation, then drag to crop.")))

            ;; Current-photo editor (remounts on active-id × rotation change;
            ;; crop edits update !staged without remounting).
            (dom/div
              (dom/props {:style {:display "flex" :flex-direction "column" :gap "8px"
                                  :align-items "center"}})
              (if (nil? active)
                (dom/div (dom/props {:style {:padding "40px" :color "var(--color-text-secondary)"
                                             :font-size "13px"}})
                  (dom/text "Add a photo to begin."))
                (case (:status active)
                  :converting (dom/div (dom/props {:style {:padding "40px" :font-size "13px"}})
                                (dom/text "Preparing…"))
                  :error (dom/div (dom/props {:style {:padding "40px" :color "#c0392b" :font-size "13px"}})
                           (dom/text (or (:error active) "Couldn’t read this image")))
                  (e/for-by identity [[aid _rot] [[active-id active-rot]]]
                    (dom/div
                      (dom/props {:style {:width "100%" :display "flex" :justify-content "center"
                                          :min-height "220px" :background "var(--color-bg-subtle, #f3f4f6)"
                                          :border "1px solid var(--color-border)" :border-radius "6px"
                                          :overflow "hidden"}})
                      (let [host dom/node
                            !handle (atom nil)]
                        (mount-crop-editor! !handle host !staged aid set-crop!)
                        (e/on-unmount (fn [] (destroy-crop-editor! !handle))))))))
              ;; Rotate (only for the active ready photo).
              (when (= :ready (:status active))
                (dom/button
                  (dom/props {:style (assoc btn-style :text-align "center")})
                  (dom/text "↻ Rotate 90°")
                  (dom/On "click" (fn [_] (rotate! active-id)) nil))))

            ;; Staging strip.
            (when (seq staged)
              (dom/div
                (dom/props {:style {:display "flex" :flex-wrap "wrap" :gap "8px"}})
                (e/for [entry (e/diff-by :id staged)]
                  (let [id (:id entry) st (:status entry) active? (= id active-id)]
                    (dom/div
                      (dom/props {:style {:position "relative" :width (str thumb-px "px")
                                          :height (str thumb-px "px") :cursor "pointer"
                                          :border (if active? "2px solid var(--color-accent, #2d6cdf)"
                                                      "1px solid var(--color-border)")
                                          :border-radius "4px" :overflow "hidden"
                                          :display "flex" :align-items "center" :justify-content "center"
                                          :background "var(--color-bg-card)" :font-size "11px"}})
                      (dom/On "click" (fn [_] (reset! !active-id id)) nil)
                      (case st
                        :converting (dom/text "…")
                        :error (dom/div (dom/props {:style {:color "#c0392b" :padding "4px"
                                                            :text-align "center"}})
                                 (dom/text "!"))
                        (e/for-by identity [_k [[(:rotation entry) (:crop entry)]]]
                          (dom/element "canvas"
                            (dom/props {:width (str thumb-px) :height (str thumb-px)
                                        :style {:width "100%" :height "100%"}})
                            (paint-thumb! dom/node entry))))
                      (dom/button
                        (dom/props {:title "Remove"
                                    :style {:position "absolute" :top "2px" :right "2px"
                                            :font-size "12px" :line-height "1" :padding "2px 5px"
                                            :border "none" :border-radius "3px" :cursor "pointer"
                                            :background "rgba(0,0,0,0.55)" :color "white"}})
                        (dom/text "✕")
                        (dom/On "click" (fn [e] (.stopPropagation e) (remove! id)) nil)))))))

            (when commit-error
              (dom/div (dom/props {:style {:color "#c0392b" :font-size "12px"}})
                (dom/text commit-error)))

            ;; Footer: add another + commit.
            (dom/div
              (dom/props {:style {:display "flex" :gap "8px" :justify-content "flex-end"
                                  :align-items "center"}})
              (dom/button
                (dom/props {:style (assoc btn-style :text-align "center") :disabled busy})
                (dom/text "＋ Add another")
                (dom/On "click" (fn [_] (open-sheet!)) nil))
              (dom/button
                (dom/props {:title "Add staged photos as pages"
                            :disabled (or busy converting? (empty? staged))
                            :style {:padding "10px 18px" :border "none" :border-radius "6px"
                                    :font-weight "600" :font-size "14px"
                                    :cursor (if (or busy converting? (empty? staged)) "not-allowed" "pointer")
                                    :background "var(--color-accent, #2d6cdf)" :color "white"
                                    :opacity (if (or busy converting? (empty? staged)) "0.6" "1")}})
                (dom/text (cond busy "Uploading…"
                                converting? "Preparing…"
                                :else (str "Done · " (count staged) " page" (when (not= 1 (count staged)) "s"))))
                (dom/On "click" (fn [_] (commit!)) nil)))))))))

(e/defn LiveDocEmptyState
  "Blob-less Live Document body: a centered light card inviting the first photo
   batch. Rendered by DocumentBody in place of the whole document layout while
   the doc has no pages; normal layout returns once the first page commits.
   Binds document-id from the ambient pdf-root-id for the add-photos flow."
  []
  (e/client
    (dom/div
      (dom/props {:style {:flex "1" :display "flex" :align-items "center"
                          :justify-content "center" :padding "24px"
                          :min-height "0" :overflow "auto"
                          :background "var(--color-bg-subtle, #f3f4f6)"}})
      (dom/div
        (dom/props {:style {:display "flex" :flex-direction "column" :align-items "center"
                            :gap "16px" :text-align "center" :max-width "360px"
                            :padding "32px 24px" :border-radius "10px"
                            :background "var(--color-bg-card)"
                            :border "1px solid var(--color-border)"
                            :box-shadow "0 1px 3px rgba(0,0,0,0.08)"}})
        (dom/div (dom/props {:style {:font-size "17px" :font-weight "600"
                                     :color "var(--color-text-primary)"}})
          (dom/text "Empty Live Document"))
        (dom/div (dom/props {:style {:font-size "14px" :line-height "1.5"
                                     :color "var(--color-text-secondary)"}})
          (dom/text "Take photos or upload images of your material — each becomes a page you can keep adding to."))
        (binding [dctx/document-id dctx/pdf-root-id dctx/compact? false]
          (LiveDocAddPhotos))))))
