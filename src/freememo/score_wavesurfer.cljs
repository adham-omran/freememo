(ns freememo.score-wavesurfer
  "CLJS-only wavesurfer.js v7 wrapper for the Score waveform strip.

   Loaded as CDN globals (window.WaveSurfer + window.WaveSurfer.Regions),
   matching the pdf.js/Konva/Quill precedent. One instance at a time — the
   Score editor is per-document like pdf-viewer's singleton viewer state; the
   live handle is kept module-level so the card-creation pipeline can reuse
   the ALREADY-DECODED AudioBuffer (getDecodedData) instead of re-fetching
   and re-decoding the recording.

   Invariant: at most ONE region exists — creating a new one removes the rest,
   so the pending card's audio segment is never ambiguous.

   API (occlusion-editor's init/destroy lifecycle split):
     (init! {:container el :url s :on-region (fn [{:keys [start-ms end-ms]}])
             :on-ready (fn [duration-s])}) -> handle atom, or nil w/o the lib
     (set-region! start-ms end-ms)  — programmatic selection (edit mode)
     (clear-region!)                — drop the selection (after card add)
     (play-region!) (play-pause!)   — playback
     (decoded-buffer)               — AudioBuffer of the loaded file, or nil
     (destroy! handle)"
  (:require [freememo.logging :as log]))

(defonce ^:private !current (atom nil))

(defn- region->ms [^js region]
  {:start-ms (js/Math.round (* 1000 (.-start region)))
   :end-ms (js/Math.round (* 1000 (.-end region)))})

(defn- sole-region ^js []
  (when-let [{:keys [^js regions]} (some-> @!current deref)]
    (first (.getRegions regions))))

(defn init!
  [{:keys [container url on-region on-ready]}]
  (when (and container (.-WaveSurfer js/window))
    (let [WS (.-WaveSurfer js/window)
          RegionsPlugin (.-Regions WS)
          ^js regions (.create RegionsPlugin)
          ^js ws (.create WS #js {:container container
                                  :url url
                                  :height 72
                                  :normalize true
                                  :waveColor "#9ca3af"
                                  :progressColor "#6366f1"
                                  :cursorColor "#6366f1"
                                  :plugins #js [regions]})
          handle (atom {:ws ws :regions regions :container container
                        :loop? false})]
      (.enableDragSelection regions #js {:color "rgba(99,102,241,0.25)"})
      (.on regions "region-created"
        (fn [^js region]
          ;; single-region invariant
          (doseq [^js r (vec (.getRegions regions))]
            (when-not (identical? r region) (.remove r)))
          (when on-region (on-region (region->ms region)))))
      (.on regions "region-updated"
        (fn [^js region]
          (when on-region (on-region (region->ms region)))))
      ;; Loop: replay the region when playback leaves it while loop is on.
      (.on regions "region-out"
        (fn [^js region]
          (when (:loop? @handle) (.play region))))
      (.on ws "ready" (fn [] (when on-ready (on-ready (.getDuration ws)))))
      (.on ws "error"
        (fn [err] (log/log-error (str "[score-wavesurfer] load failed: " err))))
      (reset! !current handle)
      handle)))

(defn set-zoom-factor!
  "Zoom the waveform to factor × fit (factor 1 = whole file fits the strip).
   No-op before the audio is decoded (duration unknown)."
  [factor]
  (when-let [{:keys [^js ws container]} (some-> @!current deref)]
    (let [dur (.getDuration ws)]
      (when (pos? dur)
        (.zoom ws (* factor (/ (.-clientWidth container) dur)))))))

(defn set-loop!
  "Toggle region-loop playback. Returns the new state."
  [on?]
  (when-let [handle @!current]
    (swap! handle assoc :loop? (boolean on?))
    (boolean on?)))

(defn set-volume!
  "Playback volume, clamped to [0, 1]."
  [v]
  (when-let [{:keys [^js ws]} (some-> @!current deref)]
    (.setVolume ws (js/Math.max 0 (js/Math.min 1 v)))))

(defn set-region!
  "Replace the selection with [start-ms, end-ms] (edit mode load)."
  [start-ms end-ms]
  (when-let [{:keys [^js regions]} (some-> @!current deref)]
    (doseq [^js r (vec (.getRegions regions))] (.remove r))
    (.addRegion regions #js {:start (/ start-ms 1000)
                             :end (/ end-ms 1000)
                             :color "rgba(99,102,241,0.25)"})))

(defn clear-region! []
  (when-let [{:keys [^js regions]} (some-> @!current deref)]
    (doseq [^js r (vec (.getRegions regions))] (.remove r))))

(defn play-region! []
  (when-let [^js region (sole-region)]
    (.play region)))

(defn play-pause! []
  (when-let [{:keys [^js ws]} (some-> @!current deref)]
    (.playPause ws)))

(defn decoded-buffer
  "AudioBuffer of the loaded recording (wavesurfer already decoded it for the
   waveform), or nil before ready."
  []
  (when-let [{:keys [^js ws]} (some-> @!current deref)]
    (.getDecodedData ws)))

(defn destroy! [handle]
  (when handle
    (when-let [{:keys [^js ws]} @handle]
      (try (.destroy ws) (catch :default _)))
    (when (identical? handle @!current)
      (reset! !current nil))
    (reset! handle nil)))
