(ns freememo.video-wavesurfer
  "CLJS-only wavesurfer.js v7 wrapper for the video waveform strip.

   Differs from `freememo.score-wavesurfer` in one decisive way: wavesurfer does
   NOT own playback here. The `<video>` element is the single clock
   (plans/incremental-video.md §4.8 8.2) — two media elements playing the same
   recording would drift audibly within seconds, and the user would hear both.

   Mechanism: decode the extracted MP3 in a throwaway instance to obtain peaks,
   destroy it, then create the real instance with `media` bound to the existing
   `<video>` and those peaks pre-supplied. wavesurfer then renders the waveform
   and drives its cursor from `media.currentTime`, and a click on the strip sets
   `media.currentTime`. Nothing else decodes, plays, or holds a second clock.

   Invariant: at most ONE region exists — the pending extract's range is never
   ambiguous. Same rule as the score editor, same reason.

   API:
     (init! {:container el :media video-el :audio-url s
             :on-region (fn [{:keys [start-ms end-ms]}]) :on-ready (fn [dur-s])})
       -> handle atom, or nil without the library
     (set-region! start-ms end-ms) (clear-region!) (play-region!) (set-loop! b)
     (set-zoom-factor! f) (destroy! handle)"
  (:require [freememo.logging :as log]))

(defonce ^:private !current (atom nil))

(defn- region->ms [^js region]
  {:start-ms (js/Math.round (* 1000 (.-start region)))
   :end-ms (js/Math.round (* 1000 (.-end region)))})

(defn- sole-region ^js []
  (when-let [{:keys [^js regions]} (some-> @!current deref)]
    (first (.getRegions regions))))

(defn- attach!
  "Create the display instance over `media`, rendering `peaks`, and fill
   `handle` with it. `handle` is created before the decode starts and is what
   the caller holds, so `destroy!` identifies the same atom whether it is called
   before or after the decode resolves."
  [handle {:keys [container media on-region on-ready]} peaks duration]
  (let [WS (.-WaveSurfer js/window)
        RegionsPlugin (.-Regions WS)
        ^js regions (.create RegionsPlugin)
        ;; `media` + `peaks` + `duration` and NO `url`: wavesurfer skips fetching
        ;; and decoding entirely and treats the video element as its transport.
        ^js ws (.create WS #js {:container container
                                :media media
                                :peaks peaks
                                :duration duration
                                :height 72
                                :normalize true
                                :waveColor "#9ca3af"
                                :progressColor "#6366f1"
                                :cursorColor "#6366f1"
                                :plugins #js [regions]})]
    (reset! handle {:ws ws :regions regions :container container
                    :media media :loop? false})
    (.enableDragSelection regions #js {:color "rgba(99,102,241,0.25)"})
    (.on regions "region-created"
      (fn [^js region]
        (doseq [^js r (vec (.getRegions regions))]
          (when-not (identical? r region) (.remove r)))
        (when on-region (on-region (region->ms region)))))
    (.on regions "region-updated"
      (fn [^js region]
        (when on-region (on-region (region->ms region)))))
    (.on regions "region-out"
      (fn [^js region]
        (when (:loop? @handle) (.play region))))
    (when on-ready (on-ready duration))
    (reset! !current handle)
    handle))

(defn init!
  "Decode `audio-url` for peaks, then attach the strip to `media`.

   Two-phase because the waveform's source and the playback's source are
   different objects: the peaks come from the small extracted MP3, the
   transport is the video. Returns a handle atom immediately; the strip appears
   once the decode resolves.

   Degrades to nil (no strip) when wavesurfer is absent or the MP3 has not been
   extracted yet — a video uploaded seconds ago has no audio blob, and the pane
   must still render."
  [{:keys [container media audio-url] :as opts}]
  (when (and container media (.-WaveSurfer js/window))
    (let [WS (.-WaveSurfer js/window)
          handle (atom nil)
          ;; Off-DOM decoder. Never appended, so it paints nothing; it exists
          ;; only to run wavesurfer's fetch+decode and hand back peaks.
          scratch (.createElement js/document "div")
          ^js probe (.create WS #js {:container scratch :url audio-url :height 1})]
      (reset! !current handle)
      (.on probe "ready"
        (fn []
          (let [peaks (.exportPeaks probe)
                ;; Prefer the VIDEO's duration: it is what the cursor is scaled
                ;; against. The MP3's own duration can differ by a frame, which
                ;; would skew the whole strip against the playhead.
                dur (let [d (.-duration media)]
                      (if (and (number? d) (js/isFinite d) (pos? d))
                        d
                        (.getDuration probe)))]
            (try (.destroy probe) (catch :default _ nil))
            ;; Unmounted while the decode was in flight: destroy! cleared the
            ;; handle, so there is nothing left to attach to.
            (when (identical? handle @!current)
              (attach! handle opts peaks dur)))))
      (.on probe "error"
        (fn [err]
          (log/log-error (str "[video-wavesurfer] peak decode failed: " err))
          (try (.destroy probe) (catch :default _ nil))))
      handle)))

(defn set-zoom-factor!
  "Zoom to factor × fit (1 = whole file visible)."
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

(defn set-region!
  "Replace the selection with [start-ms, end-ms]."
  [start-ms end-ms]
  (when-let [{:keys [^js regions]} (some-> @!current deref)]
    (doseq [^js r (vec (.getRegions regions))] (.remove r))
    (.addRegion regions #js {:start (/ start-ms 1000)
                             :end (/ end-ms 1000)
                             :color "rgba(99,102,241,0.25)"})))

(defn clear-region! []
  (when-let [{:keys [^js regions]} (some-> @!current deref)]
    (doseq [^js r (vec (.getRegions regions))] (.remove r))))

(defn play-region!
  "Play just the selection. Region.play() seeks and plays the bound media —
   which is the <video>, so the picture follows the audio."
  []
  (when-let [^js region (sole-region)]
    (.play region)))

(defn destroy! [handle]
  (when handle
    (when-let [{:keys [^js ws]} @handle]
      (try (.destroy ws) (catch :default _ nil)))
    (when (identical? handle @!current)
      (reset! !current nil))
    (reset! handle nil)))
