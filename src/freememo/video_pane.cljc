(ns freememo.video-pane
  "The `<video>` surface and the wavesurfer region strip (§4.8 8.1–8.3, 8.7).

   Playback contract, stated once because three components depend on it: the
   `<video>` element is the ONLY clock. wavesurfer renders a waveform over it
   and moves its cursor from `media.currentTime`; the transcript pane seeks by
   writing `currentTime`. Nothing else plays audio, and nothing keeps a second
   position of its own.

   The element node is published into `dctx/!video-el` so the transcript pane
   can seek without a round-trip through the reactive graph — a browser-local
   effect needs no token (CLAUDE.md, 'Selection that invokes a side effect').
   The clock's readings go the other way through `dctx/!video-playhead`, which
   is what lets the transcript highlight the segment being spoken."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.doc-context :as dctx]
   [freememo.tooltip :as tooltip]
   [freememo.video-format :refer [format-ms]]
   [freememo.video-extract :refer [VideoExtractButton]]
   #?(:cljs [freememo.video-wavesurfer :as vws])))

;; ---------------------------------------------------------------------------
;; Platform wrappers — reader conditionals live in plain defns, never in an
;; e/defn body (Electric compiles a different signal count per peer otherwise).
;; ---------------------------------------------------------------------------

(defn save-position!
  "§4.8 8.7 — beacon the current position.

   `sendBeacon` rather than fetch: this fires on pause and on unmount, and a
   fetch issued during teardown is cancelled with the page. The browser queues
   a beacon independently and delivers it after navigation. Same-origin, so the
   session cookie rides along."
  [topic-id pos-ms]
  #?(:cljs (when (and topic-id pos-ms (.-sendBeacon js/navigator))
             (let [body (str "topic_id=" topic-id "&pos_ms=" (js/Math.round pos-ms))
                   blob (js/Blob. #js [body]
                          #js {:type "application/x-www-form-urlencoded"})]
               (.sendBeacon js/navigator "/api/video/position" blob))
             nil)
     :clj nil))

(defn element-time-ms
  "currentTime of a media element in ms, or nil."
  [el]
  #?(:cljs (when el
             (let [t (.-currentTime el)]
               (when (and (number? t) (js/isFinite t)) (js/Math.round (* 1000 t)))))
     :clj nil))

(defn publish-playhead!
  "Republish the playhead for this topic, incrementing `:seeks` when the change
   came from a seek rather than from ordinary playback.

   Pre:  `!playhead` is dctx/!video-playhead; `topic-id` is the video being
         played; `el` is its element.
   Post: the atom holds {:topic-id :ms :seeks} for THIS topic.
   Invariant: `:seeks` never decreases within one topic — the transcript treats
   any increment as 'the user asked to jump', and a reset would re-arm follow
   spuriously."
  [!playhead topic-id el seek?]
  #?(:cljs (when !playhead
             (swap! !playhead
               (fn [prev]
                 {:topic-id topic-id
                  :ms (element-time-ms el)
                  :seeks (cond-> (if (= topic-id (:topic-id prev)) (:seeks prev 0) 0)
                           seek? inc)}))
             nil)
     :clj nil))

(defn seek-to!
  "Set a media element's position, in ms. The transcript pane's whole
   interaction (§4.8 8.5)."
  [el ms]
  #?(:cljs (when (and el ms)
             (set! (.-currentTime el) (/ ms 1000))
             nil)
     :clj nil))

(defn resume-at!
  "Restore `pos-ms` once metadata is known, unless it is at or past the end —
   resuming to the final frame would look like a broken video."
  [el pos-ms]
  #?(:cljs (when (and el pos-ms (pos? pos-ms))
             (let [d (.-duration el)]
               (when (or (not (js/isFinite d)) (< (/ pos-ms 1000) (- d 1)))
                 (set! (.-currentTime el) (/ pos-ms 1000))))
             nil)
     :clj nil))

(defn init-waveform!
  "Deferred wavesurfer init — the container must be in the DOM, and the video
   element must exist, before wavesurfer can bind to it.

   Takes the ATOM holding the element, not the element: `@!video-el` at call
   time is a plain deref with no subscription, so a nil read (the strip's body
   running before the player's `reset!`) would be captured permanently and the
   waveform would never appear. Deref inside the timeout instead, by which point
   the player's DOM has committed."
  [!handle container !video-el audio-url on-region]
  #?(:cljs (do (js/setTimeout
                 (fn []
                   (when-let [media @!video-el]
                     (when (.-isConnected container)
                       (reset! !handle
                         (vws/init! {:container container
                                     :media media
                                     :audio-url audio-url
                                     :on-region on-region})))))
                 0)
             nil)
     :clj nil))

(defn destroy-waveform! [!handle]
  #?(:cljs (do (vws/destroy! @!handle) (reset! !handle nil) nil) :clj nil))

(defn play-region!* [] #?(:cljs (do (vws/play-region!) nil) :clj nil))
(defn set-loop!* [on?] #?(:cljs (do (vws/set-loop! on?) nil) :clj nil))
(defn clear-region!* [] #?(:cljs (do (vws/clear-region!) nil) :clj nil))
(defn set-zoom!* [f] #?(:cljs (do (vws/set-zoom-factor! f) nil) :clj nil))

;; ---------------------------------------------------------------------------
;; Components
;; ---------------------------------------------------------------------------

(e/defn VideoPlayer
  "The `<video>` element: resume on load, beacon on pause and unmount.

   Own e/defn for the 64KB method cap, and so the element's mount/unmount
   lifecycle is a frame of its own — the unmount beacon must fire when the
   VIDEO goes away, not when some ancestor re-renders."
  []
  (e/client
    (let [last-pos dctx/video-last-pos-ms
          !video-el dctx/!video-el
          !video-playhead dctx/!video-playhead]
      ;; Frame isolation keyed on the topic. Navigating video→video re-BINDS
      ;; dctx/video-topic-id without unmounting the enclosing `when`, and
      ;; `e/on-unmount`'s closure captures that reactive value: without a keyed
      ;; frame Electric rebuilds the closure and fires the OLD one while the
      ;; player is still mounted, clearing !video-el under a live element
      ;; (execution-model.md, "A Clojure function closing over an Electric value
      ;; gets rebuilt when that value changes"). Keying makes the transition an
      ;; honest unmount + mount, and gives each source its own <video> node
      ;; rather than swapping `src` on one element with the previous media's
      ;; buffered ranges still attached.
      (e/for-by identity [topic-id [dctx/video-topic-id]]
        (dom/div
          ;; `flex: 1 1 auto`.
          ;; SHRINK 1 is the load-bearing half, unchanged: the waveform strip is
          ;; a sibling below and is itself unshrinkable; when the content↕cards
          ;; split leaves the top region shorter than video + strip (the default
          ;; on a <900px window is a 50/50 split), an unshrinkable player pushed
          ;; the strip past the overflow:hidden boundary and it vanished
          ;; entirely. The player is the element that should give up space — it
          ;; has a picture to scale, the strip has a fixed 72px canvas.
          ;; GROW 1 was added with VideoSplitPane's vertical handle. The stack
          ;; now has a definite height, so spare space exists for the first time;
          ;; with grow 0 the player ignored it and dragging the handle down
          ;; enlarged the container while the picture stayed put.
          (dom/props {:style {:background "#000" :display "flex" :justify-content "center"
                              :align-items "center" :flex "1 1 auto"
                              ;; min-height, not max-height: the vertical drag
                              ;; handle in VideoSplitPane owns this height now, and
                              ;; a viewport-relative cap competed with it. The floor
                              ;; keeps controls plus one line of video visible when
                              ;; dragged to the extreme. FLAGGED: 120px is a chosen
                              ;; starting value, not a measured one.
                              :min-height "120px" :overflow "hidden"}})
          (dom/element "video"
            (dom/props {:controls true
                        :preload "metadata"
                        :src (str "/api/video/" topic-id)
                        ;; 100% of the (possibly shrunken) container, not 52vh —
                        ;; otherwise the video ignores the shrink and re-clips.
                        :style {:max-width "100%" :max-height "100%"
                                :display "block" :outline "none"}})
            (let [el dom/node]
              (reset! !video-el el)
              ;; Resume needs the duration, which is only known after metadata.
              (dom/On "loadedmetadata" (fn [_] (resume-at! el last-pos) nil) nil)
              (dom/On "pause" (fn [_] (save-position! topic-id (element-time-ms el)) nil) nil)
              ;; The transcript's highlight rides on these two. `timeupdate` is
              ;; throttled by the browser to ~250 ms (measured 3.6 Hz in
              ;; Chromium), which is the highlight's latency floor; `seeked`
              ;; exists so a scrub or a transcript-row click lands immediately
              ;; instead of waiting out that quarter second.
              (dom/On "timeupdate"
                (fn [_] (publish-playhead! !video-playhead topic-id el false)) nil)
              (dom/On "seeked"
                (fn [_] (publish-playhead! !video-playhead topic-id el true)) nil)
              (e/on-unmount
                (fn []
                  (save-position! topic-id (element-time-ms el))
                  ;; Compare-and-clear, not a bare reset!: the successor frame's
                  ;; mount may have already published its own element, and
                  ;; unmount/mount order between keyed branches is not specified.
                  ;; Only retract the pointer if it is still mine.
                  (swap! !video-el #(when-not (identical? % el) %)))))))))))

(e/defn VideoWaveformStrip
  "Waveform + range transport under the player (§4.8 8.3).

   Drag-select writes the range into `dctx/!video-region`; Play and Loop replay
   it through the video element. Reuses the score editor's single-region
   invariant, so the pending extract's range is never ambiguous.

   Renders an explanatory placeholder rather than an empty box when the audio
   has not been extracted yet — a video is watchable the instant it uploads,
   minutes before ffmpeg finishes."
  []
  (e/client
    (let [topic-id dctx/video-topic-id
          has-audio? dctx/video-has-audio?
          !video-region dctx/!video-region
          !video-el dctx/!video-el
          region (e/watch !video-region)
          !handle (atom nil)
          !zoom (atom 1)
          !loop? (atom false)
          loop? (e/watch !loop?)
          zoom-by! (fn [mult] (set-zoom!* (swap! !zoom #(max 1 (min 20 (* % mult))))))]
      (dom/div
        ;; Own stacking context, as in ScoreWaveformStrip: wavesurfer's
        ;; absolutely-positioned cursor/region layers would otherwise escape to
        ;; the root and paint over the toolbars above.
        ;; COLUMN, not a row. The waveform previously shared one flex line with
        ;; four buttons and a timestamp; in the viewer's middle column — now
        ;; narrower because the Transcript panel opens by default — that left the
        ;; canvas ~168px for a 43-minute file, about 15 seconds per pixel, which
        ;; paints as a flat line. wavesurfer was attaching correctly the whole
        ;; time; it had nowhere to draw. Controls get their own line, the
        ;; waveform gets the full width beneath them.
        (dom/props {:style {:display "flex" :flex-direction "column" :gap "6px"
                            :padding "6px 10px" :border-bottom "1px solid var(--color-border)"
                            :background "var(--color-bg-surface)"
                            :position "relative" :z-index "1" :flex-shrink "0"}})
        (if-not has-audio?
          (dom/span
            (dom/props {:style {:font-size "12px" :color "var(--color-text-secondary)"}})
            (dom/text "No audio track extracted yet — press Transcribe in the transcript pane."))
          (dom/div
            (dom/props {:style {:display "flex" :flex-direction "column" :gap "6px"
                                :min-width "0"}})
            (dom/div
            (dom/props {:style {:display "flex" :align-items "center"
                                :gap "var(--sp-2)" :min-width "0" :flex-wrap "wrap"}})
            (dom/button
              (dom/props {:class "btn btn-sm btn-secondary" :disabled (nil? region)})
              (tooltip/Tooltip! "Play the marked range")
              (dom/text "▶ range")
              (dom/On "click" (fn [_] (play-region!*)) nil))
            (dom/button
              (dom/props {:class (if loop? "btn btn-sm btn-primary" "btn btn-sm btn-secondary")})
              (tooltip/Tooltip! "Loop the marked range")
              (dom/text "⟲ loop")
              (dom/On "click" (fn [_] (set-loop!* (swap! !loop? not))) nil))
            (dom/span
              (dom/props {:style {:font-size "12px" :white-space "nowrap"
                                  :color "var(--color-text-secondary)"
                                  :min-width "104px"}})
              (dom/text (if region
                          (str (format-ms (:start-ms region)) " – " (format-ms (:end-ms region)))
                          "drag to mark")))
            (VideoExtractButton)
            (dom/div
              (dom/props {:style {:display "flex" :flex-direction "column" :gap "2px"}})
              (dom/button
                (dom/props {:class "btn btn-secondary"
                            :style {:padding "0 6px" :font-size "12px" :line-height "1.4"}})
                (tooltip/Tooltip! "Zoom waveform in")
                (dom/text "+")
                (dom/On "click" (fn [_] (zoom-by! 1.5)) nil))
              (dom/button
                (dom/props {:class "btn btn-secondary"
                            :style {:padding "0 6px" :font-size "12px" :line-height "1.4"}})
                (tooltip/Tooltip! "Zoom waveform out")
                (dom/text "−")
                (dom/On "click" (fn [_] (zoom-by! (/ 1 1.5))) nil)))
            ) ; end controls row
            ;; Full-width waveform row.
            (dom/div
              (dom/props {:style {:width "100%" :min-width "0" :overflow-x "auto"}})
              (let [host dom/node]
                (init-waveform! !handle host !video-el
                  (str "/api/audio/" topic-id)
                  (fn [r] (reset! !video-region r)))
                (e/on-unmount (fn [] (destroy-waveform! !handle)))))))))))

(e/defn VideoPane
  "Player + waveform strip, stacked above the editor/transcript row."
  []
  (e/client
    (VideoPlayer)
    (VideoWaveformStrip)))
