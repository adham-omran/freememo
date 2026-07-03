(ns freememo.score-audio
  "CLJS-only MP3 clip cutting for Score cards.

   Slices a sample range out of an already-decoded AudioBuffer and encodes it
   with lamejs (CDN global window.lamejs) at 128 kbps. MP3 over OGG/Opus is
   deliberate: AnkiMobile/iOS plays neither OGG containers nor Opus.

   Encoding runs on the main thread; lamejs is ~20× realtime so a typical
   phrase clip (5–30 s) encodes in well under two seconds.")

(defn- to-int16 ^js [^js f32]
  (let [n (.-length f32)
        out (js/Int16Array. n)]
    (dotimes [i n]
      (let [v (js/Math.max -1 (js/Math.min 1 (aget f32 i)))]
        (aset out i (js/Math.round (* v 32767)))))
    out))

(defn cut-mp3
  "Encode [start-ms, end-ms] of `audio-buffer` (AudioBuffer) to MP3.
   Pre:  end-ms > start-ms; audio-buffer non-nil.
   Post: Promise<Blob audio/mpeg>; rejects when lamejs is absent or the
         range is empty after clamping to the buffer."
  [^js audio-buffer start-ms end-ms]
  (js/Promise.
    (fn [resolve reject]
      (try
        (let [lame (.-lamejs js/window)]
          (when-not lame
            (throw (js/Error. "MP3 encoder (lamejs) is not loaded")))
          (let [sr (.-sampleRate audio-buffer)
                stereo? (> (.-numberOfChannels audio-buffer) 1)
                s0 (js/Math.max 0 (js/Math.floor (* sr (/ start-ms 1000))))
                s1 (js/Math.min (.-length audio-buffer)
                     (js/Math.ceil (* sr (/ end-ms 1000))))
                n (- s1 s0)]
            (when-not (pos? n)
              (throw (js/Error. "Empty audio selection")))
            (let [l16 (to-int16 (.subarray (.getChannelData audio-buffer 0) s0 s1))
                  r16 (when stereo?
                        (to-int16 (.subarray (.getChannelData audio-buffer 1) s0 s1)))
                  ^js encoder (new (.-Mp3Encoder lame) (if stereo? 2 1) sr 128)
                  block 1152
                  chunks #js []]
              (loop [i 0]
                (when (< i n)
                  (let [e (js/Math.min n (+ i block))
                        ^js buf (if r16
                                  (.encodeBuffer encoder (.subarray l16 i e) (.subarray r16 i e))
                                  (.encodeBuffer encoder (.subarray l16 i e)))]
                    (when (pos? (.-length buf)) (.push chunks buf))
                    (recur e))))
              (let [^js tail (.flush encoder)]
                (when (pos? (.-length tail)) (.push chunks tail)))
              (resolve (js/Blob. chunks #js {:type "audio/mpeg"})))))
        (catch :default e (reject e))))))
