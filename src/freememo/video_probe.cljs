(ns freememo.video-probe
  "Read a picked File's duration in the browser, before any byte is uploaded
   (plans/incremental-video.md §15.3 5.3).

   Its own namespace rather than a fn in `freememo.video-upload`: that namespace
   is the chunked transfer protocol, and reading container metadata off a local
   File is not part of it. Nothing here talks to the server.

   Why the browser at all — the honest answer is ordering. `ffprobe` would be
   exact, but the bytes are not on the server until the upload finishes, which is
   after the decision this figure informs. A `<video preload=\"metadata\">` reads
   the container's own header locally in milliseconds.

   The cost of that: a container the browser cannot decode reports nothing. MKV
   plays in Chromium and not in Safari, so the same file yields a duration in one
   and nil in the other — which is why every caller must have a nil path.")

(def ^:private probe-timeout-ms
  "A metadata read is a header read — milliseconds for a local file. 5 s is the
   wall for a browser that fires neither `loadedmetadata` nor `error`, which
   would otherwise leave the object URL alive for the page's lifetime."
  5000)

(defn probe-duration-seconds!
  "Duration of `file` in seconds, as a Promise.

   Pre:  `file` is a File or Blob.
   Post: resolves to a positive number, or to NIL — never rejects. Nil means
         'this browser cannot tell us', from any of four causes: an `error`
         event, a non-finite duration (a stream with no declared length), a
         non-positive duration, or the timeout.
   Invariant: the object URL is revoked on every path, exactly once. A leaked
         blob URL pins the whole file in memory until the page unloads, and these
         files are gigabytes.

   `!settled` is a plain local atom, not a `defonce` — one probe, one lifetime.
   The CAS is what makes the timeout and the events mutually exclusive."
  [^js file]
  (js/Promise.
    (fn [resolve _reject]
      (let [url (js/URL.createObjectURL file)
            el (js/document.createElement "video")
            !settled (atom false)
            settle! (fn [v]
                      (when (compare-and-set! !settled false true)
                        (js/URL.revokeObjectURL url)
                        (resolve v)))
            timer (js/setTimeout #(settle! nil) probe-timeout-ms)
            finish! (fn [v] (js/clearTimeout timer) (settle! v))]
        (set! (.-preload el) "metadata")
        (.addEventListener el "loadedmetadata"
          (fn [_]
            (let [d (.-duration el)]
              (finish! (when (and (number? d) (js/isFinite d) (pos? d)) d)))))
        (.addEventListener el "error" (fn [_] (finish! nil)))
        (set! (.-src el) url)))))
