(ns freememo.video-format
  "Timestamp formatting shared by the video pane, transcript, and extract
   button. Its own namespace so those three can agree on the format without
   requiring each other — video-pane already requires video-extract, so the
   reverse edge would be a cycle.")

(defn format-bytes
  "Bytes as a short human string: \"512 KB\", \"700 MB\", \"8.0 GB\".

   Pure and platform-neutral, so a limit rendered server-side and a file size
   measured in the browser read in the same units — a rejection that says
   \"needs 700 MB\" must match the picker that said \"700 MB\" a moment earlier."
  [n]
  (let [n (long (max 0 (or n 0)))]
    (cond
      (< n 1048576) (str (quot n 1024) " KB")
      (< n 1073741824) (str (quot n 1048576) " MB")
      :else (let [tenths (quot (* 10 n) 1073741824)]
              (str (quot tenths 10) "." (rem tenths 10) " GB")))))

(defn format-ms
  "Milliseconds → \"m:ss\", or \"h:mm:ss\" past the hour. Lecture-length videos
   are common enough that a bare minute count would read as nonsense."
  [ms]
  (let [total (long (/ (max 0 (or ms 0)) 1000))
        h (quot total 3600)
        m (quot (rem total 3600) 60)
        s (rem total 60)
        pad2 (fn [n] (if (< n 10) (str "0" n) (str n)))]
    (if (pos? h)
      (str h ":" (pad2 m) ":" (pad2 s))
      (str m ":" (pad2 s)))))
