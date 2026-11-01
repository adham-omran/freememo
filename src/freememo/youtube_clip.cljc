(ns freememo.youtube-clip
  "The time arithmetic of a YouTube topic: where playback resumes, when a clip
   pauses at its end, and which ranges a clip may have
   (plans/youtube-kind.md 3.7, 4.2, 4.5, 5.2).

   No requires, so the rules a test checks are the rules the player and the
   server run. Times are integer milliseconds. A clip's range is
   [start-ms, end-ms); a root video has no range.")

(def poll-ms
  "How often the player reads the clock while a clip plays. Only the end check
   needs a poll: marks and the position save read the clock when they need it."
  250)

(def min-clip-ms
  "The shortest clip. A shorter range is almost always a double click on Mark
   in and Mark out, and it would pause before a frame is seen."
  1000)

(defn clip?
  "True when the topic data names a range."
  [{:keys [start-ms]}]
  (some? start-ms))

(defn resume-ms
  "Where playback starts on load.

   Root video: the saved position, unless it is within 1 s of a known end,
   where resuming would look like a broken video. Then 0.
   Clip: the saved position when it is inside the range, else the start.

   Post: an integer >= 0."
  [{:keys [start-ms end-ms duration-ms last-pos-ms]}]
  (let [pos (max 0 (or last-pos-ms 0))]
    (if (some? start-ms)
      (if (and (>= pos start-ms) (< pos end-ms)) pos start-ms)
      (if (and duration-ms (>= pos (- duration-ms 1000))) 0 pos))))

(defn max-step-ms
  "The largest clock step between two polls that still counts as playback.
   Twice the poll interval allows for one late poll; the playback rate scales
   how far the clock moves in that time. A larger step is a seek."
  [rate]
  (* 2 poll-ms (if (and (number? rate) (pos? rate)) rate 1)))

(defn crossed-end?
  "True when one poll saw playback cross the end of a clip from inside it.

   A seek past the end is not a crossing, so the user can watch on past a clip
   on purpose (decision E1, plans/youtube-kind.md 5.2.1).

   Pre: `prev-ms` is the clock at the previous poll, or nil before the first."
  [{:keys [start-ms end-ms]} prev-ms cur-ms rate]
  (boolean
    (and (some? start-ms) (some? prev-ms) (some? cur-ms)
         (>= prev-ms start-ms) (< prev-ms end-ms)
         (>= cur-ms end-ms)
         (<= (- cur-ms prev-ms) (max-step-ms rate)))))

(defn range-error
  "Why a clip range is invalid, or nil when it is valid.

   Pre:  `duration-ms` is the video's length, or nil when unknown.
         `parent` is the parent's {:start-ms :end-ms}; both nil for a root.
   Blame: a non-nil answer is the caller's bad input."
  [start-ms end-ms duration-ms {parent-start :start-ms parent-end :end-ms}]
  (cond
    (not (and (int? start-ms) (int? end-ms)))
    "Mark both the start and the end."

    (neg? start-ms)
    "The start cannot be before the video starts."

    (>= start-ms end-ms)
    "The end must come after the start."

    (< (- end-ms start-ms) min-clip-ms)
    "A clip must be at least 1 second long."

    (and duration-ms (> end-ms duration-ms))
    "The end is after the video ends."

    (and parent-start (or (< start-ms parent-start) (> end-ms parent-end)))
    "A clip of a clip must stay inside its range."

    :else nil))

(defn parse-time
  "A typed time in ms, or nil when it is not one.
   Accepts `ss`, `m:ss` and `h:mm:ss`, each with an optional fraction of a
   second (`1:05.5`)."
  [s]
  (when (string? s)
    (when-let [[_ a b c frac] (re-matches #"^\s*(\d+)(?::(\d{1,2}))?(?::(\d{1,2}))?(?:\.(\d{1,3}))?\s*$" s)]
      (let [n (fn [x] (if x #?(:clj (Long/parseLong x) :cljs (js/parseInt x 10)) 0))
            [h m sec] (cond c [(n a) (n b) (n c)]
                            b [0 (n a) (n b)]
                            :else [0 0 (n a)])
            frac-ms (if frac
                      (n (subs (str frac "00") 0 3))
                      0)]
        ;; The leading field is unbounded (`75:00` is 1:15:00); every field
        ;; after it is a clock field and stays under 60.
        (when (cond c (and (< m 60) (< sec 60))
                    b (< sec 60)
                    :else true)
          (+ (* 1000 (+ (* 3600 h) (* 60 m) sec)) frac-ms))))))
