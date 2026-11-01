(ns freememo.youtube-player
  "One YouTube embedded player, as plain JS state behind a handle
   (plans/youtube-kind.md 3.2–3.11, 5.1, 5.2).

   `YT.Player` REPLACES its target element with an iframe. So the target is a
   child `div` this namespace appends to `host`, and `host` is an
   Electric-owned element with no Electric children. Electric never indexes
   into a leaf, so the foreign iframe inside it is safe (CLAUDE.md, the
   deployment-hook section on foreign nodes).

   The player never autoplays. It cues the video at the resume position, so it
   starts paused there.

   A clip pauses when playback crosses its end, read by polling the clock every
   `youtube-clip/poll-ms` while it plays. A root video starts no timer."
  (:require [freememo.youtube-api :as api]
            [freememo.youtube-clip :as clip]))

(def ^:private nocookie-host "https://www.youtube-nocookie.com")

(def ^:private state-ended 0)
(def ^:private state-playing 1)
(def ^:private state-paused 2)

(defn current-ms
  "The player's clock in ms, or nil before the player is ready."
  [handle]
  (when-let [^js p (some-> handle deref :player)]
    (when (fn? (.-getCurrentTime p))
      (let [t (.getCurrentTime p)]
        (when (and (number? t) (js/isFinite t)) (js/Math.round (* 1000 t)))))))

(defn- duration-ms [^js p]
  (let [d (when (fn? (.-getDuration p)) (.getDuration p))]
    (when (and (number? d) (pos? d)) (js/Math.round (* 1000 d)))))

(defn- stop-poll! [handle]
  (when-let [t (:timer @handle)] (js/clearInterval t))
  (swap! handle assoc :timer nil :prev-ms nil))

(defn- poll! [handle]
  (let [{:keys [range]} @handle
        ^js player (:player @handle)
        cur (current-ms handle)
        rate (when (fn? (.-getPlaybackRate player)) (.getPlaybackRate player))]
    (when (clip/crossed-end? range (:prev-ms @handle) cur rate)
      (.pauseVideo player))
    (swap! handle assoc :prev-ms cur)))

(defn- start-poll! [handle]
  (stop-poll! handle)
  (when (clip/clip? (:range @handle))
    (swap! handle assoc :timer (js/setInterval #(poll! handle) clip/poll-ms))))

(defn- report-duration!
  "Call `on-duration` once, the first time the player knows the length. A cue
   makes it known on ready; a root at position 0 is never cued, so the first
   state change is the next chance."
  [handle on-duration]
  (when-not (:duration-reported? @handle)
    (when-let [d (some-> (:player @handle) duration-ms)]
      (swap! handle assoc :duration-reported? true)
      (when on-duration (on-duration d)))))

(defn- on-state-change [handle {:keys [on-pause on-duration]} ^js e]
  (let [state (.-data e)]
    (report-duration! handle on-duration)
    (cond
      (= state state-playing) (start-poll! handle)
      (or (= state state-paused) (= state state-ended))
      (do (stop-poll! handle)
          (when on-pause (on-pause (current-ms handle))))
      :else nil)))

(defn mount!
  "Create a player inside `host` and return its handle.

   Pre:  `host` is connected and has no children Electric manages.
         `opts` is {:video-id S :start-ms N? :end-ms N? :resume-ms N
                    :on-error (fn [code]) :on-pause (fn [ms])
                    :on-duration (fn [ms])}. A code is YouTube's onError
         number, or :load when the API did not load.
   Post: a handle atom. The player appears when the API is ready; `destroy!`
         before then cancels it."
  [host {:keys [video-id start-ms end-ms resume-ms on-error on-duration] :as opts}]
  (let [handle (atom {:player nil :timer nil :prev-ms nil
                      :range {:start-ms start-ms :end-ms end-ms}
                      :destroyed? false :duration-reported? false})
        target (.createElement js/document "div")]
    (.appendChild ^js host target)
    (-> (api/ensure!)
        (.then
          (fn [^js yt]
            (when-not (:destroyed? @handle)
              (let [Player (.-Player yt)
                    p (new Player target
                        #js {:host nocookie-host
                             :videoId video-id
                             :width "100%"
                             :height "100%"
                             :playerVars #js {:playsinline 1 :rel 0}
                             :events
                             #js {:onReady
                                  (fn [_]
                                    ;; Cue, never seek: a seek before the first
                                    ;; play starts playback, and the player
                                    ;; must not autoplay (3.11). A cue leaves
                                    ;; it paused at the resume position.
                                    (if (pos? resume-ms)
                                      (let [^js p (:player @handle)]
                                        (.cueVideoById p
                                          #js {:videoId video-id
                                               :startSeconds (/ resume-ms 1000)}))
                                      (report-duration! handle on-duration)))
                                  :onStateChange #(on-state-change handle opts %)
                                  :onError #(when on-error (on-error (.-data ^js %)))}})]
                (swap! handle assoc :player p)))))
        (.catch (fn [_] (when on-error (on-error :load)))))
    handle))

(defn replay!
  "Play a clip again from its start."
  [handle]
  (when-let [^js p (:player @handle)]
    (when-let [start (get-in @handle [:range :start-ms])]
      (.seekTo p (/ start 1000) true)
      (.playVideo p))))

(defn destroy!
  "Stop the poll and remove the player. Safe before the player exists and
   safe twice.

   Post: the clock reading at teardown, or nil when there was none."
  [handle]
  (when handle
    (let [ms (current-ms handle)]
      (stop-poll! handle)
      (swap! handle assoc :destroyed? true)
      (when-let [^js p (:player @handle)]
        (when (fn? (.-destroy p)) (.destroy p)))
      (swap! handle assoc :player nil)
      ms)))
