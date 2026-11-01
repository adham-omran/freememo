(ns freememo.youtube-pane
  "The source pane of a YouTube topic: the embedded player, its error notice,
   and the clip controls (plans/youtube-kind.md §3, §4, §5).

   The player is plain JS behind a handle (`freememo.youtube-player`). Electric
   owns an empty element and the player puts its iframe inside it. Marks, the
   error and the handle are client atoms local to one topic's frame, because
   they are browser-local state no other surface reads.

   A root video and a clip use the same pane. A clip adds Replay clip and
   pauses at its end; both can mark and extract a clip, because clips of clips
   are allowed (decision N1)."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.electric-forms5 :as forms]
   [freememo.doc-context :as dctx]
   [freememo.tooltip :as tooltip]
   [freememo.video-format :refer [format-ms]]
   [freememo.video-extract :refer [range-title]]
   [freememo.video-pane :refer [save-position!]]
   [freememo.youtube-clip :as clip]
   [freememo.youtube-url :as youtube-url]
   #?(:clj [freememo.commands :as commands])
   #?(:clj [freememo.db :as db])
   #?(:cljs [freememo.youtube-player :as player])))

;; ---------------------------------------------------------------------------
;; Server
;; ---------------------------------------------------------------------------

(defn create-clip!*
  "Server side of Extract clip. Bumps :extract-clip (→ :tree-mutations) so the
   hierarchy panel shows the new child without a :refresh that would re-init
   the Quill editor beside the player.

   Post: {:success true :id N} or {:success false :error S}."
  [user-id parent-id start-ms end-ms title]
  #?(:clj (try
            (let [r (db/create-youtube-clip! user-id parent-id start-ms end-ms title)]
              (when (:success r) (commands/bump! user-id :extract-clip))
              r)
            (catch Exception e
              {:success false :error (.getMessage e)}))
     :cljs nil))

;; ---------------------------------------------------------------------------
;; Pure
;; ---------------------------------------------------------------------------

(defn error-message
  "The notice for a player error. `code` is YouTube's onError number, or :load
   when the player API did not load."
  [code]
  (cond
    (= code :load) "The YouTube player did not load. Check the connection, then open the topic again."
    (= code 100) "This video was removed or is private."
    (or (= code 101) (= code 150)) "The owner does not allow this video to play outside YouTube."
    (= code 153) "The YouTube player rejected its configuration, so the video cannot play here."
    :else "The YouTube player failed. Open the topic again, or watch it on YouTube."))

(defn embed-blocked?
  "True for the errors that only YouTube itself can play past."
  [code]
  (or (= code 101) (= code 150) (= code 153)))

(defn- start-of [yt] (:start-ms yt))

;; ---------------------------------------------------------------------------
;; Platform wrappers — reader conditionals live in plain defns, never in an
;; e/defn body.
;; ---------------------------------------------------------------------------

(defn mount-player!
  "Create the player for `topic-id` inside `host` and store its handle.

   Pre:  `host` is the Electric-owned empty element of this topic's frame.
   Post: `!handle` holds the handle. A player error lands in `!error`. A pause
         beacons the position; the first known duration beacons with it when
         the row has none."
  [!handle host topic-id yt !error]
  #?(:cljs (do
             (reset! !handle
               (player/mount! host
                 {:video-id (:video-id yt)
                  :start-ms (:start-ms yt)
                  :end-ms (:end-ms yt)
                  :resume-ms (clip/resume-ms yt)
                  :on-error (fn [code] (reset! !error code))
                  :on-pause (fn [ms] (save-position! topic-id ms))
                  :on-duration (fn [d]
                                 (when-not (:duration-ms yt)
                                   (save-position! topic-id
                                     (or (player/current-ms @!handle) (clip/resume-ms yt))
                                     d)))}))
             nil)
     :clj nil))

(defn destroy-player!
  "Beacon the position and remove the player. Runs on unmount."
  [!handle topic-id]
  #?(:cljs (do (when-let [ms (player/destroy! @!handle)]
                 (save-position! topic-id ms))
               (reset! !handle nil)
               nil)
     :clj nil))

(defn mark-now!
  "Set mark `k` (:start-ms or :end-ms) to the player's clock."
  [!handle !marks k]
  #?(:cljs (do (when-let [ms (player/current-ms @!handle)]
                 (swap! !marks assoc k ms))
               nil)
     :clj nil))

(defn replay-clip! [!handle]
  #?(:cljs (do (player/replay! @!handle) nil) :clj nil))

(defn show-time!
  "Write a mark into its field. Imperative, as in the inline number input
   pattern: a reactive :value prop would fight the text the user is typing."
  [node ms]
  #?(:cljs (do (set! (.-value node) (if ms (format-ms ms) "")) nil)
     :clj nil))

(defn commit-time!
  "Read a typed time into mark `k`. An unreadable entry restores the field to
   the mark it had, so the field never shows a time the command will not use."
  [!marks k e]
  #?(:cljs (let [node (.-target e)]
             (if-let [ms (clip/parse-time (.-value node))]
               (swap! !marks assoc k ms)
               (show-time! node (get @!marks k)))
             nil)
     :clj nil))

;; ---------------------------------------------------------------------------
;; Components
;; ---------------------------------------------------------------------------

(e/defn TimeField [label k !marks marks]
  (dom/input
    (dom/props {:class "input youtube-time" :type "text" :inputmode "decimal"
                :placeholder "m:ss" :aria-label label})
    (show-time! dom/node (get marks k))
    (dom/On "change" (fn [e] (commit-time! !marks k e)) nil)))

(e/defn ClipExtractButton
  "Create a child clip from the marks. Forms5 `Button!` for the reason
   `VideoExtractButton` records: its token comes from the click, so two clips
   with the same start still both fire. The range travels in the command, so
   the effect uses the marks as they were at the click.

   Clears the marks on success only, so a failed range stays for the user to
   fix, and the error shows as `aria-invalid`."
  [topic-id !marks marks]
  (e/client
    (let [user-id dctx/user-id
          title dctx/video-title
          start (:start-ms marks)
          end (:end-ms marks)]
      (e/for [[t [_op start-ms end-ms]]
              (forms/Button! [::extract start end]
                :class "btn btn-sm btn-primary"
                :disabled (not (and start end (< start end)))
                :label "Extract clip")]
        (let [result (e/server
                       (e/Offload
                         #(create-clip!* user-id topic-id start-ms end-ms
                            (range-title title start-ms end-ms))))]
          (case result
            (if (:success result)
              (do (e/on-unmount #(reset! !marks nil))
                  (t))
              (t (:error result)))))))))

(e/defn ClipControls
  "Mark in, Mark out, the two time fields, Extract clip, Replay clip on a
   clip, and the link to YouTube."
  [topic-id yt !handle]
  (e/client
    (let [!marks (atom nil)
          marks (e/watch !marks)]
      (dom/div
        (dom/props {:class "youtube-controls"})
        (dom/button
          (dom/props {:class "btn btn-sm btn-secondary"})
          (tooltip/Tooltip! "Set the clip start to the current time")
          (dom/text "Mark in")
          (dom/On "click" (fn [_] (mark-now! !handle !marks :start-ms)) nil))
        (TimeField "Clip start" :start-ms !marks marks)
        (dom/button
          (dom/props {:class "btn btn-sm btn-secondary"})
          (tooltip/Tooltip! "Set the clip end to the current time")
          (dom/text "Mark out")
          (dom/On "click" (fn [_] (mark-now! !handle !marks :end-ms)) nil))
        (TimeField "Clip end" :end-ms !marks marks)
        (ClipExtractButton topic-id !marks marks)
        (when (clip/clip? yt)
          (dom/button
            (dom/props {:class "btn btn-sm btn-secondary"})
            (tooltip/Tooltip! "Play this clip again from its start")
            (dom/text "Replay clip")
            (dom/On "click" (fn [_] (replay-clip! !handle)) nil)))
        (dom/a
          (dom/props {:class "youtube-link"
                      :href (youtube-url/watch-url (:video-id yt) (start-of yt))
                      :target "_blank" :rel "noopener noreferrer"})
          (dom/text "Open on YouTube"))))))

(e/defn PlayerError [code yt]
  (dom/div
    (dom/props {:class "youtube-error" :role "alert"})
    (dom/text (error-message code))
    (when (embed-blocked? code)
      (dom/a
        (dom/props {:href (youtube-url/watch-url (:video-id yt) (start-of yt))
                    :target "_blank" :rel "noopener noreferrer"})
        (dom/text "Open on YouTube")))))

(e/defn YoutubePlayer
  "One topic's player frame.

   Keyed on the topic, as `VideoPlayer` is: navigating from one YouTube topic
   to another re-binds the topic id without unmounting the caller, and the
   unmount closure must belong to the topic it beacons for. Keying makes the
   change an unmount and a mount, so each topic gets its own player."
  [topic-id yt]
  (e/client
    (e/for-by identity [topic-id [topic-id]]
      (let [!handle (atom nil)
            !error (atom nil)
            error (e/watch !error)]
        (dom/div
          (dom/props {:class "youtube-stage"})
          ;; The player's host. It has no Electric children, ever: the player
          ;; replaces its own target with an iframe inside it.
          (dom/div
            (dom/props {:class "youtube-frame"})
            (let [host dom/node]
              (e/snapshot (mount-player! !handle host topic-id yt !error))
              (e/on-unmount #(destroy-player! !handle topic-id))))
          (when (some? error)
            (PlayerError error yt))
          (ClipControls topic-id yt !handle))))))

(e/defn DesktopNotice
  "The desktop app shows a link in place of the player (decision D1). The
   Electron window opens an http link in the system browser."
  [yt]
  (dom/div
    (dom/props {:class "youtube-stage youtube-desktop"})
    (dom/p (dom/text "The desktop app cannot play YouTube videos."))
    (dom/a
      (dom/props {:class "btn btn-sm btn-primary"
                  :href (youtube-url/watch-url (:video-id yt) (start-of yt))
                  :target "_blank" :rel "noopener noreferrer"})
      (dom/text "Open in browser"))))

(e/defn YoutubePane
  "The player, or the desktop notice.

   Pre:  dctx/youtube is non-nil and dctx/video-topic-id is the topic in view."
  []
  (e/client
    (let [yt dctx/youtube]
      (if (:desktop? yt)
        (DesktopNotice yt)
        (YoutubePlayer dctx/video-topic-id yt)))))
