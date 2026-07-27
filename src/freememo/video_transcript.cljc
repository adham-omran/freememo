(ns freememo.video-transcript
  "Transcript pane for video topics (§4.8 8.4–8.6).

   The transcript is a REFERENCE artifact, not the topic's content: it sits in
   its own read-only column beside the editor, and a single Copy action is the
   only automatic path from it into `content`. That separation is the reason a
   video's `content` starts empty — the user curates what is worth keeping
   instead of inheriting a wall of speech-to-text that card generation would
   then chew through.

   Clicking a segment seeks the player. The interaction is browser-local (write
   `currentTime` on the element published in `dctx/!video-el`), so it needs no
   token and no server round-trip.

   The reverse direction — highlighting the segment being spoken and scrolling
   it into view — reads `dctx/!video-playhead`. Two things make that affordable.
   The active index is derived on the CLIENT from a server-projected vector of
   start times, so the 3.6 Hz clock costs no round-trips; and the index changes
   about once per segment, so Electric's work-skipping keeps the 3.6 Hz signal
   from reaching the DOM."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.electric-forms5 :as forms]
   ;; Scroll-window from the app-local copy, not the jar's: only it honours
   ;; `:reset-key`, and the transcript must NOT snap to the top when a
   ;; re-transcription changes the segment count under a reading user
   ;; (CLAUDE.md, virtual-scroll rules). `Tape` is unchanged upstream, so it
   ;; still comes from the jar — same split as library_cards.
   [hyperfiddle.electric-scroll0 :refer [Tape]]
   [freememo.scroll :refer [Scroll-window]]
   [freememo.doc-context :as dctx]
   [freememo.commands :as commands]
   [freememo.video-pane :as vp]
   [freememo.video-format :refer [format-ms]]
   #?(:clj [freememo.db :as db])
   #?(:clj [freememo.video :as video])
   #?(:clj [freememo.user-state :as us])))

(def ^:private row-height
  "Taller than the 36px table convention: a transcript row wraps to two lines of
   speech, where a table row holds one line of metadata."
  46)

(defn active-segment-index
  "Index of the segment being spoken at `ms`, or -1 when none is.

   Start times only, deliberately: a pause between segments keeps the previous
   row lit rather than blanking the highlight, and one array crosses the wire
   instead of two. The cost is that a long silence still shows the last thing
   said, which is what a reader following along wants to see anyway.

   Binary search because this runs at the clock's rate (3.6 Hz) against a
   vector that reaches a few thousand entries on a lecture.

   Pre:  `starts` is ascending (the query orders by `ord`, and `ord` is assigned
         in chunk-offset order — `video/process-video!`).
   Post: -1, or an index i with (<= (starts i) ms) and either i is last or
         (< ms (starts (inc i)))."
  [starts ms]
  (if (or (nil? ms) (empty? starts) (< ms (nth starts 0)))
    -1
    (loop [lo 0 hi (dec (count starts))]
      (if (>= lo hi)
        lo
        (let [mid (quot (+ lo hi 1) 2)]
          (if (<= (nth starts mid) ms)
            (recur mid hi)
            (recur lo (dec mid))))))))

(defn scroll-row-to-center!
  "Park row `idx` at the vertical centre of `node`, and record where we left the
   scroll.

   Centre, not nudge-into-view: the playhead then sits at a fixed reading
   position with context above and below it, instead of drifting to an edge and
   jumping a screenful. The cost is a scroll on every segment change (~5 s)
   rather than one per screenful.

   Computed from the INDEX, not from the row's node: `Tape` only mounts the
   visible window, so when the user has scrolled far away the active row has no
   node at all and `scrollIntoView` would have nothing to call. The arithmetic
   works whether or not the row is mounted, and it does not scroll ancestors.

   Clamping is the browser's: an index near either end cannot be centred, so
   `scrollTop` saturates at 0 or at the bottom. That is why `!last-auto-top`
   takes the READ-BACK rather than the requested value — it is what lets the
   scroll handler tell our write from the user's. Instant, never
   `behavior: smooth`: an animation would both fight the segment cadence and
   make that comparison meaningless.

   Pre:  `node` is the `.tape-scroll` container; `row-height` matches the CSS
         `--row-height` the rows are positioned by.
   Post: the row's centre is the viewport's centre, or the scroll is saturated
         at an end; `!last-auto-top` holds the scrollTop we are responsible for.
   Invariant: every write to `node`'s scrollTop from this namespace updates
   `!last-auto-top` in the same breath, or the next scroll event is misread as
   the user's and follow suspends spuriously."
  [node idx row-height !last-auto-top]
  #?(:cljs (when (and node (nat-int? idx))
             (let [view (.-clientHeight node)
                   furthest (max 0 (- (.-scrollHeight node) view))
                   target (-> (- (* idx row-height) (/ (- view row-height) 2))
                            (max 0)
                            (min furthest))]
               ;; Skip a write that would not move anything: the row is already
               ;; centred, or the scroll is already saturated at the end it
               ;; would saturate at. Clamping BEFORE the comparison is what makes
               ;; that second case detectable — an uncentrable row near either
               ;; end otherwise re-writes the same saturated value every segment,
               ;; and each write costs a scroll event.
               (when (> (js/Math.abs (- target (.-scrollTop node))) 1)
                 (set! (.-scrollTop node) target)
                 (reset! !last-auto-top (.-scrollTop node))))
             nil)
     :clj nil))

(defn user-scrolled?
  "Whether `node`'s current scrollTop differs from the one we last wrote.

   A programmatic write fires the same `scroll` event a wheel does, so the
   position itself is the only honest discriminator. One pixel of tolerance
   covers fractional scrollTop on a real display."
  [node !last-auto-top]
  #?(:cljs (and node
             (> (js/Math.abs (- (.-scrollTop node) @!last-auto-top)) 1))
     :clj false))

(defn get-transcript*
  "Transcript rows for a topic.

   `_refresh` is the watched :refresh counter, PASSED IN rather than closed over
   or discarded at the call site. Electric drops a binding whose value the
   server form does not use, and with it the subscription — so a transcript
   written by the background pipeline would never appear until an unrelated
   mutation happened to re-run the query (precedent:
   hierarchy_side_panel/get-pins-for-topic*)."
  [_refresh topic-id]
  #?(:clj (vec (db/get-video-transcript topic-id))
     :cljs []))

(defn copy-transcript!*
  "Server side of the Copy action. Bumps :refresh itself so the editor reloads
   with the new body — the caller only sequences the token."
  [user-id topic-id]
  #?(:clj (let [r (db/copy-video-transcript-to-content! topic-id)]
            (when (:ok r) (commands/bump! user-id :transcribe))
            r)
     :cljs nil))

(defn start-processing!*
  "Launch (or re-launch) the ffmpeg + Whisper pipeline for a video.

   Pre:  topic-id is a kind='video' topic owned by user-id with a topic_videos row.
   Post: returns immediately; the run reports through toasts and :refresh.
         A second request while one is in flight is dropped by
         `video/start-processing!`'s own dedup, so the disabled state below is
         a courtesy, not the guard.

   Re-running is safe and total: duration and the extracted audio are
   overwritten, and `replace-video-transcript!` swaps the whole transcript in
   one transaction. It does NOT touch `content` — only the Copy action does.

   Always passes transcribe? TRUE (§15.3 2.8). This button IS the request to
   transcribe, so the upload-time preference has no bearing on it — a user who
   skips at upload and clicks here must get a transcript."
  [user-id topic-id]
  #?(:clj (video/start-processing! user-id topic-id true)
     :cljs nil))

(defn transcription-estimate*
  "Credits transcribing this video would cost, or nil (§15.3 6.2).

   Pre:  `duration-ms` is `dctx/video-duration-ms` — already fetched with the
         topic overview, so this reads no row of its own. Stage 1 writes it long
         before an empty transcript can render, which is why this needs none of
         the import modal's browser probing.
   Post: a long, or nil when credits are disabled, fx/markup are unconfigured, or
         the pipeline never recorded a duration."
  [user-id duration-ms]
  #?(:clj (when duration-ms
            (video/transcription-cost-estimate user-id (/ duration-ms 1000.0)))
     :cljs nil))

(defn empty-transcript-message
  "What to show when a video has no transcript (§15.3 6.1).

   Pre:  `credits` is `transcription-estimate*`'s reading — a long or nil;
         `duration-ms` may be nil.
   Post: a string, always. Never asserts that a transcript is on its way: the old
         copy — \"It appears once the video has been processed\" — is false for a
         video whose pipeline finished with transcription skipped, and equally
         false when transcription failed. Skipped, failed and never-run are
         indistinguishable from here, so the sentence must hold for all three.
   Invariant: the figure is omitted, never the sentence (§15.3 6.3) — self-host
         has no credits to quote and an unprobed video has no duration."
  [credits duration-ms]
  (if (and credits duration-ms)
    (str "No transcript. Transcribe to create one — about " credits
      " credit" (when (not= 1 credits) "s") " for " (format-ms duration-ms) ".")
    "No transcript. Transcribe to create one."))

(e/defn TranscriptRow
  "One segment: a timestamp gutter and the text. Clicking anywhere seeks.

   `active?` is rendered through Electric rather than toggled imperatively on
   the node: `Tape` recycles DOM slots as the window moves, so a class set
   outside the reactive graph would stay with the SLOT and light whichever
   segment scrolled into it."
  [i row active?]
  (e/client
    (dom/tr
      (dom/props {:class (when active? "transcript-row--active")
                  :aria-current (when active? "true")
                  :style {:--order i :cursor "pointer"
                          :border-bottom "1px solid var(--color-bg-subtle)"}})
      (dom/On "click" (fn [_] (vp/seek-to! @dctx/!video-el (:start_ms row))) nil)
      (dom/td
        (dom/props {:style {:color "var(--color-text-secondary)" :font-size "12px"
                            :font-variant-numeric "tabular-nums" :padding-inline "6px"
                            :vertical-align "top" :white-space "nowrap"}})
        (dom/text (format-ms (:start_ms row))))
      (dom/td
        (dom/props {:style {:font-size "13px" :line-height "1.35" :padding-inline "6px"
                            :vertical-align "top" :overflow "hidden"}})
        (dom/text (:text row))))))

(e/defn TranscriptToolbar
  "Header row: segment count, the pipeline trigger, and the Copy action.

   Both buttons are Forms5 `Button!`s combined with `e/amb` and consumed by ONE
   service `e/for` that dispatches on the command head. `Button!` owns the
   token, the busy `:disabled`, and the `aria-busy`/`aria-invalid` reflection,
   so no click atom is needed — and the token comes from the DOM event, which is
   a fresh object per click and therefore cannot collide with a previous value."
  [topic-id seg-count]
  (e/client
    (let [user-id dctx/user-id
          processing? (e/server
                        (contains? (e/watch (us/get-atom user-id :processing-videos)) topic-id))]
      (dom/div
        (dom/props {:style {:display "flex" :align-items "center" :gap "var(--sp-2)"
                            :padding "6px 8px" :flex-shrink "0" :flex-wrap "wrap"
                            :border-bottom "2px solid var(--color-border)"}})
        (dom/span
          (dom/props {:style {:flex "1" :min-width "60px" :font-size "12px" :font-weight "600"
                              :color "var(--color-text-secondary)"}})
          (dom/text (if (pos? seg-count)
                      (str "Transcript · " seg-count " segments")
                      "Transcript")))
        ;; e/amb, never a `do` — a `do` returns only the last value and would
        ;; swallow the other button's token, hanging that control.
        (e/for [[t [op]] (e/amb
                           (forms/Button! [::transcribe]
                             :class "btn btn-sm btn-primary"
                             :disabled processing?
                             :label (cond processing?       "Transcribing…"
                                      (pos? seg-count) "Re-transcribe"
                                      :else            "Transcribe"))
                           (forms/Button! [::copy]
                             :class "btn btn-sm btn-secondary"
                             :disabled (zero? seg-count)
                             :label "Copy"))]
          (case op
            ;; start-processing! returns immediately after launching the task,
            ;; so the token spends on ACCEPTANCE, not completion. Progress and
            ;; failure surface through :processing-videos and toasts.
            ::transcribe (case (e/server (start-processing!* user-id topic-id))
                           (t))
            ;; Copy is synchronous and can fail (no transcript yet) — spend with
            ;; the error so the button reflects aria-invalid instead of
            ;; reporting a success that did not happen.
            ::copy (let [r (e/server (e/Offload #(copy-transcript!* user-id topic-id)))]
                     (case r
                       (if (:ok r) (t) (t (:error r)))))))))))

(e/defn VideoTranscriptPane
  "Windowed transcript column. Follows the C1c per-row `--order` convention:
   the table's height is the scroll range and each row positions itself, so
   position and content commit in the same Electric frame and rows never skew
   at a window boundary."
  []
  (e/client
    (let [user-id dctx/user-id
          topic-id dctx/video-topic-id
          ;; SERVER-FORM binding, not an e/defn call: the vector stays on the
          ;; server and only `(e/server (nth segs i nil))` window rows cross.
          refresh (e/server (e/watch (us/get-atom user-id :refresh)))
          segs (e/server (get-transcript* refresh topic-id))
          seg-count (e/server (count segs))
          ;; The ONE thing the client needs the whole timeline for. Projected
          ;; server-side so only the ints cross: `(mapv :start_ms …)` on the
          ;; client would drag every full row over with it (CLAUDE.md's
          ;; 372 KB → 13 KB case). ~2000 ints for a three-hour lecture.
          starts (e/server (mapv :start_ms segs))
          ;; Readings from another topic's player are ignored rather than
          ;; cleaned up: video→video navigation does not order the old player's
          ;; unmount against the new one's mount.
          playhead (e/watch dctx/!video-playhead)
          pos-ms (when (= topic-id (:topic-id playhead)) (:ms playhead))
          seeks (:seeks playhead)
          active-idx (active-segment-index starts pos-ms)
          ;; Follow is armed until the user scrolls, and re-armed by any seek —
          ;; which covers both re-arm cases, because clicking a row IS a seek.
          !follow-playhead? (atom true)
          follow? (e/watch !follow-playhead?)
          !last-auto-top (atom -1)]
      (dom/div
        ;; Fills its tab in RightSidePanel — the panel owns width, collapse and
        ;; the border, so this must not set any of them.
        (dom/props {:style {:display "flex" :flex-direction "column"
                            :flex "1" :min-height "0" :min-width "0"}})
        (TranscriptToolbar topic-id seg-count)
        (if (zero? seg-count)
          (dom/div
            (dom/props {:style {:padding "16px" :font-size "13px"
                                :color "var(--color-text-secondary)"}})
            (dom/text (empty-transcript-message
                        (e/server (transcription-estimate* user-id dctx/video-duration-ms))
                        dctx/video-duration-ms)))
          (dom/div
            (dom/props {:class "tape-scroll"
                        :style {:flex "1" :overflow-y "auto" :min-height "0"
                                :scrollbar-gutter "stable"}})
            (let [scroller dom/node]
              ;; Applying a plain fn to a reactive value runs the effect when
              ;; that value CHANGES — the same shape Scroll-window uses for its
              ;; reset-to-top (scroll.cljc:112). The arguments are passed rather
              ;; than closed over so the dependency is explicit and Electric can
              ;; work-skip: the playhead ticks at 3.6 Hz, `active-idx` changes
              ;; about once per segment, and only the latter gets here.
              ((fn [idx armed?]
                 (when armed?
                   (scroll-row-to-center! scroller idx row-height !last-auto-top)))
               active-idx follow?)
              ;; Re-arm on seek. `:seeks` also arrives once at mount, which is a
              ;; no-op against the initial `true`.
              ((fn [_] (reset! !follow-playhead? true)) seeks)
              (dom/On "scroll"
                (fn [_]
                  (when (user-scrolled? scroller !last-auto-top)
                    (reset! !follow-playhead? false))
                  nil)
                nil))
            (let [[offset limit] (Scroll-window row-height seg-count dom/node
                                   ;; Keyed on the topic, NOT the segment count:
                                   ;; a re-transcription that changes the count
                                   ;; must not yank a reading user to the top.
                                   {:overquery-factor 2 :reset-key topic-id})]
              (dom/props {:style {:--count seg-count
                                  :--grid-cols "52px 1fr"
                                  :--row-height (str row-height "px")}})
              (dom/table
                (dom/props {:style {:width "100%"}})
                (e/for [i (Tape offset limit)]
                  (let [row (e/server (nth segs i nil))]
                    (when row
                      (TranscriptRow i row (= i active-idx)))))))))))))
