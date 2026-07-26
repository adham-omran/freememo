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
   token and no server round-trip."
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
   one transaction. It does NOT touch `content` — only the Copy action does."
  [user-id topic-id]
  #?(:clj (video/start-processing! user-id topic-id)
     :cljs nil))

(e/defn TranscriptRow
  "One segment: a timestamp gutter and the text. Clicking anywhere seeks."
  [i row]
  (e/client
    (dom/tr
      (dom/props {:style {:--order i :cursor "pointer"
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
          seg-count (e/server (count segs))]
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
            (dom/text "No transcript yet. It appears once the video has been processed."))
          (dom/div
            (dom/props {:class "tape-scroll"
                        :style {:flex "1" :overflow-y "auto" :min-height "0"
                                :scrollbar-gutter "stable"}})
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
                      (TranscriptRow i row))))))))))))
