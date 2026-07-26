(ns freememo.video-extract
  "Extract a marked time range into a child topic (§4.9).

   The extract is an ORDINARY topic from the moment it exists: its `content` is
   the overlapping transcript text copied in at creation, editable thereafter,
   and the `video_segments` row is provenance only. Nothing re-derives the text
   on read.

   That forfeits a no-drift invariant on purpose. An extract whose text was
   copied survives its source video's bytes being reclaimed for unpaid storage;
   an extract that re-derived on read would go blank. Reviewing text you wrote
   months ago must not depend on still paying rent on a 700 MB file."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-forms5 :as forms]
   [freememo.doc-context :as dctx]
   [freememo.commands :as commands]
   [freememo.video-format :refer [format-ms]]
   #?(:clj [freememo.db :as db])))

(defn create-extract!*
  "Server side of Extract. Bumps :extract-video (→ :tree-mutations) so the
   hierarchy panel picks up the new child WITHOUT a :refresh storm that would
   tear down and re-init the Quill editor beside it."
  [user-id video-topic-id start-ms end-ms title]
  #?(:clj (try
            (let [r (db/create-video-extract! user-id video-topic-id start-ms end-ms title)]
              (when (:success r) (commands/bump! user-id :extract-video))
              r)
            (catch clojure.lang.ExceptionInfo e
              {:success false :error (or (ex-message e) "Invalid range")})
            (catch Exception e
              {:success false :error (.getMessage e)}))
     :cljs nil))

(defn range-title
  "Default title for a range extract: the source title plus its timestamps, so
   a list of extracts from one lecture is still legible."
  [source-title start-ms end-ms]
  (str (or source-title "Video") " " (format-ms start-ms) "–" (format-ms end-ms)))

(e/defn VideoExtractButton
  "Create a child topic from the marked range. Disabled until a range exists.

   Forms5 `Button!` rather than a hand-rolled click atom, for a specific reason
   beyond convention: an atom fed `(:start-ms region)` would not re-fire when
   two successive ranges share a start. Marking [10s,20s], extracting, then
   marking [10s,30s] and clicking would write the same value, `e/Token` would
   see no change, and the button would silently do nothing. `Button!`'s token
   comes from the DOM event — a fresh object per click.

   The range travels IN the command, so the effect uses the bounds as they were
   at click time even if the user re-drags while the server call is in flight.

   Clears the range on success only: on failure the mark stays so the user can
   adjust and retry, and Forms5 reflects the error as `aria-invalid`."
  []
  (e/client
    (let [user-id dctx/user-id
          topic-id dctx/video-topic-id
          title dctx/video-title
          !video-region dctx/!video-region
          region (e/watch !video-region)]
      (e/for [[t [_op start-ms end-ms]]
              (forms/Button! [::extract (:start-ms region) (:end-ms region)]
                :class "btn btn-sm btn-primary"
                :disabled (nil? region)
                :label "Extract range")]
        (let [result (e/server
                       (e/Offload
                         #(create-extract!* user-id topic-id start-ms end-ms
                            (range-title title start-ms end-ms))))]
          (case result
            (if (:success result)
              ;; Registered inside the success branch, so the range is only
              ;; cleared when a topic was actually created.
              (do (e/on-unmount #(reset! !video-region nil))
                  (t))
              (t (:error result)))))))))
