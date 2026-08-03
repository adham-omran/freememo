(ns freememo.video-upload
  "CLJS video upload — the video half of the chunked transport.

   The transport itself (init → chunk* → finalize, with abort on every failure
   path) lives in `freememo.chunked-upload`, which the archive import flow
   shares. What stays here is what only video means: the transcribe flag, the
   playlist parent, and reading a topic id out of finalize's reply.

   Resume (§4.3 3.2) asks the server how many bytes it actually holds and
   restarts from there, rather than trusting a client-side counter that a
   partially-applied chunk would have made a lie."
  (:require [freememo.chunked-upload :as cu]))

(defn upload-file!
  "Upload one video end to end.

   `opts`: {:file :parent-id :transcribe? :on-progress (fn [sent total])
            :cancelled? (fn [])}
   Returns a Promise of the new topic id.

   `:transcribe?` defaults to true and rides `finalize`, not `init`: the server
   needs it only at the moment it launches the pipeline, and putting it on the
   session row would be state to migrate for no gain (§15.3 3.2).

   `:cancelled?` is polled between chunks rather than mid-request: a chunk
   already in flight has already been paid for in bandwidth, and letting it land
   keeps `received_bytes` consistent with the object's real size."
  [{:keys [^js file parent-id on-progress cancelled? transcribe?]
    :or {on-progress (fn [_ _]) cancelled? (fn [] false) transcribe? true}}]
  (-> (cu/upload!
        {:file file
         :base "/api/video"
         :error-id :video/upload
         :init-params {:filename (.-name file)
                       :mime_type (.-type file)
                       :parent_id parent-id}
         ;; `transcribe` is always sent, both values: `post-json` drops nil
         ;; params, so relying on omission to mean false would send nothing and
         ;; the server would read the absent-⇒-transcribe default.
         :finalize-params {:transcribe (boolean transcribe?)}
         :on-progress on-progress
         :cancelled? cancelled?})
    (.then (fn [^js d] (.-doc_id d)))))

(defn upload-files!
  "Upload `files` one after another, reporting per-file progress.

   Sequential, not parallel: two concurrent 700 MB uploads would double the
   quota reservation, the disk write pressure, and the ffmpeg queue depth for
   no wall-clock win on a single connection.

   `on-file` receives {:index :count :filename :sent :total}. Resolves with the
   vector of created topic ids; rejects on the first failure, leaving already-
   uploaded files in place (they are complete topics, not partial state).

   `:transcribe?` applies to the whole batch (§15.3 3.5). Per-file would mean a
   control on every staged row for a choice the user makes once."
  [{:keys [files parent-id on-file cancelled? transcribe?]
    :or {on-file (fn [_]) cancelled? (fn [] false) transcribe? true}}]
  (let [fs (vec files)
        n (count fs)]
    (letfn [(step [i acc]
              (if (>= i n)
                (js/Promise.resolve acc)
                (let [^js f (nth fs i)]
                  (-> (upload-file!
                        {:file f
                         :parent-id parent-id
                         :transcribe? transcribe?
                         :cancelled? cancelled?
                         :on-progress (fn [sent total]
                                        (on-file {:index i :count n
                                                  :filename (.-name f)
                                                  :sent sent :total total}))})
                    (.then (fn [id] (step (inc i) (conj acc id))))))))]
      (step 0 []))))
